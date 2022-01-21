;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.viewport.scroll-bars
  (:require
   [app.common.uuid :as uuid]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.point :as gpt]
   [app.common.spec :as us]
   [app.main.data.workspace :as dw]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [app.main.ui.hooks :as hks]
   [app.main.ui.workspace.viewport.actions :as actions]
   [app.main.ui.workspace.viewport.utils :as utils]
   [app.util.dom :as dom]
   [beicon.core :as rx]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))


(defn translate-point-to-viewport [viewport zoom pt]
  (when-let [_ (and viewport zoom pt)]
    (let [vbox     (.. ^js viewport -viewBox -baseVal)
          brect    (dom/get-bounding-rect viewport)
          brect    (gpt/point (:left brect)
                              (:top brect))
          box      (gpt/point (.-x vbox) (.-y vbox))
          zoom     (gpt/point zoom)]
      (-> (gpt/subtract pt brect)
          (gpt/divide zoom)
          (gpt/add box)))))

;; TODO: renane
(defn update-vertical-scroll-position [y-delta scroll-top? scroll-bottom?]
  (ptk/reify ::update-vertical-scroll-position
    ptk/UpdateEvent
    (update [_ state]
            ;; (println "update-vertical-scroll-position" y-delta scroll-top? scroll-bottom?)
            (update-in state [:workspace-local :vbox]
                       (fn [vbox]
                         (-> vbox
                             (update :y #(+ % y-delta))
                             (update :scroll-top? #(-> scroll-top?))
                             (update :scroll-bottom? #(-> scroll-bottom?))
                             (update :y-delta #(-> y-delta))
                             (update :scroll-speed #(-> 1))))))))

(defn keep-scrolling [zoom]
  (ptk/reify ::keep-scrolling
    ptk/UpdateEvent
    (update [_ state]
            (update-in state [:workspace-local :vbox]
                       (fn [vbox]
                         (let [y-delta (-> (/ 100 zoom)
                                           (* (:scroll-speed vbox)))
                               scroll-speed (-> (:scroll-speed vbox)
                                                (* 2)
                                                (min 10))
                               _ (println "keep -scrolling" zoom scroll-speed y-delta)
                               update-y (cond
                                          (:scroll-top? vbox) #(- % y-delta)
                                          (:scroll-bottom? vbox) #(+ % y-delta)
                                          :else #(-> %))]
                           (-> vbox
                               (update :y update-y)
                               (update :scroll-speed #(-> scroll-speed)))))))))

(defn start-vertical-scrolling [zoom]
  (ptk/reify ::start-vertical-scrolling
    ptk/WatchEvent
    (watch [_ state stream]
      (let [stopper (->> stream (rx/filter (ptk/type? ::finish-vertical-scrolling)))]
        (when-not (get-in state [:workspace-local :scrolling])
           (->> (rx/interval 300)
                (rx/map #(keep-scrolling zoom))
                ;; (rx/tap #(println "ASDASD" zoom))
                (rx/take-until stopper)
                #_(rx/ignore)))))))


(defn finish-vertical-scrolling []
  (ptk/reify ::finish-vertical-scrolling
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update :workspace-local dissoc :scrolling)))))

(mf/defc viewport-vertical-scrollbar
  {::mf/wrap [mf/memo]}
  [{:keys [viewport-ref zoom vbox]}]

  (let [scrolling?-ref         (mf/use-ref false)
        scrollbar-height-ref   (mf/use-ref nil)
        start-ref              (mf/use-ref nil)
        current-ref            (mf/use-ref nil)
        cursor-to-scroll-ref   (mf/use-ref nil)

        base-objects           (mf/deref refs/workspace-page-objects)
        root-shapes            (get-in base-objects [uuid/zero :shapes])
        shapes                 (->> root-shapes (mapv #(get base-objects %)))
        base-objects-rect      (gsh/selection-rect shapes)

        top-offset             (max 0 (- (:y vbox) (:y base-objects-rect)))
        bottom-offset          (max 0 (- (:y2 base-objects-rect) (+ (:y vbox) (:height vbox))))
        vertical-offset        (+ top-offset bottom-offset)
        show-vertical-scroll?  (or (mf/ref-val scrolling?-ref) (> vertical-offset 0))

        top-offset             (if (> vertical-offset (:height vbox))
                                 (/ (* (:height vbox) top-offset) vertical-offset)
                                 top-offset)
        bottom-offset          (if (> vertical-offset (:height vbox))
                                 (/ (* (:height vbox) bottom-offset) vertical-offset)
                                 bottom-offset)

        _ (println "top-offset" top-offset "bottom-offset" bottom-offset)

        inv-zoom               (/ 1 zoom)
        coords                 (hks/use-rxsub ms/mouse-position)

        state-cursor-y         (get-in @st/state [:workspace-local :cursor-y])
        state-scrollbar-y      (get-in @st/state [:workspace-local :scrollbar-y])
        state-scrollbar-height (get-in @st/state [:workspace-local :scrollbar-height])

        _ (println "vertical-offset" vertical-offset)

        scrollbar-x            (+ (:x vbox) (:width vbox) (* inv-zoom -40) #_(* zoom -20))
        scrollbar-height       (- (:height vbox) vertical-offset)
        scrollbar-height       (max scrollbar-height (* inv-zoom 20))
        scrollbar-height       (if (mf/ref-val scrolling?-ref)
                                 (mf/ref-val scrollbar-height-ref)
                                 scrollbar-height)


        viewport               (mf/ref-val viewport-ref)

        scrollbar-y            (-> (+ (:y vbox) top-offset)
                                   (max (+ (:y vbox) (* inv-zoom 40)))
                                   (min (+ (:y vbox) (:height vbox) (- scrollbar-height) (- (* inv-zoom 40)))))

        ;; (if (and (mf/ref-val scrolling?-ref) (not (scroll-top?)))
        scrollbar-y            (if (mf/ref-val scrolling?-ref)
                                 (-> (translate-point-to-viewport viewport zoom (mf/ref-val start-ref))
                                     (:y)
                                     (- (mf/ref-val cursor-to-scroll-ref)))
                                 scrollbar-y)

        scroll-top?            false ;;(< scrollbar-y (:y vbox))
        scroll-bottom?         false ;;(> (+ scrollbar-y scrollbar-height) (+ (:y vbox) (:height vbox)))

        ;; scrollbar-y (if scroll-bottom?
        ;;               (+ (:y vbox) (:height vbox) (- scrollbar-height))
        ;;               scrollbar-y)


        on-scroll-down         (actions/on-scroll-down (:y coords) scrollbar-y scrollbar-height)
        on-scroll-up           (actions/on-scroll-up)

        on-mouse-move
        (mf/use-callback
         (mf/deps viewport vbox top-offset zoom scroll-top? scroll-bottom?)
         (fn [event]
           (when-let [_ (mf/ref-val scrolling?-ref)]
             (let [start-pt (mf/ref-val start-ref)
                   current-pt (dom/get-client-position event)
                   delta (/ (- (:y current-pt) (:y start-pt)) zoom)]

               (mf/set-ref-val! start-ref current-pt)
               (st/emit! (update-vertical-scroll-position delta scroll-top? scroll-bottom?))))))

        on-mouse-down
        (mf/use-callback
         (mf/deps coords zoom scrollbar-height scrollbar-y)
         (fn [event]
           (mf/set-ref-val! scrolling?-ref true)
           (mf/set-ref-val! scrollbar-height-ref scrollbar-height)
           (mf/set-ref-val! start-ref (dom/get-client-position event))
           (mf/set-ref-val! cursor-to-scroll-ref (-> (- (:y coords) scrollbar-y)
                                                     (max 0)))
           (st/emit! (start-vertical-scrolling zoom))))

        on-mouse-up
        (mf/use-callback
         (mf/deps)
         (fn [event]
           (let [start-pt   (mf/ref-val start-ref)
                 current-pt (dom/get-client-position event)
                 delta      (/ (- (:y current-pt) (:y start-pt)) zoom)]

             (mf/set-ref-val! scrolling?-ref false)
             (st/emit! (finish-vertical-scrolling)))))]

    (when show-vertical-scroll?
      [:g.vertical-scroll
       [:rect {:on-mouse-move on-mouse-move
               :on-mouse-down on-mouse-down
               :on-mouse-up       on-mouse-up
               :width (* inv-zoom 10)
               :rx (* inv-zoom 4)
               :ry (* inv-zoom 4)
               :height scrollbar-height
               :transform (str "translate(" scrollbar-x ", " scrollbar-y ")")}]])))
