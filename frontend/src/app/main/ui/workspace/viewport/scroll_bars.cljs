;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.viewport.scroll-bars
  (:require
   [app.common.uuid :as uuid]
   [app.common.geom.shapes :as gsh]
   [app.common.spec :as us]
   [app.main.data.workspace :as dw]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [app.main.ui.hooks :as hks]
   [app.main.ui.workspace.viewport.actions :as actions]
   [app.util.dom :as dom]
   [beicon.core :as rx]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))


;; TODO: renane
(defn update-vertical-scroll-position [y-delta]
  (ptk/reify ::update-vertical-scroll-position
    ptk/UpdateEvent
    (update [_ state]
      (println "update-vertical-scroll-position" y-delta)
      (update-in state [:workspace-local :vbox]
                 (fn [vbox]
                   (-> vbox
                       (update :y #(+ % y-delta))
                       (update :y-delta #(-> y-delta))))))))


(defn start-vertical-scrolling []
  (ptk/reify ::start-vertical-scrolling
    ptk/WatchEvent
    (watch [_ state stream]
      (let [stopper (->> stream (rx/filter (ptk/type? ::finish-vertical-scrolling)))]
        (when-not (get-in state [:workspace-local :scrolling])
           (->> (rx/interval 100)
                ;; (rx/tap #(println "TODO" (get-in state [:workspace-local :vbox :y-delta])))
                (rx/take-until stopper)
                ;; (rx/map (fn [delta]
                ;;             (update-vertical-scroll-position (get-in state [:workspace-local :vbox :y-delta]))))))))))
                (rx/ignore)))))))

(defn finish-vertical-scrolling []
  (ptk/reify ::finish-vertical-scrolling
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update :workspace-local dissoc :scrolling)))))

(mf/defc viewport-vertical-scrollbar
  {::mf/wrap [mf/memo]}
  [{:keys [zoom vbox]}]

  (let [base-objects      (mf/deref refs/workspace-page-objects)
        root-shapes   (get-in base-objects [uuid/zero :shapes])
        shapes        (->> root-shapes (mapv #(get base-objects %)))
        base-objects-rect (gsh/selection-rect shapes)

        top-offset (max 0 (- (:y vbox) (:y base-objects-rect)))
        bottom-offset (max 0 (- (:y2 base-objects-rect) (+ (:y vbox) (:height vbox))))
        vertical-offset (+ top-offset bottom-offset)

        ;; _ (println "top-offset1 " top-offset "bottom-offset" bottom-offset "height" (:height vbox))

        top-offset (if (> vertical-offset (:height vbox))
                     (/ (* (:height vbox) top-offset) vertical-offset)
                     top-offset)
        bottom-offset (if (> vertical-offset (:height vbox))
                        (/ (* (:height vbox) bottom-offset) vertical-offset)
                        bottom-offset)
        ;; _ (println "top-offset2 " top-offset "bottom-offset" bottom-offset "height" (:height vbox))

        inv-zoom          (/ 1 zoom)
        coords (hks/use-rxsub ms/mouse-position)
        ;; dom/get-client-position
        ;; dividir entre el zoom para pasar a distancia viewport

        ;; TODO: fix this state
        scrolling?-ref     (mf/use-ref false)
        scrolling? (mf/ref-val scrolling?-ref)


        ;; _ (println "-------------" scrolling?)

        start-ref (mf/use-ref nil)

        state-cursor-y         (get-in @st/state [:workspace-local :cursor-y])
        state-scrollbar-y         (get-in @st/state [:workspace-local :scrollbar-y])
        state-scrollbar-height         (get-in @st/state [:workspace-local :scrollbar-height])

        scrollbar-x       (+ (:x vbox) (:width vbox) (* inv-zoom -40) #_(* zoom -20))
        scrollbar-height  (- (:height vbox) vertical-offset)
        scrollbar-height  (max scrollbar-height (* inv-zoom 20))
        ;; scrollbar-height  (if scrolling?
        ;;                     state-scrollbar-height
        ;;                     scrollbar-height)

        ;; _ (println "scrolling" scrolling?)
        ;; _ (println "state-scrollbar-x" state-scrollbar-x)
        ;; _ (println "state-scrollbar-height" state-scrollbar-height)
        ;; _ (println "scrollbar-height" scrollbar-height)

        ;; _ (println "state-cursor-y" state-cursor-y)
        ;; _ (println "state-scrollbar-y" state-scrollbar-y)
        ;; _ (println "state-scrollbar-height" state-scrollbar-height)
        ;; _ (println "coords" (:y coords))

        scroll-top? (< (:y vbox) (:y base-objects-rect))
        scroll-bottom?  (< (:y2 base-objects-rect) (+ (:y vbox) (:height vbox)))
      _ (println "scroll-top?" scroll-top? "scroll-bottom?" scroll-bottom?)

        scrollbar-y       (+ (:y vbox) top-offset)
        scrollbar-y       (max scrollbar-y (+ (:y vbox) (* inv-zoom 40)))
        scrollbar-y       (min scrollbar-y (+ (:y vbox) (:height vbox) (- scrollbar-height) (- (* inv-zoom 40))))
        ;; scrollbar-y       (if scrolling?
        ;;                     (- (:y coords) (- state-cursor-y state-scrollbar-y))
        ;;                     scrollbar-y)

        ;; _ ( println "scrolling" scrolling? scrollbar-y (:y vbox))
        ;; _ (println (:y coords))

        on-scroll-down    (actions/on-scroll-down (:y coords) scrollbar-y scrollbar-height)
        on-scroll-up      (actions/on-scroll-up)
        show-vertical-scroll?    (or scrolling? (> vertical-offset 0))

        ;; _ (println "vertical-offset" vertical-offset show-vertical-scroll?)

        on-mouse-move
        (mf/use-callback
         (mf/deps zoom)
         (fn [event]
           (when-let [_ (mf/ref-val scrolling?-ref)]
             (let [start-pt (mf/ref-val start-ref)
                   current-pt (dom/get-client-position event)
                   delta (/ (- (:y current-pt) (:y start-pt)) zoom)
                  ;;  _ (println "delta" delta start-pt current-pt)
                   ]

              ;;  (println "YES!")
               (mf/set-ref-val! start-ref current-pt)
              ;;  (st/emit! (update-viewport-position {:y #(+ % delta)}))))))
               (st/emit! (update-vertical-scroll-position delta))))))

        on-mouse-down
        (mf/use-callback
         (mf/deps)
         (fn [event]
           (mf/set-ref-val! scrolling?-ref true)
           (mf/set-ref-val! start-ref (dom/get-client-position event))
           (st/emit! (start-vertical-scrolling))))


            ;;  (let [position (dom/get-client-position event)
            ;;        _ (println "on-mouse-down" position)])))

        on-mouse-up
        (mf/use-callback
         (mf/deps)
         (fn [event]
           (let [start-pt (mf/ref-val start-ref)
                 current-pt (dom/get-client-position event)
                 delta (/ (- (:y current-pt) (:y start-pt)) zoom)
                ;;  _ (println "delta" delta start-pt current-pt)
                 ]

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
