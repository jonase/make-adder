(ns game
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [clojure.browser.dom :as dom]
            [goog.events.KeyHandler.EventType :as event-type]
            [goog.events.KeyCodes :as key-codes]
            [goog.ui.Dialog :as Dialog]
            [goog.ui.Dialog.ButtonSet :as ButtonSet]
            [goog.ui.Dialog.ButtonSet.DefaultButtons :as default-buttons]
            [goog.ui.Dialog.EventType :as dialog-event-type]))

;; State
(def nums       (atom nil))
(def num        (atom nil))
(def score      (atom nil))
(def drop-count (atom nil))

(defn reset-game-state []
  (reset! nums       {:left () :right ()})
  (reset! num        (rand-int 100))
  (reset! score      0.5)
  (reset! drop-count 0))

(defn pad [n fill coll]
  (if (< (count coll) n)
    (recur n fill (conj coll fill))
    coll))

(defn render-nums [& _]
  (let [new-ul (dom/element :ul {:id "nums"})
        lhs (->> @nums :left vec (pad 3 "") (take 3))
        rhs (->> @nums :right vec (pad 3 "") (take 3))
        li-items (map #(dom/element :li (str %))
                      (concat (reverse lhs)
                              ["?"]
                              rhs))]
    (doseq [li li-items]
      (dom/append new-ul li))
    (gdom/replaceNode new-ul (dom/get-element "nums"))))



(defn move-right []
  (when-let [n (-> @nums :right first)]
    (swap! nums #(-> %
                     (update-in [:right] pop)
                     (update-in [:left] conj n)))))

(defn move-left []
  (when-let [n (-> @nums :left first)]
    (swap! nums #(-> %
                     (update-in [:left] pop)
                     (update-in [:right] conj n)))))

(defn render-expr [& _]
  (let [a (rand-int @num)
        b (- @num a)]
    (gdom/setTextContent (dom/get-element "expr")
                         (str a " + " b))))

(defn drop-num []
  (let [a (-> @nums :left first)
        b (-> @nums :right first)]
    (if (and (or (nil? a)
                 (<= a @num))
             (or (nil? b)
                 (<= @num b)))      
      (do ;; Ok
        (swap! nums update-in [:left] conj @num)
        (reset! num (rand-int 100))
        (swap! score + 0.1)
        (when (> @score 1)
          (reset! score 1))
        (swap! drop-count inc))
      (do ;; Fail
        (swap! score - 0.1)))))

(defn register-keys []
  (events/listen (events/KeyHandler. js/document)
                 event-type/KEY
                 (fn [e]
                   (condp = (.keyCode e)
                     key-codes/LEFT (move-left)
                     key-codes/RIGHT (move-right)
                     key-codes/SPACE (drop-num)
                     nil))))

(defn render-score [& _]
  (let [ctx (.getContext (dom/get-element "score") "2d")
        linear-gradient (.createLinearGradient ctx 0 0 766 15)]
    (doto linear-gradient
      (.addColorStop 0 "#F00")
      (.addColorStop 1 "#0F0"))
    (set! (.fillStyle ctx) linear-gradient)
    (.clearRect ctx 0 0 766 15)
    (.fillRect ctx 0 0 (* 766 @score) 15)))

(declare game-over-dialog)

(defn start-game []
  (reset-game-state)
  (let [interval-id (atom -1)]
    (reset! interval-id
            (js/setInterval
             #(do
                (if (or (< @score 0)
                        (> @drop-count 15))
                  (do ;; Game over
                    (js/clearInterval @interval-id)
                    (.setVisible (game-over-dialog @score) true))
                  (do 
                    (swap! score - 0.0005))))
             20))))

(defn welcome-dialog []
  (doto (goog.ui/Dialog.)
    (.setTitle "Welcome to make-adder")
    (.setContent (str "<p style='width: 300px'>"
                      "Move left and right with your <strong>arrow-keys</strong>. "
                      "When the sum is in range, hit <strong>space-bar</strong> to get a new expression. "
                      "Complete 15 additions to win the game. If time runs out you loose."
                      "</p>"
                      "<strong>Good luck!</strong>"))
    (.setButtonSet (.addButton (goog.ui.Dialog.ButtonSet.)
                               default-buttons/CONTINUE
                               true))
    (events/listen dialog-event-type/SELECT  #(start-game))))

(defn game-over-dialog [score]
  (let [msg (if (<= score 0)
              "Too bad. Try again!"
              (str "Nice work! You scored " (* 100 score) "%"))]
    (doto (goog.ui/Dialog.)
      (.setTitle "Game Over")
      (.setContent msg)
      (.setButtonSet (.addButton (goog.ui.Dialog.ButtonSet.)
                                 default-buttons/CONTINUE
                                 true))
      (events/listen dialog-event-type/SELECT #(start-game)))))

(defn ^:export start []
  (add-watch nums :nums-change render-nums)
  (add-watch score :score-change render-score)
  (add-watch num :num-change render-expr)
  (register-keys)
  (.setVisible (welcome-dialog) true))
