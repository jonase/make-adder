(ns game
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [goog.events.KeyHandler.EventType :as event-type]
            [goog.events.KeyCodes :as key-codes]
            [goog.ui.Dialog :as Dialog]
            [goog.ui.Dialog.ButtonSet :as ButtonSet]
            [goog.ui.Dialog.ButtonSet.DefaultButtons :as default-buttons]
            [goog.ui.Dialog.EventType :as dialog-event-type]))

;; State
(def left  (atom ())) ;; nums left of ?
(def right (atom ())) ;; nums right of ?
(def num   (atom (rand-int 100)))
(def score (atom 0.5))
(def drop-count (atom 0))

(defn reset-game-state []
  (reset! left ())
  (reset! right ())
  (reset! num (rand-int 100))
  (reset! score 0.5)
  (reset! drop-count 0))

(defn pad [coll n fill]
  (if (< (count coll) n)
    (recur (conj coll fill) n fill)
    coll))

(defn render-nums []
  (let [new-ul (dom/createDom "ul" (js* "{id: 'nums'}"))
        lhs (pad (vec (take 3 @left)) 3 "")
        rhs (pad (vec (take 3 @right)) 3 "")
        li-items (map #(dom/createDom "li"
                                      nil
                                      (dom/createTextNode (str %)))
                      (concat (reverse lhs)
                              ["?"]
                              rhs))]
    (doseq [li li-items]
      (dom/appendChild new-ul li))
    (dom/replaceNode new-ul (dom/getElement "nums"))))

(defn move-right []
  (let [n (first @right)]
    (when n
      (swap! right pop)
      (swap! left conj n)
      (render-nums))))  

(defn move-left []
  (let [n (first @left)]
    (when n
      (swap! left pop)
      (swap! right conj n)
      (render-nums))))

(defn render-expr []
  (let [a (rand-int @num)
        b (- @num a)]
    (dom/setTextContent (dom/getElement "expr")
                        (str a " + " b))))

(defn drop-num []
  (let [a (first @left)
        b (first @right)]
    (if (and (or (nil? a)
                 (<= a @num))
             (or (nil? b)
                 (<= @num b)))      
      (do ;; Ok
        (swap! left conj @num)
        (reset! num (rand-int 100))
        (render-nums)
        (render-expr)
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

(register-keys)

(defn render-score []
  (let [ctx (.getContext (dom/getElement "score") "2d")
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
  (render-nums)
  (render-expr)
  (render-score)
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
                    (swap! score - 0.0005)
                    (render-score))))
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
  (.setVisible (welcome-dialog) true))
