(ns pl.danieljanus.setgame
  (:gen-class)
  (:use clojure.set 
        clojure.contrib.fcase
        clojure.contrib.math
        pl.danieljanus.iter)
  (:import [javax.swing JPanel JFrame JButton JLabel]
           [java.awt Dimension Polygon Color BasicStroke GradientPaint GridBagLayout GridBagConstraints Font]
           [java.awt.event ActionListener MouseAdapter]
           [java.awt.geom Ellipse2D$Float Rectangle2D$Float]))

;; Game logic

(defn myset? [card1 card2 card3]
  (every? #(#{1 3} %)
          (map #(count (set [%1 %2 %3])) card1 card2 card3)))

(defn decode-card [x]
  (map #(mod (quot x (expt 3 %)) 3) (range 4)))

(defn complement-set [card1 card2]
  (map #(if (= %1 %2)
          %1
          (first (difference #{1 2 3} #{%1 %2})))
       card1 card2))

(defn set-anywhere? [cards]
  (let [cardset (set cards)]
    (reduce #(or %1 %2)
            (map (fn [[x y]] (cardset (complement-set x y)))
                 (for [c1 cards c2 cards :when (not= c1 c2)] [c1 c2])))))

(defn shuffle [seq]
  (let [gen (java.security.SecureRandom.)
        a (make-array Byte/TYPE 20)]
    (map first (sort-by second (map #(do (.nextBytes gen a) (list % (vec a))) seq)))))

(defn adjust-positions [game]
  (let [{:keys [cards-shown cards-positions]} game
        positions-set (set cards-positions)]
    (assoc game :cards-positions 
           (concat cards-positions 
                   (iter (repeat cards-shown)
                         (for y initially 0 then (if (= y 2) 0 (inc y)))
                         (for x initially 0 then (if (= y 2) (inc x) x))
                         (collect [x y] if (not (positions-set [x y]))))))))

(defn start-game []
  (adjust-positions
   {:started true
    :cards (map decode-card (shuffle (range 81)))
    :cards-shown 12
    :cards-positions []
    :time 0
    :picked #{}}))

(defn xorset [s x]
  (if (s x)
    (difference s #{x})
    (conj s x)))

;; GUI

(defn polygon [& points]
  (Polygon. (into-array Integer/TYPE (map first points))
            (into-array Integer/TYPE (map second points))
            (count points)))

(defn draw-shape [g shape color fill]
  (let [shape (case shape
                0 (Ellipse2D$Float. 0 0 50 50)
                1 (Rectangle2D$Float. 0 0 50 50)
                2 (polygon [0 47] [25 3] [50 47]))
        color (case color
                0 Color/RED
                1 Color/GREEN
                2 Color/BLUE)
        paint (case fill
                0 Color/WHITE
                1 (GradientPaint. 0 0 Color/WHITE 0 50 color)
                2 color)]
    (doto g
      (.setStroke (BasicStroke. 5))
      (.setPaint color)
      (.draw shape)
      (.setPaint paint)
      (.fill shape))))    

(defn draw-card [g x y [shape cnt color fill] picked?]
  (let [transform (.getTransform g)]
    (doto g
      (.translate x y)
      (.setStroke (BasicStroke. 2))
      (.setPaint Color/BLACK)
      (.draw (Rectangle2D$Float. 5 5 75 185)))
    (when picked?
      (.setPaint g Color/DARK_GRAY)
      (.fill g (Rectangle2D$Float. 5 5 75 185)))
    (.translate g 15 (+ 10 (* 30 (- 2 cnt))))
    (doseq [_ (range (inc cnt))]
      (draw-shape g shape color fill)
      (.translate g 0 60))
    (.setTransform g transform)))

;; Yay, Clojure makes handling GridBagLayouts a breeze!
(defmacro add-gridbag [panel component & args]
  (let [c (gensym)]
    `(let [~c (GridBagConstraints.)]
       (do
         ~@(map (fn [[k v]] `(set! (. ~c ~(symbol (name k))) ~v))
                (partition 2 args))
         (.add ~panel ~component ~c)))))

(defmacro button [name & on-click]
  `(let [b# (JButton. ~name)]
     (.addActionListener b# (proxy [ActionListener] []
                              (actionPerformed [_#] ~@on-click)))
     b#))

(defn paint [g {:keys [cards cards-shown cards-positions started picked]}]
  (if started
    (iter (for card in cards)
          (for [x y] in cards-positions)
          (for i from 0 to (dec cards-shown))
          (draw-card g (* x 90) (* y 200) card (picked i)))
    (let [script ["xxx.xxx.xxx"
                  "x...x....x."
                  "xxx.xxx..x."
                  "..x.x....x."
                  "xxx.xxx..x."]
          rnd (java.util.Random.)
          transform (.getTransform g)]
      (iter (for y from 150 by 55) 
            (for line in script)
            (iter (for x from 10 by 55)
                  (for ch in line)
                  (when (= ch \x)
                    (.translate g x y)
                    (draw-shape g (.nextInt rnd 3) (.nextInt rnd 3) (.nextInt rnd 3))
                    (.setTransform g transform))))
      (.setPaint g Color/BLACK)
      (.setFont g (Font. "Serif" Font/BOLD 72))
      (.drawString g "Clojure" 160 90)
      (.setFont g (Font. "Dialog" Font/PLAIN 16))
      (.drawString g "Written by Daniel Janus, 20 Nov 2009" 170 500))))

(defmacro debug
  "Wrap any form in this macro to make it evaluate the same thing, but also output
the result to *err*."
  [expr]
  `(let [x# ~expr]
     (do
       (binding [*out* *err*] (println "DEBUG:" x#))
       x#)))

(defn handle-click [event state]
  (let [x (quot (.getX event) 90)
        y (quot (.getY event) 200)
        {:keys [cards cards-shown cards-positions picked started time]} @state
        _ (debug cards-positions)
        pos (first (iter (for c in cards-positions)
                         (for i from 0)
                         (collect i if (= c [x y]))))]
    (when (and started pos (< (debug pos) cards-shown))
      (let [new-picked (xorset picked pos)]
        (if (= 3 (count new-picked))
          (let [cards-picked (map #(nth cards %) new-picked)
                ok? (apply myset? cards-picked)
                eog? (and (<= (count cards) cards-shown)
                          (not (set-anywhere? cards)))
                cards (if ok?
                        (iter (for c in cards)
                              (for i from 0)
                              (collect c if (not (new-picked i))))
                        cards)
                cards-positions (if ok? ;; XXX: this repetition is a little ugly
                                  (iter (for c in cards-positions)
                                        (for i from 0)
                                        (collect c if (not (new-picked i))))
                                  cards-positions)]
            (reset! state 
                    (if eog?
                      {:started false, :time time}
                      (adjust-positions {:started true, :cards cards, :cards-positions cards-positions, :cards-shown (max 12 (- cards-shown 3)), :time (if ok? time (+ time 60)), :picked #{}}))))
          (swap! state #(assoc % :picked new-picked))))
      true)))

(defn visible-cards [{:keys [cards-shown cards started]}]
  (when started
    (take cards-shown cards)))

(defn set-status [{:keys [cards time started]} label]
  (.setText label
            (if started
              (str "Cards left: " (count cards)
                   "    Elapsed time: " time " s")
              (str "Game complete!  Total time: " time " s"))))

(defn make-frame []
  (let [state (atom {:started false})
        frame (JFrame. "Clojure SET")
        content (JPanel.)
        main-pane (proxy [JPanel] []
                    (paintComponent [g]
                      (proxy-super paintComponent g)
                      (paint g @state)))
        status-pane (JLabel. "Welcome to Clojure SET.  Press Start to begin.")]
    (doto main-pane
      (.setPreferredSize (Dimension. (* 7 90) 600))
      (.addMouseListener (proxy [MouseAdapter] []
                           (mouseClicked [e]
                                         (when (handle-click e state)
                                           (set-status @state status-pane)
                                           (.repaint main-pane))))))
    (send-off (agent nil)
              (fn timer [_]
                (Thread/sleep 1000)
                (when (:started @state)
                  (swap! state #(assoc % :time (inc (:time %))))
                  (set-status @state status-pane))
                (send-off *agent* timer)))
    (doto content
      (.setLayout (GridBagLayout.))
      (add-gridbag main-pane :fill GridBagConstraints/NONE :gridx 0 :gridy 0 :gridwidth 2)
      (add-gridbag status-pane :fill GridBagConstraints/HORIZONTAL :gridx 0 :gridy 2 :gridwidth 2)
      (add-gridbag (button "Start" 
                           (reset! state (start-game))
                           (.repaint main-pane)
                           (set-status @state status-pane))
                   :fill GridBagConstraints/HORIZONTAL :gridx 0 :weightx 0.5 :gridy 1)
      (add-gridbag (button "No more sets"
                           @state
                           (when (:started @state)
                             (swap! state
                                    #(if (debug (set-anywhere? (visible-cards %)))
                                       (assoc % :time (+ (:time %) 60))
                                       (adjust-positions (assoc % :cards-shown (inc (:cards-shown %))))))
                             (.repaint main-pane)
                             (set-status @state status-pane)))
                   :fill GridBagConstraints/HORIZONTAL :gridx 1 :weightx 0.5 :gridy 1))
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setContentPane content)
      .pack
      .show)))

(defn -main []
  (make-frame))