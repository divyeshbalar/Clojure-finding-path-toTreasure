;; Divyeshkumar Balar(40062267)
;; Divyeshkumar_balar@outlook.com
;; github: https://github.com/divyeshbalar 
;; code will be uploaded on github after submission date


(defn make-empty-matrix [row, col, value] 
    (vec (replicate row (vec (replicate col value)))))


(defn Example []
   (with-open [rdr (clojure.java.io/reader "map.txt")]
   (reduce conj [] (line-seq rdr))))
;;(Example) ;; read the file

(def mapData (vec (Example))) ;; making vector of map information
;;(def mapVec (vec (Example)))
(def rowlen (count mapData))
(def mapVec (vector [])) 

;;this vector is to modify during operation
;;(def maxcols 0)
;;------------------------------------------------------------------------------
;; (loop [x 0]
;;       (when (< x rowlen)
;;       (do
;;               (def cLine (vec (char-array (nth mapData x))))
;;               (if(< maxcols (count cLine))
;;                   (def maxcols (count cLine))
;;                   (print "")
;;                   )
;;               (def mapVec (assoc mapVec x cLine))
;;       )
;;       (recur (+ x 1))))
;;-----------------------------------------------------------------------------
(def maxcols (count (first mapData)))
(def mapFlag true)
(loop [x 0]
      (when (< x rowlen)
      (do
              (def cLine (vec (char-array (nth mapData x))))
              (if(or (< maxcols (count cLine))(> maxcols (count cLine)))
                  (def mapFlag false)
                  (print "")
                  )
              (def mapVec (assoc mapVec x cLine))
      )
      (recur (+ x 1))))
;;-----------------------------------------------------------------------------
(def isVisited (make-empty-matrix rowlen maxcols -1))
(def line (first mapData)) ;; getting very first line of the map information
;;(println (count mapData)(count line)) ;; getting count of rows and column
;;(println mapData "is map data")
;; (def len (count line)) ;;assigning length of first line/columns
;; (def len1 (count mapData)) ;;assigning lenght of tows
;;looping through map and picking every single character and printing it


(println "This is my Challange: ")
  (loop [x 0]
      (when (< x rowlen)
      (do(println (nth mapData x)))
      (recur (+ x 1))))
(def flag false)
(if(true? mapFlag)
(do
(if (true? (CheckNext 0 0 ""))
(do
  (println "Bingo Dear | You just got the treasure")
  (loop [x 0]
      (when (< x rowlen)
      (do
              (println (nth mapVec x))
      )
      (recur (+ x 1))))
)
(do
  (println "-------------------------------Sorry mate | Better luck in next cave :(((((")
)))
(do (println "invalid map")))
;;-----------------------------------------------------------------------
(defn CheckNext [i j steps]  ;;steps is list of vector containing the value of x,y 
;;try cons and conj in order to achieve the right steps
  (def i (int i))
  (def j (int j))
  (def collen (count (get mapData (int i))))
  
  (if(or (>= i rowlen) (>= j collen))
    (do 
      (true? false)
    )
    (do
      (if(true? flag)
        (do
          (true? true)
        )
        (do
          (def currentChar (get (get mapVec i) j))
          (if(and (= \+ currentChar) (= 1 (get (get isVisited i)j)))
            (do
              (true? true)
            )
            (do
              (if(= 1 (get (get isVisited i) j))
                (do
                  (true? false)
                )
                (do
                  (def currentRow (get isVisited i))
                  (def currentRow (assoc currentRow j 1))
                  (def isVisited (assoc isVisited i currentRow))
                  (case currentChar
                    \@ (do
                          (def flag true)
                          (println "To reach the treasure; I followed these steps : " steps)
                          (true? true)
                       )
                    \# (do
                          (false? true)
                       )

                    (if(or (= \- currentChar) (= \+ currentChar)) ;; default
                      (do
                        (def currentMapRow (get mapVec i))
                        (def currentMapRow (assoc currentMapRow j \+))
                        (def mapVec (assoc mapVec i currentMapRow))
                        (if(and (= 0 i) (= 0 j))
                          (do
                          (def t1 (CheckNext i (inc j) (concat steps " right"))) 
                          (def t2 (CheckNext (inc i) j (concat steps " down")))
                            (if(and (= false t1) (= false t2))
                              (do
                                  (def currentMapRow (get mapVec i))
                                  (def currentMapRow (assoc currentMapRow j \!))
                                  (def mapVec (assoc mapVec i currentMapRow))
                                  (false? true)
                              )
                              (do ;;else
                                (or t1 t2)
                              )
                            )
                          )
                          (do
                              (if(and (= i (- rowlen 1)) (= j 0)) ;; bottom left
                                (do
                                  (def t1 (CheckNext i (inc j) (concat steps " right")))
                                  (def t2 (CheckNext (dec i), j (concat steps " up")))
                                  (if(or (= false t1) (= false t2))
                                    (do
                                        (def currentMapRow (get mapVec i))
                                        (def currentMapRow (assoc currentMapRow j \!))
                                        (def mapVec (assoc mapVec i currentMapRow))
                                        (false? true)
                                    )
                                    (do ;;else
                                      (or t1 t2)
                                    )
                                  )
                        
                                )
                                (do
                                  (if(and (= i 0) (= j (- (count (nth mapData i)) 1))) ;; top right
                                    (do
                                      (def t1 (CheckNext (inc i) j (concat steps " Down")))
                                      (def t2 (CheckNext i (dec j) (concat steps " left")))
                                      (if(or (= false t1) (= false t2))
                                        (do
                                            (def currentMapRow (get mapVec i))
                                            (def currentMapRow (assoc currentMapRow j \!))
                                            (def mapVec (assoc mapVec i currentMapRow))
                                            (false? true)
                                        )
                                        (do ;;else
                                          (or t1 t2)
                                        )
                                      )
                        
                                    )
                                    (do
                                      (if(and (= i (- rowlen 1)) (= j (- (count (nth mapData i)) 1))) ;;bottom right
                                        (do
                                          (def t1 (CheckNext (dec i), j (concat steps " up")))
                                          (def t2 (CheckNext i (dec j) (concat steps " left")))
                                          (if(or (= false t1) (= false t2))
                                            (do
                                              (def currentMapRow (get mapVec i))
                                              (def currentMapRow (assoc currentMapRow j \!))
                                              (def mapVec (assoc mapVec i currentMapRow))
                                              (false? true)
                                            )
                                            (do ;;else
                                              (or t1 t2)
                                            )
                                          ) 
                                        )
                                        (do
                                          (if(= i 0) ;;top row
                                            (do
                                                (def t1 (CheckNext (inc i) j (concat steps " Down")))
                                                (def t2 (CheckNext i (dec j) (concat steps " left")))
                                                (def t3 (CheckNext i (inc j) (concat steps " right")))
                                                (if(or (and (= false t1) (= false t2)) (and (= false t1) (= false t3)) (and (= false t2) (= false t3)))
                                                  (do
                                                    (def currentMapRow (get mapVec i))
                                                    (def currentMapRow (assoc currentMapRow j \!))
                                                    (def mapVec (assoc mapVec i currentMapRow))
                                                    (false? true)
                                                  )
                                                  (do ;;else
                                                    (or t1 t2 t3)
                                                  )
                                                )
                                              )
                                              (do 
                                                (if(= j 0) ;;left most column
                                                  (do
                                                    (def t1 (CheckNext i (inc j) (concat steps " right")))
                                                    (def t2 (CheckNext (dec i), j (concat steps " up")))
                                                    (def t3 (CheckNext (inc i) j (concat steps " Down")))
                                                    (if(or (and (= false t1) (= false t2)) (and (= false t1) (= false t3)) (and (= false t2) (= false t3)))
                                                      (do
                                                        (def currentMapRow (get mapVec i))
                                                        (def currentMapRow (assoc currentMapRow j \!))
                                                        (def mapVec (assoc mapVec i currentMapRow))
                                                        (false? true)
                                                      )
                                                      (do ;;else
                                                        (or t1 t2 t3)
                                                      )
                                                    )
                                                  )
                                                  (do
                                                    (if(= i (- rowlen 1)) ;;bottom most row
                                                    (do
                                                      (def t1 (CheckNext (dec i), j (concat steps " up")))
                                                      (def t2 (CheckNext i (dec j) (concat steps " left")))
                                                      (def t3 (CheckNext i (inc j) (concat steps " right")))
                                                      (if(or (and (= false t1) (= false t2)) (and (= false t1) (= false t3)) (and (= false t2) (= false t3)))
                                                        (do
                                                          (def currentMapRow (get mapVec i))
                                                          (def currentMapRow (assoc currentMapRow j \!))
                                                          (def mapVec (assoc mapVec i currentMapRow))
                                                          (false? true)
                                                        )
                                                        (do ;;else
                                                          (or t1 t2 t3)
                                                        )
                                                      )
                                                    )
                                                    (do 
                                                      (if(= j (- (count (nth mapData i)) 1)) ;;right most column
                                                          (do
                                                            (def t1 (CheckNext (dec i), j (concat steps " up")))
                                                            (def t2 (CheckNext i (dec j) (concat steps " left")))
                                                            (def t3 (CheckNext (inc i) j (concat steps " Down")))
                                                            (if(or (and (= false t1) (= false t2)) (and (= false t1) (= false t3)) (and (= false t2) (= false t3)))
                                                              (do
                                                                (def currentMapRow (get mapVec i))
                                                                (def currentMapRow (assoc currentMapRow j \!))
                                                                (def mapVec (assoc mapVec i currentMapRow))
                                                                (false? true)
                                                              )
                                                              (do ;;else
                                                                (or t1 t2 t3)
                                                              )
                                                            )
                                                          )
                                                          (do
                                                            (def t1 (CheckNext (dec i), j (concat steps " up")))
                                                            (def t2 (CheckNext i (inc j) (concat steps " right")))
                                                            (def t3 (CheckNext i (dec j) (concat steps " left")))
                                                            (def t4 (CheckNext (inc i) j (concat steps " Down")))
    (if(or (and (= false t1) (= false t2) (= false t3)) (and (= false t1) (= false t3)(= false t4)) (and (= false t2) (= false t3) (= false t4))(and (= false t1)(= false t4)(= false t2)))
                                                              (do
                                                                (def currentMapRow (get mapVec i))
                                                                (def currentMapRow (assoc currentMapRow j \!))
                                                                (def mapVec (assoc mapVec i currentMapRow))
                                                                (false? true)
                                                              )
                                                              (do ;;else
                                                                (or t1 t2 t3 t4)
                                                              )
                                                            )
                                                          )
                                                      )
                                                    ) 
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                      )
                      (do;;else
                        (println "Something wrong with the map")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
  
  
  

 
)



                            
                            ;; (CheckNext (inc i) j (concat steps " Down"))
                            ;; (CheckNext i (inc j) (concat steps " right"))
                            ;; (CheckNext (dec i), j (concat steps " up"))
                            ;; (CheckNext i (dec j) (concat steps " left"))