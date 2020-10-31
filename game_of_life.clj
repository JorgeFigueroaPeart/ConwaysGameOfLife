;Initial parameters
(def m 10) ; Size of the world is m x m
(def n 5)  ; Number of generations that will be calculated and printed

;These functions will print the matrix
(defn printArr [arr]
    (loop [x 0]
        (if(< x m)
            (do (print (get arr x) " ")(recur (+ x 1)))
         )
     )
    (println)
)
(defn printMat [mat gen]
    (println "Generation:" gen)
    (loop [x 0]
        (if(< x m)
            (do 
                (printArr (get mat x))
                (recur (+ x 1))
            )
         )
     )
)

;Inserts and element in an x y position in the matrix
(defn insertOnMat [mat x y]
    (assoc mat x (assoc (get mat x) y 1))
)

;Here we can establish generation zero, by adding cells where we need them
(def matrix (vec (replicate m (vec (replicate m 0)))))
(def matrix (insertOnMat matrix 0 1))
(def matrix (insertOnMat matrix 1 2))
(def matrix (insertOnMat matrix 2 0))
(def matrix (insertOnMat matrix 2 1))
(def matrix (insertOnMat matrix 2 2))
(printMat matrix 0)

;Does the all around checking over a single cell
(defn checkCell [mat x y]
    (def i 0)
    (if (> x 0)
        (do
            (if (> y 0)
                (do
                    (if (= 1 (get (get mat (- x 1)) (- y 1)))
                        (def i (+ i 1)) ;Top Left
                    )
                )     
            )
            (if (< y m)
                (if (= 1 (get (get mat (- x 1)) (+ y 1)))
                    (def i (+ i 1)) ;Top Right
                )
            )
            (if (= 1 (get (get mat (- x 1)) y))
                (def i (+ i 1)) ;Top Mid
            )
        )
     )
    (if (> y 0)
        (do
            (if (< x m)
                (if (= 1 (get (get mat (+ x 1)) (- y 1)))
                    (def i (+ i 1)) ;Bot Left
                )
            )
            (if (= 1 (get (get mat x) (- y 1)))
                (def i (+ i 1)) ;Mid Left
            )
        )
    )
    (if (< x m)
        (do
            (if (< y m)
                (if (= 1 (get (get mat (+ x 1)) (+ y 1)))
                    (def i (+ i 1)) ;Bot Right
                )
            )
            (if (= 1 (get (get mat (+ x 1)) y ))
                (def i (+ i 1)) ;Bot Mid
            )
        )
    )
    (if (< y m)
        (if (= 1 (get (get mat  x) (+ y 1) ))
                (def i (+ i 1)) ;Mid Right
        )
    )
    (if (< i 2)
        0
        (if (and (= i 3) (= (get (get mat x) y) 0))
            1
            (if (> i 3)
                0
                (if (and (or (= i 2) (= i 3)) (= (get (get mat x) y) 1))
                    1
                    0
                )
            )
        )         
     )
 )

;iterates through the whole matrix and checks every cell
(defn checkLine [mat line x]
    (loop [y 0 newMat [] ]
        (if(< y m)
            (do 
                (recur (+ y 1) (conj newMat (checkCell mat x y)))
            )
            newMat
         )
     )
    
 )
(defn newGen [mat]
    (loop [x 0 newMat [] ]
        (if(< x m)
            (do 
                (recur (+ x 1) (conj newMat (checkLine mat (get mat x) x)))
            )
            newMat
         )
     )
)

;Main, this will loop every generation needed and print them
(def currGen matrix)
(printMat matrix 0)
(loop [i 1]
   (if (<= i n)
       (do
           (def currGen (newGen currGen))
           (printMat currGen i)
           (recur (+ i 1))
        )
    )
)
"Simulation over"
