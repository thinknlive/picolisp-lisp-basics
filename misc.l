# Given a list move the first element of the list (car) N positions into the list
# (setq X '(* A B C D))
# (shiftCar X 1)
# -> (A * B C D)

(de shiftCar (X N)
   (let L
      (ifn (or (>= 0 N) (atom X))
         (cons
            (cadr X)
            (shiftCar (cons (car X) (cddr X)) (dec N)) )
         X )
      L ) )

# flatten a list structure (remove all nesting)
# (consFlatten (cons '(A B C) '(D E F (G H I) J K)) )
# -> (A B C D E F G H I J K)

(de consFlatten (L)
   (let
      (Flatten
         '((L)
            (if (atom L)
               (link L)
               (mapcar '((E) (Flatten E)) L) ) ) )
      (make (Flatten L)) ) )

# Generate a UTC datetime string in format of yymmdd-hhmmss
# 2024-12-31 01:25:37 -> 20241231-012537

(de timeStamp NIL
   (pack
      (mapcar
         '((X)
            (if (num? (format X))
               (format X)
               (if (= " " X) "-" NIL) ) )
         (chop (stamp T)) ) ) )
