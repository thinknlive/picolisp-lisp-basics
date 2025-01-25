(symbols 'ljl 'pico)

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

# Write the current (history) list to a timestamped file

(de writeHistory NIL
   (let (FileName (pack (timeStamp) "-pil-history.l"))
      (out FileName (pretty (history)))
      FileName ) )

# Read a previously saved (history) list and append to current (history) list

(de readHistory (FileName)
   (let
      (savedHistory (if (info FileName) (in FileName (read)) NIL)
         currentHistory (history) )
      (history (if savedHistory (consFlatten (cons currentHistory savedHistory)) currentHistory))
      (length savedHistory) ) )


# Generate a password of specified PLength, selecting characters at random from C1
# If SymCnt is given then, every N characters, insert a random 'comma' from C2

(de makePasswd (PLength SymCnt)
   (default PLength 7)
   (default SymCnt (inc PLength))
   (let
      (C1 (chop "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz")
         L1 (length C1)
         C2 (chop ".#+!")
         L2 (length C2) )
      (pack
         (make
            (for N PLength
               (link
                  (if (=0 (% N SymCnt))
                     (nth C2 (rand 1 L2) 1)
                     (nth C1 (rand 1 L1) 1) ) ) ) ) ) ) )
