# All the code below requires a release of picolisp with support for tail calls
: (version)
25.2.8
-> (25 2 8)

# The functions below have recursive and tail call versions
# The code in each function is identical except for the use of 'recur','recurse' and 'tco','tc'
# The recursive versions run in stack issues fairly quickly and are generally quite a bit slower for large lists.

# ---------------------------------------------------------------------------------------------
# Destructive swap
# Given a cons pair and a list, swap the elements at the positions specified by the cons pair

(de swappRecur (P L)
   (when (and (pair P) (lst? L))
      (ifn (> (cdr P) (car P))
         (setq P (cons (cdr P) (car P))) )
      (let (Lst L  Cnt 1)
         (use (Lhs Rhs LhsN RhsN)
            (recur (Lst)
               (when Lst
                  (cond
                     ((= Cnt (cdr P))
                        (setq Rhs Lst)
                        (setq LhsN (car Lhs)  RhsN (car Rhs)) )
                     ((= Cnt (car P)) (setq Lhs Lst)) )
                  (when (<= Cnt (cdr P))
                     (inc 'Cnt)
                     (recurse (cdr Lst)) ) ) )
            (when (and Rhs Lhs) (set Lhs RhsN Rhs LhsN)) )
         L ) ) )
         
(de swappTco (P L)
   (when (and (pair P) (lst? L))
      (ifn (> (cdr P) (car P))
         (setq P (cons (cdr P) (car P))) )
      (let (Lst L  Cnt 1)
         (use (Lhs Rhs LhsN RhsN)
            (tco
               (Lst)
               (when Lst
                  (cond
                     ((= Cnt (cdr P))
                        (setq Rhs Lst)
                        (setq LhsN (car Lhs)  RhsN (car Rhs)) )
                     ((= Cnt (car P)) (setq Lhs Lst)) )
                  (when (<= Cnt (cdr P))
                     (inc 'Cnt)
                     (tc (cdr Lst)) ) ) )
            (when (and Rhs Lhs) (set Lhs RhsN Rhs LhsN)) )
         L ) ) )
    
# Shuffle a List    
: (bench (let (
    Func swappRecur 
    Cnt (** 2 16) 
    L (range 1 (** 2 10)) 
    Len (length L)) 
    (for N Cnt (Func (cons (rand 1 Len) (rand 1 Len)) L)) 
    (println (head 32 L)) 
    (println (tail 32 L)))) T

(458 415 417 398 609 2 221 268 951 1000 455 291 827 972 390 699 804 1022 780 28 164 343 95 63 543 437 933 351 562 706 229 347)
(593 506 350 834 348 630 832 290 68 488 635 945 855 610 618 426 158 598 326 48 274 1001 966 1003 537 293 449 847 266 133 712 248)
3.153 sec
-> T

: (bench (let (Func swappTco Cnt (** 2 16) L (range 1 (** 2 10)) Len (length L)) (for N Cnt (Func (cons (rand 1 Len) (rand 1 Len)) L)) (println
 (head 32 L)) (println (tail 32 L)))) T

(851 479 737 419 1015 805 671 968 613 304 688 804 243 50 944 17 458 189 911 892 428 376 718 972 784 321 666 626 168 630 92 520)
(393 2 639 884 703 746 728 908 494 890 47 465 869 320 853 828 910 137 978 331 411 117 72 368 7 942 537 174 245 209 337 61)
2.777 sec
-> T

# Reverse a List 
: (bench (let (Func swappTco Cnt (** 2 10) Lst (range 1 Cnt) Len (length Lst) LL 1 RR Len) 
    (while (> RR LL) (Func (cons LL RR) Lst) (inc 'LL) (dec 'RR)) 
    (println LL RR) 
    (println (head 32 Lst)) 
    (println (tail 32 Lst)))) T
513 512
(1024 1023 1022 1021 1020 1019 1018 1017 1016 1015 1014 1013 1012 1011 1010 1009 1008 1007 1006 1005 1004 1003 1002 1001 1000 999 998 997 996 995 994 993)
(32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
0.046 sec
-> T
:

# ---------------------------------------------------------------------------------------------------------------------
# Move To Front
# Move the N'th element in a list to the first

(de mtfTco1 (X N)
  #(msg N " : " (sym X))
      (make 
        (tco (X N)
          (cond 
            ((=1 N) (when X (yoke (car X)) (chain (cdr X))))
            ((> N 1) (when X (link (car X)) (tc (cdr X) (dec 'N))))
            (T X)))))

(de mtfRecur1 (X N)
  #(msg N " : " (sym X))
      (make 
        (recur (X N)
          (cond 
            ((=1 N) (when X (yoke (car X)) (chain (cdr X))))
            ((> N 1) (when X (link (car X)) (recurse (cdr X) (dec 'N))))
            (T X)))))


# ---------------------------------------------------------------------------------------------------------------------
# Move From Top
# Move the first element in a list to the N'th position

(de mftTco (X N)
      (if (or (atom X) (le0 N))
         X
         (let 
          (Lst (cdr X) 
            Cnt N
            Head (car X)
            Result (make
                (tco (Lst)
                  #(msg Head " : " Cnt " : " (sym Lst))
                    (cond
                      ((=1 Cnt) (link Head) (when Lst (chain Lst)))
                      ((> Cnt 1) (when Lst (link (car Lst))) (dec 'Cnt) (tc (cdr Lst)))
                    )))) Result )))
                      

(de mftRecur (X N)
      (if (or (atom X) (le0 N))
         X
         (let 
          (Lst (cdr X) 
            Cnt N
            Head (car X)
            Result (make
                (recur (Lst)
                  #(msg Head " : " Cnt " : " (sym Lst))
                    (cond
                      ((=1 Cnt) (link Head) (when Lst (chain Lst)))
                      ((> Cnt 1) (when Lst (link (car Lst))) (dec 'Cnt) (recurse (cdr Lst)))
                    )))) Result )))

# -------------------------------------------------                      
# Examples of mft and mtf
# Using tc only. The recurse versions work but are slower and run out of stack fairly quickly

: (gc 200 200)
-> 200

: (let (Cnt (** 2 10) L (range 1 Cnt) Len (length L) Rnd (rand 1 Len)) (bench (for N 1 (setq L (mtfTco L Rnd)))) (println Rnd " : " (head 17 L) " : " (head 3 (nth L (dec Rnd))))) T
0.000 sec
618 " : " (618 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) " : " (616 617 619)
-> T

: (let (Cnt (** 2 10) L (range 1 Cnt) Len (length L) Rnd (rand 1 Len)) (bench (for N 1 (setq L (mftTco L Rnd)))) (println Rnd " : " (head 17 L) " : " (head 3 (nth L (dec Rnd))))) T
0.000 sec
17 " : " (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 1) " : " (17 1 18)
-> T
:

# --- Shuffle a list ---
: (let (Cnt (** 2 8)
                L (range 1 Cnt)
                Len Cnt
                Rnd NIL)
                (bench (for N Cnt (setq Rnd (rand 1 Len) L (mftTco L Rnd))))
                (println Rnd ': (head 17 L) ': (head 3 (nth L (dec Rnd))))) T
0.004 sec
221 : (91 161 118 52 86 162 20 23 25 53 15 163 42 94 3 130 49) : (147 136 36)
-> T

: (let (Cnt (** 2 8)
                L (range 1 Cnt)
                Len Cnt
                Rnd NIL)
                (bench (for N Cnt (setq Rnd (rand 1 Len) L (mtfTco L Rnd))))
                (println Rnd ': (head 17 L) ': (head 3 (nth L (dec Rnd))))) T
0.005 sec
120 : (73 30 35 60 20 98 191 84 132 80 169 137 204 58 43 2 47) : (176 109 11)
-> T
:

: (let (Cnt (** 2 16) L (range 1 Cnt) Len (length L) Rnd NIL) (bench (for N Cnt (setq Rnd (rand 1 Len) L (mtfTco L Rnd)))) (println Rnd " : " (head 17 L) " : " (head 3 (nth L (dec R
nd))))) T
238.254 sec
28742 " : " (42702 1570 21161 31691 45631 35656 24178 65366 36808 33025 59108 55283 4788 1808 11877 10732 27018) " : " (8276 4513 3903)
-> T

: (let (Cnt (** 2 16) L (range 1 Cnt) Len (length L) Rnd NIL) (bench (for N Cnt (setq Rnd (rand 1 Len) L (mftTco L Rnd)))) (println Rnd " : " (head 17 L) " : " (head 3 (nth L (dec R
nd))))) T
212.558 sec
7524 " : " (40159 22317 41327 17860 41328 27087 4470 39032 41329 37544 10145 41330 41331 9719 3626 4082 41332) " : " (44129 41326 41043)
-> T
:
