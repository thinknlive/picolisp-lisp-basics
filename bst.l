# Binary Search Tree
# Functions to Make, Search, Traverse Binary Search Tree

: (mapc pp '(bstMake bstSearch bstInOrder bstPreOrder bstPostOrder bstLevelOrder))

# Make a binary search tree. L is an ordered list
(de bstMake (L)
   (let
      (BST
         '((L Len)
            (recur (L Len)
               (when L
                  (let
                     (Mid (/ Len 2)
                        LeftT (cut Mid 'L)
                        Root (++ L)
                        RightT L )
                     (list
                        Root
                        (recurse LeftT Mid)
                        (recurse RightT (if (gt0 Mid) (dec Mid) Mid)) ) ) ) ) ) )
      (BST L (length L)) ) )

# Use Fun to search in a binary search tree
# Fun should return 0 for match, <0, >0 otherwise

(de bstSearch (Fun Tree)
   (recur (Fun Tree)
      (cond
         ((or (not Tree) (=0 (Fun (car Tree))))
            Tree )
         ((lt0 (Fun (car Tree)))
            (recurse Fun (caddr Tree)) )
         (T (recurse Fun (cadr Tree))) ) ) )

# Traverse binary search tree in given order, calling Fun for each element

(de bstInOrder (Fun Tree)
   (recur (Fun Tree)
      (when Tree
         (if (atom Tree) (setq Tree (list Tree)))
         (recurse Fun (cadr Tree))
         (Fun (car Tree))
         (recurse Fun (caddr Tree)) ) ) )

(de bstPreOrder (Fun Tree)
   (recur (Fun Tree)
      (when Tree
         (if (atom Tree) (setq Tree (list Tree)))
         (Fun (car Tree))
         (preOrder Fun (cadr Tree))
         (preOrder Fun (caddr Tree)) ) ) )

(de bstPostOrder (Fun Tree)
   (recur (Fun Tree)
      (when Tree
         (if (atom Tree) (setq Tree (list Tree)))
         (recurse Fun (cadr Tree))
         (recurse Fun (caddr Tree))
         (Fun (car Tree)) ) ) )

(de bstLevelOrder (Fun Tree)
   (for (Q (circ Tree) Q)
      (let N (fifo 'Q)
         (if (atom N) (setq N (list N)))
         (Fun (car N))
         (when (cadr N) (fifo 'Q @))
         (when (caddr N) (fifo 'Q @)) ) ) )

# ----------------------------------------------------------------
# Examples

: (gc 200 200)
-> 200

# ------------------------------
# Make a BST with 2^20 - 1 nodes

: (bench (setq *Tree (bstMake (range 1 (dec (** 2 20)))))) T
0.319 sec
-> T
: (bench (setq *Miss (make (for C (** 2 20) (unless (bstSearch '((X) (- X C)) *Tree) (link C)))))) T
1.605 sec
-> T
: (bench (setq *Hit (make (for C (** 2 20) (when (bstSearch '((X) (- X C)) *Tree) (link C)))))) T
1.589 sec
-> T
: (length *Miss)
-> 1
: (length *Hit)
-> 1048575

# Convert tree to array (in order)
: (setq *Array (make (bstInOrder link *Tree))) T
-> T
: (head 10 *Array)
-> (1 2 3 4 5 6 7 8 9 10)
: (tail 10 *Array)
-> (1048566 1048567 1048568 1048569 1048570 1048571 1048572 1048573 1048574 1048575)

# ------------------------------
# Make a tree with 2^20 nodes
: (bench (setq *Tree (bstMake (range 1 (** 2 20))))) T
0.342 sec
-> T

# Do 10000 searches for random values in the range 1 - 2^16  (all should be found)
: (bench (let (N 0 Rnd 0) (do 10000 (setq Rnd (rand 1 (** 2 16))) (when (bstSearch '((X) (- X Rnd)) *Tree) (inc 'N))) N))
0.027 sec
-> 10000

# Do 10000 searches for random values in the range 1 - 2^32  (0 or some will be found)
: (bench (let (N 0 Rnd 0) (do 10000 (setq Rnd (rand 1 (** 2 32))) (when (bstSearch '((X) (- X Rnd)) *Tree) (inc 'N))) N))
0.018 sec
-> 3
:

# ------------------------------
# Make a tree with 2^4 - 1 nodes

: (bench (setq *Tree (bstMake (range 1 (dec (** 2 4)))))) T
0.000 sec
-> T
: (println *Tree) T
(8 (4 (2 (1 NIL NIL) (3 NIL NIL)) (6 (5 NIL NIL) (7 NIL NIL))) (12 (10 (9 NIL NIL) (11 NIL NIL)) (14 (13 NIL NIL) (15 NIL NIL))))
-> T
: (bench (setq *Miss (make (for C (** 2 4) (unless (bstSearch '((X) (- X C)) *Tree) (link C)))))) T
0.000 sec
-> T
: (bench (setq *Hit (make (for C (** 2 4) (when (bstSearch '((X) (- X C)) *Tree) (link C)))))) T
0.000 sec
-> T
: (length *Hit)
-> 15
: (length *Miss)
-> 1
: (setq *Array (make (bstInOrder link *Tree))) T
-> T
: (println *Array) T
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
-> T
: (view *Tree)
+-- 8
|
+---+-- 4
|   |
|   +---+-- 2
|   |   |
|   |   +---+-- 1
|   |   |   |
|   |   |   +-- NIL
|   |   |   |
|   |   |   +-- NIL
|   |   |
|   |   +---+-- 3
|   |       |
|   |       +-- NIL
|   |       |
|   |       +-- NIL
|   |
|   +---+-- 6
|       |
|       +---+-- 5
|       |   |
|       |   +-- NIL
|       |   |
|       |   +-- NIL
|       |
|       +---+-- 7
|           |
|           +-- NIL
|           |
|           +-- NIL
|
+---+-- 12
    |
    +---+-- 10
    |   |
    |   +---+-- 9
    |   |   |
    |   |   +-- NIL
    |   |   |
    |   |   +-- NIL
    |   |
    |   +---+-- 11
    |       |
    |       +-- NIL
    |       |
    |       +-- NIL
    |
    +---+-- 14
        |
        +---+-- 13
        |   |
        |   +-- NIL
        |   |
        |   +-- NIL
        |
        +---+-- 15
            |
            +-- NIL
            |
            +-- NIL
-> NIL
:
