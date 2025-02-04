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

# undo it
(de unshiftCar (X N)
   (if (or (le0 N) (atom X))
      X
      (let (Lhs (head N X)  Rhs (nth X (inc N)))
         (consFlatten (cons (car Rhs) Lhs (cdr Rhs))) ) ) )


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
   (default SymCnt 8)
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

# Pseudo random number generators
# (let (rndFn (randomLehmer 9999)) (do 20 (println (rndFn))))

(de randomLehmer (Seed)
      (let @Seed (or Seed (inc (abs (rand))))
         (fill
            '(()
               (job '((State . @Seed))
                  (setq State (% (* State 279470273) 4294967291)) ) ) ) ) )

(de randomParkMiller (@Seed)
   (curry
      (@Seed (State))
      NIL
      (unless State (setq State @Seed))
      (setq State (% (* State 48271) 2147483647)) ) )

# FNV Hash Curry

(de fnvHasher (Bits)
   (use (@Prime @Offset @Mask)
      (cond
         ((= Bits 32) (setq @Prime 16777619 @Offset 2166136261))
         ((= Bits 64) (setq @Prime 1099511628211 @Offset 2166136261))
         ((= Bits 128) (setq @Prime 309485009821345068724781371 @Offset 2166136261))
         ((= Bits 256) (setq @Prime 374144419156711147060143317175368453031918731002211 @Offset 100029257958052580907070968620625704837092796014241193945225284501741471925557))
         ((= Bits 512) (setq @Prime 35835915874844867368919076489095108449946327955754392558399825615420669938882575126094039892345713852759 @Offset 9659303129496669498009435400716310466090418745672637896108374329434462657994582932197716438449813051892206539805784495328239340083876191928701583869517785))
         ((= Bits 1024) (setq @Prime 5016456510113118655434598811035278955030765345404790744303017523831112055108147451509157692220295382716162651878526895249385292291816524375083746691371804094271873160484737966720260389217684476157468082573 @Offset 1419779506494762106872207064140321832088062279544193396087847491461758272325229673230371772250864096521202355549365628174669108571814760471015076148029755969804077320157692458563003215304957150157403644460363550505412711285966361610267868082893823963790439336411086884584107735010676915))
         (T (throw "fnvHasher: Pick one of 32, 64, 128, 256, 512 or 1024")) )
      (setq @Mask
         (hex (pack (need (/ Bits 4) "F"))) )
      (curry
         (@Prime @Offset @Mask)
         (Lst)
         (let (Hash @Offset)
            (mapc
               '((Val)
                  (setq Hash (x| Hash Val))
                  (setq Hash (* Hash @Prime))
                  (setq Hash (& Hash @Mask)) )
               Lst )
            Hash ) ) ) )


# Password Curry (use the functions above to generate passwords from a given 'dictionary' key
# Shuffles the provided key phrase using a prng seeded with the fnv hash of the phrase

(de passwdCurry (Dict Shuffle)
      (let
         (L (chop Dict)
            Hasher (fnvHasher 32)
            RndFn
            (randomLehmer
               (Hasher (make (for C *L (link (char C))))) ) )
         (default Shuffle (length L))
         (do Shuffle
            (setq L (shiftCar L (% (RndFn) (length L)))) )
         (pack L) ) )

#{
Example:
: (ljl~passwdCurry "1234567890JQKA")
-> "78904JQK13265A"
: (ljl~passwdCurry "1234567890JQKA" 97)
-> "46J78129A53K0Q"
: (ljl~passwdCurry "TheFiveBoxingWizardsJumpQuickly")
-> "uneFmvzpQioiuTeicBxWiJaklsdyhrg"
: (ljl~passwdCurry "TheFiveBoxingWizardsJumpQuickly.." 97)
-> "iuzorexuiB.mTyQdshWklFnJecpa.viig"
: (ljl~passwdCurry "0123456789ABCDEF")
-> "718694A0BC52DE3F"
: (ljl~passwdCurry "0123456789ABCDEF" 73)
-> "8E42C10B9D36F75A"
:
}#

# ShiftN and UnshiftN...
# Keep track of the shifts made and return the stack so we can 'unshift'

(de passwdCurry (Dict Shuffle)
   (let
      (L (chop Dict)
         Hasher (fnvHasher 32)
         RndFn
         (randomLehmer
            (Hasher (make (for C *L (link (char C))))) )
         Hist NIL
         Val NIL )
      (default Shuffle (length L))
      (do Shuffle
         (setq Val (% (RndFn) (length L)))
         (push 'Hist Val)
         (setq L (shiftCar L Val)) )
      (cons (pack L) Hist) ) )

: (setq Curry (passwdCurry "abcdef0123456789ABCDEF" 37) 
         Pwd (chop (car Curry)) 
         Hst (cdr Curry)) 
   (prinl (pack Pwd)) 
   (prinl (pack (for C Hst (setq Pwd (unshiftCar Pwd C)))))

d0Cb7c3A8D6a21fE9e5BF4
abcdef0123456789ABCDEF
-> "abcdef0123456789ABCDEF"
:
