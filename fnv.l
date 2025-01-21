#{
Function to implement fnv-1a hash for 32,64,128,256,512,1024
TestHashIdx and TestHashEnum exercise the hash using (idx) or (enum) respectively to report collisions

TODO: Add an option for 'folding' per RFC for better avalanche behavior.

Example use

: (bench (TestHashEnum (** 2 16) 12 32))
1.134 sec
-> (65536 . 0)
: (bench (TestHashEnum (** 2 16) 12 32))
1.129 sec
-> (65536 . 0)
: (bench (TestHashEnum (** 2 16) 12 32))
> Collision: 31711; 3176390521; (6608 3176390521 (112 248 24 104 173 15 92 206 56 111 107 84)); (197 219 102 22 135 100 62 183 184 134 69 245)
1.125 sec
-> (65536 . 1)
: (bench (TestHashIdx (** 2 16) 12 32))
> Collision: 49874; 4261145272; (4261145272 17444 (218 67 162 28 31 114 41 80 188 242 205 61)); (153 79 240 103 130 79 156 5 58 18 69 159)
> Collision: 51057; 519297767; (519297767 27769 (130 144 17 4 203 130 141 157 162 9 122 231)); (148 21 16 191 207 232 210 145 54 167 36 219)
1.235 sec
-> (65536 . 2)
: (bench (TestHashIdx (** 2 16) 12 32))
> Collision: 61721; 2951649830; (2951649830 31208 (56 26 212 173 188 33 186 56 65 115 250 35)); (24 203 192 164 178 242 115 66 165 64 58 124)
1.129 sec
-> (65536 . 1)
: (bench (TestHashIdx (** 2 16) 12 32))
> Collision: 65411; 3517629964; (3517629964 55220 (83 215 179 5 209 75 55 35 99 44 38 148)); (89 70 240 146 105 190 248 35 84 186 234 34)
1.169 sec
-> (65536 . 1)
: (bench (TestHashIdx (** 2 16) 12 32))
1.161 sec
-> (65536 . 0)
:
: (bench (TestHashIdx (** 2 16) 12 64))
1.258 sec
-> (65536 . 0)
: (bench (TestHashEnum (** 2 16) 12 64))
1.175 sec
-> (65536 . 0)
:

: (mapc pp '(fnvHasher makeRandomList TestHashEnum TestHashIdx))

}#

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

(de makeRandomList (N)
   (in "/dev/urandom" (make (do N (link (rd 1))))) )

(de TestHashEnum (N LstCnt Bits)
   (off *HT)
   (default LstCnt 17)
   (default Bits 32)
   (let
      (Dups 0
         Val NIL
         Lst NIL
         CurVal NIL
         Hasher (fnvHasher Bits)
         SmallNumberMask
         (hex
            (pack (need (- (/ 64 4) 1) "F")) ) )
      (for I N
         (setq Lst (makeRandomList LstCnt))
         (setq Val (& (Hasher Lst) SmallNumberMask))
         (setq CurVal (val (enum? *HT Val)))
         (cond
            ((and CurVal (<> (caddr CurVal) Lst))
               (inc 'Dups)
               (prinl
                  "> Collision: "
                  I
                  "; "
                  Val
                  "; "
                  (sym (val (enum? *HT Val)))
                  "; "
                  (sym Lst) ) )
            (T (set (enum '*HT Val) (list I Val Lst))) ) )
      (cons N . Dups) ) )

(de TestHashIdx (N LstCnt Bits)
   (off *HT)
   (default LstCnt 17)
   (default Bits 32)
   (let (Dups 0  Val NIL  Lst NIL  CurVal NIL  Hasher (fnvHasher Bits))
      (for I N
         (setq Lst (makeRandomList LstCnt))
         (setq Val (Hasher Lst))
         (setq CurVal (lup *HT Val))
         (cond
            ((and CurVal (<> (caddr CurVal) Lst))
               (inc 'Dups)
               (prinl
                  "> Collision: "
                  I
                  "; "
                  Val
                  "; "
                  (sym (lup *HT Val))
                  "; "
                  (sym Lst) ) )
            (T (idx '*HT (list Val I Lst) T)) ) )
      (cons N . Dups) ) )
