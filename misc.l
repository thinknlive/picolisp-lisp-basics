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

# Shuffle and undo by prime number of times

: (setq Primes (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997))
: (for N Primes 
      (setq Curry (passwdCurry "abcdefghijklmnopqrstuvwxyz0123456789" N) 
            Pwd (chop (car Curry)) 
            Hst (cdr Curry)) 
      (prin (pad 4 N) ": " (pack Pwd) " --> ") 
      (prinl (pack (for C Hst (setq Pwd (unshiftCar Pwd C))))))

0002: cdebfghijklmnopqrstauvwxyz0123456789 --> abcdefghijklmnopqrstuvwxyz0123456789
0003: debfghijklmcnopqrstauvwxyz0123456789 --> abcdefghijklmnopqrstuvwxyz0123456789
0005: bfghijklmcnoepqrstaudvwxyz0123456789 --> abcdefghijklmnopqrstuvwxyz0123456789
0007: ghijklmcnoepqrsbtafudvwxyz0123456789 --> abcdefghijklmnopqrstuvwxyz0123456789
0011: klmcngoepqhrsbtafiudvwxyz01234567j89 --> abcdefghijklmnopqrstuvwxyz0123456789
0013: mcngoepkqhrsbtafiludvwxyz01234567j89 --> abcdefghijklmnopqrstuvwxyz0123456789
0017: oepkmqchrsbtnafiludvwxyz0g1234567j89 --> abcdefghijklmnopqrstuvwxyz0123456789
0019: pkmqcehrsbtnoafiludvwxyz0g1234567j89 --> abcdefghijklmnopqrstuvwxyz0123456789
0023: cehrpsbtnoafiludvwxyz0g123q4567j89mk --> abcdefghijklmnopqrstuvwxyz0123456789
0029: sbtnoafpiludvwxyz0ge123hq456r7cj89mk --> abcdefghijklmnopqrstuvwxyz0123456789
0031: tnoafpiludvwxyz0gbe123hsq456r7cj89mk --> abcdefghijklmnopqrstuvwxyz0123456789
0037: pilaudvwxyz0ngbe12f3hsq456r7cjo8t9mk --> abcdefghijklmnopqrstuvwxyz0123456789
0041: uldvwxyz0ngbe12fp3hsq45a6r7cjo8t9mik --> abcdefghijklmnopqrstuvwxyz0123456789
0043: dvwxyz0ungbe12fp3hsql45a6r7cjo8t9mik --> abcdefghijklmnopqrstuvwxyz0123456789
0047: xyzw0ungbe12fpv3hsql45da6r7cjo8t9mik --> abcdefghijklmnopqrstuvwxyz0123456789
0053: ungbe12fpv3hzsqly045da6rw7cjox8t9mik --> abcdefghijklmnopqrstuvwxyz0123456789
0059: 12fpv3hzsqly0u45da6rw7cbnjox8tg9mike --> abcdefghijklmnopqrstuvwxyz0123456789
0061: fpv3hzs1qly0u452da6rw7cbnjox8tg9mike --> abcdefghijklmnopqrstuvwxyz0123456789
0067: s1qly0zu452dva6prw7cbnfjo3x8tg9mikhe --> abcdefghijklmnopqrstuvwxyz0123456789
0071: y0zu4152dvas6qprw7clbnfjo3x8tg9mikhe --> abcdefghijklmnopqrstuvwxyz0123456789
0073: zu4152dva0s6qyprw7clbnfjo3x8tg9mikhe --> abcdefghijklmnopqrstuvwxyz0123456789
0079: dva0s6zq2yp4urw71clbnf5jo3x8tg9mikhe --> abcdefghijklmnopqrstuvwxyz0123456789
0083: s60zqv2ypd4urw71clbnf5jo3x8tg9amikhe --> abcdefghijklmnopqrstuvwxyz0123456789
0089: 2ypd4surw7q1zclbnf5jo3xv8t6g9ami0khe --> abcdefghijklmnopqrstuvwxyz0123456789
0097: w7q14zclbnf5rjo23xvsuy8t6g9dapmi0khe --> abcdefghijklmnopqrstuvwxyz0123456789
0101: 4z1clbnf5rjow23xvsuq7y8t6g9dapmi0khe --> abcdefghijklmnopqrstuvwxyz0123456789
0103: 1clbnzf5rjow23xvsuq7y8t6g9dapmi04khe --> abcdefghijklmnopqrstuvwxyz0123456789
0107: nzf5crjowlb23xvsu1q7y8t6g9dapmi04khe --> abcdefghijklmnopqrstuvwxyz0123456789
0109: f5crjowlb23xvzsu1q7y8t6gn9dapmi04khe --> abcdefghijklmnopqrstuvwxyz0123456789
0113: jowlb23xvzsu1q7y8f5t6gn9dapmi0c4kher --> abcdefghijklmnopqrstuvwxyz0123456789
0127: 2u1q7by8vfw5t6g3xn9daospmzi0c4khjelr --> abcdefghijklmnopqrstuvwxyz0123456789
0131: 72by81vfw5t6g3xn9daoqspmzui0c4khjelr --> abcdefghijklmnopqrstuvwxyz0123456789
0137: vfw5t67g3xn9dao81bqspmzui02c4ykhjelr --> abcdefghijklmnopqrstuvwxyz0123456789
0139: w5t6f7g3xn9dao81bqspmzui0v2c4ykhjelr --> abcdefghijklmnopqrstuvwxyz0123456789
0149: xn9dao781btqspm6zfui0vw2c4ykg3hjel5r --> abcdefghijklmnopqrstuvwxyz0123456789
0151: 9dao781btqspm6nzfuxi0vw2c4ykg3hjel5r --> abcdefghijklmnopqrstuvwxyz0123456789
0157: 891btqspm6nzfuxi07vw2c4ykg3hjeld5roa --> abcdefghijklmnopqrstuvwxyz0123456789
0163: spm68n1zfbuxi07vw2c4ykg3hjel9d5rqota --> abcdefghijklmnopqrstuvwxyz0123456789
0167: 8sn1zfmbuxi07vw2c4ypkg36hjel9d5rqota --> abcdefghijklmnopqrstuvwxyz0123456789
0173: mbzuxi07svw2nc4ypkg361hjefl9d85rqota --> abcdefghijklmnopqrstuvwxyz0123456789
0179: 07svw2nc4ypzbkg36x1hjeufl9d85rqoimta --> abcdefghijklmnopqrstuvwxyz0123456789
0181: svw2nc4ypzbkg36x1hj7eufl9d85r0qoimta --> abcdefghijklmnopqrstuvwxyz0123456789
0191: ypzb4kg36x1hj7eufl9d2s8c5vr0qnoimtwa --> abcdefghijklmnopqrstuvwxyz0123456789
0193: zb4kyg36x1hj7eufl9d2s8c5vr0qnoimtpwa --> abcdefghijklmnopqrstuvwxyz0123456789
0197: yg3k6xb1hzj7eufl9d2s8c5vr40qnoimtpwa --> abcdefghijklmnopqrstuvwxyz0123456789
0199: 3k6xyb1hzjg7eufl9d2s8c5vr40qnoimtpwa --> abcdefghijklmnopqrstuvwxyz0123456789
0211: 6bg7eufl91d2ys8c5vjr40qhnoi3mzxktpwa --> abcdefghijklmnopqrstuvwxyz0123456789
0223: dgl2yse8fc65uvjr40qhno1i3mzxbktp9w7a --> abcdefghijklmnopqrstuvwxyz0123456789
0227: yse8fcd6l5uvjr40qh2no1i3mzxbktpg9w7a --> abcdefghijklmnopqrstuvwxyz0123456789
0229: e8fcd6l5uvsyjr40qh2no1i3mzxbktpg9w7a --> abcdefghijklmnopqrstuvwxyz0123456789
0233: d6cl5uvsyjr40qh2nfo1i3mzxbk8tpeg9w7a --> abcdefghijklmnopqrstuvwxyz0123456789
0239: 5uvsycjr40qh2nfdol1i3mzxbk8tpeg9w67a --> abcdefghijklmnopqrstuvwxyz0123456789
0241: vs5yucjr40qh2nfdol1i3mzxbk8tpeg9w67a --> abcdefghijklmnopqrstuvwxyz0123456789
0251: 0q4h2ncfdoyl1ui53vmzxbk8tpergj9ws67a --> abcdefghijklmnopqrstuvwxyz0123456789
0257: cfd0oylq1ui53vm4zxbk28tpergj9nws6h7a --> abcdefghijklmnopqrstuvwxyz0123456789
0263: flq1oui5c3vm4z0xbk28tpergj9dnws6hy7a --> abcdefghijklmnopqrstuvwxyz0123456789
0269: uli5c3vm4z0xbk28tpqeorgj9dn1ws6hy7af --> abcdefghijklmnopqrstuvwxyz0123456789
0271: i5c3vm4z0xubk28tpqleorgj9dn1ws6hy7af --> abcdefghijklmnopqrstuvwxyz0123456789
0277: 4z0xubkc28tmpqileorgj59dn1ws6hy7va3f --> abcdefghijklmnopqrstuvwxyz0123456789
0281: ubkc28txmpqileorgj4590dnz1ws6hy7va3f --> abcdefghijklmnopqrstuvwxyz0123456789
0283: kc28btxmpqileorugj4590dnz1ws6hy7va3f --> abcdefghijklmnopqrstuvwxyz0123456789
0293: q2ilmteorugj458k90dnpxzc1ws6hby7va3f --> abcdefghijklmnopqrstuvwxyz0123456789
0307: j458ik9q0rdnupxzc1wgs62htby7valme3of --> abcdefghijklmnopqrstuvwxyz0123456789
0311: ijk9q0rd5nupxzc1w8gs62ht4by7valme3of --> abcdefghijklmnopqrstuvwxyz0123456789
0313: k9q0rd5nuipxzc1w8gsj62ht4by7valme3of --> abcdefghijklmnopqrstuvwxyz0123456789
0317: rdq5nuipxzc1w8gsj62ht4by70vkalme93of --> abcdefghijklmnopqrstuvwxyz0123456789
0331: c1w8gsj62ht45byu70vxkdralimez93qnopf --> abcdefghijklmnopqrstuvwxyz0123456789
0337: g1csj62ht45byu78w0vxkdralimez93qnopf --> abcdefghijklmnopqrstuvwxyz0123456789
0347: 45byu78w0vghx6ksdrjal1imec2z93qtnopf --> abcdefghijklmnopqrstuvwxyz0123456789
0349: byu78w0vg54hx6ksdrjal1imec2z93qtnopf --> abcdefghijklmnopqrstuvwxyz0123456789
0353: 8w0vgb54hxu76ksdrjal1imec2z93qtnyopf --> abcdefghijklmnopqrstuvwxyz0123456789
0359: b54hx0u768ksdrjal1imwegc2z93vqtnyopf --> abcdefghijklmnopqrstuvwxyz0123456789
0367: 7h68ksdrj5al104imwexgc2z93uvqtnyopfb --> abcdefghijklmnopqrstuvwxyz0123456789
0373: d6rj5ahl1074kimwexgc2z983uvqtsnyopfb --> abcdefghijklmnopqrstuvwxyz0123456789
0379: ahl10r674kimwexgc2z983u5vqtsnyjdopfb --> abcdefghijklmnopqrstuvwxyz0123456789
0383: 0r674kimwexagc2z9h183ul5vqtsnyjdopfb --> abcdefghijklmnopqrstuvwxyz0123456789
0389: 7i6mwexagc2z9h01843ul5vqtksnyjdorpfb --> abcdefghijklmnopqrstuvwxyz0123456789
0397: x7amgwc2zi9h01843ul5vqteksn6yjdorpfb --> abcdefghijklmnopqrstuvwxyz0123456789
0401: gwc2zi9h01843ul5avqxteksn67yjmdorpfb --> abcdefghijklmnopqrstuvwxyz0123456789
0409: 9h01843uli5avq2xtcekgsnz67yjmdorpfwb --> abcdefghijklmnopqrstuvwxyz0123456789
0419: l05a3vq2uxtcekgsnz16h7yjmd4orp98ifwb --> abcdefghijklmnopqrstuvwxyz0123456789
0421: 05a3vq2uxltcekgsnz16h7yjmd4orp98ifwb --> abcdefghijklmnopqrstuvwxyz0123456789
0431: ltvc3ekugs5n0z16h7yjmd24aorqp98ifwbx --> abcdefghijklmnopqrstuvwxyz0123456789
0433: vc3ekugs5n0tz16h7yjmd24aorqp98ifwlbx --> abcdefghijklmnopqrstuvwxyz0123456789
0439: gs5n0tz16h7yjc3evmd24akorqup98ifwlbx --> abcdefghijklmnopqrstuvwxyz0123456789
0443: 0tzn16h7yjc3evmd24akorg5qups98ifwlbx --> abcdefghijklmnopqrstuvwxyz0123456789
0449: h7yjc3zenv0md24ako1rg5qups98if6wlbxt --> abcdefghijklmnopqrstuvwxyz0123456789
0457: jzenv0md24hako1rg5qu3yps98cif6w7lbxt --> abcdefghijklmnopqrstuvwxyz0123456789
0461: ve0md24hakozn1rg5qu3yps98jcif6w7lbxt --> abcdefghijklmnopqrstuvwxyz0123456789
0463: 0md24hakozn1rg5qu3yps9v8jceif6w7lbxt --> abcdefghijklmnopqrstuvwxyz0123456789
0467: 4hakozn1rgd5qu3ymps9v08jceif6w7lbx2t --> abcdefghijklmnopqrstuvwxyz0123456789
0479: d5oqu3ymps9nzv048jcheirkf6agw7l1bx2t --> abcdefghijklmnopqrstuvwxyz0123456789
0487: ps9n5zvm3048jcheirkyfu6agdw7oql1bx2t --> abcdefghijklmnopqrstuvwxyz0123456789
0491: 5zvm30p48jcheirkyfu6nagdw7oql91bxs2t --> abcdefghijklmnopqrstuvwxyz0123456789
0499: p48jcheir3kyzfu56nagdw7voq0l91bxs2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0503: cheir3kyzfpu56najgdw78vo4q0l91bxs2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0509: 3kyezfpu5h6nacjgdw78vro4q0li91bxs2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0521: h6nacjgdwpky78vzruoe4q053li91bfxs2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0523: nacjgdw6pky78vzruoe4qh053li91bfxs2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0541: rug7nzoe68k4qhp05yc3lwvid9j1bfxsa2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0547: zo7e68k4qhp05yc3lwviud9j1bfxsangr2tm --> abcdefghijklmnopqrstuvwxyz0123456789
0557: p0e5y8c3lwvihkud69j174bfxsangr2toqmz --> abcdefghijklmnopqrstuvwxyz0123456789
0563: 8cye3lwvihkud69j174bfxsp5a0ngr2toqmz --> abcdefghijklmnopqrstuvwxyz0123456789
0569: wvihkud69jl174bfxs8ypc5a0negr2toq3mz --> abcdefghijklmnopqrstuvwxyz0123456789
0571: ihkud69jl17v4bfxws8ypc5a0negr2toq3mz --> abcdefghijklmnopqrstuvwxyz0123456789
0577: 69jli17v4kbfxws8ypcu5a0negr2dhtoq3mz --> abcdefghijklmnopqrstuvwxyz0123456789
0587: kvbfixw6js8ypcu54a01negr2d9ht7loq3mz --> abcdefghijklmnopqrstuvwxyz0123456789
0593: w6jivs8ypcu54a01xnegr2d9ht7lkobq3mzf --> abcdefghijklmnopqrstuvwxyz0123456789
0599: 8yipcu5s4a01xnevgjwr2d9ht7lko6bq3mzf --> abcdefghijklmnopqrstuvwxyz0123456789
0601: ipc8u5s4a01xnevgjwr2d9ht7ylko6bq3mzf --> abcdefghijklmnopqrstuvwxyz0123456789
0607: 85s4ai01xnevgjwr2d9ht7ylkco6bqu3mpzf --> abcdefghijklmnopqrstuvwxyz0123456789
0613: 401xnevgjawr2d9ht75ylskico6bqu38mpzf --> abcdefghijklmnopqrstuvwxyz0123456789
0617: nevgjawr2d49ht75ylskico6bq01u3x8mpzf --> abcdefghijklmnopqrstuvwxyz0123456789
0619: vgjnawr2d49het75ylskico6bq01u3x8mpzf --> abcdefghijklmnopqrstuvwxyz0123456789
0631: 49het75ylskico6wabqj0g1uv3x82mpdzfnr --> abcdefghijklmnopqrstuvwxyz0123456789
0641: kico6w4albqj0tg91uh5ysv3ex872mpdzfnr --> abcdefghijklmnopqrstuvwxyz0123456789
0643: co6w4albqj0tg91uh5ysv3iexk872mpdzfnr --> abcdefghijklmnopqrstuvwxyz0123456789
0647: 4albqj0tg91uh5ysv3iexko8c72mpdzfw6nr --> abcdefghijklmnopqrstuvwxyz0123456789
0653: 0tg91uh5ysv3iexkao48c72mpqdzbfljw6nr --> abcdefghijklmnopqrstuvwxyz0123456789
0659: uh5ysv3iextkao498c72gmp0qdzbf1ljw6nr --> abcdefghijklmnopqrstuvwxyz0123456789
0661: 5ysv3iexthkao498c72gmp0qdzbf1ljw6nur --> abcdefghijklmnopqrstuvwxyz0123456789
0673: aekov49i8c372gmtp0qd5zxbsfy1ljw6nhur --> abcdefghijklmnopqrstuvwxyz0123456789
0677: v49i8ec372gmtp0qd5zxbksfoy1ljw6nhura --> abcdefghijklmnopqrstuvwxyz0123456789
0683: c372gmtp0qd5zxbksfoy1ljv4we68nhui9ra --> abcdefghijklmnopqrstuvwxyz0123456789
0691: m0qd5tzxb3k7scfoy12ljv4we68nghui9rpa --> abcdefghijklmnopqrstuvwxyz0123456789
0701: k0d7scfoty312bljvx4wem6z8ng5hui9rpaq --> abcdefghijklmnopqrstuvwxyz0123456789
0709: ty312bk70ljcvx4wdesm6z8ngo5hfui9rpaq --> abcdefghijklmnopqrstuvwxyz0123456789
0719: lkjcvxt4wdesm260yz8n1bgo357hfui9rpaq --> abcdefghijklmnopqrstuvwxyz0123456789
0727: t4wdesm26v0yz8jn1bxkgo3c57hfui9lrpaq --> abcdefghijklmnopqrstuvwxyz0123456789
0733: sm26v0yzd8jn1bxktgwo3c57hfui49lrepaq --> abcdefghijklmnopqrstuvwxyz0123456789
0739: yzd8jn1bxk6tgwo3c57hfui49mlrve2paq0s --> abcdefghijklmnopqrstuvwxyz0123456789
0743: jn1bxk86tzgwo3c5y7hfuid49mlrve2paq0s --> abcdefghijklmnopqrstuvwxyz0123456789
0751: tzgwo83c5y17hfbnuid49mlrvej2pa6xkq0s --> abcdefghijklmnopqrstuvwxyz0123456789
0757: 3c5y17hfbnuzid49mlrvegj2wpa86xkoqt0s --> abcdefghijklmnopqrstuvwxyz0123456789
0761: 17hfbnuzcid49mlrv3yegj2wpa86xkoqt0s5 --> abcdefghijklmnopqrstuvwxyz0123456789
0769: cid479mhlrv3yfebguj2wpa1n86xkoqt0sz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0773: 79mhldrv3yfebgujc2wpa1n86xkoqt04siz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0787: bgujdcev92fwphra1n78y6xk3olmqt04siz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0797: g92dfbwphra1n78y6xk3olmqtuc0ejv4siz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0809: ra1n7h298y6xkg3olmqtwuc0ejv4pbsfidz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0811: 1n7h29r8y6xkga3olmqtwuc0ejv4pbsfidz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0821: 6x1kgra3olh2mq9twuc0ejvn84pbysf7idz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0823: 1xkgra3olh2mq9tw6uc0ejvn84pbysf7idz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0827: gra3oklh12mq9tw6uc0ejvn84pbxysf7idz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0829: a3oklh12mqrg9tw6uc0ejvn84pbxysf7idz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0839: oqrg9twm6auc10ejvn83k4h2pbxysfl7idz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0853: 1go0qecjvn83kr4hm62pbxya9sfl7tiwduz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0857: o0qecjv1n83kr4hm62pbxya9sfl7tiwgduz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0859: qecjv1n83kr4hm62pbxyoa9sfl7tiwg0duz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0863: v1n83qkjr4hm62epbxycoa9sfl7tiwg0duz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0877: 62epbxy43coa9nsflk7jtihqw81gm0druvz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0881: bxy43coa9nepsflk7j2tihqw81g6m0druvz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0883: y43bcoa9nepsflk7j2tixhqw81g6m0druvz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0887: c3oab9nepsflk7j24tixhqw81g6m0dryuvz5 --> abcdefghijklmnopqrstuvwxyz0123456789
0907: 24tix3hqw8np17jgob6m0dryekulvzac95fs --> abcdefghijklmnopqrstuvwxyz0123456789
0911: x3hqw8npt17jgo4b6m0diryekulvza2c95fs --> abcdefghijklmnopqrstuvwxyz0123456789
0919: pt17jgon4b6m03qdiryekhulvza2wxc958fs --> abcdefghijklmnopqrstuvwxyz0123456789
0929: gj6m03nqtdobiry7ekhulvza2wpxc94518fs --> abcdefghijklmnopqrstuvwxyz0123456789
0937: tmdobiry7e3khulvza2gqwpx0c9nj4518fs6 --> abcdefghijklmnopqrstuvwxyz0123456789
0941: dmbiry7oe3khtulvza2gqwpx0c9nj4518fs6 --> abcdefghijklmnopqrstuvwxyz0123456789
0947: 7oe3khtulvyza2mgqwpx0c9rnijd4b518fs6 --> abcdefghijklmnopqrstuvwxyz0123456789
0953: tuklvyoza23megqwpx0c9rnijd4bh5187fs6 --> abcdefghijklmnopqrstuvwxyz0123456789
0967: eglovtqwkpx0c9rznij3ad24bhyum5187fs6 --> abcdefghijklmnopqrstuvwxyz0123456789
0971: vtqlwkpx0c9rznoiej3ad24bhyum5187fsg6 --> abcdefghijklmnopqrstuvwxyz0123456789
0977: px0c9rznvoiejt3adw24bhqyuml51k87fsg6 --> abcdefghijklmnopqrstuvwxyz0123456789
0983: rznvoiecjt3adw24b0hqyuml5p1k8x7fsg69 --> abcdefghijklmnopqrstuvwxyz0123456789
0991: jt3adw24b0hqoyuml5pvcez1k8rx7fisg69n --> abcdefghijklmnopqrstuvwxyz0123456789
0997: 24b0thqoyuml5pvcezd1ak8rx73wfisgj69n --> abcdefghijklmnopqrstuvwxyz0123456789
