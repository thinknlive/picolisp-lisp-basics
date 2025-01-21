
# Compress the current input stream using the lzw algorithm and return a list of encoded data

#{
Requires (fnvHash)
The functions use (idx) to store code, indexed by their hash. 
An (assoc) list is used to manage any hash collisions

Example use:

: (out "compressed.dat" (mapc pr (in "20250121-012422-pil-history.l" (lzwCompress))))

: (out "original.dat" (mapc '((X) (wr X)) (in "compressed.dat" (lzwDecompress))))

: (call "md5sum" "20250121-012422-pil-history.l" "original.dat")
8f6d6f5ef2a8dc0b66b697fb508fda4e  20250121-012422-pil-history.l
8f6d6f5ef2a8dc0b66b697fb508fda4e  original.dat

{#


(de lzwCompress NIL
   (setq *LZWDict NIL)
   (let
      (Codes 256
         Hash (fnvHasher 64)
         MakeHashKey
         '((Val)
            (Hash (if (lst? Val) Val (list Val))) )
         LzwDictLookup
         '((Key CountIt)
            (let
               (Code NIL
                  HashKey (MakeHashKey Key)
                  Entry (lup *LZWDict HashKey)
                  Words NIL
                  Elt NIL )
               (when Entry
                  (setq Words (cadr @))
                  (when (assoc Key Words)
                     (setq Elt @)
                     (setq Code (cadr Elt))
                     (if CountIt (inc (nth Elt 3))) ) )
               (when NIL
                  (msg
                     '?
                     " Key "
                     (sym Key)
                     "; Entry: "
                     (sym Entry)
                     "; Words: "
                     (sym Words)
                     "; Lookup: "
                     (sym (assoc Key Words))
                     "; Code: "
                     Code ) )
               Code ) )
         LzwDictUpdate
         '((Key Code)
            (use (Entry Words Word HashKey)
               (setq HashKey (MakeHashKey Key))
               (setq Word (list Key Code 0))
               (setq Words (list Word))
               (setq Entry (list HashKey Words))
               (cond
                  ((lup *LZWDict HashKey)
                     (setq Words (cdr @))
                     (ifn (assoc Key Words) (push Words Word))
                     (setq Entry (lup *LZWDict HashKey))
                     (when NIL
                        (msg
                           '+
                           " Key:"
                           (sym Key)
                           "; Code: "
                           Code
                           "; Entry: "
                           (sym Entry) ) ) )
                  (T (idx '*LZWDict Entry T)
                     (when T
                        (when NIL
                           (msg
                              '>
                              " Key: "
                              (sym Key)
                              "; Code: "
                              Code
                              "; Entry: "
                              (sym Entry) ) ) ) ) ) ) ) )
      (use (Ndx Entry Words Word Key HashKey)
         (setq Ndx Codes)
         (while (gt0 Ndx)
            (dec 'Ndx)
            (setq Key (list Ndx))
            (LzwDictUpdate Key Ndx) ) )
      (msg '> " Codes: " Codes "; " (depth *LZWDict) " " @@)
      (when T
         (make
            (let (Cnt 100000000  W NIL  C NIL)
               (while
                  (and
                     (ge0 (dec 'Cnt))
                     (setq C (rd 1)) )
                  (let (WC (append W (list C))  Code (LzwDictLookup WC))
                     (when NIL
                        (msg
                           '=
                           " C: "
                           C
                           "; WC: "
                           (sym WC)
                           "; Code: "
                           Code
                           "; W: "
                           (sym W) ) )
                     (cond
                        ((setq Code (LzwDictLookup WC)) (setq W WC))
                        ((setq Code (LzwDictLookup W T))
                           (link Code)
                           (LzwDictUpdate WC (inc 'Codes))
                           (setq W (list C)) )
                        (T (msg '! " NO CODE")) ) )
                  (when NIL (msg '(----))) )
               (and W (link (LzwDictLookup W T))) ) ) ) ) )


# Decompress a stream previously created with lzwCompress
(de lzwDecompress NIL
   (setq *LZWDict NIL)
   (let
      (Codes 256
         Hash (fnvHasher 64)
         MakeHashKey
         '((Val)
            (Hash (if (lst? Val) Val (list Val))) )
         LzwDictLookup
         '((Code)
            (let
               (Word NIL
                  HashCode (MakeHashKey Code)
                  Entry (lup *LZWDict HashCode)
                  Words NIL )
               (when Entry
                  (setq Words (cadr @))
                  (if (assoc Code Words) (setq Word (cadr @))) )
               (when NIL
                  (msg
                     '?
                     " Word "
                     (sym Word)
                     "; Entry: "
                     (sym Entry)
                     "; Words: "
                     (sym Words)
                     "; Lookup: "
                     (sym (assoc Code Words))
                     "; Word: "
                     Word ) )
               Word ) )
         LzwDictUpdate
         '((Code Word)
            (use (Entry Words HashCode)
               (setq HashCode (MakeHashKey Code))
               (setq Word (list Code Word))
               (setq Words (list Word))
               (setq Entry (list HashCode Words))
               (cond
                  ((lup *LZWDict HashCode)
                     (setq Words (cdr @))
                     (ifn (assoc Code Words) (push Words Word))
                     (setq Entry (lup *LZWDict HashCode))
                     (when NIL
                        (msg
                           '+
                           " Code: "
                           (sym Code)
                           "; Word: "
                           (sym Word)
                           "; Entry: "
                           (sym Entry) ) ) )
                  (T (idx '*LZWDict Entry T)
                     (when T
                        (when NIL
                           (msg
                              '>
                              " Key: "
                              (sym Key)
                              "; Code: "
                              Code
                              "; Entry: "
                              (sym Entry) ) ) ) ) ) ) ) )
      (use (Ndx Word Code)
         (setq Ndx Codes)
         (while (gt0 Ndx)
            (dec 'Ndx)
            (setq Code Ndx)
            (setq Word (list Ndx))
            (LzwDictUpdate Code Word) ) )
      (msg '> " Codes: " Codes "; " (depth *LZWDict) " " @@)
      (when T
         (make
            (let (Cnt 100000000  W NIL  WC NIL)
               (while (and (gt0 (dec 'Cnt)) (setq N (rd)))
                  (let
                     (WC (ifn (LzwDictLookup N) (cons (last W) W) @) )
                     (when NIL
                        (msg
                           '>
                           " # "
                           Cnt
                           "; N "
                           N
                           "; WC |"
                           (glue " " WC)
                           "| W |"
                           (glue " " W)
                           "|" ) )
                     (chain (reverse WC))
                     (when W
                        (inc 'Codes)
                        (LzwDictUpdate Codes (cons (last WC) W)) )
                     (setq W WC) ) ) ) ) ) ) )
