# Work in progress: JSON parser
# Parses JSON values and arrays, [], (possibly nested) into assoc lists
# TODO: 
#    Add support for objects, {}
#    Add convenvience functions to fetch values.

#{

: (let (Lst (chop "[1,2,[[3,4],[\"A\"],[5,6],7,8],9]")) (pretty (jsonParseArray Lst)))
(((4 "9" . "number")
      (3
         (5 "8" . "number")
         (4 "7" . "number")
         (3 (2 "6" . "number") (1 "5" . "number") (NIL . "Array"))
         (2 (1 "A" . "string") (NIL . "Array"))
         (1 (2 "4" . "number") (1 "3" . "number") (NIL . "Array"))
         (NIL . "Array") )
      (2 "2" . "number")
      (1 "1" . "number")
      (NIL . "Array") )
   NIL )-> " )"
: (let (Lst (chop "[1,2,3,4,5,6]")) (pretty (jsonParseArray Lst)))
(((6 "6" . "number")
      (5 "5" . "number")
      (4 "4" . "number")
      (3 "3" . "number")
      (2 "2" . "number")
      (1 "1" . "number")
      (NIL . "Array") )
   NIL )-> " )"
: (let (Lst (chop "[  271,\"c329\",2,\"422497BF15C26E3C\"]")) (pretty (jsonParseArray Lst)))
(((4 "422497BF15C26E3C" . "string")
      (3 "2" . "number")
      (2 "c329" . "string")
      (1 "271" . "number")
      (NIL . "Array") )
   NIL )-> " )"
:
  
  
}#

  # (prinl "jsonSkipWhiteSpace: +|" Lst "|")
  (let (C NIL NotDone T) 
    (while (and NotDone Lst (setq C (car Lst)))
      (case C
        ('(`(char 32) `(char 9) `(char 10) `(char 13)) 
          (++ Lst))
        (T (setq NotDone NIL))
        ))
    # (prinl "jsonSkipWhiteSpace: =|" Lst "|")
    (cons (length Lst) Lst)))
 
(de jsonParseValue (Lst)
   (let
      (NotDone T
         InQuote NIL
         Quoted NIL
         Fld NIL
         Value NIL
         Result NIL
         C NIL )
      (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
      # (prinl "jsonParseValue: +|" Lst "|")
      (while (and NotDone Lst (push 'Fld (++ Lst)))
         # (prinl "jsonParseValue: -|" Lst "|")
         (case (car Fld)
            ('(" " "^I" "^J" "^M")
               (if (and (not InQuote) (cdr Fld))
                  (throw T "jsonParseValue: Unexpected whitespace.") ) )
            ("\\"
               (cond
                  (InQuote
                     (pop 'Fld)
                     (setq C (++ Lst))
                     (case C
                        ("b" (push 'Fld (char 8)))
                        ("f" (push 'Fld (char 12)))
                        ("n" (push 'Fld (char 10)))
                        ("r" (push 'Fld (char 13)))
                        ("t" (push 'Fld (char 9)))
                        ("\"" (push 'Fld (char 34)))
                        ("\\" (push 'Fld (char 92)))
                        ("u"
                           (push 'Fld
                              (char (hex (pack (cut 4 'Lst)))) ) )
                        ("U"
                           (push 'Fld
                              (char (hex (pack (cut 6 'Lst)))) ) )
                        (T (and C (push 'Fld C))) ) )
                  (T (throw T "jsonParseValue: Not in string")) ) )
            ('("," "]" "}")
               (unless InQuote
                  (push 'Lst (pop 'Fld))
                  (setq Value (pack (reverse Fld))  Fld NIL  NotDone NIL) ) )
            ('("[" "{")
               (unless InQuote
                  (push 'Lst (pop 'Fld))
                  (if Fld (throw T "jsonParsValue: unexpected group open"))
                  (setq Value (pack (reverse Fld))  Fld NIL  NotDone NIL) ) )
            ("\""
               (cond
                  (InQuote
                     (pop 'Fld)
                     (setq
                        Value (pack (reverse Fld))
                        Fld NIL
                        InQuote NIL
                        NotDone NIL
                        Quoted T ) )
                  (T (pop 'Fld)
                     (when Fld (throw T "jsonParseValue: unexpected quote"))
                     (setq InQuote T) ) ) ) ) )
      (if Fld (setq Value (pack (reverse Fld))))
      (if Value
         (let (value (lowc Value) S (chop value))
            (setq Result
               (cond
                  (Quoted (cons Value "string"))
                  ((= "true" value) (cons value "boolean"))
                  ((= "false" value) (cons value "boolean"))
                  ((= "null" value) (cons value "null"))
                  ((= "number" (prog
                      (loop (NIL S) (NIL (sub? (++ S) (chop "0123456789+-e"))))
                      (ifn S "number"))) (cons value "number"))
                  (T (cons Value "undefined")) ) ) ) 
         (setq Result (cons Value "undefined")))
      # (prinl "jsonParseValue: =|" Lst "|")
      # (prinl "jsonParseValue: >>>>|" Value "|-->|" Result "|")
      (list Result Lst) ) )  
  

(de jsonParseArray (Lst)
  (let (Array (list (cons NIL . "Array")) Ndx 0 Value NIL NotDone T)
    # (prinl "jsonParseArray: +|" Lst "|")
    (ifn (and Lst (= "[" (car Lst)))
      (throw T "jsonParseArray: Expected '['"))
    (pop 'Lst)
    (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
    (loop
      (NIL Lst)
      (NIL NotDone)
      (T (and Lst (= "]" (car Lst))))
      # (prinl "jsonParseArray: >>>|" Ndx "|--|" Lst "|")
      (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
      (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
      #(prinl "jsonParseArray: @|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
      (case (car Lst)
        ('("]" ",") 
          (push 'Array (cons (inc 'Ndx) Value))
          #(prinl "jsonParseArray: +|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )
      )
      (case (car Lst)
        ("["
          # (prinl "jsonParseArray: RECURSE |" Lst "|")
          (let Result (jsonParseArray Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Array (cons (inc 'Ndx) Value))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          (case (car Lst)
            ("," (++ Lst))
            ("]"  (setq NotDone NIL))
            (T (throw T "jsonParseArray: expected ',' or ']'")))
          # (prinl "jsonParseArray: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )  
        ("," (++ Lst))
        ("]"  (setq NotDone NIL))
        (T (throw T "jsonParseArray: expected ',' or ']'"))
      )
    )
    # (prinl "jsonParseArray: =|" Lst "|")
    (ifn (and Lst (= "]" (car Lst)))
      (throw T "jsonParseArray: expected ']'"))
    (pop 'Lst)
    (list Array Lst))
)

