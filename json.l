# JSON parser: Compliant (mostly)
# Uses 'assoc' lists for objects and arrays
# Stores all values as strings but attaches an inferred type in a cons pair; 
#     string,boolean,null,number,undefined,chars. The last for unquoted strings that don't look like valid numbers
# Arrays: ((NIL . "array")(1 "1" "number)...)
# Objects: ((NIL . "object")("1" 1 "number")...)

# TODO: Add a helper function to extract values and types, something like 'nth' but for json
# TODO: More compact format? Perhaps using ints or abbreviations for types

# Some examples

#{
: (let (Lst (chop "\"ABCD\"")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("ABCD" . "string")-> ("ABCD" . "string")
: (let (Lst (chop "1234")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("1234" . "number")-> ("1234" . "number")
: (let (Lst (chop "true")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("true" . "boolean")-> ("true" . "boolean")
: (let (Lst (chop "false")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("false" . "boolean")-> ("false" . "boolean")
: (let (Lst (chop "null")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("null" . "null")-> ("null" . "null")

: (let (Lst (chop "")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
(NIL . "undefined")-> (NIL . "undefined")

: (let (Lst (chop "[{}]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array") (1 (NIL . "object")))-> ((NIL . "array") (1 (NIL . "object")))

: (let (Lst (chop "{array:[]}")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "object") ("array" (NIL . "array")))-> ((NIL . "object") ("array" (NIL . "array")))

: (let (Lst (chop "[1,2,3,4]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array")
   (1 "1" . "number")
   (2 "2" . "number")
   (3 "3" . "number")
   (4 "4" . "number") )-> " )"

: (let (Lst (chop "{\"Language\":\"PicoLisp\",Name:\"BuoyantHacker\",\"Level\":3,\"FavBook\":\"Modes of Thought\"}")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "object")
   ("Language" "PicoLisp" . "string")
   ("Name" "BuoyantHacker" . "string")
   ("Level" "3" . "number")
   ("FavBook" "Modes of Thought" . "string") )-> " )"

: (let (Lst (chop "[[1,2,[[3,4],[\"A\"],[5,6],7,8],9]]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array")
   (1
      (NIL . "array")
      (1 "1" . "number")
      (2 "2" . "number")
      (3
         (NIL . "array")
         (1 (NIL . "array") (1 "3" . "number") (2 "4" . "number"))
         (2 (NIL . "array") (1 "A" . "string"))
         (3 (NIL . "array") (1 "5" . "number") (2 "6" . "number"))
         (4 "7" . "number")
         (5 "8" . "number") )
      (4 "9" . "number") ) )-> " )"

: (let (Lst (chop "[  271,\"c329\",2,\"422497BF15C26E3C\"]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array")
   (1 "271" . "number")
   (2 "c329" . "string")
   (3 "2" . "number")
   (4 "422497BF15C26E3C" . "string") )-> " )"

: (let (Lst (chop "[1,2,[[3,4],{\"A\":\"Object\"},[5,6],7,8],9]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array")
   (1 "1" . "number")
   (2 "2" . "number")
   (3
      (NIL . "array")
      (1 (NIL . "array") (1 "3" . "number") (2 "4" . "number"))
      (2 (NIL . "object") ("A" "Object" . "string"))
      (3 (NIL . "array") (1 "5" . "number") (2 "6" . "number"))
      (4 "7" . "number")
      (5 "8" . "number") )
   (4 "9" . "number") )-> " )"

: (assoc 2 (assoc 3 *JP))
-> (2 (NIL . "object") ("A" "Object" . "string"))
: (assoc "A" (assoc 2 (assoc 3 *JP)))
-> ("A" "Object" . "string")
: (cadr (assoc "A" (assoc 2 (assoc 3 *JP))))
-> "Object"

}#

# The code

(de jsonParse (Lst)
  (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
  # (prinl "jsonParse: ::|" Lst "|")
  (case (car Lst) 
    ("{" (jsonParseObject Lst))
    ("[" (jsonParseArray Lst))
    (T (jsonParseValue Lst))))

(de jsonSkipWhiteSpace (Lst)
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
            ('("," "]" "}" ":")
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
                  ((= S (sect S (chop "0123456789+-e"))) (cons value "number"))
                  (T (cons Value "chars")) ) ) ) 
         (setq Result (cons Value "undefined")))
      # (prinl "jsonParseValue: >>>>|" (sym Value) "|-->|" Result "|---|" Lst "|")
      (list Result Lst) ) )  
  

(de jsonParseArray (Lst)
  (let (Array (list (cons NIL . "array")) Ndx 0 Value NIL NotDone T)
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
      (case (car Lst)
        ("["
          # (prinl "jsonParseArray: RECURSE |" Lst "|")
          (let Result (jsonParseArray Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Array (cons (inc 'Ndx) Value))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          # (prinl "jsonParseArray: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (case (car Lst)
            ("," (++ Lst))
            ("]"  (setq NotDone NIL))
            (T (throw T "jsonParseArray: expected ',' or ']'")))
        )  
        ("{"
          # (prinl "jsonParseArray: RECURSE |" Lst "|")
          (let Result (jsonParseObject Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Array (cons (inc 'Ndx) Value))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          # (prinl "jsonParseArray: ____|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (case (car Lst)
            ("," (++ Lst))
            ("]"  (setq NotDone NIL))
            (T (throw T "jsonParseArray: expected ',' or ']'")))
        )
        ("," (++ Lst))
        ("]"  (setq NotDone NIL))
         (T
            (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
            (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
            # (prinl "jsonParseArray: @|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
            (case (car Lst)
              ("," 
                (++ Lst)
                (push 'Array (cons (inc 'Ndx) Value))
              )
              ("]"
                (if Value (push 'Array (cons (inc 'Ndx) Value)))
                (setq NotDone NIL)
              )
            )
         )
      )
    )
    # (prinl "jsonParseArray: =|" Lst "|")
    (ifn (and Lst (= "]" (car Lst)))
      (throw T "jsonParseArray: expected ']'"))
    (pop 'Lst)
    (list (reverse Array) Lst))
)

(de jsonParseObject (Lst)
  (let (Object (list (cons NIL . "object")) Name NIL Value NIL NotDone T)
    # (prinl "jsonParseObject: +|" Lst "|")
    (ifn (and Lst (= "{" (car Lst)))
      (throw T "jsonParseObject: Expected '{'"))
    (pop 'Lst)
    (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
    (loop
      (NIL Lst)
      (NIL NotDone)
      (T (and Lst (= "}" (car Lst))))
      # (prinl "jsonParseObject: >>>|" Ndx "|--|" Lst "|")
      (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
      (case (car Lst)
        ("["
          (ifn Name (throw T "jsonParseObject: unexpected '['"))
          # (prinl "jsonParseObject: RECURSE |" Lst "|")
          (let Result (jsonParseArray Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Object (cons Name Value))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          (case (car Lst)
            ("," (++ Lst))
            ("}"  (setq NotDone NIL))
            (T (throw T "jsonParseObject: expected ',' or '}'")))
          # (prinl "jsonParseObject: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )  
        ("{"
          (ifn Name (throw T "jsonParseObject: unexpected '{'"))
          # (prinl "jsonParseObject: RECURSE |" Lst "|")
          (let Result (jsonParseObject Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Object (cons Name Value))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          (case (car Lst)
            ("," (++ Lst))
            ("}"  (setq NotDone NIL))
            (T (throw T "jsonParseObject: expected ',' or '}'")))
          # (prinl "jsonParseObject: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )
        (":"
          (ifn Name (throw T "jsonParseObject: unexpected ':'"))
          (++ Lst)
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          (case (car Lst)
          ('("[" "{") T)
          (T 
            (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
            (push 'Object (cons Name Value))
            (setq Name NIL)
            (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
            (case (car Lst)
              ("," (++ Lst))
              ("}"  (setq NotDone NIL))
              (T (throw T "jsonParseObject: expected ',' or '}'")))
              ))
        )
        (T 
          (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
          (setq Lst (cdr (jsonSkipWhiteSpace Lst)))
          # (prinl "jsonParseObject: @@|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (case (car Lst)
            (":"
              (setq Name (car Value))
              # (prinl "jsonParseObject: +|" Ndx "|-->|" (sym Name) "|---|" Lst "|")
            )
            (T (throw T "jsonParseObject: Expected ':'"))
          )
        )
      )
    )
  # (prinl "jsonParseObject: =|" Lst "|")
  (ifn (and Lst (= "}" (car Lst)))
     (throw T "jsonParseObject: expected '}'"))
  (pop 'Lst)
  (list (reverse Object) Lst))
)
