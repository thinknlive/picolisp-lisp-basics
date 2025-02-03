# JSON parser: Compliant+

# I needed something like this to process json results returned from sqlite
# I wanted a very consistent structure, easy to read and extract values from using picolisp list functions like assoc, nth, car, mapc, etc; 
#    nested 'assoc' lists and cons pairs fit nicely for this purpose. 
#    If I need better performance from a given list it is easy to convert to an 'idx' or 'enum'
# I wanted to be able to handle multiple 'results' or result sets --> 'jsonParse' returns a list of json entities that it parses from the provided list

# The 'assoc' lists for objects and arrays look like this
#    Arrays: ((NIL . "array")(1 "1" "number")...)
#    Objects: ((NIL . "object")("1" 1 "number")...)
# So for any 'list' in a result (assoc NIL Lst) will return the type; "array" or "object"

# All values are represented as strings but the inferred type is included in a cons pair so it is easy to format, or interpret, as needed for given use case 
#     string,boolean,null,number,undefined,chars. 
#     The last 'chars' are unquoted strings that don't look like valid numbers

# TODO: More compact data structure? Perhaps using ints or abbreviations for types
# TODO: Add a helper function to extract values and types, something like 'nth' but for this data structure.
#       Currently you have to use 'assoc' (see examples)

# Some examples

#{
: (let (Lst (chop "\"ABCD\"")) (setq *JP (jsonParse Lst)) (pretty *JP) (car *JP))
(("ABCD" . "string"))-> ("ABCD" . "string")

: (let (Lst (chop "1234")) (setq *JP (jsonParse Lst)) (pretty *JP) (caar *JP))
(("1234" . "number"))-> "1234"

: (let (Lst (chop "true")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (= "true" (car *JP)))
("true" . "boolean")-> T

: (let (Lst (chop "false")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (= "true" (car *JP)))
("false" . "boolean")-> NIL

: (let (Lst (chop "null")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
("null" . "null")-> ("null" . "null")

: (let (Lst (chop "")) (setq *JP (jsonParse Lst)) (pretty *JP))
NIL-> NIL

: (let (Lst (chop "[{}]")) (setq *JP (car (jsonParse Lst))) (pretty *JP) T)
((NIL . "array") (1 ((NIL . "object"))))-> T

: (let (Lst (chop "{array:[]}")) (setq *JP (jsonParse Lst)) (pretty (car *JP)) T)
((NIL . "object") ("array" ((NIL . "array"))))-> T

: (let (Lst (chop "[1,2,3,4]")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (assoc NIL *JP))
((NIL . "array")
   (1 "1" . "number")
   (2 "2" . "number")
   (3 "3" . "number")
   (4 "4" . "number") )-> (NIL . "array")

: (let (Lst (chop "{\"Language\":\"PicoLisp\",Name:\"BuoyantHacker\",\"Level\":3,\"FavBook\":\"Modes of Thought\"}")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (assoc NIL *JP))
((NIL . "object")
   ("Language" "PicoLisp" . "string")
   ("Name" "BuoyantHacker" . "string")
   ("Level" "3" . "number")
   ("FavBook" "Modes of Thought" . "string") )-> (NIL . "object")

: (let (Lst (chop "[[1,2,[[3,4],[\"A\"],[5,6],7,8],9]]")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (assoc NIL *JP))
((NIL . "array")
   (1
      ((NIL . "array")
         (1 "1" . "number")
         (2 "2" . "number")
         (3
            ((NIL . "array")
               (1 ((NIL . "array") (1 "3" . "number") (2 "4" . "number")))
               (2 ((NIL . "array") (1 "A" . "string")))
               (3 ((NIL . "array") (1 "5" . "number") (2 "6" . "number")))
               (4 "7" . "number")
               (5 "8" . "number") ) )
         (4 "9" . "number") ) ) )-> (NIL . "array")

: (let (Lst (chop "[  271,\"c329\",2,\"422497BF15C26E3C\"]")) (setq *JP (car (jsonParse Lst))) (pretty *JP) (car *JP))
((NIL . "array")
   (1 "271" . "number")
   (2 "c329" . "string")
   (3 "2" . "number")
   (4 "422497BF15C26E3C" . "string") )-> (NIL . "array")

: (let (Lst (chop "[1,2,[[3,4],{\"A\":\"Object\"},[5,6],7,8],9]")) (setq *JP (car (jsonParse Lst))) (pretty *JP))
((NIL . "array")
   (1 "1" . "number")
   (2 "2" . "number")
   (3
      ((NIL . "array")
         (1 ((NIL . "array") (1 "3" . "number") (2 "4" . "number")))
         (2 ((NIL . "object") ("A" "Object" . "string")))
         (3 ((NIL . "array") (1 "5" . "number") (2 "6" . "number")))
         (4 "7" . "number")
         (5 "8" . "number") ) )
   (4 "9" . "number") )-> " )"

: (assoc 2 (cadr (assoc 3 *JP)))
-> (2 ((NIL . "object") ("A" "Object" . "string")))

: (assoc "A" (cadr (assoc 2 (cadr (assoc 3 *JP)))))
-> ("A" "Object" . "string")

: (cadr (assoc "A" (cadr (assoc 2 (cadr (assoc 3 *JP))))))
-> "Object"
: (car (assoc "A" (cadr (assoc 2 (cadr (assoc 3 *JP))))))
-> "A"
}#

# A more complex example using adapting the code from sqlite.1
# Insert 100,000 rows then query
# Returning 1 result set of rows with no name collision
#   and another result set of only collision rows

#{

: (gc 200 200)
-> 200
: (bench (let (Lst NIL Cnt 100000 Data NIL Sql '(
  ".mode quote"
  "drop table if exists test;"
  "create table test(id integer primary key, name text, cnt int default 1);"
  "create unique index uix_test_name on test(name);"
  ))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do Cnt (link (pack
        "insert into test (name) values (" (sqlQuote (makeRndName 5)) ") on conflict (name) do update set cnt = cnt + 1;"))))
      '("commit;")))
    
    (msg "Insert: " Cnt)
    (bench (runExe Sql sqlite3 "testdb.sqlite"))

    (msg "Query: ")
    (setq Sql (append
      '(".mode json")
      '("select id, name, cnt, (hex(randomblob(4))) as rnd from test where cnt = 1;")
      '("select id, name, cnt, (hex(randomblob(4))) as rnd from test where cnt > 1;")
      ))
    (bench (setq Data (runExe Sql sqlite3 "testdb.sqlite")))
    (setq Lst (chop (car (pack (car Data)))))
    (msg "Parse: ")
    (bench (setq Data (jsonParse Lst)))
    (out "sql-results.txt" (pretty Data))
    (msg "Total Time: ")
    T
))
"Insert: "100000
0.936 sec
"Query: "
0.578 sec
"Parse: "
1.420 sec
"Total Time: "
3.917 sec
-> T
: (call "ls" "-lh" "sql-results.txt")
-rw-r--r-- 1 llawrence llawrence 19M Jan 31 01:20 sql-results.txt
-> T
: (call "head" "-n 40" "sql-results.txt")
(((NIL . "array")
      (1
         ((NIL . "object")
            ("id" "1" . "number")
            ("name" "H86dJ" . "string")
            ("cnt" "1" . "number")
            ("rnd" "9D52DBB4" . "string") ) )
      (2
         ((NIL . "object")
            ("id" "2" . "number")
            ("name" "vAdzs" . "string")
            ("cnt" "1" . "number")
...
:  (call "tail" "-n 200" "sql-results.txt")
...
            ("rnd" "8F0152D5" . "string") ) )
      (99959
         ((NIL . "object")
            ("id" "99979" . "number")
            ("name" "3a53M" . "string")
            ("cnt" "1" . "number")
            ("rnd" "6C85D343" . "string") ) )
      (99960
         ((NIL . "object")
            ("id" "99980" . "number")
            ("name" "f4sXc" . "string")
            ("cnt" "1" . "number")
            ("rnd" "3D5F4383" . "string") ) ) )
   ((NIL . "array")
      (1
         ((NIL . "object")
            ("id" "1518" . "number")
            ("name" "0I2f6" . "string")
            ("cnt" "2" . "number")
...
            ("rnd" "D1311EB2" . "string") ) )
      (20
         ((NIL . "object")
            ("id" "80866" . "number")
            ("name" "V897o" . "string")
            ("cnt" "2" . "number")
            ("rnd" "38631516" . "string") ) ) ) )-> T


# Working with the results
: (in "sql-results.txt" (setq *RS1 (read)) T)
-> T
: (println (length (car *RS1)) (length (cadr *RS1)))
99961 21
-> 21
:
: (bench (let (L (car *RS1) Cnt (length L)) (do 1000 (assoc (rand 1 Cnt) L))) T)
0.681 sec
-> T
: (let (L (car *RS1) Cnt (length L)) (do 10 (println (assoc (rand 1 Cnt) L))) T)
(87543 ((NIL . "object") ("id" "87563" . "number") ("name" "V9eKt" . "string") ("cnt" "1" . "number") ("rnd" "F2C04074" . "string")))
(13608 ((NIL . "object") ("id" "13613" . "number") ("name" "tNYIC" . "string") ("cnt" "1" . "number") ("rnd" "C68497BB" . "string")))
(86937 ((NIL . "object") ("id" "86957" . "number") ("name" "2UNob" . "string") ("cnt" "1" . "number") ("rnd" "C4EA01F7" . "string")))
(3144 ((NIL . "object") ("id" "3145" . "number") ("name" "g583A" . "string") ("cnt" "1" . "number") ("rnd" "ED0D0999" . "string")))
(6040 ((NIL . "object") ("id" "6044" . "number") ("name" "03rAi" . "string") ("cnt" "1" . "number") ("rnd" "4DC963FA" . "string")))
(41640 ((NIL . "object") ("id" "41652" . "number") ("name" "OKOH5" . "string") ("cnt" "1" . "number") ("rnd" "BF085CD6" . "string")))
(44137 ((NIL . "object") ("id" "44150" . "number") ("name" "0S3Af" . "string") ("cnt" "1" . "number") ("rnd" "65C3937D" . "string")))
(1954 ((NIL . "object") ("id" "1955" . "number") ("name" "j1Ttr" . "string") ("cnt" "1" . "number") ("rnd" "C0A05A20" . "string")))
(17419 ((NIL . "object") ("id" "17425" . "number") ("name" "KdMMh" . "string") ("cnt" "1" . "number") ("rnd" "3BFE2CDE" . "string")))
(93464 ((NIL . "object") ("id" "93484" . "number") ("name" "CIbgB" . "string") ("cnt" "1" . "number") ("rnd" "7ABF0C07" . "string")))
-> T
:
: (use (A) (bench (setq A (filter '((X) (if (= "OKOH5" (cadr (assoc "name" (cadr X)))) X)) (car *RS1)))) (pretty A) T)
0.019 sec
((41640
      ((NIL . "object")
         ("id" "41652" . "number")
         ("name" "OKOH5" . "string")
         ("cnt" "1" . "number")
         ("rnd" "BF085CD6" . "string") ) ) )-> T

: (use (A) (bench (setq A (filter '((X) (if (= "4DC963FA" (cadr (assoc "rnd" (cadr X)))) X)) (car *RS1)))) (pretty A) T)
0.016 sec
((6040
      ((NIL . "object")
         ("id" "6044" . "number")
         ("name" "03rAi" . "string")
         ("cnt" "1" . "number")
         ("rnd" "4DC963FA" . "string") ) ) )-> T
: (use (A) 
      (bench (setq A (filter '((X) (if (= "4DC963FA" (cadr (assoc "rnd" (cadr X)))) X)) (car *RS1)))) 
         (prin (caar A)) (mapc '((X)
         (if (car X) (prin " " (cadr X)))) (cadar A)) T)
0.017 sec
6040 6044 03rAi 1 4DC963FA-> T

}#


# -----------------------------------------------------------------------------
# The code

(de jsonParse (Lst)
  (use (Data)
    (setq Lst (jsonSkipWhiteSpace Lst))
    # (prinl "jsonParse: ::|" Lst "|")
    (make 
      (while Lst
          (case (car Lst) 
            ("{" 
              (setq Data (jsonParseObject Lst))
            )
            ("[" 
              (setq Data (jsonParseArray Lst))
            )
            ('("]" "}" ",") (throw T "jsonParseValue: Unexpected token."))
            (T 
              (setq Data (jsonParseValue Lst))
          ))
          #(println "jsonParse: +|" Data "|")
          (link (car Data))
          (setq Lst (cadr Data))))))

(de jsonSkipWhiteSpace (Lst)
    (while (member (car Lst) '(" " "^I" "^J" "^M"))
       (++ Lst) )
    Lst )
 
(de jsonParseValue (Lst)
   (let
      (NotDone T
         InQuote NIL
         Quoted NIL
         Fld NIL
         Value NIL
         Result NIL
         C NIL )
      #(prinl "jsonParseValue: +|" Lst "|")
      (setq Lst (jsonSkipWhiteSpace Lst))
      (while (and NotDone Lst (push 'Fld (++ Lst)))
         # (prinl "jsonParseValue: -|" Lst "|")
         (case (car Fld)
            ('(" " "^I" "^J" "^M")
               (if (and (not InQuote) (cdr Fld))
                  (throw T "jsonParseValue: Unexpected whitespace.") ) )
            ("\\"
               (cond
                  (InQuote
                     (++ Fld)
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
                  (push 'Lst (++ Fld))
                  (setq Value (pack (reverse Fld))  Fld NIL  NotDone NIL) ) )
            ('("[" "{")
               (unless InQuote
                  (push 'Lst (++ Fld))
                  (if Fld (throw T "jsonParseValue: unexpected group open"))
                  (setq Value (pack (reverse Fld))  Fld NIL  NotDone NIL) ) )
            ("\""
               (cond
                  (InQuote
                     (++ Fld)
                     (setq
                        Value (pack (reverse Fld))
                        Fld NIL
                        InQuote NIL
                        NotDone NIL
                        Quoted T ) )
                  (T (++ Fld)
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
      #(prinl "jsonParseValue: >>>>|" (sym Value) "|-->|" Result "|---|" Lst "|")
      (list Result Lst) ) )  
  

(de jsonParseArray (Lst)
  (let (Array (list (cons NIL . "array")) Ndx 0 Value NIL NotDone T)
    #(prinl "jsonParseArray: +|" Lst "|")
    (ifn (and Lst (= "[" (car Lst)))
      (throw T "jsonParseArray: Expected '['"))
    (++ Lst)
    (setq Lst (jsonSkipWhiteSpace Lst))
    (loop
      (NIL Lst)
      (NIL NotDone)
      (T (and Lst (= "]" (car Lst))))
      #(prinl "jsonParseArray: >>>|" Ndx "|--|" Lst "|")
      (case (car Lst)
        ("["
          # (prinl "jsonParseArray: RECURSE |" Lst "|")
          (let Result (jsonParseArray Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Array (cons (inc 'Ndx) (list Value)))
          (setq Lst (jsonSkipWhiteSpace Lst))
          # (prinl "jsonParseArray: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (case (car Lst)
            ("," (++ Lst))
            ("]"  (setq NotDone NIL))
            (T (throw T "jsonParseArray: expected ',' or ']'")))
        )  
        ("{"
          # (prinl "jsonParseArray: RECURSE |" Lst "|")
          (let Result (jsonParseObject Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Array (cons (inc 'Ndx) (list Value)))
          (setq Lst (jsonSkipWhiteSpace Lst))
          # (prinl "jsonParseArray: ____|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (case (car Lst)
            ("," (++ Lst))
            ("]"  (setq NotDone NIL))
            (T (throw T "jsonParseArray: expected ',' or ']'")))
        )
        ("," (++ Lst)
            (setq Lst (jsonSkipWhiteSpace Lst))
            (when (and Lst (sect (list (car Lst)) '("," "]"))) 
              (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
              (setq Lst (jsonSkipWhiteSpace Lst))
              #(prinl "jsonParseArray: @|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
              (push 'Array (cons (inc 'Ndx) Value))
              (case (car Lst)
                ("," T)
                ("]" 
                  (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
                  (push 'Array (cons (inc 'Ndx) Value))
                  #(prinl "jsonParseArray: @@|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
                  (setq NotDone NIL))
                (T (throw T "jsonParseArray: expected ',' or ']'")))
                )
        )
        ("]"  (setq NotDone NIL))
        (T
          (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
          (setq Lst (jsonSkipWhiteSpace Lst))
          #(prinl "jsonParseArray: @|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
          (push 'Array (cons (inc 'Ndx) Value))
        )
      )
    )
    # (prinl "jsonParseArray: =|" Lst "|")
    (ifn (and Lst (= "]" (car Lst)))
      (throw T "jsonParseArray: expected ']'"))
    (++ Lst)
    (setq Lst (jsonSkipWhiteSpace Lst))
    (list (reverse Array) Lst))
)

(de jsonParseObject (Lst)
  (let (Object (list (cons NIL . "object")) Name NIL Value NIL NotDone T)
    #(prinl "jsonParseObject: +|" Lst "|")
    (ifn (and Lst (= "{" (car Lst)))
      (throw T "jsonParseObject: Expected '{'"))
    (++ Lst)
    (setq Lst (jsonSkipWhiteSpace Lst))
    (loop
      (NIL Lst)
      (NIL NotDone)
      (T (and Lst (= "}" (car Lst))))
      # (prinl "jsonParseObject: >>>|" Ndx "|--|" Lst "|")
      (setq Lst (jsonSkipWhiteSpace Lst))
      (case (car Lst)
        ("["
          (ifn Name (throw T "jsonParseObject: unexpected '['"))
          # (prinl "jsonParseObject: RECURSE |" Lst "|")
          (let Result (jsonParseArray Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Object (cons Name (list Value)))
          (setq Lst (jsonSkipWhiteSpace Lst) Name NIL)
          (case (car Lst)
            ("," (++ Lst)
              
            )
            ("}"  (setq NotDone NIL))
            (T (throw T "jsonParseObject: expected ',' or '}'")))
          # (prinl "jsonParseObject: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )  
        ("{"
          (ifn Name (throw T "jsonParseObject: unexpected '{'"))
          # (prinl "jsonParseObject: RECURSE |" Lst "|")
          (let Result (jsonParseObject Lst) (setq Value (car Result) Lst (cadr Result)))
          (push 'Object (cons Name (list Value)))
          (setq Lst (jsonSkipWhiteSpace Lst) Name NIL)
          (case (car Lst)
            ("," (++ Lst))
            ("}"  (setq NotDone NIL))
            (T (throw T "jsonParseObject: expected ',' or '}'")))
          # (prinl "jsonParseObject: ====|" Ndx "|-->|" (sym Value) "|---|" Lst "|")
        )
        (":"
          (ifn Name (throw T "jsonParseObject: unexpected ':'"))
          (++ Lst)
          (setq Lst (jsonSkipWhiteSpace Lst))
          (case (car Lst)
          ('("[" "{") T)
          (T 
            (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
            (push 'Object (cons Name Value))
            (setq Name NIL)
            (setq Lst (jsonSkipWhiteSpace Lst))
            (case (car Lst)
              ("," (++ Lst))
              ("}"  (setq NotDone NIL))
              (T (throw T "jsonParseObject: expected ',' or '}'")))
              ))
        )
        (T 
          (let Result (jsonParseValue Lst) (setq Value (car Result) Lst (cadr Result)))
          (setq Lst (jsonSkipWhiteSpace Lst))
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
  (++ Lst)
  (setq Lst (jsonSkipWhiteSpace Lst))
  (list (reverse Object) Lst))
)

