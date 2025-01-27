# Example showing 
# 1. How to reliably INSERT into a sqlite db from picolisp 
#    - uses the function 'sqlQuote' to properly encode parameter values for sqlite
# 2. How to reliably SELECT from a sqlite db and parse the response back into picolisp
#    - uses the 'json_array' function of sqlite to return each row of the result set as a json array
#    - uses the function 'fldParseQ' to parse a sqlite json array into a list of transients, handling escapes, unicode, etc

# =======================================================================================================
# Without further ado...
# Insert 10,000 rows of random names of given length that also include unicode chars
# Track collisions, then query for and report them
  

: (when (info "testdb.sqlite") (call "rm" "testdb.sqlite"))
-> T
: (bench (let (Sql '(
  ".mode quote"
  "drop table if exists test;"
  "create table test(id integer primary key, name text, cnt int default 1);"
  "create unique index uix_test_name on test(name);"
  ))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 10000 (link (pack
        "insert into test (name) values (" (sqlQuote (makeRndName 4)) ") on conflict (name) do update set cnt = cnt + 1;"))))
      '("commit;")
      '("select json_array(id, name, cnt, (hex(randomblob(16)))) from test where cnt > 1;")
      '("select json_array('CollisionCount', count(*)) from test where cnt > 1;")
      '("select json_array('RecordCount', count(*)) from test;")))
  (bench (mapc '((R) (let (Row (head -2 (nth (chop R) 3))
                           Flds (fldParseQ Row))
                     (prin R " --> ") (print Flds) (prinl  " : " (length Flds))))
                (car (runExe Sql sqlite3 "testdb.sqlite"))) ]

'[91,",57,",2,"ADFAA494D85CFF36E3CD136EA18CF47B"]' --> ("91" ",57," "2" "ADFAA494D85CFF36E3CD136EA18CF47B") : 4
'[353,"👍ᘐ\\,",2,"A011BA30FABC582E4507D73917DC556F"]' --> ("353" "👍ᘐ\\," "2" "A011BA30FABC582E4507D73917DC556F") : 4
'[426,",ᘐ,\\",2,"D4E28D7F0FA5C1B6AEB4608FAB6A8C44"]' --> ("426" ",ᘐ,\\" "2" "D4E28D7F0FA5C1B6AEB4608FAB6A8C44") : 4
'[1099,"\n\n8ᘐ",2,"016FECE7567C6E107B6FBB64C5BD45FC"]' --> ("1099" "^J^J8ᘐ" "2" "016FECE7567C6E107B6FBB64C5BD45FC") : 4
'[1620,"👍H2N",2,"9CE9F9B97AFC42A7AEE3B8435E6CDC7F"]' --> ("1620" "👍H2N" "2" "9CE9F9B97AFC42A7AEE3B8435E6CDC7F") : 4
'[4097,"ᘐ4,4",2,"41FF6006FDD872AC07560E9A5BB8B6DC"]' --> ("4097" "ᘐ4,4" "2" "41FF6006FDD872AC07560E9A5BB8B6DC") : 4
'[6070,"\n\\ᘐ\n",2,"871DE1E026AAF6C4DB0DF2360639DBBF"]' --> ("6070" "^J\\ᘐ^J" "2" "871DE1E026AAF6C4DB0DF2360639DBBF") : 4
'[7228,"2👍\"\\",2,"F5AB86B62AB4D7FBFB413C6F7650859D"]' --> ("7228" "2👍\"\\" "2" "F5AB86B62AB4D7FBFB413C6F7650859D") : 4
'[8139,"4\",\\",2,"16A1182CD11B97B6936E077BC8417C99"]' --> ("8139" "4\",\\" "2" "16A1182CD11B97B6936E077BC8417C99") : 4
'[8381,"\"n\"\\",2,"6592FE3F4A1D336A8E74A6BB97CC374E"]' --> ("8381" "\"n\"\\" "2" "6592FE3F4A1D336A8E74A6BB97CC374E") : 4
'["CollisionCount",10]' --> ("CollisionCount" "10") : 2
'["RecordCount",9990]' --> ("RecordCount" "9990") : 2
0.110 sec
0.135 sec
-> 2

# =======================================================================================================
# The following functions are used in the above example

# Parse scalar values from a json encoded array and return a list of picolist transient values
# Note: No support for nesting of objects or arrays. Just scalar values (number, string, boolean) etc.
  
(de fldParseQ (Str)
   (let
      (S (chop Str)
         Cnt (length S)
         InFld T
         InQuote NIL
         Flds NIL
         Fld NIL
         C NIL )
      (while (and (dec 'Cnt) (push 'Fld (++ S)))
         (case (car Fld)
            ("\\"
               (pop 'Fld)
               (setq C (++ S))
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
                        (char (hex (pack (cut 4 'S)))) ) )
                  ("U"
                     (push 'Fld
                        (char (hex (pack (cut 6 'S)))) ) )
                  (T (and C (push 'Fld C))) ) )
            (","
               (unless InQuote
                  (pop 'Fld)
                  (push 'Flds (pack (reverse Fld)))
                  (setq Fld NIL) ) )
            ("\""
               (cond
                  (InQuote
                     (pop 'Fld)
                     (push 'Flds (pack (reverse Fld)))
                     (setq Fld NIL)
                     (setq InQuote NIL)
                     (while (and S (<> "," (car S)))
                        (++ S) )
                     (ifn (= "," (car S)) (setq InFld NIL))
                     (++ S) )
                  (T (pop 'Fld) (setq InQuote T)) ) ) ) )
      (if InFld (push 'Flds (pack (reverse Fld))))
      (reverse Flds) ) )

# Given a string, quote it appropriately for use as a sql parameter
# e.g. insert into test (name) values ( sqlQuote("Orange's one braincell") );
  
(de sqlQuote (Str)
   (let (S (chop Str)  C NIL)
      (pack
         (make
            (link "'")
            (while (setq C (++ S))
               (ifn (= C "'")
                  (link C)
                  (link C)
                  (cond
                     ((= C (car S)) (link (++ S)))
                     (T (link C)) ) ) )
            (link "'") ) ) ) )                  


# A PRNG. 
# Note how code is data in Picolisp 
#   And we set State to Seed's value before returning the generated function
  
(de randomLehmer (Seed)
   (let
      (Fn
         (curry
            ((State . 1))
            NIL
            (setq State (% (* State 279470273) 4294967291)) ) )
      (set
         (nth Fn 2 2 2)
         (cons
            'State
            (if Seed Seed (inc (abs (rand)))) ) )
      Fn ) )

(setq *RndFn (randomLehmer (time)))
(de randR (Lo Hi) (+ Lo (% (*RndFn) (inc (- Hi Lo)))))

# Used to generate random names, throwing in some funky chars as well.
  
(de makeRndName (Cnt)
   (default Cnt 7)
   (pack
      (make
         (for N Cnt
            (link
               (char
                  (case (randR 1 4)
                     (1 (randR 48 57))
                     (2 (randR 65 90))
                     (3 (randR 97 122))
                     (4 (char (nth '("," "\\" "\"" ~(char 10) ~(char (hex "1f44d")) ~(char (hex "1610"))) (randR 1 6) 1))) ) ) ) ) ) ))
                     

# Used to pipe an external process (in this case sqlite) and capture the results
# Note: This assumes line oriented text response
# Note: stdout and stderr capture of the external process are returned in separate lists.
  
(de runExe (Cmds . Exe)
   (use (LOut LErr)
      (let (ErrFile (tmp "exerr." (rand)))
         (finally (call "rm" ErrFile)
            (pipe
               (err ErrFile (out Exe (mapc prinl Cmds)))
               (setq LOut (make (while (line T) (link @)))) )
            (in ErrFile
               (setq LErr (make (while (line T) (link @)))) ) ) )
      (list LOut LErr) ) )



