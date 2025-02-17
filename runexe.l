# RunExe: run a list of commands in an external process and return it's stdout and stderr output as separate lists

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

# A few auxillary functions to generate random strings using a Lehmer pseudo random generator
# Note: (randR) is quite slow compared to the built-in (rand) function

(de randomLehmer (@Seed)
   (curry
      (@Seed (State))
      NIL
      (unless State (setq State @Seed))
      (setq State (% (* State 279470273) 4294967291)) ) )

Or, (better performance by eliminating the 'unless' guard

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

Or, simpler code using 'fill.  (suggestion from abu)

(de randomLehmer (Seed)
      (let @Seed (or Seed (inc (abs (rand))))
         (fill
            '(()
               (job '((State . @Seed))
                  (setq State (% (* State 279470273) 4294967291)) ) ) ) ) )

      
(setq *RndFn (randomLehmer (time)))
(de randR (Lo Hi) (+ Lo (% (*RndFn) (+ (- Hi Lo) 1))))
      
(de makeRndName (Cnt)
   (default Cnt 7)
   (pack
      (make
         (for N Cnt
            (link
               (char
                  (case (randR 1 3)
                     (1 (randR 48 57))
                     (2 (randR 65 90))
                     (3 (randR 97 122)) ) ) ) ) ) ) )                     

# Example: Interfacing with sqlite

: (when (info "testdb.sqlite") (call "rm" "testdb.sqlite"))
-> T

: (gc 200 200)
-> 200

# Insert 1,000,000 records with names of length 7

: (bench (let (Sql '(
  ".mode quote"
  "drop table if exists test;"
  "create table test(id integer primary key, name text);"))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 1000000 (link (pack "insert into test (name) values ('" (makeRndName 7) "');"))))
      '("commit;")
      '("select count(*) from test;")))
  (bench (runExe Sql sqlite3 "-bail" "testdb.sqlite"))))
2.216 sec
4.369 sec
-> (("1000000") NIL)

# Insert another 1,000,000 records with names of length 7

: (bench (let (Sql '(
  ".mode quote"))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 1000000 (link (pack "insert into test (name) values ('" (makeRndName 7) "');"))))
      '("commit;")
      '("select count(*) from test;")))
  (bench (runExe Sql sqlite3 "-bail" "testdb.sqlite"))))
2.166 sec
4.537 sec
-> (("2000000") NIL)

# Insert 100,000 records with names of length 5. Track and report collisions

: (bench (let (Sql '(
  ".mode quote"
  "drop table if exists test;"
  "create table test(id integer primary key, name text, cnt int default 1);"
  "create unique index uix_test_name on test(name);"))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 100000 (link (pack
        "insert into test (name) values ('" (makeRndName 5) "') on conflict (name) do update set cnt = cnt + 1;"))))
      '("commit;")
      '("select id, name, cnt from test where cnt > 1;")
      '("select 'Collision Count: '||count(*) from test where cnt > 1;")
      '("select 'Record Count: ' || count(*) from test;")))
  (bench (runExe Sql sqlite3 "testdb.sqlite"))))
1.046 sec
1.244 sec
-> (("446,'21fYs',2" "2308,'G4z06',2" "11141,'0P9r9',2" "14757,'L3i68',2" "15012,'56E50',2" "16398,'Il5IX',2" 
     "18635,'2h74k',2" "21103,'4172Q',2" "24928,'76wj5',2" "31774,'1SbTK',2" "32120,'898E5',2" "35358,'471G7',2" 
     "36303,'c3evW',2" "37695,'14E1u',2" "41323,'kv69b',2" "44134,'5L8Nf',2" "54609,'G934m',2" "55687,'L43Lx',2" 
     "85253,'62135',2" 
     "'Collision Count: 19'" 
     "'Record Count: 99981'") NIL)

# Insert 100,000 records with names of length 7. Track and report collisions

: (bench (let (Sql '(
  ".mode quote"
  "drop table if exists test;"
  "create table test(id integer primary key, name text, cnt int default 1);"
  "create unique index uix_test_name on test(name);"))
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 100000 (link (pack
        "insert into test (name) values ('" (makeRndName 7) "') on conflict (name) do update set cnt = cnt + 1;"))))
      '("commit;")
      '("select id, name, cnt from test where cnt > 1;")
      '("select 'Collision Count: '||count(*) from test where cnt > 1;")
      '("select 'Record Count: ' || count(*) from test;")))
  (bench (runExe Sql sqlite3 "testdb.sqlite"))))
0.777 sec
1.221 sec
-> (("'Collision Count: 0'" "'Record Count: 100000'") NIL)

# Generate and show an error

: (bench (let (Sql '(
  ".mode quote"
  "create table if not exists test(id integer primary key, name text, cnt int default 1);"
  "create unique index uix_test_name on test(name);"))
  (runExe Sql sqlite3 "-bail" "testdb.sqlite")))
0.125 sec
-> (NIL ("Error: near line 3: index uix_test_name already exists"))
:
