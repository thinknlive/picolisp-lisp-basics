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
9.205 sec
17.357 sec
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
9.668 sec
17.395 sec
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
4.113 sec
4.792 sec
-> (("603,'49p0f',2" "5234,'59q18',2" "9197,'RD129',2" "21510,'2ZJYw',2" 
      "22670,'mW697',2" "25518,'4397H',2" "25916,'2yL1F',2" "26721,'S5w7k',2" 
      "42965,'u68r2',2" "43144,'2U29u',2" "54592,'57T91',2" "59298,'24956',2" 
      "71996,'3T706',2" "73238,'5Kv8A',2" "91299,'O6z7o',2" 
      "'Collision Count: 15'" 
      "'Record Count: 99985'") NIL)

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
2.671 sec
3.406 sec
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
