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
                  (case (rand 1 3)
                     (1 (rand 48 57))
                     (2 (rand 65 90))
                     (3 (rand 97 122)) ) ) ) ) ) ) )
                     

# Example: Interfacing with sqlite

: (call "rm" "testdb.sqlite")
-> T

# Insert 1,000,000 random names of length 7

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
  (runExe Sql sqlite3 "testdb.sqlite")))
11.325 sec
-> (("1000000") NIL)

# Insert another 1,000,000 random names of length 7

: (bench (let (Sql '(
  ".mode quote"
  (setq Sql
    (append
      Sql
      '("begin transaction;")
      (make (do 1000000 (link (pack "insert into test (name) values ('" (makeRndName 7) "');"))))
      '("commit;")
      '("select count(*) from test;")))
  (runExe Sql sqlite3 "testdb.sqlite")))
11.361 sec
-> (("2000000") NIL)
:

# Use a unique index to report collisions inserting 100,000 names of length 5

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
  (runExe Sql sqlite3 "testdb.sqlite")))
3.203 sec
-> (("3586,'EvX2P',2" "22646,'i7M73',2" "23745,'TBBl3',2" "24059,'Ht9q1',2" 
     "33958,'94w50',2" "35168,'2n711',2" "40635,'Pgw90',2" 
     "45943,'6Y47O',2" "51983,'225U1',2" "60823,'5D9F9',2" 
     "67098,'MhSE7',2" "70981,'ABkB5',2" "72260,'95508',2" 
     "74481,'jaz13',2" "76302,'1oDd1',2" "78656,'d8y91',2" 
     "'Collision Count: 16'" "'Record Count: 99984'") NIL)

# Use a unique index to report collisions inserting 100,000 names of length 7

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
  (runExe Sql sqlite3 "testdb.sqlite")))
3.485 sec
-> (("'Collision Count: 0'" "'Record Count: 100000'") NIL)
