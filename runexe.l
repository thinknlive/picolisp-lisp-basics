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

# Send a list of commands, Cmds, to an external process, Exe, via it's stdin
# Returns a list with 
#     a list of lines from the stdout of Exe in its car (or NIL if none)
#     a list of lines from the stderr of Exe in its cadr (or NIL if none)

#{

For example:

: (mapc '((X) (mapc println X)) 
     (runExe '(
         ".mode quote" 
         "create table test(id integer primary key, name);" 
         "insert into test (name) values ('Fred'),('James'), ('Jane');" 
         "select id, name from test;" 
         ".exit") sqlite3))
"1,'Fred'"
"2,'James'"
"3,'Jane'"
-> NIL

}#

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
