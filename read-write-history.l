# (mapc pp '(readHistory writeHistory))

# Save current REPL command history
(de writeHistory NIL
   (let (FileName (pack (timeStamp) "-pil-history.l"))
      (out FileName (mapc prinl (history)))
      FileName ) )

# Load a saved REPL history and append to current history
(de readHistory (FileName)
   (let
      (savedHistory
         (if (info FileName)
            (in FileName (make (while (line T) (link @)))) )
         currentHistory (history) )
      (history (if savedHistory (consFlatten (cons currentHistory savedHistory)) currentHistory))
      (length savedHistory) ) )


