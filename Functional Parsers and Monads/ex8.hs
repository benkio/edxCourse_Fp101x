import Parsing

expr1
  = do n <- natural
       ns <- many
               ( do symbol "-"
                    natural)
       return (foldl (-) n ns)

expr2
  = do n <- natural
       symbol "-"
       n' <- natural
       return (n - n')

{-expr3 = do n <- natural
           ns <- many (do symbol "-"
                          natural)-}

expr4
  = do n <- natural
       symbol "-"
       e <- expr4
       return (e - n)
