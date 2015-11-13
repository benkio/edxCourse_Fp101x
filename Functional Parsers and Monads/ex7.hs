import Parsing

comment1
  = do string "--"
       sat (/= '\n')
       return ()

comment2
  = do string "--"
       many (sat (/= '\n'))

comment3
  = do string "--"
       sat (== '\n')
       return ()

comment4
  = do string "--"
       many (sat (/= '\n'))
       return ()
