import Parsing

bind1 p f
  = P (\inp ->
         case parse p inp of
              [] -> []
              [(v,out)] -> parse (f v) inp)

bind2 p f
  = P (\inp ->
        case parse p inp of
             [(v,out)] -> parse (f v) out
             [] -> [] )

bind3 p f
  = P (\inp ->
        case parse (f inp) inp of
             [] -> []
             [(v,out)] -> parse p out)

{- bind4 p f
  = P (\inp ->
        case parse (f inp) inp of
             [] -> []
             (v : out) -> parse (f v) out)-}
