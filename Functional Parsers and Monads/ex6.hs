import Parsing

int1 = char '-' >>= (\c -> nat >>= (\n -> (return (-n) +++ nat)))
-- int2 = (nat +++ char '-') >>= (\c -> nat >>= (\n -> return (-n)))
int3 = (do char '-'
           n <- nat
           return (-n)) +++ nat

int4 = (do char '-'
           nat) +++ nat 
