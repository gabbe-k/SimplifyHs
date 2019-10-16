exprToPoly (BExp MulOp (X exp) (N n)) = fromList (n : replicate exp 0)
exprToPoly (BExp MulOp (N n) (X exp)) = fromList (n : replicate exp 0)
exprToPoly (BExp op (N n) (N n2))     = fromList [(opActual op) n n2] 
  where opActual AddOp                = (+)
        opActual MulOp                = (*)
exprToPoly (BExp op (X n) (X n2))     | op == MulOp = exprToPoly (X (n+n2))
                                      | otherwise   = 
                                        fromList( 1 : ((replicate (deltaExp - 1) 0) ++ [1] 
                                        ++ (replicate (minExp) 0)))
  where deltaExp | n > n2    = n - n2
                 | otherwise = n2 - n
        minExp   = n `min` n2
exprToPoly (BExp AddOp e1 e2)           = fromList(zipWith (+) (exList eList1) (exList eList2)) 
  where exList list | (length list) == maxList = list
                    | (length list) < maxList = (replicate delta 0) ++ list
        maxList = (length(eList1)) `max` (length(eList2))
        minList = (length(eList1)) `min` (length(eList2))
        delta   = maxList - minList
        eList1  = toList(exprToPoly e1)
        eList2  = toList(exprToPoly e2)
exprToPoly (BExp MulOp e1 e2)           = fromList(zipWith (customMul) (exList eList1) (exList eList2)) 
  where exList list | (length list) == maxList = list
                    | (length list) < maxList = (replicate delta 0) ++ list
        maxList = (length(eList1)) `max` (length(eList2))
        minList = (length(eList1)) `min` (length(eList2))
        delta   = maxList - minList
        eList1  = toList(exprToPoly e1)
        eList2  = toList(exprToPoly e2)
        customMul i1 i2 | (i1 == 0) || (i2 == 0) = i1 + i2
                        | otherwise              = i1 * i2