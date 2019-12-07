withG :: (M.Map String a -> M.Map String a) -> G a -> G a
withG mf (G m) = G $ mf m