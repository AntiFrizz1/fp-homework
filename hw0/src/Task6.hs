module Task6
       ( foo
       ) where

--Task 6

--distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
--WHNF=(Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing


--null $ mapMaybe foo "pole chudes ochen' chudesno"
--WHNF=False
