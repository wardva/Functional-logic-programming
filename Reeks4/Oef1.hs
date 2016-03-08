data BoolT = TrueT | FalseT

and' :: BoolT -> BoolT -> BoolT
and' TrueT TrueT = TrueT
and' _ _ = FalseT

or' :: BoolT -> BoolT -> BoolT
or' TrueT _ = TrueT
or' _ TrueT = TrueT
or' _ _ = FalseT

instance Eq BoolT where
  TrueT == TrueT = True
  FalseT == FalseT = True
  _ == _ = False

instance Show BoolT where
  show TrueT = "De waarde is waar!"
  show FalseT = "De waarde is onwaar!"
