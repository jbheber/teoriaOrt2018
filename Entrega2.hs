data Exp = V Id | C Id | Lam [Id] Exp | App Exp [Exp] | Case Exp [Rama] | Rec Id Exp
    deriving (Show)

type Rama = (Id, (Id, Exp))

type Id = String