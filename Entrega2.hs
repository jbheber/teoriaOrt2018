data Exp = V Id | C Id | Lam [Id] Exp | App Exp [Exp] | Case Exp [Rama] | Rec Id Exp
    deriving (Show)

type Rama = (Id, (Id, Exp))

type Id = String

data Val = CV Id [Val] | LamC [Id] Exp
    deriving (Show)

data Con = CC Id [Exp] | LamV [Id] Exp
 deriving (Show)

--Expresiones
exp1::Exp
exp1 = V "x"

exp2::Exp
exp2 = V "y"

exp3::Exp
exp3 = Case exp1 [("T", ([], C "F")), ("F", ([], exp2))]

exp4::Exp
exp4 = Lam ["x"] exp3

exp5::Exp
exp5 = Lam ["y"] exp3

