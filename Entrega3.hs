data Val = CV Id [Val] | Null
 deriving (Show, Eq)

data Exp = V Id | C Id [Exp]
 deriving (Show, Eq)

data Instr = Ass [(Id, Exp)] |Case Id [Rama] | While Id [Rama]
 deriving (Show, Eq)

type Prog = [Instr]
type Rama = (Id, ([Id], Prog))
type Mem = [(Id, Val)]
type Id = String

--Funciones auxiliares

notb :: Prog
notb = Case "b" [("True", ([], Ass [("b", C "False" [])])), ("False", ([], Ass [("b", C "True" [])]))]
       
pos :: Prog
pos = Case "n" [("O", ([], Ass [("x", C "False" [])])), ("S", (["y"], Ass [("z", C "True" [])]))]