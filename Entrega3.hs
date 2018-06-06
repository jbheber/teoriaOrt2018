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

-- Memoria

lookupMemory :: Mem -> Id -> Val
lookupMemory [] id = Null
lookupMemory ((x,y):xs) id = if (x == id) then y else lookupMemory xs id

update :: Mem -> [(Id, Val)] -> Mem
update m ls = ls ++ m

---
--  Programa

eval :: Mem -> Exp -> Val
eval = \memory -> \e -> case e of {
    V id -> lookupMemory memory id;
    C id exps -> CV id (map (eval memory) exps);
};

---
notb :: Prog
notb = Case "b" [("True", ([], Ass [("b", C "False" [])])), ("False", ([], Ass [("b", C "True" [])]))]