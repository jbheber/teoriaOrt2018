data Val = CV Id [Val] | Null
 deriving (Show, Eq)

data Exp = V Id | C Id [Exp]
 deriving (Show, Eq)

data Instr = Ass [(Id, Exp)] | Case Id [Rama] | While Id [Rama]
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
eval m (V id) = lookupMemory m id
eval m (C id exps) = CV id (map (eval m) exps)

step :: Mem -> Prog -> (Mem, Prog)
step m ((Ass as):ps) = (update m (map (\(id, exp) -> (id, eval m exp)) as), ps)
step m ((Case id ramas):ps) = case lookupMemory m id of {
    CV id2 vs -> case lookup id2 ramas of {
       Just (ids, prog) -> (update m (zip ids vs), prog ++ ps);
       Nothing -> error "El id no esta en las ramas"
    };
    Null -> error "La variable no se encuentra en memoria"
};
step m ((While id ramas):ps) = case lookupMemory m id of {
    CV id2 vs -> case lookup id2 ramas of {
        Just (ids, prog) -> ((update m (zip ids vs)), (prog++[(While id ramas)]++ps));
        Nothing -> (m, ps)
   };
   Null -> error "La variable no se encuentra en memoria"
};
---
