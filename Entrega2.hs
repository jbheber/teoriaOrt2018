data Exp = V Id | C Id | Lam [Id] Exp | App Exp [Exp] | Case Exp [Rama] | Rec Id Exp
    deriving (Show)

type Rama = (Id, ([Id], Exp))

type Id = String

data ExpV = CV Id [ExpV] | LamV [Id] Exp
    deriving (Show)

data ExpCD = CCD Id [Exp] | LamCD [Id] Exp
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

false::Exp
false = C "False"

true::Exp
true = C "True"

sust_mult::[(Id, Exp)]->Exp->Exp
sust_mult xs (V id) = case (lookup id xs) of {
    Just t -> t;
    Nothing -> V id;
}
sust_mult xs (C id) = C id
sust_mult xs (Lam ids e1) = Lam ids (sust_mult (filter (\(id,exp) -> not (elem id ids)) xs) e1)
sust_mult xs (App e1 es) = App (sust_mult xs e1) (map (sust_mult xs) es)
sust_mult xs (Case e1 r) = Case (sust_mult xs e1) (map (sust_rama xs) r)
sust_mult xs (Rec id e1) = Rec id (sust_mult (filter (\(id2,exp) -> id2 /= id ) xs) e1)

sust_rama::[(Id, Exp)]->Rama-> Rama
sust_rama = \xs (id, (ids, exp)) -> (id, (ids, (sust_mult (filter (\(id2,exp2) -> not (elem id2 ids)) xs) exp)))

evalCD::Exp->ExpCD
evalCD = \e -> case e of { 
        C c -> CCD c [];
        Lam z y -> LamCD z y;
        Rec i exp -> evalCD (sust_mult [(i, e)] (exp));
        App e1 exps -> case (evalCD e1) of {
            LamCD ids e3 -> case (length ids == length exps) of {
                False -> error "no son del mismo largo";
                True -> evalCD (sust_mult (zip ids exps) e3);
            };
            CCD id exps2 -> CCD id (exps2++exps);
        };
        Case exp bs -> case (evalCD exp) of {
            CCD id exps -> case (lookup id bs) of {
                Just (ids, e2) -> case (length exps == length ids) of {
                    False -> error "las listas no son del mismo tamaño";
                    True -> evalCD (sust_mult (zip ids exps) e2);
                };
                Nothing -> error "error no esta en las ramas";
            };
            LamCD id x -> error "No evalua a un Constructor";
        };
        V y -> error "Variable libre";
}

evalV::Exp->ExpV
evalV = \e -> case (evalCD e) of {
    LamCD ids exp -> LamV ids exp;
    CCD id exps -> CV id (map evalV exps);
}

verificarRepetidos::Exp->Bool
verificarRepetidos (V id) = True
verificarRepetidos (C id) = True
verificarRepetidos (Lam ids exp) = verificarRepIds ids && verificarRepetidos exp
verificarRepetidos (App exp exps) = (verificarRepetidos exp) && checkArray (map (verificarRepetidos) exps)
verificarRepetidos (Case exp ramas) = (verificarRepetidos exp) && (verificarRepRamas ramas)
verificarRepetidos (Rec id exp) = verificarRepetidos exp

verificarRepIds::[Id]->Bool
verificarRepIds [] = True
verificarRepIds [_] = True
verificarRepIds (i:ids) = if elem i ids then False else verificarRepIds ids

verificarRepRamas::[Rama]->Bool
verificarRepRamas [] = True
verificarRepRamas ((id,(ids, exp)):rs) = (verificarRepIds ids) && (verificarRepetidos exp) && (verificarRepRamas rs)

checkArray::[Bool] -> Bool
checkArray [] = True
checkArray (x:xs) = x && checkArray xs

par::Exp
par = Rec "par" (Lam ["n"] (Case (V "n") [("O", ([], true)), ("S", (["x"], App notChi [(App (V "par") [V "x"])]))]))

notChi::Exp
notChi = Lam ["n"] (Case (V "n") [("True", ([], false)), ("False", ([], true))])

suma::Exp
suma = Rec "suma" (Lam ["n", "m"] (Case (V "m") [("O", ([], V "n")), ("S", (["x"], (App (C "S") [(App (V "suma") [V "n", V "x"])])))]))

largo::Exp
largo = Rec "largo" (Lam ["xs"] (Case (V "xs") [("[]", ([], C "O")), (":", (["z","zs"], (App (C "S") [(App (V "largo") [V "zs"])])))]))

mapChi::Exp
mapChi = Rec "mapChi" (Lam ["f", "xs"] (Case (V "xs") [("[]", ([], (C "[]"))), (":", (["z", "zs"], App (C ":") [App (V "f") [V "z"], App (V "mapChi") [V "f", V "zs"]]))]))

-- Auxiliares
naturalChi::Int->Exp
naturalChi 0 = C "O"
naturalChi n = App (C "S") [naturalChi(n-1)]

crearListaChi::[Int]->Exp
crearListaChi [] = C "[]"
crearListaChi (x:xs) = App (C ":") [naturalChi x, crearListaChi xs]