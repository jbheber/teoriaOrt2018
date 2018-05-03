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

evalCD :: Exp -> ExpCD
evalCD = \e -> case e of { 
        C c -> CCD c [];
        Lam z y -> LamCD z y;
        Rec i exp -> evalCD (sust_mult [(i, exp)] (Rec i exp));
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
