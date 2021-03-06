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

run :: Mem -> Prog -> Mem
run m [] = m
run m (p:ps) = case (step m (p:ps)) of {
    (m2, p2) -> run m2 p2;
};
---
-- Funciones
false :: Exp
false = C "F" []

true :: Exp
true = C "V" []

cero :: Val
cero = CV "O" []

uno :: Val
uno = CV "S" [cero]

dos :: Val
dos = CV "S" [uno]

listaVacia :: Val
listaVacia = CV "[]" []

listaUnElem :: Val
listaUnElem = CV ":" [(CV "a" []), (listaVacia)]

listaDosElem :: Val
listaDosElem = CV ":" [(CV "b" []), (listaUnElem)]

memoriaTest :: Mem
memoriaTest = [("n", dos), ("m", uno), ("xs", listaDosElem), ("b", CV "T" [])]

notB :: Prog
notB = [Case "b" [("V", ([], [Ass [("b", false)]])), ("F", ([], [Ass [("b", true)]]))]]

par :: Prog
par = [
    Ass [("b", true)], 
    While "n" [
        ("S", 
            (["x"], 
                (notB ++ [Ass [("n", V "x")]])
            )
        )
    ]]

mas :: Prog
mas = [
    While "n" [
        ("S", 
            (["x"], 
                [Ass [("m", C "S" [V "m"]),("n", V "x")]]
            )
        )
    ]]

largo :: Prog
largo = [
    Ass [("ret", C "O" [])],
    While "xs" [
        (":",
            (["y","ys"],
                [ Ass [("ret", C "S" [V "ret"]), ("xs", V "ys")]]
            )
        )
    ]]

igualdad :: Prog
igualdad = [
    Ass [("b", true)],
    Case "m" [
        ("O", 
            ([], [(Case "n" [
                    ("O",
                        ([], [Ass [("b", true)]])
                    ),
                    ("S",
                        (["x"], [Ass [("b", false)]])
                    )
                ])
            ])
        ),
        ("S",
            (["x"], [
                While "m" [
                    ("S",
                        (["y"], [
                            (Case "n" [
                                ("O",
                                    ([], [
                                        Ass [("b", false)],
                                        Ass [("m", C "O" [])]
                                    ])
                                ),
                                ("S",
                                    (["z"], [
                                        Ass [("m", V "y")],
                                        Ass [("n", V "z")]
                                    ])
                                )
                            ]),
                            (Case "n" [
                                ("O",
                                    ([], [Ass [("b", V "b")]])
                                ),
                                ("S",
                                    (["t"], [Ass [("b", false)]])
                                )
                            ])
                        ])
                    )
                ]
            ])
        )
    ]]

---
