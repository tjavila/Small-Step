import Estado


data AExp = Num Int
     |Var String
     |Som AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
     | Leq  AExp AExp
   deriving(Show)

data CExp =    While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Skip
   deriving(Show)                



aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2,s)
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Sub ef e2,s)
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Mul(Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Mul ef e2,s)


interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False



bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE,s)      = (TRUE,s)
bSmallStep (Not TRUE,s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)
bSmallStep (Ig (Num x) (Num y),s) = if (x == y) then (TRUE, s) else (FALSE, s)
bSmallStep(Ig (Num x) e2, s) = let (bn,sn) = aSmallStep(e2, s)
								in (Ig (Num x) bn, sn)
bSmallStep(Ig e1 e2, s) = let (en,sn) = aSmallStep(e1,s)
								in (Ig en e2, sn)
bSmallStep (Leq (Num x) (Num y),s) = if (x <= y) then (TRUE, s) else (FALSE, s)
bSmallStep(Leq (Num x) e2, s) = let (bn,sn) = aSmallStep(e2, s)
								in (Leq (Num x) bn, sn)
bSmallStep(Leq e1 e2, s) = let (en,sn) = aSmallStep(e1,s)
								in (Leq en e2, sn)

bSmallStep (Or TRUE b2,s) = (TRUE, s)
bSmallStep(Or FALSE b2, s) = (b2, s)
bSmallStep(Or b1 b2, s) = let (bn,sn) = bSmallStep(b1,s)
								in (Or bn b2, sn)


interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False




cSmallStep :: (CExp,Estado) -> (CExp,Estado)
cSmallStep (If TRUE c1 c2,s) = (c1, s)
cSmallStep (If FALSE c1 c2,s) = (c2, s)
cSmallStep (If b c1 c2,s) = let(bn,sn) = bSmallStep(b,s)
							in cSmallStep(If bn c1 c2, sn)
cSmallStep (Seq Skip c2,s)  = (c2,s)
cSmallStep (Seq c1 c2,s)  = let(cn,sn) = cSmallStep(c1,s)
							in cSmallStep(Seq cn c2, sn)
cSmallStep (Atrib (Var x) (Num y) ,s) = (Skip, mudaVar s x y)
cSmallStep (Atrib (Var x) e ,s) = let(en,sn) = aSmallStep(e,s)
								  in cSmallStep(Atrib (Var x) en, sn)

cSmallStep(While b c,s) = if let (bn,sn) = interpretB(b,s)
							 in isFinalAux(bn)
							 then cSmallStep(Seq c (While b c),s)
							 else (Skip,s)



interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep(c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC _ = False

isFinalAux :: BExp -> Bool
isFinalAux TRUE = True
isFinalAux FALSE = False

meuEstado :: Estado
meuEstado = [("x",10), ("y",0)]


exemplo :: AExp
exemplo = Mul (Num 3) (Sub (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = Or (Ig (Num 2) (Var "x")) (And (Not (TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])


--- TESTES


testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

trab:: CExp
trab = While((Leq(Var "x")(Num 10)))
		(Seq(Atrib(Var "x")(Som(Var"x")(Num 1)))
			(Atrib(Var "y")(Som(Var"y")(Num 1))))