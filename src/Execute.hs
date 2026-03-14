module Execute where
import Types
import qualified Data.Map as Map

-- execute em c para ficar muito otimizada

-- ----->
-- (Stack, Env, Code, Dump, Store)
execute :: Conf -> Conf
execute (s, e, ((LDC n): c), d, m) = ((I n):s, e, c, d, m)
execute (s, e, (LD i : c), d, m) =
    case drop i e of
        v:_ -> (v : s, e, c, d, m)
        []  -> error ("LD: invalid environment index " ++ show i)
execute (((I i1) : (I i2) : s), e, (ADD:c), d, m) = ((I (i2+i1)):s, e, c, d, m)
execute (((I i1) : (I i2) : s), e, (SUB:c), d, m) = ((I (i2-i1)):s, e, c, d, m)
execute (((I i1) : (I i2) : s), e, (MUL:c), d, m) = ((I (i2*i1)):s, e, c, d, m)
execute (s,e,(LDF f):c, d, m) =
    let a = nextAddr m
    in (A a : s, e, c, d, Map.insert a (f,e) m)
execute (v : A a : s,e, AP:c, d, m) =
    case (Map.lookup a m) of
        Just (c',e') -> ([], v:e', c', (s,e,c):d, m)
        Nothing -> error "did not find given address in mem"
execute (v : _, _, RTN : _, (s', e', c') : d, m) = (v : s', e', c', d, m)
execute (I 0 : s, e, (SEL c1 _) : c, d, m) = (s, e, c1, ([], [], c):d, m)
execute (I _ : s, e, (SEL _ c2) : c, d, m) = (s, e, c2, ([], [], c):d, m)
execute (s,e, JOIN:_, (_,_,c'):d, m) = (s,e,c',d, m)
execute (s,e,(LDRF c'):c,d,m) =
    let a = nextAddr m
        m'= Map.insert a (c', A a : e) m
    in (A a : s,e,c,d,m')
execute conf = error ("uncaught execute error: " ++ show conf)

-- -----> *
executeT :: Conf -> [Conf]
executeT = go
  where
    isFinal (_, _, c, _, _) = null c
    go conf
      | isFinal conf = [conf]
      | otherwise    = conf : go (execute conf)

nextAddr :: Store -> Addr
nextAddr store = 1 + Map.size store
