
class ToFields a where
  toFields :: a -> [Field]

class
  ToFields a =>
  NonoData a
  where
  fromFields :: [Field] -> a

convert :: (ToFields a, NonoData b) => a -> b
convert = fromFields . toFields

instance ToFields [Field] where
  toFields = id

instance NonoData [Field] where
  fromFields = id

data Block
  = Block
      { b_size :: Int,
        space :: (Bool, Bool)
      }
  | Open
      { b_size :: Int
      }
  | Sep
      { b_size :: Int
      }
  deriving (Show)

data FieldsView
  = REP
      { v_type :: Field,
        v_size :: Int,
        v_rest :: [Field]
      }
  | OP
      { v_size :: Int,
        v_rest :: [Field]
      }

fieldsView :: [Field] -> FieldsView
fieldsView (O : P : xs) = let (l, r) = span (== P) xs in OP (length l + 1) r
fieldsView (O     : xs) = case span (== O) xs of
  (l, P : r) -> REP O (length l) (O : P : r)
  (l, r    ) -> REP O (length l + 1) r
fieldsView (x : xs) = let (l, r) = span (== x) xs in REP x (length l + 1) r

instance ToFields [Block] where
  toFields (Block { b_size, space = (l, r) } : xs) =
    (O <$ guard l) ++ replicate b_size P ++ (O <$ guard r) ++ toFields xs
  toFields (Sep { b_size }  : xs) = replicate b_size X ++ toFields xs
  toFields (Open { b_size } : xs) = replicate b_size O ++ toFields xs

instance NonoData [Block] where
  fromFields []                         = []
  fromFields (fieldsView -> REP X s xs) = Sep s : fromFields xs
  fromFields (fieldsView -> REP O s xs) = Open s : fromFields xs
  fromFields (fieldsView -> b         ) = do
    let l = case b of
          REP{} -> False
          OP{}  -> True
    let (r, bs) = case v_rest b of
          O : P : xs -> (False, P : xs)
          O     : xs -> (True, xs)
          xs         -> (False, xs)
    Block (v_size b) (l, r) : fromFields bs

iff :: Monoid a => Bool -> a -> a
iff True  = id
iff False = const mempty

instance NonoAttr Block where
  isOpen (Open _) = True
  isOpen _        = False
  isSep (Sep _) = True
  isSep _       = False
  isBlock Block{} = True
  isBlock _       = False

data Joint
  = JOpen
      { maxCap :: Int
      }
  | JBlock
      { maxCap :: Int,
        minCap :: Int,
        offset :: Int
      }
  | JSep
      { width :: Int
      }
  deriving (Show)

instance NonoAttr Joint where
  isOpen JOpen{} = True
  isOpen _       = False
  isSep JSep{} = True
  isSep _      = False
  isBlock JBlock{} = True
  isBlock _        = False

join :: [Block] -> [[Joint]]
join []           = return []
join (Sep s : xs) = (JSep s :) <$> join xs
join (x : xs) =
  let (l, r) = break isSep xs
      joins  = scanl join2 (asJoint x) l
      bs     = tails $ l ++ r
  in  concat $ zipWith (\a b -> (a :) <$> join b) joins bs

incIf :: Num a => Bool -> a -> a
incIf True = (+ 1)
incIf _    = id

asJoint :: Block -> Joint
asJoint (Open s) = JOpen s
asJoint (Block s (l, r)) =
  JBlock { maxCap = (incIf r . incIf l) s, minCap = s, offset = incIf l 0 }

join2 :: Joint -> Block -> Joint
join2 JOpen { maxCap } Block { b_size, space = (True, r) } = JBlock
  { maxCap = incIf r maxCap + b_size + 1
  , minCap = b_size
  , offset = maxCap + 1
  }
join2 b Open { b_size } = b { maxCap = maxCap b + b_size }
-- Block has (possibly hidden) left margin by construction
join2 b@JBlock { offset, maxCap } Block { b_size, space = (_, r) } = b
  { maxCap = incIf r maxCap + b_size + 1
  , minCap = b_size + 1 + maxCap - offset
  }

extendLeft :: [Int] -> [Joint] -> Maybe [Field]
extendLeft [] j | isNull $ find isBlock j = return $ concatMap conv j
                | otherwise               = Nothing
extendLeft _ [] = Nothing
extendLeft (i : is) (JBlock { minCap, maxCap, offset } : js)
  | i > maxCap || i < minCap = Nothing
  | otherwise = do
    let sk = iff (i < offset + minCap) . Sum $ offset + minCap - i
    let rest = maxCap - i - getSum sk
    ex <- extendLeft is ([ JOpen (rest - 1) | rest > 1 ] ++ js)
    return $ replicate (getSum sk) X ++ replicate i P ++ [ X | rest > 0 ] ++ ex
extendLeft is (JSep { width } : js) =
  (replicate width X ++) <$> extendLeft is js
extendLeft (i : is) (JOpen { maxCap } : js)
  | i > maxCap = (replicate maxCap X ++) <$> extendLeft (i : is) js
  | otherwise = do
    r <- extendLeft is ([ JOpen $ maxCap - i - 1 | maxCap > i + 1 ] ++ js)
    return $ replicate i P ++ [ X | maxCap > i ] ++ r

conv :: Joint -> [Field]
conv JSep { width }   = replicate width X
conv JOpen { maxCap } = replicate maxCap O

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) (a, b) = (f a, g b)

idify :: [Field] -> [(Field, Int)]
idify = concat . snd . mapAccumL f 0 . group
  where f i x = let j = incIf (P == head x) i in (j, (, j) <$> x)


leftMost :: [Int] -> [Field] -> Maybe [Field]
leftMost task =
  getFirst . foldMap (First . extendLeft task) . join . fromFields
