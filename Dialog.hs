module Dialog where

data Dialog a = Answer a | Question String (String -> Dialog a)

instance Monad Dialog where
  return x = Answer x
  (Answer x) >>= f = f x
  (Question q cont) >>= f = Question q (\z -> cont z >>= f) -- (cont >=> f)

instance Functor Dialog where
  fmap f d = d >>= return . f

instance Show a => Show (Dialog a) where
  show (Answer x) = "Answer " ++ show x
  show (Question q _) = "Question " ++ show q

question :: String -> Dialog String
question q = Question q Answer

