module Dialog where

data Dialog t a = 
  Answer a |
  Question t (t -> Dialog t a)

instance Monad (Dialog t) where
  return x = Answer x
  (Answer x) >>= f = f x
  (Question q cont) >>= f = Question q (\z -> cont z >>= f) -- (cont >=> f)

instance Functor (Dialog t) where
  fmap f d = d >>= return . f

instance (Show t, Show a) => Show (Dialog t a) where
  show (Answer x) = "Answer " ++ show x
  show (Question q _) = "Question " ++ show q

question :: t -> Dialog t t
question q = Question q Answer

