double :: Int -> Int
double x = x + x

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

listlength :: List a -> Int
listlength Nil = 0
listlength (Cons _x xs) = 1 + listlength xs

newtype Q u = Q Double
  deriving (Show)

instance Num (Q u) where
  Q x + Q y = Q (x + y)
  Q x * Q y = Q (x * y)
  abs (Q x) = Q (abs x)
  signum (Q x) = Q (signum x)
  fromInteger x = Q (fromInteger x)
  negate (Q x) = Q (negate x)

square :: Q u -> Q u
square x = x * x

data Mass

data Joules

data Velocity

someMass :: Q Mass
someMass = 123

someVelocity :: Q Velocity
someVelocity = 9001

energy :: Q Mass -> Q Velocity -> Q Joules
energy (Q m) (Q v) =
  Q (0.5 * m * (v * v))
