module Problems.Class where

-- | A class for problems that can be solved.
class Problem a where
  -- | Solve the problem with the given input.
  solve :: a -> String -> String

data Problem_ = forall a. Problem a => Problem_ a

instance Problem Problem_ where
  solve (Problem_ x) = solve x
