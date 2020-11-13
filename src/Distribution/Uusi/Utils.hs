module Distribution.Uusi.Utils
  ( (|>),
    (<|),
    chain,
  )
where

import Data.Function ((&))
import Distribution.Uusi.Types (Op)

infixl 1 |>

infixr 0 <|

-- qwq
(|>) :: a -> (a -> b) -> b
(|>) = (&)

(<|) :: (a -> b) -> a -> b
(<|) = ($)

chain :: [Op a] -> Op a
chain = foldr (.) id