-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Utilities that don't make a lot of sense.
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

-- | The same as '&', but lovelier
(|>) :: a -> (a -> b) -> b
(|>) = (&)

-- | The same as '$', but lovelier
(<|) :: (a -> b) -> a -> b
(<|) = ($)

-- | Connect a series of 'Op'
chain :: [Op a] -> Op a
chain = foldr (.) id
