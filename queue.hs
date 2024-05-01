module Queue where
import Text.XHtml (base)
import Data.Bool (Bool)

type Queue1 a = ([a], [a])

emptyI = ([], [])

addI x (f, b) = flipQ (f, x:b)

isemptyI (f, b) = null f

frontI (x:f, b) = x

removeI (x:f, b) = flipQ (f, b)

flipQ ([], b) = (reverse b, [])
flipQ q = q

retrieve :: Queue1 Integer -> [Integer]
retrieve (f, b) = f ++ reverse b

invariant :: Queue1 Integer -> Bool
invariant (f, b) = not (null f) || null b