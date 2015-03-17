-- http://www.reddit.com/r/dailyprogrammer/comments/2z68di/20150316_challenge_206_easy_recurrence_relations/
module RecurrenceRelations where

type Operator = Integer -> Integer
type Expression = String

createExpression :: [Operator] -> Integer -> Integer
createExpression []     seed = seed
createExpression (x:xs) seed = createExpression xs (x seed)

parse :: Expression -> [Operator]
parse = map toOperator . words

toOperator :: String -> Operator
toOperator ('*':xs) = (* (read xs :: Integer))
toOperator ('-':xs) = ((-) (read xs :: Integer))
toOperator ('/':xs) = ((div) (read xs :: Integer))
toOperator ('+':xs) = (+ (read xs :: Integer))
toOperator _        = error "Malformed expression"

recurrence :: Expression -> Integer -> [Integer]
recurrence expr seed = iterate (createExpression $ parse expr) seed 


