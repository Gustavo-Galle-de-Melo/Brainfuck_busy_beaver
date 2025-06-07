-- Runs the programs in programs_n.txt and evaluates whether they halt or not

import Data.Bits ((.&.))

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Programs
import State

data Result
	= Halts !Int	-- Halts <n> = halts in n steps
	| Loops	!Int	-- Loops <n> = the state reached in n steps has already happened (or it was close enough)
	| IDK
	deriving (Eq, Show)

-- given a result of Loops <n> we can deduce the cycle length to be n - <largest power of 2 less than n>
-- so Loops 42 would imply a cycle of 42-32=10 steps, for example

-- runs the program for <limit> steps and returns whatever it finds out
eval :: Int -> String -> Result
eval limit code = let state = toState code in brent limit 1 state (step state)

-- brent's cycle detection algorithm
-- can detect any loop with a constant cycle length
brent :: Int -> Int -> State -> State -> Result
brent limit n tortoise hare
	| n > limit					= IDK
	| halted hare				= Halts n
	| isLooping tortoise hare	= Loops n
	| isPow2 n					= brent limit (n + 1) hare (step hare)
	| otherwise					= brent limit (n + 1) tortoise (step hare)

isPow2 :: Int -> Bool
isPow2 n = n .&. (n - 1) == 0

main :: IO ()
main = do
	args <- getArgs
	case args of
		[len', limit'] -> do

			let len = read len'
			let limit = read limit'

			let programFile = "programs_" ++ show len ++ ".txt"
			let todoFile = "TODO_" ++ show len ++ ".txt"
			let recordFile = "record_" ++ show len ++ ".txt"

			programs <- lines <$> readFile programFile
			writeFile todoFile ""
			writeFile recordFile ""

			filterPrograms todoFile recordFile limit 0 1 programs
		_ -> putStrLn "Use: .\BF_BB <program length> <evaluation limit>"

filterPrograms :: String -> String -> Int -> Int -> Int -> [String] -> IO ()
filterPrograms todoFile recordFile limit record n [] = putStrLn "Done"
filterPrograms todoFile recordFile limit record n (p : ps) = do
	putStr $ show n ++ "\t" ++ p ++ "\t"
	hFlush stdout

	let result = eval limit p
	print result

	case result of

		Halts k -> do
			if k > record then do
				writeFile recordFile $ p ++ "\t" ++ show k ++ "\n"
				filterPrograms todoFile recordFile limit k (n + 1) ps
			else if k == record then do
				appendFile recordFile $ p ++ "\t" ++ show k ++ "\n"
				filterPrograms todoFile recordFile limit k (n + 1) ps
			else do
				filterPrograms todoFile recordFile limit record (n + 1) ps

		Loops _ -> do
			filterPrograms todoFile recordFile limit record (n + 1) ps

		IDK -> do
			appendFile todoFile $ p ++ "\n"
			filterPrograms todoFile recordFile limit record (n + 1) ps
