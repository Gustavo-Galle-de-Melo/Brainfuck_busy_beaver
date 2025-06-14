-- This module lists promissing candidates for busy beavers
-- all tests performed here are done based only on the code itself, nothing gets executed yet

module Programs (
	generateProgramFile, -- :: Int -> IO ()
) where

import Data.List (isInfixOf)
import System.Environment (getArgs)

main :: IO ()
main = do
	args <- getArgs
	case args of
		[len'] -> do
			let len = read len'
			generateProgramFile len
		_ -> putStrLn "Use: .\\Programs <program length>"

generateProgramFile :: Int -> IO ()
generateProgramFile len = writeFile ("programs_" ++ show len ++ ".txt") $ unlines $ listPrograms len


-- lists promissing programs of a given length
listPrograms :: Int -> [String]
listPrograms len = filter endFilter $ filter midFilter $ map ('+' :) $ createList 0 (len - 1)

createList :: Int -> Int -> [String]
createList 1 1 = ["]"] -- the program must end in ']', any commands after the last loop could be a '.' and be inside the loop
createList _ 1 = []
createList depth len
	| depth < 0	= []
	| depth > len	= [] -- not enough characters to close all loops
	| otherwise		= do
		c <- "+-<>.[]"
		case c of
			'['	-> filter midFilter $ map (c :) $ createList (depth + 1) (len - 1)
			']'	-> filter midFilter $ map (c :) $ createList (depth - 1) (len - 1)
			_	-> filter midFilter $ map (c :) $ createList depth (len - 1)

-- filter that will be applied at each step of the generation of the code
-- used to filter out things that shouldn't happen anywhere in the program
-- True = keep, False = discard
midFilter :: String -> Bool
midFilter ('+' : '-' : _) = False	-- could have been ..
midFilter ('-' : '+' : _) = False	-- could have been ..
midFilter ('<' : '>' : _) = False	-- could have been ..
midFilter ('>' : '<' : _) = False	-- could have been ..
midFilter ('[' : ']' : _) = False	-- infinite loop
midFilter (']' : '[' : _) = False	-- unenterable loop
midFilter ('+' : '.' : _) = False	-- could have been .+
midFilter ('-' : '.' : _) = False	-- could have been .-
midFilter ('<' : '.' : _) = False	-- could have been .<
midFilter ('>' : '.' : _) = False	-- could have been .>
midFilter (']' : '.' : _) = False	-- could have been .]
midFilter ('.' : ']' : _) = False	-- implies that there is a [.], so this is an infinite loop
-- uncomment this line to implement another filter that was not included in the pre-computed results
--midFilter ('.' : '[' : _) = False	-- implies that there is a [.[, so it could have been [[.
midFilter _ = True

-- filter that will applied once after the code is complete
-- used to filter out things that are only forbidden in specific places/quantities
-- True = keep, False = discard
endFilter :: String -> Bool
endFilter code
	| isLeftHanded code						= False -- the first shift could have been to the right
	| multipleNopGroups code				= False	-- could have been a single group of nops
	| foldl gcd 0 (shiftSizes code) > 1		= False	-- does not use the tape in the most efficient way
	| impossibleToHalt code					= False	-- always reaches the last ']' with the same cell value
	| uselessLoop code						= False -- in situations like [b [a...]a ]b and [b [a...]c ]a the [a ]a pair can be removed
	| inefficientBinary code				= False -- if the only modified cells are zeroes, then we can assume all
													-- non-zero cells are 1 and discard programs that write othher values
	| otherwise								= True

isLeftHanded :: String -> Bool
isLeftHanded "" = False
isLeftHanded ('<' : _) = True
isLeftHanded ('>' : _) = False
isLeftHanded (_ : code) = isLeftHanded code

multipleNopGroups :: String -> Bool
multipleNopGroups code = '.' `elem` dropWhile (== '.') (dropWhile (/= '.') code)

shiftSizes :: String -> [Int]
shiftSizes "" = []
shiftSizes code@('<' : _) = length (takeWhile (== '<') code) : shiftSizes (dropWhile (== '<') code)
shiftSizes code@('>' : _) = length (takeWhile (== '>') code) : shiftSizes (dropWhile (== '>') code)
shiftSizes code = shiftSizes $ tail code

impossibleToHalt :: String -> Bool
impossibleToHalt code
	| not $ ']' `elem` end		= False
	| '[' `elem` betweenLoops	= False
	| otherwise					= length (filter (== '<') betweenLoops) == length (filter (== '>') betweenLoops)
	where
		end = tail $ reverse code
		betweenLoops = takeWhile (/= ']') end

uselessLoop :: String -> Bool
uselessLoop "" = False
uselessLoop ('[' : '[' : code) = uselessLoop' 0 '[' code || uselessLoop code
	where
		uselessLoop' _ _ ""					= False
		uselessLoop' d _ ('[' : code)		= uselessLoop' (d + 1) '[' code
		uselessLoop' 0 ']' (']' : code)		= True	-- [b [a ... ]c ]a
		uselessLoop' 0 _ (']' : ']' : code)	= True	-- [b [a ... ]a ]b
		uselessLoop' d _ (']' : code)		= uselessLoop' (d - 1) ']' code
		uselessLoop' d _ (cmd : code)		= uselessLoop' d cmd code
uselessLoop (_ : code) = uselessLoop code

inefficientBinary :: String -> Bool
inefficientBinary code
	| any (`isInfixOf` code) ["<+", ">+", ".+", "[+", "<-", ">-", ".-", "[-"]	= False	-- modifies non-zero cell
	| '-' `elem` code		= True -- writes a number <=-1
	| "++" `isInfixOf` code	= True -- writes a number >=2
	| otherwise				= False
