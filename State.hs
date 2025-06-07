-- This module implements a Brainfuck interpreter and a function for identifying loops
-- The tape is infinite in both directions and the cells are 8 bit and warp

module State (
	State,		-- :: *
	toState,	-- :: String -> State
	halted,		-- :: State -> Bool
	step,		-- :: State -> State
	isLooping,	-- :: State -> State -> Bool
) where

data State = State {
	steps :: !Int,
	program :: !Program,
	tape :: !Tape
}

data Program = Program {
	programPtr :: !Int,
	leftP :: !String,	-- commands to the left of the program pointer (reversed, this is a stack)
	command :: !Char,
	rightP :: !String	-- commands to the right of the program pointer
}

data Tape = Tape {
	dataPtr :: !Int,
	leftT :: ![Cell],	-- this is a stack, the first cell in this list is the closest to the data pointer
	cell :: !Cell,
	rightT :: ![Cell]
}

data Cell = Cell {
	value :: !Int,
	lastVisited :: !Int
}

instance Show State where
	show state = show (steps state) ++ ":\t" ++ show (program state) ++ "\t" ++ show (tape state)

instance Show Program where
	show program = reverse (leftP program) ++ " $ " ++ [command program] ++ rightP program

instance Show Tape where
	show tape = unwords (map show $ reverse $ leftT tape) ++ " {" ++ show (cell tape) ++ "} " ++ unwords (map show $ rightT tape)

instance Show Cell where
	show = show . value

toState :: String -> State
toState code = State {steps = 0, program = toProgram code, tape = newTape}

toProgram :: String -> Program
toProgram code = Program {programPtr = 0, leftP = "", command = head code, rightP = tail $ code}

newTape :: Tape
newTape = Tape {dataPtr = 0, leftT = [], cell = Cell 0 0, rightT = []}


halted :: State -> Bool
halted state = command (program state) == 'H'

step :: State -> State
step state = State {steps = stepNumber, program = nextInstruction isCellZero $ program state, tape = nextTape}
	where
		isCellZero = (value $ cell $ tape state) == 0
		stepNumber = steps state + 1
		nextTape = setLastVisited stepNumber $ case command (program state) of
			'+' -> increment $ tape state
			'-' -> decrement $ tape state
			'<' -> shiftLeft $ tape state
			'>' -> shiftRight $ tape state
			'.' -> tape state
			'[' -> tape state
			']' -> tape state

setLastVisited :: Int -> Tape -> Tape
setLastVisited stepNumber tape = tape {cell = (cell tape) {lastVisited = stepNumber}}

increment :: Tape -> Tape
increment tape = tape {cell = (cell tape) {value = (value (cell tape) + 1) `mod` 256}}

decrement :: Tape -> Tape
decrement tape = tape {cell = (cell tape) {value = (value (cell tape) - 1) `mod` 256}}

shiftLeft :: Tape -> Tape
shiftLeft tape
	| null $ leftT tape = Tape {
			dataPtr = dataPtr tape - 1,
			leftT = [],
			cell = Cell 0 0,
			rightT = cell tape : rightT tape
		}
	| otherwise = Tape {
			dataPtr = dataPtr tape - 1,
			leftT = tail $ leftT tape,
			cell = head $ leftT tape,
			rightT = cell tape : rightT tape
		}

shiftRight :: Tape -> Tape
shiftRight tape
	| null $ rightT tape = Tape {
			dataPtr = dataPtr tape + 1,
			leftT = cell tape : leftT tape,
			cell = Cell 0 0,
			rightT = []
		}
	| otherwise = Tape {
			dataPtr = dataPtr tape + 1,
			leftT = cell tape : leftT tape,
			cell = head $ rightT tape,
			rightT = tail $ rightT tape
		}

-- the first argument is True if the current cell is 0 and False if it isn't
nextInstruction :: Bool -> Program -> Program
nextInstruction _		p@(Program ptr left 'H' right) = p
nextInstruction True	p@(Program ptr left '[' right) = findRightBracket 0 $ moveRight p
nextInstruction False	p@(Program ptr left ']' right) = findLeftBracket 0 $ moveLeft p
nextInstruction _		p@(Program ptr left cmd right) = moveRight p

-- increments the program pointer
moveRight :: Program -> Program
moveRight (Program ptr left cmd right)
	| null right	= Program (ptr + 1) (cmd : left) 'H' []
	| otherwise		= Program (ptr + 1) (cmd : left) (head right) (tail right)

-- decrements the program pointer
moveLeft :: Program -> Program
moveLeft (Program ptr left cmd right) = Program (ptr - 1) (tail left) (head left) (cmd : right)

-- the first argument is the depth, i.e. how many loops it needs to exit to find the correct ']'
findRightBracket :: Int -> Program -> Program
findRightBracket 0     p@(Program ptr left ']' right) = moveRight p
findRightBracket depth p@(Program ptr left '[' right) = findRightBracket (depth + 1) $ moveRight p
findRightBracket depth p@(Program ptr left ']' right) = findRightBracket (depth - 1) $ moveRight p
findRightBracket depth p@(Program ptr left cmd right) = findRightBracket depth $ moveRight p

-- the first argument is the depth, i.e. how many loops it needs to exit to find the correct '['
findLeftBracket :: Int -> Program -> Program
findLeftBracket 0     p@(Program ptr left '[' right) = moveRight p
findLeftBracket depth p@(Program ptr left '[' right) = findLeftBracket (depth - 1) $ moveLeft p
findLeftBracket depth p@(Program ptr left ']' right) = findLeftBracket (depth + 1) $ moveLeft p
findLeftBracket depth p@(Program ptr left cmd right) = findLeftBracket depth $ moveLeft p


-- detects whether state2 is close enough to state1 to guarantee that it will loop
-- state2 is assumed to be a future state of state1
isLooping :: State -> State -> Bool
isLooping state1 state2
	| programPtr (program state1) /= programPtr (program state2)	= False
	| otherwise = case compare (dataPtr $ tape state2) (dataPtr $ tape state1) of
		-- moving to the left
		LT -> isLoopingLeft (steps state1) (steps state2) (tape state1) (tape state2)
		-- staying in place
		EQ -> isLoopingInPlace (steps state1) (steps state2) (tape state1) (tape state2)
		-- moving to the right
		GT -> isLoopingRight (steps state1) (steps state2) (tape state1) (tape state2)

isLoopingLeft :: Int -> Int -> Tape -> Tape -> Bool
isLoopingLeft steps1 steps2 tape1 tape2
	| getAge steps1 (cell tape1) /= getAge steps2 (cell tape2)						= False	-- different current cells
	| map (getAge steps1) (leftT tape1) /= map (getAge steps2) (leftT tape2)		= False	-- different left tapes
	| recentCells steps1 dt (rightT tape1) /= recentCells steps2 dt (rightT tape2)	= False	-- different recent right tapes
	| otherwise																		= True	-- the tapes are similar enough
	where
		dt = steps2 - steps1

isLoopingInPlace :: Int -> Int -> Tape -> Tape -> Bool
isLoopingInPlace steps1 steps2 tape1 tape2
	| getAge steps1 (cell tape1) /= getAge steps2 (cell tape2)						= False	-- different current cells
	| recentCells steps1 dt (leftT  tape1) /= recentCells steps2 dt (leftT  tape2)	= False	-- different recent left tapes
	| recentCells steps1 dt (rightT tape1) /= recentCells steps2 dt (rightT tape2)	= False	-- different recent right tapes
	| otherwise																		= True	-- the tapes are similar enough
	where
		dt = steps2 - steps1

isLoopingRight :: Int -> Int -> Tape -> Tape -> Bool
isLoopingRight steps1 steps2 tape1 tape2
	| getAge steps1 (cell tape1) /= getAge steps2 (cell tape2)						= False	-- different current cells
	| recentCells steps1 dt (leftT  tape1) /= recentCells steps2 dt (leftT  tape2)	= False	-- different recent left tapes
	| map (getAge steps1) (rightT tape1) /= map (getAge steps2) (rightT tape2)		= False	-- different right tapes
	| otherwise																		= True	-- the tapes are similar enough
	where
		dt = steps2 - steps1

-- given the current time t, compute how long it has been since the cell was visited for the last time
-- returns (<value>, <age>)
getAge :: Int -> Cell -> (Int, Int)
getAge t cell = (value cell, t - lastVisited cell)

-- given an interval dt, find all the cells that have been visited in that interval and return their ages
recentCells :: Int -> Int -> [Cell] -> [(Int, Int)]
recentCells t dt = takeWhile ((<= dt) . snd) . map (getAge t)
