
Context: https://codegolf.stackexchange.com/q/282018/98257

The purpose of this repository is to provide pre-computed lists of candidates for Brainfuck busy beavers for lengths up to 13.

The files `programs_n.txt` contain the filtered programs that have not been executed yet. The `record_n.txt` files contain the programs that took the longest to halt within the step limit (in this case 20 000 000 steps). And the `TODO_n.txt` files contain the programs that need to be evaluated further.

To generate these lists again follow these steps:

1. Create an initial list of candidates using ".\Programs <program length>". Example:

	.\Programs 13

2. Evaluate each one of these programs up to <limit> steps using ".\BF_BB <length> <limir>". Example:

	.\BF_BB 13 20000000

3. Go manually through each program in `TODO_<length>.txt` and find out if/when they halt. If none of them halt then the busy beaver can be found in `record_<length>.txt`


An extra filter can be added by uncommenting line 58 of `Programs.hs`.
