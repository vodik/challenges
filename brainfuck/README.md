# Brainfuck

A [Brainfuck][bf] interpreter written in Haskell.

Uses a `MachineT` monad to track memory and output and to be haltable.
This interpreter has been extended to understand `#` to mark
breakpoints.

Two different memory implementations are provided. `Tape` is
implemented in a simple zipper. `Sparse` uses as map and only
represents cells with values and is considerably slower.

  [bf]: http://esolangs.org/wiki/Brainfuck

## Install

	runhaskell Setup configure --prefix=$HOME --user
	runhaskell Setup build
	runhaskell Setup install
