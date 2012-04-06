# Brainfuck

A [Brainfuck][bf] interpreter written in Haskell.

Uses a `MachineT` monad to track memory and output and to be haltable.
This interpreter has been extended to understand `#` to mark
breakpoints.

Two different memory implementations are provided. `Tape` is
implemented in a simple zipper. `Sparse` uses a `Data.IntMap` to be
able to only represent cells with values. It is slower.

  [bf]: http://esolangs.org/wiki/Brainfuck

## Install

	runhaskell Setup configure --prefix=$HOME --user
	runhaskell Setup build
	runhaskell Setup install
