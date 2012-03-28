# Brainfuck

A [brainfuck][bf] interpreter written in haskell.

Uses a `MachineT` monad to track memory and output and to be haltable.
This interpreter has been extended to understand `#` as a breakpoint.

Three different memory implementations are provided. `Tape` is a
simple zipper. `Sparse` uses as map and thus only represents cells
with values. `Sequence` is like `Tape` but implemented with
`Data.Sequence` rather than with Haskell lists.

  [bf]: http://esolangs.org/wiki/Brainfuck
