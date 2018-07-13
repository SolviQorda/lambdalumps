# Lambdalumps

LambdaLumps is a Tetris clone written in Haskell, structured in a way that the game model is independent of any rendering packages.

The source files do also contain implementation for the game in [Gloss](https://hackage.haskell.org/package/gloss), and an implementation using [Gloss-Rhine](https://hackage.haskell.org/package/rhine-gloss) is currently under construction (unstable).

# Installation

1. `git clone https://github.com/SolviQorda/LambdaLumps`

2. Ensure that Haskell and Cabal are installed

3. `cabal sandbox init

   `cabal update`

4. `cabal install --only-dependencies`

5. `cabal build`

6. `cabal run lambdalumps`


Recommended settings: gloss backend, and level 3 difficulty.
