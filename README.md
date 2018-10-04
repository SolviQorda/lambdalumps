# Lambdalumps

LambdaLumps is a Tetris clone written in Haskell, structured in a way that the game model is independent of any rendering packages.

The source files do also contain implementation for the game in [Gloss](https://hackage.haskell.org/package/gloss), and an implementation using [Gloss-Rhine](https://hackage.haskell.org/package/rhine-gloss).

Tutorial coming soon!

# Installation (Linux and OSX)

1. `git clone https://github.com/SolviQorda/LambdaLumps`

2. `cd LambdaLumps`

3. Ensure that [Haskell](https://www.haskell.org/platform) is installed (core). Cabal is included in the Haskell platform.

4. `cabal sandbox init`

5. `cabal update`

4. `cabal install --only-dependencies` (if you already have cabal packages installed, you might get a recommendation to use the --force-reinstalls flags)

5. `cabal build`

6. `cabal run lambdalumps`

Special thanks to [Manuel Bärenz](https://github.com/turion), [Avery Lychee](https://github.com/AveryLychee) and [Becky Conning](https://github.com/beckyconning) for support and contributions.
