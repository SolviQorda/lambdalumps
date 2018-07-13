# lambdalumps

Installation

(in a folder you've chosen for the game)

git clone https://github.com/SolviQorda/LambdaLumps

(if you see the warning git not found, use sudo apt-get install git
  you'll need   your password for this - then try the git clone command again)

cd lambdalumps

sudo apt-get install cabal

cabal sandbox init
cabal install --only-dependencies
cabal build

cabal run lambdalumps
