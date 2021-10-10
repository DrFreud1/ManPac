# ManPac

### Description

ManPac is a variant of the traditional PacMan game.

The reverse Pac-Man mechanic works as follows: The player
is in control of the ghost and he will guide him around the
board using the arrows of the keyboard, while the behavior
of Pac-Man will be determined by the PC. When the game
begins, Pac-Man will be on his pen, and he will immediately
go out to try to get the coins from the board while fleeing from
the ghost. The player’s objective will be to catch Pac-Man
before he eats all the coins. The difficulty of the game relies
on the fact that the board will be full of regularly distributed
coins, of which 18 are special and cause the ghost to slow
down. If Pac-Man fails to accomplish its mission, the player
will be directed to the next level; otherwise, the ghost dies
and the game ends. The final score is calculated based on the
number of coins remaining, the more coins remaining on the
board the higher the score will be.

### Execution

The game can be executed with cabal as follows: 
  cabal ManPac.cabal
  
