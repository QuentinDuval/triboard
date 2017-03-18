# Triboard

This project is aimed at creating a small game based on the old Tribolo DOS game, whose description can be found here: http://www.mobygames.com/game/tribolo.

You can play the game at the following address: https://quentinduval.github.io/tribolo/.

You can learn how to implement such a game by reading the series of blog post starting with https://deque.blog/2017/02/27/building-a-clojurescript-game/.


## Rules

Three players (Blue, Red and Green) play one after the other on board made of 16 times 11 cells. Each player tries to convert the maximum number of cells to their own color.

A player can convert cells of another player by “jumping” over them: the player targets an empty cell that has a direct line of sight (vertical, horizontal or diagonal) with one cell that he already controls.

At each turn, a player must do a valid jump. If no valid jump are available, the player pass his/her turn and the game proceeds with the next player.



### Jumping

To be a valid jump, a line must only contain cells belonging to one single opponent. It means that Blue cannot convert a line if the line contains both Red and Green cells. In addition, the line cannot contain cells that are not owned by any player.

If multiple jumps are possible from a given empty cell, converting this empty cell triggers the conversion of the cells of each line jumped over. It means that Blue can convert at the same time a line containing exclusively Green cells and a line containing exclusively Red cells.

The game also features one special type of cells: walls. Walls cannot be converted and block line of sight, which means a player cannot jump over them.



### Start and end conditions

The game starts with each player owning 12 random cells. The game continues until no player can convert any more cells. The winner is the player owning the most cells at the end of the game.

Each game has a fixed number of 12 walls, randomly placed on the board at the start of the game. Their position will never evolve.



### Additional info

Tribolo is a single player game. Blue is controlled by the only human player of the game, while Red and Green are controlled by an artificial intelligence. We will have to develop at least a rudimentary AI to play the game.

Because the board is quite big, identifying available jump might sometimes be challenging. We will therefore add a help feature, to reveal the available moves for the the human player, as the original game does.

Finally, we want our human player to be able to cancel his last move or restart the same game from the start. We will add these features as actions available in a menu.
