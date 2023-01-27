# chessOS
https://en.wikipedia.org/wiki/Swiss-system_tournament

## Swiss bracket
* Fixed number of rounds
* With odd number of players, lowest running player gets a bye (full point)
* Cannot play a given opponent more than once
* Cannot receive more than one bye
___
## Monrad pairings
* Before first round, assign all players a random starting number
* Each round, sort players first by score, then by starting number
* #1 plays #2, #3 plays #4, and so on
* Adjustments made to adhere to Swiss rules
___
## Chess specifics
* Aim for players to play equal games as white as black
* Alternating colors is ideal
* Cannot play the same color 3 rounds in a row
___
## Bracket generation improvements
* At round 1, simply create the bracket as (#1,#2),(#3,#4),...
* Instead of generating all pairings and then filtering out legal ones, filter by legality while generating all pairings
* Stop generating duplicate pairing sequences, i.e. pairing (Pano,Ian) then (Michael,Chany) is the same as first pairing (Michael,Chany) then (Pano,Ian)
* When generating all pairings, don't specify who is white and who is black, allow either player to be either color, unless one player must play a color, then specify that the colors are locked

* Also, ELOs should be updated all at once