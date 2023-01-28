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
* First round rapid bracket generation
* Early prunning by determining legality during pairing generation
* Lazy color selection
* Stop generating duplicate pairing sequences, i.e. pairing (Pano,Ian) then (Michael,Chany) is the same as first pairing (Michael,Chany) then (Pano,Ian)
___
## Technical debt list
* Instead of using Option[Outcome], just add Bye as a case of Outcome
