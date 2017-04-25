setup:
	Advanced Setup:
	each player:
		picks color
		lays down 2 settlements then two roads on board

Roll Seven:
	Players pick which cards they want to give up instead of just giving back random cards

ui code

.
.
.
.
.
.
.
..
.
.
.
.
.
.
.




GamePlay Flow:

Player turn starts:
main thread rolls dice, updates resources
main puts roll into rollVar
gui takes roll from rollvar
-- (do we even need MoveRobber in this case?????)

if rolls 7 then requestVar holds MoveRobber
	main thread waits for robberVar to hold the tile choice
	gui thread waits for gameVar to update
	main thread sends back updated game state
	main thread sends StealFrom request
	gui thread sends back color choice in colorVar
	main thread gets colorVar
	main thread updates logic based on choice

mainThread puts NextMove in requestVar
main thread waits for next action
gui thread somehow displays each of the player's options

Build Road
Build Sett
Build City
Buy Card
Play Card
Trade with another player
Trade with bank/harbor

gui thread updates the display's callbacks based on the user's choice
gui thread puts the action
main thread takes next action, runs game logic

if end of turn, check win conditions, advance turn
else main thread waits for next action
