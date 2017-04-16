setup:
	tiles laid down:
		beginner is hardcoded, advanced is random
	each player:
		picks color
		lays down 2 roads and 2 settlements on board

Each turn, each player:
	play up to one development card

details:
	Road:
		player with longest road gets longest road card (>=5 roads)
			worth 2 victory points
	Settlement:
	Development Card:
		3 different kinds of cards:
			Knight
				Activate robber (but don't kill off > 7 resources)
				(3+ knights activates largest army, worth 2 points)
			Progress
				Follow the instructions
					Road Building
					Year of Plenty
					Monopoly

Rolling Robber:
	If you roll a 7, you get the robber
	All players with 7+ resources return half (rounded down) to bank
	Then move robber to any terrain hex or to the desert
	Steal a random resource card from an opponent with settlement adjacent to the robber
Playing development cards:
	At any time you may play a development card
	Cannot be one you bought that turn
Trade:
	Must be players turn
	cannot trade matching resources (ie. 3 ore for 1 ore)

Corner Map needs to get done

REFACTOR CORNERS AND FIX THE MISTAKES I MADE

Add tests for some stuff once board is setup

Make an Actions Type:  we get user input and then we get back an action which we can handle.
