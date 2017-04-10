setup:
	tiles laid down:
		beginner is hardcoded, advanced is random
	each player:
		picks color
		gets 5 settlements, 4 cities, and 15 roads
		lays down 2 roads and 2 settlements on board

Each turn, each player:
	then can
		trade with other players, ports(if adjacent), or bank (4:1)
		build roads, settlements, or cities
		buy development cards
		play up to one development card

details:
	Road:
		Brick, Lumber
		Connects an existing road, settlement, city
		player with longest road gets longest road card (>=5 roads)
			worth 2 victory points
	Settlement:
		Brick, Lumber, Wool, Grain
		only if all 3 adjacent intersections are vacant
		When a terrain hex gets resources, you recieve 1 resource for each settlement adjacent to it
		Worth 1 VP
	City:
		3 ore, 2 grain, settlement
		twice as many resources as settlements
		worth 2 VP
	Development Card:
		Ore wool grain
		3 different kinds of cards:
			Knight
				Activate robber
				(3+ knights activates largest army, worth 2 points)
			Progress
				Follow the instructions
					Road Building
					Year of Plenty
					Monopoly
			Victory Point
				hidden, but worth one point
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



Game over at 10 victory points on your turn!
