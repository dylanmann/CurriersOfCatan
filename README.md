# _**Curriers of Catan**_
## [CIS 552](https://www.seas.upenn.edu/~cis552/) Spring 2017 Final Project
### By Dylan Mann and David Cao
#### Professor: Stephanie Weirich, TA: Antal Spector-Zabusky

[Hosted on Github](https://github.com/cis552/project_mannd_davidcao-proj)

##### **Description of Classes In Order of Reading:**
##### [Board.hs](Catan/Board.hs):
The class where all board-related datatypes are defined.  Contains the primitive board types and wraps unsafe operations to provide a total
mapping from `CornerLocations` and `TileLocations`, and so no other code has to touch the `Board` and invariants will still be maintained.  Also contains some setup methods.  These are the static references to the board, ie. `Tile` `Corner` `Resource` `CornerLocation` and `TileLocation` (how corners and tiles are indexed in the board).  Types wrapped up here mostly cannot be constructed except through various initalization methods (ie `setupBoard`, `makeCornerLocation` and `makeTileLocation`)

##### [Types.hs](Catan/Types.hs):
This is where all types can be safely used outside of this modeule.  Contains game primitives as well as the `Game` class that is eventually maintained by `StateT` in the execution of the game.  Exports `Board` because any module using `Types` will also need to use the types in `Board`.

##### [Actions.hs](Catan/Actions.hs):
This module is where game execution happens.  this module's main component is the `handleAction` method, which accepts a PlayerAction as input, checks its validity, and advances the `GameState` if valid, and returns `False` if invalid.  A few of the other functions exported are `gameOver` for checking terminal conditions of the `Game`, `allocateRewards`, which takes a token and allocates resources to all players with buildings who receive rewards for that roll, and rollSeven, which penalizes players who are hoarding resources and moves the robber.

##### [ActionParsing.hs](Catan/ActionParsing.hs):
This module represents the depricated model of single threaded gameplay with commandline input that the early stages of the game used in its execution.  It later evolved into the early stages of our multi-threaded model, before being phased out entirely.  We decided to include this module both to show how far our game has come from the early stages, but also in case anybody wanted to hook into the game using a pure javascript client or something similar, where the text parsing capabilities may end up coming in handy.

##### [CatanGUI.hs](Catan/CatanGUI.hs):
All UI code lives here.  The module exports a single function, `beginGUI`, which launches a thread and sets up the `ThreePenny` server that communicates with localhost:8023.  All comunication between this thread and the main, game logic thread happens in the form of `MVar`s (the `CatanMVars` type in the `Game` Datatype.  It lives in a FRP world where events are sent by callbacks from user interactions with the javascript elements.  Therefore, the way it is setup is that it sends actions to the gameplay thread after each user action, and updates the GUI based on the gamestate sent back from the ui thread.

##### [GamePlay.hs](Catan/GamePlay.hs):
This is the main module in the program.  It lives in a completely monadic world built on StateT, where the only `IO` that happens is by spawning the initial GUI thread and putting and taking `MVar`s.  The game runs by looping on an event and updating the state each time a `PlayerAction` is received from the UI thread, and getting those actions handled by `handleAction`, then when the player ends their turn, it advances the player and allocates resources/ asks for the robber to be moved.  Most of the time, this thread lives blocked on the UI thread, waiting for input.

##### [Parser.hs](Catan/Parser.hs), [ParserCombinator.hs](Catan/ParserCombinator.hs):
This is the Applicative Parsing library that we built up during lecture and used in hw7.  We reused much of its functionality in the implementation of the ActionParsing module.

##### **Additional Required Cabal Packages:**
[random-shuffle](https://hackage.haskell.org/package/random-shuffle)
used for shuffling of initial game state.

[threepenny-gui](https://hackage.haskell.org/package/threepenny-gui)
used for rendering the board and accepting user input in a FRP style.

[lifted-base](https://hackage.haskell.org/package/lifted-base)
used for lifting mvar operations
