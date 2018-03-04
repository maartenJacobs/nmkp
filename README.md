# nmkp

`nmkp` is a small game written in Haskell for Paddle's 2nd Hackathon,
which took place on March 2nd of 2018. It was originally a single file
application, not including assets.

## Notes

- Haskell's sum types made adding states to the game immensely simple.
Even late into the hackathon, adding a new screen or more complex feature
was a breeze, and hardly ever affecting any other functionality. This was
mainly due to the errors and warnings the compiler displays when building
the application, i.e. mentioning when a case is unimplemented.
- Working with record types often resulted in immensely long parameters,
e.g. `world@World{state = state, battle = Just battle, …}`. A few syntactic
extensions were tried out to alleviate this problem, although the situation
could still be improved, at least partially, using better datatypes, 
e.g. `World | WorldBattle`.

## Credit

- "Game Over" screen thanks to https://eathertrainer.deviantart.com/art/GAME-OVER-Pokemon-Red-Green-and-Blue-274456696
- All other sprites thanks to https://www.spriters-resource.com/
