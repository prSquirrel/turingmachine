Simple implementation in Haskell, done in TDD.

# Usage:
#### Building: 
`stack build`

#### Running: 
`stack exec turingmachine-exe <path to program.yaml> <iterations> <delay milliseconds>`

Example:

`stack exec turingmachine-exe program.yaml 100000000 50`


#### Example YAML configuration:

```
meta: # This section configures aliases for certain control symbols
  noActionSymbol: '*' # Used in place of write or move position
  anySymbol: '*' # Used to accept any state or head symbol 
  emptySymbol: '_' # Alias for space
  emptyTape: ""

start:
  state: "S0" # State can be a String
  tape: "0000000000000000000000[0]*0000" # Square brackets specify head starting position


# <Accepting state> <Accepting symbol> <Write symbol | noAction> <Move L | R | noAction> <Next state> 
rules: 
  - S0 0 0 R S0
  - S0 1 1 R S0
  - S0 * * L S1
  - S1 0 1 R S0
  - S1 1 0 L S1
```
