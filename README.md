# types.ss
A collection of Scheme DSLs that work with types, especially strict typing.  
- functions.ss is a DSL that implements strict typing in function definitions  
- patterns.ss is a single macro that implements pattern matching.  

## Compatibility
Tested and working on Chez Scheme in the following versions
	- 10.3.0
	- 10.4.0
	
I tried my hardest to keep it as standard (R6RS) as possible, but some syntactic
differences might cause issues.  
However, IF any tweaking is needed for it to work with your  
implementation of choice, the work required should be minimal.
