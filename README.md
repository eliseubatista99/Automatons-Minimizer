# Automatons-Minimizer
## Minimization of complete deterministic finite automatons

### Input

  -a line with the integer n, specifying the set S = {1..n};
  
  -a line with the integer representing the initial state s0;
  
  -a line with the integer f (cardinality of the final state set F);
  
  -a line with distinct integers that form the set of final states;
  
  -a line with the number m of transitions (the cardinality of);
  
  -m lines where each introduces a transition in the form of i c j, i being the integer representing the departure state of the transition, and the character on the transition label and j is the integer representing the arrival state.
  
### Output

The description of the resulting automaton, following exactly the input format.

Since the set of states of the calculated automaton can be significantly distanced
of the state set of A, be mindful of the need to rename the states so that

These form a set of the form {1..m}.

The list of input states, final states, and transitions is sorted in order
lexicographic is usual.

### Sample Input

3

1

2

2 3

6

1 a 2

1 b 3

2 a 2

2 b 3

3 a 2

3 b 3

### Sample Output

2

1

1

2

4

1 a 2

1 b 2

2 a 2

2 b 2
