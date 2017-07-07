(* ::Package:: *)

Needs["betaFunctions`"];

(* Test "polynomials" module *)

polynomialPlus[polynomial[{37}],polynomial[{5}]] == polynomial[{42}]
polynomialPlus[polynomial[{37,{{g,3},{y,2}}},{2,{{g,1}}}],
              polynomial[{5},{5,{{g,3},{y,2}}},{3,{{y,2}}}]] == 
              polynomial[{42,{{g,3},{y,2}}},{2,{{g,1}}},{5},{3,{{y,2}}}]

polynomialTimes[polynomial[{37}],polynomial[{5}]] == polynomial[{37*5}]
polynomialTimes[polynomial[{37,{{g,3},{y,2}}},{2,{{g,1}}}],
                polynomial[{5},{3,{{y,2}}}]] ==
                polynomial[{37*5,{{g,3},{y,2}}},{37*3,{{g,3},{y,4}}},{10,{{g,1}}},{6,{{g,1},{y,2}}}]

(* Test "betaFunctions" module *)

coeff=solve[{betaFunctionEquation[g,polynomial[{1},{2,{{g,2},{y,1}}}]],
             betaFunctionEquation[y,polynomial[{1,{{y,3}}}]]},
            {initialCondition[g,polynomial[{42}]],
             initialCondition[y,polynomial[{1}]]},
            3];

Table[coeff[[1,2]][k],{k,0,3}] == {polynomial[{42}],
  polynomial[{3529}],
  polynomial[{298200}],
  polynomial[{75603446/3}]}

Table[coeff[[2,2]][k],{k,0,3}] == {polynomial[{1}],
  polynomial[{1}],
  polynomial[{3/2}],
  polynomial[{5/2}]}



