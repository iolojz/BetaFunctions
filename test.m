Needs["betaFunctions`"];

coeff=solve[{betaFunctionEquation[g,polynomial[{1},{2,{{g,2},{y,1}}}]],
             betaFunctionEquation[y,polynomial[{1,{{y,3}}}]]},
            {initialCondition[g,42],
             initialCondition[y,1]},
            3]

Table[coeff[[1,2]][k],{k,0,3}] == {42,3529,298200,75603446/3}

Table[coeff[[2,2]][k],{k,0,3}] == {1,1,3/2,5/2}

