(* ::Package:: *)

BeginPackage["betaFunctions`","polynomials`"] 
Clear[betaFunctionEquation]
Clear[initialCondition]
Clear[betaFunctionExpansion]
Clear[solve]

betaFunctionEquation::usage = "Represents a beta function equation.
betaFunctionEquation[] expects a name_Symbol as first argument and a \
p_polynomial as second argument. This corresponds to s' = p."; 
 
initialCondition::usage = "Represents the initial condition of a \
beta function. Its first argument is a name_Symbol and its second \
argument is a the value_ at b[0]. This corresponds to b[0] = value."; 
 
betaFunctionExpansion::usage = "Represents a beta function expanded \
in powers of the independent parameter. The first argument is the \
name_Symbol of the beta function and the second parameter is the \
coefficient map."; 
   
solve::usage = "Solves a system of beta function equations. The first \
argument is a List containg betaFunctionEquation[]s and the second \
argument is a list containing the initialCondition[]s. solve[] returns \
a List of betaFunctionExpansion[]s corresponding to the solutions of \
the given system.";

solve[equations_List,
      initialConditions_List,
      maxOrder_Integer] :=
  Module[{c = Unique[coefficients],cExtractorEq,cExtractorTerm,cExtractorFactor},
    Function[init,c[init[[1]]][0] = init[[2]]] /@ initialConditions;
    
    cExtractorFactor[{betaFunction_Symbol,power_Integer},order_Integer] :=
    Module[{cRanges, cOrder, cCombinations},
          cRanges = Sequence @@ Table[{cOrder[l],0,order},
                                      {l,1,power}];
          cCombinations = Table[Table[cOrder[k],{k,1,power}],
                                Evaluate[cRanges]];
          cCombinations = Select[Flatten[cCombinations,power - 1],
                                 ((Plus @@ #) == order) &];

          Plus @@ Times @@@ Map[c[betaFunction],cCombinations,{2}]
          ];

    cExtractorTerm[{coeff_},0] := coeff;
    cExtractorTerm[{coeff_},order_Integer] := 0;
    cExtractorTerm[{coeff_,form_List},order_Integer] :=
    Module[{bRanges, bOrder, bCombinations},
          bRanges = Sequence @@ Table[{bOrder[l],0,order},
                                      {l,1,Length[form]}];
          bCombinations = Table[Table[bOrder[k],{k,1,Length[form]}],
                                Evaluate[bRanges]];
          bCombinations = Select[Flatten[bCombinations,Length[form] - 1],
                                 ((Plus @@ #) == order) &];
          If[Length[bCombinations] === 0,Return[0]];

          coeff * Plus @@ Times @@@ Apply[cExtractorFactor,
              Table[{form[[i]],bCombinations[[j,i]]},
                    {j,1,Length[bCombinations]},{i,1,Length[form]}],
                                          {2}]
          ];

    cExtractorEq[eq_betaFunctionEquation,order_Integer] :=
      Plus @@ cExtractorTerm @@@ Tuples[{eq[[2]],{order}}];

    Function[{name,order},c[name][Except[0,order]] =
        1/order * cExtractorEq[FirstCase[equations,betaFunctionEquation[name,_]],
                                      order-1];
    ] @@@ Flatten[Table[{name,order},{order,Table[k,{k,maxOrder}]},
                             {name,initialConditions[[All,1]]}],1];

    betaFunctionExpansion[#,c[#]] & /@ initialConditions[[All,1]]] /; 
        DuplicateFreeQ[equations, (#1[[1]] == #2[[1]]) &] &&
        Sort[equations[[All,1]]] == Sort[initialConditions[[All,1]]]

EndPackage[];

