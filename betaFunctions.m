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
argument is a the value_polynomial at b[0]. This corresponds to \
b[0] = value."; 
 
betaFunctionExpansion::usage = "Represents a beta function expanded \
in powers of the independent parameter. The first argument is the \
name_Symbol of the beta function and the second parameter is the \
coefficient map."; 

runTo::usage = "Evaluates a b_betaFunctionExpansion at a given scale \
to a given order_Integer. The first argument is the \
b_betaFunctionExpansion, the second one is the scale_ and the third \
is the order_Integer."
   
solve::usage = "Solves a system of beta function equations. The first \
argument is a List containg betaFunctionEquation[]s and the second \
argument is a list containing the initialCondition[]s. solve[] returns \
a List of betaFunctionExpansion[]s corresponding to the solutions of \
the given system.";

runTo[b_betaFunctionExpansion,scale_,order_Integer] :=
  polynomialPlus @@ polynomialTimes @@@
    Transpose[{Table[b[[2]][k],{k,0,order}],
               Flatten[{polynomial[{1}],Table[polynomial[{scale^k}],{k,1,order}]}]}] /;
  NumericQ[scale]
runTo[b_betaFunctionExpansion,scale_,order_Integer] :=
  polynomialPlus @@ polynomialTimes @@@
    Transpose[{Table[b[[2]][k],{k,0,order}],
               Flatten[{polynomial[{1}],Table[polynomial[{1,{{scale,k}}}],{k,1,order}]}]}]

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

          polynomials`polynomialPlus @@ polynomials`polynomialTimes @@@
            Map[c[betaFunction],cCombinations,{2}]
          ];

    cExtractorTerm[{coeff_},0] := polynomial[{coeff}];
    cExtractorTerm[{coeff_},order_Integer] := polynomial[{0}];
    cExtractorTerm[{coeff_,form_List},order_Integer] :=
    Module[{cForm,bForm,cPoly,bRanges, bOrder, bCombinations},
          {bForm,cForm} = GroupBy[form,MemberQ[equations[[All,1]],#[[1]]] &] /@
                          {True, False};
          cPoly = If[Head[cForm] === Missing,
                     polynomials`polynomial[{coeff}],
                     polynomials`polynomial[{coeff,cForm}]];

          bRanges = Sequence @@ Table[{bOrder[l],0,order},
                                      {l,1,Length[bForm]}];
          bCombinations = Table[Table[bOrder[k],{k,1,Length[bForm]}],
                                Evaluate[bRanges]];
          bCombinations = Select[Flatten[bCombinations,Length[bForm] - 1],
                                 ((Plus @@ #) == order) &];
          If[Length[bCombinations] === 0,Return[0]];

          polynomials`polynomialTimes[cPoly,
            polynomials`polynomialPlus @@ polynomials`polynomialTimes @@@
              Apply[cExtractorFactor,
                Table[{bForm[[i]],bCombinations[[j,i]]},
                    {j,1,Length[bCombinations]},{i,1,Length[bForm]}],
                    {2}]]
          ];

    cExtractorEq[eq_betaFunctionEquation,order_Integer] :=
      polynomialPlus @@ cExtractorTerm @@@ Tuples[{eq[[2]],{order}}];

    Function[{name,order},c[name][Except[0,order]] =
        polynomials`polynomialTimes[polynomials`polynomial[{1/order}],
                        cExtractorEq[FirstCase[equations,
                                               betaFunctionEquation[name,_]],
                                     order-1]];
    ] @@@ Flatten[Table[{name,order},{order,Table[k,{k,maxOrder}]},
                             {name,initialConditions[[All,1]]}],1];

    betaFunctionExpansion[#,c[#]] & /@ initialConditions[[All,1]] ] /; 
        DuplicateFreeQ[equations, (#1[[1]] == #2[[1]]) &] &&
        Sort[equations[[All,1]]] == Sort[initialConditions[[All,1]]]

EndPackage[];







