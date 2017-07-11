(* ::Package:: *)

Needs["betaFunctions`"];

(***** Test "polynomials" module *****)

polynomialPlus[polynomial[{37}],polynomial[{5}]] == polynomial[{42}]
polynomialPlus[polynomial[{37,{{g,3},{y,2}}},{2,{{g,1}}}],
              polynomial[{5},{5,{{g,3},{y,2}}},{3,{{y,2}}}]] == 
              polynomial[{42,{{g,3},{y,2}}},{2,{{g,1}}},{5},{3,{{y,2}}}]

polynomialTimes[polynomial[{37}],polynomial[{5}]] == polynomial[{37*5}]
polynomialTimes[polynomial[{37,{{g,3},{y,2}}},{2,{{g,1}}}],
                polynomial[{5},{3,{{y,2}}}]] ==
                polynomial[{37*5,{{g,3},{y,2}}},{37*3,{{g,3},{y,4}}},{10,{{g,1}}},{6,{{g,1},{y,2}}}]

(***** Test "betaFunctions" module *****)

(* test some random numbers *)
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

runTo[coeff[[2]],s,3] == polynomial[{1},{1,{{s,1}}},{3/2,{{s,2}}},{5/2,{{s,3}}}]
runTo[coeff[[2]],1,3] == polynomial[{6}]

(* test against excercise 1 of QFT 2/QCD Sheet 6 by Prof. Stoeckinger *)
bEq[g3] = betaFunctionEquation[g3,polynomial[{-7,{{kl,1},{g3,3}}}]];
bEq[yt] = betaFunctionEquation[yt,polynomial[{-8,{{kl,1},{yt,1},{g3,2}}}]];
bEq[v] = betaFunctionEquation[v,polynomial[{0}]];
bEq[mz] = betaFunctionEquation[mz,polynomial[{0}]];
bEq[lambda] = betaFunctionEquation[lambda,polynomial[{-12,{{kl,1},{yt,4}}}]];

init[g3] = g30;
init[yt] = yt0;
init[v] = v0;
init[mz] = mz0;
init[lambda] = lambda0;

coeffs = solve[{bEq[g3],bEq[yt],bEq[v],bEq[mz],bEq[lambda]},
               {init[g3],init[yt],init[v],init[mz],init[lambda]}];

(* test against excercise 1 of QFT 2/QCD Sheet 6 by Prof. Stoeckinger *)
bEq[g3] = betaFunctionEquation[g3,polynomial[{-7,{{kL,1},{g3,3}}}]];
bEq[yt] = betaFunctionEquation[yt,polynomial[{-8,{{kL,1},{yt,1},{g3,2}}}]];
bEq[v] = betaFunctionEquation[v,polynomial[{0}]];
bEq[mz] = betaFunctionEquation[mz,polynomial[{0}]];
bEq[lambda] = betaFunctionEquation[lambda,polynomial[{-12,{{kL,1},{yt,4}}}]];

coeffs = solve[{bEq[g3],bEq[yt],bEq[v],bEq[mz],bEq[lambda]},
               {initialCondition[g3,polynomial[{g30}]],
                initialCondition[yt,polynomial[{yt0}]],
                initialCondition[v,polynomial[{v0}]],
                initialCondition[mz,polynomial[{mz0}]],
                initialCondition[lambda,polynomial[{lambda0}]]},
               3];

lambdaExpansion = SelectFirst[coeffs,#[[1]] == lambda &];
mzExpansion = SelectFirst[coeffs,#[[1]] == mz &];
vExpansion = SelectFirst[coeffs,#[[1]] == v &];

lambdaSM = runTo[lambdaExpansion,0,3];
lambdaSUSY = runTo[lambdaExpansion,tS,3];
vSM = runTo[vExpansion,0,3];
vSUSY = runTo[vExpansion,tS,3];
mzSUSY2 = polynomialTimes[runTo[mzExpansion,tS,3],runTo[mzExpansion,tS,3]];

lambdaV2SM = polynomialTimes[lambdaSM,vSM,vSM];
lambdaV2SUSY = polynomialTimes[lambdaSUSY,vSUSY,vSUSY];

mHiggsSM = polynomialPlus[mzSUSY2,polynomialMinus[lambdaV2SM,lambdaV2SUSY]];
mHiggsSM = Select[mHiggsSM,!PossibleZeroQ[#[[1]]] &];

trim[mHiggsSM,{{tS,3},{kL,3}}] == polynomial[{mz0^2},
                       {12 v0^2 yt0^4,{{kL,1},{tS,1}}},
                       {-192 v0^2 yt0^4 g30^2,{{kL,2},{tS,2}}},
                       {2944 v0^2 yt0^4 g30^4,{{kL,3},{tS,3}}}]
