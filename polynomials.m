(* ::Package:: *)

BeginPackage["polynomials`"];
Clear[polynomial];
Clear[constantTerm];
Clear[polynomialPlus];
Clear[polynomialMinus];
Clear[polynomialTimes];
Clear[trim];

polynomial::usage = "Represents a polynomial. polynomial[] expects a Sequence of Lists of \
the form {coefficient,form} where form is a List of tuples \
{{name1_Symbol,power1_Integer}, ...} where all powers have to be at least one. A \
constant term may be represented as {constant}. Every power has to be unique.";
SetAttributes[polynomial,Orderless];

constantTerm::usage = "Gives the constant term of a p_polynomial.";
polynomialPlus::usage = "Adds polynomials.";
polynomialMinus::usage = "Subtracts polynomials.";
polynomialTimes::usage = "Multiplies polynomials.";
trim::usage = "Trims a p_polynomial to orders less equal than specified by a form. \
The first argument is the p_polynomial and the second argument is the form.";

constantTerm[polynomial[{constant_},___]] := constant 
constantTerm[polynomial[___]] := 0 

polynomialPlus[p_polynomial] := p
polynomialPlus[p1_polynomial,p2_polynomial,tail__] :=
  polynomialPlus[polynomialPlus[p1,p2],tail];
polynomialPlus[polynomial[],p2_polynomial] := p2 
polynomialPlus[polynomial[{c1_},tail1___],polynomial[{c2_},tail2___]] := 
  polynomial[{c1+c2},
              Sequence @@ polynomialPlus[   
                polynomial[tail1], 
                polynomial[tail2]]]
polynomialPlus[polynomial[{c1_,form1_List},tail1___],
               polynomial[{c2_,form2_List},tail2___]] :=  
  polynomial[{c1+c2,form1},
              Sequence @@ polynomialPlus[ 
                polynomial[tail1], 
                polynomial[tail2]]] /; equalForms[form1,form2]
polynomialPlus[polynomial[term_List,tail___],p2_polynomial] := 
  polynomial[term,Sequence @@ polynomialPlus[polynomial[tail],p2]]

polynomialMinus[p1_polynomial,p2_polynomial] :=
  polynomialPlus[p1,polynomialTimes[polynomial[{-1}],p2]]

polynomialTimes[p_polynomial] := p
polynomialTimes[p1_polynomial,p2_polynomial,tail__] :=
  polynomialTimes[polynomialTimes[p1,p2],tail];
polynomialTimes[polynomial[],p2_polynomial] := polynomial[]
polynomialTimes[polynomial[{c1_},tail___],p2_polynomial] :=
  polynomialPlus[p2 /. {c2_,form___} :> {c1*c2,form},
                 polynomialTimes[polynomial[tail],p2]]
polynomialTimes[polynomial[{c1_,form_List},tail___],
                p2_polynomial] :=
  polynomialPlus[multiplyWithForm[polynomialTimes[polynomial[{c1}],p2],form],
                 polynomialTimes[polynomial[tail],p2]]

trim[p_polynomial,form_List] :=
  Select[p,orderLessEqualQ[#,form] &]

Begin["private`"]; 
Clear[equalForms];
Clear[multiplyTermWithForm];
Clear[multiplyForms];
Clear[orderLessEqualQ];

equalForms[form1_List,form2_List] := (Sort[form1] == Sort[form2])

multiplyWithForm[polynomial[terms__],form_List] :=
  polynomial @@ (multiplyTermWithForm[#,form] & /@ {terms})

multiplyTermWithForm[{c1_},form_List] := {c1, form}
multiplyTermWithForm[{c1_,form1_List},form2_List] := {c1, multiplyForms[form1,form2]}

multiplyForms[{},form_List] := form
multiplyForms[{{x_,p1_},tail___},{front___,{x_,p2_},back___}] := 
  Join[multiplyForms[{tail},{front,back}],{{x,p1+p2}}]
multiplyForms[{{x_,p1_},tail___},form2_List] := 
  Join[multiplyForms[{tail},form2],{{x,p1}}]

orderLessEqualQ[{c_},form_List] := True
orderLessEqualQ[{_,{{b_,p1_Integer}}},form_List] :=
  Module[{exp = FirstCase[form,{b,p2_} :> p2]},\
    If[Head[exp] == Missing,Return[False]];
    Return[p1 <= exp]]
orderLessEqualQ[{_,form1_List},form2_List] :=
  AllTrue[orderLessEqualQ[{1,{#}},form2] & /@ form1,TrueQ]

End[] (* "private`" *)
EndPackage[]

