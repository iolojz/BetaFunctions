(* ::Package:: *)

BeginPackage["polynomials`"];
Clear[polynomial]
Clear[polynomialPlus]

polynomial::usage = "Represents a polynomial. polynomial[] expects a Sequence of Lists of \
the form {coefficient,form} where form is a List of tuples \
{{name1_Symbol,power1_Integer}, ...} where all powers have to be at least one. A \
constant term may be represented as {constant}. Every power has to be unique."
SetAttributes[polynomial,Orderless];

constantTerm[polynomial[___,{constant_},___]] := constant 
constantTerm[polynomial[___]] := 0 
 
polynomialPlus[polynomial[],p2_polynomial] := p2 
polynomialPlus[polynomial[{c1_},tail___],polynomial[front___,{c2_},back___]] := 
  polynomial[{c1+c2},
              Sequence @@ polynomialPlus[   
                polynomial[tail], 
                polynomial[front,back]]]  
polynomialPlus[polynomial[{c1_,form_List},tail___],
               polynomial[front___,{c2_,form_List},back___]] :=  
  polynomial[{c1+c2,form},
              Sequence @@ polynomialPlus[ 
                polynomial[tail], 
                polynomial[front,back]]]   
polynomialPlus[polynomial[term_List,tail___],p2_polynomial] := 
  polynomial[term,Sequence @@ polynomialPlus[polynomial[tail],p2]]

Begin["private`"]; 
 
End[] (* "private`" *)
EndPackage[]

