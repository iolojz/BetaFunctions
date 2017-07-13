(* ::Package:: *)

Needs["betaFunctions`"];

betaLambda1L = polynomial[{4,{{lambda,2}}},
                          {12 yt^2 - 9 g1^2 - 3 g2^2,{{lambda,1}}},
                          {-36 yt^4 + 9/4 g2^4 + 9/2 g1^2 g2^2 + 27/4 g1^4}
                          ];
betaLambda2L = polynomial[{-26/3,{{lambda,3}}},
                          {-24 yt^2 + 6(3 g1^2 + g2^2),{{lambda,2}}},
                          {-3 yt^4 + 80 g3^2 yt^2 + 45/2 g1^2 yt^2 + 85/6 g2^2 yt^2 + (10 nG - 313/8) g1^4 + 39/4 g1^2 g2^2 + (50/9 nG + 229/24) g2^4,{{lambda,1}}},
                          {180 yt^6 - 192 yt^4 g3^2 -16 yt^4 g2^2 - 27/2 yt^2 g1^4 + 63 yt^2 g1^2 g2^2 - 57/2 yt^2 g2^4 + 3( (497/8 - 8nG) g1^6 - (97/24 + 8/3 nG) g1^4 g2^2 - (239/24 + 40/9 nG) g1^2 g2^4 - (59/24 + 40/9 nG) g2^6)}
                          ];
