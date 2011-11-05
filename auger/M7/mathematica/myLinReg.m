(* ::Package:: *)

BeginPackage["myLinReg`",{"ErrorBarPlots`"}]

LinReg::usage="LinReg berechnet Werte und Fehler der 'daten' durch Lineare Regression.";
LinRegOrigin::usage="LinRegOrigin berechnet Werte und Fehler der 'daten' durch Lineare Regression f\[UDoubleDot]r eine Gerade durch den Ursprung."
StreuFehler::usage = "StreuFehler bestimmt die Fehler der Steigung und Achsenabschnitt aus der Streuung und Vergleicht diese mit der Linearen Regression.\n
Wenn Gewichtfunktion f oder konstanter Faktor sigma ungleich 1 ist, so m\[UDoubleDot]ssen diese als 2. und 3. Argument \[UDoubleDot]bergeben werden.\n
f hat die Form f[x,y,\[CapitalDelta]y].";

Begin["`Private`"]
LinReg[daten_]:= Module[{x,y,\[CapitalDelta]y,S,yBar,\[CapitalDelta]a0,\[CapitalDelta]b0,a0,b0,n,sSqr},
n=Length[daten];
x=daten[[All,1]];
y=daten[[All,2]];
\[CapitalDelta]y = daten[[All,3]];
(* Berechnungen f\[UDoubleDot]r die Lineare Regression *)
S=(\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(1\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\(x[\([i]\)]\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)-(\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\))^2;
a0=((\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\(x[\([i]\)]\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(y[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)-(\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[\([i]\)]\ y[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\))/S;
b0=((\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(1\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[\([i]\)]\ y[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)-(\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(y[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\))/S;
\[CapitalDelta]a0=Sqrt[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[
SuperscriptBox[\(x[\([i]\)]\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]/S\)\)];
\[CapitalDelta]b0=Sqrt[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[\(1\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]/S\)\)];
(* Ausgabe der Werte und Fehler *) 
Print["Lineare regression mit ",n," Messwerten:"];
Print[TableForm[{{"","Wert","Fehler"},{"Achsenabschnitt a",a0,\[CapitalDelta]a0},{"Steigung b",b0,\[CapitalDelta]b0}}]];
(* Berechnung und Ausgabe des R^2 *)
(* tempor\[ADoubleDot]r ben\[ODoubleDot]tigtes yBar: *)
yBar=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[\(y[\([i]\)]\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(1\), 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)\)\);
Print[R^2, " = ", 1-\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - a0 - b0\ x[\([i]\)])\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - yBar)\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]\)\)\)];
(* Berechnung und Ausgabe des \[Chi]^2/(n-2) *)
Print[s^2," = ",\[Chi]^2/(n-2)," = ",sSqr=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - a0 - b0\ x[\([i]\)])\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[\([i]\)]\), \(2\)]]/\((n - 2)\)\)\)];
(* Zeichnen der Messwerte mit Ausgleichs und Grenzgeraden *)
(* Print[Show[ErrorListPlot[daten],Plot[{a0+b0 t,a0+\[CapitalDelta]a0+ (b0 + \[CapitalDelta]b0) t, a0-\[CapitalDelta]a0+ (b0 - \[CapitalDelta]b0) t}, {t,Min[x]-1,Max[x]+1}, PlotStyle->{Black,Dashed,Dashed}],GridLines->Automatic,GridLinesStyle->{Thin,Gray}, ImageSize->Large,Frame->True,FrameTicks->All]];
*)
{a->a0,\[CapitalDelta]a->\[CapitalDelta]a0,b->b0,\[CapitalDelta]b->\[CapitalDelta]b0,pS->S,psSqr->sSqr}
(* sSqr und S werden lediglich zur weiterrechnung \[UDoubleDot]bergeben und k\[ODoubleDot]nnen im regelfall ignoriert werden. *)
];

StreuFehler[daten_] := Module[{},
StreuFehler[daten,#3&,1]]

StreuFehler[daten_,f_,sigma_]:= Module[{n,x,y,\[CapitalDelta]y,Sprime,yBar,ff,S,sSqr,a0,\[CapitalDelta]a0,b0,\[CapitalDelta]b0,\[CapitalDelta]as,\[CapitalDelta]bs},

(* Alle alten Ausgaben und \[UDoubleDot]bernahme der alten Berechnungen.*)
{S,sSqr,a0,\[CapitalDelta]a0,b0,\[CapitalDelta]b0}={pS,psSqr,a,\[CapitalDelta]a,b,\[CapitalDelta]b}/.LinReg[daten];
n=Length[daten];
x=daten[[All,1]];
y=daten[[All,2]];
\[CapitalDelta]y = daten[[All,3]];
(* Annahme: \[Sigma]^2 \[TildeEqual] s^2 f\[UDoubleDot]r alle Messwerte gleich *)
Sprime=sigma^4 * S;
ff[j_]:=f[x[[j]],y[[j]],\[CapitalDelta]y[[j]]];
sSqr=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - a0 - b0\ x[\([i]\)])\), \(2\)], 
SuperscriptBox[\(ff[i]\), \(2\)]]/\((n - 2)\)\)\);
\[CapitalDelta]as=Sqrt[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[\(
\*SuperscriptBox[\(x[\([i]\)]\), \(2\)]\ sSqr\), 
SuperscriptBox[\(ff[i]\), \(2\)]]/Sprime\)\)];
\[CapitalDelta]bs=Sqrt[\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[\(sSqr\), 
SuperscriptBox[\(ff[i]\), \(2\)]]/Sprime\)\)];
Print["Fehler durch Streuung: "];
Print[TableForm[{{"","Wert","Fehler"},{"Achsenabschnitt a",a0,\[CapitalDelta]as},{"Steigung b",b0,\[CapitalDelta]bs}}]];
(* Berechnung und Ausgabe des R^2 *)
(* tempor\[ADoubleDot]r ben\[ODoubleDot]tigtes yBar: *)
yBar=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[\(y[\([i]\)]\), 
SuperscriptBox[\(ff[i]\), \(2\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(1\), 
SuperscriptBox[\(ff[i]\), \(2\)]]\)\)\);
Print[R^2, " = ", 1-\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]\(
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - a0 - b0\ x[\([i]\)])\), \(2\)], 
SuperscriptBox[\(ff[i]\), \(2\)]]/\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\((y[\([i]\)] - yBar)\), \(2\)], 
SuperscriptBox[\(ff[i]\), \(2\)]]\)\)\)];
(* Ausgabe des \[Chi]^2/(n-2) *)
Print[s^2," = ", sSqr];
(*Print[Show[ErrorListPlot[daten],Plot[{a0+b0 t,a0+\[CapitalDelta]as+ (b0 + \[CapitalDelta]bs) t, a0-\[CapitalDelta]as+ (b0 - \[CapitalDelta]bs) t}, {t,Min[x]-1,Max[x]+1}, PlotStyle->{Black,Dashed,Dashed}],GridLines->Automatic,GridLinesStyle->{Thin,Gray}, ImageSize->Large,Frame->True,FrameTicks->All]];
*)
{a->a0,\[CapitalDelta]a->\[CapitalDelta]as,b->b0,\[CapitalDelta]b->\[CapitalDelta]bs,pS->S,psSqr->sSqr}
];
LinRegOrigin[daten_]:= Module[{x,y,\[CapitalDelta]y,yBar,\[CapitalDelta]b0,b0,n,sSqr},
n=Length[daten];
x=daten[[All,1]];
y=daten[[All,2]];
\[CapitalDelta]y = daten[[All,3]];
(* Berechnungen f\[UDoubleDot]r die Lineare Regression *)
b0=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(x[[i]]\ y[[i]]\), 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\(x[[i]]\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\);
\[CapitalDelta]b0=Sqrt[1/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\(x[[i]]\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)];
(* Ausgabe der Werte und Fehler *) 
Print["Lineare regression mit ",n," Messwerten:"];
Print[TableForm[{{"","Wert","Fehler"},{"Steigung b",b0,\[CapitalDelta]b0}}]];
(* Berechnung und Ausgabe des R^2 *)
(* tempor\[ADoubleDot]r ben\[ODoubleDot]tigtes yBar: *)
yBar=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(y[[i]]\), 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[\(1\), 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\);
Print[R^2, " = ", 1-\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\((y[[i]] - b0\ x[[i]])\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)/\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\((y[[i]] - yBar)\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)];
(* Berechnung und Ausgabe des \[Chi]^2/(n-2) *)
Print[s^2," = ",\[Chi]^2/(n-2)," = ",sSqr=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i\), \(n\)]
\*FractionBox[
SuperscriptBox[\((y[[i]] - b0\ x[[i]])\), \(2\)], 
SuperscriptBox[\(\[CapitalDelta]y[[i]]\), \(2\)]]\)/(n-1)];
(* Zeichnen der Messwerte mit Ausgleichs und Grenzgeraden *)
Print[Show[ErrorListPlot[daten],Plot[{b0 t, (b0 + \[CapitalDelta]b0) t, (b0 - \[CapitalDelta]b0) t}, {t,Min[x]-1,Max[x]+1}, PlotStyle->{Black,Dashed,Dashed}],GridLines->Automatic,GridLinesStyle->{Thin,Gray}, ImageSize->Large,Frame->True,FrameTicks->All]];
{b->b0,\[CapitalDelta]b->\[CapitalDelta]b0,psSqr->sSqr}
(* sSqr und S werden lediglich zur weiterrechnung \[UDoubleDot]bergeben und k\[ODoubleDot]nnen im regelfall ignoriert werden. *)
];
End[]
EndPackage[]



