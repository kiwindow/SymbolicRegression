(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Biostatistics`"];
Needs["HypothesisTesting`"];

Setup::usage="Setup[] changes the current directory to the one where the current NoteBook is saved.";
SetUp::usage="etUp[] changes the current directory to the one where the current NoteBook is saved.";
Setdown::usage="Setdown[] changes the current directory to the defaul one.";
SetDown::usage="SetDown[] changes the current directory to the defaul one.";
PB::usage="PB[] prints a blank line.";
Ordinalize::usage="Ordinalize[integer] show the ordinalized number of the integer.";
MinToHourMinSec::usage="MinToHourMinSec[minute] retruns {hour, minute, second} converted from minute.";
HourMinSecToSec::usage="HourMinSecToSec[hour, min, sec] returns time in seconds.";
rawSecToHourMinSec::usage="rawSecToHourMinSec[sec] returns {hour, min, sec} from a time in seonds.";
DateTime::usage="DateTime[vec] shows time in the form of Date YYYY,MM,DD, Hour hour,min,sec";
PresentTime::usage="PresentTime shows the present time.";
DayTime::usage="DayTime[] shows the present time in details.";
JoinDataItems::usage="JoinDataItems[data, items] joins the data with the second column in the items.";
NF::usage="NF[number, n] shows an approximate value of a number with n digits.";
CutLast::usage="CutLast[vec] removed the last element in a vecotr of vec";
Numberings::usage="Numberings[list] prepends a running number to each entitiy of a list.";
NPosition::usage="NPosition[list, ele] returns a position of ele in list which is flattened.";
RealVectorQ::usage = "RealVectorQ[{yy,mm,dd,hh,mm,ss}] affirms that the argument is a vector of numeric elements.";
NullRemoving::usage="NullRemoving[list] returns a list that does not contain the element Null without changing the original order of elements in the list.";
NullCleaning::usage="NullCleaning[list] returns a list that do not contain a vector which contains Null without changing the original order of elements in the list.";
StringRemoving::usage="StringRemovming[list] removes non-number entities from a list.";
TakeElement::usage="TakeElement[list, n, ele] takes records that have an element of ele in the n-th column.";
FilterElement::usage="FilterElement[list, n, func] takes records that yields True when the function of func is applied to the element in the n-th column.";
MakeTwin::usage="MakeTwin[vector1, vector2] makes pairs of elemets from vector1 and vector2, both of which have the same length.";
Fuse::usage="Fuse[list1, list2] contatenates two lists, list1 and list2, horizontally.";
NMean::usage="NMean[list] enable to execute the built-in function Mean even to a list which contains non-number entities.";
NStandardDeviation::usage="NStandardDeviation[list] enable to execute the built-in function StandardDeviation even to a list which contains non-number entities.";
NMedian::usage="NMedian[vector] returns a median value of the vector.";
NMax::usage="NMax[list] enable to execute the built-in function Max even to a list which contains non-number entities.";

NMin::usage="NMin[list] enable to execute the built-in function Min even to a list which contains non-number entities.";

ErrorMessage::usaeg="ErrorMessage[string] prints string in green. ErrorMessage[string1, string2] prints joined errormessage. The color of fonts can be s by {r, b, g} vector placed after the string.";
NFindFile::usage=
"NFindFile[filename] searchs for a pass of a spreadsheet called filename. The extention of the filename can be omitted.";

NFindFile3::usage=
"NFindFile3[filename] searchs for a pass of a spreadsheet called filename and returns True or False. The extention of the filename can be omitted.";
IntactComplement::usage="IntactComplement[list1, list2] returns a list that is a relative complement of list2 with respect of list1 without changing the original order of elements in the list1.";
cleandata::usage="cleandata[list] is used in the DataIn[] function to change some elements in the imported data file into those in the executable format.";
ExportExcel::usage="ExportExcel[body, head, name] export a dataset with body as the main body of data and head as the running-numbered vairalbes names as an Excel file with a file name of name and the time when the file is exported.";
ExportExcel2::usage="ExportExcel2[body, head, name] export a dataset with body as the main body of data and head as the running-numbered vairalbes names as an Excel file with a file name of name and the time when the file is exported.";
ExportExcel3::usage="ExportExcel3[body, head, name] export a dataset with body as the main body of data and head as the running-numbered vairalbes names as an Excel file with a file name of name.";
ListNormalize::usage="ListNormalize[body, head] normalizes the matrix data with body as its numeric data and head as the runnning-numbered item names";
NRandomSample::usage="NRandomSample[vec, int, seed] generates a vector of random permutation of a number of int count taken from a vector of vec without permitting an overlap of elements with a seed of seed.";

NullChecker::usage="NullChecker[list, items] interpolate null cells in a matrix of list with its head names as items.";

NGetCorrelation::usage="NGetCorrelation[list, items] returns correlation coefficents of each explanatory variable and the target variable. The list is a matrix with its last column as the target varialbes and other columns as explanatory variables.";

ShowConfusionMatrix2::usage="ShowConfusionMatrix2[pairs, cutoff] shows the result of statistical analyses with a dataset of pairs that is composed of a list of {predicted value, obersved value} with a cutoff of cutoff as a theahold of predicted value to make it dichotomous. The observed value is either 1 or 0.";

cStatistics::usage="cStatistics[pairs] returns a c-statistics value from a dataset of {predicted value, obseved value. The observed value consists of 1 or 0.";

DeterminationCoefficient::usage="DeterminationCoefficient[observed_values, estimated_values] resturns a determination coefficient, R^2.";

DataIn::usage="DataIn[] imports a spread sheet file and makes executable lists without deleting empty records. The body of data is expressed by DATA or DATA[1], DATA[2], ....., if the file is composed of multiple sheets. The first row of the sheet is expressed by ITEMS or ITEMS[1], ITEMS[2], ..... that are running-numberd. In the typical setting of an Excel file to be imported for survival rate analyses, the 1st, 2nd, 3rd, 4th colums should be ID number(dummy column), starting date, ending(death or lost) date, censored date. The 1st column is not considered in the calcualtion and could be omitted.  The date should be enterd in the spread sheet file in a format of {2012,3,4}, 2012/3/4 or 2012,3,4. It is also allowed to enter the date and time of day like {2012,3,4,14,31,23} that is {yy, mm, dd, hh, mm, ss}. The 4th column and later in the sheet should be data of the characteristics or risk factors of each subject, like age, gender, ht, wt, and so on. Both numeric and symbolic inputs are allowed. It is expected in the programs of this package that each row represents the data for one subject and that each column represents items needed for survival rate analyses, such as ID, dates needed for the analysis, risk factors and background variables.";

PruneEnsembleVariables::usage="PruneEnsembleVariables[function] returns a list of variables that are used in teh function. The function is in a form of Function in Mathematica.";

(* ROC curve *)

ToROCAssociation::usage="ToROCAssociation[ {trueLabel, falseLabel}, actualLabels, predictedLabels] converts two labels lists (actual and predicted) into an Association that can be used as an argument for the ROC functions. See ROCFunctions .";

ROCAssociationQ::usage="Verifies that the argument is a valid ROC Association object. A ROC Association object has the keys \"TruePositive\", \"FalsePositive\", \"TrueNegative\", and \"FalseNegative\" .";

ROCFunctions::usage="Gives access to the implement ROC functions.
It can be used as Thread[ROCFunctions[][rocAssoc]] or Thread[ROCFunctions[{\"TPR\",\"SPC\"}][rocAssoc]] .See ROCFunctions[\"FunctionInterpretations\"] for available functions and their interpretations.";

ROCPlot::usage="Makes a standard ROC plot for specified parameter list and corresponding ROC Association objects. ROCPlot takes all options of Graphics and additional options for ROC points size, color, callouts, tooltips, and joining. The allowed signatures are: \nROCPlot[ aROCs:{_?ROCAssociationQ..}, opts] \nROCPlot[ parVals:({_?NumericQ..}|Automatic), aROCs:{_?ROCAssociationQ..}, opts] \nROCPlot[ xFuncName_String, yFuncName_String, aROCs:{_?ROCAssociationQ..}, opts] \nROCPlot[ xFuncName_String, yFuncName_String, parVals:({_?NumericQ..}|Automatic), aROCs:{_?ROCAssociationQ..}, opts]";

ROCValues::usage="ROCValues[predictionProbabilities_Dataset, actualLabels_List, thRange_?VectorQ ] computes ROC associations (for ROCPlot).";

ToClassifyROCCurvePlot::usage="Changes the style of ROCPlot plots. (Experimental.)";

ConfusionMatrixPlot::usage="ConfusionMatrixPlot[ aROC_?ROCAssociationQ, labelNames: {yesLabel_, noLabel_}
plots a confusion matrix based on a ROC association.";

ConfusionMatrixPlotFrame::usage="ConfusionMatrixPlotFrame[mat, refMat, rowNames, columnNames, opts]
frames a given confusion matrix.";

TPR::usage="";
SPC::usage="";
PPV::usage="";
NPV::usage="";
FPR::usage="";
FDR::usage="";
FNR::usage="";
ACC::usage="";
FOR::usage="";
F1::usage="";
AUROC::usage="";
MCC::usasge="";
aROCAcronyms::usasge="";
aROCFunctions::usage="";
ROCSpecQ::usage="";


Begin["`Private`"];    

$Path=Join[{ ToFileName[{$HomeDirectory,"desktop"}], "/Library/Mathematica/Autoload/Labodata", "/Library/WolframDesktop/Autoload/Labodata","/Users/spaceblue/Documents","/Users/spaceblue/Documents/jsdt",
"C:\\ProgramData\\Mathematica\\Autoload\\Labodata"}, $Path];

Setup[]:=Module[ {current, input},
current=NotebookDirectory[];
Print[ "                                                 "];
Print[Style[StringForm["\[FilledSquare] Setting the Working Directory to the Directory where this notebook was SAVED"],Bold,16,Purple]];
Print[ "                                                 "];
Label["notesave"];
If[
current===$Failed,
Print[Style[StringForm["Frist, save this notebook.\nAnd Press OK to set up the working directory."], Bold, Red, 15]];
Print[ "                                                 "];
input=InputString["Press OK AFTER SAVING this notebook.\nType end to quit.", WindowMargins->{{Automatic, 10}, {Automatic, 10}}];
Which[
input==="end"||input==="quit",
Goto["setupending"],
input===""&&(NotebookDirectory[]=!=$Failed),
Print[Style[StringForm["The previous working directory was ``", Directory[]],Bold,15]];Goto["next"],
input===""&&(NotebookDirectory[]===$Failed),
ErrorMessage["This notebook is not saved. Save it, first."];Goto["notesave"],
True,
ErrorMessage["Unexpected error"];Goto["setupending"]
],
Print[Style[StringForm["The previous working directory was ``", Directory[]],Bold, 15]]
];
Label["next"];

SetDirectory[NotebookDirectory[]];
Print[Style[StringForm["\[FilledDiamond] The working directory has been changed to ``", Directory[]],16,Bold,Blue]];
Print[ "                                                 "];
Print[Style[StringForm["The Working Directory can be returned to the HOME Directory by running Setdown[] command."], Bold,15]];
Print[ "                                                 "];
Label["setupending"];
];

SetUp[]:=Setup[];

Setdown[]:=Module[ {current, input, home},
current=NotebookDirectory[];
home=$HomeDirectory;
Print[ "                                                 "];
Print[Style[StringForm["\[FilledSquare] Returning the Working Directory to the HOME Directory"],Bold,16,Purple]];
Print[ "                                                 "];
Label["notesave"];

Which[
current===$Failed&&Directory[]===home,
Print[Style[StringForm["\[EmptyDiamond] The current Working directory is already the HOME directory ``", home],Bold,16]];
Print[ "                                                 "];
Goto["setdownending"],

current===$Failed&&Directory[]=!=home,
Print[Style[StringForm["\[EmptyDiamond] The current Working directorywas ``",Directory[]],Bold,16]];
SetDirectory[];
Print[Style[StringForm["\[FilledDiamond] The working directory has been changed to the HOME directory ``", Directory[]],16,Bold,Blue]];
Print[ "                                                 "],

current=!=$Failed&&Directory[]=!=home,
Print[Style[StringForm["\[EmptyDiamond] The current Working directory was ``",NotebookDirectory[]],Bold,16]];
SetDirectory[];
Print[Style[StringForm["\[FilledDiamond] The working directory  has been changed to the HOME directory ``", Directory[]],16,Bold,Blue]];
Print[ "                                                 "],

current=!=$Failed&&Directory[]===home,
Print[Style[StringForm["\[EmptyDiamond] The current Working directory was already the HOME directory ``", home],Bold,15]];
Print[ "                                                 "];
Goto["setdownending"],

True,
ErrorMessage["Unexpected Error."];Goto["setdownending"]
];

Label["setdownending"];
];

SetDown[]:=Setdown[];

PB[]:= Print["                              "];

Ordinalize[m_]:= Module[ {n,letters, lastL, rest},
(* \:6574\:6570\:3092\:5e8f\:6570\:306b\:5909\:3048\:308b\:30021st, 2nd, 3rd, 4th, ..... *)
n=ToExpression@m;
If[ IntegerQ@n&&Positive@n,

letters=StringSplit[ToString@n, ""];
lastL=Last@letters;LASTL=lastL;
rest=If[ Length@letters===1, {""}, Drop[letters,-1]];
Which[
n===1, "1st",
n===2, "2nd",
n===3, "3rd",
4<= n<= 20, StringJoin[ToString@n, "th"],
n>= 21,
	Which[
		lastL==="1", StringJoin[StringJoin[rest], "1st"],
		lastL==="2", StringJoin[StringJoin[rest], "2nd"],
		lastL==="3", StringJoin[StringJoin[rest], "3rd"],
		True,  StringJoin[StringJoin[letters], "th"]
	],

True, m
],
m]
];

MinToHourMinSec[ele_]:= Module[ {h,m,s},
If[ NumberQ[ele],
h=Floor[ele/60.];m=Floor[ele-h*60]; s=(ele-h*60-m )*60;
{h,m,Floor[s]}, ErrorMessage["The input must be a number."];ele, ErrorMessage["The input must be a number."];ele]];

HourMinSecToSec[vec_]:=
Which[ vec===Null, Null,RealVectorQ[vec,3]||RealVectorQ[ToExpression[vec],3],vec[[3]]+60*vec[[2]]+60*60*vec[[1]], True, Null];

rawSecToHourMinSec[sec_]:=Module[ {fullsec=Null, hour, min, secon},
Which[Length[sec]===0, fullsec=sec, Length[sec]===1||Length[sec]===2,
fullsec=sec[[1]],True,Print["StoHMSerror"]];
Which[ NumberQ[fullsec]&&fullsec>= 0,
hour=Floor[N[fullsec/(60 60)]];
min=Floor[N[(fullsec-60 60hour)/60.]];
secon=Round[fullsec-60 60 hour-60min];
{hour,min,secon},
NumberQ@fullsec&&fullsec<0,
hour=Ceiling[N[fullsec/(60 60)]];
min=Ceiling[N[(fullsec-60 60hour)/60.]];
secon=Round[fullsec-60 60 hour-60min];
{hour,min,secon},
True, {"error","error","error"}
]
];

DateTime[vec_, opt___]:=If[ RealVectorQ[vec]&&Length@vec===6,
(* Date[]\:306e\:51fa\:529b\:3092\:3001\:5e74\:6708\:65e5\:3068\:6642\:5206\:79d2\:306b\:5206\:3051\:3066\:8868\:793a\:3002opt\:306fSyle\:306eparamter *)
Style[StringForm["Date: ``, ``, ``, Hour: ``, ``, ``", vec[[1]],vec[[2]],vec[[3]],vec[[4]],vec[[5]],NumberForm[vec[[6]],4]], opt],Null,Null];

PresentTime:=StringJoin[Map[ ToString, Join[Drop[Date[],-1], Floor[ Take[Date[],-1]]]]];

DayTime[opts___]:=Module[ {day, time0,time},
day=Take[Now[[1]],3];
time0=Drop[Now[[1]],3];
time={time0[[1]],time0[[2]],Round[time0[[3]]]};
Print[Style[StringForm["\[FilledDiamond] Present Time: ``\:5e74 ``\:6708 ``\:65e5\:3000``\:6642 ``\:5206 ``\:79d2", day[[1]],day[[2]],day[[3]],time[[1]],time[[2]], time[[3]]], opts]]
];

JoinDataItems[data_, items_]:=
 Prepend[data, items[[All,2]] ] ;

NF[numb_, digit_:4]:=If[ NumberQ[numb]===False, Null, NumberForm[N[numb],digit]];

CutLast[list_]:=If[ Length[list]>0, Drop[list,-1], list, list];

Numberings[list_?ListQ]:=
MakeTwin[ Range[Length[list]],list ];

NPosition[list_, ele_]:=Flatten[ Position[ list, ele]]; 

MakeTwin[ data1_, data2_]:=
If[ Length[data1]===Length[data2],
Transpose[ {data1,data2}],
Message[MakeTwin::badarg]];

MakeTwin::badarg="The lengths of two arguments must be equal.";

MakeTwin[{data1_, data2_}]:=
MakeTwin[data1, data2];

Fuse[vec1_, vec2_]:=
Module[ {dim1=Dimensions[vec1],dim2=Dimensions[vec2], output=Null},
(* vec1, vec2 \:306b\:306f\:540c\:3058\:9577\:3055\:306e\:30ea\:30b9\:30c8\:304c\:305d\:308c\:305e\:308c\:6765\:308b *)
If[ dim1[[1]] =!=dim2[[1]], ErrorMessage[ "The lengths of both data must be equal."];Goto[ending]];
Which[ Length[dim1]===1 && Length[dim2] ===1,
output=MapThread[ {#1,#2}&,{vec1,vec2}],
Length[dim1]>= 2 && Length[dim2]===1,
output=MapThread[ Append[#1, #2]&, {vec1, vec2}],
Length[dim1]===1&&Length[dim2]>= 2,
output = MapThread[ Prepend[#2,#1]&, {vec1, vec2}],
Length[dim1]>= 2 && Length[dim2]>= 2,
output = MapThread[ Join, {vec1,vec2}],
True,
ErrorMessage["Fuse error"];Goto[ending]
];
Label[ending];
output
];

Fuse[{vec1_,vec2_}]:= Fuse[vec1,vec2];

NullRemoving[list_]:=
DeleteCases[ list, Null];

NullCleaning[list_]:= 
IntactComplement[Map[ If[MemberQ[#, Null]||MemberQ[#, "Null"],Null, #]&, list], {Null}];

StringRemoving[list_]:=
Module[ {f, x},
f[x_]:= If[  NumberQ[x]===False, Null, x, Null];
NullRemoving[ Map[ f, list ] ]
];

TakeElement[{list_?ListQ, n_?IntegerQ}, ele_]:=
Module[ {output = Null, column, pos},

If[list==={}, Goto["ending"]];

If[  n <= 0 || n >Dimensions[list][[2]],
ErrorMessage["TakeElement Error : inapproapriate n"];Goto["ending"] ];

column = list[[All, n]];
If[ MemberQ[ Union[ column] , ele ] =!= True,
(* ErrorMessage[ele, "not included in the designated column."]; *)Goto["ending"] ];

output = Part[ list, Flatten[ Position[ column, ele ] ] ];

Label["ending"];
output 
];

TakeElement[list_?ListQ, n_?IntegerQ, ele_]:= TakeElement[{list,n},ele];

FilterElement[list_?ListQ, n_?IntegerQ, func_]:=
Module[ {len,data, pos, output={}},
If[list==={}, Goto["ending"]];
If[ SyntaxQ[ ToString[ func ] ]===False,
ErrorMessage["The func must be a syntax."];Goto["ending"] ];
len = Dimensions[list][[2]];
If[ n > len || n < 0, 
ErrorMessage[ "The n must be a positive integer greater than the number of columns."];Goto["ending"] ];
data = list[[All, n]];
pos = Flatten[ Position[ Map[ func, data ], True]];
output=Part[ list, pos];
Label["ending"];
output
];

FilterElement[{list_?ListQ, n_?IntegerQ}, func_?SyntaxQ]:=
FilterElement[list, n, func];

RealVectorQ[vec_, n_]:=
If[(StringFreeQ[ToString[vec], "{"]===False)&&(StringFreeQ[ToString[vec], "}"]===False)&&VectorQ[ToExpression[N[vec]]]&&Union[Map[NumericQ,ToExpression[N[vec]]]]==={True}&&Length[vec]===n,True,False];

RealVectorQ[vect_]:=
If[(StringFreeQ[ToString[vect], "{"]===False)&&(StringFreeQ[ToString[vect], "}"]===False)&&VectorQ[ToExpression[N[vect]]]&&Union[Map[NumericQ,ToExpression[N[vect]]]]==={True},True,False];

NMean[list_?ListQ]:=Module[ {canda, nmf, nmele},
nmf[nmele_]:= If[ NumberQ[nmele], 1, 0, 0];
canda=NPosition[Map[ nmf, list],1];
If[Length[canda]>0, N[ Mean[ Part[list,canda]]], Null, Null]];

NStandardDeviation[list_?VectorQ]:=Module[{cands},
	cands = StringRemoving[ list ];
	Which[
	Length[cands]===0,
		Null,
	Length[cands]===1,
		0, 
	Length[cands]>= 2,
	 N[ StandardDeviation[  cands] ], 
	True, Null ]];

NMedian[list_?ListQ]:=Module[ {canda, nmf, nmele},
nmf[nmele_]:= If[ NumberQ[nmele], 1, 0, 0];
canda=NPosition[Map[ nmf, list],1];
If[Length[canda]>0, N[ Median[ Part[list,canda]]], Null, Null]];

NMedian[list__]:= If[ VectorQ[{list}]===True,NMedian[{list}]];

NMax[list_?VectorQ]:=Module[{cands},
cands =StringRemoving[ list ];
If[ Length[cands]>0,
Max[cands], Null, Null ]
];

NMax[list__]:= Module[{can},
can=StringRemoving[ Apply[ List, Hold[list]]];
If[ can ==={}, Null, Max[can] ] ];

NMin[list_?VectorQ]:=Module[{cands},
cands = StringRemoving[ list ];
If[ Length[cands]>0,
Min[cands], Null, Null ]
];

NMin[list__]:= Module[{can},
can=StringRemoving[ Apply[ List, Hold[list]]];
If[ can ==={}, Null, Min[can] ] ];

ErrorMessage[string_]:=Print[Style[string,FontColor->RGBColor[0, 0.719463, 0.0528573]]];

ErrorMessage[string_,{r_,g_,b_}]:=Print[Style[string,FontColor->RGBColor[r,g,b]]];

ErrorMessage[string1_,string2_]:=Print[Style[ToString[string1]<>" : "<>ToString[string2],FontColor->RGBColor[0, 0.719463, 0.0528573]]];

ErrorMessage[string1_,string2_,{r_,g_,b_}]:=Print[Style[ToString[string1]<>" : "<>ToString[string2],FontColor->RGBColor[r,g,b]]];

NFindFile[filename_]:= Module[{judge0, judge1,judge2, judge3,out, choice,readables, existQ, existN, existP, alljudges, exist1,exist2,exist3, exist4},
judge0=FindFile[ToString[filename]];
judge1=FindFile[StringJoin[ ToString[filename], ".xls"]];
judge2=FindFile[StringJoin[ToString[filename],".xlsx" ]];
judge3=FindFile[StringJoin[ToString[filename],".csv" ]];

alljudges={judge0,judge1,judge2,judge3};

existQ=Map[ #=!=$Failed&, alljudges];
existN=Count[existQ, True];
existP=NPosition[ existQ, True];

Label["choiceagain"];
Which[
existN===0,
readables=$Path; PB[];Print[Style[StringForm["\[FilledDiamond] The file `` was not found. \n\nCheck if `` is located in the following folders where Mathematica can read.:\n
$Path = ``\n\nThese paths can be identified by evaluating $Path.", filename, filename, readables],14]];PB[];
out=$Failed,

existN===1,
out=Part[alljudges,  existP[[1]]],

existN===2,
exist1=alljudges[[existP[[1]]]]; exist2= alljudges[[existP[[2]]]];
choice= InputString[StringForm["Two files were found:\n`` and ``\n Type 1 for ``, Type 2 for ``\n\nPress OK to choose ``\nType end to quit",
					exist1,exist2, exist1, exist2, exist1], WindowMargins-> {{Automatic, 10},{Automatic,10}}];
Which[choice==="1",out= exist1, choice==="2", out=exist2, choice==="", out = exist1, choice==="end"||choice==="quit", Goto["datainending"],True,  ErrorMessage["error"]; Goto["choiceagain"]],

existN===3,
exist1=alljudges[[existP[[1]]]]; exist2= alljudges[[existP[[2]]]]; exist3=alljudges[[existP[[3]]]];
choice= InputString[StringForm["Three files were found:\n``, `` and ``\n Type 1 for ``, Type 2 for ``, Type 3 for ``\n\nPress OK to choose ``\nType end to quit",
					exist1, exist2, exist3, exist1, exist2, exist3  ]];
Which[choice==="1",out= exist1, choice==="2", out=exist2,choice==="3", out=exist3, choice==="", out = exist1,choice==="end"||choice==="quit", Goto["datainending"], True,  ErrorMessage["error"]; Goto["choiceagain"]],

existN===4,
exist1=alljudges[[existP[[1]]]]; exist2= alljudges[[existP[[2]]]]; exist3=alljudges[[existP[[3]]]];exist4=alljudges[[existP[[4]]]];
choice= InputString[StringForm["Four files were found:\n``, ``, `` and ``\n Type 1 for ``, Type 2 for ``, Type 3 for ``, Type 4 for ``\n\nPress OK to choose ``\nType end to quit",
					exist1, exist2, exist3,exist1, exist1, exist2, exist3,exist4  ]];
Which[choice==="1",out= exist1, choice==="2", out=exist2,choice==="3", out=exist3, choice==="4", out=exist4, choice==="", out = exist1,choice==="end"||choice==="quit", Goto["datainending"],True,  ErrorMessage["error"]; Goto["choiceagain"]],

True, ErrorMessage["Unexpected choice error"]; Goto["choiceagain"]
];
Label["datainending"];
out
];

NFindFile3[filename_]:= Module[{judge0, judge11,judge2, judge3,out, choice,readables, existQ, existN, existP, alljudges, exist1,exist2,exist3, exist4},

(* NFindFile3 only checks if the file exits or not. *)

judge0=FindFile[ToString[filename]];
judge11=FindFile[StringJoin[ ToString[filename], ".xls"]];
judge2=FindFile[StringJoin[ToString[filename],".xlsx" ]];
judge3=FindFile[StringJoin[ToString[filename],".csv" ]];

alljudges={judge0,judge11,judge2,judge3};

existQ=Map[ #=!=$Failed&, alljudges];
existN=Count[existQ, True];
existP=NPosition[ existQ, True];

Which[
existN===0,
readables=$Path; PB[];Print[Style[StringForm["\[FilledDiamond] The file `` was not found. \n\nCheck if `` is located in the following folders where Mathematica can read.:\n
$Path = ``\n\nThese paths can be identified by evaluating $Path.", filename, filename, readables],14]];PB[];
out=False,

existN>= 1,
out=True,

True, ErrorMessage["Unexpected choice error"]; out = False
];

out
];

IntactComplement[list_?ListQ,omit_?ListQ]:=
Module[ {nlist, gathered, len,k},
nlist = Map[ If[ MemberQ[ omit, #], Null, #, 
		Message[IntactComplement::badarg] ]&, list ];
gathered = {}; len = Length[nlist];
For[ k = 1, k <= len, k++,
If[ nlist [[k]] =!= Null,
	gathered = Append[gathered, nlist[[k]] ] ]
	];
gathered];

IntactComplement::badarg="Unexpected data error";

cleandata[list_?ListQ]:= Module[ {f, ele, g, tag, nvectorq,check, periodcleaner},

nvectorq[ele_]:=Length[ele]>= 3 && VectorQ[ele]&&Union[Map[NumberQ, ele]]==={True};

g[ele_, tag_]:= ToExpression[StringJoin["{", StringReplace[ToString[ele], tag -> ","], "}" ]];

check[ele_]:= StringReplace[ele, {"[" -> "{", "]" -> "}", " " -> ""}];

periodcleaner[ele_]:= StringReplace[ ele, "."-> ","];

f[ele_]:= 
Which[ ele===Null,Null,
ele==={},{},
ele==="", Null, 
ele===" ", Null,
ele==="\:3000", Null,
ele==="Null", Null,
DateObjectQ[ele], ele[[1]],
IntegerPart[ele]-ele===0.,IntegerPart[ele], 

RealVectorQ[ToExpression[StringJoin["{", ToString[ele], "}"]],3], 
ToExpression[ StringJoin["{", ToString[ele], "}"] ],

LetterQ[ToString[ele]], ToExpression[ele],
NumberQ[ele],ele,

StringQ[ele]&&NumberQ[ToExpression[ele]],ToExpression[ele],
StringQ[ele]&&Length[ToExpression[ele]]===0,
ele,

StringQ[ele]&&RealVectorQ[ToExpression[check[ele]],3],ToExpression[check[ele]],
StringQ[ele]&&RealVectorQ[ToExpression[periodcleaner[ele]],3], ToExpression[periodcleaner[ele]],

nvectorq[g[ele, "."]],g[ele, "."],
StringQ[ele]&& NumberQ[g[ele, "."]], g[ele, ","],
nvectorq[g[ele, "/"]],g[ele, "/"],

RealVectorQ[ToExpression[StringJoin[ToString[ele], "}"]],3],
ToExpression[StringJoin[ToString[ele], "}"]],
RealVectorQ[ToExpression[StringJoin["{",ToString[ele]]],3],
ToExpression[StringJoin["{",ToString[ele]]],

Length[ele]===6&&Total[Drop[ele,3]]==0, Take[ele,3],

Length[ToExpression[ele]]===6&&Total[Drop[ToExpression[ele],3]]==0,Take[ToExpression[ele],3],

True,ToExpression[ele]
];

Map[f, list, {2}]
];

ExportExcel[filebody_,filehead_,excelname_]:=Module[ {fdim, rownumb, eei, eecount, denovoname,eefulldata, current},

(* filebody: \:30c7\:30fc\:30bf\:672c\:4f53\:3001filehead: \:9805\:76ee\:540d\:3001excename: \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0 *)
(* \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0\:306f excelname + No + integer + Date + \:5e74\:6708\:65e5.xlsx \:3068\:306a\:308b *)
(* \:5217\:306e\:6570\:306f200\:4ee5\:4e0b\:305a\:3064\:306b\:306a\:308b *)

fdim=Dimensions[filebody];Global`EEFDIM=fdim;
Which[
ListQ[filebody]===False, ErrorMessage["The body of data was not a list."];Goto["EEending"],
ListQ[filehead]===False, ErrorMessage["The head of data was not a list."];Goto["EEending"],
fdim[[2]]=!=Length[filehead], ErrorMessage["The column nmber of the body of data was not equal to the length of the head of the data."];Goto["EEending"]
];

rownumb=fdim[[2]];eecount=Ceiling[rownumb/200];
Global`ROWNUMB=rownumb; Global`EECOUNT=eecount;
eefulldata=JoinDataItems[filebody, filehead];Global`EEFULLDATA=eefulldata;

current=NotebookDirectory[];

Which[ eecount===1,

If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No",ToString@eei,*) "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+200(eei-1), NMin[200 eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nwith the following name: ``", $HomeDirectory,denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No",ToString@eei, *)"Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+200(eei-1), NMin[200 eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nin `` files with the following names: ``", NotebookDirectory[],eecount,denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],

ErrorMessage["Unexpected Export Error"];Goto["EEending"]

],


IntegerQ[eecount]&&eecount>= 2,


If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],"No",ToString@eei, "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+200(eei-1), NMin[200 eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nin `` files with the following names: ``", $HomeDirectory,eecount,Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],"No",ToString@eei, "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+200(eei-1), NMin[200 eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nin `` files with the following names: ``", NotebookDirectory[],eecount,Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],

ErrorMessage["Unexpected Export Error"];Goto["EEending"]

],

True, ErrorMessage["The data was not exported for an unknow reasons"]
];



Label["EEending"];
];

ExportExcel2[filebody_,filehead_,excelname_]:=Module[ {fdim, rownumb, eei, eecount, denovoname,eefulldata,mleng, current},

(* \:5217\:306e\:6570\:30921200\:4ee5\:4e0b\:306b\:3057\:305f\:3082\:306e *)
(* filebody: \:30c7\:30fc\:30bf\:672c\:4f53\:3001filehead: \:9805\:76ee\:540d\:3001excename: \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0 *)
(* \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0\:306f excelname + No + integer + Date + \:5e74\:6708\:65e5.xlsx \:3068\:306a\:308b *)

mleng=1200;  (* mleng: \:4e00\:3064\:306eExcel\:306e\:5217\:306e\:9577\:3055\:306e\:6700\:5927 *)

fdim=Dimensions[filebody];Global`EEFDIM=fdim;
Which[
ListQ[filebody]===False, ErrorMessage["The body of data was not a list."];Goto["EEending"],
ListQ[filehead]===False, ErrorMessage["The head of data was not a list."];Goto["EEending"],
fdim[[2]]=!=Length[filehead], ErrorMessage["The column nmber of the body of data was not equal to the length of the head of the data."];Goto["EEending"]
];

rownumb=fdim[[2]];eecount=Ceiling[rownumb/mleng];
Global`ROWNUMB=rownumb; Global`EECOUNT=eecount;
eefulldata=JoinDataItems[filebody, filehead];Global`EEFULLDATA=eefulldata;

current=NotebookDirectory[];

Which[ eecount===1,

If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No",ToString@eei,*) "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nwith the following name: ``", $HomeDirectory,denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No",ToString@eei, *)"Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nwith the following name: ``", NotebookDirectory[],denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],

ErrorMessage["Unexpected Export Error"];Goto["EEending"]
],

IntegerQ[eecount]&&eecount>= 2,

If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],"No",ToString@eei, "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nin `` files with the following names: ``", eecount,$HomeDirectory,Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],"No",ToString@eei, "Date", PresentTime,ToString[eei], ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nwith the following name: ``", NotebookDirectory[],Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],

ErrorMessage["Unexpected Export Error"];Goto["EEending"]
],

True, ErrorMessage["The data was not exported for an unknow reasons"]
];



Label["EEending"];
];

ExportExcel3[filebody_,filehead_,excelname_]:=Module[ {fdim, rownumb, eei, eecount, denovoname,eefulldata,mleng, current},

(* \:5217\:306e\:6570\:30921200\:4ee5\:4e0b\:306b\:3057\:305f\:3082\:306e *)
(* filebody: \:30c7\:30fc\:30bf\:672c\:4f53\:3001filehead: \:9805\:76ee\:540d\:3001excename: \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0(\:3053\:308c\:306b\:306f.xlsx\:306f\:3064\:3051\:306a\:3044) *)
(* \:51fa\:529b\:30d5\:30a1\:30a4\:30eb\:306e\:540d\:79f0\:306f excelname.xlsx \:3068\:306a\:308b\:3002\:5e74\:6708\:65e5\:306f\:3064\:304b\:306a\:3044\:3002 *)

mleng=1200;  (* mleng: \:4e00\:3064\:306eExcel\:306e\:5217\:306e\:9577\:3055\:306e\:6700\:5927 *)

fdim=Dimensions[filebody];Global`EEFDIM=fdim;
Which[
ListQ[filebody]===False, ErrorMessage["The body of data was not a list."];Goto["EEending"],
ListQ[filehead]===False, ErrorMessage["The head of data was not a list."];Goto["EEending"],
fdim[[2]]=!=Length[filehead], ErrorMessage["The column nmber of the body of data was not equal to the length of the head of the data."];Goto["EEending"]
];

rownumb=fdim[[2]];eecount=Ceiling[rownumb/mleng];
Global`ROWNUMB=rownumb; Global`EECOUNT=eecount;
eefulldata=JoinDataItems[filebody, filehead];Global`EEFULLDATA=eefulldata;

current=NotebookDirectory[];

Which[ eecount===1,

If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No", ToString@eei,*) ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nwith the following name: ``", $HomeDirectory,denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],(* "No", ToString@eei,*) ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nwith the following name: ``", NotebookDirectory[],denovoname[1]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],


ErrorMessage["Unexpected Export Error"];Goto["EEending"]

],

IntegerQ[eecount]&&eecount>= 2,


If[current===$Failed,

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname], "No", ToString@eei, ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the home directory: ``\nin `` files with the following names: ``", $HomeDirectory,eecount,Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]],

SetDirectory[NotebookDirectory[]];

For[eei=1, eei<= eecount, eei++,
denovoname[eei]=StringJoin[ToString[excelname],"No", ToString@eei,  ".xlsx"];
Export[denovoname[eei], eefulldata[[All, Range[1+mleng(eei-1), NMin[mleng*eei, rownumb  ]  ] ]] ]
];

Print[Style[StringForm["\[FilledDiamond] The data in the analysis was exported in the current directory: ``\nin `` files with the following names: ``", NotebookDirectory[],eecount,Table[denovoname[eei], {eei,1,eecount}]],15,RGBColor[0.00709545, 0.646342, 0.0588235]]];

SetDirectory[],

ErrorMessage["Unexpected Export Error"];Goto["EEending"]

],


True, ErrorMessage["The data was not exported for an unknow reasons"]
];

Label["EEending"];
];


ListNormalize[databody_, dataheads_]:=Module[ {nullchecker, bmlist0, bmitems0, elems, databody2, dataheads2, databody3, dataheads3},

If[(ListQ@databody&&(Depth@databody===3))===False, ErrorMessage["The first augment is inappropriate."];Goto["liststandending"]];
If[(ListQ@dataheads&&(Depth@dataheads===3))===False, ErrorMessage["The second augment is inappropriate."];Goto["liststandending"]];
If[Dimensions[databody][[2]]=!=Dimensions[dataheads][[1]], ErrorMessage["The column count of the body of data must be equal to the row count of the heads of data."];Goto["liststandending"]];

(* Null Checker *)
nullchecker[bmlist0_, bmitems0_]:=Module[ {nclen, ji, nbag, nline, nlout, nljudge, allmeans, address1, func, bpos, bmean, rules1, realpos, allmedians, address2, rules2, ncleng, mbag, mline,realrow, emptyrow, emptyitems, bmlist, bmitems},

nbag={};mbag={};
func[bpos_,bmean_]:={bpos-> bmean[[bpos[[2]]]]};
Global`BMLIST0=bmlist0;

ncleng=Length[Transpose[bmlist0]];

(* \:7a7a\:306e\:5217\:306e\:51e6\:7406 *)
For[ji=1, ji<= ncleng, ji++,
mline=Transpose[bmlist0][[ji]];
AppendTo[mbag, If[Union[Map[NumberQ, mline]]==={False}, 1, 0, 0]]
];

If[MemberQ[mbag, 1]===False, bmlist=bmlist0; bmitems=bmitems0;Goto["nextphase"]];

realrow=NPosition[mbag, 0]; emptyrow=NPosition[mbag,1];
bmlist=Transpose[Part[Transpose[bmlist0], realrow]];interlist=bmlist;
bmitems=Part[ bmitems0, realrow];
emptyitems=MakeTwin[emptyrow,Part[ bmitems0, emptyrow]];
Print[Style[StringForm["\[EmptyDiamond] Completely empty or non-numeric rows were excluded.\n  Their row numbers and item names are ``.", emptyitems], Bold, 15, Red]];
Print["                                                    "];


Label["nextphase"];

nclen=Length[bmlist];
nlout={bmlist,bmitems};

(* \:7a7a\:306e\:30bb\:30eb\:306e\:51e6\:7406 *)
For[ ji=1, ji<= nclen, ji++,
nline=bmlist[[ji]];
AppendTo[nbag, If[Union[Map[NumberQ, nline]]==={True}, 0, 1, 1]]
];

If[MemberQ[nbag, 1]===False,  Goto["ncending"]];Global`NBAG=nbag;
Global`BMLIST=bmlist;

Label["nljudgeagain"];
nljudge=InputString["Some columns contain non-numeric cells.\nPress OK to replace empty cell with median value of the row.\n\nType a to replace empty cell with average value of the row.\nType n to leave empty cells as they are.\nType x to excluede those columns.\nType end to quit.", WindowMargins->{{Automatic,10}, {Automatic, 10}}];

allmeans=Map[ NMean, Transpose[bmlist]];Global`ALLMEANS=allmeans;
allmedians=Map[ NMedian, Transpose[bmlist]];Global`ALLMEDIANS=allmedians;

Which[
nljudge==="end"||nljudge==="end", nlout="terminated";Goto["ncending"],

nljudge==="x",

realpos=NPosition[nbag, 0];Global`REALPOS=realpos;
nlout={Part[bmlist, realpos],bmitems};
Print[Style["Columns that contain non-numeric cells were excluded.",15, Purple, Bold]];
Print[Style[StringForm["\[EmptyDiamond] Initial N of columns = ``, Final N of columns = ``", nclen, Length[nlout[[1]]]], 15,Bold]];
Print["                                           "],

nljudge==="a"&&Union[Map[NumberQ, allmeans]]==={True},

address1=Position[Map[NumberQ, bmlist,{2}], False];Global`ADDRESS1=address1;
rules1=Flatten[Map[ func[#,allmeans ]&, address1],1];Global`RULES1=rules1;
nlout={ReplacePart[bmlist, rules1],bmitems};
Print[Style["\[EmptyDiamond] Non-numeric cells were replaced with average values of the row.", 15,Purple, Bold]];
Print["                                           "],

nljudge===""&&Union[Map[NumberQ, allmedians]]==={True},

address2=Position[Map[NumberQ, bmlist,{2}], False];
rules2=Flatten[Map[ func[#,allmedians]&, address2],1];
nlout={ReplacePart[bmlist, rules2],bmitems};
Print[Style["\[EmptyDiamond] Non-numeric cells were replaced with median values of the row.",15, Purple, Bold]];
Print["                                           "],

nljudge==="n",
Print[Style["\[EmptyDiamond] Empty cells were left unchanged.",15,Bold]];
Print["                                           "];Goto["ncending"],

nljudge==="a"&&Union[Map[NumberQ, allmeans]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value.",15, Purple, Bold];Goto["nljudgeagain"],

nljudge===""&&Union[Map[NumberQ, allmedians]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value."];Goto["nljudgeagain"],

True,
ErrorMessage["Type in, again."];Goto["nljudgeagain"]
];

Label["ncending"];
nlout
];


If[  Union[Map[ NumericQ, Flatten@databody ]]=!={True},

Print["                                        "];
Print[Style[StringForm["\[FilledSquare] The data must be EMPTY CELL FREE for Normalization.\nINTERPOLATE the empty cells, first."], Bold, Red, 15]];
Print["                                        "];
{databody2, dataheads2}=nullchecker[databody,dataheads],

{databody2, dataheads2}={databody, dataheads}

         ];
Global`DATABODY2=databody2; Global`DATAHEADS2=dataheads2;

databody3=N@Transpose@Map[Rescale, Transpose@databody2];
dataheads3=dataheads2;

Label["liststandending"];
Global`DATABODY=databody3; Global`DATAHEADS=dataheads3;

{databody3, dataheads3}
];

NRandomSample[vec_, int_, seed_:Null]:= Module[ {output={}},
(* \:6574\:6570\:306e\:30d9\:30af\:30c8\:30eb\:3067\:3042\:308bvec\:304b\:3089\:3001\:91cd\:8907\:3092\:8a31\:3055\:305aint\:306e\:6570\:306e\:9806\:5217\:3092\:30e9\:30f3\:30c0\:30e0\:306b\:4f5c\:308a\:51fa\:3059 *)
(* seed\:304cNull\:3067\:3042\:308b\:3068\:3001\:4e71\:6570\:306e\:7a2e\:306f\:4efb\:610f\:3068\:306a\:308b *)
(* seed\:304c\:6574\:6570\:3067\:3042\:308b\:3068\:3001\:4e71\:6570\:306e\:7a2e\:304c\:56fa\:5b9a\:3055\:308c\:308b *)
If[RealVectorQ[ vec ]===False||IntegerQ[int]===False||(seed=!=Null&&IntegerQ[seed]===False),Goto["ending"]];
If[Union[Map[IntegerQ, vec]]=!={True}, Goto["ending"]];
If[Length[vec]<int, ErrorMessage["The counts of integers in the premitted range must be equal to or more than the count of the resultatnt permutation."];Goto["ending"]];
output=Which[IntegerQ@seed,SeedRandom[seed];RandomSample[vec, int],
		seed==Null,RandomSample[vec,int],
		True, error];
Label["ending"];
output
];

NullChecker[bmlist0_, bmitems0_]:=Module[ {nclen, ji, nbag, nline, nlout, nljudge, allmeans, address1, func, bpos, bmean, rules1, realpos, allmedians, address2, rules2, ncleng, mbag, mline,realrow, emptyrow, emptyitems, bmlist, bmitems},

(* non-numeric cells \:3092\:6570\:5b57\:3067\:7f6e\:304d\:63db\:3048\:308b *)
(* \:8a08\:7b97\:3092\:4e2d\:65ad\:3055\:305b\:305f\:5834\:5408\:306f {{},{}} \:3092\:51fa\:529b\:3059\:308b *)

nbag={};mbag={};nlout={{},{}};
func[bpos_,bmean_]:={bpos-> bmean[[bpos[[2]]]]};
Global`BMLIST0=bmlist0;

ncleng=Length[Transpose[bmlist0]];

(* \:7a7a\:306e\:5217\:306e\:51e6\:7406 *)
For[ji=1, ji<= ncleng, ji++,
mline=Transpose[bmlist0][[ji]];
AppendTo[mbag, If[Union[Map[NumberQ, mline]]==={False}, 1, 0, 0]]
];

If[MemberQ[mbag, 1]===False, bmlist=bmlist0; bmitems=bmitems0;Goto["nextphase"]];

realrow=NPosition[mbag, 0]; emptyrow=NPosition[mbag,1];
bmlist=Transpose[Part[Transpose[bmlist0], realrow]];

(* interlist=bmlist; *)
bmitems=Part[ bmitems0, realrow];
emptyitems=MakeTwin[emptyrow,Part[ bmitems0, emptyrow]];
Print["                                           "];
Print[Style[StringForm["\[EmptyDiamond] Completely empty or non-numeric rows were excluded.\n  Their row numbers and item names are ``.", emptyitems], Bold, 15, Red]];
Print["                                                    "];


Label["nextphase"];

nclen=Length[bmlist];
nlout={bmlist,bmitems};

(* \:7a7a\:306e\:30bb\:30eb\:306e\:51e6\:7406 *)
For[ ji=1, ji<= nclen, ji++,
nline=bmlist[[ji]];
AppendTo[nbag, If[Union[Map[NumberQ, nline]]==={True}, 0, 1, 1]]
];

If[MemberQ[nbag, 1]===False,  Goto["ncending"]];Global`NBAG=nbag;
Global`BMLIST=bmlist;

Label["nljudgeagain"];
nljudge=InputString["Some columns contain non-numeric cells.\nTo test the accuracy of the created model on the training data, non-numeric cells must be replaced with a number.\n\nPress OK to replace empty cell with median value of the row.\nType a to replace empty cell with average value of the row.\nType x to excluede those columns.\nType end to quit.", WindowMargins->{{Automatic,10}, {Automatic, 10}}];

allmeans=Map[ NMean, Transpose[bmlist]];Global`ALLMEANS=allmeans;
allmedians=Map[ NMedian, Transpose[bmlist]];Global`ALLMEDIANS=allmedians;

Which[
nljudge==="end"||nljudge==="end", nlout="terminated";Goto["ncending"],

nljudge==="x",

realpos=NPosition[nbag, 0];Global`REALPOS=realpos;
nlout={Part[bmlist, realpos],bmitems};
Print["                                           "];
Print[Style["Columns that contain non-numeric cells were excluded.",15, Purple, Bold]];
Print[Style[StringForm["\[EmptyDiamond] Initial N of columns = ``, Final N of columns = ``", nclen, Length[nlout[[1]]]], 15,Bold]];
Print["                                           "],

nljudge==="a"&&Union[Map[NumberQ, allmeans]]==={True},

address1=Position[Map[NumberQ, bmlist,{2}], False];Global`ADDRESS1=address1;
rules1=Flatten[Map[ func[#,allmeans ]&, address1],1];Global`RULES1=rules1;
nlout={ReplacePart[bmlist, rules1],bmitems};
Print["                                           "];
Print[Style["\[EmptyDiamond] Non-numeric cells were replaced with average values of the row.", 15,Purple, Bold]];
Print["                                           "],

nljudge===""&&Union[Map[NumberQ, allmedians]]==={True},

address2=Position[Map[NumberQ, bmlist,{2}], False];
rules2=Flatten[Map[ func[#,allmedians]&, address2],1];
nlout={ReplacePart[bmlist, rules2],bmitems};
Print["                                           "];
Print[Style["\[EmptyDiamond] Non-numeric cells were replaced with median values of the row.",15, Purple, Bold]];
Print["                                           "],

nljudge==="a"&&Union[Map[NumberQ, allmeans]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value.",15, Purple, Bold];Goto["nljudgeagain"],

nljudge==="m"&&Union[Map[NumberQ, allmedians]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value."];Goto["nljudgeagain"],

True,
ErrorMessage["Type in, again."];Goto["nljudgeagain"]
];
{Global`NUMERICDATA,Global`NUMERICITEMS}=nlout;
Label["ncending"];
nlout
];

NGetCorrelation[list_, gcitems_]:=Module[ {explan, trans, target, check, posT, posF,output, newitems, newdata, newtarget, newtrans, flag, corrcoef, errorcheck,errorpos, rpart},

(* list: \:6700\:7d42\:5217\:3092\:76ee\:7684\:5909\:6570\:3001\:305d\:308c\:4ee5\:5916\:3092\:8aac\:660e\:5909\:6570\:3068\:3059\:308b\:884c\:5217 *)
(* \:5404\:8aac\:660e\:5909\:6570\:3068\:76ee\:7684\:5909\:6570\:3068\:306e\:76f8\:95a2\:4fc2\:6570\:3092\:6c42\:3081\:308b *)
(* list\:3068gcitems\:306f\:3001DataIn\:3067\:5f97\:3089\:308c\:305fDATA\:3068ITEMS\:306b\:76f8\:5f53\:3059\:308b *)
(* \:51fa\:529b\:306f\:5b9f\:6570\:5316\:3055\:308c\:308b *)

flag=0;
output={list, gcitems};

If[MatrixQ@list===False, ErrorMessage["The input must be a matrix."];flag=1;Goto["gcending"]];

target=list[[All,-1]];
explan=Map[ CutLast,list];
trans=Transpose@explan;

check=Map[Union,Map[ NumericQ, trans,{2}]];
posT=NPosition[ check, {True}];
posF=Join[NPosition[ check, {False,True}],NPosition[check, {False}]];

If[posF==={},

corrcoef=Map[ Correlation[#, target]&, trans];
output=corrcoef,

Label["nullagain"];
{newdata, newitems}=NullChecker[list, gcitems];
If[{newdata, newitems}==={{},{}}, Goto["gcending"]];
newtarget=newdata[[All,-1]];
newtrans=Transpose@Map[CutLast,newdata];

If[
Union[Map[Union,Map[ NumericQ, newtrans,{2}]]]==={{True}},

corrcoef=Map[ Correlation[#,newtarget]&, newtrans];
output=corrcoef,

ErrorMessage["All the elements must be numeric."];flag=1;Goto["nullagain"]
]
];

errorcheck=Map[NumericQ, corrcoef];
If[ MemberQ[errorcheck, False],

errorpos=NPosition[errorcheck,False];
rpart=Map[ToExpression,MapThread[StringJoin, {Map[ToString,errorpos], Table[ "\[Rule] Null", {Length@errorpos}]}]];
output=N@ReplacePart[corrcoef, rpart],

output=N@corrcoef];

Label["gcending"];
output
];


ShowConfusionMatrix2[pairlist0_, cutoff_:0.5]:=Module[ {sizefont1=16, sizefont2=14, pairlist, listcheck, listpos, listlen, twinlist, kinds, rounded, rounder,elem, inter1, inter2,pospos,posneg,negpos,negneg,cmdata,ppvalue,npvalue,sensvalue,specvalue,accuvalue,fvalue,cvalue,theta0,thRange,testLabels,modelValues,aROCs,ROCcurve,AUCvalue,rocFuncs,rocFuncTips,rocvalues,ROCgraph,triplebag, sizegraph=600},

(* pairlist: a list composed of {predicted value, observed value}  *)

(* ---------------------------- Preparation --------------------------------- *)

PB[];
Print[Style["\[FilledSquare] Classification Performance in Confusion Matrix with ROC Curve", Bold, sizefont1, Purple]];
PB[];

If[ ListQ@pairlist0===False||Dimensions[pairlist0][[2]]=!=2, ErrorMessage["The input data is inappropriate."];Goto["SCMending"]];

listcheck=Map[ NumberQ@#[[1]]&&NumberQ@#[[2]]&, pairlist0]; Global`LISTCHECK=listcheck;

If[MemberQ[listcheck, False],
listpos=NPosition[ listcheck, True];
Print[Style[StringForm["`` elements were omitted from the input due to non-number nature.", Length@pairlist0-Length@listpos],sizefont2]];
pairlist=NPosition[ pairlist0, listpos];listlen=Length@pairlist;
Print[Style[StringForm["The length of data: `` (`` elements were omitted)", listlen, Length@pairlist0-listlen], Bold, sizefont2]],

pairlist=pairlist0; listlen=Length@pairlist;
Print[Style[StringForm["The length of data: ``", listlen], Bold, sizefont2]]
];
Global`PAIRLIST=pairlist;
PB[];

kinds=Union@pairlist[[All,2]];Global`KINDS=kinds;
rounder[elem_]:= Which[
elem<= NMin@kinds, NMin@kinds,
elem>= NMax@kinds, NMax@kinds,
True, Round@elem
];
rounded=If[ Length@kinds===2,
		Map[If[#>= cutoff, 1, 0]&, pairlist[[All,1]]],
		Map[ rounder, pairlist[[All,1]]]];
Global`ROUNDED=rounded;
twinlist=MakeTwin[rounded, pairlist[[All,2]]];
Global`TWINLIST=twinlist;

Which[
Length@kinds===2,

(* -------------------------------------- Confusion Matrix -------------------------------------- *)

pospos=Count[twinlist, {1,1}];
posneg=Count[twinlist, {1,0}];
negpos=Count[twinlist, {0,1}];
negneg=Count[twinlist, {0,0}];

cmdata={{pospos, posneg, pospos+posneg},{negpos, negneg, negpos+negneg},{pospos+negpos, posneg+negneg, pospos+posneg+negpos+negneg}};
Global`CMDATA=cmdata;

ppvalue=100. pospos/(pospos+posneg);
npvalue=100. negneg/(negpos+negneg);
sensvalue=100. pospos/(pospos+negpos);
specvalue=100. negneg/(posneg+negneg);
accuvalue=100. (pospos+negneg)/listlen;
fvalue=2.(ppvalue*sensvalue)/(ppvalue+sensvalue);
cvalue=cStatistics[twinlist][[1]];
Global`FVALUE=fvalue; Global`CVALUE=cvalue;
Global`PPVALUE=ppvalue; Global`SENSVALUE=sensvalue;


(* ------------------------------------------- ROC ---------------------------------------------- *)

theta0=0.01;
thRange=Range[-0.5, 1.5, theta0];
Global`THRANGE=thRange;

testLabels=pairlist[[All,2]];
modelValues=pairlist[[All,1]];
aROCs=Table[ToROCAssociation[{1,0},testLabels,Map[If[#>theta,1,0]&,modelValues]],{theta,thRange}];
ROCcurve=ROCPlot[thRange,aROCs,"PlotJoined"->Automatic,"ROCPointCallouts"->False,"ROCPointTooltips"->True,GridLines->Automatic, ImageSize-> sizegraph];
AUCvalue=NF@N@ROCFunctions["AUROC"][aROCs];

Global`TESTLABELS=testLabels;
Global`MODELVALUES=modelValues;
Global`AROCS=aROCs;
Global`ROCCURVE=ROCcurve;
Global`AUCVALUE=AUCvalue;

rocFuncs={"PPV","NPV","TPR","ACC","SPC","MCC"};rocFuncTips=Map[#<>", "<>(ROCFunctions["FunctionInterpretations"][#])&,rocFuncs];

inter1=Transpose[Map[Through[ROCFunctions[rocFuncs][#]]&,aROCs]];
inter2=rocFuncTips;


rocvalues=MapThread[Tooltip[Transpose[{thRange,#1}],#2]&,{inter1,inter2}]; 
Global`ROCVALUES=rocvalues;
ROCgraph=ListLinePlot[rocvalues,Frame->True,ImageSize -> sizegraph,FrameLabel->Map[Style[#,Larger]&,{"threshold, \[Theta]","rate"}],PlotLegends->rocFuncTips,GridLines->Automatic, PlotRange-> {{0,1},{0,1}}];
Global`ROCFUNCS=rocFuncs;
Global`ROCFUNCTIPS=rocFuncTips;
Global`ROCGRAPH=ROCgraph;

Print[Style["\[FilledSquare] Confusion Matrix", Bold, sizefont2, Blue]];
Print["                                      "];
Print[Style[TableForm[ cmdata, TableHeadings->{{"Predicted as 1","Predicted as 0","Total"},{"Observed as 1","Observed as 0", "Total"}}],Bold,sizefont2]];
PB[];


Print[Style[StringForm["1. Accuracy(\:6b63\:78ba\:5ea6): ``%", NF@accuvalue], Bold, sizefont2, Red]];
Print[Style[StringForm["2. Precision(\:7cbe\:5ea6=\:967d\:6027\:7684\:4e2d\:7387): ``%  NPV(\:9670\:6027\:7684\:4e2d\:7387): ``%", NF@ppvalue, NF@npvalue], Bold, sizefont2, Blue]];
Print[Style[StringForm["3. Recall(\:518d\:73fe\:7387=\:611f\:5ea6): ``%  Specificity(\:7279\:7570\:5ea6): ``%", NF@sensvalue, NF@specvalue], Bold, sizefont2, Blue]];
Print[Style[StringForm["4. F1-measure(F1\:5024): ``  C-statistic(C\:7d71\:8a08\:91cf): ``", NF@fvalue, NF@cvalue], Bold, sizefont2]];
Print[Style[StringForm["5. AUC of ROC curve: ``", AUCvalue], Bold, Red, sizefont2]];

PB[];
Print[Style["\[FilledSquare] Receiver Operating Characteristics Analysis", Bold, sizefont1, Blue]];
PB[];

Print[Style["\[FilledDiamond] ROC curve", Bold, sizefont2]];
Print@ROCcurve;
PB[];
Print[Style[StringForm["\[FilledDiamond] AUC of ROC curve: ``", AUCvalue], Bold, Red, sizefont2]];
PB[];
Print[Style["\[FilledDiamond] Effect of THRESHOLD between positive and negative", Bold, sizefont2]];
Print@ROCgraph;
PB[],




Length@kinds>= 3,

(* -------------------------------------- Confusion Matrix -------------------------------------- *)

pospos=Count[Map[ #[[1]]=== #[[2]]&, twinlist], True];

accuvalue=100. pospos/listlen;

Global`ACCUVALUE=accuvalue;

(* ------------------------------------------- ROC ---------------------------------------------- *)


Print[Style["\[FilledSquare] Confusion Matrix", Bold, sizefont2, Blue]];

PB[];


Print[Style[StringForm["1. Accuracy(\:6b63\:78ba\:5ea6): ``%", NF@accuvalue], Bold, sizefont2, Red]];

PB[] ,

True,
ErrorMessage["Unexpected kinds error."];Goto["SCMending"]

];

triplebag=Fuse[Fuse[pairlist[[All,1]],twinlist[[All,1]]], pairlist[[All,2]]];Global`TRIPLEBAG=triplebag;
ExportExcel[triplebag, Numberings[{"Predicted","Rounded","Observed"}], "ConfusionMatrixData"];
Print[Style[StringForm["\[FilledDiamond] The Raw Dataset for ROC analysis is expressed as TRIPLEBAG\n  with {predicted value, rounded value, observed value}."],14, Bold]];

PB[];


Label["SCMending"];
];

cStatistics[twopairs_]:=Module[ {occured, free,occured0,free0, output=Null, perc,resp, scores, concord, ele,vecd, cstat, accessnumb,pairnumb, n1,n2,n3,n4},

(* twopairs: {{estimated ratio, 1 or 0}, ...} or {{estimated value, actual value}, .....} *)

concord[ele_,vec_]:=Map[ Which[ele[[1]]> #, 1, ele[[1]]===#,0.5,ele[[1]]<#,0,True, 0]&, vec[[All,1]]]; (* ele: \:967d\:6027\:306e\:30b1\:30fc\:30b9, vec: \:9670\:6027\:306e\:30b1\:30fc\:30b9 *)

If[ListQ[twopairs]===False||Length[twopairs]<=1, ErrorMessage["The input is inappropriate."];Goto["csend"]];
If[ Depth[twopairs]=!=3, ErrorMessage["The input has inappropriate depth."];Goto["csend"]];

perc=twopairs[[All,1]]; resp=twopairs[[All,2]];
If[ Union[Map[ MemberQ[{0.`,0,1,1.`,Null},#]&,Union[resp]]]=!={True},
ErrorMessage["The second element should be either 0, 1 or Null."];Goto["csend"]];
If[ Union[Map[ If[NumberQ[#]||#=== Null, True,False,False]&, perc]]=!={True},
ErrorMessage["The first element should be a real number between 0 and 1."];Goto["csend"]];

occured0=FilterElement[ twopairs,2,MemberQ[{1, 1.`}, #]&];
free0=FilterElement[ twopairs, 2,MemberQ[{0, 0.`}, #]&];

occured=NullRemoving[occured0]; (* \:967d\:6027\:306e\:30b1\:30fc\:30b9 *)
free=NullRemoving[free0]; (* \:9670\:6027\:306e\:30b1\:30fc\:30b9 *)

n1=Length[occured0];n2=Length[occured];
If[n1-n2>0, Print[StringForm["`` elements were omitted from the first column due to non-number.", n1-n2]]];
n3=Length[free0];n4=Length[free];
If[n3-n4>0, Print[StringForm["`` elements were omitted from the second column due to non-number.", n1-n2]]];

scores=Map[ concord[#, free]&, occured];
pairnumb=Length[Flatten[scores]];
accessnumb=Total[Flatten[scores]];
cstat=accessnumb/pairnumb;

output={N[cstat],{accessnumb, pairnumb},scores,{occured, free}};

(* {c-statics, concordant+0.5*ties, all pairs, comparison, {with events, without events}} *)

Label["csend"];
output
];


DeterminationCoefficient[obs_, est_]:= Module[{obs1,est1,out={obs,est}, len1, len2,obsmean,estmean, obsdiff, estdiff, obssquare, estsquare, Rsquare},
(* \:6570\:5024\:306e\:30d9\:30af\:30c8\:30eb\:304b\:3089\:6c7a\:5b9a\:4fc2\:6570R^2\:3092\:6c42\:3081\:308b *)
(* \:89b3\:6e2c\:5024\:306e obs \:306b\:5bfe\:3059\:308b \:4e88\:6e2c\:5024 est \:306e\:6c7a\:5b9a\:4fc2\:6570\:3092\:6c42\:3081\:308b *)

If[VectorQ[obs]===False||Depth@obs=!=2||Length@obs<2, Goto["dcending"]];
If[VectorQ[est]===False||Depth@est=!=2||Length@est<2, Goto["dcending"]];

obs1=StringRemoving[obs];
est1=StringRemoving[est];

len1=Length@obs1-Length@obs;
len2 =Length@est1-Length@est;

If[len1>0, ErrorMessage[StringForm["\[EmptyDiamond] `` entries were deleted because of non-numeric cells."]];Goto["dcending"]]; 
If[Length@obs1<2, ErrorMessage["\[EmptyDiamond]\:3000The length of the data is less than 2 after eliminating non-numeric entries."];Goto["dcending"]];

If[len2>0, ErrorMessage[StringForm["\[EmptyDiamond] `` entries were deleted because of non-numeric cells."]];Goto["dcending"]]; 
If[Length@est1<2, ErrorMessage["\[EmptyDiamond]\:3000The length of the data is less than 2 after eliminating non-numeric entries."];Goto["dcending"]];


obsmean=NMean[obs1];
estmean=NMean[est1];
obsdiff=obs1-obsmean;
estdiff=est1-estmean;

obssquare=Total[obsdiff^2];
estsquare=Total[estdiff^2];

Rsquare=N[estsquare/obssquare];

If[ Rsquare>1, out=1/Rsquare;ErrorMessage["The inverse value of the result was returned because the R square shoud be 1 or less."], out=Rsquare];

Label["dcending"];
out
];

DataIn[]:= Module[ {file, output={}, raw, len, i, f, vec, data,item, probe, extent, filetype},

Needs["JLink`"];
ReinstallJava[JVMArguments -> "-Xmx512m"];

file=InputString["Type in the FILE NAME of spread sheet, like data.xlsx.\nOr like /Users/me/data.xls\n\nEach column should be composed of\n{ID, start date, event date, censored date, factor1, ...}.\nlike {1234, {2012,2,1}, Null, {2012,5,6}, male, ...}\n or  {4567, 2012/2/3, 2012/6/7, Null, female, ...}\nType end to quit.   (ID is a dummy column.)", WindowMargins -> {{Automatic, 20},{Automatic,20}}];Global`DIFILE=file;
If[ file ==="end"||file==="quit", Goto["ends"]];

probe=NFindFile[file];Global`DIPROBE=probe;
raw=Import[probe];Global`DIRAW=raw;
len=Length[raw];Global`DILEN=len;

If[raw=!=$Failed, extent=StringTake[probe, -4];
filetype=Switch[extent, ".csv", 0, ".xls", 1, "xlsx", 2, _ , "error"]
];

(* f[vec_]:= If[ Union[ vec] ==={Null}, Null, vec]; *)

Which[probe===$Failed,
ErrorMessage[StringForm["The `` was not found.", file]],

MemberQ[{1, 2}, filetype]&&len===1,
(* data= cleandata2[Rest[raw[[1]] ]]; *)
data=Rest[raw[[1]] ];
Clear[Global`DATA];Global`DATA=data;
item=Numberings[First[raw[[1]]]];
Clear[Global`ITEMS]; Global`ITEMS=item;
output={item, data};
Print[StringForm["The file name: ``   Its dimensions: ``", file, Dimensions[data]]];
Print["The body of data read in is expressed as DATA.\nThe running numbered column name is expressed as ITEMS."],

MemberQ[{1, 2}, filetype]&&len>= 2,
Clear[Global`DATA, Global`ITEMS];
Print[StringForm["The file name: ``", file]];
For[i=1, i<= len, i++,
(* data[i]=cleandata2[Rest[raw[[i]] ]]; *)
data[i]=Rest[raw[[i]] ];
Global`DATA[i]=data[i];
item[i]=Numberings[First[raw[[i]]]];
Global`ITEMS[i]=item[i];
AppendTo[output,{item[i], data[i]}];
Print[StringForm["The dimensions of sheet ``: ``", i, Dimensions[data[i]]]];
];
Print["The bodies of data read in are expressed as DATA[1], DATA[2], ..., for each sheet.\nThe column names are expressed as ITEMS[1], ITEMS[2], ..., for each sheet."],

filetype===0,
raw={raw};
(* data=cleandata2[Rest[raw[[1]] ]]; *)
data=Rest[raw[[1]] ];
Clear[Global`DATA];Global`DATA=data;
item=Numberings[First[raw[[1]]]];
Clear[Global`ITEMS]; Global`ITEMS=item;
output={item, data};
Print[StringForm["The file name: ``   Its dimensions: ``", file, Dimensions[data]]];
Print["The body of data read in is expressed as DATA.\nThe running numbered column name is expressed as ITEMS."],


True,
ErrorMessage["Unexpected DataIn error"]

];
Label["ends"];
output
];

PruneEnsembleVariables[ ensemble_]:= Module[ {head,vari, func, pos,pos2, output={}},
(* Input: an ensemble function in its phenotype created by DataModeler *)
(* Output: a list of variables that are used in the body of the ensemble *)
head=Head[ensemble]; 
If[ head=!=Function, Print[Style["The input must be a Function in Mathematica with variables and a body of the function."]];Goto["ending"]];
vari=ensemble[[1]]; func=ensemble[[2,1]];
pos=Flatten@Position[ Map[ If[ Position[ func, #]==={}, 0, 1,0]&, vari],1];
pos2=Append[pos, Length@vari];
Global`PosVari=pos2;
output=vari[[pos2]];
Label["ending"];
output
];

(* ROC curve *)

Clear[ToROCAssociation];

ToROCAssociation::nalbl="The the first argument is expected to be list of two atomic elements,
or a list of an atomic label and a list of atomic labels.";

ToROCAssociation::nvecs="The the second and third arguments are expected to be vectors of the same length.";

ToROCAssociation::sgntrs="The allowed signatures are one of : \nToROCAssociation[ {trueLabel_?AtomQ, falseLabel:(_?AtomQ|{_?AtomQ..})}, actualLabels_, predictedLabels_ ] , \nToROCAssociation[ {trueLabel_?AtomQ, falseLabel_?AtomQ}, apfAssoc_Association] .";

ToROCAssociation[{trueLabel_,falseLabel_},actualLabels_List,predictedLabels_List]:=Block[{ra,localFalseLabel,flRules},If[!(AtomQ[trueLabel]&&(AtomQ[falseLabel]||MatchQ[falseLabel,{_?AtomQ..}])),Message[ToROCAssociation::nalbl];
Return[$Failed]];
If[!(VectorQ[actualLabels]&&VectorQ[predictedLabels]&&Length[actualLabels]==Length[predictedLabels]),Message[ToROCAssociation::nvecs];
Return[$Failed]];
If[AtomQ[falseLabel],localFalseLabel=falseLabel;
ra=Tally[Transpose[{actualLabels,predictedLabels}]],(*ELSE*)localFalseLabel="Not-"<>ToString[trueLabel];
flRules=Dispatch[Thread[falseLabel->localFalseLabel]];
ra=Tally[Transpose[{actualLabels/.flRules,predictedLabels/.flRules}]]];
ra=Association[Rule@@@ra];
ra=Join[Association@Flatten[Outer[{#1,#2}->0&,{trueLabel,localFalseLabel},{trueLabel,localFalseLabel}]],ra];
ToROCAssociation[{trueLabel,localFalseLabel},ra]];

ToROCAssociation[{trueLabel_?AtomQ,falseLabel_?AtomQ},apfAssoc_Association]:=Block[{},Association[{"TruePositive"->apfAssoc[{trueLabel,trueLabel}],"FalsePositive"->apfAssoc[{falseLabel,trueLabel}],"TrueNegative"->apfAssoc[{falseLabel,falseLabel}],"FalseNegative"->apfAssoc[{trueLabel,falseLabel}]}]];

ToROCAssociation[___]:=(Message[ToROCAssociation::sgntrs];$Failed);

Clear[ROCAssociationQ]
ROCAssociationQ[obj_]:=AssociationQ[obj]&&Length[Intersection[Keys[obj],{"TruePositive","FalsePositive","TrueNegative","FalseNegative"}]]==4;

TPR[rocAssoc_?ROCAssociationQ]:=(rocAssoc["TruePositive"])/(rocAssoc["TruePositive"]+rocAssoc["FalseNegative"]);
TPR[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[TPR,rocs];

SPC[rocAssoc_?ROCAssociationQ]:=(rocAssoc["TrueNegative"])/(rocAssoc["FalsePositive"]+rocAssoc["TrueNegative"]);
SPC[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[SPC,rocs];

PPV[rocAssoc_?ROCAssociationQ]:=(rocAssoc["TruePositive"])/(rocAssoc["TruePositive"]+rocAssoc["FalsePositive"]);
PPV[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[PPV,rocs];

NPV[rocAssoc_?ROCAssociationQ]:=(rocAssoc["TrueNegative"])/(rocAssoc["TrueNegative"]+rocAssoc["FalseNegative"]);
NPV[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[NPV,rocs];

FPR[rocAssoc_?ROCAssociationQ]:=(rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"]+rocAssoc["TrueNegative"]);
FPR[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[FPR,rocs];

FDR[rocAssoc_?ROCAssociationQ]:=(rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"]+rocAssoc["TruePositive"]);
FDR[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[FDR,rocs];

FNR[rocAssoc_?ROCAssociationQ]:=(rocAssoc["FalseNegative"])/(rocAssoc["FalseNegative"]+rocAssoc["TruePositive"]);
FNR[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[FNR,rocs];

ACC[rocAssoc_?ROCAssociationQ]:=(rocAssoc["TruePositive"]+rocAssoc["TrueNegative"])/Total[Values[rocAssoc]];
ACC[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[ACC,rocs];

FOR[rocAssoc_?ROCAssociationQ]:=1-NPV[rocAssoc];
FOR[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[FOR,rocs];

F1[rocAssoc_?ROCAssociationQ]:=2*PPV[rocAssoc]*TPR[rocAssoc]/(PPV[rocAssoc]+TPR[rocAssoc]);
F1[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[F1,rocs];

(*Note the addition of the points {0,0} and {1,1}.If rps=Transpose[{ROCFunctions["FPR"]/@pROCs,ROCFunctions["TPR"]/@pROCs}]] has points {0,p0} and at {1,p1} then after applying Sort and Partition[#,2,1]& we will get 0-length intervals and correctly ordered pairs,i.e.{{{0,0},{0,p0}},{{0,p0},_},___,{_,{1,p1}},{{1,p1},{1,1}}}.Hence the trapezoidal formula integration is going to work correctly.*)
AUROC[pROCs:{_?ROCAssociationQ..}]:=Total[Partition[Sort@Join[{{0,0},{1,1}},Transpose[{ROCFunctions["FPR"]/@pROCs,ROCFunctions["TPR"]/@pROCs}]],2,1]/.{{x1_,y1_},{x2_,y2_}}:>(x2-x1) (y1+(y2-y1)/2)];
AUROC[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[AUROC,rocs];

MCC[rocAssoc_?ROCAssociationQ]:=Block[{tp,tn,fp,fn,tpfp,tpfn,tnfp,tnfn},{tp,tn,fp,fn}=Through[{TPR,SPC,FPR,FNR}[rocAssoc]];
{tpfp,tpfn,tnfp,tnfn}=Map[If[#==0,1,#]&,{tp+fp,tp+fn,tn+fp,tn+fn}];
(tp*tn-fp*fn)/Sqrt[tpfp*tpfn*tnfp*tnfn]];
MCC[rocs:({_?ROCAssociationQ..}|<|_?ROCAssociationQ..|>)]:=Map[MCC,rocs];


aROCAcronyms=AssociationThread[{"TPR","TNR","SPC","PPV","NPV","FPR","FDR","FNR","ACC","AUROC","FOR","F1","MCC","Recall","Precision","Accuracy","Sensitivity"},{"true positive rate","true negative rate","specificity","positive predictive value","negative predictive value","false positive rate","false discovery rate","false negative rate","accuracy","area under the ROC curve","false omission rate","F1 score","Matthews correlation coefficient","same as TPR","same as PPV","same as ACC","same as TPR"}];

aROCFunctions=Join[AssociationThread[{"TPR","TNR","SPC","PPV","NPV","FPR","FDR","FNR","ACC","AUROC","FOR","F1","MCC"},{TPR,SPC,SPC,PPV,NPV,FPR,FDR,FNR,ACC,AUROC,FOR,F1,MCC}],AssociationThread[{"Recall","Sensitivity","Precision","Accuracy","Specificity","FalsePositiveRate","TruePositiveRate","FalseNegativeRate","TrueNegativeRate","FalseDiscoveryRate","FalseOmissionRate","F1Score","AreaUnderROCCurve","MatthewsCorrelationCoefficient"},{TPR,TPR,PPV,ACC,SPC,FPR,TPR,FNR,SPC,FDR,FOR,F1,AUROC,MCC}]];


Clear[ROCFunctions]
ROCFunctions["Methods"]:={"FunctionInterpretations","FunctionNames","Functions","Methods","Properties"};
ROCFunctions["Properties"]:=ROCFunctions["Methods"];
ROCFunctions["FunctionNames"]:=Keys[aROCAcronyms];
ROCFunctions["FunctionInterpretations"]:=aROCAcronyms;
ROCFunctions["FunctionsAssociation"]:=aROCFunctions;
ROCFunctions["Functions"]:=Union[Values[aROCFunctions]];
ROCFunctions[]:=Evaluate[ROCFunctions["FunctionsAssociation"]];
ROCFunctions[fnames:{_String..}]:=aROCFunctions/@fnames;
ROCFunctions[fname_String]:=aROCFunctions[fname];

Clear[ROCPlot];

ROCPlot::apv="The parameter values are specified as Automatic, but extracting \"ROCParameter\" from the ROC data did not produce a numerical vector.";

Options[ROCPlot]=Join[{"ROCPointSize"->0.02,"ROCColor"->Lighter[Blue],"ROCPointColorFunction"->Automatic,"ROCPointTooltips"->True,"ROCPointCallouts"->True,"ROCCurveColorFunction"->Automatic,"PlotJoined"->True},Options[Graphics]];

ROCSpecQ[arg_]:=MatchQ[arg,{_?ROCAssociationQ..}|{{_?ROCAssociationQ..}..}|Association[(_->{_?ROCAssociationQ..})..]];

ROCPlot[aROCs_?ROCSpecQ,opts:OptionsPattern[]]:=ROCPlot["FPR","TPR",Automatic,aROCs,opts];

ROCPlot[parVals_List,aROCs_?ROCSpecQ,opts:OptionsPattern[]]:=ROCPlot["FPR","TPR",parVals,aROCs,opts];

ROCPlot[xFuncName_String,yFuncName_String,aROCs_?ROCSpecQ,opts:OptionsPattern[]]:=ROCPlot[xFuncName,yFuncName,Automatic,aROCs,opts];

ROCPlot[xFuncName_String,yFuncName_String,parValsArg:(Automatic|{_?NumericQ..}|_List),aROCs:{{_?ROCAssociationQ..}..},opts:OptionsPattern[]]:=ROCPlot[xFuncName,yFuncName,parValsArg,AssociationThread[Range[Length[aROCs]],aROCs],opts];

ROCPlot[xFuncName_String,yFuncName_String,parValsArg:(Automatic|{_?NumericQ..}|_List),aROCs:Association[(_->{_?ROCAssociationQ..})..],opts:OptionsPattern[]]:=Block[{rocCurveColorFunc,cls,grs},rocCurveColorFunc=OptionValue[ROCPlot,"ROCCurveColorFunction"];
If[TrueQ[rocCurveColorFunc===Automatic],rocCurveColorFunc=ColorData["DarkBands","ColorFunction"];];
cls=rocCurveColorFunc/@Rescale[Range[Length[aROCs]]];
grs=MapThread[ROCPlot[xFuncName,yFuncName,#2,opts,"PlotJoined"->True,"ROCColor"->#3]&,{Keys[aROCs],Values[aROCs],cls}];
Legended[Show[grs],SwatchLegend[cls,Keys[aROCs]]]];

ROCPlot[xFuncName_String,yFuncName_String,parValsArg:(Automatic|{_?NumericQ..}|_List),aROCs:{_?ROCAssociationQ..},opts:OptionsPattern[]]:=Block[{xFunc,yFunc,psize,rocc,pt,pc,pj,rocpcf,points,parVals=parValsArg,pred},psize=OptionValue["ROCPointSize"];
rocc=OptionValue["ROCColor"];
rocpcf=OptionValue["ROCPointColorFunction"];
{pt,pc,pj}=TrueQ[OptionValue[#]]&/@{"ROCPointTooltips","ROCPointCallouts","PlotJoined"};
pj=pj||!pj&&TrueQ[OptionValue["PlotJoined"]===Automatic];
{xFunc,yFunc}=ROCFunctions[{xFuncName,yFuncName}];
points=Map[Through[{xFunc,yFunc}[#1]]&,aROCs];
If[TrueQ[parVals===Automatic],parVals=Map[#["ROCParameter"]&,aROCs]];
(*If[!VectorQ[parVals,NumericQ],Message[ROCPlot::apv];
Return[$Failed]];*)pred=Map[VectorQ[#,NumericQ]&,points];
points=Pick[points,pred];
parVals=Pick[parVals,pred];
Graphics[{If[pj,{Lighter[rocc],Line[points]},{}],PointSize[psize],rocc,If[pj,Line[points]],If[TrueQ[rocpcf===Automatic]||pj,Which[pt,MapThread[Tooltip[Point[#1],#2]&,{points,parVals}],!pt,Point[points],True,Nothing],(*ELSE*)Which[pt,MapThread[{rocpcf[#1,#2,#3],Tooltip[Point[#1],#2]}&,{points,parVals,Range[Length[points]]}],True,MapThread[{rocpcf[#1,#2,#3],Point[#]}&,{points,parVals,Range[Length[points]]}]]],Black,If[pc,MapThread[Text[#2,#1,{-1,2}]&,{points,parVals}],{}]},AspectRatio->1,Frame->True,FrameLabel->Map[Style[StringRiffle[{#,Lookup[ROCFunctions["FunctionInterpretations"],#,Nothing]},", "],Larger,Bold]&,{xFuncName,yFuncName}],DeleteCases[{opts},("ROCPointSize"|"ROCColor"|"ROCPointColorFunction"|"ROCPointTooltips"|"ROCPointCallouts"|"ROCCurveColorFunction"|"PlotJoined")->_]]]/;Length[parValsArg]==Length[aROCs]||TrueQ[parValsArg===Automatic];


Clear[ROCValues];

ROCValues::nrng="The range argument is expected to be a list of numbers between 0 and 1.";

ROCValues::nlen="The prediction probabilities Dataset object and the actual labels (the first and second arguments) are expected to have equal lengths.";

ROCValues::nlbl="The value of \"ClassLabel\" is expected to be one of the columns of the first argument.";

ROCValues::args="The arguments are expected to be a predictions probabilities Dataset, a list of actual labels, and threshold range.";

Options[ROCValues]={"ClassLabel"->Automatic};

ROCValues[clRes_Dataset,testLabels_List,opts:OptionsPattern[]]:=ROCValues[clRes,testLabels,Range[0,1,0.05],opts];

ROCValues[predictionProbabilities_Dataset,actualLabels_List,thRange_?VectorQ,opts:OptionsPattern[]]:=Block[{focusClassLabel,classLabels,predictedLabels,rocRes,mainLabel,notMainLabel,modifiedActualLabels},If[Length[predictionProbabilities]!=Length[actualLabels],Message[ROCValues::nlen];
$Failed];
If[!(VectorQ[thRange,NumberQ]&&Apply[And,Map[1>=#>=0&,thRange]]),Message[ROCValues::nrng];
$Failed];
focusClassLabel=OptionValue[ROCValues,"ClassLabel"];
If[TrueQ[focusClassLabel===Automatic],focusClassLabel=First@Normal@Keys[predictionProbabilities[1]]];
If[!MemberQ[Normal@Keys[predictionProbabilities[1]],focusClassLabel],Message[ROCValues::nlbl];
$Failed];
mainLabel=ToString[focusClassLabel];
notMainLabel="Not-"<>mainLabel;
modifiedActualLabels=If[#==mainLabel,#,notMainLabel]&/@actualLabels;
(*This is no longer actual:classLabels=Normal[Keys[predictionProbabilities[1]]];*)classLabels={mainLabel,notMainLabel};
Table[predictedLabels=Normal@predictionProbabilities[All,If[#[[1]]>=th,mainLabel,notMainLabel]&];
rocRes=ToROCAssociation[classLabels,modifiedActualLabels,predictedLabels];
If[AssociationQ[rocRes],Join[<|"ROCParameter"->th|>,rocRes],$Failed],{th,thRange}]];

ROCValues[___]:=Block[{},Message[ROCValues::args];
$Failed];

Clear[ToClassifyROCCurvePlot];
ToClassifyROCCurvePlot[gr_]:=Block[{cols,pFunc},pFunc[x_,{cedge_RGBColor,cface_RGBColor}]:={EdgeForm[cedge],FaceForm[{cface,Opacity[0.34]}],Polygon[x]};
cols=Cases[gr,_RGBColor,Infinity];
gr/.{Line[x__]->pFunc[x,{Darker[Blue],LightBlue}],PointSize[x_]->PointSize[0.001]}];

(*Modified/productized version of kglr's MSE answer:https://mathematica.stackexchange.com/a/200221/34008.*)

Clear[ConfusionMatrixPlot];

Options[ConfusionMatrixPlot]=Join[{"Normalize"->False},Options[MatrixPlot]];

ConfusionMatrixPlot[aROC_?ROCAssociationQ,labelNames:{yesLabel_,noLabel_}:{"True","False"},opts:OptionsPattern[]]:=Block[{mat,refMat},mat={{aROC["FalseNegative"],aROC["TruePositive"]},{aROC["TrueNegative"],aROC["FalsePositive"]}};
refMat=mat;
If[TrueQ[OptionValue[ConfusionMatrixPlot,"Normalize"]],mat=N[mat/{aROC["TruePositive"]+aROC["FalseNegative"],aROC["TrueNegative"]+aROC["FalsePositive"]}];];
ConfusionMatrixPlotFrame[mat,refMat,labelNames,labelNames,opts]];

Clear[ConfusionMatrixPlotFrame];

Options[ConfusionMatrixPlotFrame]=Options[MatrixPlot];

ConfusionMatrixPlotFrame[mat_?MatrixQ,rowNames_List,columnNames_List,opts:OptionsPattern[]]:=ConfusionMatrixPlotFrame[mat,mat,rowNames,columnNames,opts];

ConfusionMatrixPlotFrame[mat_?MatrixQ,refMat_?MatrixQ,rowNames_List,columnNames_List,opts:OptionsPattern[]]:=Block[{},MatrixPlot[mat,FilterRules[{opts},Options[MatrixPlot]],ColorRules->{0->White},Frame->True,FrameLabel->{"actual","predicted"},FrameTicks->{{MapIndexed[{#2[[1]],#}&,rowNames],MapIndexed[{#2[[1]],#}&,Total@Transpose@refMat]},{MapIndexed[{#2[[1]],#}&,Total[refMat]],MapIndexed[{#2[[1]],#}&,columnNames]}},(*ColorFunction\[Rule]"Rainbow",*)Epilog->MapIndexed[Text[#,#2-1/2]&,Transpose@Reverse@mat,{2}]]];

ConfusionMatrixPlotFrame[smat_?AssociationQ,opts:OptionsPattern[]]:=Block[{},ConfusionMatrixPlotFrame[Sequence@@Values[smat],opts]]/;Sort[Keys[smat]]==Sort[{"SparseMatrix","RowNames","ColumnNames"}];



End[];    
EndPackage[];     
