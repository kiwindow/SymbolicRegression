(* ::Package:: *)

(* ::Input::Initialization:: *)
BuildModel[]:=Module[{output="Process Terminated",phasenumber,datain, file, phaser, phanum, flag, indata0, darkblue,indata,tag1,initems,
path, readin, sheet, allNames, allTrainingData, allnamesimage, response0, response, windowaddress1, windowaddress2, windowaddress3,
predictor, criterion, leng,numb, initemsimage, predictorNames, criterionName, alldataimage, completeimage, univariateimage,
bivariateimage,distributionimage, correlationmatrix, correlationchart, size1, size2,summaryimage, projectname,projectname0,
timeconstraint0, timeconstraint, evolutions0, evolutions, judge1, start,  paretofront, developedModels, totaltime,
quality0, quality, interestingModels, interestimage, linearModels, freeformModels, linearimage, freeformimage, fontsize,
dimensionimage, combinationiamge, archivedimage, archivedModels, judge, presenceimage,metavariableimage, anEnsemble, 
ensembleimage, ensembleinpareto,performanceimage,outlierIndices,comparisonimage,nicheimage, phenotype, createdModel,
estimatedModel, estimatedValues, observedValues, comparedimage, pairs, pairs2,j,startingtime, finishingtime,realtime,
xaxes, poarrows,midline,judgement,lesspairs,morepairs,bprights,bpwrongs,rightrate, LL,MM,LM,ML,rwdata,
realchecker, pairvec, pairs1, nullchecker1, bmlist0, bmitems0,newdim, nullcheckout, joineddata,joineditems, joinedname,
modelcount0,modelcount, modelcountD, modelcountA, imodelcount,fontsize2,judgebackG, traditionalForm, eachfunction, wholefunction,
cstat, cstatres, nullchecker2, nullflag, alltrainingsIP, allnamesIP, starttime, endtime, expectedduration,consumedtime,
NFindFile2, filename,valimeth0,valimeth, selectpara0, selectpara, splitratio, crossnumb0, crossnumb, testsize, trainsize, trainpos, hotraindata, hotestdata,foldsize, foldbag, ii, randpos, randorder, foldnumb, samplesize, blockbag, testpos,
rsquare, PredictedTestValues, ObservedTestValues, TrainData, TestData, TestPairs, allTestIP, estimatedTestValues, observedTestValues, testpairs, testpairs1, testpairs2, testcstatres, testcstat, testxaxes, testpoarrows, testmidline, testjudgement, testcomparedimage, lesstestpairs, moretestpairs, testLL, testMM, testLM, testML, testbprights, testbpwrongs, testrightrate, testrwdata, testrsquare, estimatedTrainValues, trainrsquare, randcage, jj, observedTrainValues, trainpairs,  trainpairs1, trainpairs2, 
traincstatres, traincstat, trainxaxes,trainpoarrows, trainmidline, trainjudgement, traincomparedimage, lesstrainpairs, moretrainpairs, trainLL, trainMM, trainLM, trainML, trainbprights, trainbpwrongs, trainrightrate, trainrwdata,triplebag,triplebagOM,
seed, seednumber0, seednumber, trainingcases, testcases ,loolen, i, partTrainingData, partTestData, fulltime, paretofront2,archivedbag,traindatabag, testdatabag, modelbag,paretobag,interestingbag,imagebag, countbag, presencebag, ensemblebag,
createdbag,allTrainingIP, allNamesIP, partTrainingIP, partTestIP, partNamesIP, estimatedValue,observedValue,pair,
roundedValue,judged,looresult,criteria, traditionalbag, allbag, partbag, pairbag, pairbag2, pair2, pospos,
posneg, negpos, negneg, ppvalue,npvalue, sensvalue, specvalue, accuvalue, fvalue, cvalue, testLabels, modelValues,
theta, thRange, aROCs, AUCvalue,theta0, ROCcurve, rocFuncs, rocFuncTips, ROCgraph,numbers, model,  opts,CreateStandaloneModel,
minmax, monitors, limits0, limits, minP, maxP, Anumb, Pnumb, firstModels, firstcount, Smodels, Cnumb, Lnumb,
deltaC, deltaL,finalquality,usedvariables, variablebox, topfive,vect,bestbag,bestfive,
presencefinalimage,presencefinalimage15, presencefinalimage10, presencefinalimage5, presencefinal,
finaltopfive, minnumb, wrongcases, rightwrong, scores,summary, labeling, now0,future0,future4,future8,AUCvalue2,rocvalues,allinterestingModels,allEnsemble,allensembleimage,allensembleinpareto,alloutlierIndices,allperformanceimage,allcomparisonimage,allnicheimage,allphenotype,allcreatedModel,allestimatedModel,alltraditionalForm,allestimatedValues,allobservedValues,allpairs,allpairs1,allpairs2,allcstatres,allcstat,alljudgement,allcomparedimage,allrwdata,allrsquare,alljoineddata,alljoineditems,alljoinedname,precision, recall,allFvalue,allvariables, Nof1, mypercent, Nof0, subsetsize, alltestLabels, allmodelValues, allaROCs, allROCcurve, allAUCvalue, allrocFuncs, allrocFuncTips,allrocvalues,allROCgraph,
allroundedValues, allpairs3, allresult, allrightwrong, allscores, allsummary, cmdata, rightcases,paretofrontlinear,paretofrontfree,
correlationchartloo0,correlationchartloo, looscores, looscore, lootext, lootexts,loograph, version, error, errorbag,errorminmax,
errortext,errorgraph, evolutionStrategyLOO, robustModelLOO, selectionStrategyLOO, subsetsizefunc, numRec,
evolutionStrategyHO, robustModelHO, selectionStrategyHO, benchmark, corecount, future, findstartpoint, rawmodels,startgradient,
darkgreen, fitindex0, fitindex, inquiry, quartetbag, totalbag, totalnames, remainingtime, timelag, lm, x, rsquared, labels,
alllm, allrsquared,qualityflag,paretofront1, nsjudge0,nsjudge,  liststandout,listnormout, allTrainingData0, allNames0,
explanatdata, targetdata, graphS, graphN,presencefinalimage20, graphO,allgraph, partgraph, allmodels, partmodels,
allarchivedModels, allprojectname, partprojectname, importantvariables, dominantvariables, top20variables, top15variables, top10variables, top5variables,
trainlines, testlines, traintestpos, usefulvariables, coeffvalues, coefftable, numbcoefftable, minpercent0, minpercent, heads, unionh,
cutoff0, cutoff ,classes
},

(* BuildModel version 3.0.1  2021.12.24  Monitor off *)
version="BuildModel version 3.0.1 made on December 24th, 2021";

(* Set the currrent directory where the notebook you are running BuildModel[] on is saved as the working directory *)
SetDirectory[NotebookDirectory[]];

corecount=$ProcessorCount;
Global`CORECOUNT=corecount;

(* Hypterparameter Settings for LOO method *)
evolutionStrategyLOO=BalancedGP;
robustModelLOO=True;
selectionStrategyLOO=ParetoFrontSelect;
(* startgradient=0.85; *)

(* Hypterparameter Settings for Hold-out method *)
evolutionStrategyHO=BalancedGP;
robustModelHO=True;
selectionStrategyHO=ParetoFrontSelect;

(* Parameter Setting *)

deltaC=1; deltaL=0.01;minnumb=100;
startingtime=Now[[1]];Global`STARTINGTIME=startingtime;

darkblue=RGBColor[0, 0.0376287, 0.760174];
darkgreen=RGBColor[0, 0.676539, 0];
windowaddress1={{Automatic, 10},{Automatic, 10}};
windowaddress2={{Automatic, 250},{Automatic , 350}};
windowaddress3={{Automatic, 150},{Automatic, 10}};
size1=650; size2=900;fontsize=16;fontsize2=20;

(* SubSetSizefunction *)

subsetsizefunc[numRec_]:=With[{sizeCand=33 + 8215/(5.3+numRec)-2.3N@Log[numRec]},If[numRec<100, 100, sizeCand]];

(* NFindFile2 *)

NFindFile2[filename_]:= Module[{judge0, judge11,judge2, judge3,out, choice,readables, existQ, existN, existP, alljudges, exist1,exist2,exist3, exist4},
judge0=FindFile[ToString[filename]];
judge11=FindFile[StringJoin[ ToString[filename], ".xls"]];
judge2=FindFile[StringJoin[ToString[filename],".xlsx" ]];
judge3=FindFile[StringJoin[ToString[filename],".csv" ]];

alljudges={judge0,judge11,judge2,judge3};

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
choice= InputString[StringForm["Two files were found:\n`` and ``\n Type 1 for ``, Type 2 for ``\n\nPress OK to select the FIRST file ``\nType end to quit.",
					exist1,exist2, exist1, exist2, exist1], WindowMargins-> {{Automatic, 10},{Automatic,10}}];
Which[choice==="1"||choice==="",out= exist1, choice==="2", out=exist2,choice==="end"||choice==="quit", Goto["endingbuildmodel"],
 True,  ErrorMessage["error"]; Goto["choiceagain"]],

existN===3,
exist1=alljudges[[existP[[1]]]]; exist2= alljudges[[existP[[2]]]]; exist3=alljudges[[existP[[3]]]];
choice= InputString[StringForm["Three files were found:\n``, `` and ``\n Type 1 for ``, Type 2 for ``, Type 3 for ``\n\nPress OK to select the FIRST file ``\nType end to quit.",
					exist1, exist2, exist3, exist1, exist2, exist3, exist1  ]];
Which[choice==="1"||choice==="",out= exist1, choice==="2", out=exist2,choice==="3", out=exist3, choice==="end"||choice==="quit", Goto["endingbuildmodel"],
True,  ErrorMessage["error"]; Goto["choiceagain"]],

existN===4,
exist1=alljudges[[existP[[1]]]]; exist2= alljudges[[existP[[2]]]]; exist3=alljudges[[existP[[3]]]];exist4=alljudges[[existP[[4]]]];
choice= InputString[StringForm["Four files were found:\n``, ``, `` and ``\n Type 1 for ``, Type 2 for ``, Type 3 for ``, Type 4 for ``\n\nPress OK to select the FIRST file ``\nType end to quit.",
					exist1, exist2, exist3,exist1, exist1, exist2, exist3,exist4, exist1  ]];
Which[choice==="1"||choice==="",out= exist1, choice==="2", out=exist2,choice==="3", out=exist3, choice==="4", out=exist4,choice==="end"||choice==="quit", Goto["endingbuildmodel"],
True,  ErrorMessage["error"]; Goto["choiceagain"]],

True, ErrorMessage["Unexpected choice error"]; Goto["choiceagain"]
];

out
];

(* NFindFile3 *)

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


(* Create Standalone Model *)
CreateStandaloneModel[model:(_GPModel | _ModelEnsemble), opts___?OptionQ] := Module[
{inputVars, expression},
inputVars = DataVariables /. {opts} /. ModelPersonality@model;
expression = ModelPhenotype[model, opts];
Function@@{inputVars, expression}
];

(* DataIn function *)
datain[file_]:= Module[ {paths, out1={}, raw, len, i0, f, vec, data,item, csvQ},

paths=NFindFile2[file];raw=Import[paths];
len=Length[raw];csvQ=If[paths=!=$Failed&&StringTake[paths,-3]==="csv", 1, 0, 0];CSVQ=csvQ;
f[vec_]:= If[ Union[ vec] ==={Null}, Null, vec];

Which[raw===$Failed,
ErrorMessage[StringForm["The `` was not found.", file]];
out1="nofilefound"; Goto["ends"],

csvQ===1,
data=IntactComplement[ Map[ f,cleandata[Rest[raw ]]], {Null}];
Clear[Global`DATA];Global`DATA=data;
item=Numberings[First[raw]];
Clear[Global`ITEMS]; Global`ITEMS=item;
out1={1,{item, data}},

len===1,
data=IntactComplement[ Map[ f,cleandata[Rest[raw[[1]] ]]], {Null}];
Clear[Global`DATA];Global`DATA=data;
item=Numberings[First[raw[[1]]]];
Clear[Global`ITEMS]; Global`ITEMS=item;
out1={1,{item, data}},

len>= 2,
Clear[Global`DATA, Global`ITEMS];
For[i0=1, i0<= len, i0++,
data[i0]=Complement[ Map[ f,cleandata[Rest[raw[[i0]] ]]], {Null}];
Global`DATA[i0]=data[i0];
item[i0]=Numberings[First[raw[[i0]]]];
Global`ITEMS[i0]=item[i0];
AppendTo[out1,{item[i0], data[i0]}]
]; out1={len, out1},

True,
ErrorMessage["Unexpected DataIn error"]; out1="nofilefound"; Goto["ends"]

];
Label["ends"];
out1
];


(* Real Cheker *)
realchecker[pairvec_]:= Module[ {checknumb, vectors, rscore, rpos},
checknumb[vectors_]:= If[ NumberQ[vectors[[1]]]&&NumberQ[vectors[[2]]], 1, 0];
rscore=Map[ checknumb, pairvec];
rpos=NPosition[ rscore, 1];
Part[ pairvec, rpos]
];




(* Null Checker *)
nullchecker1[bmlist0_, bmitems0_]:=Module[ {nclen, ji, nbag, nline, nlout, nljudge, allmeans, address1, func, bpos, bmean, rules1, realpos, allmedians, address2, rules2, ncleng, mbag, mline,realrow, emptyrow, emptyitems, bmlist, bmitems, nullcounts, allcounts, nullpercent},

nbag={};mbag={};
func[bpos_,bmean_]:={bpos-> bmean[[bpos[[2]]]]};
Global`BMLIST0=bmlist0;Global`BMITEMS0=bmitems0;

ncleng=Length[Transpose[bmlist0]];

(* \:7a7a\:306e\:5217\:306e\:51e6\:7406 *)
For[ji=1, ji<= ncleng, ji++,
mline=Transpose[bmlist0][[ji]];
AppendTo[mbag, If[Union[Map[NumberQ, mline]]==={False}, 1, 0, 0]]
];

If[MemberQ[mbag, 1]===False, bmlist=bmlist0; bmitems=bmitems0;Goto["nextphase"]];

realrow=NPosition[mbag, 0]; emptyrow=NPosition[mbag,1];
bmlist=Transpose[Part[Transpose[bmlist0], realrow]];Global`INTERLIST=bmlist;
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
];Global`NBAG=nbag;

If[MemberQ[nbag, 1]===False,  Goto["ncending"]];
Global`NBAG=nbag;Global`BMLIST=bmlist;   

nullcounts= Count[Map[ NumberQ,Flatten@bmlist],False];Global`NULLCOUNTS=nullcounts;
allcounts=Length@Flatten@bmlist;Global`ALLCOUNTS=allcounts;
nullpercent=NF[100.nullcounts/allcounts];Global`NULLPERCENT=nullpercent;

Label["nljudgeagain"];
nljudge=InputString[StringForm["``% of cells in the dataset are empty.\nPress OK to replace empty cell with median value of the row.\n\nType a to replace empty cell with average value of the row.\nType n to leave empty cells as they are.\nType x to excluede those columns.\nType end to quit.", nullpercent], WindowMargins->{{Automatic,10}, {Automatic, 10}}];

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



(* non-numeric cells \:3092\:6570\:5b57\:3067\:7f6e\:304d\:63db\:3048\:308b *)
nullchecker2[bmlist0_, bmitems0_]:=Module[ {nclen, ji, nbag, nline, nlout, nljudge, allmeans, address1, func, bpos, bmean, rules1, realpos, allmedians, address2, rules2, ncleng, mbag, mline,realrow, emptyrow, emptyitems, bmlist, bmitems, nullcounts, allcounts, nullpercent},

nbag={};mbag={};
func[bpos_,bmean_]:={bpos-> bmean[[bpos[[2]]]]};
Global`BMLIST0=bmlist0;Global`BMITEMS0=bmitems0;

ncleng=Length[Transpose[bmlist0]];

(* \:7a7a\:306e\:5217\:306e\:51e6\:7406 *)
For[ji=1, ji<= ncleng, ji++,
mline=Transpose[bmlist0][[ji]];
AppendTo[mbag, If[Union[Map[NumberQ, mline]]==={False}, 1, 0, 0]]
];

If[MemberQ[mbag, 1]===False, bmlist=bmlist0; bmitems=bmitems0;Goto["nextphase"]];

realrow=NPosition[mbag, 0]; emptyrow=NPosition[mbag,1];
bmlist=Transpose[Part[Transpose[bmlist0], realrow]];Global`INTERLIST=bmlist;
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
];Global`NBAG=nbag;

If[MemberQ[nbag, 1]===False,  Goto["ncending"]];Global`NBAG=nbag;
Global`BMLIST=bmlist;

nullcounts=Count[ Map[NumberQ,Flatten@bmlist], False];Global`NULLCOUNTS=nullcounts;
allcounts=Length@Flatten@bmlist;Global`ALLCOUNTS=allcounts;
nullpercent=NF[100.nullcounts/allcounts];Global`NULLPERCENT=nullpercent;

Label["nljudgeagain"];
nljudge=InputString[StringForm["``% of cells in the dataset are empty.\nTo test the accuracy of the created model on the training data, non-numeric cells must be replaced with a number.\n\nPress OK to replace empty cell with median value of the row.\nType a to replace empty cell with average value of the row.\nType x to excluede those columns.\nType end to quit.", nullpercent], WindowMargins->{{Automatic,10}, {Automatic, 10}}];

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

nljudge==="a"&&Union[Map[NumberQ, allmeans]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value.",15, Purple, Bold];Goto["nljudgeagain"],

nljudge==="m"&&Union[Map[NumberQ, allmedians]]=!={True},
ErrorMessage["Type in again, because some rows have no numeric value."];Goto["nljudgeagain"],

True,
ErrorMessage["Type in, again."];Goto["nljudgeagain"]
];

Label["ncending"];
nlout
];




phaser[phanum_]:= Module[ {phas, phaseboard, conts},

phaseboard=CreateWindow[WindowSize -> {500, 180}, WindowMargins -> windowaddress2, WindowTitle -> "Phase names"];

conts=Cell["Phase 1  Patient data\nPhase 2  Group selection\nPhase 3  Component selection\nPhase 4  Parameter setting\nPhase 5  Data loading\nPhase 6  Data analysis & plotting", FontSize -> 20];
NotebookWrite[phaseboard,conts];

Label["phasagain"];
phas=InputString[StringForm["Type in a phase NUMBER to return\nfrom `` to ``\nType end to quit", 1, phanum], WindowMargins-> windowaddress1];

NotebookClose[phaseboard];

Which[ phas==="end"||phas==="quit", flag="terminated";Goto["endinglabodata"],
IntegerQ[ToExpression[phas]]&& 1 <=ToExpression[phas]<= phanum, Goto[StringJoin["phase", phas]],
True, ErrorMessage["Type in, again"];Goto["phasagain"]
];

];


topfive[vect_]:=Which[ListQ[vect]===False, {{Null,Null}}, Length@vect<5, vect[[All,2]],Length@vect>= 5, Take[vect,5][[All,2]], True, {{Null,Null}}];



(* a function to find out the starting values of QualityBox *)
findstartpoint[rawmodels_]:=Module[{QB0, QB1, QB2,medians, ratio, medians2,  startpoint,allgrads,nears, lenpos, lencands, lenlens, startpoint0},
QB0=Map[ModelQuality, rawmodels];
QB1=Map[{#[[1]],100#[[2]]}&, QB0];
medians=Map[NMedian, Transpose@QB1];
ratio=medians[[2]]/medians[[1]];
QB2=Map[ {ratio*#[[1]],#[[2]]}&, QB1];
(* medians2=Map[NMedian, Transpose@QB2]; *)
allgrads=Map[ #[[2]]/#[[1]]&, QB2];
nears=Nearest[ allgrads, fitindex, Round[0.2Length@rawmodels]];
lenpos=Flatten@Map[NPosition[allgrads,#]&, nears];
lencands=Part[QB2, lenpos];
lenlens=Map[Norm, lencands];
startpoint0=lencands[[Last@NPosition[lenlens, NMin@lenlens]]];
startpoint={startpoint0[[1]]/ratio,startpoint0[[2]]/100};
startpoint
];


(* Load DataModeler *)

(* <<"DataModeler`"; *)









(* --------- PHASE 0 -------------------------------------------- Set Up ---------------------------------------------------  -*)

SetUp[];
Print[                                                                              ];

(* start of body of program *)
(* ------- PHASE 1 ----------------------------------------------Reading in raw data-------------------------------------------------- *) 


Label["phase1"];phasenumber=1; Global`PHASENUMBER=1;
Print[Style["\[FilledSquare]\:3000Building functions with Symbolic regression",Bold, Purple,fontsize2]];
Print[Style[StringForm["\[FilledDiamond] ``", version],Bold,fontsize, Blue]];
Print[                                                                              ];
Print[                                                                              ];
Print[Style["\t\[EmptySquare]\:3000Phase 1  Reading in data",Bold, Purple,fontsize2]];
Print[                                                                              ];
Print[Style[StringForm["Starting time: date ``.``.`` time ``.``.``", 
startingtime[[1]],startingtime[[2]],startingtime[[3]],startingtime[[4]],startingtime[[5]],Round@startingtime[[6]]], Bold, fontsize]];

Label["indataagain"];
indata0=InputString[Style["[Inevitable Data]\nType in a FILE NAME of spread sheet, like data(.xlsx).\nOr type in an expression of DATA in Mathematica.\nPress OK to use DATA as an input data.\nType p to use the PREVIOUS data.\nType u to use the PREIVOUS FILTERED data.\nType end to quit",darkblue], WindowMargins -> windowaddress1];

If[indata0==="end"||indata0==="quit",flag="terminated";Goto["endinglabodata"]];
Global`INDATA0=indata0;

(* tag1=0: indata & initems, separately, tag1=1: Excel file *)
Which[
indata0===""&&ListQ[Global`DATA],
indata=Global`DATA; tag1=0,

ListQ[ToExpression[indata0]]&&NameQ[indata0],
indata=ToExpression[indata0]; tag1=0,

indata0==="p" && ListQ[Global`INDATA]&&IntegerQ[Global`TAG1]&&ListQ[Global`INITEMS], indata=Global`INDATA; tag1=Global`TAG1; initems=Global`INITEMS;indata0=Global`INDATA0; Goto["pass1"],

MemberQ[{"y", "u"}, indata0]&& ListQ[Global`INDATA1]&&IntegerQ[Global`TAG1]&&ListQ[Global`INITEMS], indata1=Global`INDATA1; tag1=Global`TAG1; initems=Global`INITEMS;indata0=Global`INDATA0; Print[Style[StringForm["Patient data name: ``, its dimensions: ``", If[indata0==="", "DATA", indata0], Dimensions[indata1]],fontsize,Bold]];Goto["pass2"],

indata0==="end"||indata0==="quit",flag="terminated";Goto["endinglabodata"],

path=NFindFile3[indata0]; path===True,
readin=datain[indata0];tag1=1;Global`RAWREADIN=readin;
Which[
readin[[1]]===1,
{initems, indata}=readin[[2]](*; initems=AddDots[initems] *),

NumberQ[readin[[1]]]&&readin[[1]]>= 2,
For[i=1, i<= readin[[1]], i++,
Print[StringForm["Sheet ``", i]];
Print[readin[[2, i,1]]]
];Print["                             "];
Label["sheetagain"];
sheet=Input[StringForm["Type in a NUMBER of the sheet for analysis\n\nPress OK to select the FIRST sheet\nType end to quit."], Null,WindowMargins-> windowaddress3];
Which[
IntegerQ[sheet]&&1<= sheet<=readin[[1]],
{initems, indata}={readin[[2,sheet,1]], readin[[2,sheet,2]]}(* ;initems=AddDots[initems] *),
sheet===Null,
{initems, indata}={readin[[2,1,1]], readin[[2,1,2]]}(* ;initems=AddDots[initems] *),
 ToString[sheet]==="end"||ToString[sheet]==="quit",
flag="terminated";Goto["endinglabodata"],
True, ErrorMessage["Type in, again."];Goto["sheetagain"]
]
],

True,ErrorMessage["Type in a file name or an expression, again."];Goto["indataagain"]
];PATH=path;Global`READIN=readin;TAG1=tag1;



Label["initemsagain"];
If[ tag1===0,
initems=InputString[Style["[Inevitable Data]\nType in an expression of COLUMN NAMES Numbered\nof the original data\nPress OK to use ITEMS as colum names\n\nType r to return to a previous phase.\nType end to quit.",darkblue], WindowMargins-> windowaddress1];

Which[ initems===""&&ListQ[Global`ITEMS], initems=Global`ITEMS, 
initems==="end"||initems==="quit", flag="terminated";Goto["endinglabodata"],
ListQ[ToExpression[initems]], initems=ToExpression[Map[ ToExpression, initems]],
initems==="r"||initems==="b", phaser[phasenumber],
 True, ErrorMessage["Type in, again."];Goto["initemsagain"]
];
(* initems=AddDots[initems]; *)
]; 
Global`INDATA=indata; Global`INITEMS=initems;Global`INDATA0=indata0;
Label["pass1"];

Print[Style[StringForm["Patient data name: ``, its dimensions: ``", If[indata0==="", "DATA", indata0], Dimensions[indata]],Bold,fontsize]];
leng=Length[initems]; numb=Length[indata]; Global`LENG=leng;
Print["                                      "];

If[leng===1, ErrorMessage["There must be more than one row in the data set."];Goto["endingbuildmodel"]];









(* ------- PHASE 2 -------Data Definition------------------------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 2  Data Definition",Bold,Purple,fontsize2]];
Label["phase2"];phasenumber=2; Global`PHASENUMBER=2;
Label["response"];

Print[Style[StringForm["The origianl colum labels:"],fontsize,Bold]];

initemsimage=DisplayForm[FrameBox[ initems]];
Print[ initemsimage];
Print["                             "];

response0=InputString[Style["Type in a colum NUMBER of the RESPONSE variable.\nPress OK to use the LAST column for the response.\n\nType r to return to a previous phase.\nType end to quit.",darkblue], WindowMargins->windowaddress1];
response=ToExpression[response0];

Which[
response0==="end"||response0==="quit", flag="terminated"; Goto["endingbuildmodel"],

response0==="r"||response0==="b", phaser[phasenumber],

response0==="", response=leng;criterion=indata[[All, leng]]; predictor=indata[[All, Range[1, leng-1]]];
allNames=initems[[All,2]]; allTrainingData=indata; predictorNames=Drop[initems, -1][[All,2]]; criterionName=Last[initems][[2]],

IntegerQ[response]&&1<= response<= leng, criterion=indata[[All, response]]; predictor= Transpose[ Drop[ Transpose[indata], {response}]];
criterionName= initems[[All,2]][[response]];allNames=Join[ Drop[initems[[All,2]], {response}], {criterionName}];
allTrainingData=Fuse[ predictor, criterion] ,

True, ErrorMessage["Type in, again."]; Goto["response"]
];

Global`CRITERION=criterion; Global`PREDICTOR=predictor;Global`CRITERIONNAME=criterionName; Global`PREDICTORNAMES=predictorNames;
Global`ALLNAMES=allNames; Global`ALLTRAININGDATA=allTrainingData;

criterionkinds=Union[criterion];Global`CRITERIONKINDS=criterionkinds;

allnamesimage=DisplayForm[FrameBox[ allNames]];
alldataimage=DisplayForm[FrameBox[ Short[allTrainingData, 100]]];

Print[Style[StringForm["\[FilledDiamond] allNames (The last variable `` is a target response)", criterionName],fontsize, Bold]];
Print[ allnamesimage];

Print[Style["\[FilledDiamond] allTrainingData (The last column is a response variable, the others are explanatory variables)",fontsize, Bold]];
Print[alldataimage];
Print["\t(The above output is suppressed below 100 lines.)"];
Print["                             "];
Print["                             "];

heads=Sort[allNames]; unionh=Sort[Union[allNames]]; 
If[ SameQ[heads, unionh]===False, 
Print[Style[StringForm["\[FilledDiamond]\:3000The following item names are overlapping in the dataset: ``",   Delete[ heads, Map[Part[#,1]&, Map[Position[heads,#]&,unionh]]]], Bold,16]];
Print[Style[StringForm["The ITEMS NAMES must be UNIQUE with each other."],Bold,16]];
Print[Style[StringForm["The process is terminated."],Bold,Red,16]];Goto["endingbuildmodel"]];







(* ------- PHASE 3 -------Interpolation of Empty cells and Rescaling data ---------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 3  Interpolation of Empty Cells",Bold,Purple,fontsize2]];
Label["phase3"];phasenumber=3;  Global`PHASENUMBER=3;

Print["                             "];

Print[Style["\[FilledDiamond] Data Completeness Check",Bold,fontsize]];

completeimage=DataCompletenessMap[
allTrainingData,
PlotRange->Automatic
];
Print[completeimage];Global`COMPLETEIMAGE=completeimage;
Print["                             "];



(* \:866b\:98df\:3044\:30c7\:30fc\:30bf\:306e\:88dc\:9593 *)
Global`ALLTRAININGDATA1=allTrainingData;Global`ALLNAMES1=allNames;
nullcheckout=nullchecker1[allTrainingData, allNames];
If[nullcheckout==="terminated", Goto["endingbuildmodel"]];
{allTrainingData, allNames}=nullcheckout;
Global`ALLTRAININGDATA2=allTrainingData;Global`ALLNAMES2=allNames;
If[ Dimensions[allTrainingData][[2]]=!=Length[allNames],ErrorMessage["Unknown nullchecker error. Row lengths do not match."];Goto["endingbuildmodel"]];
criterion=allTrainingData[[All,Dimensions[allTrainingData][[2]]]];Global`NEWCRITERION=criterion;

(* Real Data for analysis after modifying empty cells *)
Global`ALLTRAININGDATA=allTrainingData;Global`ALLNAMES=allNames;

(* \:5206\:6790\:5bfe\:8c61: allTrainingData, allNames *)
(* target varialbe \:306f\:3001allTrainingData\:306e\:6700\:7d42\:5217 *)

If[
Global`ALLTRAININGDATA1=!=Global`ALLTRAININGDATA2,

Print[Style["\[FilledDiamond] 1. New Data Completeness Check",Bold,fontsize]];
completeimage=DataCompletenessMap[
allTrainingData,
PlotRange->Automatic
];
Print[completeimage];
Print["                             "]; newdim=Dimensions[allTrainingData];
Print[Style[StringForm["\[FilledSquare] N of columns = ``,  n of rows = ``", newdim[[1]], newdim[[2]]], Purple, 15, Bold]];
Print[Style[StringForm["\[FilledSquare] Selected variables (the last one is the response varialbe:\n ``", Numberings[allNames]], 15, Bold]];
Print["                             "],

newdim=Dimensions[allTrainingData];
Print[Style[StringForm["\[FilledSquare] N of columns = ``,  n of rows = ``", newdim[[1]], newdim[[2]]], Purple, 15, Bold]];
Print[Style[StringForm["\[FilledSquare] Selected variables (the last one is the response varialbe:\n ``", Numberings[allNames]], 15, Bold]];
];


(* Exporting the interpolated data *)

Print["                                                      "];
Print[Style[StringForm["\[FilledDiamond]\:3000Exporting the Dataset for the Analysis after Interporated if necessary."], Bold, 16, Blue]];
Print["                                                      "];
ExportExcel3[allTrainingData, Numberings@allNames, "DatasetAfterNullCheck"];
Print["                                                      "];


(* Rescaling: Standardization & Normalization *)
Label["nsagain"];
nsjudge0=InputString["Do you RESCALE the explanatory variables?\nPress OK or type 0 to go WITHOUT rescaling.\nType 1 or s to STANDARDEIZE them with mean 0 and SD 1.\nType 2 or n to NORMALIZE them between 0 and 1.\n\nType end to quit.",WindowMargins-> windowaddress1];
nsjudge=ToExpression@nsjudge0;

Which[
nsjudge0===""||nsjudge0==="0", 
nsjudge=0,
nsjudge0==="1",
Null,
nsjudge0==="s",
nsjudge=1,
nsjudge0==="2",
Null,
nsjudge0==="n",
nsjudge=2,
nsjudge0==="end"||nsjudge0==="quit", Goto["endingbuildmodel"],
True, ErrorMessage["Type in, again."];Goto["nsagain"]
];

If[MemberQ[Map[NumericQ,Flatten@allTrainingData],False],

Print["                                          "];Print[Style[StringForm["\[FilledSquare] There are some NON-NUMERIC cells in the data."], Bold, Red, 15]];Print["                                          "]];

Which[
nsjudge===0,

Print["                                          "];
Print[Style[StringForm["\[EmptyDiamond] The explanatory variables are used in the ORIGINAL SCALE."], Bold,Blue, 14]];
Print["                                          "],

nsjudge===1,

{allTrainingData0, allNames0}={allTrainingData, allNames};
Global`ALLTRAININGDATA0=allTrainingData0; Global`ALLNAMES0=allNames0; 
explanatdata=Map[Drop[#,-1]&, allTrainingData];targetdata=Map[Last,allTrainingData];
Global`EXPLANATDATA=explanatdata; Global`TARGETDATA=targetdata;
liststandout=ListStandardize[explanatdata, Numberings@Drop[allNames,-1]];
allTrainingData=Fuse[liststandout[[1]], targetdata];
Global`ALLTRAININGDATAS=allTrainingData; 
Print[Style[StringForm["\[FilledDiamond] The explanatory variables are used in the STANDARDIZED SCALE with the average as 0 and the standard deviation as 1."], Bold,Blue, 16]];
Print[Style[StringForm["\[EmptyDiamond] The explanatory variables have been STANDARDIZED."], Bold,16]];

Print["                                          "];
Print[Style[StringForm["\[EmptySquare] The Plot of Origianl Distributions of variables"], Bold,16]];
graphO=ListPlot[allTrainingData0, ImageSize-> 750, PlotRange-> All, PlotMarkers->Automatic, AxesLabel-> {Style["Explanatory\nVariable", 15],Style["Original\nValue",15]},
TicksStyle->Directive["Label", 14,Bold]];
Print@graphO;
Print["                                          "];

Print[Style[StringForm["\[EmptySquare] The Plot of Distributions of Standarzed variables"], Bold,16]];
graphS=ListPlot[allTrainingData, ImageSize-> 750, PlotRange-> All, PlotMarkers->Automatic, AxesLabel-> {Style["Explanatory\nVariable", 15],Style["Standardized\nValue",15]},
TicksStyle->Directive["Label", 14,Bold]];
Print@graphS;
Print["                                          "],

nsjudge===2,

{allTrainingData0, allNames0}={allTrainingData, allNames};
Global`ALLTRAININGDATA0=allTrainingData0; Global`ALLNAMES0=allNames0; 
explanatdata=Map[Drop[#,-1]&, allTrainingData];targetdata=Map[Last,allTrainingData];
Global`EXPLANATDATA=explanatdata; Global`TARGETDATA=targetdata;
listnormout=ListNormalize[explanatdata, Numberings@Drop[allNames,-1]];
allTrainingData=Fuse[listnormout[[1]], targetdata];
Global`ALLTRAININGDATAN=allTrainingData;  
Print[Style[StringForm["\[FilledDiamond] The explanatory variables are used in the NORMALIZED SCALE that are scaled between 0 and 1."], Bold,Blue, 16]];
Print[Style[StringForm["\[EmptyDiamond] The explanatory variables have been NORMALIZED."], Bold,16]];

Print["                                          "];
Print[Style[StringForm["\[EmptySquare] The Plots of Origianl Distributions of variables"], Bold,16]];
graphO=ListPlot[allTrainingData0, ImageSize-> 750, PlotRange-> All, PlotMarkers->Automatic, AxesLabel-> {Style["Explanatory\nVariable", 15],Style["Original\nValue",15]},
TicksStyle->Directive["Label", 14,Bold]];
Print@graphO;
Print["                                          "];

Print[Style[StringForm["\[EmptySquare] The Plots of Distributions of Normalized variables"], Bold,16]];
graphN=ListPlot[allTrainingData, ImageSize-> 750, PlotRange-> All, PlotMarkers->Automatic, AxesLabel-> {Style["Explanatory\nVariable", 15],Style["Normalized\nValue",15]},
TicksStyle->Directive["Label", 14,Bold]];
Print@graphN;
Print["                                          "],

True,
ErrorMessage["Unexpected Rescaling Error"];Goto["endingbuildmodel"]
];

(* Exporting the rescaled data *)

If[ nsjudge=!=0,
Print["                                                      "];
Print[Style[StringForm["\[FilledDiamond]\:3000Exporting the Dataset for the Analysis after Rescaling if necessary."], Bold, 16, Blue]];
Print["                                                      "];
ExportExcel3[allTrainingData, Numberings@allNames, "DatasetAfterRescaling"];
];
Print["                                                      "];






(* ------- PHASE 4 ------- Selection of Validation Method ---------------------------------------------------------------- *) 

Print["                             "];
Print["                             "];

Print[Style["\t\[EmptySquare]\:3000Phase 4  Validation Method",Bold,Purple,fontsize2]];
Label["phase3"];phasenumber=4;  Global`PHASENUMBER=4;

Print["                             "];
Print[Style["\[FilledDiamond] Selection of Validation Method",Bold,fontsize]];
Print["                             "];

(* Selection from Hold-out method, Cross validation method or No division *)
Label["valimeth"];
valimeth0=InputString[Style[StringForm["\[FilledSquare] VALIDATION method\nPress OK or type h to use HOLD-OUT method to split data into Training data and Test data.\nType l (el) or o (ou) to use LEAVE-ONE-OUT method.\nType n not to divide data into them.\nType end to quit."]], WindowMargins-> windowaddress1];

(* Type k to use K-FOLD CROSS VALIDATION method (NOT READY).\n*)

Which[
valimeth0==="end"||valimeth0==="quit", Goto["endingbuildmodel"],

valimeth0===""||valimeth0==="h",
valimeth=1;
Print[Style[StringForm["\t\[EmptyDiamond] Validataion method: Hold-out Validation method was selected."], Bold, fontsize, Blue]],

valimeth0==="o"||valimeth0==="l"||valimeth0==="L",
valimeth = 2;
Print[Style[StringForm["\t\[EmptyDiamond] Validataion method: Leave-One-Out Cross Validation method was selected."], Bold, fontsize, Blue]],

(* valimeth0==="k",
valimeth = 3;
Print[Style[StringForm["\t\[EmptyDiamond] Validataion method: K-Fold Cross Validation method was selected."], Bold, fontsize, Blue]], *)

valimeth0 === "n",
valimeth = 0;
Print[Style[StringForm["\t\[EmptyDiamond] Validataion method: No validation."], Bold, fontsize, Blue]],

True, 
ErrorMessage["Type in, again."];
Goto["valimeth"]
];Global`VALIMETH=valimeth;

PB[];
PB[];
PB[];

(* Setting the parameter of slitting or folding *)
Which[

valimeth===0,

 Print[Style[StringForm["\[EmptyDiamond] All the raw data is used for both training data and test data without divided."],Bold,fontsize]];Goto["skip"],

valimeth===1,

(* Hold out method *)
Label["selectagain"];
selectpara0=InputString["\[FilledSquare] HOLD=OUT method\nPress OK to set Training data : Test data = 7 : 3.\nType a NUMBER for a TEST SIZE Between 0\:301c1.\n\nType end to quit.", WindowMargins-> windowaddress1];
selectpara=ToExpression[selectpara0];
Which[selectpara0==="end"||selectpara0==="quit",Goto["endingbuildmodel"],
selectpara0==="", splitratio=0.3,
NumberQ[selectpara]&&0<selectpara<1,
					splitratio =selectpara,
True, ErrorMessage["Type in, again."];Goto["selectagain"]
]; Print[Style[StringForm["\[FilledSquare] The hold-out method was chosen.\n\[EmptyDiamond] All the raw data was split into training data and test data with `` : `` ratio.", 1-splitratio, splitratio],Bold,fontsize]];
Global`SPLITRATIO=splitratio,

valimeth===2,

(* Leave-One-Out Cross validation *)
Goto["LOOmethod"],



valimeth===3,

(* k-Fold Cross validation method *)
ErrorMessage["NOT PROGRAMMED YET."];Goto["endingbuildmodel"];

Label["crossagain"];
crossnumb0=InputString["Press OK to do 5-FOLD CROSS VALIDATION.\nType an INTEGER (2 or more) for a number of blocks.\n\nType end to quit.", WindowMargins-> windowaddress1];
crossnumb=ToExpression[crossnumb0];
Which[crossnumb0==="end"||crossnumb0==="quit", Goto["endingbuildmodel"],
crossnumb0==="",  foldnumb=5,
IntegerQ[crossnumb]&&1<crossnumb,
					foldnumb =crossnumb,
True, ErrorMessage["Type in, again."];Goto["crossagain"]
] ;
Print[Style[StringForm["\[FilledSquare] The cross-validation method was chosen.\n\[EmptyDiamond] All the raw data was divided into `` blocks.", foldnumb],Bold,fontsize]];
Global`FOLDNUMB=foldnumb,


True,
ErrorMessage["Unexpected valimth error"];Goto["endingbuildmodel"]

];

Print["                             "];

(* Selecting specific data into each subgroup for validation *)

Label["seedagain"];
seednumber0=InputString["Press OK to use RandomInteger[10000] as a SEED.\n\nType in an INTEGER for a seed.\nType f to FIX the seed for RANDOMIATION (seed=0).\nType end to quit.",WindowMargins-> windowaddress1];
seednumber=ToExpression[seednumber0];

Which[seednumber0==="end"||seednumber0==="quit", Goto["endingbuildmodel"],
seednumber0==="f", seed=0,
IntegerQ@seednumber, seed=seednumber,
seednumber0===""||seednumber0==="r", seed=RandomInteger[10000],
True, Print["Type an integer or n, again"];Goto["seedagain"]
];

Print["                             "];

(* training data and test data specified by randomization *)
samplesize=Length@allTrainingData;
randorder=NRandomSample[Range@samplesize, samplesize, seed];
Print[Style[StringForm["\[FilledSquare] The seed for randomization is ``.", seed], 16, Bold,Red]];
Print["                             "];
Global`SAMPLESIZE=samplesize;
Global`RANDORDER=randorder;

Label["skip"];
Which[

valimeth===1,

(* Hold out method *)
testsize=Floor[splitratio*samplesize];
trainsize=samplesize-testsize;
Global`TESTSIZE=testsize; Global`TRAINSIZE=trainsize; 

trainpos=Take[randorder, trainsize];
testpos=Drop[ randorder, trainsize];
Print[Style[StringForm["\[FilledDiamond] The position of test data is `` with a randomization seed number ``.\nThe whole position of test data is shown by TESTPOS\nThe whole position of training data is shown by TRAINPOS.", Short[testpos], seed], 16]];
Print["                             "];
Global`TRAINPOS=trainpos; Global`TESTPOS=testpos;
trainlines=MakeTwin[trainpos , Table[1, {Length@trainpos}]];
testlines=MakeTwin[testpos, Table[0, {Length@testpos}]];
traintestpos=Sort[Join[trainlines,testlines], #1[[1]]<= #2[[1]]&];
Global`TRAINLINES=trainlines; Global`TESTLINES=testlines;
Global`TRAINTESTPOS=traintestpos;

hotraindata=Part[allTrainingData, trainpos];
hotestdata=Part[allTrainingData, testpos];
Global`HOTRAINDATA=hotraindata; Global`HOTESTDATA=hotestdata;
Global`TRAINSAMPLES=hotraindata; Global`TESTSAMPLES=hotestdata;


Print[Style[StringForm["\[FilledDiamond]\:3000Hold-out method: Traing data N = ``, Test data N = ``  (`` : ``)", Length@hotraindata, Length@hotestdata, 1-splitratio, splitratio], Bold, 15]];
Print[Style["They are expression as TRAINSAMPLES and TESTSAMPLES.", Bold, fontsize]];
PB[];

ExportExcel3[traintestpos, Numberings@{"CaseNo","TrainingData"}, "TrainingdataPosition"];
Print[Style[StringForm["\[FilledDiamond]\:3000The positions of traingdata and testdata with a randomization seed number ``\n\tare exported in the Excel file, TraingindataPositioin.", seed],Bold, 15, Red]];
PB[];
ExportExcel2[hotraindata, Numberings@allNames, "Trainingdataset"];
ExportExcel2[hotestdata, Numberings@allNames, "Testdataset"];
Print[Style[StringForm["\[FilledDiamond]\:3000Trainingdata and Testdata for the Hold-Out method are expressed as TRAINSAMPLES and TESTSAMPLES.\n\tThey are exported in the Excel file, Trainingdataset and Testdataset."],Bold, 15, Red]];
PB[],


valimeth===3,

(* k-fold Cross validation method *)
foldsize=Round@(samplesize/foldnumb);
foldbag={};blockbag={};
randcage[1]:= randorder;Global`RANDCAGE[1]=randcage[1];

For[ii=1, ii<= foldnumb-1, ii++,
randpos[ii]=RandomSample[randcage[ii], foldsize];
AppendTo[foldbag, randpos[ii]];
AppendTo[blockbag,{ii, Part[allTrainingData, randpos[ii]]}];
randcage[ii+1]=IntactComplement[randcage[ii],randpos[ii]]     ];
AppendTo[foldbag, randcage[foldnumb]];AppendTo[blockbag, {foldnumb, Part[allTrainingData, randcage[foldnumb]]}];
Global`FOLDBAG=foldbag;Global`BLOCKBAG=blockbag;

Print[Style[StringForm["\[FilledDiamond]\:3000``-fold Cross Validation method: The data was divided into `` blocks. The case counts in them are ``, respectively.",foldnumb, foldnumb, Map[Length,foldbag]], Bold, fontsize]];
Print[Style[StringForm["The `` folds of data blocks are expressed as BLOCKBAG.", foldnumb], Bold, fontsize]]

];
Print["                             "];



(* Designation of selected dataset to TrainData with allNames as item names *)

Which[
valimeth===0,
	(* No validation *)
	TrainData=allTrainingData;
	TestData={},
valimeth===1,
	(* Hold-out method *)
	TrainData=hotraindata;
	TestData=hotestdata,
valimeth===3,
	(* k-fold cross validation *)
	TrainData=Drop[blockbag,-1];
	TestData=Take[blockbag,-1],
True,
	ErrorMessage["Unexpected valimeth designation error"];Goto["endingbuildmodel"]
];
Global`P4TRAINDATA=TrainData;
Global`P4TESTDATA =TestData;






(* ------- PHASE 5 -------Exploring Data---------------------------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 5  Exploring Data with the Whole Dataset",Bold,Purple,fontsize2]];
Label["phase3"];phasenumber=5;  Global`PHASENUMBER=5;

Print["                             "];

Print[Style["\[FilledDiamond] Data Completeness Check",Bold,fontsize]];



Print[Style["\[FilledDiamond] 1. Summary of Basic Statistics",Bold,fontsize]];
summaryimage=DataSummaryTable[
allTrainingData,
DataVariableLabels-> allNames,
SummaryStatistics->Automatic
];
Print[summaryimage];
Print["                             "];


Print[Style["\[FilledDiamond] 2. Distribution Plot of Data", Bold,fontsize]];
(* distributionimage=DataDistributionPlot[
allTrainingData,
DataVariableLabels\[Rule] allNames
]; *)
Print[distributionimage];
Print["                             "]; 


Print[Style["\[FilledDiamond] 3. Correlation Matrix Plot of Data",Bold,fontsize]];
correlationmatrix=CorrelationMatrixPlot[
allTrainingData,
DataVariableLabels-> allNames,
PlotLegends->BarLegend[{Hue[1/3 (#+1)]&,{-1,1}}],
ImageSize-> size1
];
Print[correlationmatrix];
Print["                             "];

Print[Style[StringForm["\[EmptySquare] Ordered Correlation Coefficients of All variables"],Bold, fontsize, Blue]];
coeffvalues=NGetCorrelation[allTrainingData, Numberings@allNames];
Global`COEFFVALUES=coeffvalues;
coefftable=Sort[MakeTwin[CutLast@allNames, coeffvalues], #1[[2]]>= #2[[2]]&];
Global`COEFFTABLE=coefftable;
numbcoefftable=Fuse[Range[Length@CutLast@allNames], coefftable];
Global`NUMBCOEFFTABLE=numbcoefftable;
Print@TextGrid[numbcoefftable, Frame-> All];
Print[Style[StringForm["\t\[EmptyDiamond] Raw correlation coefficients are expressed as COEFFVALUES. Correlation Coeffcienets Tables is expressed as COEFFTABLE and NUMBCOEFFTABLE."],Bold,fontsize]];


Print["                             "];

Print[Style["\[FilledDiamond] 4. Univariate Plot of Data (X-axes is the target varible)",fontsize,Bold]];
univariateimage=UnivariatePlot[
allTrainingData,
DataVariableLabels-> allNames,
SortBy->-1,
ImageSize-> size1
];
Print[univariateimage];
Print["                             "];


(* Print["\[FilledDiamond] 3. Bivariate Plot of Data"];
bivariateimage=BivariatePlot[
TrainData,
DataVariableLabels\[Rule] allNames
];
Print[bivariateimage];
Print["                             "];  *)


Print[Style["\[FilledDiamond] 5. Correlation Chart of Data in the original order\n\t(BLUE: Positive correlation,  RED: Negative correlation)",fontsize,Bold]];
correlationchart=Quiet@CorrelationChart[
allTrainingData,
DataVariableLabels-> allNames,
ImageSize-> size2
];
Print[correlationchart];
Print["                             "];

Print[Style["\[FilledDiamond] 6. Correlation Chart of Data in the correlation coefficient order\n\t(BLUE: Positive correlation,  RED: Negative correlation)",fontsize,Bold]];
correlationchart=Quiet@CorrelationChart[
allTrainingData,
DataVariableLabels-> allNames,
SortBy-> Correlation,
ImageSize-> size2
];
Print[correlationchart];
Print["                             "];
Print["                             "];







(* ------- PHASE 6 -------Model Development Validated with Hold-out method -------------------------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 6 Model Development with Genetic Programming",fontsize2,Bold, Purple]];
Label["phase4"];phasenumber=6;  Global`PHASENUMBER=6;
Print["                             "];

Label["projectnameagain"];
projectname0=InputString[StringForm["Type in a PROJECT NAME.\nPress OK to use Predict`` as a project name.\nType end to quit.", criterionName], WindowMargins-> windowaddress1];
Which[
projectname0==="quit"||projectname0==="end", Goto["endingbuildmodel"],
projectname0==="", projectname=StringJoin["Predict", criterionName],
StringQ[projectname0],projectname=projectname0,
True, ErrorMessage["Type in, again."];Goto["projectnameagain"]
];
Print[Style[StringForm["\[FilledDiamond] The project name is ``.", projectname], Bold, fontsize]];
Print["                                 "];


Label["judgeagain"];
judge=InputString[StringForm["Press OK to start a NEW MODEL DEVELOPMENT.\nType a to use ARCHIVED MODELS of  ``\nType end to quit.", projectname]];


Which[
judge==="end"||judge==="quit", Goto["endingbuildmodel"],



(* \:65b0\:898f\:306e\:8fd1\:4f3c\:95a2\:6570\:3092\:4f5c\:6210 *)
judge==="",

Print[Style["\[EmptySquare] A new process of model development starts.", Bold, fontsize]];
Print["                                                     "];
Label["timeconstraintagain"];
timeconstraint0=InputString["Type in a TIME in MINUTES for time constraint,\nlike 30 for 30 minutes calculation for each evolution.\n\nPress OK to set it as 30 minutes.\nType end to quit.", WindowMargins-> windowaddress1];
timeconstraint=ToExpression[timeconstraint0];
Print[Style[StringForm["\[FilledSquare] The Setting for Symbolic Regression"], Bold, fontsize, Red]];
Which[
timeconstraint0==="quit"||timeconstraint0==="end", Goto["endingbuildmodel"],
timeconstraint0==="", timeconstraint=30,
NumberQ[timeconstraint]&&Positive[timeconstraint], Null,
True, ErrorMessage["Type in, again."];Goto["timeconstraintagain"]
];
Print[Style[StringForm["\[EmptyDiamond] The time constraint is `` min for one evolution.", timeconstraint], Bold,fontsize]];

Label["evolutionsagain"];
evolutions0=InputString["Type in an Integer of independent evolutions, like 4, 8 or 12.\nPress OK to set it as 8.\nType end to quit.", WindowMargins-> windowaddress1];
evolutions=ToExpression[evolutions0];
Which[
evolutions0==="end"||evolutions0==="quit", Goto["endingbuildmodel"],
evolutions0==="", evolutions=8,
IntegerQ[evolutions]&&1<= evolutions<= 64, Null,
evolutions>64, judge1=InputString[StringForm["The tyed-in integer is ``.\nIf it's correct, press OK.\nIf it's wring, type n to chnage it.\nType end to quit", evolutions]];
Which[judge1==="", Null, judge1==="n", Goto["evolutionsagain"], judge1==="end"||judge1==="quit", Goto["endingbuildmodel"],True, Goto["evolutionsagain"]],
True, ErrorMessage["Type in, again."]; Goto["evolutinsagain"]
];
Print[Style[StringForm["\[EmptyDiamond] A number of independent evolutions is ``.", evolutions], fontsize,Bold]];

cutoff=0.5;
classes=Union@allTrainingData[[All,-1]];Global`CLASSES=classes;
If[MemberQ[{{0,1},{0,1,Null},{0,1,""},{0,1,Null,""},{0`, 1`}, {0., 1.}}, classes],
Label["cutoffagain"];
cutoff0=InputString["Type in a Number between 0 and 1 for the threshold in the dichotomous classification problem.\nPress OK to set it as 0.5.\nType end to quit.", WindowMargins-> windowaddress1];
cutoff=ToExpression@cutoff0;
Which[
cutoff0==="end"||cutoff0==="quit",Goto["endingbuildmodel"],
cutoff0==="",cutoff=0.5,
NumberQ@cutoff&&0<cutoff<1, Null,
True, ErrorMessage["Type in, again."];Goto["cutoffagain"]
];Global`CUTOFF=cutoff;
Print[Style[StringForm["\[EmptyDiamond] The threshold for dichotomous classification is ``.", cutoff], fontsize,Bold]];
];

totaltime=MinToHourMinSec[timeconstraint*evolutions];Global`TOTALTIME=totaltime;
Print[Style[StringForm["\[EmptyDiamond] A total time of calculation is ``.", totaltime], Bold, fontsize]];
Print["                             "];

now0=Now[[1]];Global`NOW0=now0;
Print[Style[StringForm["\[FilledDiamond] Present Time             : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", now0[[1]],now0[[2]],now0[[3]],now0[[4]],now0[[5]],Round@now0[[6]]],Bold,fontsize]];


future=DateList[AbsoluteTime[Now]+(60*timeconstraint*evolutions)/(corecount/2)];Global`FUTURE=future;
Print[Style[StringForm["\[FilledDiamond] Finish Time with `` cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", corecount,future[[1]],future[[2]],future[[3]],future[[4]],future[[5]],Round@future[[6]]],Bold,fontsize]];

(* future0=DateList[AbsoluteTime[Now]+(HourMinSecToSec@totaltime)];Global`FUTURE0=future0;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 2 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future0[[1]],future0[[2]],future0[[3]],future0[[4]],future0[[5]],Round@future0[[6]]],Bold,fontsize]];
future4=DateList[AbsoluteTime[Now]+(HourMinSecToSec@totaltime)/2];Global`FUTURE4=future4;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 4 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future4[[1]],future4[[2]],future4[[3]],future4[[4]],future4[[5]],Round@future4[[6]]],Bold,fontsize]];
future8=DateList[AbsoluteTime[Now]+(HourMinSecToSec@totaltime)/4];Global`FUTURE8=future8;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 8 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future8[[1]],future8[[2]],future8[[3]],future8[[4]],future8[[5]],Round@future8[[6]]],Bold,fontsize]]; *)

Print["                             "];



(* Nof0=Count[criterion, 0];
Global`NOF0=Nof0;
Nof1=100. (Length@criterion-Nof0)/Length@criterion;
Global`NOF1=Nof1; *)


(* DataSubsetSize *)
subsetsize=subsetsizefunc[trainsize];
Global`DATASUBSETSIZE=subsetsize;
Print[Style[StringForm["\[EmptyDiamond] The DataSubsetSize: ``%", NF@subsetsize], Bold,fontsize]];
PB[];


Label["startagain"];
start=InputString[StringForm["The setting for symbolic regression is as follows.\n1. Project name: ``\n2. Time Constraint: ``\n3. Independent Evolutions: ``\n4. Total calculation time: ``\n\nPress OK to start the caliculation for Symbolic Regression.\nType r to reset the setting.\nType end to quit.", projectname, MinToHourMinSec[timeconstraint], evolutions, totaltime,WindowMargins-> windowaddress1]];

Which[
start==="end"||start==="quit", Goto["endingbuildmodel"],
start ==="r", Goto["phase4"],
start==="", 


(* Expeted time when the calculation is completed. *)
starttime=Date[];endtime=DateList[AbsoluteTime[starttime]+timeconstraint*evolutions*60];Global`ENDTIME=endtime;
Print[Style[StringForm["\[FilledDiamond] The start time: ``", DateTime[starttime]],14]];
Print[Style[StringForm["\[FilledDiamond] The required time for calculation: `` min with 2 cores", timeconstraint*evolutions],14]];
Print[Style[StringForm["\[FilledDiamond] The expected end time: `` with 2 cores", DateTime[endtime]],14]];
Print["                                 "];


(* Model Creation *)
paretofront=ParetoFrontPlot[
developedModels = SymbolicRegression[
TrainData,
DataVariables->allNames,
TargetColumn-> criterionName,

RobustModels-> robustModelHO,
EvolutionStrategy-> evolutionStrategyHO,
SelectionStrategy-> selectionStrategyHO,
DataSubsetSize-> subsetsize,

(* MonitorModelSearch \[Rule] All,
GenerationMonitor \[Rule] ParetoFront,
CascadeMonitor \[Rule] ParetoFront, 
RunMonitor \[Rule] ParetoFront,
EvolutionMonitor \[Rule] ModelQuality, *)

Quiet->True,
Speak-> False,
ProjectName->projectname,
StoreModelSet->True,
MultiCore->Automatic,
IndependentEvolutions-> evolutions,
TimeConstraint-> timeconstraint*60,
ImageSize-> size1,
ToLowerCase-> False
], DensityPlot-> False,PlotRange-> {Automatic,{0,1.05}}    (* {Automatic, {0, 1.15, 0.1}} *)
];

Global`PARETOFRONT=paretofront;
modelcount0=Length[developedModels];
paretofront1=Show[paretofront];
Global`PARETOFRONT1=paretofront1; 

Print[Style["\[FilledDiamond] ParetoFront Plot", fontsize,Bold]];
Print[paretofront1];
Print[Style[StringForm["\[FilledSquare] A total of `` models were created with Symbolic Regression", modelcount0], Bold,Red, fontsize]];
Print["                           "];




linearModels = UniqueFitnessModels@Cases[developedModels, model_GPModel/;MatchQ[True,OptimizeLinearModel /. ModelPersonality@model]
];
linearimage=ParetoFrontContextPlot[linearModels,developedModels, (*  GridLines\[Rule]  {Automatic, Automatic}, *)
PlotLabel->LabelForm[{Length@linearModels,"models from",Length@IndependentEvolutions@linearModels,"IndependentEvolutions used OptimizeLinearModel"},Joined->True], PlotRange-> {Automatic,{0,1.05}} , ImageSize-> size1];
Global`LINERMODELS=linearModels;Global`LINEARIMAGE=linearimage;
paretofrontlinear=Show[linearimage];
Global`PARETOFRONTLINEAR=paretofrontlinear;

Print[Style["\[FilledDiamond] Linear Model Plot", Bold, fontsize]];
Print[paretofrontlinear];
Print[Style[StringForm["\[FilledSquare] `` LINEAR models are included in `` models",Length[linearModels], modelcount0], Bold, fontsize]];
Print["                           "];



freeformModels = Cases[developedModels, model_GPModel/;MatchQ[False,OptimizeLinearModel /. ModelPersonality@model]
];
freeformimage=ParetoFrontContextPlot[freeformModels,developedModels, (*  GridLines\[Rule]  {Automatic ,Automatic}, *)(* {Automatic, {0, 1.15, 0.1}} *)
PlotLabel->LabelForm[{Length@freeformModels,"models from",Length@IndependentEvolutions@freeformModels,"IndependentEvolutions did not use OptimizeLinearModel"},Joined->True], PlotRange->{Automatic,{0,1.05}}, ImageSize-> size1];
Global`FREEFORMODELS=freeformModels;Global`FREEFORMIMAGE=freeformimage;
paretofrontfree=Show[freeformimage];
Global`PARETOFRONTFREE=paretofrontfree;

Print[Style["\[FilledDiamond] Freeform Model Plot",fontsize,Bold]];
Print[paretofrontfree];
Print["                           "];
Print[Style[StringForm["\[FilledSquare] `` FREEFORM models are included in `` models",Length[freeformModels], modelcount0], Bold, fontsize]];



archivedModels=developedModels;
Global`DEVELOPEDMODELS=developedModels;modelcountD=Length[developedModels];
Global`ARCHIVEDMODELS=archivedModels,

True, ErrorMessage["Type in, again."];Goto["startagain"]
],


(* \:65e2\:306b\:8a08\:7b97\:3055\:308c\:305f\:8fd1\:4f3c\:95a2\:6570\:3092\:4f7f\:7528 *)
judge==="a",


Print[Style[StringForm["\[FilledDiamond] Archived models in `` are loaded.", projectname], Bold, fontsize]];
archivedModels= RetrieveModelSets[
ProjectName->projectname
];
Global`ARCHIVEDMODELS=archivedModels;modelcountA=Length[archivedModels];
developedModels=archivedModels;Global`DEVELOPEDMODELS=developedModels;

archivedimage=ParetoFrontPlot[#,
PlotLabel->LabelForm[{Length@#,"models were developed"},Joined->True    ], DensityPlot-> False
]&@archivedModels;
Print[archivedimage];
Print["                           "],

True, ErrorMessage["Type in,again."];Goto["judgeagain"]];



modelcount=Which[judge==="", modelcountD, judge==="a", modelcountA, True, Null];
Global`MODELCOUNT=modelcount;

Print["                             "];



(* ------- PHASE 7 -------Model Selection---------------------------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 7  Selection of Model with Accurary and Simplicity",Bold,Purple,fontsize2]];
Label["phase5"];phasenumber=7;  Global`PHASENUMBER=7;
Print["                             "];


Print[Style["\[FilledDiamond] 1. Model of Interest",Bold,fontsize]];

Label["qualityagain"];
quality0=InputString["Press OK to use AUTOMATIC Quality Box setting program.\n\nType in a VECTOR of TWO NUMBERS for {Complixity, Accuracy}, like {200, 0.4} for choosing a region of interest.\nType end to quit."];
quality=ToExpression[ quality0];

Which[
quality0==="end"||quality0==="quit", Goto["endingbuildmodel"],
quality0==="", qualityflag=1,
RealVectorQ[quality]&&Length[quality]===2, qualityflag=0,
True, ErrorMessage["Type in, again."];Goto["qualityagain"]
];





(* Quality Box setting program starts *)

Which[
qualityflag===0, Null,


qualityflag===1,


(* Fitting Index\:306e\:8a2d\:5b9a *)
Label["fitagain"];
fitindex0=InputString["Type a REAL NUMBER around 0.75 for FITTING INDEX.\n\nPress OK to set FITTING INDEX as 0.75.\nThe BIGGER it goes, the more it UNDER FITS.\nThe SMALLER it goes, the more it OVER FITS.\n\nType end to quit.", WindowMargins-> windowaddress1];
fitindex=ToExpression@fitindex0;

Which[
fitindex0==="", fitindex=0.75,
fitindex0==="quit"||fitindex0==="end"||fitindex0==="stop", Goto["endingbuildmodel"],
NumberQ@fitindex&&(0.3<= fitindex<= 2), Null,
NumberQ@fitindex&&(0.3> fitindex||fitindex> 2), 
	inquiry=InputString["The fitting index might be out of ordinary range.\nPress OK to use it.\nType a NUMBER as an index.\n\nType end to quit."];
	Which[inquiry==="", Null, inquiry==="y", Null,NumberQ@ToExpression@inquiry, fitindex=ToExpression@inquiry, True, Goto["endingbuildmodel"]],
True, Print["Type it, again."];Goto["fitagain"]
];

Print[Style[StringForm["\[EmptyDiamond] Fitting index is ``.", fitindex], fontsize,Bold, Red]];
Print[Style[StringForm["(Fitting index sets the initial values of QualityBox, \n which set the tendency for overfitting or underfitting)"], fontsize,Bold]];(* Fitting index is the gradient of a line to find out the quality box levels. *)
PB[];




(* Quality Box\:306e\:4e2d\:306b\:542b\:307e\:308c\:308b\:95a2\:6570\:306e\:6700\:5c0f\:306e\:5272\:5408 *)
Label["minagain"];
minpercent0=InputString["Type in a REAL NUMBER between 1 and 50 for the MINIMUM PERCENT of functions selected by QALITY BOX.\n\nPress OK to set is as 8%.\nType end to quit.", WindowMargins-> windowaddress1];
minpercent=ToExpression@minpercent0;

Which[
minpercent0==="", minpercent = 8,
minpercent0==="end"||minpercent0==="quit"||minpercent0==="stop", Goto["endingbuildmodel"],
NumberQ@minpercent&&0<minpercent<= 50, Null,
NumberQ@minpercent&&(minpercent<= 0||minpercent> 50),
inquiry=InputString["The minimum perent typed in is out of the expeted range.\nType in a NUMBER between 1 and 50.\nPress OK to set in as 8%.\n\nType end to quit."];
	Which[inquiry==="", minpercent=8, NumberQ@ToExpression@inquiry&&(0<ToExpression@inquiry<50), minpercent=ToExpression@inquiry, True, Goto["endingbuildmodel"]],
True, Print["Type it, again."];Goto["fitagain"]
];

Print[Style[StringForm["\[EmptyDiamond] Minimum percent is ``.", minpercent], fontsize,Bold, Red]];
Print[Style[StringForm["(Minimum percent sets the minimum percentage of functions selected by QualityBox.)"], fontsize,Bold]];(* Fitting index is the gradient of a line to find out the quality box levels. *)
PB[];





(* QualityBox values *)
quality=findstartpoint[developedModels];
Global`QUALITYBOX=quality;


paretofront2=Show[paretofront,  PlotRange-> {Automatic,{0,1}}, GridLines-> {Automatic, {0, 1.15, 0.1}}, Epilog-> {Green,AbsolutePointSize[11], Point[quality]}];


archivedbag={};

archivedModels=developedModels;
Clear[Global`ARCHIVEDMODELS];
Global`ARCHIVEDMODELS=archivedModels;

AppendTo[archivedbag, archivedModels]; 



(* The 1st developed models *)

Print[Style["\[FilledDiamond] ParetoFront Plot", fontsize,Bold]];
PB[];
Print[Style[StringForm["\[FilledDiamond] A total of `` models were created with Symbolic Regression though machien learning", modelcount], Bold,Blue, fontsize]];
Print[paretofront2];
Print["                           "];



(* Interesting models *)

Print[Style["\[FilledDiamond] Setting Quality Box values",Bold,fontsize]];

PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Initial QuatiliyBox levels are {``, ``} in this first turn.", NF@quality[[1]], NF@quality[[2]]],Bold,fontsize, darkgreen]];
PB[];

limits={minpercent, 55};
interestingbag={};

Global`LIMITS=limits;minP=limits[[1]]/100.; maxP=limits[[2]]/100.;
Print[Style[StringForm["\t\[EmptyDiamond] Limits of percents for narrowing down models: Min ``%, Max ``%", limits[[1]], NF@limits[[2]]],fontsize, Bold]];
PB[];


(* Selecting the 1st interesting model *)

firstModels=SelectModels[archivedModels,QualityBox->quality];
Global`FIRSTMODELS=firstModels;
Smodels=firstModels;

firstcount=Length@firstModels;
Pnumb=firstcount;
Anumb=modelcount;

Cnumb=quality[[1]];
Lnumb=quality[[2]];
finalquality={Cnumb, Lnumb};
Global`PNUMB=Pnumb; Global`ANUMB=Anumb;Global`MINP=minP; Global`MAXP=maxP;
Global`CNUMB=Cnumb; Global`LNUM =Lnumb;Global`DELTAC=deltaC; Global`DELTAL=deltaL;


j=1;labeling=0;
If[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100, 

labeling=1;
PB[];Print[Style[StringForm["\[FilledSquare] Process for adjusting levels of Quality Box is initiated\n\tmainly to limit the selected models between ``% and ``% of all generated models.", 100. minP, NF@100. maxP], fontsize, Blue]];
Print[Style[StringForm["\[FilledSquare] Generated models: ``   Selected models: `` (``%)", Anumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
Print[Style[StringForm["\[FilledDiamond] Inning ``. Complexity: ``  Error: ``  Number of Selected models: `` (``%)", 0, NF@Cnumb,NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
];


While[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100,


Which[
Pnumb/Anumb<minP,

Cnumb=Cnumb+deltaC;
Lnumb=Lnumb+deltaL;
Smodels=SelectModels[archivedModels,QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

Pnumb/Anumb>minP,

Cnumb=Cnumb-deltaC;
Lnumb=Lnumb-deltaL;
Smodels=SelectModels[archivedModels,QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

True,
ErrorMessage["Unexpected limiting error"]
];

Print[Style[StringForm["\[FilledDiamond] Inning ``.  Complexity: ``  Error: ``  Number of Selected models: `` (``%)", j, NF@Cnumb, NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
j++

];

PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Final QuatiliyBox levels are {``, ``} in this first turn.", NF@Cnumb, NF@Lnumb],Bold,fontsize, darkgreen]];
PB[];


(* The final 1st interesting model *)

If[labeling===1, PB[]];

finalquality={Cnumb, Lnumb};
Global`FINALQUALITY=finalquality;
interestingModels =Smodels;
imodelcount=Length[interestingModels];

Global`INTERESTINGMODELS=interestingModels;Global`IMODELCOUNT=imodelcount;
AppendTo[interestingbag, interestingModels];
Global`INTERESTINGBAG=interestingbag;

interestimage=ParetoFrontPlot[interestingModels, 
PlotLabel->LabelForm[{Length@interestingModels,"models are judged as interesting"},Joined->True],
ImageSize-> size1, DensityPlot-> False
];


Print[Style[StringForm["\[FilledDiamond] `` interesting models were selected", imodelcount],Bold, fontsize, Red]];
Print[Style[StringForm[ "\t\[EmptyDiamond] Quatiliy Box values are ``.", finalquality],Bold,fontsize]];
Print[interestimage],




True, ErrorMessage["Unexpeted qualityflag error"];Goto["qualityagain"]


];

(* Quality Box setting program ends *)







Print[Style[StringForm[ "\t\[EmptyDiamond] Quatiliy Box values are ``.", quality],Bold,fontsize]];

interestingModels = SelectModels[archivedModels,QualityBox->quality];imodelcount=Length[interestingModels];
interestimage=ParetoFrontPlot[interestingModels, 
PlotLabel->LabelForm[{Length@interestingModels,"models are judged as interesting"},Joined->True],
ImageSize-> size1, DensityPlot-> False
];
Print[interestimage];
Print[Style[StringForm["\[FilledSquare] `` interesting models were selected", imodelcount],Bold, fontsize, Blue]];

Print["                             "];
Global`INTERESTINGMODELS=interestingModels;Global`IMODELCOUNT=imodelcount;



Print[Style["\[FilledDiamond] 2. Model Dimensionality Assessment (How many variables are required)",Bold,fontsize]];
Print["                             "];

dimensionimage=ModelDimensionalityTable@interestingModels;
Print[dimensionimage];
Print["                             "];



Print[Style["\[FilledDiamond] 3. Variable Combinations (Ranked popularity of variable combinations)",Bold,fontsize]];
Print["                             "];

combinationiamge=VariableCombinationTable@interestingModels;
Print[combinationiamge];
Print["                             "];



Print[Style["\[FilledDiamond] 4-1. Variable Presence (Popularity of each variable) in the ORIGINAL order",Bold,fontsize]];
Print["                             "];

presenceimage=VariablePresenceChart[interestingModels, ImageSize-> size1];
Print[presenceimage];

Print["                             "];


Print[Style["\[FilledDiamond] 4-2. Variable Presence (Popularity of each variable) SORTED by frequencies",Bold,fontsize]];
presenceimage=VariablePresenceChart[interestingModels, 
VariablesToPlot-> Flatten@{DriverVariables@interestingModels,
DeleteCases[ DataVariables@First@interestingModels,
Alternatives@@DriverVariables[interestingModels]
]
},
ImageSize-> size1];
Print[presenceimage];
Print["                             "];

Print[Style[StringForm["\[FilledSquare] Important Variables with Significant Level more than 5%"], Bold, 16, Red]];
importantvariables=VariablePresence[interestingModels, QualityBox-> quality,SignificanceLevel-> 0.05,
PresenceMetric-> "Percent"];Global`IMPORTANTVARIABLES=importantvariables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Variables used in more than 5% models and their Frequencies of Use(percent)",Bold,fontsize]];
Print["                             "];
Print@Column[importantvariables, ItemSize-> 14, Frame-> True];
Print["                             "];
Print[Style["\t\[EmptyDiamond] These Variables in the order of usage frequencies are expressed as IMPORTANTVARIABLES",Bold,fontsize]];
Print["                             "];
Print@importantvariables;
Print["                             "];


Print[Style[StringForm["\[FilledSquare] Useful Variables which are used at least once"], Bold, 16, Red]];
usefulvariables=VariablePresence[interestingModels, QualityBox-> quality,SignificanceLevel-> 0.0,
PresenceMetric-> "Percent"];Global`USEFULVARIABLES=usefulvariables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Variables used at least once and their Frequencies of Use(percent)",Bold,fontsize]];
Print["                             "];
Print@Column[usefulvariables, ItemSize-> 14, Frame-> True];
Print["                             "];
Print[Style["\t\[EmptyDiamond] These Variables in the order of usage frequencies are expressed as USEFULVARIABLES",Bold,fontsize]];
Print["                             "];
Print@usefulvariables;
Print["                             "];
Print[Style[StringForm["\[EmptyDiamond] Usefulvariables are exported in the Excel as UsefulVariables."]]];
ExportExcel2[usefulvariables, Numberings@{"Percent","Variable"}, "UsefulVariables"];
Print["                             "];

Print[Style["\[FilledDiamond] 4-3. Variable Presence (Popularity of each variable) FOCUSED on important variables",Bold,fontsize]];
Print["                             "];


presenceimage=VariablePresenceChart[interestingModels, 
VariablesToPlot-> DriverVariables@interestingModels,
ImageSize-> size1];
Print[presenceimage];
Print["                             "];

If[ Length[initems]>= 100,
Print[Style["\[FilledDiamond] 4-4. Variable Presence (Popularity of each variable) FOCUED on TOP 15 variables",Bold,fontsize]];
Print["                             "];


presenceimage=VariablePresenceChart[interestingModels, 
VariablesToPlot-> DriverVariables[interestingModels,15],
ImageSize-> size1];
Print[presenceimage];
Print["                             "];
];



Print[Style["\[FilledDiamond] 5. Metavariable Distribution (Popularity of metavariables)",Bold,fontsize]];
Print["                             "];

metavariableimage=MetaVariableDistributionTable[interestingModels, QualityBox -> quality, ImageSize-> size1];
Print[metavariableimage];
Print["                             "];
Print["                             "];



(* ------- PHASE 8 -------Selecting Phenotype---------------------------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 8  Selecting the best Ensemble and its Phenotype",Bold,Purple,fontsize2]];
Label["phase6"];phasenumber=7;  Global`PHASENUMBER=8;
Print["                             "];



Print[Style["\[FilledDiamond] 1. Defining Ensembles", Bold, fontsize]];
anEnsemble = CreateModelEnsemble[interestingModels,TrainData];
ensembleimage=ModelSelectionReport[anEnsemble, ImageSize-> size1];
Print[ensembleimage];
Print["                             "];
Global`ANENSEMBLE=anEnsemble;



Print[Style["\[FilledDiamond] 2. Ensembles in ParetoFront", Bold, fontsize]];
ensembleinpareto=ParetoFrontContextPlot[anEnsemble,interestingModels, GridLines-> Automatic,ImageSize-> size1];
Print[ensembleinpareto];
Print["                             "];



Print[Style["\[FilledDiamond] 3. Prediction Performance", Bold, fontsize]];
outlierIndices = DataOutlierIndices[anEnsemble,TrainData];
Print[Style[StringForm["   \[EmptyDiamond] Ensemble Prediction Plot"], Bold, 15]];
performanceimage=EnsemblePredictionPlot[anEnsemble,TrainData,
DataOutliers->outlierIndices,
ToolTipLimit->300,
 ImageSize-> size1];
Print[performanceimage];
Print["                             "];
Print[Style[StringForm["   \[EmptyDiamond] Model Prediction Comparison Plot"], Bold, 15]];
comparisonimage=ModelPredictionComparisonPlot[anEnsemble,TrainData,
SortBy->"Observed",
DataOutliers->outlierIndices,
ImageSize-> size1
];
Print[comparisonimage];
Print["                             "];



Print[Style["\[FilledDiamond] 4. Niche Models", Bold, fontsize]];
nicheimage=GridTable[
{Length@#,ModelDimensionality@First@#,ModelSubspace@First@#,Sequence@@(VariablePresence[First@#,MaxNumberOfAutoSymbols->9,PresenceMetric->"Presence"] /. {0->"",1->"X"})}&/@Sort[NicheModels[
interestingModels,
NicheBy->ModelVariables
],Length@#1>Length@#2&],
TableHeadings-> {Automatic,{"Rank","# models","# variables","subspace",Sequence@@ToString/@AutoSymbolList["x",13]}},
Label->"Subspace Distribution in Selected Models"
];Global`NICHEIMAGE=nicheimage;
(* Print[nicheimage]; *)  Print["The output of the image is omitted. The image is expressed as NICHEIMAGE."];
Print@nicheimage;
Print["                             "];



Print[Style["\[FilledDiamond] 5. Selected Phenotype", Bold, fontsize]];
phenotype=ModelPhenotype[anEnsemble];
Global`PHENOTYPE=phenotype;
createdModel=CreateStandaloneModel[ anEnsemble];
Global`CREATEDMODEL=createdModel;
estimatedModel=createdModel;
Global`ESTIMATEDMODEL=estimatedModel;
traditionalForm=TraditionalForm[phenotype];
Global`TRADITIONALFORM=TraditionalForm[phenotype];

Print[DisplayForm[FrameBox[traditionalForm]]];
Print["                             "];


(* estimatedModel=Function@@{allNames, phenotype}; *)


Print[Style["\[FilledSquare] Created standalone model is expressed as PHENOTYE, created function as CREATEDMODEL.", fontsize, Bold]];
Print[Style["\t(Predicted values are calculated with CREATEDMODEL @@@ ALLTRAININGDATA)"]];
Print["                             "];

Print[Style[StringForm["\[FilledDiamond] The created model (expressed as CREATEDMODEL) is as follows."], 15, Bold]];
Print["                               "];
Print[createdModel];
Print["                               "];

Print[Style["\[FilledDiamond] 6. Test of creaed Models with ANOVA", Bold, fontsize]];

Print["                               "];
Print[Style["\[EmptyDiamond] Test Results for each Approximate Function with ANOVA", Bold, 14]];
Print["                               "];

eachfunction=GridTable[{Sequence@@ModelQuality@#,Column@ModelVariables@#,GridTable@ModelBasisSet@#, ModelExpression@#,ConvertToFittedModel[#,TrainData]["ANOVATable"]}&/@anEnsemble[[2]],
TableHeadings->{Automatic,{"Complexity", "1-\!\(\*SuperscriptBox[\(R\), \(2\)]\)","Variables","ModelBasisSet",  "Model Expression", "ANOVA Table"}}
];
Print[eachfunction];
Global`EACHFUNCTION=eachfunction;

Print["                               "];
Print[Style["\[EmptyDiamond] Test Result for the Whole Approximate Function with ANOVA", Bold, 14]];
Print["                               "];

wholefunction=ConvertToFittedModel[anEnsemble,TrainData]["ANOVATable"];
Print[wholefunction];
Global`WHOLEFUNCTION=wholefunction;

Print["                               "];
Print["                               "];






(* ------- PHASE 9 ----------------------Verification with Original Training Data ------------------------------------------- *) 


Print[Style["\t\[EmptySquare]\:3000Phase 9  Verification of Created Model with the WHOLE Original data",Bold,Purple,fontsize2]];
Label["phase6"];phasenumber=9;  Global`PHASENUMBER=9;
Print["                             "];



Print[Style["\[FilledDiamond] 1. Comparison Between Predicted values(Red) and Observed ones(Blue) using the original data", fontsize, Bold]];


nullflag=Position[ allTrainingData, Null];Global`NULLFLAG=nullflag;
If[
Length@nullflag>= 1,

Print["                             "];
{alltrainingsIP, allnamesIP}=nullchecker2[allTrainingData, allNames];
Global`ALLTRAININGSIP=alltrainingsIP; Global`ALLNAMESIP=allnamesIP;
allTrainingData=alltrainingsIP; allNames=allnamesIP;
Print[Style[StringForm["\[FilledDiamond] The `` empty cells in the training data were replaced with appropriate numbers.", Length@nullflag]]];
Print["                            "];

];


(* Report of Hyper-parameter settings in Hold-out Method *)

PB[];
Print[Style["\[FilledSquare] Report of Hyper-parameter Settings with No Validation", Bold, fontsize, Blue]];
PB[];

Print[Style[StringForm["1. TimeConstraint: ``min, N of Evolutions: ``", timeconstraint, evolutions],fontsize, Bold]];
Print[Style[StringForm["2. Evolution Strategy: ``, Selection Strategy: ``, RobustModels: ``", evolutionStrategyHO, selectionStrategyHO, robustModelHO],fontsize, Bold]];
Print[Style[StringForm["3. Computation time for all evolutions: ``", totaltime],fontsize]];
Print[Style[StringForm["4. Initial QuatlityBox values: Comlexity < ``, Error < ``", quality[[1]], quality[[2]] ],fontsize]];
Print[Style[StringForm["5. Subset size for model evaluation: ``%", NF@subsetsize ],fontsize]];
Print["                               "];



estimatedValues=createdModel@@@allTrainingData;
observedValues=criterion;
Global`ESTIMATEVALUES=estimatedValues;Global`OBSERVEDVALUES=observedValues;
pairs=MakeTwin[ estimatedValues, observedValues];Global`PAIRS=pairs;
pairs1=realchecker[pairs];Global`PAIRS1=pairs1;
pairs2=Sort[ pairs1, #1[[1]]<= #2[[1]]&];
Global`PAIRS=pairs; Global`PAIRS2=pairs2;

cstatres=cStatistics[pairs];Global`CSTATRES=cstatres;
cstat=cstatres[[1]];Global`CSTAT=cstat;

xaxes=Table[i,{i,1,Length[pairs2]}];
Global`XAXES=xaxes;

poarrows=Map[ {Arrowheads[Medium],Thickness[Medium],Brown,Arrow[{{#[[1]], #[[2,1]]}, {#[[1]], #[[2,2]]}}]}&, MakeTwin[ xaxes, pairs2]];
Global`POARROWS=poarrows;
midline={Green,Line[{{0, cutoff}, {NMax[xaxes], cutoff}}]};

judgement=SubsetQ[ {0,0.,1,1.,0`,1`},Union[criterion]]&&MemberQ[ pairs2[[All,2]],0]&&MemberQ[ pairs2[[All,2]],1];
Global`JUDGEMENT=judgement;

comparedimage=ListPlot[ {MakeTwin[ xaxes, pairs2[[All,1]]], MakeTwin[ xaxes, pairs2[[All,2]]]}, ImageSize-> 700,
PlotStyle-> {Red, Blue},PlotRange-> All, Epilog ->If[ judgement===True,{poarrows, midline}, poarrows]];
Print["                                  "];
Print[comparedimage];
Global`COMPAREDIMAGE=comparedimage;

Which[ judgement===True,

ShowConfusionMatrix2[pairs,cutoff];

Print["                                  "],


judgement===False,

rsquare=DeterminationCoefficient[observedValues, estimatedValues];Global`RSQUARE=rsquare;
Print["                                  "];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b Original Data \:306e\:4e88\:6e2c\:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[rsquare],NF[100. rsquare]], fontsize, Bold]];
Print["                                  "]
];




Print[Style["\[FilledDiamond] 2. Training Data File with Predicted values and Observed values", fontsize, Bold]];
Print["                             "];

joineddata=Fuse[ Map[ Drop[#,-1]&,allTrainingData], pairs];
joineditems=Numberings[Join[ Drop[allNames,-1], {"Predicted Value"}, {Last[allNames]}]];
Global`JOINEDDATA=joineddata;
Global`JOINEDITEMS=joineditems;
joinedname=StringJoin["BuildModelData", PresentTime, ".xlsx"];
Export[joinedname, JoinDataItems[joineddata, joineditems] ];
Print[Style[StringForm["\[EmptySquare] The ANALYZED training data with PREDICTED values and OBSERVED targeted values are exported as ``.", joinedname], 15, Bold]];
Print[Style[StringForm["\[EmptyDiamond] The ANALYZED training data with PREDICTED values and OBSERVED targeted values are expressed as JOINEDDATA and JOINEDITEMS."], 15]];
Print["                               "];
Print["                               "];





(* ------- PHASE 10 ----------------------Verification with Test Data ---------------------------------------------- *) 

Which[
valimeth===0,

(* No verificatiopn *)
Print[Style[StringForm["\t\[EmptySquare]\:3000Phase 10  Verification of Created Model with test data"],Bold,Purple,fontsize2]];
Label["phase10"];phasenumber=10;  Global`PHASENUMBER=10;
Print["                                                           "];
Print[Style["\[FilledDiamond] This process is OMITTED>", fontsize, Bold]];

Print["                                                           "];
Print["                                                           "],


valimeth===1,

(* Holt out method *)
Print[Style[StringForm["\t\[EmptySquare]\:3000Phase 10  Verification of Created Model with Test Data by Hold-out method (test size =``)", splitratio],Bold,Purple,fontsize2]];
Label["phase10"];phasenumber=10;  Global`PHASENUMBER=10;
Print["                                                           "];


(* Hold-out method *)
Print[Style["\[FilledDiamond] 1. Comparison Between Predicted values(Red) and Observed ones(Blue) using Test data", fontsize, Bold]];


nullflag=Position[ TestData, Null];Global`NULLFLAG=nullflag;
If[
Length@nullflag>= 1,

Print["                             "];
{allTestIP, allnamesIP}=nullchecker2[TestData, allNames];
Global`ALLTRAININGSIP=allTestIP; Global`ALLNAMESIP=allnamesIP;
TestData=allTestIP; allNames=allnamesIP;
Print[Style[StringForm["\[FilledDiamond] The `` empty cells in the training data were replaced with appropriate numbers.", Length@nullflag]]];
Print["                            "];

];


Print["                                                           "];

Print[Style[StringForm["\t\[EmptyDiamond] Total N of Original data = ``", Length@allTrainingData], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] N of Training data = ``", Length@TrainData], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] N of Test data = ``", Length@TestData], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] Split ratio = ``", splitratio], fontsize, Bold]];

Print["                                                                    "];

(* Report of Hyper-parameter settings in Hold-out Method *)

PB[];
Print[Style["\[FilledSquare] Report of Hyper-parameter Settings in Hold-Out Method", Bold, fontsize, Blue]];
PB[];

Print[Style[StringForm["1. TimeConstraint: ``min, N of Evolutions: ``", timeconstraint, evolutions],fontsize, Bold]];
Print[Style[StringForm["2. Evolution Strategy: ``, Selection Strategy: ``, RobustModels: ``", evolutionStrategyHO, selectionStrategyHO, robustModelHO],fontsize, Bold]];
Print[Style[StringForm["3. Computation time for all evolutions: ``", totaltime]]];
Print[Style[StringForm["4. Initial QuatlityBox values: Comlexity < ``, Error < ``", quality[[1]], quality[[2]] ]]];
Print[Style[StringForm["5. Subset size for model evaluation: ``%", NF@subsetsize ]]];


(* TestData\:3067\:306e\:4e88\:6e2c\:306e\:56f3\:793a *)

estimatedTestValues=createdModel@@@TestData;
observedTestValues=TestData[[All,-1]];
Global`ESTIMATEDTESTVALUES=estimatedTestValues;Global`OBSERVEDTESTVALUES=observedTestValues;

testpairs=MakeTwin[ estimatedTestValues, observedTestValues];Global`TESTPAIRS=testpairs;
testpairs1=realchecker[testpairs];Global`TESTPAIRS1=testpairs1;
testpairs2=Sort[ testpairs1, #1[[1]]<= #2[[1]]&];
Global`TESTPAIRS=testpairs; Global`TESTPAIRS2=testpairs2;

testcstatres=cStatistics[testpairs];Global`TESTCSTATRES=testcstatres;
testcstat=testcstatres[[1]];Global`TESTCSTAT=testcstat;

testxaxes=Table[i,{i,1,Length[testpairs2]}];
Global`TESTXAXES=testxaxes;

testpoarrows=Map[ {Arrowheads[Medium],Thickness[Medium],Brown,Arrow[{{#[[1]], #[[2,1]]}, {#[[1]], #[[2,2]]}}]}&, MakeTwin[ testxaxes, testpairs2]];
Global`TESTPOARROWS=testpoarrows;
testmidline={Green,Line[{{0, cutoff}, {NMax[testxaxes], cutoff}}]};

testjudgement=SubsetQ[ {0,0.,1,1.,0`,1`},Union[observedTestValues]]&&MemberQ[ testpairs2[[All,2]],0]&&MemberQ[ testpairs2[[All,2]],1];
Global`TESTJUDGEMENT=testjudgement;

PB[];
testcomparedimage=ListPlot[ {MakeTwin[ testxaxes, testpairs2[[All,1]]], MakeTwin[ testxaxes, testpairs2[[All,2]]]}, ImageSize-> 700,
PlotStyle-> {Red, Blue},PlotRange-> All, Epilog ->If[ testjudgement===True,{testpoarrows, testmidline}, testpoarrows]];
Print[Style[StringForm["\[EmptySquare] \:691c\:8a3c\:30c7\:30fc\:30bf(Test data: \:5168\:4f53\:306e ``%)\:306b\:5bfe\:3059\:308b\:4e88\:6e2c\:95a2\:6570\:306e\:4e88\:6e2c\:7d50\:679c", NF[100. splitratio]], fontsize+1, Bold, Purple]];
Print["                                  "];
Print[testcomparedimage];
Print["                                  "];
Global`TESTCOMPAREDIMAGE=testcomparedimage;


(* TrainData\:3067\:306e\:4e88\:6e2c\:306e\:56f3\:793a *)

estimatedTrainValues=createdModel@@@TrainData;
observedTrainValues=TrainData[[All,-1]];
Global`ESTIMATEDTRAINVALUES=estimatedTrainValues;Global`OBSERVEDTRAINVALUES=observedTrainValues;

trainpairs=MakeTwin[ estimatedTrainValues, observedTrainValues];Global`TRAINPAIRS=trainpairs;
trainpairs1=realchecker[trainpairs];Global`TRAINPAIRS1=trainpairs1;
trainpairs2=Sort[ trainpairs1, #1[[1]]<= #2[[1]]&];
Global`TRAINPAIRS=trainpairs; Global`TRAINPAIRS2=trainpairs2;

traincstatres=cStatistics[trainpairs];Global`TRAINCSTATRES=traincstatres;
traincstat=traincstatres[[1]];Global`TRAINCSTAT=traincstat;

trainxaxes=Table[i,{i,1,Length[trainpairs2]}];
Global`TRAINXAXES=trainxaxes;

trainpoarrows=Map[ {Arrowheads[Medium],Thickness[Medium],Brown,Arrow[{{#[[1]], #[[2,1]]}, {#[[1]], #[[2,2]]}}]}&, MakeTwin[ trainxaxes, trainpairs2]];
Global`TRAINPOARROWS=trainpoarrows;
trainmidline={Green,Line[{{0, cutoff}, {NMax[trainxaxes],cutoff}}]};
 
trainjudgement=SubsetQ[ {0,0.,1,1.,0`,1`},Union[observedTrainValues]]&&MemberQ[ trainpairs2[[All,2]],0]&&MemberQ[ trainpairs2[[All,2]],1];
Global`TRAINJUDGEMENT=trainjudgement;

traincomparedimage=ListPlot[ {MakeTwin[ trainxaxes, trainpairs2[[All,1]]], MakeTwin[ trainxaxes, trainpairs2[[All,2]]]}, ImageSize-> 700,
PlotStyle-> {Red, Blue},PlotRange-> All, Epilog ->If[ trainjudgement===True,{trainpoarrows, trainmidline}, trainpoarrows]];
Print[Style[StringForm["\[EmptySquare] \:5b66\:7fd2\:30c7\:30fc\:30bf(Training data: \:5168\:4f53\:306e ``%)\:306b\:5bfe\:3059\:308b\:4e88\:6e2c\:95a2\:6570\:306e\:4e88\:6e2c\:7d50\:679c", NF[100. (1-splitratio)]], fontsize, Bold, Purple]];
Print["                                  "];
Print[traincomparedimage];
Print["                                  "];
Print["                                  "];
Global`TRAINCOMPAREDIMAGE=traincomparedimage;


Which[ testjudgement===True,

(* lesstestpairs=FilterElement[ testpairs2, 1, #<0.5&];
moretestpairs=FilterElement[ testpairs2, 1, #\[GreaterEqual] 0.5&];
Global`LESSTESTPAIRS=lesstestpairs; Global`MORETESTPAIRS=moretestpairs;
testLL=Count[lesstestpairs[[All,2]],0] ; testMM=Count[moretestpairs[[All,2]], 1];
testLM=Count[lesstestpairs[[All,2]],1];testML=Count[moretestpairs[[All,2]], 0];
Global`TESTLLCOUNT=testLL; Global`TESTMMCOUNT=testMM;
Global`TESTLMCOUNT=testLM; Global`TESTMLCOUNT=testML;
testbprights=testLL+testMM;
testbpwrongs=testLM+testML ;
Global`TESTBPRIGHTS=testbprights; Global`TESTBPWRONGS=testbpwrongs;
testrightrate=100.testbprights/(testbprights+testbpwrongs);
Global`TESTRIGHTRATE=rightrate;Count[moretestpairs[[All,2]], 1];
testrwdata={{testMM ,testML, testMM+testML},{testLM,testLL,testLM+testLL}, {testMM+testLM, testML+testLL, testMM+testML+testLM+testLL}};
Global`TESTRAWDATA=testrwdata; *)

Print["                                  "];
Print[Style[StringForm["\[EmptySquare] \:691c\:8a3c\:30c7\:30fc\:30bf(Test data: \:5168\:4f53\:306e ``%)\:306b\:304a\:3051\:308b\:4e88\:6e2c\:7d50\:679c", NF[100. splitratio]], fontsize2, Bold,Red]];

(* Print[Style[TableForm[ testrwdata, TableHeadings\[Rule]{{"\:4e88\:6e2c\:5024\:ff11","\:4e88\:6e2c\:5024\:ff10","\:5408\:8a08"},{"\:89b3\:6e2c\:5024\:ff11","\:89b3\:6e2c\:5024\:ff10", "\:5408\:8a08"}}],Bold,fontsize]];
Print[Style[StringForm["\t\[EmptyDiamond] \:ff11\:306e\:7684\:4e2d\:7387 ``%\:3001\:ff10\:306e\:7684\:4e2d\:7387 ``%", NF[100. testMM/(testMM+testML)],NF[100.testLL/(testLL+testLM)]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:ff11\:306e\:611f\:5ea6 ``%\:3001 \:ff10 \:306e\:7279\:7570\:5ea6 ``%", NF[100. testMM/(testMM+testLM)],NF[100.testLL/(testLL+testML)]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:691c\:8a3c\:30c7\:30fc\:30bf\:306b\:304a\:3051\:308b\:5168\:7684\:4e2d\:7387(accuracy) ``%", NF[testrightrate]], fontsize,Bold]];
Print["                                  "];
Print[Style[StringForm["\t\[FilledDiamond] c-Statistics = `` (`` %)",NF[ testcstat], NF[100*testcstat]], Bold, fontsize]]; *)

ShowConfusionMatrix2[testpairs, cutoff];

Print["                                  "],

testjudgement===False,

testrsquare=DeterminationCoefficient[observedTestValues, estimatedTestValues];Global`TESTRSQUARE=testrsquare;
trainrsquare=DeterminationCoefficient[observedTestValues, estimatedTrainValues];Global`TRAINRSQUARE=trainrsquare;
Print["                                  "];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b TestData \:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[testrsquare],NF[100. testrsquare]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b TrainData \:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[trainrsquare],NF[100. trainrsquare]], fontsize, Bold]];
Print["                                  "];
Print["                                  "]
];



Which[ trainjudgement===True,

(* lesstrainpairs=FilterElement[ trainpairs2, 1, #<0.5&];
moretrainpairs=FilterElement[ trainpairs2, 1, #\[GreaterEqual] 0.5&];
Global`LESSTRAINPAIRS=lesstrainpairs; Global`MORETRAINPAIRS=moretrainpairs;
trainLL=Count[lesstrainpairs[[All,2]],0] ; trainMM=Count[moretrainpairs[[All,2]], 1];
trainLM=Count[lesstrainpairs[[All,2]],1];trainML=Count[moretrainpairs[[All,2]], 0];
Global`TRAINLLCOUNT=trainLL; Global`TRAINMMCOUNT=trainMM;
Global`TRAINLMCOUNT=trainLM; Global`TRAINMLCOUNT=trainML;
trainbprights=trainLL+trainMM;
trainbpwrongs=trainLM+trainML ;
Global`TRAINBPRIGHTS=trainbprights; Global`TRAINBPWRONGS=trainbpwrongs;
trainrightrate=100.trainbprights/(trainbprights+trainbpwrongs);
Global`TRAINRIGHTRATE=rightrate;Count[moretrainpairs[[All,2]], 1];
trainrwdata={{trainMM ,trainML, trainMM+trainML},{trainLM,trainLL,trainLM+trainLL}, {trainMM+trainLM, trainML+trainLL, trainMM+trainML+trainLM+trainLL}};
Global`TRAINRAWDATA=trainrwdata; *)

Print["                                  "];
Print[Style[StringForm["\[EmptySquare] \:5b66\:7fd2\:30c7\:30fc\:30bf(Training data: \:5168\:4f53\:306e ``%)\:306b\:304a\:3051\:308b\:4e88\:6e2c\:7d50\:679c", NF[100.(1- splitratio)]], fontsize, Bold]];

(* Print[Style[TableForm[ trainrwdata, TableHeadings\[Rule]{{"\:4e88\:6e2c\:5024\:ff11","\:4e88\:6e2c\:5024\:ff10","\:5408\:8a08"},{"\:89b3\:6e2c\:5024\:ff11","\:89b3\:6e2c\:5024\:ff10", "\:5408\:8a08"}}],Bold,fontsize]];
Print[Style[StringForm["\t\[EmptyDiamond] \:ff11\:306e\:7684\:4e2d\:7387 ``%\:3001\:ff10\:306e\:7684\:4e2d\:7387 ``%", NF[100. trainMM/(trainMM+trainML)],NF[100.trainLL/(trainLL+trainLM)]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:ff11\:306e\:611f\:5ea6 ``%\:3001 \:ff10 \:306e\:7279\:7570\:5ea6 ``%", NF[100. trainMM/(trainMM+trainLM)],NF[100.trainLL/(trainLL+trainML)]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:5b66\:7fd2\:30c7\:30fc\:30bf\:306b\:5bfe\:3059\:308b\:5168\:7684\:4e2d\:7387(accuracy) ``%", NF[trainrightrate]], fontsize,Bold]];
Print["                                  "];
Print[Style[StringForm["\t\[FilledDiamond] c-Statistics = `` (`` %)",NF[ traincstat], NF[100*traincstat]], Bold, fontsize]]; *)

ShowConfusionMatrix2[trainpairs,cutoff];

Print["                                  "],

trainjudgement===False,

testrsquare=DeterminationCoefficient[observedTestValues, estimatedTestValues];Global`TESTRSQUARE=testrsquare;
trainrsquare=DeterminationCoefficient[observedTrainValues, estimatedTrainValues];Global`TRAINRSQUARE=trainrsquare;
Print["                                  "];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b TestData \:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[testrsquare],NF[100. testrsquare]], fontsize, Bold]];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b TrainData \:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[trainrsquare],NF[100. trainrsquare]], fontsize, Bold]];
Print["                                  "];
Print["                                  "]
],





valimeth===2,

(* Cross Validation method *)
Null,


True,
ErrorMessage["Unexpected test error"];Goto["endingbuildmodel"]



];Goto["endingbuildmodel"];
PB[];







(* ------- PHASE LOO ---------------------- Leave-One-Out cross validation ---------------------------------------------- *) 


Label["LOOmethod"];

Print[Style["\t\[EmptySquare]\:3000Phase 12  Leave-One-Out cross validation",Bold,Purple,fontsize2]];
Label["phase6"];phasenumber=12;  Global`PHASENUMBER=12;
Print["                             "];


Print[Style[StringForm["\[FilledSquare] The LEAVE-ONE-OUT Cross-Validation method was chosen.\n\n\[EmptyDiamond] This method is the same as K-fold cross-validation when the K is equal to the sample size.\nThe number of test data is one, and the rest is used as the training data.\nThen the validation is repeated K times.\nThe accuracy is figured out with the K results."],Bold,fontsize]];





nullflag=Position[ allTrainingData, Null];Global`NULLFLAG=nullflag;
If[
Length@nullflag>= 1,

Print["                             "];
Print[Style[StringForm["\[FilledDiamond] There must be NO EMPTY CELL in the LEAVE-ONE-OUT cross validation."],15, Bold, Red]];
Print["                             "];

{allTrainingData, allNames}=nullchecker2[allTrainingData, allNames];


Print[Style[StringForm["\[FilledDiamond] The `` empty cells in the training data were replaced with appropriate numbers.", Length@nullflag],15,Bold]];
Print["                            "];

];
Global`ALLTRAININGDATA=allTrainingData;
Global`ALLNAMES=allNames;
PB[];



Print[Style[StringForm["\[FilledSquare] Preliminary Analysis: Correlation Coefficient between Explanatory Vairables and Target Variable"], fontsize, Bold, Blue]];
Print["                            "];

Print[Style["\[FilledDiamond] 1. Correlation Chart of Data in the original order\n\t(BLUE: Positive correlation,  RED: Negative correlation)",fontsize,Bold]];
correlationchartloo0=Quiet@CorrelationChart[
allTrainingData,
DataVariableLabels-> allNames,
ImageSize-> size2
];
Print[correlationchartloo0];Global`CORRELATIONCHARTLOO0=correlationchartloo0;
Print["                             "];

Print[Style["\[FilledDiamond] 2. Correlation Chart of Data in the correlation coefficient order\n\t(BLUE: Positive correlation,  RED: Negative correlation)",fontsize,Bold]];
correlationchartloo=Quiet@CorrelationChart[
allTrainingData,
DataVariableLabels-> allNames,
SortBy-> Correlation,
ImageSize-> size2
];
Print[correlationchartloo];Global`CORRELATIONCHARTLOO=correlationchartloo;


Print["                             "];

Print[Style[StringForm["\[EmptySquare] Ordered Correlation Coefficients of All variables"],Bold, fontsize, Blue]];
coeffvalues=NGetCorrelation[allTrainingData, Numberings@allNames];
Global`COEFFVALUES=coeffvalues;
coefftable=Sort[MakeTwin[CutLast@allNames, coeffvalues], #1[[2]]>= #2[[2]]&];
Global`COEFFTABLE=coefftable;
numbcoefftable=Fuse[Range[Length@CutLast@allNames], coefftable];
Global`NUMBCOEFFTABLE=numbcoefftable;
Print@TextGrid[numbcoefftable, Frame-> All];
Print[Style[StringForm["\t\[EmptyDiamond] Raw correlation coefficients are expressed as COEFFVALUES. Correlation Coeffcienets Tables is expressed as COEFFTABLE and NUMBCOEFFTABLE."],Bold,fontsize]];


Print["                             "];


Print["                             "];
Print["                             "];


Print[Style[StringForm["\[FilledSquare] Setting HypterParameters in Leave-One-Out Cross Validation"], fontsize, Bold, Blue]];
Print["                             "];


(* \:30b5\:30f3\:30d7\:30eb\:30b5\:30a4\:30ba = LOO-CV\:306e\:6570 *)
loolen=newdim[[1]];
Global`LOOLEN=loolen;


(* \:30d7\:30ed\:30bb\:30b9\:540d *)
Label["projectnameagain2"];
projectname0=InputString[StringForm["Type in a PROJECT NAME.\nPress OK to use Predict`` as a project name.\nType end to quit.", criterionName], WindowMargins-> windowaddress1];
Which[
projectname0==="quit"||projectname0==="end", Goto["endingbuildmodel"],
projectname0==="", projectname=StringJoin["Predict", criterionName],
StringQ[projectname0],projectname=projectname0,
True, ErrorMessage["Type in, again."];Goto["projectnameagain2"]
];
Print[Style[StringForm["\[FilledDiamond] The project name is ``.", projectname], Bold, fontsize]];
Print["                                 "];


(* \:30d7\:30ed\:30bb\:30b9\:306e\:6642\:9593\:8a2d\:5b9a *)
Print[Style["\[EmptySquare] Leave-One-Out cross validation starts.", Bold, fontsize]];
Print["                                                     "];
Label["timeconstraintagain2"];
timeconstraint0=InputString["Type in a TIME in MINUTES for time constraint per one validation, like 3 for 3 minutes calculation for each validation.\n\nPress OK to set it as 0.5min (30 seconds).\nType end to quit.", WindowMargins-> windowaddress1];
timeconstraint=ToExpression[timeconstraint0];
Print[Style[StringForm["\[FilledSquare] The Setting for Symbolic Regression"], Bold, fontsize]];
Which[
timeconstraint0==="quit"||timeconstraint0==="end", Goto["endingbuildmodel"],
timeconstraint0==="", timeconstraint=0.5,
NumberQ[timeconstraint]&&Positive[timeconstraint], Null,
True, ErrorMessage["Type in, again."];Goto["timeconstraintagain2"]
];
Print[Style[StringForm["\[EmptyDiamond] The time constraint is `` min for one validation.", timeconstraint],Red, Bold,fontsize]];


(* \:30d7\:30ed\:30bb\:30b9\:306e\:672c\:6570\:8a2d\:5b9a *)
Label["evolutionsagain2"];
evolutions0=InputString["Type in an Integer of independent evolutions, like 4, 8 or 12.\n\nPress OK to set it as 4 in each validation.\nType end to quit.", WindowMargins-> windowaddress1];
evolutions=ToExpression[evolutions0];
Which[
evolutions0==="end"||evolutions0==="quit", Goto["endingbuildmodel"],
evolutions0==="", evolutions=4,
IntegerQ[evolutions]&&1<= evolutions<= 64, Null,
evolutions>64, judge1=InputString[StringForm["The tyed-in integer is ``.\nIf it's correct, press OK.\nIf it's wring, type n to chnage it.\nType end to quit", evolutions]];
Which[judge1==="", Null, judge1==="n", Goto["evolutionsagain"], judge1==="end"||judge1==="quit", Goto["endingbuildmodel"],True, Goto["evolutionsagain"]],
True, ErrorMessage["Type in, again."]; Goto["evolutinsagain2"]
];
Print[Style[StringForm["\[EmptyDiamond] A number of independent evolutions per validation is ``.", evolutions], fontsize,Bold]];


(* Fitting Index\:306e\:8a2d\:5b9a *)
Label["fitagain"];
fitindex0=InputString["Type a REAL NUMBER around 0.75 for FITTING INDEX.\n\nPress OK to set FITTING INDEX as 0.80.\nThe BIGGER it goes, the more it UNDER FITS.\nThe SMALLER it goes, the more it OVER FITS.\n\nType end to quit.", WindowMargins-> windowaddress1];
fitindex=ToExpression@fitindex0;

Which[
fitindex0==="", fitindex=0.80,
fitindex0==="quit"||fitindex0==="end"||fitindex0==="stop", Goto["endingbuildmodel"],
NumberQ@fitindex&&(0.3<= fitindex<= 2), Null,
NumberQ@fitindex&&(0.3> fitindex||fitindex> 2), 
	inquiry=InputString["The fitting index might be out of ordinary range.\nPress OK to use it.\nType a NUMBER as an index.\n\nType end to quit."];
	Which[inquiry==="", Null, inquiry==="y", Null,NumberQ@ToExpression@inquiry, fitindex=ToExpression@inquiry, True, Goto["endingbuildmodel"]],
True, Print["Type it, again."];Goto["fitagain"]
];

Print[Style[StringForm["\[EmptyDiamond] Fitting index is ``.", fitindex], fontsize,Bold]];
Print[Style[StringForm["(Fitting index sets the initial values of QualityBox, \n which set the tendency for overfitting or underfitting)"], fontsize,Bold]];(* Fitting index is the gradient of a line to find out the quality box levels. *)
PB[];


(* Quality Box\:306e\:4e2d\:306b\:542b\:307e\:308c\:308b\:95a2\:6570\:306e\:6700\:5c0f\:306e\:5272\:5408 *)
Label["minagain"];
minpercent0=InputString["Type in a REAL NUMBER between 1 and 50 for the MINIMUM PERCENT of functions selected by QALITY BOX.\n\nPress OK to set is as 8%.\nType end to quit.", WindowMargins-> windowaddress1];
minpercent=ToExpression@minpercent0;

Which[
minpercent0==="", minpercent = 8,
minpercent0==="end"||minpercent0==="quit"||minpercent0==="stop", Goto["endingbuildmodel"],
NumberQ@minpercent&&0<minpercent<= 50, Null,
NumberQ@minpercent&&(minpercent<= 0||minpercent> 50),
inquiry=InputString["The minimum perent typed in is out of the expeted range.\nType in a NUMBER between 1 and 50.\nPress OK to set in as 8%.\n\nType end to quit."];
	Which[inquiry==="", minpercent=8, NumberQ@ToExpression@inquiry&&(0<ToExpression@inquiry<50), minpercent=ToExpression@inquiry, True, Goto["endingbuildmodel"]],
True, Print["Type it, again."];Goto["fitagain"]
];

Print[Style[StringForm["\[EmptyDiamond] Minimum percent is ``.", minpercent], fontsize,Bold]];
Print[Style[StringForm["(Minimum percent sets the minimum percentage of functions selected by QualityBox.)"], fontsize,Bold]];(* Fitting index is the gradient of a line to find out the quality box levels. *)
PB[];


(* \:6240\:8981\:6642\:9593 *)
totaltime=MinToHourMinSec[timeconstraint*evolutions];Global`TOTALTIME=totaltime;
fulltime=MinToHourMinSec[timeconstraint*evolutions*loolen];Global`FULLTIME=fulltime;
realtime=MinToHourMinSec[timeconstraint*evolutions*loolen/(corecount/2)];Global`REALTIME=realtime;
Print[Style[StringForm["\[EmptyDiamond] A total time of calculation per validation is ``.", totaltime], Bold, fontsize]];
Print[Style[StringForm["\[EmptyDiamond] A total time of calculation with a basic ``-core computer would be ``.", 2,fulltime], Bold, fontsize]];
Print[Style[StringForm["\[EmptyDiamond] A number of cores in the PRESENT computer is  ``.", corecount], Bold, fontsize]];
PB[];
now0=Now[[1]];Global`NOW0=now0;
Print[Style[StringForm["\[FilledDiamond] Present Time                      : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", now0[[1]],now0[[2]],now0[[3]],now0[[4]],now0[[5]],Round@now0[[6]]],Bold,fontsize]];

future=DateList[AbsoluteTime[Now]+(HourMinSecToSec@realtime)];Global`FUTURE=future;
Print[Style[StringForm["\[FilledDiamond] Finish Time with ``-core computer : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", corecount,future[[1]],future[[2]],future[[3]],future[[4]],future[[5]],Round@future[[6]]],Bold,fontsize]];

Print[Style[StringForm["\[EmptyDiamond] A total time of calculation using the PRESENT ``-core CPU is ``.", corecount,realtime], Bold, fontsize]];

(* future0=DateList[AbsoluteTime[Now]+(HourMinSecToSec@fulltime)];Global`FUTURE0=future0;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 2 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future0[[1]],future0[[2]],future0[[3]],future0[[4]],future0[[5]],Round@future0[[6]]],Bold,fontsize]];
future4=DateList[AbsoluteTime[Now]+(HourMinSecToSec@fulltime)/2];Global`FUTURE4=future4;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 4 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future4[[1]],future4[[2]],future4[[3]],future4[[4]],future4[[5]],Round@future4[[6]]],Bold,fontsize]];
future8=DateList[AbsoluteTime[Now]+(HourMinSecToSec@fulltime)/4];Global`FUTURE8=future8;
Print[Style[StringForm["\[FilledDiamond] Finish Time with 8 cores : ``\:5e74 ``\:6708 ``\:65e5 ``\:6642 ``\:5206 ``\:79d2", future8[[1]],future8[[2]],future8[[3]],future8[[4]],future8[[5]],Round@future8[[6]]],Bold,fontsize]];*)

Print["                             "];

(* Nof0=Count[criterion, 0];
Global`NOF0=Nof0;
Nof1=100. (Length@criterion-Nof0)/Length@criterion;
Global`NOF1=Nof1; *)


(* DataSubsetSize *)
subsetsize=subsetsizefunc[loolen];
Global`DATASUBSETSIZE=subsetsize;
Print[Style[StringForm["\[EmptyDiamond] The DataSubsetSize: ``%", NF@subsetsize], Bold,fontsize]];
PB[];

(* Leave-One-Out method\:306e\:8a2d\:5b9a\:306e\:78ba\:8a8d *)
Label["startagain2"];
start=InputString[StringForm["The setting for Leave-One-Out is as follows.\n1. Project name: ``\n2. Time Constraint per validation: ``\n3. N of Independent Evolutions: ``\n4. Calculation time per validation: ``\n5. Total Raw Calculation time for LOO: ``\n\nPress OK to start the caliculation for Leave-One-Out method.\nType r to reset the setting.\nType end to quit.", projectname, MinToHourMinSec[timeconstraint], evolutions, totaltime,fulltime,WindowMargins-> windowaddress1]];

traindatabag={};
testdatabag={};
modelbag={};
archivedbag={};
interestingbag={};
imagebag={};
countbag={};
presencebag={};
ensemblebag={};
createdbag={};
looresult={};
paretobag={};
traditionalbag={};
allbag={};
partbag={};
pairbag={};
pairbag2={};
variablebox={};
bestbag={};
wrongcases={};
rightcases={};
looscores={};
lootexts={};
errorbag={};
i=1;


Which[
start==="end"||start==="quit", Goto["endingbuildmodel"],
start ==="r", Goto["LOOmethod"],
start==="", Null];


PB[];
Print[Style[StringForm["\[FilledSquare] The 1st cross-validation out of `` turns", loolen], Bold, 21, Purple]];
PB[];
Print[Style[StringForm["\[EmptySquare] The 1st Genetic Programming has started."], Bold, 14]];
DayTime[Bold, 14];
PB[];

partTrainingData[1]=Drop[allTrainingData, {1}];
Global`PARTTRAININGDATA[1]=partTrainingData[1];
partTestData[1]=Take[allTrainingData, {1}];
Global`PARTTESTDATA[1]=partTestData[1];


AppendTo[traindatabag, partTrainingData[1]];
AppendTo[testdatabag, partTestData[1]];


(* The 1st Model Creation *)
paretofront[1]=ParetoFrontPlot[
developedModels[1] = SymbolicRegression[
partTrainingData[1],
DataVariables->allNames,
TargetColumn-> criterionName,

RobustModels-> robustModelLOO,
EvolutionStrategy-> evolutionStrategyLOO,
SelectionStrategy-> selectionStrategyLOO, 
DataSubsetSize-> subsetsize,

(* MonitorModelSearch \[Rule] All,
GenerationMonitor \[Rule] ParetoFront,
CascadeMonitor \[Rule] ParetoFront, 
RunMonitor \[Rule] ParetoFront,
EvolutionMonitor \[Rule] ModelQuality, *)

Quiet->True,
Speak-> False,
ProjectName->projectname,
StoreModelSet->True,
MultiCore->Automatic,
IndependentEvolutions-> evolutions,
TimeConstraint-> timeconstraint*60,
ImageSize-> size1,
ToLowerCase-> False
], PlotRange-> {{0,1.05},Automatic}, DensityPlot-> False
];

Print[Style[StringForm["\[EmptySquare] The 1st Genetic Programming has ended."], Bold, 14]];
DayTime[Bold, 14];
PB[];

Print[Style["\[FilledDiamond] Monitors Plot", fontsize,Bold]];
monitors[1]=MonitorPlot[ImageSize-> 600];
Print@monitors[1];
Global`MONITORS[1]=monitors[1];

AppendTo[modelbag, developedModels[1]];
AppendTo[paretobag, paretofront[1]];

Global`PARETOFRONT[1]=paretofront[1];
Global`DEVELOPEDMODELS[1]=developedModels[1];
modelcount0[1]=Length[developedModels[1]];
Anumb=modelcount0[1];
AppendTo[allbag, modelcount0[1]];
Global`ALLBAG=allbag;

(* QualityBox values *)
quality[1]=findstartpoint[developedModels[1]];
Global`QUALITYBOX[1]=quality[1];


paretofront2[1]=Show[paretofront[1],  PlotRange-> {Automatic,{0,1}}, GridLines-> {Automatic, {0, 1.15, 0.1}}, Epilog-> {Green,AbsolutePointSize[11], Point[quality[1]]}];

archivedModels[1]=developedModels[1];
Clear[Global`ARCHIVEDMODELS];
Global`ARCHIVEDMODELS[1]=archivedModels[1];

AppendTo[archivedbag, archivedModels[1]];



(* The 1st developed models *)

Print[Style["\[FilledDiamond] ParetoFront Plot", fontsize,Bold]];
PB[];
Print[Style[StringForm["\[FilledDiamond] A total of `` models were created with Symbolic Regression though machien learning", modelcount0[1]], Bold,Blue, fontsize]];
Print[paretofront2[1]];
Print["                           "];



(* Interesting models *)

Print[Style["\[FilledDiamond] Setting Quality Box values",Bold,fontsize]];

PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Initial QuatiliyBox levels are {``, ``} in this first turn.", NF@quality[1][[1]], NF@quality[1][[2]]],Bold,fontsize, darkgreen]];
PB[];

limits={minpercent, 55};

Global`LIMITS=limits;minP=limits[[1]]/100.; maxP=limits[[2]]/100.;
Print[Style[StringForm["\t\[EmptyDiamond] Limits of percents for narrowing down models: Min ``%, Max ``%", limits[[1]], NF@limits[[2]]],fontsize, Bold]];
PB[];


(* Selecting the 1st interesting model *)

firstModels[1]=SelectModels[archivedModels[1],QualityBox->quality[1]];
Global`FIRSTMODELS[1]=firstModels[1];
Smodels=firstModels[1];

firstcount[1]=Length@firstModels[1];
Pnumb=firstcount[1];

Cnumb=quality[1][[1]];
Lnumb=quality[1][[2]];
finalquality[1]={Cnumb, Lnumb};
Global`PNUMB=Pnumb; Global`ANUMB=Anumb;Global`MINP=minP; Global`MAXP=maxP;
Global`CNUMB=Cnumb; Global`LNUM =Lnumb;Global`DELTAC=deltaC; Global`DELTAL=deltaL;


j=1;labeling=0;
If[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100, 

labeling=1;
PB[];Print[Style[StringForm["\[FilledSquare] Process for adjusting levels of Quality Box is initiated\n\tmainly to limit the selected models between ``% and ``% of all generated models.", 100. minP, NF@100. maxP], fontsize, Blue]];
Print[Style[StringForm["\[FilledSquare] Generated models: ``   Selected models: `` (``%)", Anumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
Print[Style[StringForm["\[FilledDiamond] Inning ``. Complexity: ``  Error: ``  Number of Selected models: `` (``%)", 0, NF@Cnumb,NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
];


While[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100,


Which[
Pnumb/Anumb<minP,

Cnumb=Cnumb+deltaC;
Lnumb=Lnumb+deltaL;
Smodels=SelectModels[archivedModels[1],QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

Pnumb/Anumb>minP,

Cnumb=Cnumb-deltaC;
Lnumb=Lnumb-deltaL;
Smodels=SelectModels[archivedModels[1],QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

True,
ErrorMessage["Unexpected limiting error"]
];

Print[Style[StringForm["\[FilledDiamond] Inning ``.  Complexity: ``  Error: ``  Number of Selected models: `` (``%)", j, NF@Cnumb, NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
j++

];

PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Final QuatiliyBox levels are {``, ``} in this first turn.", NF@Cnumb, NF@Lnumb],Bold,fontsize, darkgreen]];
PB[];


(* The final 1st interesting model *)

If[labeling===1, PB[]];

finalquality[1]={Cnumb, Lnumb};
Global`FINALQUALITY[1]=finalquality[1];
interestingModels[1] =Smodels;
imodelcount[1]=Length[interestingModels[1]];

Global`INTERESTINGMODELS[1]=interestingModels[1];Global`IMODELCOUNT[1]=imodelcount[1];
AppendTo[interestingbag, interestingModels[1]];
Global`INTERESTINGBAG=interestingbag;

interestimage[1]=ParetoFrontPlot[interestingModels[1], 
PlotLabel->LabelForm[{Length@interestingModels[1],"models are judged as interesting"},Joined->True],
ImageSize-> size1, DensityPlot-> False
];


Print[Style[StringForm["\[FilledDiamond] `` interesting models were selected", imodelcount[1]],Bold, fontsize, Red]];
Print[Style[StringForm[ "\t\[EmptyDiamond] Quatiliy Box values are ``.", finalquality[1]],Bold,fontsize]];
Print[interestimage[1]];

Print["                             "];

AppendTo[countbag, imodelcount[1]];
AppendTo[imagebag, interestimage[1]];
AppendTo[partbag, imodelcount[1]];
Global`PARTBAG=partbag;


Print[Style["\[FilledDiamond] Variable Presence (Popularity of each variable) FOCUSED on important variables",Bold,fontsize]];
Print["                             "];

(* Highly used variables *)

presenceimage[1]=VariablePresenceChart[interestingModels[1], 
VariablesToPlot-> DriverVariables@interestingModels[1],
ImageSize-> size1];

usedvariables[1]=VariablePresence[interestingModels[1], QualityBox-> quality[1], SignificanceLevel-> All, PresenceMetric-> "Percent"];
Global`USEDVARIANBLES[1]=usedvariables[1];
AppendTo[variablebox, usedvariables[1]];
bestfive[1]=topfive[usedvariables[1]];
Global`BESTFIVE[1]=bestfive[1];
AppendTo[bestbag, bestfive[1]];
Print[Style[StringForm["\[FilledDiamond] TOP FIVE HIGHLY USED vairbles: ``", bestfive[1]], fontsize, Red, Bold]];
Print[presenceimage[1]];
Print["                             "];

AppendTo[presencebag, presenceimage[1]];


(* Ensemble learning *)

Print[Style["\[FilledDiamond] Defining Ensembles", Bold, fontsize]];
anEnsemble[1] = CreateModelEnsemble[interestingModels[1] ,partTrainingData[1] ];
ensembleimage[1] =ModelSelectionReport[anEnsemble[1] , ImageSize-> size1];
Print[ensembleimage[1] ];
Print["                             "];
Global`ANENSEMBLE[1] =anEnsemble[1] ;


Print[Style["\[FilledDiamond] Ensembles in ParetoFront", Bold, fontsize]];
ensembleinpareto[1] =ParetoFrontContextPlot[anEnsemble[1] ,interestingModels[1] , ImageSize-> size1];
Print[ensembleinpareto[1] ];
Print["                             "];


AppendTo[ensemblebag, anEnsemble[1]];

(* Created Phenotype *)
(* Print[Style["\[FilledDiamond] Selected Phenotype", Bold, fontsize]]; *)

phenotype[1]=ModelPhenotype[anEnsemble[1]];
Global`PHENOTYPE[1]=phenotype[1];
createdModel[1]=CreateStandaloneModel[ anEnsemble[1]];
Global`CREATEDMODEL[1]=createdModel[1];
estimatedModel[1]=createdModel[1];
Global`ESTIMATEDMODEL[1]=estimatedModel[1];
traditionalForm[1]=TraditionalForm[phenotype[1]];
Global`TRADITIONALFORM[1]=TraditionalForm[phenotype[1]];

(* Print[DisplayForm[FrameBox[traditionalForm[1]]]];
Print["                             "]; *)

AppendTo[createdbag, createdModel[1]];
AppendTo[traditionalbag, traditionalForm[1]];


(* The Result of the 1st cross-validation *)

Print[Style[StringForm["\[FilledSquare] The ``st Validation with Leave-One-Out Method out of `` turns", 1, loolen], fontsize, Bold]];
PB[];

criteria=allTrainingData[[All,Dimensions[allTrainingData][[2]]]];Global`BMCRITERIA=criteria;
labels = Union@criteria; Global`LABELS = labels;


estimatedValue[1]=createdModel[1]@@partTestData[1][[1]];
Global`ESTIMATEDVALUE[1]=estimatedValue[1];
minmax=Union[criteria]; Global`MINMAX=minmax;
(* roundedValue[1]=Which[ estimatedValue[1]\[LessEqual] NMin@minmax, NMin@minmax, 
					 estimatedValue[1]\[GreaterEqual] NMax@minmax, NMax@minmax,
					True,Round[estimatedValue[1]]]; *)
roundedValue[1]=Last@Nearest[labels, estimatedValue[1]]; 
Global`ROUNDEDVALUE[1]=roundedValue[1];
observedValue[1]=criteria[[1]];
Global`OBSERVEDVALUE[1]=observedValue[1];

error[1]=estimatedValue[1]-observedValue[1];
Global`ERROR[1]=error[1];
AppendTo[errorbag, error[1]];
Global`ERRORBAG=errorbag;

pair[1]={ estimatedValue[1], observedValue[1]};Global`PAIR[1]=pair[1];
pair2[1]={roundedValue[1], observedValue[1]};Global`PAIR2[1]=pair2[1];
AppendTo[pairbag, {estimatedValue[1], observedValue[1]}];
AppendTo[pairbag2, {roundedValue[1], observedValue[1]}];

judged[1]=If[ RealVectorQ[pair[1]]&&(pair2[1][[1]]==pair2[1][[2]]), 1, 0, Null];
Global`JUDGED[1]=judged[1];
If[judged[1]===0||judged[1]===Null, AppendTo[wrongcases, Prepend[partTestData[1][[1]], StringJoin[Ordinalize[1], " case"]]]];
If[judged[1]===1, AppendTo[rightcases, Prepend[partTestData[1][[1]], StringJoin[Ordinalize[1], " case"]]]];
Global`PARTESTDATA[1]=partTestData[1];

AppendTo[looresult, judged[1]];
Global`LOORESULT=looresult;


(* Print[Style[StringForm["\[FilledDiamond] The 1st Leave-One-Out cross validation"], Bold, 14]]; *)
PB[];
Print[Style[StringForm["\tThe Estimated value: ``, The Observed value: ``, Error: ``", NF@estimatedValue[1],observedValue[1], NF@error[1]], Bold, 14]];
If[judged[1]===1,
Print[Style[StringForm["\tThe Prediction: Right"], Bold, 15, Blue]],
Print[Style[StringForm["\tThe Prediction: Wrong"], Bold, 15, Red]]];
Print[Style[StringForm["\tAccuracy so far: ``%   (``% completed)", NF[100. Count[looresult,1]/1], NF[100.i/loolen]], Bold,15, RGBColor[0, 0.712917, 0]]];

looscore[i]=100. Count[looresult, 1]/i;
Global`LOOSCORE[i]=looscore[i];
AppendTo[looscores, looscore[i]];
Global`LOOSCORES=looscores;
lootext[i]=Style[Text[ToString@NF@looscore[i], {i, looscore[i]+3}],Bold,12];
Global`LOOTEXT[i]=lootext[i];
lootexts=AppendTo[lootexts, lootext[i]];
Global`LOOTEXTS=lootexts;
loograph=ListLinePlot[looscores, PlotRange-> {{0, i+1}, {0, 105}},PlotMarkers->Automatic, PlotStyle-> {Thick, Blue}, Epilog-> Last@lootexts, AxesLabel->Map[Style[#, Bold, 14]&, {"Turn", "Accuracy(%)"}], TicksStyle->Directive["Label", 14], Ticks-> {Range[i+1], Range[0,100,10]}, GridLines->{Range[i+1], Range[0,100,10]}, ImageSize-> size1];
Global`LOOGRAPH=loograph;
PB[];
remainingtime=rawSecToHourMinSec[AbsoluteTime[future]-AbsoluteTime[Now]];
Print[Style[StringForm["\[EmptyDiamond] Estimated Remaining time: `` hr `` min `` sec",remainingtime[[1]],remainingtime[[2]],remainingtime[[3]] ], Bold,fontsize]];
PB[];
Print[Style[StringForm["\[FilledDiamond] Accuracies until the `` trun in the Leave-One-Out Cross Validation out of `` turns", Ordinalize[i], loolen], Bold, fontsize]];
Print@loograph;

errorminmax={NMin[errorbag],NMax[errorbag]};

errortext[i]=Style[Text[ToString@NF@error[i], {i, error[i]+If[error[i]>0, -0.1, 0.1, 0.1]}],Bold,12];
Global`ERRORTEXT[i]=errortext[i];
errorgraph=ListLinePlot[errorbag, PlotRange-> {{0, i+1}, {If[-1>errorminmax[[1]],errorminmax[[1]],-1,-1],If[1<errorminmax[[2]],errorminmax[[2]],1,1]}},PlotMarkers->Automatic, PlotStyle-> {Thick, Red}, Epilog-> errortext[i], AxesLabel->Map[Style[#, Bold, 14]&, {"Turn", "Error"}], TicksStyle->Directive["Label", 14], Ticks-> {Range[i+1], Range[-2,2,0.5]}, GridLines->{Range[i+1], Range[-2,2,0.5]}, ImageSize-> size1,
Epilog-> {{Thick,Green,Dashed,Line[{{0,-0.5},{i,-0.5}}]}, {Thick,Green,Dashed,Line[{{0,0.5},{i,0.5}}]}}];
PB[];
Print[Style[StringForm["\[FilledDiamond] Error(= Predicted value - Observed value) in the `` Cross Validation", Ordinalize[i]], Bold, fontsize]];
Print[Style[StringForm["\[EmptyDiamond] Average Error is ``\[PlusMinus]`` until the `` turn in the LOO method.", NF@NMean[Map[Abs, errorbag]],NF@NStandardDeviation[Map[Abs, errorbag]],Ordinalize[i]],  Bold, fontsize]];
Print@errorgraph;
PB[];

(* Expeted time when the calculation is completed. *)
starttime=Date[];endtime=DateList[AbsoluteTime[starttime]+timeconstraint*evolutions*loolen*60];
Print[Style[StringForm["\[FilledDiamond] The start time: ``", DateTime[starttime]],14]];
Print[Style[StringForm["\[FilledDiamond] The required total time for `` times calculations: `` min", Round@timeconstraint*evolutions*(loolen-1), loolen-1],14]];
Print[Style[StringForm["\[FilledDiamond] The expected end time: ``", DateTime[endtime]],14]];
Print["                                 "];







(* The i-th Leave-One-Out Cross Validation *)

For[i=2, i<= loolen, i++,

PB[];
PB[];
Print[Style[StringForm["\[FilledSquare] The `` cross-validation out of `` turns",Ordinalize[i], loolen], Bold, 21, Purple]];
PB[];
Print[Style[StringForm["\[EmptySquare] The `` Genetic Programming has started.",Ordinalize[i]], Bold, 14]];
DayTime[Bold, 14];
PB[];

partTrainingData[i]=Drop[allTrainingData, {i}];
Global`PARTTRAININGDATA[i]=partTrainingData[i];
partTestData[i]=Take[allTrainingData, {i}];
Global`PARTTESTDATA[i]=partTestData[i];

AppendTo[traindatabag, partTrainingData[i]];Global`TRAINDATABAG=traindatabag;
AppendTo[testdatabag, partTestData[i]];Global`TESTDATABAG=testdatabag;

(* The i-th Model Creation *)
paretofront[i]=ParetoFrontPlot[
developedModels[i] = SymbolicRegression[
partTrainingData[i],
DataVariables->allNames,
TargetColumn-> criterionName,

RobustModels-> robustModelLOO,
EvolutionStrategy-> evolutionStrategyLOO,
SelectionStrategy-> selectionStrategyLOO, 
DataSubsetSize-> subsetsize,

Quiet->True,
Speak-> False,
ProjectName->projectname,
StoreModelSet->True,
MultiCore->Automatic,
IndependentEvolutions-> evolutions,
TimeConstraint-> timeconstraint*60,
ImageSize-> size1,
ToLowerCase-> False
], PlotRange-> {{0,1.05},Automatic}, DensityPlot-> False
];

Print[Style[StringForm["\[EmptySquare] The ``th Genetic Programming has ended.",i], Bold, 14]];
DayTime[Bold, 14];
PB[];

Print[Style["\[FilledDiamond] Monitors Plot", fontsize,Bold]];
monitors[i]=MonitorPlot[ImageSize-> 600];
Print@monitors[i];
Global`MONITORS[i]=monitors[i];

AppendTo[modelbag, developedModels[i]];
AppendTo[paretobag, paretofront[i]];

Global`PARETOFRONT[i]=paretofront[i];
Global`DEVELOPEDMODELS[i]=developedModels[i];
modelcount0[i]=Length[developedModels[i]];
Anumb=modelcount0[i];
AppendTo[allbag, modelcount0[i]];
Global`ALLBAG=allbag;


(*  The i-th qualitybox values *)

quality[i]=findstartpoint[developedModels[i]];
Global`QUALITYBOX[i]=quality[i];


(* The i-th created model *)

paretofront2[i]=Show[paretofront[i],  PlotRange-> {Automatic,{0,1}}, GridLines->  {Automatic, {0, 1.15, 0.1}}, Epilog-> {Green,AbsolutePointSize[11], Point[quality[i]]}];


Print[Style[StringForm["\[FilledDiamond] `` models were created", modelcount0[i]],Bold, fontsize]];
Print[paretofront2[i]];

archivedModels[i]=developedModels[i];
Global`ARCHIVEDMODELS[i]=archivedModels[i];

AppendTo[archivedbag, archivedModels[i]];
Global`ARCHIVEDBAG=archivedbag;



PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Initial Quatiliy Box levels are {``, ``} in this `` turn.", NF@quality[i][[1]], NF@quality[i][[2]], Ordinalize[i]],Bold,fontsize, darkgreen]];
PB[];


(* The i-th interesting model *)

firstModels[i]=SelectModels[archivedModels[i],QualityBox->quality[i]];
Global`FIRSTMODELS[i]=firstModels[i];
Smodels=firstModels[i];

firstcount[i]=Length@firstModels[i];
Pnumb=firstcount[i];

Cnumb=quality[i][[1]];
Lnumb=quality[i][[2]];
finalquality[i]={Cnumb, Lnumb};
Global`PNUMB=Pnumb; Global`ANUMB=Anumb;Global`MINP=minP; Global`MAXP=maxP;
Global`CNUMB=Cnumb; Global`LNUM =Lnumb;Global`DELTAC=deltaC; Global`DELTAL=deltaL;


j=1;labeling=0;
If[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100, 

labeling=1;
Print[Style[StringForm["\[FilledSquare] Process for adjusting levels of Quality Box is initiated\n\tmainly to limit the selected models between ``% and ``% of all generated models.", 100. minP, NF@100. maxP], fontsize, Blue]];
Print[Style[StringForm["\[FilledSquare] Generated models: ``   Selected models: `` (``%)", Anumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
Print[Style[StringForm["\[FilledDiamond] Inning ``. Complexity: ``  Error: ``  Number of Selected models: `` (``%)", 0, NF@Cnumb, NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
];


While[(Pnumb/Anumb<minP||Pnumb/Anumb>maxP)&&Pnumb< minnumb&&j<= 100,


Which[
Pnumb/Anumb<minP,

Cnumb=Cnumb+deltaC;
Lnumb=Lnumb+deltaL;
Smodels=SelectModels[archivedModels[i],QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

Pnumb/Anumb>minP,

Cnumb=Cnumb-deltaC;
Lnumb=Lnumb-deltaL;
Smodels=SelectModels[archivedModels[i],QualityBox->{Cnumb, Lnumb}];
Pnumb=Length@Smodels,

True,
ErrorMessage["Unexpected limiting error"]
];

Print[Style[StringForm["\[FilledDiamond] Inning ``.  Complexity: ``  Error: ``  Number of Selected models: `` (``%)", j, NF@Cnumb, NF@Lnumb, Pnumb, NF[100. Pnumb/Anumb]], fontsize]];
j++

];

PB[];
Print[Style[StringForm[ "\t\[EmptyDiamond] Final Quatiliy Box levels are {``, ``} in this `` turn.", NF@Cnumb, NF@Lnumb, Ordinalize[i]],Bold,fontsize, darkgreen]];
PB[];

If[labeling===1, PB[]];

finalquality[i]={Cnumb, Lnumb};
Global`FINALQUALITY[i]=finalquality[i];
interestingModels[i] =Smodels;
imodelcount[i]=Length[interestingModels[i]];
Global`INTERESTINGMODELS[i]=interestingModels[i];Global`IMODELCOUNT[i]=imodelcount[i];
AppendTo[interestingbag, interestingModels[i]];
Global`INTERESTINGBAG=interestingbag;

interestimage[i]=ParetoFrontPlot[interestingModels[i], 
PlotLabel->LabelForm[{Length@interestingModels[i],"models are judged as interesting"},Joined->True],
ImageSize-> size1, DensityPlot-> False
];

Print[Style[StringForm["\[FilledDiamond] `` interesting models were selected", imodelcount[i]],Bold, fontsize]];
Print[Style[StringForm[ "\t\[EmptyDiamond] Quatiliy Box values are ``.", finalquality[i]],Bold,fontsize]];
Print[interestimage[i]];

Print["                             "];

AppendTo[countbag, imodelcount[i]];
AppendTo[imagebag, interestimage[i]];
AppendTo[partbag, imodelcount[i]];
Global`PARTBAG=partbag;


Print["                             "];
presenceimage[i]=VariablePresenceChart[interestingModels[i], 
VariablesToPlot-> DriverVariables@interestingModels[i],
ImageSize-> size1];

usedvariables[i]=VariablePresence[interestingModels[i], QualityBox-> finalquality[i], SignificanceLevel-> All, PresenceMetric-> "Percent"];
Global`USEDVARIANBLES[i]=usedvariables[i];
AppendTo[variablebox, usedvariables[i]];
bestfive[i]=topfive[usedvariables[i]];
Global`BESTFIVE[i]=bestfive[i];
AppendTo[bestbag, bestfive[i]];
Print[Style["\[FilledDiamond] Variable Presence (Popularity of each variable) FOCUSED on important variables",Bold,fontsize]];
Print[Style[StringForm["\[FilledDiamond] TOP FIVE HIGHLY USED vairbles: ``", bestfive[i]], fontsize, Red, Bold]];
Print[presenceimage[i]];
Print["                             "];

AppendTo[presencebag, presenceimage[i]];


(* Ensemble learning *)
Print[Style["\[FilledDiamond] Defining Ensembles", Bold, fontsize]];
anEnsemble[i] = CreateModelEnsemble[interestingModels[i] ,partTrainingData[i] ];
ensembleimage[i] =ModelSelectionReport[anEnsemble[i] , ImageSize-> size1];
Print[ensembleimage[i] ];
Print["                             "];
Global`ANENSEMBLE[i] =anEnsemble[i] ;


Print[Style["\[FilledDiamond] Ensembles in ParetoFront", Bold, fontsize]];
ensembleinpareto[i] =ParetoFrontContextPlot[anEnsemble[i] ,interestingModels[i] , ImageSize-> size1];
Print[ensembleinpareto[i] ];
Print["                             "];


AppendTo[ensemblebag, anEnsemble[i]];


(* Created Phenotype *)
(* Print[Style["\[FilledDiamond] Selected Phenotype", Bold, fontsize]]; *)
phenotype[i]=ModelPhenotype[anEnsemble[i]];
Global`PHENOTYPE[i]=phenotype[i];
createdModel[i]=CreateStandaloneModel[ anEnsemble[i]];
Global`CREATEDMODEL[i]=createdModel[i];
estimatedModel[i]=createdModel[i];
Global`ESTIMATEDMODEL[i]=estimatedModel[i];
traditionalForm[i]=TraditionalForm[phenotype[i]];
Global`TRADITIONALFORM[i]=TraditionalForm[phenotype[i]];

(* Print[DisplayForm[FrameBox[traditionalForm[i]]]];
Print["                             "]; *)

AppendTo[createdbag, createdModel[i]];
AppendTo[traditionalbag, traditionalForm[i]];
Global`CREATEDBAG=createdbag;
Global`TRADITIONALBAG=traditionalbag;

(* The Result of the i-th cross-validation *)

Print[Style[StringForm["\[FilledSquare] The `` Cross Validation with Leave-One-Out Method out of `` turns", Ordinalize[i], loolen], fontsize, Bold]];


estimatedValue[i]=createdModel[i]@@partTestData[i][[1]];
Global`ESTIMATEDVALUE[i]=estimatedValue[i];
(* roundedValue[i]=Which[ estimatedValue[i]\[LessEqual]  NMin@minmax, NMin@minmax, 
					 estimatedValue[i]\[GreaterEqual] NMax@minmax, NMax@minmax,
					True,Round[estimatedValue[i]]]; *)
roundedValue[i]=Last@Nearest[labels, estimatedValue[i]]; 
Global`ROUNDEDVALUE[i]=roundedValue[i];

(* criteria=allTrainingData[[All,Dimensions[allTrainingData][[2]]]];Global`NEWCRITERIA=criteria; *)
observedValue[i]=criteria[[i]];
Global`OBSERVEDVALUE[i]=observedValue[i];


error[i]=estimatedValue[i]-observedValue[i];
Global`ERROR[i]=error[i];
AppendTo[errorbag, error[i]];
Global`ERRORBAG=errorbag;


pair[i]={ estimatedValue[i], observedValue[i]};Global`PAIR[i]=pair[i];
pair2[i]={roundedValue[i], observedValue[i]};Global`PAIR2[i]=pair2[i];
AppendTo[pairbag, pair[i]];
AppendTo[pairbag2, pair2[i]];
Global`PAIRBAG=pairbag;
Global`PAIRBAG2=pairbag2;

judged[i]=If[ RealVectorQ[pair[i]]&&(pair2[i][[1]]==pair2[i][[2]]), 1, 0, Null];
Global`JUDGED[i]=judged[i];
If[judged[i]===0||judged[i]===Null, AppendTo[wrongcases, Prepend[partTestData[i][[1]], StringJoin[ Ordinalize[i]," case"]]]];
If[judged[i]===1, AppendTo[rightcases, Prepend[partTestData[i][[1]], StringJoin[ Ordinalize[i]," case"]]]];
Global`WRONGCASES=wrongcases;
Global`RIGHTCASES=rightcases;

AppendTo[looresult, judged[i]];
Global`LOORESULT=looresult;

PB[];
Print[Style[StringForm["\tThe Estimated value: ``, The Observed value: ``", NF@estimatedValue[i],observedValue[i]], Bold, 15]];
If[judged[i]===1,
Print[Style[StringForm["\tThe Prediction: Right"], Bold, 15, Blue]],
Print[Style[StringForm["\tThe Prediction: Wrong"], Bold, 15, Red]]];
Print[Style[StringForm["\tAccuracy so far: ``%   (``% completed)", NF[100. Count[looresult,1]/i], NF[100. i/loolen]], Bold,15, RGBColor[0, 0.712917, 0]]];
PB[];

remainingtime=rawSecToHourMinSec[AbsoluteTime[future]-AbsoluteTime[Now]];
Print[Style[StringForm["\[EmptyDiamond] Estimated Remaining time: `` hr `` min `` sec",remainingtime[[1]],remainingtime[[2]],remainingtime[[3]] ],Bold,fontsize]];
PB[];

looscore[i]=100. Count[looresult, 1]/i;
Global`LOOSCORE[i]=looscore[i];
AppendTo[looscores, looscore[i]];
Global`LOOSCORES=looscores;
lootext[i]=Style[Text[ToString@NF@looscore[i], {i, looscore[i]+3}],Bold,12];
Global`LOOTEXT[i]=lootext[i];
lootexts=AppendTo[lootexts, lootext[i]];
Global`LOOTEXTS=lootexts;
loograph=ListLinePlot[looscores, PlotRange-> {{0, i+2}, {0, 105}},PlotMarkers->Automatic, PlotStyle-> {Thick, Blue}, Epilog-> Last@lootexts, AxesLabel->Map[Style[#, Bold, 14]&, {"Turn", "Accuracy(%)"}], TicksStyle->Directive["Label", 14], Ticks-> {If[i<=  30,Range[i+1], Range[5, i+5, 5]], Range[0,100,10]}, GridLines->{Range[i+1], Range[0,100,10]}, ImageSize->size1];
Global`LOOGRAPH=loograph;

Print[Style[StringForm["\[FilledDiamond] Accuracies until the `` turn in the Leave-One-Out Cross Validation out of `` turns", Ordinalize[i],loolen], Bold, fontsize]];
Print@loograph;

errorminmax={NMin[errorbag],NMax[errorbag]};

errortext[i]=Style[Text[ToString@NF@error[i], {i, error[i]+If[error[i]>0, -0.1, 0.1, 0.1]}],Bold,12];
Global`ERRORTEXT[i]=errortext[i];
errorgraph=ListLinePlot[errorbag, PlotRange-> {{0, i+2}, {If[-1>errorminmax[[1]],errorminmax[[1]]-0.1,-1.1,-1.1],If[1<errorminmax[[2]],errorminmax[[2]]+0.1,1.1,1.1]}},PlotMarkers->Automatic, PlotStyle-> {Thick, Red}, Epilog-> errortext[i], AxesLabel->Map[Style[#, Bold, 14]&, {"Turn", "Error"}], TicksStyle->Directive["Label", 14], Ticks-> {If[i<=  30,Range[i+1], Range[5, i+5, 5]], Range[-2,2,0.5]}, GridLines->{Range[i+1], Range[-2,2,0.5]}, ImageSize-> size1,
Epilog-> {{Thick,Green,Dashed,Line[{{0,-0.5},{i,-0.5}}]}, {Thick,Green,Dashed,Line[{{0,0.5},{i,0.5}}]}}];
PB[];

Print[Style[StringForm["\[FilledDiamond] Error(= Predicted value - Observed value) in this `` Cross Validation", Ordinalize[i]],  Bold, fontsize]];
Print[Style[StringForm["\[EmptyDiamond] Average Error is ``\[PlusMinus]`` until this `` turn in the LOO method.", NF@NMean[Map[Abs, errorbag]],NF@NStandardDeviation[Map[Abs, errorbag]],Ordinalize[i]],  Bold, fontsize]];
Print@errorgraph;
PB[];


];
(* The end of i-th levae-one-out cross validation *)

PB[];
Print[Style[StringForm["\[FilledSquare] The End of Leave-One-Out Cross Validation Loop"], fontsize,Bold]];









(* ------------------------------------------- Summary of LOO -----------------------------------------------*)


PB[];
PB[];
Print[Style[StringForm["\[FilledSquare] Summary of The ``-fold Cross Validation with Leave-One-Out Method", loolen], Bold, 21, Purple]];
PB[];
Print[Style["\[FilledSquare] 0. Report of Hyper-parameter Settings in LOO-CV", Bold, fontsize2, Blue]];
PB[];

Print[Style[StringForm["\[FilledDiamond] ``", version],Bold,fontsize, Blue]];
Print[Style[StringForm["1. TimeConstraint: ``min,  N of Evolutions: ``,  Processor Count in the computer: ``", timeconstraint, evolutions, $ProcessorCount],fontsize, Bold, Red]];
Print[Style[StringForm["2. Evolution Strategy: ``,  Selection Strategy: ``,  RobustModels: ``", evolutionStrategyLOO, selectionStrategyLOO, robustModelLOO],fontsize, Bold]];
Print[Style[StringForm["3. Computation time for one evolution: ``, Total computation tiem: ``", totaltime, fulltime],fontsize, Bold]];
Print[Style[StringForm["4. Initial QuatlityBox values: Comlexity < ``, Error < ``", NF@quality[1][[1]], NF@quality[1][[2]] ],fontsize, Bold, Red]];
Print[Style[StringForm["5. Subset size for model evaluation: ``%", NF@subsetsize ],fontsize, Bold]];
Print[Style[StringForm["6. Gradient for setting the QualityBox: ``", fitindex ],fontsize, Bold]];
Print[Style[StringForm["7. Limits of selected functions: ``% \:301c ``%", limits[[1]], NF@limits[[2]] ],fontsize, Bold]];

PB[];
Print[Style[StringForm["\[FilledDiamond] Trajectory of Accuracies in the Leave-One-Out Cross Validation with `` turns",  loolen], Bold,Blue, fontsize]];
Print@loograph;

errorminmax={NMin[errorbag],NMax[errorbag]};

errortext[loolen]=Style[Text[ToString@NF@error[loolen], {loolen, error[loolen]+If[error[loolen]>0, -0.1, 0.1, 0.1]}],Bold,12];
Global`ERRORTEXT[loolen]=errortext[loolen];
errorgraph=ListLinePlot[errorbag, PlotRange-> {{0, i+1}, {If[-1>errorminmax[[1]],errorminmax[[1]],-1,-1],If[1<errorminmax[[2]],errorminmax[[2]],1,1]}},PlotMarkers->Automatic, PlotStyle-> {Thick, Red}, Epilog-> errortext[loolen], AxesLabel->Map[Style[#, Bold, 14]&, {"Turn", "Error"}], TicksStyle->Directive["Label", 14], Ticks-> {If[loolen<=  30,Range[loolen+1], Range[5, loolen+5, 5]], Range[-2,2,0.5]}, GridLines->{Range[loolen+1], Range[-2,2,0.5]}, ImageSize-> size1,
Epilog-> {{Thick,Green,Dashed,Line[{{0,-0.5},{i,-0.5}}]}, {Thick,Green,Dashed,Line[{{0,0.5},{loolen,0.5}}]}}];

PB[];
Print[Style[StringForm["\[FilledDiamond] Trajectory of Errors in the Leave-One-Out Cross Validation with `` turns", loolen], Bold,Blue, fontsize]];
Print[Style[StringForm["\[EmptyDiamond] Average Error was ``\[PlusMinus]`` in the LOO test.", NF@NMean[Map[Abs, errorbag]],NF@NStandardDeviation[Map[Abs, errorbag]],loolen],  Bold, fontsize]];
Print@errorgraph;

PB[];
Print[Style["\[FilledSquare] 1. Classification Performance with Leave-One-Out cross validation (LOO~CV)", Bold, fontsize2, Blue]];
PB[];


theta0=0.0005;
thRange=Range[-1, 2, theta0];
Global`THRANGE=thRange;

pospos=Count[pairbag2, {1,1}];
posneg=Count[pairbag2, {1,0}];
negpos=Count[pairbag2, {0,1}];
negneg=Count[pairbag2, {0,0}];

cmdata={{pospos, posneg, pospos+posneg},{negpos, negneg, negpos+negneg},{pospos+negpos, posneg+negneg, pospos+posneg+negpos+negneg}};
Global`CMDATA=cmdata;

ppvalue=100. pospos/(pospos+posneg);
npvalue=100. negneg/(negpos+negneg);
sensvalue=100. pospos/(pospos+negpos);
specvalue=100. negneg/(posneg+negneg);
accuvalue=100. (pospos+negneg)/loolen;
fvalue=2.(ppvalue*sensvalue)/(ppvalue+sensvalue);
cvalue=cStatistics[pairbag][[1]];
lm = LinearModelFit[pairbag, x, x];
rsquared = lm["RSquared"];
Global`FVALUE=fvalue; Global`CVALUE=cvalue;
Global`PPVALUE=ppvalue; Global`SENSVALUE=sensvalue;
Global`ACCUVALUE=accuvalue;
Global`RSQARED = rsquared;

testLabels=pairbag[[All,2]];
modelValues=pairbag[[All,1]];
aROCs=Table[ToROCAssociation[{1,0},testLabels,Map[If[#>theta,1,0]&,modelValues]],{theta,thRange}];
ROCcurve=ROCPlot[thRange,aROCs,"PlotJoined"->Automatic,"ROCPointCallouts"->False,"ROCPointTooltips"->True,GridLines->Automatic, ImageSize-> size1];
AUCvalue=NF@N@ROCFunctions["AUROC"][aROCs];
(* AUCvalue2=N@Total[Partition[Sort@Transpose[{ROCFunctions["FPR"]/@aROCs,ROCFunctions["TPR"]/@aROCs}],2,1]/.{{x1_,y1_},{x2_,y2_}}\[RuleDelayed](x2-x1) (y1+(y2-y1)/2)]; *)
Global`TESTLABELS=testLabels;
Global`MODELVALUES=modelValues;
Global`AROCS=aROCs;
Global`ROCCURVE=ROCcurve;
Global`AUCVALUE=AUCvalue;
(* Global`AUCVALUE2=AUCvalue2; *)

rocFuncs={"PPV","NPV","TPR","ACC","SPC","MCC"};rocFuncTips=Map[#<>", "<>(ROCFunctions["FunctionInterpretations"][#])&,rocFuncs];
rocvalues=MapThread[Tooltip[Transpose[{thRange,#1}],#2]&,{Transpose[Map[Through[ROCFunctions[rocFuncs][#]]&,aROCs]],rocFuncTips}];
Global`ROCVALUES=rocvalues;
ROCgraph=ListLinePlot[rocvalues,Frame->True,ImageSize -> size1,FrameLabel->Map[Style[#,Larger]&,{"threshold, \[Theta]","rate"}],PlotLegends->rocFuncTips,GridLines->Automatic, PlotRange-> {{0,1},{0,1}}];
Global`ROCFUNCS=rocFuncs;
Global`ROCFUNCTIPS=rocFuncTips;
Global`ROCGRAPH=ROCgraph;

Print[Style["\[FilledSquare] Confusion Matrix", Bold, fontsize]];
Print[Style[TableForm[ cmdata, TableHeadings->{{"Predicted as 1","Predicted as 0","Total"},{"Observed as 1","Observed as 0", "Total"}}],Bold,fontsize]];
PB[];

Print[Style[StringForm["1. Sample size: ``  Number of explanatory variables ``", Length@allTrainingData, Length@allNames-1], Bold, fontsize]];
Print[Style[StringForm["2. Accuracy: ``%", NF@accuvalue], Bold, fontsize, Red]];
Print[Style[StringForm["3. Precision(\:7cbe\:5ea6=\:967d\:6027\:7684\:4e2d\:7387): ``%  Negative Predictie Value(\:9670\:6027\:7684\:4e2d\:7387): ``%", NF@ppvalue, NF@npvalue], Bold, fontsize, Blue]];
Print[Style[StringForm["4. Recall(\:518d\:73fe\:7387=\:611f\:5ea6): ``%  Specificity(\:7279\:7570\:5ea6): ``%", NF@sensvalue, NF@specvalue], Bold, fontsize, Blue]];
Print[Style[StringForm["5. F1-score(F1\:5024): ``  C-statistic(C\:7d71\:8a08\:5024): ``", NF@fvalue, NF@cvalue], Bold, fontsize]];
Print[Style[StringForm["6. AUC of ROC curve: ``", AUCvalue], Bold, Red, fontsize]];
Print[Style[StringForm["7. Coefficient of Determination(\:6c7a\:5b9a\:4fc2\:6570): ``", NF@rsquared], Bold, Blue, fontsize]];
Print[Style[StringForm["8. RMSE: `` (Root Mean Square Error)", NF@RootMeanSquare@errorbag], Bold, Blue, fontsize]];
Print[Style[StringForm["9. Average Error in the Target Value: `` \[PlusMinus] ``", NF@NMean@Map[Abs, errorbag], NF@NStandardDeviation@Map[Abs, errorbag]], Bold, fontsize]];
(* Print[Style[StringForm["6`. AUC of ROC curve: ``", AUCvalue2], Bold, Red, fontsize]]; *)
PB[];
triplebag=Fuse[Fuse[pairbag[[All,1]],pairbag2[[All,1]]], pairbag[[All,2]]];Global`TRIPLEBAG=triplebag;
quartetbag=Fuse[triplebag, looresult];Global`QUARTETBAG=quartetbag;
ExportExcel[quartetbag, Numberings[{"Predicted","Rounded","Observed", "LOOresult"}], "BuildModelLOOvalues"];
Print[Style[StringForm["\[FilledDiamond] The Raw Result of Leave-One-Out Cross Validation is expressed as QUARTETBAG\n  that is made of {predicted value, rounded value, observed value, LOOCV value}.\n  And it is exported in an Excel file named BuildModelLOOvalues....xlsx"],14, Bold]];

totalbag=Fuse[allTrainingData, quartetbag];Global`TOTALBAG=totalbag;
totalnames=Join[allNames, {"Predicted","Rounded","Observed", "LOOresult"}];Global`TOTALNAMES=totalnames;
ExportExcel[totalbag, Numberings@totalnames, "BuildModelLOOCVresult"];
Print[Style[StringForm["\[FilledDiamond] The Total Result of Leave-One-Out Cross Validation is expressed as TOTALBAG\n  that is made of raw data with {predicted value, rounded value, observed value}.\n  And it is exported in an Excel file named BuildModelLOOCVresult...xlsx"],14, Bold]];

PB[];
PB[];
Print[Style["\[FilledSquare] 2. Receiver Operating Characteristics Analysis with the LOO-CV", Bold, fontsize2, Blue]];
PB[];



Print[Style["\[FilledDiamond] ROC curve", Bold, fontsize]];
Print@ROCcurve;
PB[];
Print[Style[StringForm["\[FilledDiamond] AUC of ROC curve: ``", AUCvalue], Bold, Red, fontsize]];
PB[];
PB[];
Print[Style["\[FilledDiamond] Effect of THRESHOLD between positive and negative", Bold, fontsize]];
Print@ROCgraph;


PB[];
PB[];
Print[Style["\[FilledSquare] 3. Results of Leave-One-Out Cross-Validation and its Prediction in Each Case", Bold, fontsize2, Blue]];

PB[];
Print[Style[StringForm["\[FilledDiamond] Numbers of functions generated by Genetic Programming"], Bold, fontsize]];
PB[];
numbers=TableForm[{allbag, partbag}, TableHeadings->{{"All Generated Models","All Selected Models"},Table[StringJoin["The ", Ordinalize[i], " CV"], {i,1,loolen}]}];
Print@numbers;

PB[];
Print[Style[StringForm["\[FilledDiamond] Estimated values and Observed values"], Bold, fontsize]];
PB[];
rightwrong=Map[ Which[#==1, Style["right",Blue], #==0, Style["WRONG",Bold,Red], True, error]&, looresult];
Global`RIGHTWRONG=rightwrong;
scores=Transpose@Fuse[pairbag, rightwrong];
Global`SCORES=scores;
summary=TableForm[scores, TableHeadings-> {{"Predicted","Observed","Right/Wrong"},Table[StringJoin["The ", Ordinalize[i], " CV"], {i,1,loolen}]}];
Print@summary;
Global`SUMMARY=summary;
PB[];
PB[];


PB[];
Print[Style[StringForm["\[FilledSquare] 4. FREQUENTLY USED Variables in the Selected `` Models with the LOO-CV", Total@partbag], Bold, fontsize2, Blue]];
PB[];


Print[Style[StringForm["\[FilledDiamond] N of all generated models: ``,   n of all selected models: ``", Total@allbag, Total@partbag],Bold,fontsize,Blue]];
PB[];


Print[Style[StringForm["\[FilledDiamond] Distribution of ALL GENERATED `` models in the LOO-CV on the functioin space", Total@allbag],Bold,fontsize]];

allmodels=Flatten[archivedbag,1];
Global`ALLMODELS=allmodels;
(* allmodels = allarchivedmodels *)

allgraph=ParetoFrontPlot[allmodels, DensityPlot-> False];
Print@allgraph;
Print["                             "];

Print[Style[StringForm["\[FilledDiamond] Distribution of SELECTED `` models in the LOO-CV on the functioin space", Total@partbag],Bold,fontsize]];

partmodels=Flatten[interestingbag, 1];
Global`PARTMODELS=partmodels;
(* partmodels = allinterestingmodels *)

partgraph=ParetoFrontPlot[partmodels, DensityPlot-> False];
Print@partgraph;
Print["                             "];

Print[Style[StringForm["\[FilledDiamond] Explanatory Variable Presence Percentage in all `` Selected Models in all `` Cross-Validations", Length@Flatten[interestingbag,1], loolen],Bold,fontsize]];

presencefinalimage=VariablePresenceChart[Flatten[interestingbag,1], 
VariablesToPlot-> DriverVariables@Flatten[interestingbag,1],PlotRange-> {{All,100},Automatic},
ImageSize-> size2+100];
Print[presencefinalimage];
Global`PRESENCEFINALIMAGE=presencefinalimage;

dominantvariables=VariablePresence[Flatten[interestingbag,1], QualityBox-> {All,1},
PresenceMetric-> "Percent"];Global`DOMINANTVARIABLES=dominantvariables;
Print["                             "];
Print[Style[StringForm["\t\[FilledDiamond] `` Dominant Variables and their Frequencies of Use(percent) in `` Selected Models", Length@dominantvariables,Length@Flatten[interestingbag,1]],Bold,fontsize]];
Print@Column[dominantvariables, ItemSize-> 14, Frame-> True];
Print[Style[StringForm["\t\[EmptyDiamond] `` Dominant Variables in the order of usage frequencies in `` Selected Models are expressed as DOMINANTVARIABLES", Length@dominantvariables,Length@Flatten[interestingbag,1]],Bold,fontsize]];

Print["                             "];
Print[Style["\[FilledDiamond] 4-2. Variable Presence (Popularity of each variable) SORTED by frequencies",Bold,fontsize]];
presenceimage=VariablePresenceChart[interestingModels, 
VariablesToPlot-> Flatten@{DriverVariables@interestingModels,
DeleteCases[ DataVariables@First@interestingModels,
Alternatives@@DriverVariables[interestingModels]
]
},
ImageSize-> size1];
Print[presenceimage];
Print["                             "];

If[Length@allNames>= 21,
Print[Style[StringForm["\[FilledDiamond] Explanatory Variable Presence Percentage FOCUSED on TOP 20 in all `` Selected Models in all `` Cross-Validations", Length@Flatten[interestingbag,1], loolen],Bold,fontsize]];

presencefinalimage20=VariablePresenceChart[Flatten[interestingbag,1], 
VariablesToPlot-> DriverVariables[Flatten[interestingbag,1],20],PlotRange-> {{All,100},Automatic},
ImageSize-> size1];
Print[presencefinalimage20];
Global`PRESENCEFINALIMAGE15=presencefinalimage20;

top20variables=VariablePresence[Flatten[interestingbag,1], QualityBox-> {All,1},SignificanceLevel-> 20,
PresenceMetric-> "Percent"];Global`TOP20VARIABLES=top20variables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Top 20 Variables and their Frequencies of Use(percent)",Bold,fontsize]];
Print@Column[top20variables, ItemSize-> 14, Frame-> True];
Print[Style["\t\[EmptyDiamond] To 20 Variables in the order of usage frequencies are expressed as TOP20VARIABLES",Bold,fontsize]];
Print["                             "]];

If[Length@allNames>= 16,
Print[Style[StringForm["\[FilledDiamond] Explanatory Variable Presence Percentage FOCUSED on TOP 15 in all `` Selected Models in all `` Cross-Validations", Length@Flatten[interestingbag,1], loolen],Bold,fontsize]];

presencefinalimage15=VariablePresenceChart[Flatten[interestingbag,1], 
VariablesToPlot-> DriverVariables[Flatten[interestingbag,1],15],PlotRange-> {{All,100},Automatic},
ImageSize-> size1];
Print[presencefinalimage15];
Global`PRESENCEFINALIMAGE15=presencefinalimage15;

top15variables=VariablePresence[Flatten[interestingbag,1], QualityBox-> {All,1},SignificanceLevel-> 15,
PresenceMetric-> "Percent"];Global`TOP15VARIABLES=top15variables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Top 15 Variables and their Frequencies of Use(percent)",Bold,fontsize]];
Print@Column[top15variables, ItemSize-> 14, Frame-> True];
Print[Style["\t\[EmptyDiamond] To 15 Variables in the order of usage frequencies are expressed as TOP15VARIABLES",Bold,fontsize]];
Print["                             "]];

If[Length@allNames>= 11,
Print[Style[StringForm["\[FilledDiamond] Explanatory Variable Presence Percentage FOCUSED on TOP 10 in all `` Selected Models in all `` Cross-Validations", Length@Flatten[interestingbag,1], loolen],Bold,fontsize]];

presencefinalimage10=VariablePresenceChart[Flatten[interestingbag,1], 
VariablesToPlot-> DriverVariables[Flatten[interestingbag,1],10],PlotRange-> {{All,100},Automatic},
ImageSize-> size1-100];
Print[presencefinalimage10];
Global`PRESENCEFINALIMAGE10=presencefinalimage10;

top10variables=VariablePresence[Flatten[interestingbag,1], QualityBox-> {All,1},SignificanceLevel-> 10,
PresenceMetric-> "Percent"];Global`TOP10VARIABLES=top10variables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Top 10 Variables and their Frequencies of Use(percent)",Bold,fontsize]];
Print@Column[top10variables, ItemSize-> 14, Frame-> True];
Print[Style["\t\[EmptyDiamond] To 10 Variables in the order of usage frequencies are expressed as TOP10VARIABLES",Bold,fontsize]];
Print["                             "]];

If[Length@allNames>= 6,
Print[Style[StringForm["\[FilledDiamond] Explanatory Variable Presence Percentage FOCUSED on TOP 5 in all `` Selected Models in all `` Cross-Validations", Length@Flatten[interestingbag,1], loolen],Bold,fontsize]];

presencefinalimage5=VariablePresenceChart[Flatten[interestingbag,1], 
VariablesToPlot-> DriverVariables[Flatten[interestingbag,1],5],PlotRange-> {{All,100},Automatic},
ImageSize-> size1-200];
Print[presencefinalimage5];
Global`PRESENCEFINALIMAGE5=presencefinalimage5;

top5variables=VariablePresence[Flatten[interestingbag,1], QualityBox-> {All,1},SignificanceLevel-> 5,
PresenceMetric-> "Percent"];Global`TOP5VARIABLES=top5variables;
Print["                             "];
Print[Style["\t\[FilledDiamond] Top 5 Variables and their Frequencies of Use(percent)",Bold,fontsize]];
Print@Column[top5variables, ItemSize-> 14, Frame-> True];
Print[Style["\t\[EmptyDiamond] To 5 Variables in the order of usage frequencies are expressed as TOP10VARIABLES",Bold,fontsize]];
Print["                             "]];



presencefinal=VariablePresence[Flatten[interestingbag,1], QualityBox-> All, SignificanceLevel-> All, PresenceMetric-> "Percent"];
Global`PRESENCEFINAL=presencefinal;
finaltopfive=topfive[presencefinal];
Global`FINALTOPFIVE=finaltopfive;
Print[Style[StringForm["\[FilledDiamond]\:3000Most Frequently Used Variables in all `` Selected Models; Top Five: ``", Length@Flatten[interestingbag,1],finaltopfive], fontsize, Bold, Red]];
PB[];
PB[];
allvariables=TableForm[presencefinal, TableHeadings->{Automatic,{"Frequency(%)","Variable Name"}}];
Print@allvariables;
Global`ALLVARIABLES=allvariables;

PB[];
PB[];
Print[Style[StringForm["\[EmptyDiamond] Exporting cases that were MIS-CATEGORIED in the LEAVE-ONE-OUT cross-validation as wrongcases\:30fb\:30fb\:30fb.xlsx\n\tin the home directory."], Bold, fontsize]];
ExportExcel[wrongcases,Numberings@Prepend[allNames, "Case Number"], "BuildModelWrongCases"];
Print[Style[StringForm["\[EmptyDiamond] Exporting cases that were CORRECTLY-CATEGORIED in the LEAVE-ONE-OUT cross-validation as rightcases\:30fb\:30fb\:30fb.xlsx\n\tin the home directory."], Bold, fontsize]];
(* ExportExcel[wrongcases,Numberings@Prepend[allNames, "Case Number"], "BuildModelWrongCases"]; *)
ExportExcel[rightcases,Numberings@Prepend[allNames, "Case Number"], "BuildModelRightCases"];


PB[];
PB[];
Print[Style[StringForm["\[FilledSquare] 5. Result of Discrimination of all Cases with the Optimized Model created from All `` Selected Models",  Total@partbag],Bold,fontsize2,Blue]];
PB[];


allarchivedModels=Flatten[archivedbag,1];
Global`ALLARCHIVEDMODELS=allarchivedModels;
(* allarchivedModles = allmodels *)

allinterestingModels=Flatten[interestingbag,1];
Global`ALLINTERESTINGMODELS=allinterestingModels;
(* allinterestingModels = partmodeles *)

allEnsemble = CreateModelEnsemble[allinterestingModels,allTrainingData];
Global`ALLENSEMBLE=allEnsemble;

allprojectname=StringJoin[projectname, "GeneratedModels"];

StoreModelSet[
allarchivedModels,
ProjectName->allprojectname,
Compress-> True,
GPModelSet-> True
];

Print[Style[StringForm["\[EmptyDiamond] Stored all the Generated Models as `` in the DataModelerModelSets folder.", allprojectname], Bold, fontsize]];

partprojectname=StringJoin[projectname, "SelectedModels"];

StoreModelSet[
allinterestingModels,
ProjectName->partprojectname,
Compress-> True,
GPModelSet-> True
];

Print[Style[StringForm["\[EmptyDiamond] Stored the Selected Models as `` in the DataModelerModelSets folder.", partprojectname], Bold, fontsize]];

Print[Style["\[FilledDiamond] 1. Defining Ensembles", Bold, fontsize]];
allensembleimage=ModelSelectionReport[allEnsemble, ImageSize-> size1];
Print[allensembleimage];
Print["                             "];
Global`ALLENSEMBLEIMAGE=allensembleimage;


Print[Style["\[FilledDiamond] 2. Ensembles in ParetoFront", Bold, fontsize]];
allensembleinpareto=ParetoFrontContextPlot[allEnsemble,allinterestingModels, ImageSize-> size1];
Print[allensembleinpareto];
Print["                             "];


 Print[Style["\[FilledDiamond] 3. Prediction Performance", Bold, fontsize]];


allcomparisonimage=ModelPredictionComparisonPlot[allEnsemble,allTrainingData,
SortBy->"Observed",
DataOutliers->alloutlierIndices,
ImageSize-> size1
];
Print[allcomparisonimage];
Global`ALLCOMPARISONIMAGE=allcomparisonimage;
Print["                             "];



Print[Style[StringForm["\[FilledDiamond] 4. Selected Phenotype of Optimized Model from `` Selected Models", Total@partbag], Bold, fontsize]];
allphenotype=ModelPhenotype[allEnsemble];
Global`ALLPHENOTYPE=allphenotype;
allcreatedModel=CreateStandaloneModel[ allEnsemble];
Global`ALLCREATEDMODEL=allcreatedModel;
allestimatedModel=allcreatedModel;
Global`ALLESTIMATEDMODEL=allestimatedModel;
alltraditionalForm=TraditionalForm[allphenotype];
Global`TALLRADITIONALFORM=alltraditionalForm;

Print[DisplayForm[FrameBox[alltraditionalForm]]];
Print["                             "];

Print[Style["\[EmptySquare] Created standalone model is expressed as ALLPHENOTYPE, created function as ALLCREATEDMODEL.", fontsize, Bold]];
Print[Style["\t(Predicted values are calculated with ALLCREATEDMODEL @@@ ALLTRAININGDATA)", 14, Bold]];
Print["                             "];

Print[Style[StringForm["\[FilledDiamond] 5. The Optimized Model created from `` Selected Models (expressed as ALLCREATEDMODEL) is as follows.", Total@partbag], fontsize, Bold]];
Print["                               "];
Print[allcreatedModel];
Print["                               "];



Print[Style[StringForm["\[FilledDiamond] 6. Comparison Between Predicted values(Red) and Observed ones(Blue) with all Original Dataset\n     using the Optimized Model Created from all `` Selected Models in `` Cross Validations", Total@partbag, loolen], fontsize, Bold]];


nullflag=Position[ allTrainingData, Null];Global`NULLFLAG=nullflag;
If[
Length@nullflag>= 1,

Print["                             "];
{alltrainingsIP, allnamesIP}=nullchecker2[allTrainingData, allNames];
Global`ALLTRAININGSIP=alltrainingsIP; Global`ALLNAMESIP=allnamesIP;
allTrainingData=alltrainingsIP; allNames=allnamesIP;
Print[Style[StringForm["\[FilledDiamond] The `` empty cells in the training data were replaced with appropriate numbers.", Length@nullflag]]];
Print["                            "];
];


allestimatedValues=allcreatedModel@@@allTrainingData;
allobservedValues=criterion;
Global`ALLESTIMATEVALUES=allestimatedValues;Global`ALLOBSERVEDVALUES=allobservedValues;
allpairs=MakeTwin[ allestimatedValues, allobservedValues];
allpairs1=realchecker[allpairs];
allpairs2=Sort[ allpairs1, #1[[1]]<= #2[[1]]&];
Global`ALLPAIRS=allpairs; Global`ALLPAIRS1=allpairs1;Global`ALLPAIRS2=allpairs2;
alllm=LinearModelFit[allpairs1, x, x];
allrsquared=alllm["RSquared"];
Global`ALLLM=alllm; Global`ALLRSQUARED=allrsquared;

(* allroundedValues=Map[ Which[#\[LessEqual]  NMin@minmax, NMin@minmax, #\[GreaterEqual] NMax@minmax, NMax@minmax, True, Round[#]]&,allestimatedValues]; *)
allroundedValues = Map[Last@Nearest[labels, #]&, allestimatedValues];
allpairs3=MakeTwin[allroundedValues, allobservedValues];
triplebagOM=Fuse[Fuse[allpairs1[[All,1]],allpairs3[[All,1]]], allpairs1[[All,2]]];Global`TRIPLEBAGOM=triplebagOM;
allresult=Map[ If[ RealVectorQ@#&&(#[[1]]==#[[2]]), 1, 0, Null]&, allpairs3];
allrightwrong=Map[ Which[#==1, Style["right",Blue], #==0, Style["WRONG",Bold,Red], True, "error"]&, allresult];
allscores=Transpose@Fuse[allpairs1, allrightwrong];
allsummary=TableForm[allscores, TableHeadings-> {{"Predicted","Observed","Right/Wrong"},Table[StringJoin["The ", Ordinalize[i], " case"], {i,1,loolen}]}];
Global`ALLROUNDEDVALUES=allroundedValues;
Global`ALLPAIRS3=allpairs3;
Global`ALLRESULT=allresult;
Global`ALLRIGHTWRONG=allrightwrong;
Global`ALLSCORES=allscores;
Global`ALLSUMMARY=allsummary;

allcstatres=cStatistics[allpairs];Global`ALLCSTATRES=allcstatres;
allcstat=allcstatres[[1]];Global`ALLCSTAT=allcstat;


(* ROC curve *)
alltestLabels=allpairs1[[All,2]];
allmodelValues=allpairs1[[All,1]];
allaROCs=Table[ToROCAssociation[{1,0},alltestLabels,Map[If[#>theta,1,0]&,allmodelValues]],{theta,thRange}];
allROCcurve=ROCPlot[thRange,allaROCs,"PlotJoined"->Automatic,"ROCPointCallouts"->False,"ROCPointTooltips"->True,GridLines->Automatic, ImageSize-> size1];
allAUCvalue=NF@N@ROCFunctions["AUROC"][allaROCs];

allrocFuncs={"PPV","NPV","TPR","ACC","SPC","MCC"};allrocFuncTips=Map[#<>", "<>(ROCFunctions["FunctionInterpretations"][#])&,allrocFuncs];
allrocvalues=MapThread[Tooltip[Transpose[{thRange,#1}],#2]&,{Transpose[Map[Through[ROCFunctions[allrocFuncs][#]]&,allaROCs]],allrocFuncTips}];
allROCgraph=ListLinePlot[allrocvalues,Frame->True,ImageSize -> size1,FrameLabel->Map[Style[#,Larger]&,{"threshold, \[Theta]","rate"}],PlotLegends->allrocFuncTips,GridLines->Automatic, PlotRange-> {{0,1},{0,1}}];


(* Predicted/Observed graph *)
xaxes=Table[i,{i,1,Length[allpairs2]}];
Global`XAXES=xaxes;

poarrows=Map[ {Arrowheads[Medium],Thickness[Medium],Brown,Arrow[{{#[[1]], #[[2,1]]}, {#[[1]], #[[2,2]]}}]}&, MakeTwin[ xaxes, allpairs2]];
Global`POARROWS=poarrows;
midline={Green,Line[{{0, 0.5}, {NMax[xaxes], 0.5}}]};

alljudgement=SubsetQ[ {0,0.,1,1.,0`,1`},Union[criterion]]&&MemberQ[ allpairs2[[All,2]],0]&&MemberQ[ allpairs2[[All,2]],1];
Global`ALLJUDGEMENT=alljudgement;

allcomparedimage=ListPlot[ {MakeTwin[ xaxes, allpairs2[[All,1]]], MakeTwin[ xaxes, allpairs2[[All,2]]]}, ImageSize-> 700,
PlotStyle-> {Red, Blue},PlotRange-> All, Epilog ->If[ alljudgement===True,{poarrows, midline}, poarrows]];
Print["                                  "];
Print[allcomparedimage];
Global`ALLCOMPAREDIMAGE=allcomparedimage;

Which[ alljudgement===True,

lesspairs=FilterElement[ allpairs2, 1, #<0.5&];
morepairs=FilterElement[ allpairs2, 1, #>= 0.5&];
Global`LESSPAIRS=lesspairs; Global`MOREPAIRS=morepairs;
LL=Count[lesspairs[[All,2]],0] ; MM=Count[morepairs[[All,2]], 1];
LM=Count[lesspairs[[All,2]],1];ML=Count[morepairs[[All,2]], 0];
Global`LLCOUNT=LL; Global`MMCOUNT=MM;
Global`LMCOUNT=LM; Global`MLCOUNT=ML;
bprights=LL+MM;
bpwrongs=LM+ML ;
Global`BPRIGHTS=bprights; Global`BPWRONGS=bpwrongs;
rightrate=100. bprights/(bprights+bpwrongs);
Global`RIGHTRATE=rightrate;Count[morepairs[[All,2]], 1];
allrwdata={{MM ,ML, MM+ML},{LM,LL,LM+LL}, {MM+LM, ML+LL, MM+ML+LM+LL}};
Global`ALLRAWDATA=allrwdata;
precision=N[MM/(MM+ML)];recall=N[MM/(MM+LM)];
allFvalue=N[2/(1/precision+1/recall)];
Print["                                  "];

PB[];
Print[Style["\[FilledDiamond] 7. Classification Performance with Optimized Model", Bold, fontsize, Blue]];
PB[];
Print[Style["\[FilledSquare] Confusion Matrix", Bold, fontsize]];
Print[Style[TableForm[ allrwdata, TableHeadings->{{"Predicted as 1","Predicted as 0","Total"},{"Observed as 1","Observed as 0", "Total"}}],Bold,fontsize]];
PB[];
Print[Style[StringForm["\t\[EmptyDiamond]\:30001. Sample size: ``  Number of explanatory variables ``", Length@allTrainingData, Length@allNames-1], Bold, fontsize]];
Print[Style[StringForm["\t\[EmptyDiamond] 2. Accuracy(\:6b63\:78ba\:5ea6) ``%", NF[rightrate]], fontsize,Bold, Red]];
Print[Style[StringForm["\t\[EmptyDiamond] 3. Precision(\:7cbe\:5ea6=\:967d\:6027\:7684\:4e2d\:7387) ``%\:3001 Negative Predictive Value(\:9670\:6027\:7684\:4e2d\:7387) ``%", NF[100. MM/(MM+ML)],NF[100. LL/(LL+LM)]], fontsize, Bold, Blue]];
Print[Style[StringForm["\t\[EmptyDiamond] 4. Recall(\:518d\:73fe\:7387=\:611f\:5ea6) ``%\:3001 Specificity(\:7279\:7570\:5ea6) ``%", NF[100. MM/(MM+LM)],NF[100. LL/(LL+ML)]], fontsize, Bold, Blue]];
Print[Style[StringForm["\t\[EmptyDiamond] 5. F1-measure(F1\:5024) ``\:3001 C-statistic(C\:7d71\:8a08\:91cf) `` (`` %)",NF[allFvalue],NF[ allcstat], NF[100*allcstat]], Bold, fontsize]];
Print[Style[StringForm["\t\[EmptyDiamond]\:30006. AUC of ROC curve: ``",allAUCvalue], Bold, Red, fontsize]];
Print[Style[StringForm["\t\[EmptyDiamond]\:30007. Coefficient of Determination: ``",NF@allrsquared], Bold, Blue, fontsize]];

Print["                                  "];


PB[];
ExportExcel[triplebagOM, Numberings[{"Predicted","Rounded","Observed"}], "BuildModelOptimizedResultfromLOO"];
Print[Style[StringForm["\[FilledDiamond] The Raw Result using Optimized Model created from all Selected Models in Leave-One-Out Cross Validation is expressed as TRIPLEBAGOM\n  with {predicted value, rounded value, observed value}."],14, Bold]];

PB[];
PB[];
Print[Style["\[FilledDiamond] 8. Receiver Operating Characteristics Analysis", Bold, fontsize, Blue]];
PB[];
Print[Style["\[FilledDiamond] ROC curve", Bold, fontsize]];
Print@allROCcurve;
PB[];
Print[Style[StringForm["\[FilledDiamond] AUC of ROC curve: ``", allAUCvalue], Bold, Red, fontsize]];
PB[];
PB[];
Print[Style["\[FilledDiamond] Effect of THRESHOLD between positive and negative", Bold, fontsize]];
Print@allROCgraph;
PB[];
Print[Style[StringForm["\[FilledDiamond] 9. Estimated values and Observed values"], Bold, fontsize]];
Print@allsummary;
PB[];
PB[],



alljudgement===False,

allrsquare=DeterminationCoefficient[allobservedValues, allestimatedValues];Global`ALLRSQUARE=allrsquare;
Print["                                  "];
Print[Style[StringForm["\t\[EmptyDiamond] \:4e88\:6e2c\:95a2\:6570\:306b\:3088\:308b Original Data \:306e\:4e88\:6e2c\:306b\:304a\:3051\:308b\:6c7a\:5b9a\:4fc2\:6570(R^2) = ``  (``%)", NF[allrsquare],NF[100. allrsquare]], fontsize, Bold]];
Print["                                  "]
];


Print[Style["\[FilledDiamond] 10. Training Data File with Predicted values and Observed values", fontsize, Bold]];
Print["                             "];

alljoineddata=Fuse[ Map[ Drop[#,-1]&,allTrainingData], triplebagOM];
alljoineditems=Numberings[Join[ Drop[allNames,-1], {"Predicted Value"},{"Rounded Value"}, {Last[allNames]}]];
Global`ALLJOINEDDATA=alljoineddata;
Global`ALLJOINEDITEMS=alljoineditems;
ExportExcel[alljoineddata, alljoineditems, "BuildModelAllDataOptimzedModel"];
(* alljoinedname=StringJoin["BuildModelResult", PresentTime, ".xlsx"];
Export[alljoinedname, JoinDataItems[alljoineddata, alljoineditems] ];
Print[Style[StringForm["\[EmptySquare] The ANALYZED training data with PREDICTED values and OBSERVED targeted values are exported as ``.", alljoinedname], 15, Bold]]; *)
Print[Style[StringForm["\[EmptyDiamond] The ANALYZED Training Data with PREDICTED, ROUNDED and OBSERVED Target Values are expressed as ALLJOINEDDATA and ALLJOINEDITEMS."], 15]];
PB[];
finishingtime=Now[[1]];Global`FINISHINGTIME=finishingtime;
consumedtime=rawSecToHourMinSec[AbsoluteTime@finishingtime-AbsoluteTime@startingtime];
Print[Style[StringForm["Starting time: date ``.``.`` time ``.``.``", 
startingtime[[1]],startingtime[[2]],startingtime[[3]],startingtime[[4]],startingtime[[5]],Round@startingtime[[6]]], Bold, fontsize]];
Print[Style[StringForm["Ending time  : date ``.``.`` time ``.``.``", 
finishingtime[[1]],finishingtime[[2]],finishingtime[[3]],finishingtime[[4]],finishingtime[[5]],Round@finishingtime[[6]]], Bold, fontsize]];
Print[Style[StringForm["Consumed time for caliculation: `` hr `` min `` sec", consumedtime[[1]],consumedtime[[2]],consumedtime[[3]]],fontsize,Bold,Blue]];Global`CONSUMEDTIME=consumedtime;
Print["                           "];
Print[Style[StringForm["\[FilledDiamond] Expected Finish Time with `` cores PC was date ``.``.`` time ``.``.``", corecount,future[[1]],future[[2]],future[[3]],future[[4]],future[[5]],Round@future[[6]]],Bold,fontsize]];

Print[Style[StringForm["\[EmptyDiamond] Expected total time of calculation using the PRESENT `` cores PC was ``.", corecount,realtime], Bold, fontsize]];
timelag=rawSecToHourMinSec[HourMinSecToSec@consumedtime-HourMinSecToSec@realtime];Global`TIMELAG=timelag;
Print[Style[StringForm["\[EmptyDiamond] Time lag between the expected time and the cosumed time: ``.``.``", timelag[[1]],timelag[[2]],timelag[[3]]]]];
Print["                           "];


output="";
Label["endingbuildmodel"];
SetDown[];

Print["                               "];
Print[Style["\[FilledDiamond] The end of the whole analysis", 15]];
Print["                               "];

output
]
