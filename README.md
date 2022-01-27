# SymbolicRegression

This repository supplies codes to perform Symbolic Regression via Genetic Programming using Wolfram Language (or Mathematica) and DataModeler.
Wolfram Language is provided by Wolfram Research Inc. https://www.wolfram.com
DataModeler is provided by Evolved Analytics Inc. https://evolved-analytics.com

DataModeler runs on Wolfarm Language. Although it also has a stand alone mode, this repository offers codes written in Wolfarm Languge to run DataModeler on it to perform Symbolic Regression validated with the Hold-Out method and the Leave-One-Out Cross Validation method.

## 1. Contents

There are two program files written in Wolfam Language here.

1. init.m : This file provides user-defined functions.
2. BuildModel.m : This file provides a BuildModel function to perform Symbolic Regression via Genetic Programming.

The init.nb and BuildModel.nb files are original files for the above programs.
There are also two dataset files for Machine Learning.

1. newMaxGD2.xlsx
2. newMaxGD2.csv
   
Ths contents of them are identical with each other. The last column is the dichotomous target variable. The rest columns are explanaatory variables.

## 2. Quick Start

Execute the following commands on the notebook of Wolfram Language.
DataModeler must be installed beforehand on the Wolfram Language.

0. Save a notebook of Wolfram Language (or Mathematica) in an appropriagte directory

All the results of calculation are stored automatically in the directory where the notebook was saved.

1. Import user defined functions

Import["https://raw.githubusercontent.com/kiwindow/SymbolicRegression/main/init.m"]

2. Import DataModeler

Needs["DataModeler`"]

DataModeler must be imported first, and BuildModel function should be defined thereafter.

3. Import BuildModel function to use DataModeler

Import["https://raw.githubusercontent.com/kiwindow/SymbolicRegression/main/BuildModel.m"]

4. Include the desktop directory into the default pass in Wolfram Language (Mathematica)

$Path = Join[{ ToFileName[{$HomeDirectory, "desktop"}]}, $Path];

This procedure allows the user to read in dataset file located on the desktop into Mathematica.

5. Execute the BuildModel[] function and follow the instructions in the API windows that appear one after anther.

BuildModel[]

## 3. ReadMeFist file

More detailed usage procedures are written in the ReadMeFirst notebook in this repository.
