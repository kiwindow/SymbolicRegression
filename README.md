# SymbolicRegression

This repository supplies codes to perform Symbolic Regression via Genetic Programming using Wolfram Language (or Mathematica) and DataModeler.
Wolfram Language is provided by Wolfram Research Inc. https://www.wolfram.com
DataModeler is provided by Evolved Analytics Inc. https://evolved-analytics.com

DataModeler runs on Wolfarm Language. Although it also has a stand alone mode, this repository offers codes written in Wolfarm Languge to run DataModeler on it to perform Symbolic Regression validated with the Hold-Out method and the Leave-One-Out Cross Validation method.

## 1. Contents

There are two program files written in Wolfam Language in this repository.

1. init.m : This file provides user-defined functions.
2. BuildModel.m : This file provides a BuildModel function to perform Symbolic Regression via Genetic Programming, especially to perform dichotomous classification in machine learning.

The init.nb and BuildModel.nb files are original files for the above programs.
There are also two dataset files for Machine Learning.

1. newMaxGD2.xlsx
2. newMaxGD2.csv
   
The contents of them are identical with each other. The last column is the dichotomous target variable. The rest columns are explanatory variables.

There are also two other program files written in Python.

1. MLwithLOO_CV.ipynb
2. FeatureImportanceScores.ipynb

The MLwithLOO_CV.ipynb contain codes to perform leave-one-out cross validation on dichotomous classification problem with python. It is coded for use on Google Colaboratory.
The FeatureImportanceScoers.ipynb is also for use on Colaboratory to calculate feature importance values with several statistical and machine learning methods.

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

The BuildModel[] function uses a function called SymbolicRegression provided by DataModeler.

4. Include the desktop directory into the default pass in Wolfram Language (Mathematica)

$Path = Join[{ ToFileName[{$HomeDirectory, "desktop"}]}, $Path];

This procedure allows the user to read in dataset file located on the desktop into Mathematica.

5. Execute the BuildModel[] function and follow the instructions in the API windows that appear one after another.

BuildModel[]

## 3. ReadMeFirst file

More detailed usage procedures are written in the ReadMeFirst notebook in this repository.

## 4. A sample result of the Symbolic Regression validated with leave-one-out crossvalidation method 

A sample of Wolfram Language notebook calculated with newMaxGD2 using BuildModel and DataModeler is available from the following link.

https://www.dropbox.com/s/6u5d6nk0zq9j173/20201208%20LOO%20newMaxGD2%2030sec%20if%200.80%204seeds%20Normalized1%20A.nb?dl=0

This file can be opened with Wolfram Player, which is a free software that can be obtained from the following site.

https://www.wolfram.com/player/


