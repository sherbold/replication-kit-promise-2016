[![DOI](https://zenodo.org/badge/84547874.svg)](https://zenodo.org/badge/latestdoi/84547874)

Introduction
============
Within this archive you find the replication package for the paper [Hidden Markov Models for the Prediction of Developer Involvement Dynamics and Workload](https://doi.org/10.1145/2972958.2972960) by Verena Honsel, Steffen Herbold, and Jens Grabowski published in the proceedings of the Proceeding
12th International Conference on Predictive Models and Data Analytics in Software Engineering (PROMISE 2016). The aim of this replication package is to allow other researchers to replicate our results with minimal effort. 

The content in this repository is equal to the [linked ZIP archive in the paper](https://filepool.informatik.uni-goettingen.de/publication/swe/2016/vh-material-promise2016.zip), with the exception of this README.md file, which was added. 

Requirements
============
- Java 7.
- R (tested with version 3.3.2, other should work). 
- Only tested on Windows 8 and Windows 10. Should work on Linux, but may require minor adoptions (different paths, differences between .sh and .bat scripts, etc.). 

Contents
========
This replication kit contains:
- The eval_all.R script, which is the main R script for the execution of all experiments reported on in the paper.
- The hmm-helpers.R script, which contains some functions required by eval_all.R. 
- The compiled rectanglelearner.jar file for the execution of the threshold learning algorithm (source code may be added upon request). 
- The create_thresholds.bat Windows batch file that contains an example for the execution of the treshold learning algorithm on the data from the Ant project. 
- Six subfolders with the data for each project used in the case study. 

How does it work?
=================
The eval_all.R script requires several R libraries, which can be installed by typing the following commands in your R console. 
```R
install.libraries("HMM")
install.libraries("lattice")
install.libraries("ggplot2")
install.libraries("mhsmm")
install.libraries("mass")
install.libraries("fitdistrplus")
install.libraries("stringr")
install.libraries("class")
install.libraries("monomvn")
```
Afterwards, you must update the path in the eval_all.R script to match the location of the replication kit on your local machine (Line 12 of the file). 

Now you should be able to run the contents of the eval_all.R and replication our results. 
