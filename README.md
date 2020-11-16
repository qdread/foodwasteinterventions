# foodwasteinterventions

[![DOI](https://zenodo.org/badge/258221572.svg)](https://zenodo.org/badge/latestdoi/258221572)

Read, Q. D. & Muth, M. K. (2020). Cost-effectiveness of Four Food Waste Interventions: Is Food Waste Reduction a “Win Win?” *Resources, Conservation & Recycling*, in press

Code to reproduce analysis presented in the manuscript.

## Description

Currently, this repository includes all code and intermediate output to reproduce the main analysis. 
The scripts in the folder `0_preprocessing` refer to raw data not currently included in the repository.
The raw data are archived at this repository on figshare.com.

Please refer to `script_pipeline.md` for descriptions of scripts in sequential order.

You may reproduce the analysis by running everything in the `all_analysis.R` script. If you begin at Step 1 (analysis), all data needed are included in this repository.

If you would like to begin at Step 0 and reproduce the preprocessing steps, first download all data from the Figshare repository. Unzip all zipped files. Replace the placeholder filepath `fp_rawdata` in the `all_analysis.R` script with the location of the raw data. 

## Status of code testing

The code in the folder `0_preprocessing` was successfully tested in R 4.0.2 on Linux. 

The code in the folders `1_uncertaintyanalysis` and `2_vis` was successfully tested in R 4.0.0 on Windows and R 4.0.2 on Linux.

## Note about input-output model

This repository contains the code needed to reformat the 2012 Bureau of Economic Analysis input-output tables to use the same
coding schema as the 2007 tables so that they are compatible with the USEEIO environmentally extended input-output model. However, it does not include the model build scripts and satellite tables needed to calculate the environmental impacts per dollar output of each of the BEA industries. Those precalculated values are included in the repository. Other than that, all the analysis presented in the manuscript can be reproduced directly from the raw data included here.

This Readme last modified by QDR, 16 November 2020
