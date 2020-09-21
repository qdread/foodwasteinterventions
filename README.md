# foodwasteinterventions

[![DOI](https://zenodo.org/badge/258221572.svg)](https://zenodo.org/badge/latestdoi/258221572)

Analysis of four food waste interventions

Work for a manuscript currently under review at *Resources, Conservation and Recycling*

Please refer to `prose_pipeline.md` and `script_pipeline.md` for descriptions of the code in prose form and in a sequential list of scripts to run, respectively.

Currently, this repository includes all code and intermediate output to reproduce the main analysis. 
The scripts in the folder `0_preprocessing` refer to raw data not currently included in the repository.

The code in the folders `1_uncertaintyanalysis` and `2_vis` was successfully tested in R 4.0.1 on Windows and R 4.0.2 on Linux.

## Note about input-output model

This repository contains the code needed to reformat the 2012 Bureau of Economic Analysis input-output tables to use the same
coding schema as the 2007 tables so that they are compatible with the USEEIO environmentally extended input-output model. However, it does not include the model build scripts and satellite tables needed to calculate the environmental impacts per dollar output of each of the BEA industries. Those precalculated values are included in the repository. Other than that, all the analysis presented in the manuscript can be reproduced directly from the raw data included here.

This Readme last modified by QDR, 17 September 2020
