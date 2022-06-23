      
# README #

### What is this repository for? 

- Analysis of IBSI2 Phase 1 submission data to assess consensus of contributing teams.

### Dependencies 

Code developed in Matlab 2020b (and on mac OS). It requires the following Matlab toolboxes:

 - Image Processing Toolbox
 - Statistics and Machine Learning Toolbox

### How to run

- Navigate to code folder in Matlab (use as working directory).
- Ensure all child folders (in misc_functions) are on Matlab path.
	 - e.g. run addpath(genpath('./')) when in the code folder.
- Two ways to run analysis: 
	 1. Using a script: 
		 - run the script: *main_Analysis_Phase1.m*
	 2. Using a function and json configuration file
		 - *run_Analysis_Phase1("configAnalysisPhase1.json") *

The second approach is more streamlined and only computes key plots (mainly for website display of results).   

### Options 

Depending on which approach is used, the options for analysis are either defined directly in the script or in the json configuration file.

  The options are as follows:

 - **folderName**:
	 -  Name of data folder downloaded from  submission website. 
	 -  e.g. "submissions-20XX-XX-XX",
- **folderLocation**:
	 -	The directory where folderName is located.
 - **savedir** : 
	 -	The directory where analysis output will be saved.
 - **tol**
	 -	The tolerance used for acceptable voxel-wise variation for the pairwise analysis. Each potential consensus response map (CRM) is compared pairwise with each submission.
	 - For this analysis we use 1%.
 - **saveCRMs**: 
	 -  Option to save valid CRMs if found (1= true, 0 = false)
 - **exhaustivePlot**
 	-  Option to exhaustively plot all PCA and passing rate figures for each iteration towards consensus. This takes a while. (1= true, 0 = false)


The second approach is a more streamlined analysis to generate only the necessary plots for the website. It allows for 

### Contact

pwhybra (whybrap@cardiff.ac.uk)

https://theibsi.github.io/contact/