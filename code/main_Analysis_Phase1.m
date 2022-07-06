% Main analysis for IBSI-2 Phase 1 submissions 
% -------------------------------------------------------------------------
%   Run script for main analysis of Phase 1. 
%   Set working directory to the location of this script and make sure all
%   child folders are on the path, i.e. run addpath(genpath('./'));
% -------------------------------------------------------------------------
% @uthor pwhybra (whybrap@cardiff.ac.uk)
% -------------------------------------------------------------------------

clear; close('all'); clc;

% ENSURE FOLDERS ON PATH - run: addpath(genpath('./'));

% DATA IMPORT
% -------------------------------------------------------------------------
folderName =  'submissions-2022-07-06'; 
f_dir = fullfile('..','data',folderName); 
unzip_teamFiles(f_dir);  
[dataCell,teamInfo] = batch_import_team_files(f_dir);


% OPTIONS 
% -------------------------------------------------------------------------
% MAX ACCEPTED VOXEL-WISE VARIATION (for pairwise analysis (set to 1%))
options.tol  = 1; 
options.savedir = fullfile( '..','results');
options.saveCRMs = 1;
check_Dir(options.savedir);

% EXHAUSTIVE PLOTS (takes a while, not needed)
options.exhaustivePlot = false; 

% PLOT BARCHART AND HEATMAP OF SUBMISSIONS
% -------------------------------------------------------------------------
[dataCell,nEmptyTests] = plot_bar_chart_total_submissions(...
    dataCell, teamInfo, options.savedir);

plot_heatmap_total_submissions(dataCell, teamInfo, options.savedir);

% CONSENSUS ASSESSMENT FOR EACH FILTER TEST 
% -------------------------------------------------------------------------
resultsCell  = cell(size(dataCell,1)+1,7);
resultsCell(2:end,1)  = dataCell(:,1);

% CREATE COPY OF DATACELL TO UPDATE
validTeamsDataCell = dataCell;

% LOOP THROUGH FILTER TESTS IN DATACELL
for it=1:size(dataCell,1)
    options.cFT     = dataCell{it,1};
    fprintf('\n\nCurrent Filter test: %s.\n',options.cFT);
    [r,~,validTeamsDataCell{it,2}] = run_consensus_analysis(dataCell{it,2}, options);
    resultsCell(it+1,2:7) = struct2cell(r)'; 
end

% SAVE RESULTS TABLE
% -------------------------------------------------------------------------
resultsCell(1,:) = {'Filter Test',...
                    'Number Submitted',...
                    'Consensus Reached ?',...
                    'Matching Response Maps',...
                    'Consensus Strength (measure-1)',...
                    'Consensus Stability (measure-2)',...
                    'Optimal gamma'};
saveName = fullfile( '..','results','results_table1.xlsx');

c = resultsCell(2:end,:);
rT = cell2table(c);
rT.Properties.VariableNames = resultsCell(1,:);
writetable(rT ,saveName,'WriteMode','replacefile');

% PLOT BAR CHART OF CONSENSUS
% -------------------------------------------------------------------------
plot_bar_chart_consensus_strength(resultsCell, nEmptyTests, folderName, ...
    options.savedir);

plot_heatmap_consensus_strength(dataCell, validTeamsDataCell, teamInfo, ...
    folderName, options.savedir);

% VISUALISATION GUI
% -------------------------------------------------------------------------
GUI_IBSI2_RM_comparision(dataCell);













