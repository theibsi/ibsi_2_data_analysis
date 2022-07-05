function run_Analysis_Phase1(jsonConfigFile)
% -------------------------------------------------------------------------
% Streamlined function that runs the core part of the analysis from the
% main analysis script (main_Analysis_Phase1.m) but using a 
% jsonConfigFile instead.
%
% -------------------------------------------------------------------------
% @uthor pwhybra (whybrap@cardiff.ac.uk)
% -------------------------------------------------------------------------

if nargin<1
   jsonConfigFile = 'configAnalysisPhase1.json';
end

% %  uncomment so that figures dont pop up;
% set(0,'DefaultFigureVisible','off');

% Read jsonConfigFile and get options
% -------------------------------------------------------------------------
str      = fileread(jsonConfigFile); 
options  = jsondecode(str);
options.savedir = check_Dir(options.savedir);

% data import
% -------------------------------------------------------------------------
folderName =  options.folderName;
f_dir      = fullfile(options.folderLocation,folderName); 

unzip_teamFiles(f_dir);  
[dataCell,teamInfo] = batch_import_team_files(f_dir);

% Consensus assessment for each filter test 
% -------------------------------------------------------------------------
resultsCell  = cell(size(dataCell,1)+1,7);
resultsCell(2:end,1)  = dataCell(:,1);

% create copy to update
validTeamsDataCell = dataCell;

% loop through filter tests 
for it=1:size(dataCell,1)
    options.cFT     = dataCell{it,1};
    fprintf('\n\nCurrent Filter test: %s.\n',options.cFT);
    [r,~,validTeamsDataCell{it,2}] = run_consensus_analysis(dataCell{it,2},options);
    resultsCell(it+1,2:7) = struct2cell(r)'; 
end

% Save results table (add headings)
% -------------------------------------------------------------------------
resultsCell(1,:) = {'Filter Test',...
                    'Number Submitted',...
                    'Consensus Reached ?',...
                    'Matching Response Maps',...
                    'Consensus Strength (measure-1)',...
                    'Consensus Stability (measure-2)',...
                    'Optimal gamma'};
saveName = fullfile( options.savedir,'results_table1.xlsx');

c = resultsCell(2:end,:);
rT = cell2table(c);
rT.Properties.VariableNames = resultsCell(1,:);
writetable(rT ,saveName,'WriteMode','replacefile');

% plot bar chart of consensus
% -------------------------------------------------------------------------
[~, nEmptyTests] = addemptyFilterTests(dataCell);
plot_bar_chart_consensus_strength(resultsCell,nEmptyTests,options.folderName, options.savedir);
plot_heatmap_consensus_strength(dataCell,validTeamsDataCell,teamInfo,options.folderName,options.savedir);

if (ismcc || isdeployed)
    exit
end

end













