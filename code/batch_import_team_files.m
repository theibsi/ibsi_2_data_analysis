function [dataCell,teamInfo] = batch_import_team_files(fileDIR)
% -------------------------------------------------------------------------
% FOR IBSI2 Data Analysis - read in files into a dataCell.
%
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

% find all unique teams
% ----------------------------------
filelist  = dir(fullfile(fileDIR, '**/*.nii'));
teamNames = cellfun(@(s) regexp(s, '(?<name>.*)-[0-9]+-.*\..*', 'names').name, {filelist.name}, 'UniformOutput', false);
uniqueTeamNames = unique(cellfun(@(x) x,teamNames,'UniformOutput',false))';
nteams = size(uniqueTeamNames, 1);

% case-insensitive sort of team names
[~, idx]  = sort(upper(uniqueTeamNames));
teamInfo  = teamNames(idx)';


% Assign ID and colour
% ----------------------------------
c = flip(jet(nteams)); %colorcube(size(teamInfo,1)+1);
%rng(200);
%c = c(randperm(nteams),:); 
%rng('shuffle');
smb = repmat({'o','d','s'},[1,nteams])';
for i=1:nteams
    teamInfo{i,2} = sprintf('%04d',i);
    teamInfo{i,3} = c(i,:);%randi([0 255],[1,3])./255;
    teamInfo{i,4} = smb{i};
end


% import all files (each folder is a different filter test)
% ---------------------------------------------------------
filterTests = unique({filelist.folder});

% Put 10a and 10b etc in correct place(requires sort_nat from fileExchange)
filterTests = sort_nat(filterTests); 
dataCell    = cell(numel(filterTests),2);
testNames   = getTestNamesFromFolders(filterTests);

% Put 10a and 10b etc in correct place 
testNames = sort_nat(testNames); 

for it=1:numel(filterTests)
    
    % find all nifti files in fileLOC and load
    fls = dir(fullfile(filterTests{it},'*.nii'));
    
    % load files into a struct
    dataS = struct();
    for i=1:size(fls,1)
        try
            % current file info
            crrntFile  = fullfile(fls(i).folder, fls(i).name);
            crrntTeam  = strsplit(fls(i).name,'-');
            dataS(i).name  = crrntTeam{1};
            
            % load in response map
            [dataS(i).responseMap, dataS(i).info] = nifti_Extract(crrntFile);
            
            % add team ID, color, symbol
            [dataS(i).ID,dataS(i).TeamColor,dataS(i).TeamSymbol] = getTeamID(dataS(i).name,teamInfo);
            
        catch
            warning('File %s failed to load...skipped.', fls(i).name(1:end-4));
        end
    end
    
    % remove empty response maps (if load failed)
    noRM        = cellfun(@isempty,{dataS.ID});
    dataS(noRM) = [];
    
    dataCell{it,1} = testNames{it};
    dataCell{it,2} = dataS;
end
end

function [ID, c, s] = getTeamID(crrntTeam,teamInfo)
indx = find(contains(teamInfo(:,1),crrntTeam));
ID   = teamInfo{indx,2};
c    = teamInfo{indx,3};
s    = teamInfo{indx,4};
end

function testNames = getTestNamesFromFolders(filterTests)
if ispc 
    testNames   = cellfun(@(s) strsplit(s,'\'), filterTests, 'UniformOutput', false);
else
    testNames   = cellfun(@(s) strsplit(s,'/'), filterTests, 'UniformOutput', false);
end
testNames   = unique(cellfun(@(x) x{end},testNames,'UniformOutput',false))';
end

