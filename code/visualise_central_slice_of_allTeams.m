% Visualise central slice of all teams in dataS
% Additional script used to visualise central slice of different teams.
% requires main_Analysis_Phase1 to have been run to get the dataCell;
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------
%
savdir = fullfile('../results/tmp');
mkdir(savdir);


c_dataS = dataCell{6,2}; % Pick a given filter test from dataCell
fig = figure('Position',[147 47 378 651]) ;
ha = tight_subplot(2,6,[.05 .05],[.1 .01],[.01 .01]);
slice = 32;

for i=1:numel({c_dataS.name})
    subplot(ha(i));
    im =  c_dataS(i).responseMap(:,:,slice);
    w = [min(im(:)), max(im(:))];
    i1 =imagesc(im,w); axis image; colormap gray; axis off;
    title(c_dataS(i).name);
end
ha(8).Visible = 'off';
tightfig;

print('-dpng', fullfile(savdir,'centSlices.png'),'-r500')

options.exhaustivePlot = 0; 
options.tol  = 1; % max of 1% voxel-wise variation for pairwise analysis
[~,crm,~] = run_consensus_analysis(c_dataS,options);

figure;
im =  crm(:,:,slice);
w = [min(im(:)), max(im(:))];
i1 =imagesc(im,w); axis image; colormap gray; axis off;
tightfig;
print('-dpng', fullfile(savdir,'crm.png'),'-r500')


