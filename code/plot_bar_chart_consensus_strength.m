function plot_bar_chart_consensus_strength(resultsCell,nEmptyTests, figTitle, saveDIR)
% -------------------------------------------------------------------------
% IBSI2 Data Analysis - plot bar chart of consensus strength for all filter
% tests
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------
if nargin==1
    nEmptyTests = 0;
end

% count consensus strength
con_cats  = flip({'very strong','strong','moderate','weak','none'});
cols      = [166 166 166; 242,26,0; 235,204,42 ; 120,183,197; 59,154,178]./255;
con_count = zeros(numel(con_cats),2);
for i=1:numel(con_cats)
    con_count(i,1) = sum(strcmp(resultsCell(2:end,5),con_cats{i}));
end

% Add empty tests to the "none" consensus 
con_count(1) = con_count(1) + nEmptyTests;

% plot
f = figure('Position',[543 80 276.5340 427.2329]);
H = barh(con_count,'stacked','BarWidth', 0.6);
ax = gca;
ax.YTickLabel = con_cats;
%box off;
grid
xlim([0,20])
xlabel('Count');


for i=1:numel(H)
    H(i).FaceColor = 'flat';
    H(i).CData = cols;
end

tightfig;

savName = fullfile(saveDIR,['consensus_strength_' figTitle '.png']);
% %print('-dpdf', savName,'-r500', '-bestfit')
print('-dpng', savName,'-r500')
con_count

