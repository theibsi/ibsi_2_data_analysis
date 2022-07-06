function [dataCell, nEmptyTests] = plot_bar_chart_total_submissions(dataCell, teamInfo, saveDIR)
% -------------------------------------------------------------------------
% IBSI2 Data Analysis - plot bar chart of total submissions
%
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

[n_dataCell, nEmptyTests] = addemptyFilterTests(dataCell);

% loop through filter tests and find which teams contributed
nteams = size(teamInfo,1);
ntests = size(n_dataCell,1);
b = zeros(ntests,nteams);
for i=1:ntests
    dataS = n_dataCell{i,2};
    
    if isempty(n_dataCell{i,2})
        continue 
    else
        for j=1:nteams
            b(i,j) = any(contains({dataS.name},teamInfo{j,1}));
        end
    end
end

%plot stacked bar
f = figure('Position',[18 106 1753 588]);
H = bar(b,'stacked','BarWidth', 0.65);
ax = gca;
ax.XTick = 1:ntests;
ax.XTickLabel = n_dataCell(:,1);

legend(teamInfo(:,1),'Orientation','horizontal','Location','north')
box off;
ylim([0,nteams+1])
ylabel('Submission number');
xlabel('Filter test');
tightfig;

% change colors of stack to set team colors
for i=1:numel(H)
    H(i).FaceColor = 'flat';
    H(i).CData = repmat(teamInfo{i,3}, [size(H(i).CData,1),1]);
end

savName = fullfile(saveDIR, 'Submission_overview.png');
% %print('-dpdf', savName,'-r500', '-bestfit')
print('-dpng', savName,'-r500')

fprintf('The total number of submissions = %i', sum(b(:)));
end






