function [dataCell, nEmptyTests] = plot_heatmap_total_submissions(dataCell,teamInfo,saveDIR)
% -------------------------------------------------------------------------
% IBSI2 Data Analysis - plot heatmap of total submissions
%
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

[n_dataCell, nEmptyTests] = addemptyFilterTests(dataCell);



teamList =  teamInfo(:,1);

%teamList = {'CERR';'Cardiff University';'King''s College London';'LIFEx';'LaTIM';'McGill';'NCT Dresden';'UCSF';'UK';'UPenn';'USZ';'UdeS';'Veneto Institute of Oncology ';'moddicom'};




% loop through filter tests and find which teams contributed
nteams = size(teamList,1);
ntests = size(n_dataCell,1);
b = zeros(ntests,nteams);
for i=1:ntests
    dataS = n_dataCell{i,2};
    
    if isempty(n_dataCell{i,2})
        continue 
    else
        for j=1:nteams
            b(i,j) = any(contains({dataS.name},teamList{j}));
        end
    end
end

%plot heatmap 
teamList = strrep(teamList,'UK', 'UK-Augsburg');
% Settings 
gridline = '--'; % gridline examples  :, -, -. or --.
% Create a custom colormap 
cmap_tmp = [230, 231 ,231; 76,187 152]./255;

figure('Position', [-1618 829 1239 418]);
h =heatmap_FE(b',n_dataCell(:,1),teamList,[],'ColorMap', cmap_tmp, 'NaNColor', [0.5,0.45,0.45],...
    'GridLines',gridline ,'colorbar', true,'ShowAllTicks', true, 'TickFontSize', 12,...
    'TickAngle', 50);


% Add colorbar 
cb = colorbar;
cb.Ticks = [0 0.25 0.75 1];
cb.TickLabels = {'','Not implemented', 'Submitted result',''};
cb.FontSize = 15;

xlabel('Filter Test');

savName = fullfile(saveDIR,'Submission_overview_heatmap.png');
% %print('-dpdf', savName,'-r500', '-bestfit')
print('-dpng', savName,'-r500')

%h.Parent.YAxis.FontSize = 12;



end






















