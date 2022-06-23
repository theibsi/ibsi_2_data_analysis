function plot_heatmap_consensus_strength(dataCell,validTeamsDataCell,teamInfo,figTitle, saveDIR)
% -------------------------------------------------------------------------
% IBSI2 Data Analysis - plot heatmap of consensus strength for teams
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
        continue;
    else
        for j=1:nteams
            b(i,j) = any(contains({dataS.name},teamList{j}));
        end
    end
end


[valid_DataCell, ~] = addemptyFilterTests(validTeamsDataCell);

% loop through filter tests and find which teams contributed
nteams = size(teamList,1);
ntests = size(valid_DataCell,1);
b2 = zeros(ntests,nteams);
for i=1:ntests
    dataS = valid_DataCell{i,2};
    
    if isempty(valid_DataCell{i,2})
        continue; 
    else
        for j=1:nteams
            b2(i,j) = any(contains({dataS.name},teamList{j}));
        end
    end
end

% Combine results to find those that are valid 
b3 = b+b2+1;

% set not implemented to -1
b3(b3(:)==1)=0;

% loop through rows, if contain ones but no twos, change all the ones to 0 to represent no
% consensus as "all" are deviating
for i=1:size(b3,1)
    tmp = b3(i,:);
    if any(ismember(tmp,2)) && ~any(ismember(tmp,3))
        
        tmp(ismember(tmp,2)) = 1;
        b3(i,:) = tmp;
        
    end
        
end


%plot heatmap 
teamList = strrep(teamList,'UK', 'UK-Augsburg');
% Settings 
gridline = '--'; % gridline examples  :, -, -. or --.
% Create a custom colormap 
cmap_tmp = [ 230, 231 , 231; 255, 0, 0; 247, 147, 3; 76, 187, 152]./255;

% plot heatmap
figure('Position', [-1618 829 1239 418]);
h =heatmap_FE(b3',n_dataCell(:,1),teamList,[],'ColorMap', cmap_tmp, 'NaNColor', [0.5,0.45,0.45],...
    'GridLines',gridline ,'colorbar', true,'ShowAllTicks', true, 'TickFontSize', 12,...
    'TickAngle', 50);


% Add colorbar 
cb = colorbar;
cb.Ticks = [0.3750 1.1250 1.8750 2.6250 ];
cb.TickLabels = {'Not implemented','No consensus', 'deviating', 'In consensus',};
cb.FontSize = 15;

xlabel('Filter Test');

title(figTitle)

savName = fullfile(saveDIR, ['consensus_heatmap_' figTitle '.png']);
% %print('-dpdf', savName,'-r500', '-bestfit')
print('-dpng', savName,'-r500')

%h.Parent.YAxis.FontSize = 12;

end






















