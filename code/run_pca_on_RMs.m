function [pc_score,CRM,dataS,fig] = run_pca_on_RMs(dataS,options)
% -------------------------------------------------------------------------
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

numMaps = numel({dataS.name});
% for pca collect response maps into a single matrix where each
% each row is a flattened response map for each team
rMps = zeros(numMaps,numel(dataS(1).responseMap));
for i=1:numMaps
    rMps(i,:) = dataS(i).responseMap(:);
end

% prelinimary CRM is simply the mean result 
CRM = mean(rMps,1);
CRM = reshape(CRM,size(dataS(1).responseMap));

% calculate pca centered for distance to centroid
warning('off','all'); % if response maps are within machine precision there is a warning 
[COEFF, pc_score, ~, ~,~] = pca(rMps, 'Centered', true); %#ok<ASGLU>
warning('on','all');

% if only 2 teams plot just pca 1
if size(pc_score,2)==1
    pc_score(:,2)  = 0;
end

% record euclidian distance to centroid for each team;
dist2Cntrd = zeros(size(pc_score,1),1);
for i=1:numel(dist2Cntrd)
    dist2Cntrd(i) = sqrt(sum(pc_score(i,:).^2));
    dataS(i).distance2Centroid = dist2Cntrd(i);
end

% If exhaustivePlot is true
if options.exhaustivePlot && size(pc_score,2)>1
    % plot results
    fig = figure('Position',[360 42 405 656]);
    subplot(2,1,2);
    for i=1:size(pc_score,1)  
        %plot(pc_score(i,1),pc_score(i,2),, 'MarkerSize' ,8,'MarkerFaceColor', dataS(i).TeamColor, 'MarkerEdgeColor','k');
        scatter(pc_score(i,1),pc_score(i,2),80,dataS(i).TeamSymbol,'MarkerFaceColor', dataS(i).TeamColor, 'MarkerEdgeColor','k','MarkerFaceAlpha',.2,'MarkerEdgeAlpha',0.8);
        hold on;
    end
    
    % plot centroid (should be zero as using 'Centered' = true)
    cnrdCO = mean(pc_score,1);
    hold on;
    plot(cnrdCO(1),cnrdCO(2),'kx','MarkerSize' ,20,'LineWidth',2)
    grid on;
    xlabel('PCA 1');ylabel('PCA 2');
    %legend({dataS.name}, 'FontSize',6,'Location','best');
    
    % box plot of distance to centroid
    h1 = subplot(2,1,2);
    h2 = subplot(2,1,1);
    if dist2Cntrd~=0
        xlim([0, max(dist2Cntrd)+0.05.*max(dist2Cntrd)])
    end
   
    for i=1:numel(dist2Cntrd)
        plot(dist2Cntrd(i),1+random('normal',0,0.04),'x','MarkerSize',5, 'Color', dataS(i).TeamColor);
        hold on;
    end
    pause(2);
    boxplot(dist2Cntrd,'Orientation','horizontal');
    bp = gca;
    bp.YAxis.TickLabels =[];
    bp.XAxis.TickLabelInterpreter = 'latex';
    bp.YAxis.TickLabelInterpreter = 'latex';
    xlabel('Distance to Centroid');
    set(gca,'xaxisLocation','top');
    h2.Position = [h1.Position(1)    0.4838    h1.Position(3)    0.1256];
    pause(2);
    tightfig;  
    
else 
    fig = false;
end
end



