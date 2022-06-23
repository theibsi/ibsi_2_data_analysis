function create_before_after_iteration_plots(dataCell)



options.exhaustivePlot = 0;
options.tol  = 1;

%loop through filtertests
for it=1:size(dataCell,1)
    close all;
    cTest = strrep(dataCell{it,1},'.','_');
    saveName = [cTest '_intial.png'];
    dataS    = dataCell{it,2};
    create_current_it_plot(dataS,options,saveName)
    [r,crm,valid_dataS] = run_consensus_analysis(dataS,options);

    if ~isempty(valid_dataS)
        saveName = [cTest '_valid.png'];
        create_current_it_plot(valid_dataS,options,saveName)
    end
end

end

function create_current_it_plot(dataS,options, saveName)
[pc_score,crm,dataS,~] = run_pca_on_RMs(dataS,options);

%fig = figure('Position',[360 469 545 229]); 
fig = figure('Position',[414 424 473 233]); 

ha = tight_subplot(1,2,[.05 .1],[.15 .1],[.15 .01]);
subplot(ha(1));
for i=1:size(pc_score,1)
    %plot(pc_score(i,1),pc_score(i,2),, 'MarkerSize' ,8,'MarkerFaceColor', dataS(i).TeamColor, 'MarkerEdgeColor','k');
    scatter(pc_score(i,1),pc_score(i,2),130,dataS(i).TeamSymbol,'MarkerFaceColor', dataS(i).TeamColor, 'MarkerEdgeColor','k','MarkerFaceAlpha',1,'MarkerEdgeAlpha',0.7);
    hold on;
end

% plot centroid (should be zero as using 'Centered' = true)
cnrdCO = mean(pc_score,1);
hold on;
plot(cnrdCO(1),cnrdCO(2),'kx','MarkerSize' ,20,'LineWidth',2)
grid on;
xlabel('PCA 1');ylabel('PCA 2');

rn = range([min(pc_score(:,1)),max(pc_score(:,1))]).*0.2;
if rn~=0
    xlim([min(pc_score(:,1))-rn,max(pc_score(:,1))+rn])
end
    
ax = gca;
ax.FontSize = 10; 
box on;
% ax.XTick = linspace(min(ax.XTick),max(ax.XTick),3);
% ax.YTick = linspace(min(ax.YTick),max(ax.YTick),3);

pwA  = pairwise_diff_RMs_vs_CRM(dataS,crm,options.tol);
subplot(ha(2));
plot_passingrate_figure(pwA,options.tol);
xlim([0,5])

%tightfig;
%pause(2);
print('-dpng', ['../results/tmp' saveName],'-r300'); 
end