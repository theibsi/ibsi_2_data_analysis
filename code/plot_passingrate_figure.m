function plot_passingrate_figure(pwA,tol)


maxDiff   = max(max(max([pwA.differenceMapNorm])));

if all(maxDiff(:)==0)
    maxDiff = realmin;
end

tolRange = linspace(0,maxDiff,500);

for it=1:size(pwA,2)
    % init pass rate
    passRate = zeros(size(tolRange));
    Nv = numel(pwA(it).differenceMapNorm(:));
    for i=1:numel(tolRange)
        passRate(i) = 100.* sum((pwA(it).differenceMapNorm(:)./tolRange(i)) <= 1) /Nv;
    end
    
    % express the absolute difference Norm tolerance as a percentage
    plot(tolRange,passRate,'-', 'Color', pwA(it).TeamColor, 'LineWidth',2)
    pwA(it).gammaMapStyle.tol = tolRange;
    pwA(it).gammaMapStyle.passRate = passRate;
    hold on;
     
end
xlabel('$\gamma$ tolerance (\%)','Interpreter','latex')
ylabel('Passing Rate / R$_\gamma$ (\%)','Interpreter','latex')
ylim([0 100]);
xlim([0 5])
if maxDiff<tol
    xlim([0 tol+0.01]);
else
    xlim([0 maxDiff]);
end
grid on;
hold on; 
plot([tol,tol],[0,100],'r--');
tl = sprintf('Each RM vs CRM (N = %i)',numel(pwA));
title(tl);
%legend(fixNamesForLegend({pwA.maps_compared}), 'location','best','Interpreter','latex');
end
