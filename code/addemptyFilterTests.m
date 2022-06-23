function [n_dataCell, nEmptyTests] =  addemptyFilterTests(dataCell)
% Add in missing filter tests with no submissions (if any) for plotting. 
% -------------------------------------------------------------------------
% @uthor pwhybra 
% -------------------------------------------------------------------------

fullTestList = {'1.a.1','1.a.2','1.a.3','1.a.4','1.b.1','2.a','2.b','2.c','3.a.1','3.a.2','3.a.3','3.b.1',...
    '3.b.2','3.b.3','3.c.1','3.c.2','3.c.3','4.a.1','4.a.2','4.b.1','4.b.2','5.a.1','5.a.2','6.a.1','6.a.2',...
    '7.a.1','7.a.2','8.a.1','8.a.2','8.a.3','9.a','9.b.1','9.b.2','10.a','10.b.1','10.b.2'};

nEmptyTests = 0;
n_dataCell  = cell(numel(fullTestList),2);

n_dataCell(:,1) = fullTestList;
for i=1:numel(fullTestList)
    ind = find(startsWith(dataCell(:,1),fullTestList{i}));
    if isempty(ind)
        n_dataCell{i,2} = [];
        nEmptyTests = nEmptyTests + 1;
    else 
        n_dataCell{i,2} = dataCell{ind,2};    
    end
end
end
