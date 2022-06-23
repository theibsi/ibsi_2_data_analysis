function [r,crm,c_dataS] = run_consensus_analysis(c_dataS,options)
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

% record if consensus reached
cnsnsR = 0;
% maximum difference tolerance (normally use 1%);
tol    = options.tol;

try
    nteams = numel({c_dataS.name});
catch
    nteams = 0;
end

% Don't iteratively go below 50% of initial number of response maps
it_max = round(nteams/2);

for it=1:it_max
    % Run PCA and pairwise analysis on current set of response maps
    [~,crm,c_dataS,fig] = run_pca_on_RMs(c_dataS,options);
    pwA  = pairwise_diff_RMs_vs_CRM(c_dataS,crm,tol);
    
    % if exhaustive plotting, add passing rate plot
    if options.exhaustivePlot && size(c_dataS,2)>1
        axes('Parent',fig,'OuterPosition',[1 0 1 0.684]);
        pause(3);
        plot_passingrate_figure(pwA,tol); tightfig;
        savedir = fullfile(options.savedir,options.cFT);
        check_Dir(savedir);
        
        savName = fullfile(savedir, ['iterations_' num2str(it)]);
        print('-dpdf', savName,'-r1000', '-bestfit'); close;
    end
    
    % Are all teams within tol of CRM?
    if all([pwA.withinTol]) && numel([pwA.withinTol]) >1
        fprintf('\tConsensus found.\n')
        cnsnsR = 1;
        break;
    else
        if it~=it_max
            [~,indx] = max([c_dataS.distance2Centroid]); % team furthest away (PCA)
            fprintf('\tNo consensus, removed outlier team (%s).\n',c_dataS(indx).name)
            c_dataS(indx) = []; % remove outlier response map
        else
            fprintf('\tNo consensus reached for this filter test.\n')
            crm = false;
            c_dataS = [];
        end
    end
end

% summarise results
r.TotalSubmitted = nteams;
if  cnsnsR && nteams>1
    r.isvalidCRM = 'yes';
    r.measure_1      = numel({c_dataS.name});
    r.consensusLevel = get_consensus_level(r.measure_1);
    r.measure_2      = 100 * r.measure_1/nteams;
    r.optimalGamma   = max([pwA.optimalGamma]);
else
    r.isvalidCRM = 'no';
    r.measure_1      = nan;
    r.consensusLevel = get_consensus_level(r.measure_1);
    r.measure_2      = nan;
    r.optimalGamma   = nan;
    crm = false;
end

% save CRM if option selected
if isfield(options, 'saveCRMs') && strcmp(r.isvalidCRM, 'yes')
    
    if options.saveCRMs
        check_Dir(fullfile(options.savedir,'CRMs'));
        tmpName = strrep(options.cFT,'.','_');
        savName = fullfile(options.savedir,'CRMs',[tmpName '-ValidCRM.nii']);
        load('phantom_nifti_info.mat','phantom_nifti_info');
        nifti_Save(savName, crm, phantom_nifti_info);
        fprintf('\tCRM saved.\n')
    end


end

end

function cl = get_consensus_level(N)
    if N<3 
        cl = 'weak';
    elseif isnan(N)
        cl = 'none';
    elseif N<6
        cl = 'moderate';
    elseif N<10
        cl = 'strong';
    else
        cl = 'very strong';
    end
end


