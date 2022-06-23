function pwA = pairwise_diff_RMs_vs_CRM(dataS,cRM, tol)
% -------------------------------------------------------------------------
% run pairwise analysis with each response map in dataS and the CRM
% -------------------------------------------------------------------------
% @uthor pwhybra (whybrap@cardiff.ac.uk)
% -------------------------------------------------------------------------

if nargin<3; tol = 1; end % default passing tolerance of 1% 

% pairwise analysis struct
pwA = struct([]);

% loop through and do pairwise analysis for responseMaps
numMaps = numel(dataS);
for i=1:numMaps
    c = sprintf('%s_vs_%s', dataS(i).name,'CRM');
    pwA(i).maps_compared = c;  
    
    % absolute difference map
    pwA(i).differenceMap = abs(dataS(i).responseMap-cRM);
    
    % absolute difference map normalised by intensity range of both maps as (%)
    pwA(i).differenceMapNorm = 100.* differenceMapNorm(dataS(i).responseMap, cRM);
    
    % min/max difference
    pwA(i).absMinDiff = min(pwA(i).differenceMap(:));
    pwA(i).absMaxDiff = max(pwA(i).differenceMap(:));
    
    % optimal gamma that would reach 100% passing rate
    pwA(i).optimalGamma = max(pwA(i).differenceMapNorm(:));
    
    % euclidian distance (each 64 X 64 x 64 voxel is a dimension in space)
    pwA(i).euclidianDist = euclDist(dataS(i).responseMap(:),cRM);
    
    % add associated color
    pwA(i).TeamColor = dataS(i).TeamColor;
    
    % SSIM
    %tmpR = range([dataS(i).responseMap(:);cRM(:)]);
    %[SSIMVAL, SSIMMAP] = ssim(dataS(i).responseMap,single(cRM),'DynamicRange',tmpR);
    %min(SSIMMAP(:))
    
    % are all voxel-wise differences within set tolerance?
    passCheck = sum((pwA(i).differenceMapNorm(:)./tol) <= 1) /numel(pwA(i).differenceMapNorm(:));
    if passCheck==1
        pwA(i).withinTol = 1;
    else
        pwA(i).withinTol = 0;
    end
        
end
end

function diffNorm =  differenceMapNorm(x,y)
maxRMs   = max(cat(1,x(:), y(:)));
minRMs   = min(cat(1,x(:), y(:)));
rangeRMs = maxRMs- minRMs;
diffNorm = abs(x-y)./rangeRMs;
end

function dist = euclDist(rm1,rm2)
dist = sqrt(sum((rm1(:)-rm2(:)).^2));
end