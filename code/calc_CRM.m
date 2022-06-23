function CRM = calc_CRM(dataS)
% returns a consensus response map from the provided data struct
% -------------------------------------------------------------------------
% @uthor pwhybra 
% -------------------------------------------------------------------------

numMaps = numel({dataS.name});
rMps    = zeros(numMaps,numel(dataS(1).responseMap));
for i=1:numMaps
    rMps(i,:) = dataS(i).responseMap(:);
end
CRM = mean(rMps,1);
CRM = reshape(CRM,size(dataS(1).responseMap));