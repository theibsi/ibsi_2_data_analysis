function diffNorm =  differenceMapNorm(x,y)
maxRMs   = max(cat(1,x(:), y(:)));
minRMs   = min(cat(1,x(:), y(:)));
rangeRMs = maxRMs- minRMs;
diffNorm = abs(x-y)./rangeRMs;
end