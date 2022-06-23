function unzip_teamFiles(folder)
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------
% get folders
f = dir(fullfile(folder));

% loop through every folder in f and unzip the files
for i=3:numel({f.name})
    fls = dir(fullfile(f(i).folder,f(i).name,'/*.gz'));
    
    if numel({fls.folder})~=0
        gunzip(fullfile({fls.folder},{fls.name}),fls(1).folder)
    end
end
