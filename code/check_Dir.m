function pathDir = check_Dir(pathDir)
% if pathDir directory does not exist, make it;
pathDir = fullfile(pathDir);
if ~isfolder(pathDir)
    mkdir(pathDir) 
end

end