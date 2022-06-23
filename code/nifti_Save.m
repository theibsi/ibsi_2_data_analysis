function nifti_Save(savename, vol, vol_info)
% -------------------------------------------------------------------------
%   Input:  savename
%           vol      -> image volume
%           vol_info -> nifti header info 
%
%   Output: creates nifti file with savename
%              
% -------------------------------------------------------------------------
% Also see nifti_Extract
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

% permute volume (required for saving)
if size(vol,3)==1
    vol = permute(vol, [2,1]);
elseif size(vol,3)~=1
    vol = permute(vol, [2,1,3]);
end

vol_info.Datatype = 'single';
% save
vol_info.Filename = savename;
niftiwrite(single(vol), savename ,vol_info);
