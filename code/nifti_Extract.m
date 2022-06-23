function [vol,v_info] = nifti_Extract(filename)
% -------------------------------------------------------------------------
%   Input:  nifti filename 
%
%   Output: vol    -> image volume 
%           v_info -> nifti header info    
%              
% -------------------------------------------------------------------------
% Also see niftiSave
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------

% load nifti with single/double precision
%vol     = single(niftiread(filename));
vol     = double(niftiread(filename));
%vol     = squeeze(vol);
v_info  = niftiinfo(filename);

% change datatype info 
v_info.Datatype = 'double';

% permute volume (for matlab to match other viewer, when saving permute back) 
if size(vol,3)==1
    vol = permute(vol, [2,1]);
elseif size(vol,3)~=1
    vol = permute(vol, [2,1,3]);
end
