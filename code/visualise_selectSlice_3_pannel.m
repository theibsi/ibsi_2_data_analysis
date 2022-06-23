% Simple plots of difference images
% -------------------------------------------------------------------------
% @uthor: PWhybra
% -------------------------------------------------------------------------
%
savdir = fullfile('../results/tmp');
mkdir(savdir);

c_dataS = dataCell{9,2};

slice = 32;
figure;
im =  c_dataS(5).responseMap(:,:,32);
w = [min(im(:)), max(im(:))];
i1 =imagesc(im,w); axis image; colormap gray; axis off;
pw_ImageSettings; tightfig;
cb = colorbar;
cb.FontSize = 15;

print('-dpng', fullfile(savdir,'tmp1.png'),'-r300')

% visualise valid crm
options.exhaustivePlot = 0; % takes a while but plots all results 
options.tol  = 1; % max of 1% voxel-wise variation for pairwise analysis
[~,crm,~] = run_consensus_analysis(c_dataS,options);
figure;
im2 =  crm(:,:,slice);
w  = [min(im2(:)), max(im2(:))];
i2 = imagesc(im2,w); 
pw_ImageSettings; tightfig;
cb = colorbar;
cb.FontSize = 15;
print('-dpng', fullfile(savdir,'tmp2.png'),'-r300')

% visualise difference image
pwA  = pairwise_diff_RMs_vs_CRM(c_dataS(5),crm,options.tol);
im3  = pwA(1).differenceMapNorm(:,:,slice);
figure;
w  = [min(im3(:)), max(im3(:))];
i3= imagesc(im3,w); axis image; colormap hot; axis off;
pw_ImageSettings; tightfig;
cb = colorbar;
cb.FontSize = 15;
print('-dpng', fullfile(savdir,'tmp3.png'),'-r300')

% visualise gamma map
gammaV = 1;
RM = pwA(1).differenceMapNorm;
gM     = zeros(size(RM));
gM(RM<=gammaV)= 1;




figure;
imagesc(gM(:,:,slice));
c_cmap = [ 255 0 0 ; 0,255,0]./255; %255,165,0
ax = gca;
colormap(ax,c_cmap);
axis image; axis off; tightfig; 

cb = colorbar;
cb.Ticks = [0 0.25 0.75 1];
cb.TickLabels = {'','Fail', 'Pass',''};
cb.FontSize = 15;
print('-dpng', fullfile(savdir,'tmp4.png'),'-r300')










