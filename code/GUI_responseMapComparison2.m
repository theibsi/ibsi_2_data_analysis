function f1 = GUI_responseMapComparison2(dataS,options)
% Response map results GUI visualiser 
%--------------------------------------------------------------------------
% dataS = data cell array
%
%--------------------------------------------------------------------------
% @uthor Pwhybra
%--------------------------------------------------------------------------

% is there a valid CRM to be found? 
options.exhaustivePlot  = 0;
if nargin<2
   options.tol   = 1; 
end
[~,valid_crm] = run_consensus_analysis(dataS,options);

% Add CRM
numMaps = numel({dataS.name});
rMps    = zeros(numMaps,numel(dataS(1).responseMap));
for i=1:numMaps
    rMps(i,:) = dataS(i).responseMap(:);
end
init_cRM = mean(rMps,1);
dataS(numMaps+1).name = 'Initial CRM';
dataS(numMaps+1).responseMap = reshape(init_cRM,size(dataS(1).responseMap));

% Add valid CRM
if ~islogical(valid_crm)
    dataS(numMaps+2).name = 'Valid CRM';
    dataS(numMaps+2).responseMap = valid_crm;
end

% dataS
rm1 = dataS(1).responseMap;
rm2 = dataS(1).responseMap;

% For Gamma map
gammaV = 1;
gMap =  100.*differenceMapNorm(rm1,rm2);
% pad images & make sure they are same dimensions
%rm1 = padarray(rm1, [8 8 0],nan);
%rm2 = padarray(rm2, [8 8 0],nan);
%gMap = padarray(gMap, [8 8 0],nan);
rm3 = abs(rm1-rm2);
if ~all(size(rm1)==size(rm2))
    error('images not the same dimension (visualisation)');
end


%Teams
teamList = {dataS.name};

% initialise variables and panel sizes
%--------------------------------------------------------------------------
f1 = figure('Resize','on',...
    'MenuBar', 'none',...
    'Color',[0 0 0]./255,...
    'Renderer','opengl',...
    'NumberTitle','off','visible','on','Position',[-1284 456 596 718]); %[139 12 596 718]

buttonH = 0.04;
buttonW = 0.1;
deltaX = 0.01;
fontN = 'Calibri';
panelW = (1-3*deltaX)/2;
panelH = (1-2*deltaX);
sc_colormap = 'gray';
imagePosition = [0.1 0.1 0.8 0.80];

% RM1 Pannel
%--------------------------------------------------------------------------
rm1_panel = uipanel('Units','Pixels',...
    'FontName',fontN,...
    'Units','normalized',...
    'BackgroundColor',[0,0,0],...
    'BorderType','etchedin',...
    'ForegroundColor','w',...
    'Title','RM 1',...
    'FontSize',12,...
    'Position',[deltaX,deltaX+0.5,panelW,panelH/2-2*deltaX]);

rm1_axes = axes('Parent',rm1_panel,...
    'Units','normalized',...
    'Position',[deltaX,0.15+deltaX,(1-2*deltaX),0.95],...
    'Color',[0,0,0]);
rm1_axes.Toolbar.Visible = 'off';

rm1_clim_low_edit = uicontrol('Parent',rm1_panel,...
    'Style','edit',...
    'Callback',@update_clim_Callback,...
    'String',num2str(min(rm1(:))),...
    'BackgroundColor',[1,1,1],...
    'FontSize',8,...
    'Units','normalized',...
    'Tag','clim_low_edit',...
    'Position',[deltaX,deltaX,buttonW*1.6, buttonH*2.5]);

rm1_clim_high_edit = uicontrol('Parent',rm1_panel,...
    'Style','edit',...
    'Callback',@update_clim_Callback,...
    'String',num2str(max(rm1(:))),...
    'BackgroundColor',[1,1,1],...
    'FontSize',8,...
    'Units','normalized',...
    'Tag','clim_high_edit',...
    'Position',[1-deltaX-buttonW*1.6,deltaX,buttonW*1.6, buttonH*2.5]);



% RM2 pannel
%--------------------------------------------------------------------------
rm2_panel = copyobj(rm1_panel,f1);set(rm2_panel,'Title','RM2','Position',...
    get(rm1_panel,'Position')+[0+panelW+deltaX,0,0,0]); %+[0,-0.5+deltaX,0,0]

rm2_axes =findobj(rm2_panel,'type','axes');

rm2_axes.Toolbar.Visible = 'off';

rm2_clim_low_edit  = findobj(rm2_panel,'Tag','clim_low_edit');
rm2_clim_high_edit = findobj(rm2_panel,'Tag','clim_high_edit');
set(rm2_clim_low_edit,'Callback',@update_clim_Callback,...
    'String',num2str(min(rm2(:))));
rm2_clim_high_edit = findobj(rm2_panel,'Tag','clim_high_edit');
set(rm2_clim_high_edit,'Callback',@update_clim_Callback,...
    'String',num2str(max(rm2(:))));


% Difference pannel
%--------------------------------------------------------------------------
rm3_panel = copyobj(rm1_panel,f1);set(rm3_panel,'Title','Difference Image','Position',...
    get(rm1_panel,'Position')+[0,-0.5+deltaX,0,0]);%+[0+panelW+deltaX,0,0,0]


rm3_axes =findobj(rm3_panel,'type','axes');
rm3_axes.Toolbar.Visible = 'off';
rm3_clim_low_edit  = findobj(rm3_panel,'Tag','clim_low_edit');
rm3_clim_high_edit = findobj(rm3_panel,'Tag','clim_high_edit');
set(rm2_clim_low_edit,'Callback',@update_clim_Callback,...
    'String',num2str(min(rm3(:))));
rm3_clim_high_edit = findobj(rm3_panel,'Tag','clim_high_edit');
set(rm3_clim_high_edit,'Callback',@update_clim_Callback,...
    'String',num2str(max(rm3(:))));

% Gamma map pannel
%--------------------------------------------------------------------------
rm4_panel = uipanel('Units','Pixels',...
    'FontName',fontN,...
    'Units','normalized',...
    'BackgroundColor',[0,0,0],...
    'BorderType','etchedin',...
    'ForegroundColor','w',...
    'Title','Gamma Mask',...
    'FontSize',12,...
    'Position',get(rm3_panel,'Position')+[0+panelW+deltaX,0,0,0]);


rm4_axes = axes('Parent',rm4_panel,...
    'Units','normalized',...
    'Position',[deltaX,0.15+deltaX,(1-2*deltaX),0.95],...
    'Color',[0,0,0]);

rm4_axes.Toolbar.Visible = 'off';

rm4_gamma_edit = uicontrol('Parent',rm4_panel,...
    'Style','edit',...
    'Callback',@update_gammaMap,...
    'String',num2str(1),...
    'BackgroundColor',[1,1,1],...
    'FontSize',8,...
    'Units','normalized',...
    'Tag','gmap_edit',...
    'Position',[deltaX+buttonW*4 ,deltaX,buttonW*1.6, buttonH*2.5]);




% Drop down menus
%--------------------------------------------------------------------------
rm1_selection = uicontrol('Parent',rm1_panel,...
    'Style','popupmenu',...
    'BackgroundColor',[1,1,1],...
    'Callback',@selectRM1_Callback,...
    'FontSize',12,...
    'String',teamList,...
    'Units','normalized',...
    'Position',get(rm1_clim_low_edit,'Position')+[2*buttonW,0,0.4,0.85]);


rm2_selection = uicontrol('Parent',rm2_panel,...
    'Style','popupmenu',...
    'BackgroundColor',[1,1,1],...
    'Callback',@selectRM2_Callback,...
    'FontSize',12,...
    'String',teamList,...
    'Units','normalized',...
    'Position',get(rm2_clim_low_edit,'Position')+[2*buttonW,0,0.4,0.85]);



% Initilise
%--------------------------------------------------------------------------
slice=floor(size(rm1,3)/2);
mn=1;
mx=size(rm1,3);

% Initilise mouse coordinates
C = zeros(4);

% initialise images
% -------------------------------------------------------------------------

rm1_clim_low = min(rm1(:));
m_clim_high = max(rm1(:));
w = [rm1_clim_low , m_clim_high];

rm2_clim_low = min(rm2(:));
rm2_clim_high = max(rm2(:));
w2 = [rm2_clim_low , rm2_clim_high];

i1=imshow(rm1(:,:,slice),w, 'parent',rm1_axes);
set(rm1_axes,'position',imagePosition)
i2=imshow(rm2(:,:,slice),w2, 'parent',rm2_axes);
%colormap(rm2_axes,sc_colormap)
set(rm2_axes,'position',imagePosition)
set(f1, 'WindowScrollWheelFcn', @wheel)
set (f1, 'WindowButtonMotionFcn', @mouseMove);

% Difference image
rm3_clim_low = min(rm3(:));
rm3_clim_high = max(rm3(:));
w3 = [rm3_clim_low , rm3_clim_high];
i3=imshow(rm3(:,:,slice),w3, 'parent',rm3_axes);
set(rm3_axes,'position',imagePosition)
colormap(rm3_axes, 'hot');

% Gamma Map
rm4 = createGammaMask(gMap,gammaV);
i4=imshow(rm4(:,:,slice),[0 1], 'parent',rm4_axes);
set(rm4_axes,'position',imagePosition)

c_cmap = [ 255 0 0 ; 0,255,0]./255; %255,165,0
colormap(rm4_axes,c_cmap)

% mouse position
% -------------------------------------------------------------------------
    function mouseMove (object, eventdata)
        C = get (rm1_axes, 'CurrentPoint');
        
    end

% capture mouse
% -------------------------------------------------------------------------
    function wheel(src, evnt)
        if C(1,1)>0 && C(1,1)<60 && C(1,2)>0 && C(1,2)<200
            if evnt.VerticalScrollCount > 0
                slice=slice+1;
            else
                slice=slice-1;
            end
            slice=re_eval1(slice);
        end
    end

% redraw
% -------------------------------------------------------------------------
    function slice=re_eval1(slice)
        if slice>mx
            slice=mx;
        elseif slice<mn
            slice=mn;
        else
            set(i1,'CData',rm1(:,:,slice))
            set(i2,'CData',rm2(:,:,slice))
            set(i3,'CData',rm3(:,:,slice))
            set(i4,'CData',rm4(:,:,slice))
        end
        rm1_panel.Title = ['RM1 slice ', num2str(slice)];
        rm2_panel.Title = ['RM2 slice ', num2str(slice)];
        rm3_panel.Title = ['Absolute Difference slice ', num2str(slice)];
        rm4_panel.Title = ['Gamma Mask slice ', num2str(slice)];
    end

% update clims
% -------------------------------------------------------------------------
    function update_clim_Callback(source,eventdata)
        
        if get(source,'Parent') == rm1_panel
            newlow = str2double(get(rm1_clim_low_edit,'String'));
            if isnan(newlow)
                set(rm1_clim_low_edit,'String',num2str(rm1_clim_low));
            else
                rm1_clim_low = newlow;
            end
            
            newhigh = str2double(get(rm1_clim_high_edit,'String'));
            if isnan(newhigh)
                set(rm1_clim_high_edit,'String',num2str(m_clim_high));
            else
                m_clim_high = newhigh;
            end
            w = [rm1_clim_low , m_clim_high];
            i1=imshow(rm1(:,:,slice),w, 'parent',rm1_axes);
            
        elseif get(source,'Parent') == rm2_panel
            newlow = str2double(get(rm2_clim_low_edit,'String'));
            if isnan(newlow)
                set(rm2_clim_low_edit,'String',num2str(rm2_clim_low));
            else
                rm2_clim_low = newlow;
            end
            
            newhigh = str2double(get(rm2_clim_high_edit,'String'));
            if isnan(newhigh)
                set(rm2_clim_high_edit,'String',num2str(rm2_clim_high));
            else
                rm2_clim_high = newhigh;
            end
            w2 = [rm2_clim_low , rm2_clim_high];
            i2=imshow(rm2(:,:,slice),w2, 'parent',rm2_axes);
            colormap(rm2_axes, sc_colormap);
            
        elseif get(source,'Parent') == rm3_panel
            newlow = str2double(get(rm3_clim_low_edit,'String'));
            if isnan(newlow)
                set(rm3_clim_low_edit,'String',num2str(rm3_clim_low));
            else
                rm3_clim_low = newlow;
            end
            
            newhigh = str2double(get(rm3_clim_high_edit,'String'));
            if isnan(newhigh)
                set(rm3_clim_high_edit,'String',num2str(rm3_clim_high));
            else
                rm3_clim_high = newhigh;
            end
            w3 = [rm3_clim_low , rm3_clim_high];
            i3=imshow(rm3(:,:,slice),w3, 'parent',rm3_axes);
            colormap(rm3_axes, 'hot');
        end
    end


% update selected response map
% -------------------------------------------------------------------------

    function selectRM1_Callback(source, eventdata)
        contents = get(source,'String');
        newRM_name = contents{get(source,'Value')};
        Index = contains({dataS.name},newRM_name);
        rm1 = dataS(Index).responseMap;
        rm3 = abs(rm1-rm2);
        update_clims()
        update_gammaMap(source, eventdata)
    end


    function selectRM2_Callback(source, eventdata)
        contents = get(source,'String');
        newRM_name = contents{get(source,'Value')};
        Index = contains({dataS.name},newRM_name);
        rm2 = dataS(Index).responseMap;
        rm3 = abs(rm1-rm2);
        update_clims()
        update_gammaMap(source, eventdata)
    end

    function update_clims(source, eventdata)
        rm1_clim_low = min(rm1(:));
        rm1_clim_high = max(rm1(:));
        w = [rm1_clim_low , rm1_clim_high];
        set(rm1_clim_high_edit,'String',num2str(max(rm1(:))));
        set(rm1_clim_low_edit,'String',num2str(min(rm1(:))));
        
        
        rm2_clim_low = min(rm2(:));
        rm2_clim_high = max(rm2(:));
        w2 = [rm2_clim_low , rm2_clim_high];
        set(rm2_clim_high_edit,'String',num2str(max(rm2(:))));
        set(rm2_clim_low_edit,'String',num2str(min(rm2(:))));
        
        rm3_clim_low = min(rm3(:));
        rm3_clim_high = max(rm3(:));
        w3 = [rm3_clim_low , rm3_clim_high];
        set(rm3_clim_high_edit,'String',num2str(max(rm3(:))));
        set(rm3_clim_low_edit,'String',num2str(min(rm3(:))));
        if w(1)<w(2)
            set(i1.Parent,'CLim',w);
        end
        if w2(1)<w2(2)     
            set(i2.Parent,'CLim',w2);
        end

    end

    function update_gammaMap(source, eventdata)
        newGamma = str2double(get(rm4_gamma_edit,'String'));
        gMap =  100.*differenceMapNorm(rm1,rm2);
        rm4 = createGammaMask(gMap,newGamma);
        
        
    end

end














