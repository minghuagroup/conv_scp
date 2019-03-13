clear all;
file1 = 'IAPi_0000-01-01_128x256_L30_c120110.nc';
file2 = 'IAPi_0000-01-01_128x256_L35_c180626.nc';
variable = 'U';

levelstring = '850mb';
newlevs = 850;
flag1 = 'IAPL30 initdata Zonal wind(850mb)'; 
flag2 = 'IAPL35 initdata Zonal wind(850mb)'; 

outfig1 = ['L30vsL35',variable,levelstring];

if exist(file1,'file')==0
	error(['Cannot find ',file1]);
end
if exist(file2,'file')==0
	error(['Cannot find ',file2]);
end

ncdisp(file1);
ncdisp(file2);

ps1 = ncread(file1,'PS');%lonxlat 256x128
hyam1 = ncread(file1,'hyam');
hybm1 = ncread(file1,'hybm');
lon1 = ncread(file1,'lon');
lat1 = ncread(file1,'lat');
var1 = ncread(file1,variable);
ndims_ps1 = ndims(ps1);
size_ps1 = size(ps1);
ndims_var1 = ndims(var1);%3
size_var1 = size(var1);%lon x lat x lev 256x128x30

ps2 = ncread(file2,'PS');
hyam2 = ncread(file2,'hyam');
hybm2 = ncread(file2,'hybm');
lon2 = ncread(file2,'lon');
lat2 = ncread(file2,'lat');
var2 = ncread(file2,variable);
ndims_ps2 = ndims(ps2);
size_ps2 = size(ps2);
ndims_var2 = ndims(var2);
size_var2 = size(var2);

var1_levlonlat = permute(var1,[3,1,2]);%[lev,lon,lat]
var2_levlonlat = permute(var2,[3,1,2]);%[lev,lon,lat]

%newlevs = 500;
var1_p = vinth2p(var1_levlonlat,ps1,hyam1,hybm1,newlevs,2,0);%log interpolation no extrapolation
var1_p = squeeze(var1_p);%256x128
var1_p = permute(var1_p,[2,1]);%lat x lon

var2_p = vinth2p(var2_levlonlat,ps2,hyam2,hybm2,newlevs,2,0);%log interpolation no extrapolation
var2_p = squeeze(var2_p);%256x128
var2_p = permute(var2_p,[2,1]);%lat x lon

%out1 = 
%vinth2p(in,ps,hyam,hybm,newlevs,method,extrap)

fh1 = figure('visible','off','Units','normalized',...
	'Position',[0.2,0.2,0.6,0.6]);
latlim = [-90,90];
lonlim = [0,360];
load coastlines;

sh1 = subplot(2,1,1);
set(sh1,'Position',[0.1,0.55,0.95,0.4]);%[left bottom width height] 
axesm('eqdcylin','MapLatLimit',latlim,'MapLonLimit',lonlim,...
    'Origin',[0,180,0],'FontSize',15,'FontWeight','bold',...
    'Frame','on','Grid','on','MeridianLabel','on','ParallelLabel','on')
axis off;
setm(gca,'MLabelLocation',30,'MLabelParallel','South');
setm(gca,'PLabelLocation',30);
%set(gca,'Position',[0.1,0.55,0.95,0.4]);
set(gca,'DataAspectRatio',[2,3,1]);
%set(gca,'PlotBoxAspectRatio',[3,0.75,0.75]);
%set(gca,'Position',[0.1,0.55,0.95,0.4]);
[C1,h1] = contourfm(lat1,lon1,var1_p,'LineColor','none');
cb1 = colorbar;
%set(cb1,'Location','eastoutside')%,'Position',[0.15,0.1,0.1,0.3]);
%cbp = cb1.Position;
%set(cb1,'Position',[cbp(1:3),0.8*cbp(4)]);
colormap(jet);
hold on;
plotm(coastlat,coastlon,'Color','black');
title(flag1,'FontSize',15,'FontWeight','bold');
hold off;

sh2 = subplot(2,1,2);
set(sh2,'Position',[0.1,0.1,0.95,0.4]);%[left bottom width height] 
axesm('eqdcylin','MapLatLimit',latlim,'MapLonLimit',lonlim,...
    'Origin',[0,180,0],'FontSize',15,'FontWeight','bold',...
    'Frame','on','Grid','on','MeridianLabel','on','ParallelLabel','on')
axis off;
setm(gca,'MLabelLocation',30,'MLabelParallel','South');
setm(gca,'PLabelLocation',30);
set(gca,'DataAspectRatio',[2,3,1]);
[C2,h2] = contourfm(lat2,lon2,var2_p,'LineColor','none');
cb2 = colorbar;
%set(cb1,'Location','eastoutside')%,'Position',[0.15,0.1,0.1,0.3]);
%cbp = cb1.Position;
%set(cb1,'Position',[cbp(1:3),0.8*cbp(4)]);
colormap(jet);
hold on;
plotm(coastlat,coastlon,'Color','black');
title(flag2,'FontSize',15,'FontWeight','bold');
hold off;

%[C1,h1] = contourfm(lon1,lat1,var1_p);
%[C1,h1] = contourf(lon1,lat1,var1_p);
%outfig1 = [file1(1:end-3),variable,levelstring];
saveas(fh1,outfig1,'fig');
disp([outfig1,'.fig']);
save2pdf([outfig1,'.pdf'],fh1);
