function out = vinth2p(in,ps,hyam,hybm,newlevs,method,extrap)
%
%
% in [lev, lat, lon]
% ps [lat, lon], Pa
% hyam [lev]
% hybm [lev]
% newlevs [lev2]
% extra = 0
% intype = 1
if nargin == 5
	extrap = 0;
	method = 2; %2 log
elseif nargin ~= 7
	error(['nargin = ',num2str(nargin)]);
end

if length(hyam)~= length(hybm)
	return;
end

P0	= 100000; % Pa

nlev 	= length(hyam);

newlevs	= newlevs(:);
nlev2 = length(newlevs);

dims_ps = ndims(ps);
dims_in = ndims(in);
size_ps = size(ps);
size_in = size(in);

if dims_ps ~= (dims_in-1)
	error('Variable ''in'' should have one more dimension than ''ps''.');
end
if size_in(1) ~= nlev
	error(['Either ''in'' has an inconsistent level dimension, or the' ...
	       'order (ntim, nlev, nlat, nlon) is wrong.                 ']);
end

if size_in(2:end) ~= size_ps(1:end)
	error('Variable ''in'' and ''ps'' should have consistent dimensions.');
end

npts = prod(size_in)/nlev;

in = reshape(in,nlev,npts); %downscale dimension [nlev,nlatxnlon];
ps = reshape(ps,1   ,npts);

ps = ps./100; % hPa
P0 = P0/100;  % hPa


out = repmat(NaN,[nlev2,npts]);
plevs = P0*hyam*ones(1,npts)+hybm*ps(1,1:npts);
if method==1%linear interpolation
	Xold = plevs;
	Xnew = newlevs;
elseif method==2%log interpolation
	Xold = log(plevs);
	Xnew = log(newlevs);
else
	Xold = plevs;
	Xnew = newlevs;
end
if extrap==0
	for i = 1:npts
%		out(1:nlev2,i) = interp1(log(plevs(1:nlev,i)),in(1:nlev,i),log(newlevs(1:nlev2)));
		out(1:nlev2,i) = interp1(Xold(1:nlev,i),in(1:nlev,i),Xnew(1:nlev2),'linear',NaN);
	end
else
	for i = 1:npts
		out(1:nlev2,i) = interp1(Xold(1:nlev,i),in(1:nlev,i),Xnew(1:nlev2),'linear','extrap');
	end
end


out = reshape(out,[nlev2, size_ps]);









