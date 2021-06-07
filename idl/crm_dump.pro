;goto,jump1

iop = 'coare'
iop = 'coare10'
igif = 1
ix=0
giffolder = iop+'_gifs/'
if(igif)then begin
 spawn, 'rm -rf '+giffolder
 spawn, 'mkdir '+giffolder
endif

file = '../crm/KWAJEX/KWAJEX_256x256x64_1km.nc'
file = '../crm/TOGA/TOGA_LONG_256x256x64_1km_10s.nc'

 ncdf_vars,file,vars

 x = get_fld(file,'time')
 x=x-x[0]
 p = get_fld(file,'p')
 z = get_fld(file,'z')
 nx = n_elements(time)
 nz = n_elements(z) 

 xx = x
 yy = p

 xrange = minmax(xx)
 yrange = reverse(minmax(yy))
 xtitle = 'time'
 ytitle = 'pressure'
 if(iop eq 'coare10')then xrange = [38.,48]


;===================================
 for i=3,n_elements(vars)-1 do begin
  var = vars[i]
  title = iop +' '+ var

  d1 = get_fld(file,var)
  sz = size(d1)
  get_lev,d1,var,lev1,scale
  aa = scale*d1
  print,i,'  ',var, minmax(aa)

 wwdelete
 window,/free,xsize=560.,ysize=500,title=title

  if(sz[0] eq 1)then begin
   plot,xx,aa,yrange=minmax(aa) $
      ,xrange=xrange,title=title, xtitle=xtitle,ytitle=ytitle
   if(igif)then mygif,giffolder+'1d_'+var+'.gif'
  endif

  if(sz[0] eq 2)then begin
  
   if((max(aa) ne min(aa)) and (max(lev1) ne min(lev1)) )then begin 
     title2= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)
      plot_3dhtml,aa,xx,yy,tran=1,lev1, $
       yrange=yrange,xrange=xrange,title=title2, xtitle=xtitle,ytitle=ytitle
   if(igif)then mygif,giffolder+'2d_'+var+'.gif'
  endif
 endif

  
  if(ix)then read,ix

 
 endfor ; ========= variables done

if(igif)then begin
 htmlfile = 'html/crm_'+iop+'.html'
 gifgroups = file_search(giffolder+'*.gif')
 web_view,'../'+gifgroups,htmlfile,title=iop,ncolumns=3 ;, out of html
 print,'webfile:',htmlfile
endif

stop
 
 prec = get_fld(file,'PREC')
 precc = get_fld(file,'PRECC')
 sst = get_fld(file,'SST') 
 cape = get_fld(file,'CAPE') 
 
 wmax = get_fld(file,'WMAX') 

 QV = get_fld(file,'QV') 
 QVobs = get_fld(file,'QVOBS') 

 ;plot_3dhtml,d,x,-p,tran=1,cal_lev(d,15)

 QC = get_fld(file,'QC') 
 QI = get_fld(file,'QI') 

 RELH = get_fld(file,'RELH')

 TVFLUX = get_fld(file,'TVFLUX') 
 QTFLUX = get_fld(file,'QTFLUX') 
 QTFLUXS = get_fld(file,'QTFLUX') 

 UW= get_fld(file,'UW') 

 ; = get_fld(file,'') 

end 

