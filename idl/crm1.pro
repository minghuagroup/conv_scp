goto,jump1


 ;iop = 'KWAJEX'
 ;dir = '../crm0/'+iop+'/'
 ;file=dir+'KWAJEX_256x256x64_1km.nc'
 ;ncdf_vars,file,vars

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

 jjx = where((xx le 48) and (xx ge 38))
 xx = x[jjx]

 nx = n_elements(xx) ;1155
 nz = n_elements(yy) ;64

 ;cloud
 mc     = get_fld(file,'MC') ;cloud 

 ;drafts
 mcup   = get_fldjj(file,'MCUP',jjx) ;draft
 mcdnu  = get_fldjj(file,'MCDNU',jjx)
 mcdns  = get_fldjj(file,'MCDNS',jjx)
 mfcld  = get_fldjj(file,'MFCLD',jjx) ;kg/m2/s averaged over the whole domain MC

; mcup + mcdnu + mcdns = mfcld

 ;core
 mcr    = get_fldjj(file,'MCR',jjx)  ;core
 mcrup  = get_fldjj(file,'MCRUP',jjx)
 mcrdnu = get_fldjj(file,'MCRDNU',jjx)
 mcrdns = get_fldjj(file,'MCRDNS',jjx)
 mfcor = get_fldjj(file,'MFCOR',jjx)  ;MCR whole domain 
 ;values of mfcor are the same as mcrup 
 ; mcr = mcrup + mcrdnu + mcrdns = mfcor

 ;size
 aup = get_fldjj(file,'AUP',jjx)  ;updrafts
 cor = get_fldjj(file,'COR',jjx)  ;core
 cdn = get_fldjj(file,'CDN',jjx)  ;dn core
 sup = get_fldjj(file,'SUP',jjx)  ;sat updraft
 sDN = get_fldjj(file,'SDN',jjx)  ;sat dndraft
 env = get_fldjj(file,'ENV',jjx)  ;unsaturated env
 corecl = get_fldjj(file,'CORECL',jjx) ;cloudy updraft core
 coredncl = get_fldjj(file,'COREDNCL',jjx) ;cloudy downdraft core
 cld = get_fldjj(file,'CLD',jjx)

 ;vertical velocity
 wmax  = get_fldjj(file,'WMAX',jjx) ;=0
 wcld  = get_fldjj(file,'WCLD',jjx) ; cld
 wclda = get_fldjj(file,'WCLDA',jjx) ;averaged over whole domain
 wsup  = get_fldjj(file,'WSUP',jjx) ;draft
 wsupa = get_fldjj(file,'WSUPA',jjx);whole domain
 wcor = get_fldjj(file,'WCOR',jjx)  ;in core
 wcdn  = get_fldjj(file,'WCDN',jjx) ;dncore
 wcora = get_fldjj(file,'WCORA',jjx)     ; averaged over whole domain

 pres  = get_fldjj(file,'PRES',jjx)
 wobs  = get_fldjj(file,'WOBS',jjx)
 rho   = get_fldjj(file,'RHO',jjx)
 mse   = get_fldjj(file,'MSE',jjx)
 msecld  = get_fldjj(file,'MSECLD',jjx)
 msecor = get_fldjj(file,'MSECOR',jjx)
 msesup  = get_fldjj(file,'MSESUP',jjx)
 dse     = get_fldjj(file,'DSE',jjx)
 dsecld  = get_fldjj(file,'DSECLD',jjx)
 dsecor = get_fldjj(file,'DSECOR',jjx)
 dsesup  = get_fldjj(file,'DSESUP',jjx)
 sse     = get_fldjj(file,'SSE',jjx)
 theta   = get_fldjj(file,'THETA',jjx)
 thetae  = get_fldjj(file,'THETAE',jjx)
 thetav  = get_fldjj(file,'THETAV',jjx)
 tabs    = get_fldjj(file,'TABS',jjx)
 tacor   = get_fldjj(file,'TACOR',jjx)
 tvcor   = get_fldjj(file,'TVCOR',jjx) ;Mean THETAV in core
 tvcora  = get_fldjj(file,'TVCORA',jjx) ;anomaly
 tvcld   = get_fldjj(file,'TVCLD',jjx)
 tvclda  = get_fldjj(file,'TVCLDA',jjx) ;anomaly
 tabsobs = get_fldjj(file,'TABSOBS',jjx)
 qt      = get_fldjj(file,'QT',jjx)
 qtcld   = get_fldjj(file,'QTCLD',jjx)
 qtcor  = get_fldjj(file,'QTCOR',jjx)
 qv      = get_fldjj(file,'QV',jjx)
 qvobs   = get_fldjj(file,'QVOBS',jjx)
 qsat    = get_fldjj(file,'QSAT',jjx)
 qcond   = get_fldjj(file,'QCOND',jjx)
 precip  = get_fldjj(file,'PRECIP',jjx)
 relh    = get_fldjj(file,'RELH',jjx)
 tvflux  = get_fldjj(file,'TVFLUX',jjx) ;bouyancy flux W/m2
 q1c     = get_fldjj(file,'Q1C',jjx)
 q2      = get_fldjj(file,'Q2',jjx)
 w2      = get_fldjj(file,'W2',jjx)
 tke     = get_fldjj(file,'TKE',jjx)
 w3      = get_fldjj(file,'W3',jjx)
 shear   = get_fldjj(file,'SHEAR',jjx)
 buoya   = get_fldjj(file,'BUOYA',jjx) ;m2/s3 Buoyancy production of TKE (resolved,jjx)
 buoyas  = get_fldjj(file,'BUOYAS',jjx) ; as above subscale
 PRESSTR = get_fldjj(file,'PRESSTR',jjx)
 ADVTRS  = get_fldjj(file,'ADVTRS',jjx) ;subgrid scale

 buoy_up = (dsecor-dse)*tabs/9.8/1004.

 prec = get_fldjj(file,'PREC',jjx)
 lhf  = get_fldjj(file,'LHF',jjx)
 shf  = get_fldjj(file,'SHF',jjx)
 lhfobs = get_fldjj(file,'LHFOBS',jjx)
 shfobs = get_fldjj(file,'SHFOBS',jjx)
 pw     = get_fldjj(file,'PW',jjx)
 pwobs  = get_fldjj(file,'PWOBS',jjx)
 cape   = get_fldjj(file,'CAPE',jjx)
 capeobs  = get_fldjj(file,'CAPEOBS',jjx)
 cin    = get_fldjj(file,'CIN',jjx)
 cinobs = get_fldjj(file,'CINOBS',jjx)
 zinv   = get_fldjj(file,'ZINV',jjx)
 zcb    = get_fldjj(file,'ZCB',jjx)
 
 zct    = get_fldjj(file,'ZCT',jjx)

 qtadv  = get_fldjj(file,'QTADV',jjx)
 qtdiff = get_fldjj(file,'QTDIFF',jjx)
 qtsink = get_fldjj(file,'QTSINK',jjx)
 qtsrc  = get_fldjj(file,'QTSRC',jjx)
 qtend  = get_fldjj(file,'QTEND',jjx)
 q2     = get_fldjj(file,'Q2',jjx)
 qvtend = get_fldjj(file,'QVTEND',jjx) ;VERTICICAL ADVECTION  
 qhtend = get_fldjj(file,'QHTEND',jjx) ;HORIZONTAL ADVECTION
 qnudge = get_fldjj(file,'QNUDGE',jjx)

 
 prec = get_fld(file,'PREC')
 sst = get_fld(file,'SST') 
 cape = get_fld(file,'CAPE') 
 
 wmax = get_fld(file,'WMAX') 

 QV = get_fld(file,'QV') 
 QVobs = get_fld(file,'QVOBS') 


 QC = get_fld(file,'QC') 
 QI = get_fld(file,'QI') 

 RELH = get_fld(file,'RELH')

 TVFLUX = get_fld(file,'TVFLUX') 
 QTFLUX = get_fld(file,'QTFLUX') 
 QTFLUXS = get_fld(file,'QTFLUX') 

 UW= get_fld(file,'UW') 

 ; = get_fld(file,'') 

 jump1:

 buoysup = (dsesup-dse)*tabs/g
 buoycor = (dsecor-dse)*tabs/g

 w = wcor
 dw2dz = w*0.0
 for k=1,nz-2 do begin
  for i=0,nx-1 do begin
   dw2dz[k,i] = 0.5*(w[k-1,i]*w[k-1,i] - w[k+1,i]*w[k+1,i])/(z[k-1]-z[k+1])*1000.
  endfor
 endfor 
    
 a1 = w*0
 a2 = w*0
 a3 = w*0
 jj=where(w gt 0.5)
 a1[jj] = buoycor[jj]/w[jj] /w[jj]
 a2[jj] = dw2dz[jj]  /w[jj] /w]

 
 help,xx,yy
 print,'assign aa and var to .r plot_tp.pro' 

end 

