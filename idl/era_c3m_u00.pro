; to derive u00

pro era_c3m_u00, month, u00, cld00x,cldfunc = cldfunc, filein = filein, umax=umax
;=========================================================

month=1

if(not keyword_set(filein))then filein = 'erac3m_'+strtrim(month,2)+'.sav'
if(not keyword_set(cldfunc))then cldfunc = 2
if(not keyword_set(umax))then umax = 1.0

;umax=1.0
;cldfunc =2
;filein = 'erac3m_'+strtrim(month,2)+'.sav'
 print,'file is read from saved ',filein
 restore, filein
 ;===============

; help,T2,Q2,P2,RH2,nn2,qs2,dnn_dz2,vor2,avor2,pv2,div2,duv_dz2,drh_dz2,d2rh_dz2,$
;   w2,cld2,cliq2,cice2,cloud4,cldliq4,cldice4,nx2,ny2,nz2,xx2,yy2,zz2,xx,yy,zz,xxx2

   cloud4x = ave3(cloud4,three=1) ; c3m clouds zonally averaged
 
   nx   = nx2
   ny   = ny2
   nz3  = nz2

   nu0 = 89 ;bins
   u0range = cal_lev([0.10,0.99],nu0)
   cldx = fltarr(ny,nz3,nu0) - 9999.
   u00x_5 = fltarr(ny,nz3)          ; 2d cloud
   cldx_5 = fltarr(ny,nz3)          ; 2d cloud
   jkm    = intarr(ny,nz3)

   for j=0,ny-1 do begin
      print,'j=',j
   for k=0,nz3-1 do begin

    for m = 0,nu0-1 do begin
        u0j  = u0range[m]
        cldj = fltarr(nx)
       for i = 0,nx -1 do begin
          rhj  = rh2[i,j,k]
          cldj[i] = cld_f(rhj, u0j, cldfunc,umax=umax) 
         ;----------------------------------------------
       endfor
       jj=where(cldj gt -1.0,cnt) 
       if(cnt gt 0)then cldx[j,k,m] = mean(cldj[jj])
     ;print,m,u0j,cldx[j,k,m]
    endfor

  cldg = reform(cldx[j,k,*])
  jj0= where(cldg gt -1., cnt)
 ;------------
 if((cnt gt 0) and (cloud4x[j,k] gt -1.0))then begin
  delt = abs( cldg[jj0] - cloud4x[j,k])
  jj = where(delt eq min(delt),cnt)
  jj = jj[cnt/2]

  u00x_5[j,k] = u0range[jj0[jj]]
  jkm[j,k]  = jj0[jj]
  cldx_5[j,k] = cldg[jj0[jj]]  ; calculated zonal average  
 endif
 ;-------------
endfor ;k
endfor ;j 

  u00    = u00x_5
  cld00x = cldx_5

return
end
