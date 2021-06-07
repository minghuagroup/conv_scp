; ---------------------------------
 function cld_f, rh, u0, cldfunc, umax=umax
; ---------------------------------
; input, rh, u0, cldfunc(cld scheme, 1,2), umax
; output cld amt
; ---------------------------------

 if(not keyword_Set(umax))then umax = 1.0

 cld = -9999.
 if(rh le 0.0)then return,cld
   
  if((u0 ge umax) or (rh ge umax))then begin
   cld = 1.
   return,cld
  endif

 u =  min ([rh,umax])
 u =  max( [u, u0])
 umax =  max( [umax, u0+1.0e-4])
     ;u = max([ min ([rh,umax)], u0])  !! does not work

 case cldfunc of
 1: begin
     cld = (U - u0)/max([umax - u0, 0.0001])
    end
 2: begin
     cld = 1. - sqrt((umax - U)/max([umax - u0, 0.0001]))
    end
 3: begin
     x = (u - u0)/(umax-u0)

     if(not keyword_set(a0))then begin
      a0 = umax-u0
      b0 = 1.-a0
     endif


     cld = a0*x+b0*x*x
     
    end

 else : begin
  print,'no cld scheme'
  stop
  end

 endcase
  cld = max([cld, 0.0])

  return ,cld
 end

;---------------------------
function u0_patch, u00x2
;---------------------------
; boundary patch for lat-press cross section
 sz = size(u00x2)
 ny = sz[1]
 nz3 = sz[2]
u00x2[ny-1,*] = u00x2[ny-3,*]
u00x2[ny-2,*] = u00x2[ny-3,*]
u00x2[*,nz3-1] = u00x2[*,nz3-3]
u00x2[*,nz3-2] = u00x2[*,nz3-3]
u00x2[*,0] = u00x2[*,1]
for k= nz3-5,nz3-1 do begin
 for j = ny-5,ny-1 do begin
  u00x2[j,k] = u00x2[j,nz3-4]
 endfor
endfor
return,u00x2
end

;-----------------------
pro write_u00, u0, fileout = fileout
; write to data/ in fortran code
; input u0(ny,nz)
; run era_c3m, j1, j2
;save,file='u00.sav',rh2july,rh2jan,cloud4jan,cloud4jul,$
; u0july,u0jan,xx,yy,zz,nx,ny,nz3,u0annx,u00x2,u00x3,u00x2s 
;-----------------------
  ;restore,'u00.sav'

sz = size(u0)
ny = sz[1]
nz3 = sz[2]

n14 = ny/6-1

thre = 0.01
jj = where(u0 le thre,cnt)
if(cnt gt 0)then u0[jj] = thre

if(not keyword_set(fileout))then fileout = 'u00.dat'

close,1
openw,1,fileout
 for k = 0,nz3-1 do begin 
  k1 = k+1
  printf,1,'data u00tab(',k1 ,',:) /  & ',format = '(6x,A12,I2,A10)
  for i=0,n14 do begin
   i1 = i*6
   i2 = i1+1
   i3 = i1+2
   i4 = i1+3
   i5 = i1+4
   i6 = i1+5 
 if(i lt n14)then begin
   printf,1, u0(i1,k),',',u0(i2,k),',',u0(i3,k),',',u0(i4,k),',',u0(i5,k),',',u0(i6,k),',',$
   '&', format='(5x,6(F10.5,A1,3X), A1)'
 endif else begin
   printf,1, u0(i1,k),',',u0(i2,k),',',u0(i3,k),',',u0(i4,k),',',u0(i5,k),',',u0(i6,k), $
     '/', format='(5x,6(F10.5,A1,3X), A1)'
 endelse

  endfor

 endfor ;k

; printf,1,'nz=',nz3
; printf,1,zz*0.0
; printf,1,'ny=',ny
; printf,1,yy

 close,1
 return
end

; ---------------------------------
function expand, uu, nx
; ---------------------------------
; expand uu[ny,nz] to uu[nx,ny,nz] for plot_3d view
; ---------------------------------
 ny = n_elements(uu[*,0])
 nz = n_elements(uu[0,*])
 a = fltarr(nx,ny,nz)
 for i=0,nx-1 do begin
  a[i,*,*] = uu
 endfor
 return,a
end
 
; ---------------------------------
 function cld_u0, rh, yf
; ---------------------------------
  xj2 = rh
 u0j = yf[0] + yf[1]*xj2+ yf[2]*xj2*xj2 + yf[3]*xj2^3
 return, u0j
 end 

; ---------------------------------
 function cld_u3, rh, u00, umax=umax, cldfunc=cldfunc, thres=thre 
; ---------------------------------

 if(not keyword_set(thres)) then thre = 0.1
 if(not keyword_set(cldfunc)) then cldfunc = 2  ; cld scheme
 if(not keyword_set(umax)) then umax=1.0  ; cld scheme

 sz = size(rh)
 nx = sz(1)
 ny = sz(2)
 nz = sz(3)

 u0 = u00
 rh2 = rh
 cloud5 = rh*0.0 - 9999.

  for j=0,ny-1 do begin
  for k=0,nz-1 do begin
     u0j = u0[j,k]
  for i=0,nx-1 do begin
     rhj = rh2[i,j,k]
     if(rhj gt 0.0)then begin
        cloud5[i,j,k] = cld_f(rhj, u0j, cldfunc, umax=umax)
     endif
  endfor
  endfor
  endfor

 return,cloud5

end
;------------------
 function remap_z,ua1,zz,ptop,pbot
; renormalization streching/compression
 ub1 = ua1
 pmax = ptop
 
; print,'sss'
 sz = size(ua1)
 ny2 = sz(1)
 nz2 = sz(2)

 for j=0,ny2-1 do begin
  dp = (pbot[j] - pmax[j])/nz2

  uaj1 = reform(ua1[j,0:nz2-1])
  pj1  = zz[0:nz2-1]

  pj2 = ptop[j] +  indgen(nz2)*dp

  uaj2 = interpol(uaj1,pj1,pj2,/spline)

  ub1[j,*] = uaj2

 endfor
 return,ub1

 end

;------------------
 function remap_y,ua1,yy,y0
; renormalization streching/compression centered at y0
 ub1 = ua1

 sz = size(ua1)
 ny2 = sz(1)
 nz2 = sz(2)

 ny = ny2/2

 y2 = yy
 for k=0,nz2-1 do begin

  dy1 = (yy[0]-y0)/ny
  dy2 = (y0-yy[ny2-1])/ny
  
  
  uaj1 = reform(ua1[*,k])
  yj1  = yy

  y2[0:ny-1]  = yy[0] - indgen(ny)*dy1
  y2[ny:ny2-1]  = y2[ny-1] - (indgen(ny)+1)*dy2

  uaj2 = interpol(uaj1,yy,y2,/spline)

  ub1[*,k] = uaj2

;stop
 endfor
 return,ub1

 end



