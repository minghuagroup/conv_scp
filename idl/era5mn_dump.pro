

years = indgen(20)+1995
yyyys = strtrim(years,2)  ; 1995-2014

restore, 'era5mn.sav0'

savenc = 1

dirworkE = '/glade/scratch/mzhang/E/'
dirin = dirworkE + yyyys + '/'
dirclimo = dirworkE + 'climo/'
make_folder, dirclimo

;goto,jump1

FOR itype = 0,1 do begin
;=====================================================
 if(itype eq 0) then begin
  ml = 0
  prefix = ''
  varsj = [varss, varsv, varsfi, varsm, varsac]
 endif else begin
  ml = 1
  prefix = 'ML_'
  varsj = varsm
 endelse

 nv    = n_elements(varsj)
 for iv = 0,nv-1 do begin
 ;=======================
  var0 = varsj[iv]
    print, ' iv ', iv, '  ', var0, '  ', era5mn_vars(var0)
  kk = 0
 for iy = 0, 19 do begin
 ;=======================
    print, 'year ', iy, '  ', yyyys[iy]
     era5savmn, yyyys[iy], myvars = varsj[iv], no_int=1, do_save = 0, ml=ml, aa=aa, file=file

   if(n_elements(aa) gt 10) then begin
     if(iy eq 0)then taa = aa else taa = taa + aa
      kk = kk+1
   endif
 endfor ;kk for iy

  if(kk gt 0)then begin
  taa = taa/kk

 if(savenc)then begin
  fileout = dirclimo+prefix+var0+'.nc'
  save_nc, file, fileout,var0, taa
 endif else begin
  fileout = dirclimo+prefix+var0+'.sav'
  save,file = fileout,taa,file   
 endelse
  print,' file saved: ', fileout
  
 endif ;kk > 0

endfor ; iv
;====================

endfor ; itype

end
