
igif=0
iix =0

cy=''
print,'enter case info? y n '
read,cy
if(cy eq 'n')then goto,jump22


;====================
fdir    = 'data_scm/' 

;icase2   = 'scp_toga'
icase2   = 'zm_toga2'
icase2   = 'xinscp_toga_dp0'
icase2   = 'xinscp_toga_sh0'
icase2   = 'scp_1dp_toga'
icase2   = 'scp_1sh_toga'
icase2   = 'scp_1shw001_toga'
icase2   = 'scp_1shw010_toga'
icase2   = 'scp_1shw100_toga'
icase2   = 'scp_1shtau4200'
icase2   = 'scp_1shalp4'
icase2   = 'scp_1shdnfr09'
icase2   = 'scp_1dpdnfr09'
icase2   = 'zm_nosh'
icase2   = 'zm_toga2'
icase2   = '1dpuw'
icase2   = 'scpdpdnfrc09'
icase2   = 'xinscp'
icase2   = 'scp2'  ; default with d(Mq')/dz corrected
;icase2   = 'scp3'  ; with qliqtend_det,massflx_dn, q_dn, ent_turb, corrected, uniform mb scm_diag,xc=0.5
;icase2    = 'scp7'   ; same as scp3, ent_org mid point to 0.5H,xc=UW
;icase2    = 'scp8'   ; same as scp3, ent_org B<0, ,xc=UW + alp1.0
icase2   = 'scp4'  ; scp8 + baseflux from closure
icase2   = 'scp5'  ; scp8 + baseflux from modified closure w_up_init
icase2    = 'scp6' ; as scp5,  alp=10. to lower top
icase2    = 'scp7' ; as scp8,  alp=1. to lower top, alp=1., w'/w in turb
icase2    = 'scp9' ; as scp8,  alp=1. , w'/w in turb,new clousure !crashed
icase2    = 'scp10'   ; xc=UW  alp=1.0,w'=1.,dn=wet, evap,kdntop
icase2    = 'scp11'   ; as scp10, massbase from closure
;icase2    = 'scp13'   ; as scp10, massbase from (2/wi)**2, ent_org capelmt = 0.0, evp trans
icase2    = 'scp14'   ; as scp10, massbase from (2/wi)**2 sh only, ent_org capelmt = 0.0

icase2    = 'scp11'   ; as scp12, org ent turb 2
icase2    = 'scp14'   ; as scp12, org ent
icase2    = 'scp13'   ; as scp12, org ent profile wrong
icase2    = 'scp30'   ; new baseline for normalized flux 2018-05-29

print,'icase2=?  e.g., scp30'
read,icase2

filein1 =fdir+ 'camrun_scp2.cam.h0.1992-12-19-00000.nc'
y1 = get_fld(filein1,'lev')
y2 = get_fld(filein1,'ilev')

cix = 'ix_'  ; individual plumes at it time
;cix = ''     ; sum results
print,'cix=? scm or diag'
read,cix
if(cix eq 'scm')then cix = ''
if(cix eq 'diag')then cix = 'ix_'

jump22:
it=1

case cix of

'ix_': begin ; scm diagnostic plots

filein2 =fdir+ icase2+'_scmdiag-output.nc'
;====================
dtime = get_fld(filein2,'dtime')
dtime = reform(dtime)
nt     = n_elements(dtime[0,*])
nplume = n_elements(dtime[*,0])
xp = findgen(nt)/72. ; day
yp = findgen(nplume)+1 ; number of plumes ,1,2, 
 x =  findgen(15)+1
 y = y1

 norm = 1.0
 norm = get_fld(filein2,'massflxbase')
 norm = total(norm[*,it])/15.
 if(norm le 1.0e-6)then norm=1.

   end
   
'': begin ; scm history tape
;filein2 =fdir+ 'camrun_'+icase2+'.cam.h0.1992-12-19-00000.nc'
; kwajex
filein2 =fdir+ icase2+'_camrun.cam.h0.1999-07-25-00000.nc'
;coare
filein2 =fdir+ icase2+'_camrun.cam.h0.1992-12-19-00000.nc'
cix = ''
;====================
 x = get_fld(filein2,'time')
 y = get_fld(filein2,'lev')
 xrange=[min(x),max(x)]
 xrange=[0,8]
;====================
  end
else:
endcase

icase = 'data_test/'+cix+icase2


g=file_search(icase)
if(g eq '')then spawn,'mkdir '+ icase

gif_folder = icase

;filein2 = fdir+filein2


; ncdf_vars,filein2,vars2
 xrange=[min(x),max(x)]

 yrange=[1000.,100]


ix=1

print,'display on web?'
read,ix

 strid = icase2 
 gifgroups = file_search(gif_folder+'/2_*.gif')

 
 htmlfile ='html_scm/'+cix+strid+'_2d.html'
 dir='../'
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile


 htmlfile ='html_scm/'+cix+strid+'_1d.html'
 dir='../'
 gifgroups = file_search(gif_folder+'/1_*.gif')
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile

end
