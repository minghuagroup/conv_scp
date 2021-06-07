
igif=1
iix =0

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
icase2   = 'scp2'

filein2 =fdir+ 'camrun_'+icase2+'.cam.h0.1992-12-19-00000.nc'
;====================

icase = 'data_test/'+icase2

g=file_search(icase)
if(g eq '')then spawn,'mkdir '+ icase
gif_folder = icase

;filein2 = fdir+filein2


 ncdf_vars,filein2,vars2
 x = get_fld(filein2,'time')
 y = get_fld(filein2,'lev')
 xrange=[min(x),max(x)]
 xrange=[0,8]
 

 yrange=[1000.,0]


ix=1

print,'display on web?'
read,ix

 strid = icase2 
 gifgroups = file_search(gif_folder+'/2_*.gif')

 htmlfile ='html_scm/'+strid+'_2d.html'
 dir='../'
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile


 htmlfile ='html_scm/'+strid+'_1d.html'
 dir='../'
 gifgroups = file_search(gif_folder+'/1_*.gif')
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile

end
