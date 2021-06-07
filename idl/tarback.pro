
pro tarback, type

 time = systime()
 jstr = str_sep(time, ' ')
 jstr = jstr[4]+'_'+ jstr[1]+'_' + jstr[2]+'_'+jstr[3]
; jstr = str_replace(time, ' ','_')

 spawn, 'pwd', home

 if(strpos(type,'/') lt 0) then begin
  dir2 = home
  type2 = type
 endif else begin
  dir2 = get_lastv_inlist(type,'/',rev=1) 
  type2 = get_lastv_inlist(type,'/',rev=0) 
 endelse
  
 cd, dir2
 spawn, 'pwd', dir0
 dir0 = get_homedir()
 fname = dir0 +'_' + jstr + '.tar '

 cmd   = 'tar -cvf ' + fname + type
 spawn, cmd

 cmd = ' mv '+ fname + ' ~/unix/backup/.'
 print,cmd
 spawn, cmd
 print,'file ', fname, ' moved to ~/unix/backup/.'

 cd, home 
 print,home

 return
 end


