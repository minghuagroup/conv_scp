
function now

 time = systime()
 jstr = str_sep(time, ' ')
 jstr = jstr[4]+'_'+ jstr[1]+'_' + jstr[2] +'_'+jstr[3]
 jstr = str_replace(jstr, ' ','_')
 jjstr = str_replace(jstr, ':','_')

 return,jjstr

end
