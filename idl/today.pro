function today

 time = systime()
 jstr = str_sep(time, ' ')
 jstr = jstr[4]+'_'+ jstr[1]+'_' + jstr[2] ;+'_'+jstr[3]

 return, jstr
end


