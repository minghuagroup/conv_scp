spawn,'finger mzhang > junk'
openr,2,'junk'
 c=''
 while not eof(2) do begin
   readf,2,c
    i= strpos(c,'from')
   if(i gt 0)then begin
      str = strmid(c,i+4,40)
      str=strtrim(str,2)+':0.0'
      cmd = 'setenv DISPLAY ' +str
      print,'$'+cmd
      goto,jump
    endif
 endwhile

jump:
     spawn,'rm junk'
     close,2
stop
end
