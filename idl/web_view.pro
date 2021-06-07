;all models

pro web_view,group,httpfile,title=title,ncolumns=ncolumns, nfolders = nfolders
;------------------------------------------------------------

  ;group is the name of the gif files

 if(not keyword_set(ncolumns))then ncolumns=1

 close,11
 openw,11,httpfile,width=180
 print,httpfile

      title= title
 
c=''
 printf,11,'<html>'
 printf,11,'<center>'
 printf,11,'<BODY BGCOLOR="#CCECFF" link=blue vlink=purple background=img/image001.jpg ">'
 printf,11,c
 strj = '<center> <h1> <a NAME="aa0"></a><b>'+$
         strtrim(title,2) +' </b></h1></center>'
 printf,11,c
 printf,11,strj
 printf,11,c
 printf,11,'<p> <hr ALIGN="CENTER"> '

;--------------------------
 printf,11, '<table BORDER COLS=7 WIDTH="80%" NOSAVE >'

  nmodel = n_elements(group)

  KK=0

  FOR ID =0, n_elements(group)-1 DO BEGIN

   GID=GROUP[ID]

   if ((KK mod ncolumns) eq 0)then printf,11,'<tr>'
  ; print,KK

;     printf,11,'<td><center>'+GID+'</center></td>'

       printf,11,c     
       imgfile = gid
     
; title trim
     imgspec = ' border=0 width=360 height=280 '
     if(keyword_set(nfolders))then begin
      jgid = str_Sep(gid,'/')
      jgid = reverse(jgid)
      nj = nfolders < n_elements(jgid)
      gid = '..'
      for i  = nj, 0, -1 do begin 
       gid = gid + '/'+jgid[i]
      endfor
     endif

     strj='<td><center><b>'+GID + '</b>'
     printf,11,strj
     printf,11,'<p></p>'
     strj =  '<img '+imgspec+'src="'+ imgfile + '"><p></p>'
     printf,11 ,strj
     printf,11,c
     strj = '<p></p></a></center></td>'
     printf,11,c

;print,imgfile
;stop

     KK=KK+1
;   if ( ((KK mod 2) eq 1) or (ID eq n_elements(group)-1))then printf,11,'</tr>'

;----------------
   ENDFOR

printf,11,'</table></center>'

  printf,11, '<p><a href="#aa0">go to top </a>'

printf,11,'</body>'
printf,11,'</center>'
printf,11,'</html>'


close,11

return

end


