function file_exist,file
 result = file_search( file)
 if(result[0] eq '')then return,0
; spawn, ' ls '+ file, file0 
; if(strlen(file0[0]) eq 0)then return,0
 return,1
end
