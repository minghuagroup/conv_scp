
function file_exists,file

 n = 1
 f = file_search(file,count=cnt)
 if(cnt eq 0)then n=0
 return,n

end  
 

