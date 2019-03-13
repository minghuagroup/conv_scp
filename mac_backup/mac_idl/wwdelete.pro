pro wwdelete,n=n

if(not keyword_set(n))then begin
 while(!D.window ge 0) do wdelete
endif else begin
 for i=1,n do wdelete
endelse
end

