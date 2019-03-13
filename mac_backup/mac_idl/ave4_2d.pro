
function ave4_2d,d ;,three=three

   dd = d
   sz = size(dd)

  three=1
   case three of
1: begin
    d2=reform(dd[0,0,*,*])*0.0
     for k=0,sz[3]-1 do begin
     for n=0,sz[4]-1 do begin
        dj = reform(dd[*,*,k,n])
        nn = n_elements(dj)
        d2[k,n] = total(dj)/nn
     endfor  
     endfor  
    end
else:
endcase
return,d2
end
