
function ave3n,d,first=first,missing=missing,three=three,dim2=dim2

  dd=d
  if(not keyword_Set(missing))then missing=-9999.

  if(keyword_Set(three))then begin
   sz = size(dd)
   case three of
1: begin
    d2=reform(dd[0,*,*])*0.0
     for k=0,sz[3]-1 do begin
        dj = reform(dd[*,*,k])
        dj=ave2(dj,first=1,missing=missing)
        d2[*,k] = dj
     endfor  
    end
2: begin
   d2=reform(dd[*,0,*])*0.0
     for k=0,sz[3]-1 do begin
        dj = reform(dd[*,*,k])
        dj=ave2(dj,missing=missing)
        d2[*,k] = dj
     endfor  
    end
 
3: begin
    d2=reform(dd[*,*,0])*0.0
     for k=0,sz[1]-1 do begin
        dj = reform(dd[k,*,*])
        dj=ave2(dj,missing=missing)
        d2[k,*] = dj
     endfor  
    end

4: begin
    d2=reform(dd[*,*,*,0])*0.0
     for i=0,sz[1]-1 do begin
     for j=0,sz[2]-1 do begin
     for k=0,sz[3]-1 do begin
        dj = reform(dd[i,j,k,*])
         jj=where(dj gt missing,cnt)
        if (cnt gt 0)then dj=mean(dj[kk])
        d2[i,j,k,*] = dj
     endfor  
     endfor  
     endfor  
    end
else:
endcase
 return,d2
endif 

if(keyword_set(dim2))then begin
 return,d2
endif

 if(keyword_set(first))then dd=transpose(d)

 nx=n_elements(dd[*,0])
 dm = fltarr(nx)

 if(not keyword_Set(missing))then begin

 for i=0,nx-1 do begin
  dm[i] = mean(dd[i,*])
 endfor

 endif else begin

  for i=0,nx-1 do begin
   ddj=reform(dd[i,*])
   jj=where(ddj gt missing,cnt)
   dj = -999999.
   if(cnt gt 0)then dj=mean(ddj[jj]) 
   dm[i] = dj
;   print,dj,cnt
  
  endfor
 endelse
 return,dm

end

