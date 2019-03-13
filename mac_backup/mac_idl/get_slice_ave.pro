
function get_slice_ave,file,var,index,lev3=lev3,missing=missing

  if(not keyword_Set(missing))then missing = 999990.0

  d1 = get_slice(file,var,0)*0.0
     if(keyword_set(lev3))then d1=reform(d1[*,*,lev3])
  count = d1

  nt = n_elements(index)
  for i=0,nt-1 do begin

    dj = get_slice(file,var,index[i])

     if(keyword_set(lev3))then dj=reform(dj[*,*,lev3])

    jj= where(abs(dj) lt missing,cnt)

    if(cnt gt 0)then begin
      d1[jj] = d1[jj] + dj[jj]
      count[jj] = count[jj] +1. 
    endif
   endfor

   dd = d1*0 - 999999.
   jj=where(count gt 0.,cnt)
   if(cnt gt 0)then dd[jj] = d1[jj]/count[jj] 

   return,dd

 end
