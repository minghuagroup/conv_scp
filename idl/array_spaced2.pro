function array_spaced2, aa, nn1,nn2 

 n1 = n_elements(aa[*,0])
 n2 = n_elements(aa[0,*])

 m1 = n_elements(array_spaced(reform(aa[*,0]),nn1))
 m2 = n_elements(array_spaced(reform(aa[0,*]),nn2))

 bb = aa
 bb2 = fltarr(m1,n2)
 bb3 = fltarr(m1,m2)

 for j = 0, n2-1 do begin
  dw = reform(aa[*,j])
  bw = array_spaced(dw,nn1)
  bb2[*,j] =  bw
 endfor

 for i = 0, m1-1 do begin
  dw = reform(bb2[i,*])
  bw = array_spaced(dw,nn2)
  bb3[i,*] =  bw
 endfor

 return, bb3

end


