function array_spaced, aa, nn 
 n = n_elements(aa)
 
 ind = indgen(n)*nn 
 jj = where(ind le n-1)
 bb  = aa[ind[jj]]

 return, bb

end


