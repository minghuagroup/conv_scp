function compact_2d, dd

; extract the middle core of the array

 nx = n_elements(dd[*,0])
 ny = n_elements(dd[0,*])

 nx2 = nx/2
 ny2 = ny/2

 dd3 = shift(dd, [-nx2/2,0])


 dd4 = shift(dd3,[0,-ny2/2])
 
 dd2 = dd4[0:nx2-1, 0:ny2-1] 
 dd2[0:nx2-1, ny2/2: ny2-1] = dd4[nx2:*, ny2/2: ny2-1]

 return,dd2

end



