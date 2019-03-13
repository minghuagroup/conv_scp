
 function make_ct, bc,ec,ncolors
  scale = findgen(ncolors)/(ncolors-1)
  colors = bytArr(ncolors,3)
  for j=0,2 do colors[*,j] = bc[j]+(ec[j] - bc[j])*scale
  return, colors
 end

