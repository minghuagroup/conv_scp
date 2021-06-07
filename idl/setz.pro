;if port forwarding and X not working properly, try this approach

pro setz,xsize=xsize,ysize=ysize,title=title
set_plot,'z'
 if(not keyword_set(title))then title=''
;if(n_elements(xsize) eq 0) then xsize=400
;if(n_elements(ysize) eq 0) then ysize=400
if(n_elements(xsize) eq 0) then xsize=638
if(n_elements(ysize) eq 0) then ysize=510
device,set_resolution=[xsize,ysize]
end
