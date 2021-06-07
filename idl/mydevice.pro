pro mydevice, setZ

 set_plot,'Z'
 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 !P.background = colors.white
 !P.color      = colors.black
 ;Device, Set_Resolution=[500,350];, Z_Buffer=0
 Device, Set_Resolution=[xsize0,ysize0];, Z_Buffer=0
 Erase
endif else begin
 set_plot,'X'
 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 !P.background = colors.white
 !P.color      = colors.black
 if(not keyword_set(window))then  window,xsize=xsize0,ysize=ysize0,/free,title=title
endelse

return

end


