pro winreset,xthick=xthick,ythick=ythick,xminor=xminor,yminor=yminor
; for typical X-display, reset win after using ctloc
if(!d.name eq 'X') then begin
!p.background=1
!p.color=0
device,decomposed=0
erase
if(n_elements(xthick) ne 0) then !x.thick=xthick
if(n_elements(ythick) ne 0) then !y.thick=ythick
 if(n_elements(xminor) ne 0) then !x.minor=xminor else !x.minor=2
 if(n_elements(yminor) ne 0) then !y.minor=yminor else !y.minor=2
endif else begin ;PS device
 if(n_elements(xthick) ne 0) then !x.thick=xthick else !x.thick=3
 if(n_elements(ythick) ne 0) then !y.thick=ythick else !y.thick=3
 if(n_elements(xminor) ne 0) then !x.minor=xminor else !x.minor=2
 if(n_elements(yminor) ne 0) then !y.minor=yminor else !y.minor=2
endelse
end
