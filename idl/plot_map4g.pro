; Create a simple image to be warped:

pro plot_map4g,aa,latmin,latmax,lonmin,lonmax,lat0,lon0


; Display the image so we can see what it looks like before
; warping:
image= aa

TV, image

MAP_SET, lat0, lon0, /ORTHOGRAPHIC, /ISOTROPIC, $
   LIMIT=[latmin, lonmin, latmax, lonmax]
result = MAP_IMAGE(image,Startx,Starty, COMPRESS=1, $
   LATMIN=latmin, LONMIN=lonmin, $
   LATMAX=latmax, LONMAX=lonmax)

; Display the warped image on the map at the proper position:
TV, result, Startx, Starty

; Draw gridlines over the map and image:
MAP_GRID, latdel=10, londel=10, /LABEL, /HORIZON

; Draw continent outlines:
MAP_CONTINENTS, /coasts
return
end


