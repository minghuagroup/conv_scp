  pro windbarb,dir,spd,pres

  ; plot wind barbs on skewt plot
  ; spd = wind speed in m/s
  ; dir = wind dir in deg, 0= N 90= E, etc
  ; pres = pressure level of wind ob

; Color table: 0=black, 1=dk blue 2=med blue 3=cyan 4=green 5=yellow
; 6=orange 7=red 8=grey 9=white
; Don't know why all this works, but it is necessary...if you want to write
; gifs.

   common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;   r=bytscl([0,  0,  0,  0,  0,255,255,255,127,255])
;   g=bytscl([0,  0,119,255,255,255,119,  0,127,255])
;   b=bytscl([0,255,255,255,  0,  0,  0,  0,127,255])
;   r_orig=r
;   g_orig=g
;   b_orig=b
;   tvlct,r_orig,g_orig,b_orig
;   r_curr= r_orig
;   g_curr= g_orig
;   b_curr= b_orig

   clr=2
  
   case 1 of
    (spd lt 1.25): begin
       nowind,pres,x1,y1,clr 
       goto,done
    end
    (spd le 3.75): begin
       position=1 
       barb,pres,dir,x1,y1,clr
    end
    (spd gt 3.75): begin
       position=0 
       barb,pres,dir,x1,y1,clr
    end
   endcase

   npen=floor(spd/25.)

   for i=1,npen do begin
     pennant,x1,y1,dir,position,clr
     position=position+2
   endfor
   spd1=spd-npen*25.

   nlon=floor(spd1/5.0)
   spd1=spd1-nlon*5.
; special case for rounding to a pennant
   if nlon eq 4 AND spd1 gt 3.75 then begin
     pennant,x1,y1,dir,position,clr
     goto,done
   endif

   for i=1,nlon do begin
     longbarb,x1,y1,dir,position,clr
     position=position+1
   endfor

; case statement for doing all the rounding. If rounding to a pennant, see above
   case 1 of
     (spd1 le 1.25): goto,done  
     (spd1 gt 1.25 and spd1 le 3.75): shortbarb,x1,y1,dir,position,clr
     (spd1 gt 3.75): longbarb,x1,y1,dir,position,clr
     else:print,'Something not right with short barbs'
   endcase

  done:

  end  ; end of main program

 pro barb,pressure,direct,x1,y1,clr

   R=287.0
   K=-33.0
   cp=1005.7
   kappa=R/cp

   a=findgen(17)*(!pi*2./16.)
   usersym,cos(a),sin(a),/fill

   aline=79.00  ; starting point for all wind barbs
   newb=-R*alog(pressure)
   newc2=fltarr(3)
   newc=convert_coord(aline,newb,/to_normal)
   x1=newc(0)
   y1=newc(1)

   length=0.065
   angle= (!pi/180.00)*(450-direct) ; wind dir in standard coords
   dy=length*sin(angle)
   dx=length*cos(angle)
   x2=x1+dx
   y2=y1+dy

   plots,x1,y1,psym=8,symsize=1,color=clr,/normal
   plots,x1,y1,/normal
   plots,x2,y2,/continue,thick=2,/normal,color=clr

end

pro longbarb,x1,y1,direct,position,clr

   length=0.065 - position*0.006

   angle= (!pi/180.00)*(450-direct) ; wind dir in standard coords
   dy=length*sin(angle)
   dx=length*cos(angle)
   x2=x1+dx
   y2=y1+dy

  longbarb=0.015

  barbang=390-direct
  if barbang ge 360 then barbang=barbang-360.

  dx=longbarb*cos(barbang*!pi/180.00)
  dy=longbarb*sin(barbang*!pi/180.00)

  x3=x2+dx
  y3=y2+dy

  plots,x2,y2,/normal
  plots,x3,y3,/continue,/normal,thick=2,color=clr

end

pro shortbarb,x1,y1,direct,position,clr

   length=0.065 - position*0.006

   angle= (!pi/180.00)*(450-direct) ; wind dir in standard coords
   dy=length*sin(angle)
   dx=length*cos(angle)
   x2=x1+dx
   y2=y1+dy

  shortbarb=0.0075

  barbang=390-direct
  if barbang ge 360 then barbang=barbang-360.

  dx=shortbarb*cos(barbang*!pi/180.00)
  dy=shortbarb*sin(barbang*!pi/180.00)

  x3=x2+dx
  y3=y2+dy

  plots,x2,y2,/normal
  plots,x3,y3,/continue,/normal,thick=2,color=clr
end

pro pennant,x1,y1,direct,position,clr

; pennant takes two positions, start at second position

   length=0.065 - (position+1)*0.006

   angle= (!pi/180.00)*(450-direct) ; wind dir in standard coords
   dy=length*sin(angle)
   dx=length*cos(angle)
   x2=x1+dx
   y2=y1+dy

  longbarb=0.015

  barbang=390-direct
  if barbang ge 360 then barbang=barbang-360.
 
; tip of pennant

  dx1=longbarb*cos(barbang*!pi/180.00)
  dy1=longbarb*sin(barbang*!pi/180.00)

  x3=x2+dx1
  y3=y2+dy1

; 2nd half of pennant - draw from start

   length=0.065 - (position)*0.006

   dy=length*sin(angle)
   dx=length*cos(angle)

   x4=x1+dx
   y4=y1+dy

;draw the pennant

  plots,x2,y2,/normal
  plots,x3,y3,/continue,/normal,thick=2,color=clr

  plots,x4,y4,/normal
  plots,x3,y3,/continue,/normal,thick=2,color=clr

  polyfill,[x2,x3,x4,x2],[y2,y3,y4,y2],/normal,color=clr

end

 pro nowind,pressure,x1,y1,clr

; if wind speed less than 2.5 kts (1.25 m/s) then draw a dot surrounded by a 
; larger circle

   R=287.0
   K=-33.0
   cp=1005.7
   kappa=R/cp

   a=findgen(17)*(!pi*2./16.)
   usersym,cos(a),sin(a),/fill

   aline=79.00  ; starting point for all wind barbs
   newb=-R*alog(pressure)
   newc2=fltarr(3)
   newc=convert_coord(aline,newb,/to_normal)
   x1=newc(0)
   y1=newc(1)

   plots,x1,y1,psym=8,symsize=1,color=clr,/normal

   a=findgen(17)*(!pi*2./16.)
   usersym,cos(a),sin(a)

   plots,x1,y1,psym=8,symsize=1.75,color=clr,/normal
end


