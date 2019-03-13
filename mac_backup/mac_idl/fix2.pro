
 function fix2,a

   if(a ge 0)then c= fix(a)
   if(a lt 0)then c= -fix(-a+0.9999999)

  return,c
 end

;----------------------
pro  calc_level,a,levels,a0=a0,maxv=maxv,minv=minv

   if(not keyword_Set(a0))then a0=0

      a1=min(a)
      a2=max(a)
   if(keyword_Set(maxv))then a2=maxv   
   if(keyword_Set(minv))then a1=minv   
     
 dx = (a2-a1)/15

 dx2 = fix2( alog10(dx) )
 dig=10.^dx2
 dx3 = dx/dig

 if(dx3 lt 1.5) then dx=1
 if((dx3 ge 1.5) and (dx3 lt 3)) then dx=2
 if((dx3 ge 3) and (dx3 lt 5)) then dx=4
 if((dx3 ge 5) and (dx3 lt 7)) then dx=6
 if((dx3 gt 7) and (dx3 le 9)) then dx=8
 if(dx3 gt 9 ) then dx=10
  dx = dx*dig

  a0=0
  b2 = fix2((a2-a0)/dx+0.999)
  b1 = fix2((a1-a0)/dx)

  nlev = b2-b1+1

  levels = indgen(nlev)*dx + b1*dx+a0

 end
;----------------------------
