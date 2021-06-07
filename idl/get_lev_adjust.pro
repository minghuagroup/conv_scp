
;===============================================================
function get_lev_adjust, dd0, LEV, nn=nn, lev_max=lev_max, lev_min = lev_min, missing = missing
;===============================================================
; adjust intervals based on dd


 if(not keyword_set(missing)) then missing = 1.0e15
 lev0 = lev

 dd = dd0
 dj = dd0*0.0
 if(not keyword_set(nn))then nn=20

; if(missing lt 0)then jj = where(dd0 gt missing, cnt) else jj = where(dd0 lt missing, cnt) 
jj = where(abs(dd) lt abs(missing), cnt)
 if(cnt gt 0) then dj[jj] = dj[jj] else return, lev

 if(not keyword_set(lev_max))then lev_max0 = max(dd[jj]) else lev_max0 = lev_max 
 if(not keyword_set(lev_min))then lev_min0 = min(dd[jj]) else lev_min0 = lev_min
   
; anchoring point and expected ref interval
; ------------------------------------------
 C0   = lev[0]
 del0 = lev[1] - lev[0]

 
; find interval based on data
; ------------------------------------------

 jj = where(dd ge lev_min0 and (dd le lev_max0),cnt)
 if(cnt eq 0)then return, lev0

 dmax = max(dd[jj])
 dmin = min(dd[jj])
; del = (dmax-dmin)/nn
 del = (lev_max0 - lev_min0)/nn

; set possible interval choices

 facs  = [1.e-7, 2.e-7, 5.e-7,1.e-6, 2.e-6,5.e-6,1.e-5,2.e-5,5.e-5,0.0001, 0.0002, 0.005, 0.01, 0.02, 0.05, $
        0.1, 0.2, 0.4, 0.5, $
         1., 1.2,1.4,2.,2.5,3., 4.,4.5, 5.,6,7,8,9, 10.,11,12,13,14, 15, 20.,40.,50.,100., 200., 400., 500., 1000., 2000., $
         4000., 5000., 10000.,2.e4, 3.e4, 5.e4, 1.e5, 2.e5, 5.e5, 1.e6, 2.e6, 5.e6, 1.e7, 2.e7, 5.e7]

 cc1    = facs * del0
 jj1    = where(cc1 ge del, cnt)


; if(cnt eq 0) then return, lev0

 del2 = min(cc1[jj1]) 

 lev2 = del2 * (findgen(100001)-50000) + C0

 jj2 = where(lev2 le dmax+del2 and (lev2 ge dmin-del2), cnt)

; if(cnt le 2 or cnt gt 50)then return, lev0

 lev3 = lev2[jj2]
    
;stop
 return, lev3

end
 

