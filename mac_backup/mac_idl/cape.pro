 pro cape,pres,ttt,www,height

; units for input file
; pres = mb
; temp =  K
; mixrat= kg/kg
; height = m

 common cape,a1,b1,cape,cin,li

   eps=0.622
   R=287.
   K1=-33
   cp=1004.7
   kappa=R/cp
   p0=1000.

; SORT THE SOUNDING VARIABLES SO THE HEIGHT INCREASES MONOTONICALLY
unique=uniq(height, sort(height))
height=height(unique)
pres=pres(unique)
ttt=ttt(unique)
www=www(unique)


; Calculate parcel characteristics. Several ways to do this.
; Average lowest 50 mb in this case

    L50= pres(0)-50.5
    L50p= where(pres ge L50)
    parcelt=total(ttt(L50p))/n_elements(L50p)
    parcelw=total(www(L50p))/n_elements(L50p)
    parcelp=total(pres(L50p))/n_elements(L50p)
    mixrat=parcelw*1000.


; Find LCL characteristics

    eee=(parcelw*parcelp)/(eps+parcelw)
    tlcl= (2840./(3.5*alog(parcelt)-alog(eee)-4.805))+55.
    plcl=parcelp*(tlcl/parcelt)^(1./kappa)
    lclth=tlcl*(p0/plcl)^kappa
    lclte=lclth*exp((2675.0*parcelw)/tlcl)
    lclte2=lclth*exp((3.376/tlcl-.00254)*(mixrat*(1.+.81e-3*mixrat)))
;   avepte(mmm)=lclte2


; Calculate parcel path...

    pt=fltarr(n_elements(height))
    ptv=fltarr(n_elements(height))
    pw=fltarr(n_elements(height))
    dt=fltarr(n_elements(height))
    dtv=fltarr(n_elements(height))

    dry=where(pres ge plcl AND pres le parcelp)
    moist=where(pres le plcl)
;   high=where(pres le 350. AND pres gt 175.)
;   rhigh=where(pres le 175.)

; Deal with the easy dry case

    if dry(0) ne -1 then begin
      pt(dry)=lclth*(pres(dry)/p0)^kappa
      pw(dry)=parcelw
      ptv(dry)=pt(dry)*(1.+0.6*pw(dry))

      dt(dry)=pt(dry)-ttt(dry)
;     dtv(dry)=ptv(dry)-vtemp(dry)
    endif

; Moist ascent case -- much more difficult

    for jj=0,n_elements(moist)-1 do begin
      tguess=pt(moist(jj)-1)-0.5
      GOOD=0
      count=0
      while GOOD eq 0 do begin
       count=count+1
       pp=pres(moist(jj))
       theta=tguess*(p0/pp)^kappa
       tcel=tguess-273.15
       esguess=6.112*exp((17.67*tcel)/(tcel+243.5))
       wsguess=eps*(esguess/(pp-esguess))

       mixrat=wsguess*1000.
       eee=(wsguess*pp)/(eps+wsguess)
       tlclg= (2840./(3.5*alog(tguess)-alog(eee)-4.805))+55.
       plclg=pp*(tlclg/tguess)^(1./kappa)
       te2g=theta*exp((3.376/tlclg-.00254)*(mixrat*(1.+.81e-3*mixrat)))

       tetest=lclte2-te2g
       case 1 of
         (tetest lt -0.1): tguess=tguess-0.01
         (tetest gt 0.1):tguess=tguess+0.01
         else:begin
            GOOD=1
            pt(moist(jj))=tguess
            pw(moist(jj))=wsguess
            ptv(moist(jj))=tguess*(1.+0.6*wsguess)
            dt(moist(jj))=pt(moist(jj))-ttt(moist(jj))
;           dtv(moist(jj))=ptv(moist(jj))-vtemp(moist(jj))
         end
       endcase
      endwhile
;    print,pres(moist(jj)),count
    endfor

    use=where(pres le parcelp)
    alwc=parcelw-pw
    buoy=dt/ttt
;   tvbuoy=dtv/vtemp
    buoy=buoy(use)
;   tvbuoy=tvbuoy(use)
    www2=www(use)
    ttt2=ttt(use)
    height2=height(use)
    pres2=pres(use)
    alwc=alwc(use)
    alwc=alwc > 0.
    rbuoy=buoy-alwc

    pt=pt(use)
    pw=pw(use)

     a1=pt+(K1*alog(pres2))
     b1=-R*alog(pres2)

;    oplot,a1,b1,thick=3.0,color=6


;   for iii=1,n_elements(use)-2 do dz(use(iii))=(height(iii+1)-height(iii-1))/2.
;   for iii=0,n_elements(use)-1 do print,pres2(iii),pt(iii),pw(iii)
;   dz=dz(use)

;  Determine LFC, EL, etc.

    levs=n_elements(use)-1
    pindex=use(0)
    lfcindex=pindex
    lfcindexr=pindex
    elindex=levs
    elindexr=levs
;   elindex=0
;   elindexr=0
    lfcp=pres2(pindex)
    lfcz=height2(pindex)
    lfct=ttt2(pindex)
    elp=pres2(levs)
    elz=height2(levs)
    elt=ttt2(levs)
    lfcpr=pres2(pindex)
    lfczr=height2(pindex)
    lfctr=ttt2(pindex)
    elpr=pres2(levs)
    elzr=height2(levs)
    eltr=ttt2(levs)
    lipres=abs(pres2-500.)
    mp=min(lipres,elem)
    i500=elem
    i500=i500(0)

    if i500 ne -1 then li=-dt(i500) else li=0.0
    neg=1

;   for kk=0,levs do print,pres(kk),buoy(kk)

    for kk=0,levs do begin
      if buoy(kk) gt 0.0 AND neg eq 1 then begin
        neg=0
        lfcp=pres2(kk)
        lfcz=height2(kk)
        lfct=ttt2(kk)
        lfcindex=kk
      endif

      if buoy(kk) lt 0.0 AND neg eq 0 then begin
        neg=1
        elp=pres2(kk)
        elz=height2(kk)
        elt=ttt2(kk)
        elindex=kk
      endif
    endfor

    neg=1
    for kk=0,levs do begin
      if rbuoy(kk) gt 0.0 AND neg eq 1 then begin
        neg=0
        lfcpr=pres2(kk)
        lfczr=height2(kk)
        lfctr=ttt2(kk)
        lfcindexr=kk
      endif

      if rbuoy(kk) lt 0.0 AND neg eq 0 then begin
        neg=1
        elpr=pres2(kk)
        elzr=height2(kk)
        eltr=ttt2(kk)
        elindexr=kk
      endif
    endfor


    if lfcindex-pindex gt 0 then begin
      zneg=height2(pindex:lfcindex)
      negarea=buoy(pindex:lfcindex)
      negarea=negarea < 0
      cin=int_tabulated(zneg,negarea)
    endif else cin=0.0

    if lfcindexr-pindex gt 0 then begin
      znegr=height2(pindex:lfcindexr)
      rnegarea=rbuoy(pindex:lfcindexr)
      rnegarea=rnegarea < 0
      rcin=int_tabulated(znegr,rnegarea)
    endif else rcin=0.0

    if elindex-lfcindex gt 0 then begin
      zpos=height2(lfcindex:elindex)
      posarea=buoy(lfcindex:elindex)
      posarea=posarea > 0
      cape=int_tabulated(zpos,posarea)
    endif else cape=0.0

    if lfcindex lt i500 then begin
      z500=height2(lfcindex:i500)
      pos500=buoy(lfcindex:i500)
      pos500=pos500 > 0.
      cape500=int_tabulated(z500,pos500)
    endif else cape500=0.0

    if elindexr-lfcindexr gt 0 then begin
      zposr=height2(lfcindexr:elindexr)
      rposarea=rbuoy(lfcindexr:elindexr)
      rposarea=rposarea > 0.
      rcape=int_tabulated(zposr,rposarea)
    endif else rcape=0.0
;   rcape=0

    cin=9.8*cin
    cape=9.8*cape
    rcin=9.8*rcin
    rcape=9.8*rcape
    cape500=9.8*cape500

; Print useful statistics

  print,'Parcel: ',parcelp,' mb',parcelt-273.15,' C',parcelw*1000.0,' g/kg'
  print,'LCL: ', plcl,' mb',tlcl,' K',lclth,' K',lclte2,' K'
  print,'LFC: ', lfcp,' mb',lfcz,' m',lfct-273.15,' C'
  print,'EL: ',elp,' mb',elz,' m',elt-273.15,' C'
  print,'LI: ',li
  print,'CAPE:',cape,' J/kg '
  print,'CIN:',cin,' J/kg  '
  print,'Rev. CAPE:',rcape,' J/kg  '
  print,'Rev. CIN:',rcin,' J/kg  '
  print,'500 mb CAPE:', cape500,' J/kg'
;  goto,compute

;nocompute:avecape(mmm)=0.0
;          avecin(mmm)=0.0
;          avercape(mmm)=0.0
;          avercin(mmm)=0.0
;          avec500(mmm)=0.0
;          lclts(mmm)=0.0
;          elts(mmm)=0.0
;          lfcts(mmm)=0.0
;          avepte(mmm)=0.0
;compute:

end
