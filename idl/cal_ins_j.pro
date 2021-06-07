
      pro cal_ins,t,lon,lat,ins,cosz,delta=delta

      tw=2.0*3.1416*(t-1.)/365.0
      ecc= 1.000110+0.034221*COS(tw)+0.001280*SIN(tw)+   $
             0.000719*COS(2.0*tw)+0.000077*SIN(2.0*tw) ; eccentricity

      pif=3.1416/180.0

      delta=.006918 - .399912*cos(tw) + .070257*sin(tw) -    $
            .006758*cos(2.*tw) + .000907*sin(2.*tw) -        $
            .002697*cos(3.*tw) + .001480*sin(3.*tw)       

      cr1=pif*279.367 + 0.985647*365.0/360.0*tw

      dt=-105.4*SIN(cr1)+596.2*SIN(2*cr1)+4.3*SIN(3*cr1)-12.7*SIN(4*cr1)- $
         429.2*COS(cr1)-2.1*COS(2*cr1)+19.3*COS(3*cr1)  ; in seconds

      tt=dt/3600.0/24.0*2.0*3.1416/365.0+tw;

      cosz=sin(lat*pif)*sin(delta) - cos(lat*pif)*cos(delta)*   $
            cos(tt+lon*pif)
;;            cos(tt*365.0+lon*pif)

      ins=1367.0*cosz*ecc

      if(min(ins) lt 0.0)then begin
      ins(where(ins lt 0.0))=0.0
      endif
end
