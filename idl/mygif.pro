pro mygif,filen
if(strpos(filen,'gif') le 0)then filen=filen+'.gif'
;restore,'~mzhang/idl/current.color'


;    black,  red  , green , blue  ,yellow , cyan  ,  mag  , white
c2=[   0  ,   1   ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ]
c3=[   0  ,   0   ,   1   ,   0   ,   1   ,   1   ,   0   ,   1   ]
c4=[   0  ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ,   1   ]

;vlct,c2*255,c3*255,c4*255
;R4 = c2*255
;G4 = c3*255
;B4 = c4*255

if(strpos(filen,'.gif') lt 0)then filen = filen+'.gif'
print,filen
TVLCT,R4,G4,B4,/get
write_jpeg,filen,TVRD(),R4,G4,B4
return
end

