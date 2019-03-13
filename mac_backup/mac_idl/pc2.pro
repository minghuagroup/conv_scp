;    black,  red  , green , blue  ,yellow , cyan  ,  mag  , white
c1=[   0  ,   1   ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ]
c2=[   0  ,   0   ,   1   ,   0   ,   1   ,   1   ,   0   ,   1   ]
c3=[   0  ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ,   1   ]



 c1=c1*255
 c2=c2*255
 c3=c3*255 

 restore,'~mzhang/idl/my_rain18.color'
 nm=n_elements(r2)
 tvlct,[255,r2,0],[255,g2,0],[255,b2,0]  
 print,n_elements([255,r2,0]),' colors (rainbow)'
end
