;    black,  red  , green , blue  ,yellow , cyan  ,  mag  , white
c1=[   0  ,   1   ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ]
c2=[   0  ,   0   ,   1   ,   0   ,   1   ,   1   ,   0   ,   1   ]
c3=[   0  ,   0   ,   0   ,   1   ,   0   ,   1   ,   1   ,   1   ]



 c1=c1*255
 c2=c2*255
 c3=c3*255 

 restore,'~mzhang/idl/my_rain18.color'
 nm=n_elements(r2)
 tvlct,[255,c1,r2,0],[255,c2,g2,0],[255,c3,b2,0]  
 print,n_elements([c1,r2,0])+1,' colors (combined+white)'

 r4=[255,c1,r2,0]
 g4=[255,c2,g2,0]
 b4=[255,c3,b2,0]
 save,filename='~mzhang/idl/current.color',r4,g4,b4
end
