function dev2,dd,first=first

 d=dd
 if(keyword_set(first))then d=transpose(dd)
 
 dm = ave2(d)
 n1 = n_elements(dm)
 d2 = d*0

 for i=0,n1-1 do d2[i,*] = d[i,*]-dm[i]

 if(keyword_set(first))then d2=transpose(d2)
 return,d2

end
