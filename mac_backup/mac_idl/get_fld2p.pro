function get_fld2p,filein,var,p2=p2,ps=ps

  d =  get_fld(filein,var)
  
  d2 = m2p(d,p2=p2,ps=ps)
  
 return,d2
end
 

