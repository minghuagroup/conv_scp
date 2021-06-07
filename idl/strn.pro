function strn,n,nn
 ; append 0 to n with nn digits
 n1=strtrim(100000+n,2)
 sn = strmid(n1,5-nn+1,nn)
 return,sn
end

