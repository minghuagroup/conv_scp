function levmn,d
 dmin=min(d)
 dmax = max(d)
 print,dmin,dmax
 lev=cal_lev([dmin,dmax],21)
 return,lev
end
