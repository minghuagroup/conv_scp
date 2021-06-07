function get_cmip_years, modelid, mipid, expid,yearrange = yearrange

  if(keyword_set(yearrange))then return, year_range  


case 1 of  
  (strpos(expid,'CO2') ge 0 or strpos(expid,'abrupt') ge 0) :     yearrange = [80, 99] 
  (strpos(expid,'amip') ge 0 or (strpos(expid,'historical') ge 0 )) : yearrange = [1995,2014]
  (strpos(expid,'piControl') ge 0):     yearrange = [281,300]
  (strpos(expid,'ssp') ge 0):     yearrange = [2080,2099]

else: yearrange = [80, 99]  ;default
endcase
 
   return,yearrange
end


