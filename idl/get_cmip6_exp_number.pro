function get_cmip6_exp_number, my_exp, mipid

  restore,'cmip6_macro.sav'

 Exps = get_stru(cmip6,mipid)
 
 my_e = where(Exps eq my_exp, cnt)
  if(cnt eq 0)then begin
    print,'*************************
     print,'my_exp name is incorrect, stopped in get_cmip6_exp_number.pro because ', my_exp, ' IS NOT in  ---> ',mipid
    print,'should be among ', exps
    print,'*************************
     stop
   endif
  return, my_e[0]
end


