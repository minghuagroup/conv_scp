function get_cmip6_mip_number, my_mip
  restore,'cmip6_macro.sav'
  mips = cmip6.mips

  my_k = where(mips eq my_mip, cnt)
  if(cnt eq 0)then begin
     print,'my_mip name is incorrect, stopped in get_cmip6_mip_number.pro '
     stop
   endif
  return, my_k[0]
end


