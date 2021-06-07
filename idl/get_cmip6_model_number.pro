function get_cmip6_model_number, my_model
  restore,'cmip6_macro.sav'
  models = cmip6.models

  my_k = where(models eq my_model, cnt)
  if(cnt eq 0)then begin
     print,'my_model name is incorrect, stopped in get_cmip6_model_number.pro '
     stop
   endif
  return, my_k[0]
end


