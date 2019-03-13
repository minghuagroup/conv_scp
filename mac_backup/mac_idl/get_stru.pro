function get_stru,st,name
  vars=strupcase( tag_names(st))
  jj=where(vars eq strupcase(name)) & jj=jj[0]
;  print,vars
;  print,name
; stop
  d=st.(jj)
  return,d
end

