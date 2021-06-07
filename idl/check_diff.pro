function check_diff, aa, bb

  r = 1
  cc = aa - bb
  if(min(cc) eq max(cc))then r = 0

  return, r

end

