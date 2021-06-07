function limit, aa, v

jj = where(abs(aa) ge abs(v), cnt)
if(cnt eq 0)then return, aa

aa[jj] = !Values.F_NAN

return, aa

end