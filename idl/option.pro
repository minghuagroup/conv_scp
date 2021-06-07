function option, S, K, t, sig
 sigt = sig*sqrt(t)
 d1 = (alog(S/K)+0.5*sigt*sigt)/sigt
 d2 = d1 - sigt
 N1 = gauss_pdf(d1)
 N2 = gauss_pdf(d2)
 C = S*N1 - K*N2
 return,C
end


