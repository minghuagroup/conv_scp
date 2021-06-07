file = 'KODK'
file = 'SPY'
file = 'AAPL'
file = 'LYFT'

sed_data =  READ_CSV(file+'.csv',HEADER=SedHeader, $
   N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
;PRINT, SedHeader
;PRINT, SedTableHeader
;HELP, sed_data, /STRUCTURES

date = sed_data.field1
open = sed_data.field2
high = sed_data.field3
low  = sed_data.field4
close = sed_data.field5
adj_close = sed_data.field6
volume = sed_data.field7

dev_close = stddev(close)
dev       = dev_close/mean(close)

  t = 3.

CASE file of

'LYFT': begin

  S = 27.14
  K = 27.00
  c0 = 0.59
        end
'AAPL': begin
  S = 114.09
  K = 115.
  c0 = 1.37
        end
'KODK': begin
  S  = 9.
  K  = 9.
  c0 = 0.45 

  K=6.5
  c0 = 2.5

  K=10.
 c0 = 0.2

        end
'SPY': begin
  t = 1.5
  S = 333.84
  K = 333.84
  c0 = 2.5

  t = 5.5
  K = S
  c0 = 4.4

  t=1.5
  K = 320.
  c0 = 14.0

  K = 327.
  c0 = 7.4

  t = 3.5
  t = 3.0
  S = 333.84
  K = S
  c0 = 3.6


;  t = 1.5
;  S = 3345.5
;  K = 3345.0
;  c0 = 22.5
  
  
   t = 3.5
;  S = 3345.5
;  K = 3345.0
;  c0 = 35.

;  t = 5.5
;  S = 3345.5
;  K = 3345.0
;  c0 = 43.25

       end 

 else:
endcase

t0 = t
t  = t0/253.

VOL,open[0:*],close[0:*],v1,v2
;----------------------

opt_call   = OPTION(S, K, t, v1)
opt_call2  = OPTION(S, K, t, v2)
imp_sig    = IMPLIED(S,K,t,C0)


print,'==========================='
print,'TICK: ',file
print,'mean and std',mean(close),dev
print,'option call price and implied    volatility',C0, imp_sig
print,'calculated call0  price and real volatility',opt_call, v1
print,'calculated call2  price and real volatility',opt_call2, v2
print,'==========================='



end
