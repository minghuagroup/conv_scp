#
from myReadSamples import *

###################################
mySymbol = 'ES'
bb_extract(mySymbol)
Strikes = bb_get_Strikes(mySymbol)
print('Strikes: ',Strikes)
Exps     = bb_get_Exps(mySymbol)
print('Expiration Dates:',Exps)

