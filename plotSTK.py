from ContractSamples import ContractSamples
import os
import os as os
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from datetime import datetime
import matplotlib.ticker as ticker
from ContractSamples import ContractSamples
# from import myContract_Spec import myContract_Specs
# from myContract_Spec import myfilenames_req
# from myContract_Spec import get_filename
from myContract_Spec import *
from myReadSamples import *

#######################################################################################
# myqueryTime = "20201022 23:59:00 GMT"  # end data
# myHistDate = [myqueryTime, "100 D", "20 mins", "MIDPOINT"]  # TRADES BID-ASK MIDPOINT
# myHistDate = [myqueryTime, "50 D", "20 mins", "MIDPOINT"]
x0min =  '20200801 00:00:00'
x0max =  '20201022 23:59:00'

RTH = True   #Regular Trading Hour
savefig = True
savefile = False

n_tickdays = 5
#
mySymbol, myType, myHistDate = myHead()

if(myType == 'STK' or myType == 'FUT'):
    pass
else:
    print(' >>>> Stopping in plotSTK.py. Make sure that the Head in myContract_Spec.py is set correctly')
    exit()

filename = get_filename2(myHistDate, mySymbol, myType, 'C', 1.0, '')
df  = read_Data(filename)
df2 = df2df(df, RTH)
if(savefile):
    print(filename)
    c = str(filename[0]).split('.')
    print(c)
    filename_out = c[0]+'_p.csv'
    df2.to_csv(filename_out)
    print('New data file saved in ', type(filename_out), filename_out)

if(df2.Date[0] == '00000000'):
    exit()
df3 = df2[df2['Date'] >= x0min]
df2 = df3.reset_index(drop=True)
df3 = df2[df2['Date'] <= x0max]
df2 = df3.reset_index(drop=True)
#

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

plt.figure()

fig, ax,  = plt.subplots( figsize=(10, 6))
#
y2 = df2['Close'].tolist()
x2 = df2['Date'].tolist()
x2 = mytimelist2str(x2)
print('x2 =',x2[0],' to ',x2[-1])
ax.plot(x2, y2, color='Purple', label=mySymbol)
ax.set(xlabel='Date', ylabel='Price', title = mySymbol + ' Price History')

tick_spacing = n_tickdays * (3 * 6 +2 )
ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
plt.draw()
plt.setp(ax.get_xticklabels(), rotation=45)
cticks2 = ax.get_xticklabels()
for i in range(len(cticks2)):
    a = cticks2[i].get_text()
    if (i % 10 == 1):
        b = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
    else:
        b = a[4:6] + '/' + a[6:8]
    cticks2[i].set_text(b)
ax.set_xticklabels(cticks2)#
plt.gca().yaxis.grid(True, )
plt.gca().xaxis.grid(True)
plt.draw()

plt.legend(loc='best')

if(savefig):
    dir = 'figs/'+mySymbol
    if(os.path.exists(dir) == False):
        cmd = 'mkdir '+dir
        os.mkdir(dir)
        savefile = 'figs/'+mySymbol+'/myplot'+'_'+ mySymbol+'_'+ myType+'.png'
        plt.savefig(savefile)
        print(' plot file saved as ', savefile)

plt.show(block = False)
plt.show()
