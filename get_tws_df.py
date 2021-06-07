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
# myHistDate = ["20201022 23:59:00 GMT", "50 D", "20 mins", "MIDPOINT"]
# myHistDate = ["20201030 23:59:00 GMT", "30 D", "1 hour", "MIDPOINT"]
do_plot     = True
savefig     = False
savefile    = True
RTH         = True

myHistDate = ["20201106 23:59:00 GMT", "360 D", "1 day", "MIDPOINT"]
#myHistDate = ["20201022 23:59:00 GMT", "50 D", "20 mins", "MIDPOINT"]
mySymbol = 'SPY'
myType   = 'STK'

filename = get_filename1(myHistDate, mySymbol, myType, 'C', 1.0, '')
df2      = get_tws_df(filename,savefile)

#exit()  # if just to convert a file

if(do_plot == False):
    exit()


OtherSymbols = []
OtherSymbols = ['JETS']
OtherTypes   = ['STK']
# ymin = 15.0
# ymax = 32.0
n_tickdays = int(int(myHistDate[1].split(' ')[0])/20)   # about 20 ticks

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

plt.figure()

fig, ax,  = plt.subplots( figsize=(10, 6))
#
y2 = df2['Close'].tolist()
x2 = df2['Date'].tolist()
x2 = mytimelist2str(x2)
x2 = list(map(str, x2))
print('x2 =',x2[0],' to ',x2[-1])
ax.plot(x2, y2, color='purple', label=mySymbol)
ax.set(xlabel='Date', ylabel='Price', title = mySymbol + ' Price History')

if('day' in myHistDate[1]):
    tick_spacing = n_tickdays
else:
    tick_spacing = n_tickdays*(6*3+3)

ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax.get_xticklabels(), rotation=45)
cticks = ax.get_xticklabels()
ax.set_xticklabels( convert_cticks(cticks) )
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)

#######################
# ## More than 1 line to add here
# ddf = list()
# i=0
# for other in OtherSymbols:
#     filename = get_filename1(myHistDate, other, OtherTypes[i], 'C', 1.0, '')   #######################################
#     i = i + 1
#     df3 =  get_tws_df(filename,False)
#     ax.plot(x2, df3['Close'].tolist(), color=Colors[i%6+1], label=other)
#
# plt.legend(loc='best')
#
# if(savefig):
#     dir = 'figs/'+mySymbol
#     if(os.path.exists(dir) == False):
#         cmd = 'mkdir '+dir
#         os.mkdir(dir)
#         savefile = 'figs/'+mySymbol+'/myplot'+'_'+ mySymbol+'_'+ myType+'.png'
#         plt.savefig(savefile)
#         print(' plot file saved as ', savefile)

plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None,)
plt.show(block = False)
plt.show()
