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
savefile    = False #True
RTH         = True

month_min, day_min    =  3, 1   # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max    =  10,30
date_min = month_min*100 + day_min
date_max = month_max*100 + day_max
x0min = 10000 * 2020 + date_min
x0max = 10000 * 2020 + date_max

x0min = str(x0min)
x0max = str(x0max)

x0min = x0min[0:4]+'-'+x0min[4:6]+'-'+x0min[6:8]
x0max = x0max[0:4]+'-'+x0max[4:6]+'-'+x0max[6:8]

myHistDate = ["20201106 23:59:00 GMT", "360 D", "1 day", "MIDPOINT"]
#myHistDate = ["20201022 23:59:00 GMT", "50 D", "20 mins", "MIDPOINT"]
mySymbol1 = 'SPY'
myType1   = 'STK'

filename1 = get_filename1(myHistDate, mySymbol1, myType1, 'C', 1.0, '')
df1      = get_tws_df(filename1,savefile)

df0 = df1[(df1['Date'] >= x0min) & (df1['Date'] <= x0max)]
df0.drop_duplicates(subset="Date", keep="first", inplace=True)  # for underlying stock price
df0.reset_index(drop=True, inplace=True)
df1 = df0.copy()

mySymbol2 = 'ES'
myType2   = 'FUT'

filename2 = get_filename1(myHistDate, mySymbol2, myType2, 'C', 1.0, '')
df2      = get_tws_df(filename2,savefile)

n_tickdays = int(int(myHistDate[1].split(' ')[0])/20)   # about 20 ticks
n_tickdays = 5

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

plt.figure()

fig, ax,  = plt.subplots( figsize=(10, 6))
#
x1 = df1['Date'].tolist()
y1 = df1['Close'].tolist()

x2 = df2['Date'].tolist()
y2 = df2['Close'].tolist()

y3 = get_x2_aligned(x1,y1,x2,y2)
for i in range(len(y3)):
    if(y3[i] < 500.):
        y3[i] = np.NaN
    else:
        y3[i] = y3[i]/10.

x1 = mytimelist2str(x1)
x1 = list(map(str, x1))
#print('x1 =',x1[0],' to ',x1[-1])


xd = ['20191220','20200320','20200619','20200918']
for i in range(len(xd)):
    xd[i] = xd[i] + ' 00:00:00'

yd = [1.384,1.406,1.366,1.339]
y4 = get_x2_aligned(x1,y1,xd,yd)
print(xd,yd)
print(x1[0:10])
print(y4[0:10])

if('day' in myHistDate[1]):
    tick_spacing = n_tickdays
else:
    tick_spacing = n_tickdays*(6*3+3)

ydiff = y1.copy()
for i in range(len(x1)):
    ydiff[i] = y1[i] - y3[i]

# zip_data = zip(x1,y1,y3,ydiff)
# for date, spy, es, diff in zip_data:
#     print(date, '  ', spy, '  ', es,  '  ', diff)
#
# exit()

Title0 =   mySymbol1 + ' and ' + mySymbol2 + ' Comparison'
#fig, (ax, ax2, ax3, ax[iP]) = plt.subplots(4, 1, figsize=(10, 12), sharex=True)
fig, axx = plt.subplots(2, 1, figsize=(10, 12), sharex=True)
fig.suptitle(Title0, fontsize=16)

iP = 0
axx[iP] = plt.subplot(211)
ax = axx[iP]
ax.set(xlabel='Date', ylabel='Price', title = mySymbol1 + ' and  ' + mySymbol2 + ' Price History')
ax.plot(x1, y1, color='purple', label = mySymbol1)
ax.plot(x1, y3, color='blue'  , label = mySymbol2)
ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax.get_xticklabels(), rotation=45)
cticks = ax.get_xticklabels()
ax.set_xticklabels( convert_cticks(cticks) )
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)
#ax.annotate('Hello', (30,220),textcoords ='data',size=30) #axes fraction', size=40 )#
plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None)


iP = 1
axx[iP] = plt.subplot(212)
ax = axx[iP]
ax.set(xlabel='Date', ylabel='Price Difference', title = mySymbol1 + ' and  ' + mySymbol2 + ' Divident History')
ax.plot(x1, ydiff, color='red', label = 'SPY - ES')
ax.scatter(x1, ydiff, marker = 'x', color='red', s=10)
ax.scatter(x1, y4, color='blue', label = 'Dividend', s=50)
ax.plot(x1,[0.0]*len(x1), color = 'green')
ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax.get_xticklabels(), rotation=45)
cticks = ax.get_xticklabels()
ax.set_xticklabels( convert_cticks(cticks) )

ax.set_ylim(-15.0,  15.0 )

plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)
plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None)





#ax.plot(x1, y1, color='purple', label = mySymbol1)
#######################
# ## More than 1 line to add here
# ddf = list()
# i=0
# for other in OtherSymbols:
#     filename = get_filename1(myHistDate, other, OtherTypes[i], 'C', 1.0, '')   #######################################
#     i = i + 1
#     df3 =  get_tws_df(filename,False)
#     ax.plot(x1, df3['Close'].tolist(), color=Colors[i%6+1], label=other)
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
