import os
import os as os
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from datetime import datetime
from myReadSamples import *
from mybb import *
import warnings

warnings.filterwarnings("ignore")

savefig  = True

mySymbol = 'BABA'


year_min = 2019 #2020
year_max = 2020

month_min, day_min = 9, 1  # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max = 11, 13  # 10,30

date_min = month_min*100 + day_min
date_max = month_max*100 + day_max
x0min = 10000 * year_min + date_min
x0max = 10000 * year_max + date_max
x0minExp = x0min  # Expiration date range
x0maxExp = mydaylist2int( myintlist2day(x0max) + timedelta(15) )  #Contract with expiration 15 days after time window

#
filein = 'data3/'+mySymbol+'_bb.csv'

bb_extract(mySymbol)
#####################

df   = pd.read_csv(filein, header=0)
df.drop_duplicates(subset="Date",keep="first", inplace=True)
df.reset_index(drop=True, inplace = True)
df = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]

df.to_csv('junk4.csv')
y2    = df['UnderlyingPrice'].tolist()
x2    = df['Date'].tolist()
x2 = list(map(str, x2))

n_tickdays = get_ntickdays(x2, 20, 5)   # every 5 days, but maximum tick number is 20

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']
# dx0min = datetime.strptime(x0min, "%Y%m%d")
# dx0max = datetime.strptime(x0max, "%Y%m%d")

plt.figure()

fig, ax,  = plt.subplots( figsize=(10, 6))
#
ax.plot(x2, y2, color='Purple', label=mySymbol)
ax.set(xlabel='Date', ylabel='Price', title = mySymbol + ' Price History')
ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax.get_xticklabels(), rotation=45)
cticks2 = ax.get_xticklabels()
for i in range(len(cticks2)):
    a = cticks2[i].get_text()
    b = a[4:6] + '/' + a[6:8]
    if(i % 10 == 1):
        b =   a[0:4]+'\n' +  b
    cticks2[i].set_text(b)
ax.set_xticklabels(cticks2)
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)
plt.draw()

plt.legend(loc='best')

if(savefig):
    dir = 'figs/'+mySymbol
    if(os.path.exists(dir) == False):
        cmd = 'mkdir '+dir
        os.mkdir(dir)
        savefile = 'figs/'+mySymbol+'/bbplot'+'_'+ mySymbol+'.png'
        plt.savefig(savefile)
        print(' plot file saved as ', savefile)

plt.show(block = False)
plt.show()
