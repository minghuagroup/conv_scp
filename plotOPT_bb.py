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
#######################################################################################

savefig = False
test_mode = True

year_min = 2020
year_max = 2020

month_min, day_min = 9, 1  # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max = 11, 13  # 10,30


############ overide below #####################
#
mySymbol = 'LYFT'
#
############ overide below #####################

date_min = month_min*100 + day_min
date_max = month_max*100 + day_max
x0min = 10000 * year_min + date_min
x0max = 10000 * year_max + date_max
x0minExp = x0min  # Expiration date range
x0maxExp = mydaylist2int( myintlist2day(x0max) + timedelta(15) )  #Contract with expiration 15 days after time window

filein = 'data3/'+mySymbol+'_bb.csv'

bb_extract(mySymbol)
######################

myStrikes  = bb_get_Strikes(mySymbol)
myExpDates = bb_get_Exps(mySymbol)
myRights = ['P'] #,'P']

print(myRights)
print(myStrikes)
print(myExpDates)

############ overide below to narrow range #####################
#
myStrikes = [26.5] #30.0] #340.0] #[26.0]
#
############ overide below #####################

nn = len(myExpDates)
for i in reversed(range(nn)):
    for date in myExpDates:
        if ((date < str(x0minExp)) | (date > str(x0maxExp))):
            myExpDates.remove(date)
#
if(test_mode):
    myStrikes = [myStrikes[0]]

# dx0min = datetime.strptime(str(x0min), "%Y%m%d")
# dx0max = datetime.strptime(str(x0max), "%Y%m%d")

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

ii = 0

df   = pd.read_csv(filein, header=0)
df = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]
df = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]
df0 = df.copy()
df0.drop_duplicates(subset="Date",keep="first", inplace=True)
df0.reset_index(drop=True, inplace = True)
stock   = df0['UnderlyingPrice'].tolist()
x2stock = df0['Date'].tolist()

xx3 = list()
yy3 = list()
for Right in myRights:
    for Strike in myStrikes:
        xx2 = list()
        yy2 = list()
        for Exp in myExpDates:
            #print('Right', Right,Strike,Exp)
            df2 = bb_read_data(df, gRight(Right), Strike, gExp(Exp))
            if (df.empty):
                break
            df4 = df0.copy()
            df4['Last'] = -999.
            nn = df2.shape[0]
            i = 0
            for i in range(nn):
                Date = df2['Date'].iloc[i]
                j = df0[df4['Date'] == Date].index[0]
                df4.iloc[j] = df2.iloc[i]
            df2 = df4.copy()
            y2 = df2['Last'].tolist()
            for i in range(len(y2)):
                if(y2[i] < 0.05 and y2[i] > -10.):
                    y2[i] = np.NaN
            x2 = df2['Date'].tolist()
            x2 = list(map(str, x2))
            xx2.append(x2)
            yy2.append(y2)
            n_tickdays = get_ntickdays(x2, 20, 5)  # every 5 days, but maximum tick number is 20
        plt.figure()
        Title = mySymbol + ' OPT Price at Strike ' + str(Strike) + Right + ' for Various Exp Dates'
        fig, (ax, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
        #
        ax = plt.subplot(211)
        ymax = -9999.0
        ymin = 9999.0
        for i in range(len(yy2)):
            ytemp = yy2[i].copy()
            #ytemp[np.array(ytemp) < 0] = np.NaN
            ymax = max([ymax,max(ytemp)])
            ymin = 0.0
        jj = 0  # number of lines in the OPT plot
        for Exp in myExpDates:  # [0:3]:
            x2 = xx2[jj]
            y2 = yy2[jj]
            jj = jj+1
            n_tickdays = int(len(x2) / 20)
            n_tickdays = n_tickdays - (n_tickdays % 5)
            if (n_tickdays < 5):
                n_tickdays = 5
            if (n_tickdays > 20):
                n_tickdays = 20
            if (jj == 1):
                ax.scatter(x2, y2, color=Colors[jj], label='', marker='o', s=1)  # to align the xlabels
            for i in range(len(y2)):
                if (y2[i] < 0.05):
                    y2[i] = np.NaN
            ax.plot(x2, y2, color=Colors[jj % 7], label=Exp)
            ax.set(xlabel='Date', ylabel='Option, Price', title=Title)
            ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
            if (jj == 1):
                plt.draw()
                plt.setp(ax.get_xticklabels(), rotation=45)
                #
                cticks2 = ax.get_xticklabels()
                for i in range(len(cticks2)):
                    a = cticks2[i].get_text()
                    b = a[4:6] + '/' + a[6:8]
                    if (i % 10 == 1):
                        b = a[0:4] + '\n' + b
                    cticks2[i].set_text(b)
                ax.set_xticklabels(cticks2)
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
                ax.set_ylim(bottom=0, top=ymax)  ############
                # ax.set_xlim(left=x0min, right=x0max)  # 2+dtop)
                # ax.set_ybound(upper=ytop, lower=0.0)
                # ax.set_autoscale_on(False)
                plt.draw()
        plt.legend(loc='best')
        #
        ax2 = plt.subplot(212)
        y2 = stock
        x2 = x2stock
        x2 = list(map(str, x2))
        ax2.plot(x2, y2, color=Colors[5], label='STK')
        ax2.set(xlabel='Date', ylabel='STK, Price', title='STK')
        #
        ax2.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
        plt.draw()
        plt.setp(ax2.get_xticklabels(), rotation=45)
        cticks = ax2.get_xticklabels()
        for i in range(len(cticks)):
            a = cticks[i].get_text()
            b = a[4:6] + '/' + a[6:8]
            if (i % 10 == 1):
                b = a[0:4] + '\n' + b
            cticks[i].set_text(b)
        ax2.set_xticklabels(cticks)
        plt.gca().yaxis.grid(True)
        plt.gca().xaxis.grid(True)
        # ax2.set_xlim(left=x0min, right=x0max )
        # ax2.set_autoscale_on(False)
        # #
        plt.legend(loc='best')
        #
        if (savefig):
            dir = 'figs/'+mySymbol
            if(os.path.exists(dir) == False):
                cmd = 'mkdir '+dir
                os.mkdir(dir)
                #
                savefile = 'figs/'+mySymbol+'/myplot'+'_'+ mySymbol+'_'+str(Strike)+Right+'.png'
                plt.savefig(savefile)
                print(' plot file saved as ', savefile)
        plt.show(block = False)
        plt.show()
