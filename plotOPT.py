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

x0min =  '20200910 00:00:00'
x0max =  '20201022 23:59:00'
n_tickdays = 5

RTH = True
savefig = False
test_mode = True

mySymbol, myType, myHistDate = myHead()

try:
    myContracts = myContract_Specs(mySymbol, myType)
except:
    print(' >>>> Stopping in plotOPT.py. Make sure that the Head in myContract_Spec.py is set correctly')
    exit()

#myfilenames = myfilenames_req(myHistDate, myContracts)
myRights   = from_myContracts(myContracts, 'Right')
myStrikes  = from_myContracts(myContracts, 'Strike')
myExpDates = from_myContracts(myContracts, 'ExpDate')

print(myRights)
print(myStrikes)
print(myExpDates)

if(test_mode):
    myRights=['C']
    myStrikes = [myStrikes[0]]

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']
tick_spacing = 5 * (3 * 6) + 6

filename = get_filename2(myHistDate, mySymbol, 'STK', 'C', 1.0, '20200101')
df = read_Data(filename)
df2 = df2df(df, RTH)
df3 = df2[df2['Date'] >= x0min]
df2 = df3.reset_index(drop=True)
df3 = df2[df2['Date'] <= x0max]
df2 = df3.reset_index(drop=True)
y2 = df2['Close'].tolist()
x2 = df2['Date'].tolist()
x2 = mytimelist2str(x2)
x0min = min(x2)
x0max = max(x2)
df2STK = df2
df2STK['Close'] = -9999.
print(df2STK.head(3))

tick_spacing = n_tickdays * (3 * 6 + 2)

xx3 = list()
yy3 = list()
ii = 0
for Right in myRights:
    for Strike in myStrikes:
        #
        xx2 = list()
        yy2 = list()
        plt.figure()
        Title = get_Title(mySymbol, myType, Strike, Right, 'at Different Exp Dates', RTH)
        fig, (ax, ax2) = plt.subplots(2, 1, figsize=(10, 8),sharex=True)
        #
        ax = plt.subplot(211)
        jj = 0      # number of lines in the OPT plot
        for Exp in myExpDates[0:3]:
            filename = get_filename2(myHistDate, mySymbol, myType, Right, Strike, Exp)
            df  = read_Data(filename)
            df2 = df2df(df, RTH)
            if (df.empty):
                break
            jj = jj+1
            df3 = df2[df2['Date'] >= x0min]
            df2 = df3.reset_index(drop=True)
            df3 = df2[df2['Date'] <= x0max]
            df2 = df3.reset_index(drop=True)
            #
            df4 = df2STK.copy()
            nn = df2.shape[0]
            i = 0
            for i in range(nn):
                Date = df2['Date'].iloc[i]
                j = df4[df4['Date'] == Date].index[0]
                df4.iloc[j] = df2.iloc[i]
            df2 = df4
            y2 = df2['Close'].tolist()
            x2 = df2['Date'].tolist()
            x2 = mytimelist2str(x2)
            xx2.append(x2)
            yy2.append(y2)
            #
        plt.figure()
        Title = get_Title(mySymbol, myType, Strike, Right, 'at Different Exp Dates', RTH)
        fig, (ax, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
        #
        ax = plt.subplot(211)
        ymax = -9999.0
        ymin = 9999.0
        for i in range(len(yy2)):
            ytemp = yy2[i].copy()
            #ytemp[np.array(ytemp) < 0] = np.NaN
            ymax = max([ymax,max(ytemp)])
            ymin = 0.0 #min([ymin,min(ytemp)])
        print('ymax',ymin,ymax)
        jj = 0  # number of lines in the OPT plot
        for Exp in myExpDates:
            x2 = xx2[jj]
            y2 = yy2[jj]
            jj = jj+1
            if(jj == 1):
                ax.scatter(x2, y2, color=Colors[jj], label='', marker='o', s=1) # to align the xlabels
            for i in range(len(y2)):
                if(y2[i] < 0):
                    y2[i] = np.NaN
            ax.plot(x2, y2, color=Colors[jj], label=Exp)
            #
            ax.set(xlabel='Date', ylabel='Option, Price', title=Title)
            #            tick_spacing = 2 * (3 * 6 +2 )
            ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            if(jj==1):
                plt.draw()
                plt.setp(ax.get_xticklabels(), rotation=45)
                #
                cticks = ax.get_xticklabels()
                for i in range(len(cticks)):
                    a = cticks[i].get_text()
                    if(i%10 == 1):
                        b = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
                    else:
                        b = a[4:6] + '/' + a[6:8]
                    cticks[i].set_text(b)
                ax.set_xticklabels(cticks)
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
                ax.set_ylim(bottom=0, top= ymax)                 ############
                ax.set_xlim(left=x0min, right=x0max)  # 2+dtop)
                ax.set_ybound(upper=ymax, lower=0.0)
                ax.set_autoscale_on(False)
                plt.draw()
            # At leeast one option data is available
        if(jj > 0):
            plt.legend(loc='best')
            #
            ax2 = plt.subplot(212)
            # For STK
            filename = get_filename2(myHistDate, mySymbol, 'STK', Right, Strike, Exp)
            df = read_Data(filename)
            df2 = df2df(df, RTH)
            df3 = df2[df2['Date'] >= x0min]
            df2 = df3.reset_index(drop=True)
            df3 = df2[df2['Date'] <= x0max]
            df2 = df3.reset_index(drop=True)
            #
            y2 = df2['Close'].tolist()
            x2 = df2['Date'].tolist()
            x2 = mytimelist2str(x2)
            ax2.plot(x2, y2, color=Colors[5], label='STK')
            ax2.set(xlabel='Date', ylabel='STK, Price', title='STK')
            #
            #            tick_spacing = 2 * (3 * 6 + 2 )
            ax2.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            plt.draw()
            plt.setp(ax2.get_xticklabels(), rotation=45)
            cticks2 = ax2.get_xticklabels()
            for i in range(len(cticks2)):
                a = cticks2[i].get_text()
                if (i % 10 == 1):
                    b = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
                else:
                    b = a[4:6] + '/' + a[6:8]
                cticks2[i].set_text(b)
            ax2.set_xticklabels(cticks2)
            #
            plt.gca().yaxis.grid(True)
            plt.gca().xaxis.grid(True)
            ax2.set_xlim(left=x0min, right=x0max )
            ax2.set_autoscale_on(False)
            #
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
