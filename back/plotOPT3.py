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

x0min =  '20200801 00:00:00'
x0max =  '20201022 23:59:00'

RTH = True
savefig = False
test_mode = True

mySymbol, myType, myHistDate = myHead()
if(mySymbol == 'JETS' and myType == 'OPT'):
    ytop = 4.0
elif (mySymbol == 'LYFT' and myType == 'OPT'):
    ytop = 8.0
else:
    ytop = 10.0

myContracts = myContract_Specs(mySymbol, myType)
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
    savefig = False

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']
tick_spacing = 5 * (3 * 6) + 6

ii = 0
for Right in myRights:
    for Strike in myStrikes:
        #
        plt.figure()
        Title = get_Title(mySymbol, myType, Strike, Right, 'at Different Exp Dates', RTH)
        fig, (ax, ax2) = plt.subplots(2, 1, figsize=(10, 8))  # , sharex=True)
        #
        ax = plt.subplot(211)
        jj = 0      # number of lines in the OPT plot
        for Exp in myExpDates:
            filename = get_filename2(myHistDate, mySymbol, myType, Right, Strike, Exp)
            df  = read_Data(filename)
            #print('df1',filename, df.head(20))
            if(df.empty):
                break
            df2 = df2df(df, RTH)
            if(df2.Date[0] == '00000000'):
                break
            jj = jj+1
            df3 = df2[df2['Date'] >= x0min]
            df2 = df3.reset_index(drop=True)
            df3 = df2[df2['Date'] <= x0max]
            df2 = df3.reset_index(drop=True)
            #print(df.head(3))
            #
            y2 = df2['Close'].tolist()
            x2 = df2['Date'].tolist()
            x2 = mytimelist2str(x2)
            #
            ax.plot(x2, y2, color=Colors[jj], label=Exp)
            ax.set(xlabel='Date', ylabel='Option, Price', title=Title)
            tick_spacing = 2 * (3 * 6 +1 )
            ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            if(jj==1):
                plt.draw()
                plt.setp(ax.get_xticklabels(), rotation=45)
                #
                cticks = ax.get_xticklabels()
                #c2 = mytime_label(cticks)
                #ax.set_xticklabels(c2)
                print('cticks', type(cticks),len(cticks))
                print('cticks',cticks)
                for i in range(len(cticks)):
                    a = cticks[i].get_text()
                    if(i%5 == 1):
                        b = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
                    else:
                        b = a[4:6] + '/' + a[6:8]
                    cticks[i].set_text(b)
                    #print(i)
                ax.set_xticklabels(cticks)
                #
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
                x0min = min(x2)
                x0max = max(x2)
                ax.set_ylim(bottom=0, top= ytop)                 ############
                ax.set_xlim(left=x0min, right=x0max)  # 2+dtop)
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
            tick_spacing = 2 * (3 * 6 + 2 )
            ax2.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            plt.draw()
            plt.setp(ax2.get_xticklabels(), rotation=45)
            cticks2 = ax2.get_xticklabels()
            for i in range(len(cticks2)):
                a = cticks2[i].get_text()
                if(i%5 == 1):
                    b = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
                else:
                    b = a[4:6] + '/' + a[6:8]
                cticks2[i].set_text(b)
                #print(i)
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
