from ContractSamples import ContractSamples
import os
import os as os
#import system
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from matplotlib import dates as mdates
from matplotlib import dates as mdates
from datetime import datetime
import matplotlib.ticker as ticker
from ContractSamples import ContractSamples
from myContract_Spec import myContract_Specs
from myContract_Spec import myfilenames_req
from myContract_Spec import get_filename

#from matplotlib import dates as mdates

def read_Data(filename):
    if os.path.exists(filename):
        df = pd.read_csv(filename, header=2)
        print(' in read_STK ', df.head(3))
        print('       length= ', len(df[df.columns[1]]))
        return df
    else:
        print('--- This file does not exist: ', filename)
        #    ES_FUT_20201022_50D_20mins_MIDPOINT.csv
        df = pd.DataFrame({'Date': ['00000000', '99999999'], 'Close': [0, 0]}, index=[0, 1])  #This is a dummy data
        return df

def df2df(df, RTH):
    #RTH = True
    if(df.iloc[0][0] == '00000000'):
        return df

    dd =  df.values.tolist()
    datalist = list()
    ddate  = list()
    dclose = list()
    dopen = list()
    dhigh = list()
    dlow  = list()
    dvolume = list()

    for row in dd:
        cc1 = str(row)
        cc2 = cc1.split(',')
        jdate   = cc2[1][8:26]
        jopen   = float(cc2[2][6:])
        jhigh   = float(cc2[3][6:])
        jlow    = float(cc2[4][5:])
        jclose  = float(cc2[5][7:])
        jvolume = int(cc2[6][8:])
        #newline = {'Date': jdate, 'Open': jopen, 'High':jhigh, 'Low': jlow, 'Close':jclose, 'Volume': jvolume}
        newline = [jdate, jopen, jhigh, jlow, jclose, jvolume]
        #data.append(newline, ignore_index=True)
        datalist.append(newline)
        ddate.append(jdate)
        dopen.append(jopen)
        dhigh.append(jhigh)
        dlow.append(jlow)
        dclose.append(jclose)
        dvolume.append(jvolume)
        #break

    if (RTH):
        ii = 0
        for i in range(len(ddate)):
            a = str(ddate[i])
            ah = int(a[10:12])
            am = int(a[13:15])
            if ((ah < 9) or (ah > 16)):
                pass
            elif ((ah == 9) and (am <= 30)):
                pass
            elif ((ah == 16) and (am > 0)):
                pass
            else:
                ddate[ii] = ddate[i]
                dopen[ii] = dopen[i]
                dhigh[ii] = dhigh[i]
                dlow[ii] = dlow[i]
                dclose[ii] = dclose[i]
                dvolume[ii] = dvolume[i]
                ii = ii + 1
        ddate = ddate[0:ii]
        dopen  = dopen[0:ii]
        dhigh  = dhigh[0:ii]
        dlow = dlow[0:ii]
        dclose = dclose[0:ii]
        dvolume = dvolume[0:ii]

    x = ddate
    xdate = [datetime.now()]*len(x)
    for i in range(len(x)):
        xdate[i] = datetime.strptime(x[i], "%Y%m%d %H:%M:%S")

    data = {'Date': xdate, 'Open': dopen, 'High':dhigh, 'Low': dlow, 'Close':dclose, 'Volume': dvolume}
    #data = {'Date': ddate, 'Open': dopen, 'High': dhigh, 'Low': dlow, 'Close': dclose, 'Volume': dvolume}

    df2 = pd.DataFrame(data)
    print(' in df2df ', df2.head(3))
    print('       length= ', len(df2['Date']))
    #print('len1a = ', len(list(df)))
    #print('len2a = ', len(list(df2)))

    return df2

# # #The following code does not work !! variables not defined
# def get_Title(mySymbol=mySymbol, myType=myType, myStrike=myStrike, myRight=myRight, \
#               myExpDate=myExpDate, RTH=RTH):
def get_Title(mySymbol, myType, myStrike, myRight, myExpDate, RTH):

    if (RTH):
        Title = mySymbol + ' ' + '20-Min ' + myType + ' Close Price During RTH'
    else:
        Title = mySymbol + ' ' + '20-Min ' + myType + ' Close with Extended Market'
        #ndays = 5 * 3 * 7

    if (myType == 'OPT'):
        Title = Title + ' ' + str(myStrike) + myRight + ' Expires on ' + myExpDate

    return Title


def mytimelist2str(dates):
    alist = list()
    for item in dates:
        alist.append(datetime.strftime(item, "%Y%m%d %H:%M:%S"))

    return alist


def mytime_label(c):
    c2 = c
    for i in range(len(c)):
        a = c[i].get_text()
        if (i == 1 or i == len(c) - 4 or i == int(len(c) / 2)):
            c2[i] = a[4:6] + '/' + a[6:8] + '\n  ' + a[9:14]
        else:
            c2[i] = a[4:6] + '/' + a[6:8] #+  '\n  ' + a[9:14]

    return c2

def from_myContracts(Contracts, input):
    NF = len(Contracts)
    Data = [None]*NF
    if(input == 'Right'):
        i=0
        for contract in Contracts:
            Data[i] = contract.right
            i = i+1
    elif (input == 'Strike'):
        i = 0
        for contract in Contracts:
            Data[i] = contract.strike
            i = i + 1
    elif (input == 'ExpDate'):
        i = 0
        for contract in Contracts:
            Data[i] = contract.lastTradeDateOrContractMonth
            i = i + 1

    return list(set(Data))   #unique values


#######################################################################################
myqueryTime = "20201022 23:59:00 GMT"  # end data
myHistDate = [myqueryTime, "100 D", "20 mins", "MIDPOINT"]  # TRADES BID-ASK MIDPOINT
myHistDate2 = [myqueryTime, "50 D", "20 mins", "MIDPOINT"]

#time limits
x0min = '20200901 00:00:00'
x0max = '20201022 23:59:00'
dx0min = datetime.strptime(x0min, "%Y%m%d %H:%M:%S")
dx0max = datetime.strptime(x0max, "%Y%m%d %H:%M:%S")

RTH = True #False Regular Trading Hr
savefig = False

mySymbol = 'JETS' #'LYFT'                  #'LYFT','JETS','ES','SPY','QQQ'
myType   = 'OPT'                     # 'OPT' or 'FOP'
myType2  = 'STK'                   # underlying assest STK or FUT'

myContracts = myContract_Specs(mySymbol, myType)

myRights   = from_myContracts(myContracts, 'Right')
myStrikes  = from_myContracts(myContracts, 'Strike')
myExpDates = from_myContracts(myContracts, 'ExpDate')

print(myRights)
print(myStrikes)
print(myExpDates)

#myfilenames  = myfilenames_req(myHistDate, myContracts)

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']
tick_spacing = 5 * (3 * 6) + 6

ii = 0
for Right in myRights[0:1]:
    for Strike in myStrikes[0:1]:
        #
        plt.figure()
        Title = get_Title(mySymbol, myType, Strike, Right, 'at Different Exp Dates', RTH)
        fig, (ax, ax2) = plt.subplots(2, 1, figsize=(10, 8))  # , sharex=True)
        #
        ax = plt.subplot(211)
        jj = 0      # number of lines in the OPT plot
        for Exp in myExpDates:
            filename = get_filename(myHistDate, mySymbol, myType, Right, Strike, Exp)
            df  = read_Data(filename)
            df2 = df2df(df, RTH)
            if(df2.Date[0] == '00000000'):
                break
            jj = jj+1
            df3 = df2[df2['Date'] >= x0min]
            df2 = df3.reset_index(drop=True)
            df3 = df2[df2['Date'] <= x0max]
            df2 = df3.reset_index(drop=True)
            #
            y2 = df2['Close'].tolist()
            x2 = df2['Date'].tolist()
            #print('y2', len(y2))
            #print('x2', len(x2),x2)
            x2 = mytimelist2str(x2)
            #print('x3', len(x2),x2)
            ax.set_ylabel('Option Price')
            ax.plot(x2, y2, color=Colors[jj], label=Exp)
            ax.set(xlabel='Date', ylabel='Option, Price', title=Title)
            tick_spacing = 2 * (3 * 6 +1 )
            ax.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            if(jj==1):
                plt.draw()
                plt.setp(ax.get_xticklabels(), rotation=45)
                #
                cticks = ax.get_xticklabels()
                c2 = mytime_label(cticks)
                ax.set_xticklabels(c2)
                #
                plt.gca().yaxis.grid(True, )
                plt.gca().xaxis.grid(True)
                x0min = min(x2)
                x0max = max(x2)
                #ax.set_ylim(bottom=0, top= ytop)                 ############
                ax.set_xlim(left=x0min, right=x0max)  # 2+dtop)
                ax.set_autoscale_on(False)
                plt.draw()
        # At leeast one option data is available
        if(jj > 0):
            plt.legend(loc='best')
            #
            ax2 = plt.subplot(212)
            # For STK
            filename = get_filename(myHistDate2, mySymbol, 'STK', Right, Strike, Exp)
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
            #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  use cticks values to put the labels
            tick_spacing = 2 * (3 * 6 + 2 )
            ax2.xaxis.set_major_locator(ticker.MultipleLocator(tick_spacing))
            plt.draw()
            plt.setp(ax2.get_xticklabels(), rotation=45)
            c = ax2.get_xticklabels()
            c2 = mytime_label(c)
            ax2.set_xticklabels(c2)
            #print('x2[0:10]',  len(x2), x2[-1],x2[0])
            print('x2 = ', x2)
            #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            plt.gca().yaxis.grid(True)
            plt.gca().xaxis.grid(True)
            #ax2.set_ylim(bottom=0, top=36)  # 2+dtop)
            ax2.set_xlim(left=x0min, right=x0max )
            #print('left=x0min, right=x0max ', x0min,x0max)
            #print(x2)
            ax2.set_autoscale_on(False)
            #
            plt.legend(loc='best')
            dir = 'figs/'+mySymbol
            if(os.path.exists(dir) == False):
                cmd = 'mkdir '+dir
                os.mkdir(dir)
            if(savefig):
                plt.savefig('figs/'+mySymbol+'/myplot'+'_'+ mySymbol+'_'+str(Strike)+Right+'.png')
            plt.show(block = False)
            plt.show()
