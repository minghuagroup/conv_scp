import os as os
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from datetime import datetime
from myReadSamples import *

#######################################################################################

x0min    =  20200701   # Time window
x0max    =  20200830

x0minExp =  20200702   # Expiration date range
x0maxExp =  20200915
#
savefig = True
test_mode = False

x0min    =  20200901  # Time window
x0max    =  20201030

x0minExp =  20200902   # Expiration date range
x0maxExp =  20201115
#
######################################################
#
mySymbol  = 'LYFT'   #'LYFT' #'JETS'
myStrikes = [31.00]  #[17.0] #340.0] #[26.0]
myRights  = ['P']
mySymbol  = 'SPY' #LYFT' #'JETS'
myStrikes = [340.]
#
############ overide below #############################

filein = 'data3/'+mySymbol+'_bb.csv'
if(os.path.exists(filein) == False):
    print('To extract data from bb big database for '+ mySymbol, 'This will take time....')
    bb_extract(mySymbol)

if((len(str(x0min)) != 8 ) | (len(str(x0max)) != 8 ) | (len(str(x0minExp)) != 8 ) | (len(str(x0maxExp)) != 8 )):
    print('Format of dates not correct. Must be 8 digits. You have ', x0min, x0max, x0minExp, x0maxExp)

x0minExp =  max([x0min, x0minExp])
if(x0maxExp < x0max):
    x0maxExp = x0max
bb_extract(mySymbol)
myStrikes0  = bb_get_Strikes(mySymbol)
myExpDates0 = bb_get_Exps(mySymbol)
print(myRights)
print(myStrikes0)
print(myExpDates0)

myExpDates = myExpDates0.copy()
if(mySymbol == 'SPY'):  # limit to one day per week
    temp = list()
    for day in myExpDates0:
        temp_date = mystrlist2day(day)
        wday = WeekDays(temp_date)
        if(wday == 'Mon'):  # Monday only
            temp.append(day)
    myExpDates = temp.copy()

nn = len(myExpDates)
for i in reversed(range(nn)):
    for date in myExpDates:
        if ((date < str(x0minExp)) | (date > str(x0maxExp))):
            myExpDates.remove(date)
#
if(test_mode):
    myRights=['P']
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
        # yy2_Bid = list()
        # yy2_Ask = list()
        for Exp in myExpDates:
            print('Right', Right,Strike,Exp)
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
            # y2_Bid  = df2['Bid'].tolist()
            # y2_Ask  = df2['Ask'].tolist()
            for i in range(len(y2)):
                if(y2[i] < 0.05 and y2[i] > -10.):
                    y2[i] = np.NaN
            x2 = df2['Date'].tolist()
            x2 = list(map(str, x2))
            xx2.append(x2)
            yy2.append(y2)
            # yy2_Bid.append(y2_Bid)
            # yy2_Ask.append(y2_Ask)
        plt.figure()
        Title = mySymbol + ' OPT Price at Strike ' + str(Strike) + Right + ' for Various Exp Dates'
##########
        fig, (ax, ax2, ax3, ax4) = plt.subplots(4, 1, figsize=(10, 16), sharex=True)
        #
        ax = plt.subplot(411)
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
                ax.set_xticklabels(convert_cticks(cticks2))
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
                ax.set_ylim(bottom=0, top=ymax)  ############
                # ax.set_xlim(left=x0min, right=x0max)  # 2+dtop)
                # ax.set_ybound(upper=ytop, lower=0.0)
                # ax.set_autoscale_on(False)
                plt.draw()
                #
            plt.legend(loc='best')
        ax2 = plt.subplot(412)
        y2 = stock
        x2 = x2stock
        x2 = list(map(str, x2))
        ax2.plot(x2, y2, color=Colors[5], label='STK')
        ax2.set(xlabel='Date', ylabel='STK, Price', title='STK')
        ax2.plot(x2, list(np.array(y2) * 0.0 + Strike), color='red', label='Strike')
        #
        n_tickdays = int(len(x2) / 20)
        n_tickdays = n_tickdays - (n_tickdays % 5)
        if (n_tickdays < 5):
            n_tickdays = 5
        if (n_tickdays > 20):
            n_tickdays = 20
        ax2.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
        plt.draw()
        plt.setp(ax2.get_xticklabels(), rotation=45)
        cticks = ax2.get_xticklabels()
        ax2.set_xticklabels( convert_cticks(cticks) )
        plt.gca().yaxis.grid(True)
        plt.gca().xaxis.grid(True)
        plt.legend(loc='best')
        # ax2.set_xlim(left=x0min, right=x0max )

#################   SIMULATION STARTS HERE  #####################################
#
Trading_cost = 0.0065 + stock[0]*0.002 #bid ask

jj=0
for jj in range(len(myExpDates)):
    print(' For each expiration date ==>>>>>',jj, myExpDates[jj])
    print(' This is the time series of the option price for the above expiration date. Nummber of day = ',len(yy2[jj]),yy2[jj])

print('')
nexp = len(myExpDates)
ii=0
for ii in range(len(x2)):
    zz = [0.0]*nexp
    for i in range(nexp):
        zz[i] = yy2[i][ii]
    print(' For each day ==>>>>>', ii, x2[ii])
    print('This is the option price of all contracts, in the above day. Nummber of contracts = ',len(zz),zz)

print('')

ytrades1 = [- 100.0 + np.NaN]*len(x2)
ytrades2 = [- 100.0 + np.NaN]*len(x2)
yCash    = [0.0]*len(x2)
yShort   = [0.0]*len(x2)

yy3 = list()  # take out first contact with all NaN values

myExpDates3 = list()
for jj in range(len(myExpDates)):
    yj = yy2[jj]
    if(np.isnan(yj[0])):
        pass
    else:
        yy3.append(yj)
        myExpDates3.append(myExpDates[jj])

Balance = list()
ii = 0
nexp = len(myExpDates)
index_nearest = 0  # find the initial contract
for ii in range(len(x2)):
    print(' -------------  ii = ', ii, ' --------------- on day ', x2[ii])
    Date = x2[ii]
    index = x2.index(Date)
    StkPrice = stock[index]
    StkPrice0 = StkPrice
    if(ii == 0):
        Balance.append(StkPrice0)
        #
        Found = False
        for i in range(nexp):
            OptPrice = yy2[index_nearest][index]
            #print('????? i, index_nearest, index OptPrice', i, index_nearest, index, OptPrice)
            if(Found == False):
                if(np.isnan(OptPrice)):
                    Found = False
                    index_nearest = index_nearest +1
                else:
                    Found = True
                    break
            else:
                pass
        if(index_nearest > nexp -2):
            print('  !!!!  NO CONTRACTS AVAILABLE DURING THIS PERIOD FOR STRIKE ')
            print('        DATE PERIOD, STRIKE PRICE AND INITIAL STK PRICE ARE', x2[0], x2[-1], \
                  myStrikes0[0], stock[0])
            exit()
        #
        #print(' Step 1 found contract: index_nearest, myExpDates(index_nearest),OptPrice',\
        #      index_nearest, myExpDates[index_nearest],OptPrice)
        Cash  = OptPrice - Trading_cost
        OptPrice0 = OptPrice
        Balance0 = StkPrice0
        ytrades2[ii] = myStrikes[0]
        yCash[ii] = Cash
        yShort[ii] = OptPrice
        #print(' ii index_nearest',ii,  index_nearest)
    else: # Day 2 and after
        print(' ii, index_nearest', ii, index_nearest, index)
        OptPrice = yy2[index_nearest][index]
        #print('     ii OptPrice =========== ', ii, OptPrice)
        if(np.isnan(OptPrice)):
            OptPrice = OptPrice0
        try:    # this is an expiration date
            indexExp = myExpDates.index(Date)
            TradingDay = True
            if (Date < myExpDates[index_nearest]):
                TradingDay = False
        except:
            TradingDay = False
        if(TradingDay):
            index_nearest = index_nearest + 1
            if( StkPrice >  myStrikes[0]):
                print(' ******************  OPTION EXPIRES. NEW TRADE INITIATED FOR CONTRACT WITH EXPIRATIONM  ', \
                      myExpDates[indexExp + 1], ' on ', Date, ' at price ',yy2[indexExp + 1][index] )
                opt1 = 0.0
                ytrades1[index] = myStrikes[0]
            else:
                opt1 = yy2[indexExp][index]
                ytrades2[index] = myStrikes[0]
                print(' ******************  TRADE ROLLOVER FROM Contract ', myExpDates[indexExp], \
                      ' at price ', yy2[indexExp][index], ' to Contract ',\
                      myExpDates[indexExp + 1],' at price ', yy2[indexExp + 1][index], ' on ', Date, )
            opt2 = yy2[indexExp + 1][index]  # nearest again
            if (np.isnan(opt2)):
                print(' This is a strange case in which no rollover is available opt1 opt2 = ', opt1,opt2,\
                      'for day and Expiration date: ', Date, myExpDates[indexExp + 1])
                # should find the next available contract, for now pass
                pass
                # No contact for that day
            else:
                dCash = opt2 - opt1 - Trading_cost
                OptPrice = yy2[indexExp + 1][index]
                Cash = Cash + dCash
                OptPrice = opt2
            #
        else:
            pass
        Balance.append(Balance0 - OptPrice + Cash)
        yCash[ii] = Cash
        yShort[ii] = OptPrice
    print(' On ', x2[index], ' balance consists of initial balance: ', Balance0, \
          'Cash:', Cash, ' Option short position: -', OptPrice)
    print('                  for a Total balance of ', Balance0 - OptPrice + Cash, '.  STK price is ', StkPrice)
    print('')
    OptPrice0 = OptPrice
    ii = ii +1

print('Summary of nd Balance:', Balance)
########################## BELOW MAKE PLOT OF BALANCE ########################

#plt.figure()
Title =  ' Balance with Short ' + mySymbol + ' PUT at Strike ' +str(myStrikes[0])+myRights[0]
ax3 = plt.subplot(413)

ax3.plot(x2, Balance, color='green', label='Balance')
ax3.set(xlabel='Date', ylabel='Balance', title=Title)
#
ax3.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax3.get_xticklabels(), rotation=45)
cticks = ax3.get_xticklabels()
ax3.set_xticklabels(convert_cticks(cticks))
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)

ax3.plot(x2, stock, '--',color='purple', label='Balance if hold STK')
ax3.plot(x2, list(np.array(y2)*0.0 + myStrikes[0]), color='red', label='Strike')
ax3.scatter(x2,ytrades1,color='blue', label='Expires')
ax3.scatter(x2,ytrades2,color='red', label='Trade / Rollover')

plt.legend(loc='best')

########################## BELOW MAKE PLOT OF CASH AND SHORT POSITION ########################

Title =  ' Cash and Short Positions for ' + mySymbol + ' with ' +str(myStrikes[0])+myRights[0]
ax4 = plt.subplot(414)

ax4.bar(x2, yCash, color='green', label='Cash', width=[0.5]*len(x2))
ax4.set(xlabel='Date', ylabel='Cash and Short Debt', title=Title)
ax4.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax4.get_xticklabels(), rotation=45)
cticks = ax4.get_xticklabels()
ax4.set_xticklabels(convert_cticks(cticks))
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)

ax4.bar(x2, list(- np.array(yShort)),color='blue', label='Short Debt',width=[0.5]*len(x2))
ax4.bar(x2, list(np.array(yCash)- np.array(yShort)),color='red', label='Net', width=[0.7]*len(x2))
ax4.plot(x2, list(np.array(yCash)*0.0),color='black', linewidth=1)

plt.legend(loc='best')
#
if (savefig):
    dir = 'figs/'+mySymbol
    if(os.path.exists(dir) == False):
        cmd = 'mkdir '+dir
        os.mkdir(dir)
        #
    savefile = 'figsbb/bb_balance_'+ mySymbol+'.png'
    plt.savefig(savefile)
    print(' plot file saved as ', savefile)
plt.show(block = False)
plt.show()
