import os as os
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from datetime import datetime
from datetime import timedelta
from myReadSamples import *
import colour
import json
import pickle

savefig   = True
Consider_Bid_Ask = True #False
Trading_cost = 0.0065
########################################################################
year_min = 2020
year_max = 2020

month_min, day_min    =  9, 1   # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max    =  10,30

algo        =  2   # 1 Sell covered call;
                    # 2 Sell put
mySymbol    = 'JETS'

# mySymbol    = 'SPY'
# myStrikes   = 310.
# #
# mySymbol    = 'JETS'
# myStrikes   = 17.00
#
strike_min = 17.0
strike_max = strike_min
ReadData   = True
##########################################################################

if (algo == 1):             # sell covered call
    myRights = ['C']
else:                       # sell put
    myRights = ['P']

filein = 'data3/'+mySymbol+'_bb.csv'
if(os.path.exists(filein) == False):
    print('To extract data from bb big database for '+ mySymbol, 'This will take time....')
    bb_extract(mySymbol)

bb_extract(mySymbol)                        # From bb database to extract file
myStrikes0  = bb_get_Strikes(mySymbol)      # From bb database
myExpDates0 = bb_get_Exps(mySymbol)         # From bb database
 
date_min = month_min*100 + day_min
date_max = month_max*100 + day_max
x0min = 10000 * 2020 + date_min
x0max = 10000 * 2020 + date_max
x0minExp = x0min  # Expiration date range
x0maxExp = mydaylist2int( myintlist2day(x0max) + timedelta(15) )  #Contract with expiration 15 days after time window

# Limit the expiration dates within range
nn = len(myExpDates0)
for i in reversed(range(nn)):
    for date in myExpDates0:
        if ((date < str(x0minExp)) | (date > str(x0maxExp))):
            myExpDates0.remove(date)

myExpDates = myExpDates0.copy()

# Limit the strike price range
print('myStrikes1',type(myStrikes0),myStrikes0)
temp = np.array(myStrikes0)
temp = temp[temp >= strike_min]           #########################
temp = temp[temp <= strike_max]           #########################
myStrikes = pd.Series(temp)
print('myStrikes2',type(myStrikes),myStrikes)

# For SPY, limit to one expiration day per week, here "Mon" is used.
if(mySymbol == 'SPY'):
    temp = list()
    for day in myExpDates:
        temp_date = mystrlist2day(day)
        wday = WeekDays(temp_date)
        if(wday == 'Mon'):  # Monday only
            temp.append(day)
    myExpDates = temp.copy()

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

nStrikes   = len(myStrikes)
nExp       = len(myExpDates)
nRights    = len(myRights)
print('nExp', nExp, nStrikes, nRights)

dump_file = 'temp/'+mySymbol + '_' + str(x0min) +'_' + str(x0max)+'.pickle'

ReadData = os.path.exists(dump_file) & ReadData
# # ################################################################################################################
if(ReadData == False):
    df  = pd.read_csv(filein, header=0)                        # read data
    df  = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]
    df0 = df.copy()
    df0.drop_duplicates(subset="Date",keep="first", inplace=True)   # for underlying stock price
    df0.reset_index(drop=True, inplace = True)
    #
    data4D = [[ [ [list() for col in range(4)] for col2 in range(nRights )] for col3 in range(nStrikes)] for row in range(nExp)]
    #
    for iRight in range(len(myRights)):
        Right = myRights[iRight]
        for iStrike in range(len(myStrikes)):
            Strike = myStrikes[iStrike]
            for iExp in range(len(myExpDates)):
                Exp = myExpDates[iExp]
                #print('Right', Right,Strike,Exp)
                df2 = bb_read_data(df, gRight(Right), Strike, gExp(Exp))
                nn  = df2.shape[0]
                if (df.empty):
                    break
                df4 = df0.copy()           # use stock price dataframe as dummy to replace values for plotting purpose
                df4['Last'] = -999.
                df4['Ask'] = -999.
                df4['Bid'] = -999.
                for i in range(nn):
                    Date = df2['Date'].iloc[i]
                    j = df0[ df4['Date'] == Date ].index[0]
                    df4.iloc[j] = df2.iloc[i]
                df2 = df4.copy()           # This is the updated price dataform
                x2 = df2['Date'].tolist()
                y2 = df2['Last'].tolist()
                y2_Bid  = df2['Bid'].tolist()
                y2_Ask  = df2['Ask'].tolist()
                for i in range(len(y2)):
                    if(y2[i] < 0.05 and y2[i] > -10.):
                        y2[i] = np.NaN       # for making scatter plot
                        y2_Bid[i] = np.NaN  # for making scatter plot
                        y2_Ask[i] = np.NaN  # for making scatter plot
                #print('iRight,iStrike,iExp',iRight,iStrike,iExp )
                data4D[iExp][iStrike][iRight][0] = x2
                data4D[iExp][iStrike][iRight][1] = y2
                data4D[iExp][iStrike][iRight][2] = y2_Bid
                data4D[iExp][iStrike][iRight][3] = y2_Ask
            x2 = list(map(str, x2))
    #
    temp = dump_file.split('/')[0]
    if (os.path.isdir(temp) == False):
        os.mkdir(temp)
    with open(dump_file,'wb') as file:
        pickle.dump([df0,data4D], file)
else:
    with open(dump_file, 'rb') as file2:
        temp = pickle.load(file2)
        df0 = temp[0]
        data4D = temp[1]

# ################################################################################################################

stock = df0['UnderlyingPrice'].tolist()
x2 = df0['Date'].tolist()
x2 = list(map(str, x2))

# As test
iStrike = 0
iRight  = 0

#exit()

#################   SIMULATION STARTS HERE  #####################################

if(Consider_Bid_Ask):
    pass
else:
    for iRight in range(len(myRights)):
        Right = myRights[iRight]
        for iStrike in range(len(myStrikes)):
            Strike = myStrikes[iStrike]
            for iExp in range(len(myExpDates)):
                data4D[iExp][iStrike][iRight][2] = data4D[iExp][iStrike][iRight][1]
                data4D[iExp][iStrike][iRight][3] = data4D[iExp][iStrike][iRight][1]

ytrades1 = [- 100.0 + np.NaN]*len(x2)    # to mark whether it is an expiration/new initiation
ytrades2 = [- 100.0 + np.NaN]*len(x2)    # to mark whether it is a roll-over trade
yCash    = [0.0]*len(x2)                 # cash from option trade
yShort   = [0.0]*len(x2)                 # short position debt

Balance = list()
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
        for iExp in range(nExp):
            OptPrice = data4D[index_nearest][iStrike][iRight][1][index]
            OptPrice_Bid = data4D[index_nearest][iStrike][iRight][2][index]
            OptPrice_Ask = data4D[index_nearest][iStrike][iRight][3][index]
            if(Found == False):
                if(np.isnan(OptPrice)):
                    Found = False
                    index_nearest = index_nearest +1
                else:
                    Found = True
                    break
            else:
                pass
        if(index_nearest > nExp -2):
            print('  !!!!  NO CONTRACTS AVAILABLE DURING THIS PERIOD FOR STRIKE ')
            print('        DATE PERIOD, STRIKE PRICE AND INITIAL STK PRICE ARE', x2[0], x2[-1], \
                  myStrikes0[iStrike], stock[0])
            exit()
        Cash  = OptPrice_Bid - Trading_cost
        OptPrice0 = OptPrice_Ask
        Balance0 = StkPrice0
        ytrades2[ii] = myStrikes[iStrike]
        yCash[ii] = Cash
        yShort[ii] = OptPrice_Ask
    else: # Day 2 and after
        OptPrice = data4D[index_nearest][iStrike][iRight][1][index]
        OptPrice_Bid = data4D[index_nearest][iStrike][iRight][2][index]
        OptPrice_Ask = data4D[index_nearest][iStrike][iRight][3][index]
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
            #
            if(algo == 1): # Sell covered call
                if( StkPrice <  myStrikes[iStrike]):
                    print(' ******************  OPTION EXPIRES. NEW CALL INITIATED FOR CONTRACT WITH EXPIRATIONM  ', \
                          myExpDates[indexExp + 1], ' on ', Date, ' at price ',data4D[indexExp+1][iStrike][iRight][2][index])
                    opt1 = 0.0
                    ytrades1[index] = myStrikes[iStrike]      # Make a record
                else:
                    opt1 = data4D[indexExp+1][iStrike][iRight][3][index]
                    ytrades2[index] = myStrikes[iStrike]      # Make a record
                    print(' ******************  CALL TRADE ROLLOVER FROM Contract ', myExpDates[indexExp], \
                          ' at price ', opt1, ' to Contract ', \
                          myExpDates[indexExp + 1], ' at price ', data4D[indexExp+1][iStrike][iRight][2][index], ' on ', Date, )
                opt2 = data4D[indexExp+1][iStrike][iRight][2][index] # nearest again
                if (np.isnan(opt2)):
                    print(' This is a strange case in which no rollover is available opt1 opt2 = ', opt1, opt2, \
                          'for day and Expiration date: ', Date, myExpDates[indexExp + 1])
                    # should find the next available contract, for now pass
                    pass
                    # No contact for that day
                else:
                    dCash = opt2 - opt1 - Trading_cost
                    Cash = Cash + dCash
                    OptPrice = opt2
            #
            else:  # (algo != 1) Sell put
                if( StkPrice > myStrikes[iStrike]):
                    print(' ******************  OPTION EXPIRES. NEW PUT INITIATED FOR CONTRACT WITH EXPIRATIONM  ', \
                          myExpDates[indexExp + 1], ' on ', Date, ' at price ', data4D[indexExp+1][iStrike][iRight][2][index])
                    opt1 = 0.0
                    ytrades1[index] = myStrikes[iStrike]  # Make a record
                    # change contract
                else:
                    opt1 = data4D[indexExp+1][iStrike][0][3][index]
                    ytrades2[index] = myStrikes[iStrike]  # Make a record
                    print(' ******************  PUT TRADE ROLLOVER FROM Contract ', myExpDates[indexExp], \
                          ' at price ',opt1, ' to Contract ', \
                          myExpDates[indexExp + 1], ' at price ',data4D[indexExp+1][iStrike][iRight][2][index], ' on ', Date, )
                opt2 =data4D[indexExp+1][iStrike][iRight][2][index]  # nearest again
                if (np.isnan(opt2)):
                    print(' This is a strange case in which no rollover is available opt1 opt2 = ', opt1,opt2,\
                          'for day and Expiration date: ', Date, myExpDates[indexExp + 1])
                    # should find the next available contract, for now pass
                    pass
                    # No contact for that day
                else:
                    dCash = opt2 - opt1 - Trading_cost
                    Cash = Cash + dCash
                    OptPrice = opt2
                #
        else:  #trading finished
            pass
        if(algo == 1):
            Balance.append(StkPrice - OptPrice + Cash)  # covered call
        else:
            Balance.append(Balance0 - OptPrice + Cash)  # sell put
        yCash[ii] = Cash
        yShort[ii] = OptPrice
    print(' On day ', x2[index], ' balance consists of initial balance: ', Balance0, \
          'Cash:', Cash, ' Option short position: -', OptPrice)
    print('                  for a Total balance of ', Balance0 - OptPrice + Cash, '.  STK price is ', StkPrice)
    print('')
    OptPrice0 = OptPrice
    ii = ii +1

print('Summary of nd Balance:', Balance)
print('x2 x2==> ',x2)

########################## PLOTTING STARTS HERE  #####################################################

# 1 OPT Price
for iRight in range(len(myRights)):
    Right = myRights[iRight]
    for iStrike in range(iStrike, iStrike+1): # len(myStrikes)):
        Strike = myStrikes[iStrike]
        plt.figure()
        fig, (ax, ax2, ax3, ax4) = plt.subplots(4, 1, figsize=(10, 12), sharex=True)
        if (algo == 1):
            Title0 = ' COVERED CALL \n'
        else:
            Title0 = ' SHORT PUT \n'
        #fig.suptitle(Title0, fontsize=20, fontweight='bold', y=.995)
        #
        ax = plt.subplot(411)
        #find min and max for plots
        ymax = -9999.0
        ymin = 9999.0
        for iExp in range(len(myExpDates)):
            ytemp = data4D[iExp][iStrike][iRight][1].copy()
            ymax = max([ymax,max(ytemp)])
            ymin = 0.0
        for iExp in range(len(myExpDates)):
            Exp    = myExpDates[iExp]
            y2     = data4D[iExp][iStrike][iRight][1]
            y2_Ask = data4D[iExp][iStrike][iRight][2]
            y2_Bid = data4D[iExp][iStrike][iRight][3]
            n_tickdays = get_ntickdays(x2,20,5)
            if (iExp == 1):   #first line
                ax.scatter(x2, y2, color=Colors[iExp], label='', marker='o', s=1)  # to align the xlabels
            for i in range(len(y2)):
                if (y2[i] < 0.01):
                    y2[i] = np.NaN
                    y2_Bid[i] = np.NaN
                    y2_Ask[i] = np.NaN
            ax.plot(x2, y2, color=Colors[iExp % 7], label=Exp)
            if (Consider_Bid_Ask):
                ax.plot(x2, y2_Bid, ':', color=Colors[iExp % 7] , linewidth=1 )
                ax.plot(x2, y2_Ask, ':',color= Colors[iExp % 7], linewidth=1 )
            Title =  Title0 +'\n' + mySymbol + ' OPT Price at Strike ' + str(Strike) + Right + ' for Various Exp Dates'
            ax.set(xlabel=' ', ylabel='Option, Price', title=Title)
            ax.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
            if (iExp == 1):   #first line
                plt.draw()
                Title = mySymbol + ' OPT Price at Strike ' + str(Strike) + Right + ' for Various Exp Dates'
                #
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
            plt.subplots_adjust(wspace=None, hspace=None)
            plt.legend(loc='upper left') #loc='best',
        # Next to plot the stock price
        ax2 = plt.subplot(412)
        y2 = stock
        x2 = list(map(str, df0['Date'].tolist()))
        ax2.plot(x2, y2, color=Colors[5], label='STK')
        ax2.set(xlabel=' ', ylabel='STK, Price', title='STK')
        ax2.plot(x2, list(np.array(y2) * 0.0 + Strike), color='red', label='Strike')
        ax2.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
        plt.draw()
        plt.setp(ax2.get_xticklabels(), rotation=45)
        cticks = ax2.get_xticklabels()
        ax2.set_xticklabels( convert_cticks(cticks) )
        plt.gca().yaxis.grid(True)
        plt.gca().xaxis.grid(True)
        plt.legend(loc='upper left')
        plt.subplots_adjust(wspace=None, hspace=None)
        # ax2.set_xlim(left=x0min, right=x0max )

# 2 STK Price
Title =  ' Balance with Short ' + mySymbol + ' at Strike ' +str(myStrikes[iStrike])+myRights[0]
ax3 = plt.subplot(413)

ax3.plot(x2, Balance, color='green', label='Balance')
ax3.set(xlabel=' ', ylabel='Balance', title=Title)
#
ax3.xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax3.get_xticklabels(), rotation=45)
cticks = ax3.get_xticklabels()
ax3.set_xticklabels(convert_cticks(cticks))
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)

ax3.plot(x2, stock, '--',color='purple', label='Balance if hold STK')
ax3.plot(x2, list(np.array(y2)*0.0 + myStrikes[iStrike]), color='red', label='Strike')
ax3.scatter(x2,ytrades1,color='blue', label='Expires')
ax3.scatter(x2,ytrades2,color='red', label='Trade / Rollover')
if(algo == 1):
    plt.text(0.5, 0.85, 'OPT Out of the Money (STK < Strike) ', size=12, color='blue',\
             transform=ax3.transAxes, ha = 'center')  #fontweight='bold',
    plt.text(0.5, 0.1, 'OPT In the Money  (Strike > STK) ' , size=12, color='red', \
             transform=ax3.transAxes,  ha = 'center')
else:
    plt.text(0.5, 0.1, 'OPT Out of the Money  (STK > Strike) ', size=12, color='blue',\
             transform=ax3.transAxes,   ha = 'center')
    plt.text(0.5, 0.85, 'OPT In the Money  (Strike > STK) ', size=12, color='red', \
             transform=ax3.transAxes,   ha = 'center')
plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None)

########################## BELOW MAKE PLOT OF CASH AND SHORT POSITION ########################

# 3 TRADE BALANCE

Title =  ' Cash and Short Positions for ' + mySymbol + ' with ' +str(myStrikes[iStrike])+myRights[0]
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

# 4 CASH and Short Positions
ax4.bar(x2, list(- np.array(yShort)),color='blue', label='Short Debt',width=[0.5]*len(x2))
ax4.bar(x2, list(np.array(yCash)- np.array(yShort)),color='red', label='Net', width=[0.7]*len(x2))
ax4.plot(x2, list(np.array(yCash)*0.0),color='black', linewidth=1)
#ax4.text(0.1, 0.9, 'text', size=15, color='purple',transform=ax4.transAxes())

plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None,)
#
if (savefig):
    dir = 'figs/'+mySymbol
    if(os.path.exists(dir) == False):
        cmd = 'mkdir '+dir
        os.mkdir(dir)
        #
    savefile = 'figsbb/' + mySymbol+'_'+str(myStrikes[iStrike])+ myRights[iRight]+'_'+str(x0min)+'-'+str(x0max)+'.png'
    plt.savefig(savefile,bbox_inches='tight')
    print(' plot file saved as ', savefile)
plt.show(block = False)
plt.show()

print('x2 x3==> ',x2)
print('y2 ycash==> ',yCash)
print('y2 yShort==> ',yShort)