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
from myFormulas import *

savefig   = True
Consider_Bid_Ask = True #False
Trading_cost = 0.0065
########################################################################
year_min = 2020
year_max = 2020

month_min, day_min    =  9, 1   # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max    =  10,30

algo        = 2         # 1 Sell covered call;
                        # 2 Sell put
StrikeType  = 2         # 1 Fixed Strike at Initial Selection based on Initial STK Price abd bound
                        # 2 Varying Strike with no limitation of Positive Rollover
                        # 3 Varying Strike but limit Rollover to Positive
                        # 4 Strike2 one direction change only P down
                        # 5 Strike2 one direction change only P up

VoltFac = 0.0           # Strike at distance of at least VoltFac * Volatility
ReadData   = False

mySymbol    = 'SPY'
strike_min = 310.
strike_max = 360.
bound = 0.0
#
# mySymbol    = 'LYFT'
# strike_min = 22.
# strike_max = 40.
#
mySymbol    = 'JETS'
strike_min = 15.
strike_max = 22.
bound = 0.0



##########################################################################
myRights = ['C','P']

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

temp = np.array(myStrikes0)
temp = temp[temp >= strike_min]           #########################
temp = temp[temp <= strike_max]           #########################
if(mySymbol == 'SPY'):
    for i in range(len(temp)):
        temp[i] = int(temp[i]/1)*1    # every $2 interval
    temp = list(set(temp))

myStrikes = pd.Series(temp)


# For SPY, limit to one expiration day per week, here "Mon" is used.
if(mySymbol == 'SPY'):
    temp = list()
    for day in myExpDates:
        temp_date = mystrlist2day(day)
        wday = WeekDays(temp_date)
        if(wday == 'Fri'):  # Friday only
            temp.append(day)
    myExpDates = temp.copy()

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

nStrikes   = len(myStrikes)
nExp       = len(myExpDates)
nRights    = len(myRights)
print('nExp, nStrikes, nRights =', nExp, nStrikes, nRights)
if( (nStrikes == 0 ) or (nExp == 0) or (nRights == 0)):
    exit(' There is no contract in the specified range. Check specifications')

dump_file = 'temp/'+mySymbol + '_' + str(x0min) +'_' + str(x0max)+'_Strike_'+str(strike_min)+'_'+str(strike_max)+'.pickle'

# # ################################################################################################################
if((os.path.exists(dump_file) == False) or (ReadData)):
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
                    if(y2[i] < 0.0 and y2[i] > -10.):
                        y2[i] = np.NaN      # for making scatter plot
                        y2_Bid[i] = np.NaN  # for making scatter plot
                        y2_Ask[i] = np.NaN  # for making scatter plot
                #print('iRight,iStrike,iExp',iRight,iStrike,iExp )
                data4D[iExp][iStrike][iRight][0] = x2
                data4D[iExp][iStrike][iRight][1] = y2
                data4D[iExp][iStrike][iRight][2] = y2_Bid
                data4D[iExp][iStrike][iRight][3] = y2_Ask
            x2 = list(map(str, x2))
    #
    # To remove entries where data for the whole time series is missing
    # data4D, myStrikes,myExpDates,myRights = get_clean_data4D(data4D,myStrikes,myExpDates,myRights)
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

stock = df0['UnderlyingPrice'].tolist()
x2 = df0['Date'].tolist()
x2 = list(map(str, x2))


###########################   SIMULATION STARTS HERE     #####################################

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
ytrades3 = [- 100.0 + np.NaN]*len(x2)    # to mark whether expired position
yCash    = [0.0]*len(x2)                 # cash from option trade
yShort   = [0.0]*len(x2)                 # short position debt

# dates1_beg = x2.copy()
# dates1_end = x2.copy()
# dates2_beg = x2.copy()
# dates2_end = x2.copy()
# iStrikes1 = [0.0]*len(x2)
# iStrikes2 =[0.0]*len(x2)


if (algo == 1):             # sell covered call
   iRight = 0
else:                       # sell put
    iRight = 1
    bound = - bound

bound0 = 0.1
iStrike0 = closest(myStrikes, stock[0], myRights[iRight], bound0)
iExp0    = 1

v0 = real_volatility(stock)

index = 0
iStrike = iStrike0
volatilities, bound = calc_bound(x2, index, iExp0+1, myExpDates,stock, myStrikes, data4D, iStrike, iRight, VoltFac, bound0)
bound = bound*(1.5-algo)*2
print('implied and real volatility, bound:', v0 , bound, volatilities)
print('-----------------------------------')

#
bound0 = bound
iStrike0 = closest(myStrikes, stock[0], myRights[iRight], bound0)

if(StrikeType==1):
    FixedStrike = True
else:
    FixedStrike = False

indStrikes = [0]*len(x2)
Balance = list()
index_nearest = 0  # find the initial contract
InsideContract =   [[ [ False for col in range(len(x2))] for col3 in range(nStrikes)] for row in range(nExp)]

for ii in range(len(x2)):
    print('')
    print(' -------------  ii = ', ii, ' --------------- on day ', x2[ii])
    Date = x2[ii]
    index = x2.index(Date)
    StkPrice = stock[index]
    StkPrice0 = StkPrice
    if(ii == 0):    # first day
        Balance.append(StkPrice)
        iStrike = iStrike0
        iStrike2 = iStrike0
        #
        index_nearest, iStrike = get_nearestNaN(data4D,myRights,myStrikes, myExpDates, \
                                                index_nearest, iStrike, iRight,index,0, False)
        index_nearest2 = index_nearest
        #
        OptPrice     = data4D[index_nearest][iStrike][iRight][1][index]
        OptPrice_Bid = data4D[index_nearest][iStrike][iRight][2][index]
        OptPrice_Ask = data4D[index_nearest][iStrike][iRight][3][index]
        Cash  = OptPrice_Bid - Trading_cost
        OptPrice0 = OptPrice_Ask
        Balance0 = StkPrice0
        ytrades2[ii] = myStrikes[iStrike]
        yCash[ii] = Cash
        yShort[ii] = OptPrice_Ask
        InsideContract[index_nearest][iStrike][ii]   = True
        InsideContract[index_nearest][iStrike2][ii]  = True
    else: # Day 2 and after
        InsideContract[index_nearest][iStrike][ii]   = InsideContract[index_nearest][iStrike][ii-1]
        InsideContract[index_nearest][iStrike2][ii]  = InsideContract[index_nearest][iStrike2][ii - 1]
        #
        OptPrice = data4D[index_nearest][iStrike][iRight][1][index]
        OptPrice_Bid = data4D[index_nearest][iStrike][iRight][2][index]
        OptPrice_Ask = data4D[index_nearest][iStrike][iRight][3][index]
        if(np.isnan(OptPrice)):
            OptPrice = OptPrice0
        elif (OptPrice < 0):
            OptPrice = OptPrice0
        try:    # this is an expiration date
            indexExp = myExpDates.index(Date)
            TradingDay = True
            if (Date < myExpDates[index_nearest]):
                TradingDay = False
        except:
            TradingDay = False
        if(TradingDay):
            if(StrikeType == 1):
                iStrike2 = iStrike0
            else:
                try:
                    iExp = index_nearest + 1
                    volatilities, bound = calc_bound(x2, index, iExp, myExpDates, stock, myStrikes, data4D, iStrike,
                                                     iRight, VoltFac, bound0)
                    bound = bound*(1.5-algo)*2
                except:
                    pass
                # if(index == 3):
                #     exit()
                iStrike2 = closest(myStrikes, StkPrice, myRights[iRight], bound)
            opt1 = data4D[index_nearest][iStrike][iRight][3][index]
            if(np.isnan(opt1)):
                opt1 = OptPrice0
            elif(OptPrice < 0):
                opt1 = OptPrice0
            if(( algo == 1) &  (StkPrice < myStrikes[iStrike])):
                opt1 = 0.0
            if(( algo == 2) &  (StkPrice > myStrikes[iStrike])):
                opt1 = 0.0
            index_nearest2, iStrike2 = get_nearestNaN(data4D, myRights, myStrikes, myExpDates, \
                                        index_nearest, iStrike2, iRight, index, 1, FixedStrike)
            opt2 = data4D[index_nearest2][iStrike2][iRight][2][index]
            if (StrikeType == 1):
                iStrike2 = iStrike0
            elif (StrikeType == 2):
                pass
            elif (StrikeType == 3):
                if(opt2 < opt1):
                    iStrike2 = iStrike
            elif (StrikeType == 4):
                    if (algo == 1):
                        iStrike2 = min([iStrike, iStrike2])
                    else:
                        iStrike2 = max([iStrike, iStrike2])
            elif (StrikeType == 5):
                if (algo == 1):
                    iStrike2 = max([iStrike, iStrike2])
                else:
                    iStrike2 = min([iStrike, iStrike2])
            opt2 = data4D[index_nearest2][iStrike2][iRight][2][index]
            if(opt1>0):
                ytrades1[index] = myStrikes[iStrike]  # Make a record
            else:
                ytrades3[index] = myStrikes[iStrike]
            ytrades2[index] = myStrikes[iStrike2]  # Make a record
            #
            InsideContract[index_nearest][iStrike][ii] = True
            InsideContract[index_nearest2][iStrike2][ii] = True
            # print('index_nearest,  iStrike,  ii, InsideContract[index_nearest][iStrike][ii]',\
            #       index_nearest, iStrike, ii, InsideContract[index_nearest][iStrike][ii])
            # print('index_nearest2, iStrike2, ii, InsideContract[index_nearest][iStrike][ii]', \
            #       index_nearest, iStrike, ii, InsideContract[index_nearest2][iStrike2][ii])
            #
            if (np.isnan(opt2)):
                print(' This is a strange case in which no rollover is available opt1 opt2 = ', opt1, opt2, \
                      'for day and Expiration date: ', Date, myExpDates[index_nearest2])
                exit('No contracts relevant to the quiry, exit in main program line ~ 280')
            else:
                dCash = opt2 - opt1 - Trading_cost
                Cash = Cash + dCash
                OptPrice = opt2
            #
            message1 = ' ****************** OLD ' + myRights[iRight] + ' CONTRACT EXPIRES/ROLL-OVER FOR EXPIRATION ' \
                    + str(myExpDates[index_nearest])   + ' at price ' + str(opt1) + ' with Strike ' \
                    + str(myStrikes[iStrike]) + ' on ' + str(Date)
            message2 = ' ****************** NEW ' + myRights[iRight] + ' CONTRACT INITIATED         FOR EXPIRATION ' \
                    + str(myExpDates[index_nearest2]) +  ' at price ' + str(opt2) + ' with Strike '  \
                    + str(myStrikes[iStrike2]) + ' on ' + str(Date)
            print(message1)
            print(message2)
            print(     ' ------------------ Median bound and Implied Volatilities   ', bound, volatilities)
            print('')
            iStrike = iStrike2
        else:  #trading finished
            pass
        if(algo == 1):
            Balance.append(StkPrice - OptPrice + Cash)  # covered call
        else:
            Balance.append(Balance0 - OptPrice + Cash)  # sell put
        yCash[ii] = Cash
        yShort[ii] = OptPrice
    print(' On day ', x2[index], ' Balance Consists of Initial Balance =', Balance0, \
          '.   Cash =', Cash, '.  ', myRights[iRight]+' Option Short Position = -', OptPrice, '.  Option Strikes at ', myStrikes[iStrike], \
          ' Expires on ', myExpDates[index_nearest])
    print('                  TOTAL BALANCE = ', Balance0 - OptPrice + Cash, '.  STK price is ', StkPrice, ' Bound', bound)
    OptPrice0 = OptPrice
    indStrikes[ii] = iStrike
    index_nearest = index_nearest2
    ii = ii +1
    print('ii,index_nearest', ii,index_nearest)
#exit()
print('Summary of nd Balance:', Balance)
print('x2 x2==> ',x2)

# print('dates1_beg', dates1_beg)
# print('dates1_end', dates1_end)
# print('dates2_beg', dates2_beg)
# print('dates2_end', dates2_end)
# print('istrikes1', iStrikes1)
# print('istrikes2', iStrikes2)
#exit()


bound = bound0
##########################  PLOTTING STARTS HERE  #####################################################

# 1 STK Price

n_tickdays = get_ntickdays(x2, 20, 5)
plt.figure()
if (algo == 1):
    Title0 = ' COVERED CALL OF '+ mySymbol
    color = 'red'
else:
    Title0 = ' SHORT PUT OF '+ mySymbol
    color = 'blue'

Title0 = Title0 + ' (STRIKE TYPE ' + str(StrikeType) + ')\n'

fig, ax = plt.subplots(5, 1, figsize=(12, 16), sharex=True)
#fig, (ax, ax2, ax3, ax[iP]) = plt.subplots(4, 1, figsize=(10, 12), sharex=True)

fig.suptitle(Title0, fontsize=16, fontweight="bold", color=color)

iP = 1 -1
ax[iP] = plt.subplot(511)
# 1 STK Price
#Title =  Title0 + ' ' + mySymbol + ' STK Price'
#Title =   Title0 + mySymbol + ' STK Price'
Title =   mySymbol + ' STK Price and Initial Strike Price at ' + myRights[iRight]+ str(myStrikes[iStrike0])

y2 = stock
x2 = list(map(str, df0['Date'].tolist()))
ax[iP].plot(x2, y2, color=Colors[5], label='STK',  linewidth=3.0)
ax[iP].set(xlabel=' ', ylabel='STK, Price', title=Title)
ax[iP].plot(x2, list(np.array(y2) * 0.0 + myStrikes[iStrike0]), color='red', label='Initial '+myRights[iRight] \
                                                         +'\nStrike '+str(myStrikes[iStrike0]))
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax[iP].get_xticklabels(), rotation=45)
cticks = ax[iP].get_xticklabels()
for i in range(len(cticks)):
    cticks[i].set_text('')

ax[iP].set_xticklabels( convert_cticks(cticks) )
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)
ax[iP].tick_params(labelright=True)
plt.legend(loc='upper left')
plt.subplots_adjust(wspace=None, hspace=None)
# ax[iP].set_xlim(left=x0min, right=x0max )

# 2 OPT Orice at initial strike
#
iP = 2-1
ax[iP] = plt.subplot(512)

data4E = data4D.copy()
#for iRight in range(len(myRights)):

# for iStrike in range(len(myStrikes)):
#     for iExp in range(len(myExpDates)):
#         for i in range(len(x2)):
#             #print('i, iExp',i,iExp,iStrike,InsideContract[iExp][iStrike][i])
#             if(InsideContract[iExp][iStrike][i]):
#                 pass
#             else:
#                 data4E[iExp][iStrike][iRight][1][i] = np.NaN
#                 data4E[iExp][iStrike][iRight][2][i] = np.NaN
#                 data4E[iExp][iStrike][iRight][3][i] = np.NaN
#    exit()
ymax = -9999.0
ymin = 9999.0
for iStrike in range(len(myStrikes)):
    for iExp in range(len(myExpDates)):
        ytemp = data4E[iExp][iStrike][iRight][1].copy()
        ymax = max([ymax,max(ytemp)])
        ymin = 0.0


Strike = myStrikes[iStrike0]
Right  = myRights[iRight]
Title = str(Strike) + Right + ' OPT Price for Various Expiration Dates'
ax[iP].set(xlabel=' ', ylabel='Option, Price', title=Title)
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))

for iRight0 in range(1): ##len(myRights)):
    Right = myRights[iRight]
    if(StrikeType < 10):   # copied code
        n1 = iStrike0
        n2 = n1+1
    else:
        n1 = 0
        n2 = len(myStrikes)
    ii = 0
    for iStrike in range(n1,n2): # len(myStrikes)):
        Strike = myStrikes[iStrike]
        #print('iStrikes,Strike',iStrike,Strike)
        #fig.suptitle(Title0, fontsize=20, fontweight='bold', y=.995)
        for iExp in range(len(myExpDates)):
            Exp    = myExpDates[iExp]
            y2     = data4E[iExp][iStrike][iRight][1]
            y2_Ask = data4E[iExp][iStrike][iRight][2]
            y2_Bid = data4E[iExp][iStrike][iRight][3]
            for i in range(len(y2)):
                i1 = min([i+1,len(x2)-1])
                if ((y2[i] <= 0.0) or ((y2[i] == 0.) and (y2[i1] > 0.)) ):
                    y2[i] = np.NaN
                    y2_Bid[i] = np.NaN
                    y2_Ask[i] = np.NaN
            if (ii == 0):   #first line
                ax[iP].scatter(x2, y2, color=Colors[iExp], label='', marker='o', s=1)  # to align the xlabels
                plt.draw()
                Title = mySymbol + ' OPT '+ Right + ' for Various Exp Dates and Strikes'
                if(StrikeType == 1):
                    Title = mySymbol + ' OPT ' + Right + ' for  Strike' +str(Strike)
                #
                plt.setp(ax[iP].get_xticklabels(), rotation=45)
                #
                cticks = ax[iP].get_xticklabels()
                for i in range(len(cticks)):
                    cticks[i].set_text('')
                ax[iP].set_xticklabels(convert_cticks(cticks))
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
#                ax[iP].set_ylim(bottom=0, top=ymax)  ############
                # ax[iP].set_xlim(left=x0min, right=x0max)  # 2+dtop)
                ax[iP].set_ybound(0.0, ymax)
                # ax[iP].set_autoscale_on(False)
                plt.draw()
                #
            #print('ii,iStrike,iExp,Strike ',ii,iStrike,iExp,Strike)
            # if(np.count_nonzero(~np.isnan(y2))  == 0):
            #     pass
            # else:
            #     label = Right+str(Strike)+'\nE'+Exp[4:]
            ax[iP].plot(x2, y2, color=Colors[iExp % 7], label=Exp[4:])
            #     #yl = Strike
            #     ylabv = next(item for item in y2 if (item is not np.NaN))
            #     xlab = next(k for k, v in enumerate(y2) if (v == ylabv))
            #     ylab = ylabv #np.nanmax(y2)
            #     ax[iP].annotate(label, (xlab,ylab), textcoords='data', color='black', size=10, ha='center', va = 'bottom')
            #     #
            #     y2temp = y2[::-1]
            #     #print('y2    ', type(y2),y2)
            #     #print('y2temp', type(y2temp),y2temp)
            #     ylabv2 = next(item for item in y2temp if (item is not np.NaN))
            #     xlab2 = next(k for k, v in enumerate(y2temp) if (v == ylabv2))
            #     xlab2 = len(x2) - xlab2 -1
            #     ax[iP].scatter(xlab, ylabv, color='red', label='', s=50, marker=4)  # '*')
            #     if(ytrades1[xlab2] > 0.0):
            #         ax[iP].scatter(xlab2, ylabv2, color='blue', label='', s=50, marker=5)  # '*')
            #     else:
            #          ax[iP].scatter(xlab2, ylabv2, label='', s=50,  facecolors = 'none', edgecolors = 'blue')
            #     #
            if (Consider_Bid_Ask):
                ax[iP].plot(x2, y2_Bid, ':', color=Colors[iExp % 7], linewidth=1)
                ax[iP].plot(x2, y2_Ask, ':', color=Colors[iExp % 7], linewidth=1)
            ii = ii + 1
    #
    ax[iP].tick_params(labelright=True)
    plt.subplots_adjust(wspace=None, hspace=None)
    plt.legend(loc='best', ncol=4 ) #'upper left') #loc='best',
        # Next to plot the stock price



#3 Trades Balances
IP = 3 -1
Title = ' Trades and Balance with Short ' + mySymbol + ' ' + myRights[iRight] + ' at Strike with ' \
         +str(bound) + ' Offset from STK Price. Volatility Factor ' + str(VoltFac)
ax[iP] = plt.subplot(513)

ax[iP].plot(x2, Balance, color='green', label='Balance',  linewidth=3.0)
ax[iP].set(xlabel=' ', ylabel='Balance', title=Title)
#
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax[iP].get_xticklabels(), rotation=45)
cticks = ax[iP].get_xticklabels()
for i in range(len(cticks)):
    cticks[i].set_text('')

ax[iP].set_xticklabels(convert_cticks(cticks))
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)
#
ax[iP].plot(x2, stock, '--',color='purple', label='STK',  linewidth=2.0)
#
temp = set(indStrikes)
y2 = range(len(x2))
for i in temp:
    ax[iP].plot(x2, list(np.array(y2)*0.0 + myStrikes[i]), color='red', label='',  linewidth=0.5)

if(StrikeType > 1):
    ax[iP].scatter(x2,ytrades2, color='red',  label='Sell',s=50)
else:
    ax[iP].scatter(x2, ytrades2,facecolors = 'none', color='red', label='Sell', s=50)
ax[iP].scatter(x2,ytrades3, s=50,facecolors = 'none', edgecolors = 'red')
ax[iP].scatter(x2,ytrades1, color='blue', label='Buy', s=50)

# arrowprops=dict(arrowstyle='<-', color='blue', linewidth=10, mutation_scale=150)
# an = ax.annotate('Blah', xy=(1, 1), xytext=(-1.5, -1.5), xycoords='data',
#                  textcoords='data', arrowprops=arrowprops)
for i in range(len(x2)):
    yj2 = ytrades2[i]
    yj3 = ytrades3[i]
    yj1 = ytrades1[i]
    xj = i
    if ((ytrades1[i] > -10.0) and (ytrades2[i] > -10.0) ):
        plt.annotate('',xytext = (xj, yj1),xy = (xj, yj2), va='center', ha='center', \
                 arrowprops = {'arrowstyle': "->", 'lw': 1, 'color': 'blue','shrinkB' : 6.0 ,'shrinkB' : 6.0, \
                               'mutation_scale':25})
    if ((ytrades3[i] > -10.0) and (ytrades2[i] > -10.0)):
        plt.annotate('', xytext=(xj, yj3), xy=(xj, yj2), va='center', ha='center', \
                         arrowprops={'arrowstyle': "->", 'lw': 1, 'color': 'red', 'shrinkB': 6.0, 'shrinkB': 6.0, \
                                     'mutation_scale':25})
    if(ytrades2[i] > -10.0):
        for k in range(i+1,len(x2)):
            if((ytrades1[k] > -10.0 ) or (ytrades3[k] > -10.0 )):
                xj4 = k
                break
        plt.annotate('', xytext=(xj, yj2), xy=(xj4, yj2), va='center', ha='center', \
                     arrowprops={'arrowstyle': "->", 'lw': 2, 'color': 'black', 'shrinkB': 6.0, 'shrinkB': 6.0, \
                                 'mutation_scale':25})

if(algo == 1):
    plt.text(0.5, 0.85, 'Out of the Money (STK < Strike) ', size=12, color='blue',\
             transform=ax[iP].transAxes, ha = 'center')  #fontweight='bold',
    plt.text(0.5, 0.1, 'In the Money  (Strike > STK) ' , size=12, color='red', \
             transform=ax[iP].transAxes,  ha = 'center')
else:
    plt.text(0.5, 0.1, 'Out of the Money  (STK > Strike) ', size=12, color='blue',\
             transform=ax[iP].transAxes,   ha = 'center')
    plt.text(0.5, 0.85, 'In the Money  (Strike > STK) ', size=12, color='red', \
             transform=ax[iP].transAxes,   ha = 'center')
ax[iP].tick_params(labelright=True)
plt.legend(loc='upper left', ncol = 2) #fancybox=True, framealpha=1, shadow=False, borderpad=1)
plt
plt.subplots_adjust(wspace=None, hspace=None)

########################## BELOW MAKE PLOT OF CASH AND SHORT POSITION ########################

# 4 Execution OPT Price
#
iP = 4-1
ax[iP] = plt.subplot(514)
Strike = myStrikes[iStrike0]
Right  = myRights[iRight]
Title =  ' Execution '+  Right +  ' Contract Price at Various Strike Price and Expiration Dates'

data4E = data4D.copy()
#for iRight in range(len(myRights)):

for iStrike in range(len(myStrikes)):
    for iExp in range(len(myExpDates)):
        for i in range(len(x2)):
            #print('i, iExp',i,iExp,iStrike,InsideContract[iExp][iStrike][i])
            if(InsideContract[iExp][iStrike][i]):
                pass
            else:
                data4E[iExp][iStrike][iRight][1][i] = np.NaN
                data4E[iExp][iStrike][iRight][2][i] = np.NaN
                data4E[iExp][iStrike][iRight][3][i] = np.NaN
#    exit()
ymax = -9999.0
ymin = 9999.0
for iStrike in range(len(myStrikes)):
    for iExp in range(len(myExpDates)):
        ytemp = data4E[iExp][iStrike][iRight][1].copy()
        ymax = max([ymax,max(ytemp)])
        ymin = 0.0


ax[iP].set(xlabel=' ', ylabel='Option, Price', title=Title)
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))

for iRight0 in range(1): #len(myRights)):
    Right = myRights[iRight]
    if(StrikeType == 1):
        n1 = iStrike0
        n2 = n1+1
    else:
        n1 = 0
        n2 = len(myStrikes)
    ii = 0
    for iStrike in range(n1,n2): # len(myStrikes)):
        Strike = myStrikes[iStrike]
        #print('iStrikes,Strike',iStrike,Strike)
        #fig.suptitle(Title0, fontsize=20, fontweight='bold', y=.995)
        for iExp in range(len(myExpDates)):
            Exp    = myExpDates[iExp]
            y2     = data4E[iExp][iStrike][iRight][1]
            y2_Ask = data4E[iExp][iStrike][iRight][2]
            y2_Bid = data4E[iExp][iStrike][iRight][3]
            for i in range(len(y2)):
                if (y2[i] < -10.0):
                    y2[i] = np.NaN
                    y2_Bid[i] = np.NaN
                    y2_Ask[i] = np.NaN
            if (ii == 0):   #first line
                ax[iP].scatter(x2, y2, color=Colors[iExp], label='', marker='o', s=1)  # to align the xlabels
                plt.draw()
                Title = mySymbol + ' OPT '+ Right + ' for Various Exp Dates and Strikes'
                if(StrikeType == 1):
                    Title = mySymbol + ' OPT ' + Right + ' for  Strike' +str(Strike)
                #
                plt.setp(ax[iP].get_xticklabels(), rotation=45)
                #
                cticks = ax[iP].get_xticklabels()
                for i in range(len(cticks)):
                    cticks[i].set_text('')
                ax[iP].set_xticklabels(convert_cticks(cticks))
                plt.gca().yaxis.grid(True)
                plt.gca().xaxis.grid(True)
#                ax[iP].set_ylim(bottom=0, top=ymax)  ############
                # ax[iP].set_xlim(left=x0min, right=x0max)  # 2+dtop)
                ax[iP].set_ybound(0.0, ymax)
                # ax[iP].set_autoscale_on(False)
                plt.draw()
                #
            #print('ii,iStrike,iExp,Strike ',ii,iStrike,iExp,Strike)
            if(np.count_nonzero(~np.isnan(y2))  == 0):
                pass
            else:
                label = Right+str(Strike)+'\nE'+Exp[4:]
                ax[iP].plot(x2, y2, color=Colors[iExp % 7], label='', linewidth= 2.0)
                #yl = Strike
                ylabv = next(item for item in y2 if (item is not np.NaN))
                xlab = next(k for k, v in enumerate(y2) if (v == ylabv))
                ylab = ylabv #np.nanmax(y2)
                ax[iP].annotate(label, (xlab,ylab), textcoords='data', color='black', size=10, ha='center', va = 'bottom')
                #
                y2temp = y2[::-1]
                #print('y2    ', type(y2),y2)
                #print('y2temp', type(y2temp),y2temp)
                ylabv2 = next(item for item in y2temp if (item is not np.NaN))
                xlab2 = next(k for k, v in enumerate(y2temp) if (v == ylabv2))
                xlab2 = len(x2) - xlab2 -1
                ax[iP].scatter(xlab, ylabv, color='red', label='', s=50 )  # '*')
                if(ytrades1[xlab2] > 0.0):
                    ax[iP].scatter(xlab2, ylabv2, color='blue', label='', s=50 )  # '*')
                else:
                     ax[iP].scatter(xlab2, ylabv2, label='', s=50,  facecolors = 'none', edgecolors = 'blue')
                #
                if (Consider_Bid_Ask):
                    ax[iP].plot(x2, y2_Bid, ':', color=Colors[iExp % 7], linewidth=1)
                    ax[iP].plot(x2, y2_Ask, ':', color=Colors[iExp % 7], linewidth=1)
            ii = ii + 1
    #
    plt.subplots_adjust(wspace=None, hspace=None)
    ax[iP].tick_params(labelright=True)
    plt.legend(loc='upper left') #loc='best',
        # Next to plot the stock price


# 5 CASH and Short Positions
iP = 5-1
Title =  ' Cash and Short Positions for ' + mySymbol + ' ' + myRights[iRight] + ' Contract'
ax[iP] = plt.subplot(515)

#ax[iP].bar(x2, yCash, color='green', label='Cash', width=[0.3]*len(x2))
ax[iP].bar(x2, yCash, label='', width=[0.001]*len(x2))
ax[iP].set(xlabel='Date', ylabel='Cash and Short Debt', title=Title)
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))
plt.draw()
plt.setp(ax[iP].get_xticklabels(), rotation=45)
cticks = ax[iP].get_xticklabels()
ax[iP].set_xticklabels(convert_cticks(cticks))
plt.gca().yaxis.grid(True)
plt.gca().xaxis.grid(True)

width = 0.3
deltax = width
xx2 = np.array(list(range(len(x2)))) - deltax

#print(type(xx2)),xx2
ax[iP].bar(xx2-deltax, list(-np.array(yShort)),color='blue',   label='Short Debt',width=[width]*len(x2))
ax[iP].bar(xx2, yCash,                         color='green',    label='Cash', width=[width]*len(x2))
ax[iP].bar(xx2+deltax, list(np.array(yCash)- np.array(yShort)),color='red', label='Net', width=[width]*len(x2))

ax[iP].plot(xx2, list(np.array(yCash)*0.0),color='black', linewidth=1)
#ax[iP].text(0.1, 0.9, 'text', size=15, color='purple',transform=ax[iP].transAxes())
ax[iP].tick_params(labelright=True)
plt.legend(loc='upper left', markerscale = deltax)
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
plt.show(block = False)

print('x2 x3==> ',x2)
print('y2 ycash==> ',yCash)
print('y2 yShort==> ',yShort)