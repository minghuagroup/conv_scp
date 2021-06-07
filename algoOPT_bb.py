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
from mybb import *
import warnings

warnings.filterwarnings("ignore")

savefig          = True
Consider_Bid_Ask = True  
Trading_cost     = 0.0065
########################################################################
year_min = 2020
year_max = 2020

month_min, day_min    =  9, 1   # TIME WINDOW  m%day, eg. 701, 1121
month_max, day_max    =  10,30 #10,30

algo        = 2         # 1 Sell covered call;
                        # 2 Sell put
                        
StrikeType  = 1         # 1 Fixed Strike at Initial Selection based on Initial STK Price abd bound
                        # 2 Varying Strike with no limitation of Positive Rollover
                        # 3 Varying Strike but limit Rollover to Positive
                        # 4 Strike2 one direction change only P down
                        # 5 Strike2 one direction change only P up

VoltFac     = 0.5       # Strike at distance of at least VoltFac * Volatility
ReadData     = False    # Whether to extract new data4D pickle dump data file

# mySymbol    = 'SPY'
# mySymbol    = 'LYFT'
mySymbol    = 'JETS'
# mySymbol    = 'BABA'

##########################################################################

# get bb OPT data and limit size

# bb_extract(mySymbol)                        # From bb database to extract file
# myStrikes  = bb_get_Strikes(mySymbol)      # From bb database
# myExpDates = bb_get_Exps(mySymbol)         # From bb database

date_min = month_min*100 + day_min
date_max = month_max*100 + day_max
x0min = 10000 * year_min + date_min
x0max = 10000 * year_max + date_max
x0minExp = x0min  # Expiration date range
x0maxExp = mydaylist2int( myintlist2day(x0max) + timedelta(5) )  #Contract with expiration 5 days after time window

# get data and limit dates
filein = 'data3/' + mySymbol + '_bb.csv'
df  = pd.read_csv(filein, header=0)                        # read data, big data file
df  = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]    # limit dates

# get stock data
df0 = df.copy()
df0.drop_duplicates(subset="Date",keep="first", inplace=True)   # for underlying stock price
df0.reset_index(drop=True, inplace = True)

myStrikes = list(set(df['Strike']))
print('dfE ', df['Expiration'])
myExpDates = list(set(df['Expiration']))
myExpDates.sort()
print('myE ', type(myExpDates), type(list(myExpDates)[0]), myExpDates)

x0minExp = datetime.strptime(str(x0minExp),"%Y%m%d")
x0maxExp = datetime.strptime(str(x0maxExp),"%Y%m%d")
myExpDates = [x for x in myExpDates if ( (datetime.strptime(x,"%m/%d/%Y") >= x0minExp) \
                                         and (datetime.strptime(x,"%m/%d/%Y")<= x0maxExp) )]

# For SPY, limit to one expiration day per week, here "Mon" is used.

myExpDates.sort()
if(mySymbol == 'SPY'):
    myExpDates = [x for x in myExpDates if (WeekDays(mystrlist2day(x)) == 'Fri')]
    print('myE2', myExpDates)

# get Strike range
strike_min = df['UnderlyingPrice'].min() * (1.0 - 1 / np.sqrt(253) )  # use volatility 200%
strike_max = df['UnderlyingPrice'].max() * (1.0 + 1 / np.sqrt(253) )
myStrikes = [item for item in myStrikes if ((item >= strike_min) and (item <= strike_max))]

print('myStrikes1',myStrikes)

# limit strikes
print('strike_min,strike_max,myStrikes', strike_min,strike_max,myStrikes)
if(strike_min > 500):
    myStrikes = [ (x//10)*10 for x in myStrikes]
elif(strike_min > 200):
    myStrikes = [ (x//5)*5   for x in myStrikes]
elif (strike_min > 100):
    myStrikes = [ (x//2)*2   for x in myStrikes]
elif (strike_min > 50):
    myStrikes = [ int(x)     for x in myStrikes]

myStrikes = list(set(myStrikes))
myStrikes.sort()


if(algo == 1):
    myRights = ['C']
else:
    myRights = ['P']

# Finished setting  myRights, myStrikes, myExpDates
nRights    = len(myRights)
nStrikes   = len(myStrikes)
nExp       = len(myExpDates)
print('nRights, nStrikes, nExp =',nRights,'(', myRights[0], ') ', nStrikes, nExp )
if( (nStrikes == 0 ) or (nExp == 0) or (nRights == 0)):
    exit(' There is no contract in the specified range. Check specifications')

print('myRights   = ', myRights)
print('myStrikes  = ', myStrikes)
print('myExpDates = ', myExpDates)
#exit()
dump_file = 'temp/'+mySymbol + '_'+myRights[0]+'_' + str(x0min) +'_' + str(x0max)+'_Strike_'+str(strike_min)+'_'+str(strike_max)+'.pickle'

#################################################################################################################

# check if data4D needs to be read.

if((os.path.exists(dump_file) == False) or (ReadData)):
    # filein = 'data3/' + mySymbol + '_bb.csv'
    # df  = pd.read_csv(filein, header=0)                        # read data
    # #
    # df  = df[(df['Date'] >= x0min) & (df['Date'] <= x0max) ]
    # df0 = df.copy()
    # df0.drop_duplicates(subset="Date",keep="first", inplace=True)   # for underlying stock price
    # df0.reset_index(drop=True, inplace = True)
    #
    data4D = [[ [ [list() for col in range(4)] for col2 in range(nRights )] for col3 in range(nStrikes)] for row in range(nExp)]
    #
    for iRight in range(len(myRights)):
        Right = myRights[iRight]
        for iStrike in range(len(myStrikes)):
            Strike = myStrikes[iStrike]
            print('Extracting Strike',  iStrike, Strike, Right )
            for iExp in range(len(myExpDates)):
                Exp = myExpDates[iExp] 
                df2 = bb_read_data(df, gRight(Right), Strike, gExp(Exp))
                nn  = df2.shape[0]
                if (df.empty):
                    break
                df4 = df0.copy()           # use stock price dataframe as dummy to replace values for plotting purpose
                df4['Last'] = -999.
                df4['Ask'] = -999.
                df4['Bid'] = -999.
                #
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
        #
        pickle.dump( [myRights, myStrikes, myExpDates, df0, data4D], file)
        #            ----------------------------------------------
else:
    with open(dump_file, 'rb') as file2:
        #
        temp = pickle.load(file2)
        #
        myRights   = temp[0]
        myStrikes0 = temp[1]
        myExpDates = temp[2]
        df0        = temp[3]
        data4D     = temp[4]

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

# temp arrays for latter plotting
ytrades1 = [- 100.0 + np.NaN]*len(x2)    # to mark whether it is an expiration/new initiation
ytrades2 = [- 100.0 + np.NaN]*len(x2)    # to mark whether it is a roll-over trade
ytrades3 = [- 100.0 + np.NaN]*len(x2)    # to mark whether expired position
yCash    = [0.0]*len(x2)                 # cash from option trade
yShort   = [0.0]*len(x2)                 # short position debt
 
 
# if (algo == 1):             # sell covered call
#     iRight = 0
# else:                       # sell put
#     iRight = 1

iRight = 0
# determine the initial strike price by setting the bound from the STK price
bound0   = 0.0
iExp0    = 1  # next expiration date
index    = 0
iStrike0 = closest(myStrikes, stock[0], myRights[iRight], bound0)
#
volatilities, bound = calc_bound(x2, index, iExp0, myExpDates,stock, myStrikes, data4D, iStrike0, iRight, VoltFac, bound0)
#
bound = bound*(1.5-algo)*2  # for put, add a negative sign
bound0   = bound

v0 = real_volatility(stock)

print('Real and implied volatility, volatility ranges:', v0 , bound, volatilities)
print('-----------------------------------------------')

iStrike0 = closest(myStrikes, stock[0], myRights[iRight], bound0)

if(StrikeType==1):
    FixedStrike = True
else:
    FixedStrike = False

# bookkeeping
indStrikes = [0]*len(x2)
indExps    = [0]*len(x2)
VBounds    = [0]*len(x2)
VBounds[0] = bound0

Balance    = list()
iExp_nearest = 0  # find the initial contract expiration index
#
# array to record whether a contract is traded
InsideContract =   [[ [ False for col in range(len(x2))] for col3 in range(nStrikes)] for row in range(nExp)]

# Trading sequence begins
for ii in range(len(x2)):
    print('')
    print(' -------------  ii = ', ii, ' --------------- on day ', x2[ii])
    Date = x2[ii]
    index = x2.index(Date)
    StkPrice = stock[index]
    StkPrice0 = StkPrice
    if(ii == 0):    # first day
        #
        iStrike  = iStrike0
        iStrike2 = iStrike0
        #
        iExp_nearest, iStrike = get_nearestNaN(data4D,myRights,myStrikes, myExpDates, \
                                        iExp_nearest, iStrike, iRight,index,0, False)   # 0 is to search for Exp dates
        #                                                                                # False to set FixedStrike 
        iExp_nearest2 = iExp_nearest
        #
        OptPrice     = data4D[iExp_nearest][iStrike][iRight][1][index]
        OptPrice_Bid = data4D[iExp_nearest][iStrike][iRight][2][index]
        OptPrice_Ask = data4D[iExp_nearest][iStrike][iRight][3][index]
        #
        Cash         = OptPrice_Bid - Trading_cost
        ytrades2[ii] = myStrikes[iStrike]
        yCash[ii]    = Cash
        yShort[ii]   = OptPrice_Ask
        InsideContract[iExp_nearest][iStrike][ii]   = True 
        # bookkeeping
        OptPrice0 = OptPrice_Ask
        Balance0 = StkPrice0
        Balance.append(Balance0)
        #
    else: # Day 2 and after
        #
        # Continue the previous contracts until reset on trading days
        InsideContract[iExp_nearest][iStrike][ii]   = InsideContract[iExp_nearest][iStrike][ii-1]
        InsideContract[iExp_nearest][iStrike2][ii]  = InsideContract[iExp_nearest][iStrike2][ii - 1]
        #
        OptPrice     = data4D[iExp_nearest][iStrike][iRight][1][index]
        OptPrice_Bid = data4D[iExp_nearest][iStrike][iRight][2][index]
        OptPrice_Ask = data4D[iExp_nearest][iStrike][iRight][3][index]
        if((np.isnan(OptPrice)) or (OptPrice < 0)):  # strange data, set to previous day price
            OptPrice     = OptPrice0
            OptPrice_Bid = OptPrice0
            OptPrice_Ask = OptPrice0
        #
        try:     # Check if this date is an expiration date
            indexExp = myExpDates.index(x2[ii])
            TradingDay = True
            if (x2[ii] < myExpDates[iExp_nearest]):
                TradingDay = False
        except:
            TradingDay = False
        #
        if(TradingDay):  # This is a trading day
            #
            # Current short position
            opt1 = OptPrice_Ask
            if ( ((algo == 1) & (StkPrice < myStrikes[iStrike])) or ((algo == 2) & (StkPrice > myStrikes[iStrike])) ):
                opt1 = 0.0           # option expires
            #
            # Find the new strike price
            if(StrikeType == 1):    # fixed strike price
                iStrike2 = iStrike0
            else:
                try:    # find the bound from volatility
                    iExp = iExp_nearest + 1
                    volatilities, bound = calc_bound(x2, index, iExp, myExpDates, stock, myStrikes, data4D, iStrike,
                                                     iRight, VoltFac, VBounds[index-1])
                    bound = bound*(1.5-algo)*2
                except:
                    pass
                iStrike2 = closest(myStrikes, StkPrice, myRights[iRight], bound)
            #
            # Find the new expiration date
            iExp_nearest2, iStrike2 = get_nearestNaN(data4D, myRights, myStrikes, myExpDates, \
                                        iExp_nearest, iStrike2, iRight, index, 1, FixedStrike)  # 1 to search dates

            # New contact price
            opt2 = data4D[iExp_nearest2][iStrike2][iRight][2][index]
            #
            # Set strike for difference algorithms
            if ((StrikeType == 3) and (opt2 < opt1)):  #  no roll over if no surplus cash
                    iStrike2 = iStrike
            elif (StrikeType == 4):                    # Strike movement in one direction only
                    if (algo == 1):
                        iStrike2 = min([iStrike, iStrike2])
                    else:
                        iStrike2 = max([iStrike, iStrike2])
            elif (StrikeType == 5):                    # Strike in another direction
                if (algo == 1):
                    iStrike2 = max([iStrike, iStrike2])
                else:
                    iStrike2 = min([iStrike, iStrike2])
            # Update new contact
            opt2 = data4D[iExp_nearest2][iStrike2][iRight][2][index]
            # Bookkeeping for plotting
            if(opt1>0):
                ytrades1[index] = myStrikes[iStrike]  # Make a record of contract to rollover
            else:
                ytrades3[index] = myStrikes[iStrike]  # Make a record of expired contract
            ytrades2[index] = myStrikes[iStrike2]     # Make a record if new contract
            #
            InsideContract[iExp_nearest][iStrike][ii]   = True
            InsideContract[iExp_nearest2][iStrike2][ii] = True
            #
            if (np.isnan(opt2)):
                print(' This is a strange case in which no rollover is available opt1 opt2 = ', opt1, opt2, \
                      'for day and Expiration date: ', Date, myExpDates[iExp_nearest2]), ' even after get_nearestNaN'
                exit('No contracts relevant to the query, exit in main program line ~ 280')
            else:
                dCash = opt2 - opt1 - Trading_cost
                Cash = Cash + dCash
                OptPrice = opt2
            #
            message1 = ' ****************** OLD ' + myRights[iRight] + ' CONTRACT EXPIRES/ROLL-OVER FOR EXPIRATION ' \
                    + str(myExpDates[iExp_nearest])   + ' at price ' + str(opt1) + ' with Strike ' \
                    + str(myStrikes[iStrike]) + ' on ' + str(Date)
            message2 = ' ****************** NEW ' + myRights[iRight] + ' CONTRACT INITIATED         FOR EXPIRATION ' \
                    + str(myExpDates[iExp_nearest2]) +  ' at price ' + str(opt2) + ' with Strike '  \
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
    # Day finished
    print(' On day ', x2[index], ' Balance Consists of Initial Balance =', Balance0, \
          '.   Cash =', Cash, '.  ', myRights[iRight]+' Option Short Position = -', OptPrice, '.  Option Strikes at ', myStrikes[iStrike], \
          ' Expires on ', myExpDates[iExp_nearest])
    print('                  TOTAL BALANCE = ', Balance0 - OptPrice + Cash, '.  STK price is ', StkPrice, ' Bound', bound)
    OptPrice0      = OptPrice
    iExp_nearest   = iExp_nearest2
    indStrikes[ii] = iStrike
    indExps[ii]    = iExp_nearest
    VBounds[ii]    = bound
    ii = ii +1
    # print('ii,iExp_nearest', ii,iExp_nearest)
#exit()
print('Summary of nd Balance:', Balance)
print('x2 x2==> ',x2)

bound = VBounds[0]
##########################  PLOTTING STARTS HERE  #####################################################


Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

plt.figure()
fig, ax = plt.subplots(5, 1, figsize=(12, 16), sharex=True)
if (algo == 1):
    Title0 = ' COVERED CALL OF '+ mySymbol
    color = 'red'
else:
    Title0 = ' SHORT PUT OF '+ mySymbol
    color = 'blue'

Title0 = Title0 + ' (STRIKE TYPE ' + str(StrikeType) + ')\n'
fig.suptitle(Title0, fontsize=16, fontweight="bold", color=color)
n_tickdays = get_ntickdays(x2, 20, 5)   # every 5 days, but maximum tick number is 20

# 1 STK Price
# ---------------------------------
iP = 1 -1
ax[iP] = plt.subplot(511)
Title =   mySymbol + ' STK Price and Initial Strike Price at ' + myRights[iRight]+ str(myStrikes[iStrike0])

y2 = stock
x2 = list(map(str, df0['Date'].tolist()))   # x2 converted back to list for plotting 
            
ax[iP].plot(x2, y2, color=Colors[5], label='STK',  linewidth=3.0)
ax[iP].set(xlabel=' ', ylabel='STK, Price', title=Title)
# flat line of initial strike
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
# ---------------------------------
iP = 2-1
ax[iP] = plt.subplot(512) 
Title = mySymbol + ' OPT ' + myRights[iRight] + ' for  Strike' + str(myStrikes[iStrike0] )

data4E = data4D.copy() 
ymax = -9999.0
ymin = 9999.0
for iStrike in range(len(myStrikes)):
    for iExp in range(len(myExpDates)):
        ytemp = data4E[iExp][iStrike][iRight][1].copy()
        ymax = max([ymax,max(ytemp)])
        ymin = 0.0

Strike = myStrikes[iStrike0]
Right  = myRights[iRight]

ax[iP].set(xlabel=' ', ylabel='Option, Price', title=Title)
ax[iP].xaxis.set_major_locator(ticker.MultipleLocator(n_tickdays))

Right = myRights[iRight]
n1 = iStrike0
n2 = n1+1
ii = 0
for iStrike in range(n1,n2):  
    Strike = myStrikes[iStrike] 
    for iExp in range(len(myExpDates)):
        Exp    = myExpDates[iExp]
        y2     = data4E[iExp][iStrike][iRight][1]
        y2_Ask = data4E[iExp][iStrike][iRight][2]
        y2_Bid = data4E[iExp][iStrike][iRight][3]
        for i in range(len(y2)):
            i1 = min([i+1,len(x2)-1])
            if ((y2[i] <= 0.0) or ((y2[i] == 0.) and (y2[i1] > 0.)) ):  # remove strange data
                y2[i] = np.NaN
                y2_Bid[i] = np.NaN
                y2_Ask[i] = np.NaN
        if (ii == 0):   #first line
            ax[iP].scatter(x2, y2, color=Colors[iExp], label='', marker='o', s=1)  # to align the xlabels
            plt.draw() 
            #
            plt.setp(ax[iP].get_xticklabels(), rotation=45)
            #
            cticks = ax[iP].get_xticklabels()
            for i in range(len(cticks)):
                cticks[i].set_text('')
            ax[iP].set_xticklabels(convert_cticks(cticks))
            plt.gca().yaxis.grid(True)
            plt.gca().xaxis.grid(True) 
            ax[iP].set_ybound(0.0, ymax) 
            plt.draw()
            # 
        ax[iP].plot(x2, y2, color=Colors[iExp % 7], label=Exp[4:]) 
        if (Consider_Bid_Ask):  # draw dashed lines for bib-ask
            ax[iP].plot(x2, y2_Bid, ':', color=Colors[iExp % 7], linewidth=1)
            ax[iP].plot(x2, y2_Ask, ':', color=Colors[iExp % 7], linewidth=1)
        ii = ii + 1
#
ax[iP].tick_params(labelright=True)
plt.subplots_adjust(wspace=None, hspace=None)
plt.legend(loc='best', ncol=4 ) #'upper left' 

#3 Trades & Balances
# ---------------------------------
IP = 3 -1
ax[iP] = plt.subplot(513)
Title = ' Trades and Balance with Short ' + mySymbol + ' ' + myRights[iRight] + ' at Strike with ' \
         +str(bound) + ' Offset from STK Price. Volatility Factor ' + str(VoltFac)


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
y2   = range(len(x2))
for i in temp:
    ax[iP].plot(x2, list(np.array(y2)*0.0 + myStrikes[i]), color='red', label='',  linewidth=0.5)

# draw dots
if(StrikeType > 1):
    ax[iP].scatter(x2,ytrades2, color='red',  label='Sell',s=50)
else:
    ax[iP].scatter(x2, ytrades2,facecolors = 'none', color='red', label='Sell', s=50)
    
ax[iP].scatter(x2,ytrades3, s=50,facecolors = 'none', edgecolors = 'red')
ax[iP].scatter(x2,ytrades1, color='blue', label='Buy', s=50)

# draw arrows 
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
#        plt.annotate('', xytext=(xj, yj2), xy=(xj4, yj2), va='center', ha='center', \
#                     arrowprops={'arrowstyle': "->", 'lw': 2, 'color': 'black', 'shrinkB': 6.0, 'shrinkB': 6.0, \
#                                 'mutation_scale':25})
        
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
# ---------------------------------
iP = 4-1
ax[iP] = plt.subplot(514)
Title =  ' Execution '+  Right +  ' Contract Price at Various Strike Price and Expiration Dates'
if (StrikeType == 1):
    Title = mySymbol + ' OPT ' + Right + ' for  Strike' + str(Strike)

# to retain value only if a contract is traded
data4E = data4D.copy()  
for iStrike in range(len(myStrikes)):
    for iExp in range(len(myExpDates)):
        for i in range(len(x2)):
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

Right = myRights[iRight]
if(StrikeType == 1):
    n1 = iStrike0
    n2 = n1+1
else:
    n1 = 0
    n2 = len(myStrikes)
ii = 0
for iStrike in range(n1,n2):  
    Strike = myStrikes[iStrike] 
    for iExp in range(len(myExpDates)):
        Exp    = myExpDates[iExp]
        y2     = data4E[iExp][iStrike][iRight][1]   # data4E not data4D here!
        y2_Ask = data4E[iExp][iStrike][iRight][2]
        y2_Bid = data4E[iExp][iStrike][iRight][3]
        for i in range(len(y2)):
            if (y2[i] < -10.0):
                y2[i] = np.NaN
                y2_Bid[i] = np.NaN
                y2_Ask[i] = np.NaN
        if (ii == 0):   #first line to set coordinates
            ax[iP].scatter(x2, y2, color=Colors[iExp], label='', marker='o', s=1)  # to align the xlabels
            plt.draw()
            #
            plt.setp(ax[iP].get_xticklabels(), rotation=45)
            #
            cticks = ax[iP].get_xticklabels()
            for i in range(len(cticks)):
                cticks[i].set_text('')
            ax[iP].set_xticklabels(convert_cticks(cticks))
            plt.gca().yaxis.grid(True)
            plt.gca().xaxis.grid(True)
            ax[iP].set_ybound(0.0, ymax) 
            plt.draw()
            # 
        if(np.count_nonzero(~np.isnan(y2))  == 0):
            pass
        else:
            # plot line
            ax[iP].plot(x2, y2, color=Colors[iExp % 7], label='', linewidth= 2.0)
            # label the line
            label = Right+str(Strike)+'\nE'+Exp[4:]
            ylab  = next(item for item in y2 if (item is not np.NaN))
            xlab  = next(k for k, v in enumerate(y2) if (v == ylab))
            # print contract label
            ax[iP].annotate(label, (xlab,ylab), textcoords='data', color='black', size=10, ha='center', va = 'bottom')
            #
            y2temp = y2[::-1]  # reverse the list 
            # ylab2 = next(item for item in y2temp if (item is not np.NaN))
            # xlab2 = next(k for k, v in enumerate(y2temp) if (v == ylab2))
            # xlab2 = len(x2) - xlab2 -1
            # # draw first dot
            # ax[iP].scatter(xlab, ylab, color='red', label='', s=50 )  # '*')
            # # draw second dot, make it open circle if expired
            # if(ytrades1[xlab2] > 0.0):
            #     ax[iP].scatter(xlab2, ylab2, color='blue', label='', s=50 )  # '*')
            # else:
            #      ax[iP].scatter(xlab2, ylab2, label='', s=50,  facecolors = 'none', edgecolors = 'blue')
            # # add dashed lines to plott bid ask
            # if (Consider_Bid_Ask):
            #     ax[iP].plot(x2, y2_Bid, ':', color=Colors[iExp % 7], linewidth=1)
            #     ax[iP].plot(x2, y2_Ask, ':', color=Colors[iExp % 7], linewidth=1)
        ii = ii + 1
#
plt.subplots_adjust(wspace=None, hspace=None)
ax[iP].tick_params(labelright=True)
#plt.legend(loc='upper left') #loc='best',

# 5 CASH and Short Positions
# ---------------------------------
iP = 5-1
Title =  ' Cash and Short Positions for ' + mySymbol + ' ' + myRights[iRight] + ' Contract'
ax[iP] = plt.subplot(515)
# set coordinates
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
for i in range(len(x2)):
    if( yCash[i] < -990 ):
        yCash[i] = np.NaN
        yShort[i] = np.NaN

ax[iP].bar(xx2-deltax, list(-np.array(yShort)),color='blue',   label='Short Debt',width=[width]*len(x2))
ax[iP].bar(xx2, yCash,                         color='green',    label='Cash', width=[width]*len(x2))
ax[iP].bar(xx2+deltax, list(np.array(yCash)- np.array(yShort)),color='red', label='Net', width=[width]*len(x2))

ax[iP].plot(xx2, list(np.array(yCash)*0.0),color='black', linewidth=1)
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