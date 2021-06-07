from ContractSamples import ContractSamples
import os
import os as os
import subprocess
#import system
import pandas as pd
import numpy as np
import csv
from matplotlib import pyplot as plt
from matplotlib import dates as mdates
from matplotlib import dates as mdates
from datetime import datetime, timedelta
import matplotlib.ticker as ticker
from ContractSamples import ContractSamples
from myContract_Spec import myContract_Specs
from myReadSamples import *


def bb_extract(Symbol):
    # to extract and merge two types of data files
    fileout = 'data3/' + Symbol + '_bb.csv'

    Head1 = 'UnderlyingSymbol, UnderlyingPrice, Exchange, OptionRoot, OptionExt, Type, Expiration, DataDate, ' + \
             'Strike, Last, Bid, Ask, Volume, OpenInterest, ImpliedVolatility'
            #'Strike, Last, Bid, Ask, Volume, OpenInterest, T1OpenInterest'
    Head2 = 'UnderlyingSymbol, UnderlyingPrice, Exchange, OptionRoot, OptionExt, Type, Expiration, DataDate, ' + \
            'Strike, Last, Bid, Ask, Volume, OpenInterest,  ImpliedVolatility,Delta,Gamma,Theta,Vega,'
            #'Strike, Volume, OpenInterest, Last, Bid, Ask,  ImpliedVolatility,Delta,Gamma,Theta,Vega,'

    Heads1 = Head1.split(',')
    Heads2 = Head2.split(',')
    n1 = len(Heads1)
    n2 = len(Heads2)
    for i in range(n1):
        Heads1[i] = Heads1[i].strip()

    for i in range(n2):
        Heads2[i] = Heads2[i].strip()

    if(os.path.exists(fileout)):
        # search to see if there is new update
        df = pd.read_csv(fileout, header=0)
        if(df.shape[1] == 17):           # First type of files with 15 columns as bb_options_20201030.csv
            df.drop(df.columns[[0]], axis=1, inplace=True)  # old index removed:q
            nrow = df.shape[0]
            for k in range(n1, n2):     # added the index and date
                df.insert(k+1, Heads2[k], [np.NaN] * nrow, True) ## added the  date
            df.to_csv(fileout, index=False)  # remove index
        intDay = df.iloc[-1]['Date']
        start_date = datetime.strptime(str(intDay),"%Y%m%d")
        print(' The last day in the file is:',  start_date, ' in ', fileout)
        start_date = start_date + timedelta(days=1) # next day
        #
    else:
        start_date = datetime(year=2019, month=8, day=1, hour=0, minute=0, second=0)
        #start_date = datetime(year=2020, month=10, day=29, hour=0, minute=0, second=0)
        df = pd.DataFrame()

    end_date   = datetime.today()
    delta      = end_date - start_date
    if(delta.days == 1):
        return
    datafile = open(fileout,'a')
    for iday in range(delta.days):   ######## How many days to extract
        day = start_date + timedelta(days=iday)
        sdate = datetime.strftime(day, "%Y%m%d")
        #vi
        filename = 'data2/bb_options_'+sdate +'.csv'
        filename2 = 'data2/options_' + sdate + '.csv'
        if ((os.path.exists(filename)) or (os.path.exists(filename2)) ):
            if(os.path.exists(filename)):
                df2 = pd.read_csv(filename, header=0)
                df2 = df2[df2['UnderlyingSymbol'] == Symbol]
                nrow = df2.shape[0]
                for k in range(n1,n2):
                    df2.insert(k, Heads2[k], [np.NaN] * nrow, True)
            else:
                filename = filename2
                df2 = pd.read_csv(filename)
                df2.columns = Heads2
                df2 = df2[df2['UnderlyingSymbol'] == Symbol]
            print(Symbol, ' extracting from file:', filename)
            #
            nrow = df2.shape[0]
            df2.insert(0, 'Date',[sdate]*nrow, True)
            df2.reset_index(drop=True)
            #print('df2.shape,len(Heads2)',df2.shape,len(Heads2))
            #print(df2.head(3))
            df2.to_csv(datafile,header=False, index=False)
            #df = df.append(df2)

    datafile.close()
    if(iday == 0):
        return

    df = pd.read_csv(fileout)
    df.columns = ['Date'] + Heads2
    df.sort_values(by='Date')
    df.reset_index(drop=True, inplace=True)

    # print(df.shape)
    # print(df.columns)
    # print(df.dtypes)
    # # print(df.head(3))
    # print('For ' + Symbol, 'file saved in', fileout)
    print('  updated fileout is ',fileout, ' to ', sdate)

    df.to_csv(fileout,  index=False)


def bb_read_data(df, Right, Strike, Exp):
    #print(df.shape)
    df = df[ (df['Type']==Right)  & (df['Strike']==Strike)  & (df['Expiration']==Exp) ]
    #
    # print(df.shape)
    # df.to_csv('test.csv')
    return df

def gRight(Right):
    R = 'put'
    if(Right == 'C'):
        R = 'call'

    return R

def gExp(xdate):
    return xdate[4:6]+'/'+xdate[6:9]+'/'+xdate[0:4]

def bb_get_Strikes(mySymbol):
    filename = 'data3/' + mySymbol + '_bb.csv'
    if (os.path.exists(filename) == False):
        print(' No filename to read for '+mySymbol, ' Run myExtract_bb.py first')
        return 1

    df = pd.read_csv(filename, header=0)
    df.reset_index(inplace=True)
    df.drop_duplicates(subset="Strike", keep="first", inplace=True)
    df = df.sort_values(by='Strike')
    df.reset_index(drop=True, inplace=True)
    # print(df.shape)
    df.to_csv('temp/j1.csv')
    return df['Strike']

def bb_get_Exps(mySymbol):
    filename = 'data3/' + mySymbol + '_bb.csv'
    if (os.path.exists(filename) == False):
        print(' No filename to read for '+mySymbol, ' Run myExtract_bb.py first')
        return 1

    df = pd.read_csv(filename, header=0)
    df.drop_duplicates(subset='Expiration', keep="first", inplace=True)
    df.reset_index(drop=True, inplace=True)
    sExp = df['Expiration'].tolist()
    for i in range(len(sExp)):
        a = sExp[i]
        sExp[i] = a[6:10]+a[0:2]+a[3:5]

    sExp.sort()
    return sExp

def closest(lst, p, right, bound):
    lst = np.asarray(lst.copy()) - bound  # bound > 0 for call, < 0 for put
    ns = len(lst)
    idx = (np.abs(lst - p)).argmin()
    strike_v = lst[idx]
    if (right == 'C'):  # find the nearest value larger than p by bound
        if (p <= strike_v):
            return idx
        else:
            idx2 = min([idx + 1, ns])
            return idx2
    else:
        if (p >= strike_v):
            return idx
        else:
            idx2 = max([idx - 1, 0])
            return idx2


def get_nearestNaN(data4D, myRights,myStrikes, myExpDates, index_nearest0, iStrike0, iRight, index, n, FixedStrike):
    nExp = len(myExpDates)
    nStrike = len(myStrikes)
    index_nearest = index_nearest0 + n
    iStrike = iStrike0
    kkk = 0
    if(FixedStrike):
        Found = False
        for iExp in range(index_nearest, nExp):
            OptPrice = data4D[index_nearest][iStrike][iRight][1][index]
            OptPrice_Bid = data4D[index_nearest][iStrike][iRight][2][index]
            OptPrice_Ask = data4D[index_nearest][iStrike][iRight][3][index]
            if (Found == False):
                if ((np.isnan(OptPrice)) or (np.isnan(OptPrice_Bid)) or (np.isnan(OptPrice_Ask))):
                    Found = False
                    index_nearest = index_nearest + 1
                elif(OptPrice_Bid < 0):
                    Found = False
                    index_nearest = index_nearest + 1
                else:
                    Found = True
                    break
            else:
                pass
        if (index_nearest > nExp ):
            print('  !!!!  NO CONTRACTS AVAILABLE DURING THIS PERIOD FOR STRIKE ')
            print('        DATE, STRIKE PRICE, EXPIRATION AND RIGHTS ARE  ', x2[0], x2[-1], \
                  myStrikes[iStrike], myExpDates[index_nearest], myRights[iright])
            print('        Trying to Change Strike Price')
            #
            index_nearest, istrike = get_nearestNaN(data4D, myRights, myStrikes, myExpDates, \
                                    index_nearest0+n, iStrike, iRight, index, kkk, False)
            kkk = kkk + 1
            if(kkk > 3):
                exit('Code 333 in get_nearestNaN all searched contracts are empty')
    else: # Fixed Expiration Date
        Found = False
        n1 = iStrike
        n2 = nStrike
        n3 = 1
        if(myRights[iRight] == 'P'):
            n2 = 0
            n3 = -1
        for iStr in range(n1, n2, n3):

            print(np.array(data4D).ndim)
            print(np.array(data4D).shape)
            # print('[index_nearest][iStr][iRight][1][index])', index_nearest,iStr,iRight, index )
            # print('data4D[0][4][0][1][0]',data4D[0][4][0])
            OptPrice = data4D[index_nearest][iStr][iRight][1][index]
            OptPrice_Bid = data4D[index_nearest][iStr][iRight][2][index]
            OptPrice_Ask = data4D[index_nearest][iStr][iRight][3][index]
            if (Found == False):
                if ((np.isnan(OptPrice)) or (np.isnan(OptPrice_Bid)) or (np.isnan(OptPrice_Ask))):
                    Found = False
                    iStrike = iStrike + n3
                elif(OptPrice_Bid < 0):
                    Found = False
                    iStrike = iStrike + n3
                else:
                    Found = True
                    break
            else:
                pass
        if ((iStrike > nStrike) or (iStrike < 0)):
            print('  !!!!  NO CONTRACTS AVAILABLE DURING THIS PERIOD FOR STRIKE ')
            print('        DATE, STRIKE PRICE, EXPIRATION AND RIGHTS ARE  ', x2[0], x2[-1], \
                  myStrikes[iStrike], myExpDates[index_nearest], myRights[iright])
            print('        Trying to Change Expiration Dates')
            #
            index_nearest, istrike = get_nearestNaN(data4D, myRights, myStrikes, myExpDates, \
                                                    index_nearest0 + n, iStrike0, iRight, index, kkk, True)
            kkk = kkk+1
            print('kkk2', kkk)
            if (kkk > 3):
                exit('Code 333 in get_nearestNaN all searched contracts are empty')

    return index_nearest, iStrike

#
# mySymbol = 'BIDU'
# bb_extract(mySymbol)