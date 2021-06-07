####
#### 2 places need to change
#    1: muHead(),
#    2: myContract_Specs for OPT specifications

from ContractSamples import ContractSamples
import datetime
import time
import os
import subprocess
import numpy as np

def myHead():
    myHistDate = ["20201022 23:59:00 GMT", "50 D", "20 mins", "MIDPOINT"]
    #myHistDate = ["20201030 23:59:00 GMT", "30 D", "1 hour", "MIDPOINT"]
    #myHistDate = ["20201106 23:59:00 GMT", "300 D", "1 day", "MIDPOINT"]
    myHistDate = ["20201106 23:59:00 GMT", "360 D", "1 day", "MIDPOINT"]
    myHistDate = ["20191230 23:59:00 GMT", "300 D", "1 day", "MIDPOINT"]

    mySymbol = 'SPY'
    myType   = 'STK'
    return mySymbol, myType, myHistDate
#
# Typical calling sequence below
# mySymbol, myType, myHistDate = myHead()
# myContracts = myContract_Specs(mySymbol, myType)

def myContract_Specs(mySymbol, myType):
    myRights = ['C', 'P']
    myStrikes = [0] * 1000
    myExpDates = [''] * 1000
    NF = 0
    # myType = 'OPT'
    # mySymbol = 'QQQ'

    print(mySymbol, myType)
    if (myType == 'STK'):
        myContract = ContractSamples.mySTK_Contract()
        myContract.symbol = mySymbol
    elif (myType == 'FUT'):
        myContract = ContractSamples.myFUT_Contract()
        myContract.symbol = mySymbol
    elif (myType == 'OPT'):
        myContract = ContractSamples.myOPT_Contract()
        myContract.symbol = mySymbol
    elif (myType == 'FOP'):
        myContract = ContractSamples.myFUTOPT_Contract()
        myContract.symbol = mySymbol
    else:
        print('No valid contract type is specified myType =', myType)
        return

    if (myType == 'STK' or myType == 'FUT'):
        return myContract

    # Options Contracts. Changes could be made here

    if (mySymbol == 'LYFT'):
        myStrikes = [24.0, 24.5, 25., 25.5, 26.0, 26.5, 27.0, 27.5, 28.0, 28.5, 29.0, 29.5, 30.0]
        myExpDates = ['1023', '1030', '1106','1113','1120','1127', '1204','1211','1218']
        myExpDates = ['1113']#,'1120','1127', '1204','1211','1218']
        myExpDates = ['1023','1030', '1106']
        for i in range(len(myExpDates)):
            myExpDates[i] = '2020' + myExpDates[i]
    #
    elif (mySymbol == 'JETS'):
        myStrikes = [16.5, 17.0, 17.5, 18.0, 18.5, 19.0]
        myExpDates = ['1023', '1030', '1106'] #, '1113', '1120', '1127', '1218']
        myExpDates = ['1106', '1113', '1120', '1127', '1218']
        for i in range(len(myExpDates)):
            myExpDates[i] = '2020' + myExpDates[i]
    #
    elif (mySymbol == 'SPY'):
        for i in range(41):
            myStrikes[i] = 320.0 + i * 1  # 320 to 360
        myExpDates = ['1023', '1026', '1028', '1030',
                      '1102']  # , '1104', '1106', '1111', '1113', '1116', '1118', '1120']
        for i in range(len(myExpDates)):
            myExpDates[i] = '2020' + myExpDates[i]
    #
    elif (mySymbol == 'QQQ'):
        for i in range(41):
            myStrikes[i] = 260 + i * 1  # 260 to 300
        myExpDates = ['1023', '1030', '21106', '1113', '1120', '1127', '1218']
        for i in range(len(myExpDates)):
            myExpDates[i] = '2020' + myExpDates[i]
    #
    elif (mySymbol == 'ES'):
        myStrikes = [0] * 61
        for i in range(len(myStrikes)):
            myStrikes[i] = 3200.0 + i * 5  # 3200 to 3500
        myExpDates = ['1023', '1026', '1028', '1030',
                      '1102']  # , '1104', '1106', '1111', '1113', '1116', '1118', '1120']
        for i in range(len(myExpDates)):
            myExpDates[i] = '2020' + myExpDates[i]
    else:
        print('No valid symbol is specified for OPT aor FOP contracts. mySymbol = ', mySymbol)
        return

    ii = 0
    myContracts = [None] * 1000
    for right in myRights:
        for strike in myStrikes:
            for expdate in myExpDates:
                if (myType == 'OPT'):
                    myContract = ContractSamples.myOPT_Contract()
                elif (myType == 'FOP'):
                    myContract = ContractSamples.myFUTOPT_Contract()
                myContract.symbol = mySymbol
                myContract.right = right
                myContract.strike = strike
                myContract.lastTradeDateOrContractMonth = expdate
                myContracts[ii] = myContract
                ii = ii + 1
                #print('ii=',ii-1,myContracts[ii-1])

    NF = ii
    myContracts = myContracts[0:NF]
    # for i in range(NF):
    #      print('---- All contracts', i, ' ', myContracts[i])

    if(NF==1):
        myContracts = myContracts[0:NF]

    return myContracts


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

    data2 = list(set(Data))
    data3 = np.sort(data2)
    return data3    #unique values
#
# def my_nearest_filename(filename):
#     myfolder, symbol, secType, myenddate, myduration, \
#     myfreq, myMidType, right, strike, Exp =   parse_filename(filename)  #all strings
#
#     fileseg1 = myfolder + symbol + '_' + secType + '_'
#     fileseg2 = '_' + myfreq + '_' + myMidType
#     if ((secType == 'OPT') or (secType == 'FOP')):
#         fileseg2 = fileseg2 + '_' + right +'_'+str(strike) + '_'+ Exp + '.csv'
#     else:
#         fileseg2 = fileseg2+'.csv'
#
#     lsnames = myfolder + fileseg1 + '*' +fileseg2
#     cmd = 'ls ' + lsnames + ' > junk2.ps'
#     os.system(cmd)
#     with open("junk.ps", "r") as junkfile:
#         texts = junkfile.readlines()
#     enddays = [20201022] * 1000
#     durdays = [100] * 1000
#
#     dd1
#     i = 0
#     for line in texts:
#         line = line.rstrip().lstrip()
#         a = line.split("_")
#         enddays[i] = int(a[2])
#         durdays[i] = int(a[3][0:len(a[3] - 1)])
#         i = i + 1
#     NC = i
#     enddays = enddays[0:NC]
#     durdays = durdays[0:NC]
#
#     filename = symbol + '_' + secType + '_' + myenddate + '_' + myduration + '_' + myfreq + '_' + myMidType
#     if ((secType == 'OPT') or (secType == 'FOP')):
#         filename = filename + '_' + right + '_' +  strike  + '_' + Exp
#
#

def parse_filename(filename):

    c  = filename.split('/')
    myfolder = c[0]+'/'+c[1]+'/'+c[2]+'/'

    cc = c[3]
    ccc = f1.split('_')
    symbol  = ccc[0]
    secType = ccc[1]
    myenddate  = ccc[2]
    myduration = ccc[3]
    myfreq     = ccc[4]
    myMidType  = ccc[5]
    right       = 'C'
    strike      = '0'
    Exp         = '20201030'
    if ((secType == 'OPT') or (secType == 'FOP')):
        right = ccc[6]
        strike = ccc[7]
        Exp    = ccc[8]

    return myfolder, symbol, secType, myenddate, myduration, myfreq, myMidType,right,strike,Exp

def print_list(x2):
    print('in print_list ---> type(x2), len(x2),type(x2[0]), x2[0], x2[-1]):')
    print('                  ',type(x2), len(x2), type(x2[0]), x2[0],' .... to ....', x2[-1])

def myfilenames_req(myHistDate, myContracts):

    try:
        NF = len(myContracts)
    except:
        NF = 1  #not a list of contracts


    if(NF == 1):
        myContracts = [myContracts]*1

    print(myHistDate)
#    for contract in Contracts:
#        print(contract)

    myjunk = myHistDate
    myenddate = myjunk[0].split(" ")[0]
    myduration = myjunk[1].split(" ")[0] + myjunk[1].split(" ")[1]
    myfreq = myjunk[2].split(" ")[0] + myjunk[2].split(" ")[1]
    mytype = myjunk[3]
    myfilenames = [''] * NF

    for ii in range(NF):
        myfolder = 'data/' + str(myContracts[ii].symbol) + '/' + str(myContracts[ii].secType) + '/'
        if (os.path.isdir('data/' + str(myContracts[ii].symbol)) == False):
            os.mkdir('data/' + str(myContracts[ii].symbol))
        if (os.path.isdir(myfolder) == False):
            os.mkdir(myfolder)
        myfilenames[ii] = str(myContracts[ii].symbol) + '_' + str(myContracts[ii].secType) \
                          + '_' + myenddate + '_' + myduration + '_' + myfreq + '_' + mytype
        if ((myContracts[ii].secType == 'OPT') or (myContracts[ii].secType == 'FOP')):
            myfilenames[ii] = myfilenames[ii] + '_' + myContracts[ii].right + '_' \
                              + str(myContracts[ii].strike) + '_' \
                              + myContracts[ii].lastTradeDateOrContractMonth
        myfilenames[ii] = myfolder + myfilenames[ii] + '.csv'

    for i in range(NF):
        print('---- All filenames', i, ' ', myfilenames[i])

    if(NF == 1):
        myfilenames = myfilenames[0]

    return myfilenames
#
#

#
# #####################################################
#
# myqueryTime = (datetime.datetime.today() - datetime.timedelta(days=10)).strftime("%Y%m%d %H:%M:%S")
# myqueryTime = "20201022 24:00:00 GMT"  # end data
# myHistDate = [myqueryTime, "50 D", "20 mins", "MIDPOINT"]  # TRADES BID-ASK MIDPOINT
# #
# mySymbol = 'JETS' #'ES' #LYFT'
# myType = 'OPT' #'FUT' #FOP' #OPT' #STK'
# #
# myContracts = myContract_Specs(mySymbol, myType)
# # filenames   = myfilenames_req(myHistDate, myContracts)
#
# filename = get_filename(myHistDate, 'LYFT', 'OPT', 'C', 26.0, '20201030')
#
# print(filename)