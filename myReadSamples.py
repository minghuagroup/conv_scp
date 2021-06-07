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
#from collections import Counter

def read_Data(filename2s):  #could be multiple files merged and sorted

    if(isinstance(filename2s, str)):
        NF = 1
        if(filename2s == "No Filename"):
            return pd.DataFrame()
    elif(len(filename2s) == 0):
        return pd.DataFrame()
    else:
        NF = len(filename2s)

    filenames = filename2s
    if(NF == 1):
        filenames = [filenames]

    # print('XXX2', len(filenames), type(filenames))
    # print(filenames)
    ddf = pd.DataFrame()
    ii = 0
    for filename in filenames:
            df = pd.read_csv(filename, header=2)
            ddf = ddf.append(df)
            ii = ii +1

    if(ii == 1):
        vddf = ddf
    elif(ii > 1):
        #print('YYY',type(ddf))
        vddf = ddf
        #vddf = pd.concat(ddf, axis = 0)
    else:
        print('--- These file(s) do not exist: ', filenames)
        vddf = pd.DataFrame()

    return vddf

def my_drop_pdduplicates(df):
    print('typeXXX',type(df), df.shape)
    n = df.shape[0]
    df.drop_duplicates(subset="Date", keep="first", inplace=True)
    df.reset_index(drop=True, inplace=True)
    print('typeYYY', type(df), df.shape)
    return df

def df2df(df, RTH):
    #RTH = True
    if(df.empty):
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
    # print(' in df2df ', df2.head(3))
    # print('       length= ', len(df2['Date']))
    # #print('len1a = ', len(list(df)))
    #print('len2a = ', len(list(df2)))

    df2 = df2.drop_duplicates()
    df3 = df2.sort_values(by = 'Date')
    return df3

def df2df_daily(df):
    #RTH = True
    if(df.empty):
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

    x = ddate
    xdate = [datetime.now()]*len(x)
    for i in range(len(x)):
        xdate[i] = datetime.strptime(x[i], "%Y%m%d")

    data = {'Date': xdate, 'Open': dopen, 'High':dhigh, 'Low': dlow, 'Close':dclose, 'Volume': dvolume}
    #data = {'Date': ddate, 'Open': dopen, 'High': dhigh, 'Low': dlow, 'Close': dclose, 'Volume': dvolume}

    df2 = pd.DataFrame(data)
    # print(' in df2df ', df2.head(3))
    # print('       length= ', len(df2['Date']))
    # #print('len1a = ', len(list(df)))
    #print('len2a = ', len(list(df2)))

    df2 = df2.drop_duplicates()
    df3 = df2.sort_values(by = 'Date')
    return df3


def get_filename1(myHistDate, symbol, secType, right, strike, Exp):  #one file, for my_getHistData.py

    myjunk = myHistDate
    myenddate = myjunk[0].split(" ")[0]
    myduration = myjunk[1].split(" ")[0] + myjunk[1].split(" ")[1]
    myfreq = myjunk[2].split(" ")[0] + myjunk[2].split(" ")[1]
    myMidType = myjunk[3]

    myfolder = 'data/' + symbol + '/' + secType + '/'
    if (os.path.isdir('data/'+symbol) == False):
        os.mkdir('data/' + symbol)

    if (os.path.isdir(myfolder) == False):
        os.mkdir(myfolder)

    filename =  symbol + '_' +  secType   + '_' + myenddate + '_' + myduration + '_' + myfreq + '_' +  myMidType
    if ((secType == 'OPT') or (secType == 'FOP')):
        filename = filename + '_' + right + '_'  + str(strike) + '_' + Exp

    filename   = myfolder +  filename  + '.csv'

    # if(os.path.exists(filename) == False):
    #     filename = my_nearest_filename(filename)

    #print('filename = ',filename)
    return  filename

def get_filename2(myHistDate, symbol, secType, right, strike, Exp): #get possible multiple files for plots

    myjunk = myHistDate
    myenddate = myjunk[0].split(" ")[0]
    myduration = myjunk[1].split(" ")[0] + myjunk[1].split(" ")[1]
    myfreq = myjunk[2].split(" ")[0] + myjunk[2].split(" ")[1]
    myMidType = myjunk[3]

    myfolder = 'data/' + symbol + '/' + secType + '/'
    if (os.path.isdir('data/'+symbol) == False):
        os.mkdir('data/' + symbol)

    if (os.path.isdir(myfolder) == False):
        os.mkdir(myfolder)

    fileseg1 = myfolder + symbol + '_' + secType + '_'
    fileseg2 = '_' + myfreq + '_' + myMidType
    if ((secType == 'OPT') or (secType == 'FOP')):
        fileseg2 = fileseg2 + '_' + right +'_'+str(strike) + '_'+ Exp + '.csv'
    else:
        fileseg2 = fileseg2+'.csv'

    lsnames = fileseg1 + '*' +fileseg2
    cmd = 'ls ' + lsnames + ' > junk2.ps'
    # print('cmd',cmd)

    subprocess.call(cmd, shell = True)

    with open("junk2.ps", "r") as junkfile:
        filenames2 = junkfile.readlines()

    filenames = list()
    for filename in filenames2:
        if('_p.csv' in filename):
            pass
        else:
            filenames.append(filename.rstrip())

    if(len(filenames) == 0):
        print(' No filename is found for this security in get_filename ',filename)
        print('myHistDate, symbol, secType, right, strike, Exp',myHistDate, symbol, secType, right, strike, Exp)
        return "No Filename"
        #sys.exit('Exit in get_filename in myContract_Soec.py')
    elif len(filenames) == 1:
        filenames = filenames[0]

    # print('XXX0', type(filenames))
    # print(filenames)

    return filenames

#
def get_tws_df(filename, savefile):
    c = str(filename).split('/')
    RTH = True
    #print('   Input filename:       ', filename)
    df  = read_Data(filename)
    if('day' in c[3]):              #  data/ES/FUT/ES_FUT_20201106_360D_1day_MIDPOINT.csv
        df2 = df2df_daily(df)
    else:
        df2 = df2df(df, RTH)

    if (savefile):
        print('   Input filename:       ', filename)
        s = str(filename).split('.')
        filename_out = s[0] + '_p.csv'
        df2.to_csv(filename_out)
        print('   Output file saved in ', filename_out)

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

def mydaylist2int(dates):
    alist = mydaylist2str(dates)
    if (isinstance(alist, str)):
        return int(alist)

    blist = list()
    for item in alist:
        blist.append(int(item))

    return blist

def mydaylist2str(dates):
    if (isinstance(dates, datetime)):
        return datetime.strftime(dates, "%Y%m%d")

    alist = list()
    for item in dates:
        alist.append(datetime.strftime(item, "%Y%m%d"))

    return alist


def mystrlist2day(dates):
    if (isinstance(dates, str)):
        return datetime.strptime(dates, "%Y%m%d")

    alist = list()
    for item in dates:
        alist.append(datetime.strptime(item, "%Y%m%d"))

    return alist


def myintlist2day(dates):
    if(isinstance(dates,int)):
        return datetime.strptime(str(dates), "%Y%m%d")

    alist = list()
    for item in dates:
        alist.append(datetime.strptime(str(item), "%Y%m%d"))

    return alist

def WeekDays(date):
    Days = ['Mon', 'Tue', 'Wed', 'Thr', 'Fri', 'Sat', 'Sun']
    return Days[date.weekday()]

def convert_cticks(cticks):
    for i in range(len(cticks)):
        a = cticks[i].get_text()
        if(a == ''):
            pass
        else:
            # print('i',i)
            # print(cticks)
            # print(type(a),a)
            da = datetime.strptime(a[0:8], "%Y%m%d")
            # print(type(da),da)
            day = WeekDays(da)
            b = a[4:6] + '/' + a[6:8]
            if (i % 10 == 1):
                b = a[0:4] + '\n' + b
            else:
                pass
                #b = b + '\n' + day
            cticks[i].set_text(b)

    return cticks


def get_ntickdays(x2, number, interval):
    n_tickdays = int(len(x2) / number)
    n_tickdays = n_tickdays - (n_tickdays % interval)
    if (n_tickdays < interval):
        n_tickdays = interval
    if (n_tickdays > number):
        n_tickdays = number

    return n_tickdays

def get_x2_aligned (x1,y1, x2,y2):
    y3 = y1.copy()
    for i in range(len(y3)):
        y3[i] = np.NaN

    for k in range(len(x1)):
        try:
            i = x2.index(x1[k])
            y3[k] = y2[i]
            #print(k,x1[k],y1[k],y3[k])
        except:
            pass

    return y3