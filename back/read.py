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
#from matplotlib import dates as mdates

def read_STK(mySymbol, myEndDate, myDuration ):
    myType = 'STK'
    #myEndDate = '20201022'
    #myDuration = '50'
    RTH = True  # False #True

    myfolder = 'data/' + mySymbol + '/' + myType
    myfile =  mySymbol + '_' + myType +'_' + myEndDate +'_' + myDuration +'D' + '_20mins_MIDPOINT.csv'
    filename = myfolder + '/' + myfile
    if os.path.exists(filename):
        df = pd.read_csv(filename, header=2)
        print(' in read_STK ', df.head(3))
        print('       length= ', len(df[df.columns[1]]))
        return df
    else:
        print('--- This file does not exist: ', filename)
        #    ES_FUT_20201022_50D_20mins_MIDPOINT.csv
        return []

def read_OPT(mySymbol, myEndDate, myDuration,myRight, myStrike, myExpDate):
    myType = 'OPT'
    #myEndDate = '20201022'
    #myDuration = '100'
    RTH = True  # False #True

    myfolder = 'data/' + mySymbol + '/' + myType
    optfilename = mySymbol + '_' + myType + '_' + myEndDate + '_' + myDuration + 'D' + '_20mins_MIDPOINT_' \
                  + myRight + '_' + str(myStrike) + '_' + myExpDate + '.csv'
    optfilename = myfolder + '/' + optfilename
    if os.path.exists(optfilename):
        df = pd.read_csv(optfilename, header=2)
        print(' in read_OPT ', df.head(3))
        print( '       length= ',len(df[df.columns[1]]))
        return df
    else:
        print('--- This file does not exist: ', filename)
        #    ES_FUT_20201022_50D_20mins_MIDPOINT.csv
        return []


def df2df(df, RTH):
    #RTH = True
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

    if (RTH == False):
        ii = 0
        for i in range(len(ddate)):
            a = str(ddate[i])
            ah = int(a[10:12])
            am = int(a[13:15])
            if ((ah < 9) or (ah > 16)):
                pass
            elif ((ah < 10) and (am < 30)):
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

    data = {'Date': x, 'Open': dopen, 'High':dhigh, 'Low': dlow, 'Close':dclose, 'Volume': dvolume}
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


def align_x(x2, y2, x):

    dmin = min(x)
    dmax = max(x)
    i = 0
    for i in range(len(x2)):
        if (x2[i] < min(x) or x2[i] > max(x)):
            x2[i] = np.NaN
            y2[i] = np.NaN
        i = i + 1

    #print(' a ',len(x2))
    #x2 = x2[x2 <> np.NaN]
    #y2 = y2[y2 <> np.NaN]

    #print(' b ', len(x2))
    return x2,y2

#######################################################################################

RTH = True #False
myEndDate = '20201022'
myDuration = '100'

# mySymbol = 'SPY'
# myType = 'STK'
# df = read_STK(mySymbol)

mySymbol = 'LYFT'
myType = 'OPT'
myRights = ['C', 'P']
myStrikes = [24.0, 24.5, 25., 25.5, 26.0, 26.5, 27.0, 27.5, 28.0, 28.5, 29.0, 29.5, 30.0]
myExpDates = ['20201023', '20201030', '20201106'] #'20201113'] #, '20201120', '20201127', '20201218', '20210115']

Right = 'P'
Strike = 24.0
Exp = '20201023'

Colors = ['Black', 'Red', 'Blue', 'Green', 'Cyan', 'Purple', 'Orange', 'Yellow']

Title = get_Title(mySymbol, myType, Strike, Right,  Exp, RTH)

fig, (ax,ax2) = plt.subplots(2,1, figsize=(10, 8)) #, sharex=True)

df = read_OPT(mySymbol, myEndDate, myDuration, Right, Strike, myExpDates[0])
df2 = df2df(df, RTH)
x0 = df2['Date'].tolist()

ii=1
for Exp in myExpDates:
    print('Exp', Exp)
    df  = read_OPT(mySymbol, myEndDate, myDuration, Right, Strike, Exp)
    df2 = df2df(df, RTH)
    y2 = df2['Close'].tolist()
    x2 = df2['Date'].tolist()
    x2, y2 = align_x(x2, y2, x0)
    #ax2 = ax.twinx()
    ax.set_ylabel('Option Price')
    ax.plot(x2, y2, color=Colors[ii], label=Exp)
    ax.set(xlabel='Date', ylabel='Option, Price', title=Title)
    ax.xaxis.set_major_locator(plt.MaxNLocator(10))
    plt.setp(ax.get_xticklabels(), rotation=45)
    ax.set_ylim(bottom=0, top=3)
    ii = ii + 1

#Stock price plot
df  = read_STK(mySymbol, myEndDate, "50")
df2 = df2df(df, RTH)
y = df2['Close'].tolist()
x = df2['Date'].tolist()
x, y = align_x(x, y, x0)
Title = get_Title(mySymbol, 'STK', Strike, Right,  Exp, RTH)
ax2.plot(x,y,color='Black', label = Exp)      # RTH sensitive dates
ax2.set_ylabel('Stock Price')
ax2.plot(x, y, color=Colors[0], label='Stock')
ax2.set(xlabel='Date', ylabel='Price', title= Title)
ax2.xaxis.set_major_locator(plt.MaxNLocator(10))
plt.setp(ax2.get_xticklabels(), rotation=45)
#ax.set_ylim(bottom = 0, top = 3)

#fig.tight_layout()
plt.legend(loc='best')
plt.show(block = False)
plt.show()


#
# #ax.plot(xdate,y,color='red')
# #ax.plot(xdate2,y2,color='red')      # RTH sensitive dates
# ax.plot(x3,y2,color='red')           # index as x
#
# #ax.scatter(xdate,y,color='red')
# ax.set(xlabel='Date', ylabel='Price', title= Title)
# ax.xaxis.set_major_locator(plt.MaxNLocator(10))
# #ax.xaxis.set_ticks(np.arange(min(x3), max(x3), ndays))
#
# plt.setp(ax.get_xticklabels(), rotation=45)
# w = ax.get_xticks()
# print(w)
# xlabel3 = [xdate[0]]*1000
# ii=0
# for ii in range(len(w)):
#     ij = int(w[ii])
#     if(ij > 0 and ij < len(xdate2)):
#         xlabel3[ii] = xdate2[ij]
#     else:
#         xlabel3[ii] = ''
#     ii=ii+1
#
# xlabel3 = xlabel3[0:ii]
#
# ax.set_xticklabels(xlabel3)
#
# #x2.index(100,2,200)
# #ax.set_ylim(bottom = 23, top = 35)
# plt.show()
#
# #ax.set_xticks([0.2, 0.8])
# #ax.set_xticklabels([r"$\frac{1}{2}\pi$", r"$\pi$"])
#

# Title = get_Title(mySymbol=mySymbol, myType=myType, myStrike=Strike, myRight=Right, \
#                myExpDate=Exp, RTH=RTH)
