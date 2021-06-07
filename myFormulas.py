from ContractSamples import ContractSamples
import os
import os as os
import subprocess
#import system
import pandas as pd
import numpy as np
import csv
import math
from scipy.stats import norm
import statistics

def option (S, K, T, sig):
    t = T
    sigt = sig * math.sqrt(t)
    d1 = (math.log(S/K)+0.5*sigt*sigt)/sigt
    d2 = d1 - sigt
    N1 = norm.cdf(d1)
    N2 = norm.cdf(d2)
    C = S*N1 - K*N2

    return C

def implied_volatility(S, K, T, C):
    t = T
    N = 1000
    CC = [0.0]*N
    sig0 = 0.01
    delta=1./float(N)

    for i in range(N):
        sig = sig0 + delta*i*2.
        CG  = option(S,K,t,sig)
        CC[i] = CG

    a = np.array(CC)
    jj = np.abs(a - C).argmin()
    sig0 = sig0 + delta*jj*2.

    return sig0
#

def real_volatility(close):

    flag = 2  # 1 (S2-S1)/S1, 2 log(S2/S1)
    N  = len(close)
    N1 = N-1

    dclose = np.array(close)
    if(flag == 1):
        data = ( dclose[1:]- dclose[:N1] )/dclose[1:]
    else:
        data = np.log(dclose[1:]/dclose[:N1])

    std = np.std(data)*np.sqrt(253.)
    return round(std,3)

def real_volatilities(O,H,L,C,N):
    #
    # Based on https://www.rdocumentation.org/packages/TTR/versions/0.23-4/topics/volatility
    #
    n = len(C)
    n1 = n-1

    data = {"close" : 0., "garman.klass" : 0., "parkinson" : 0., "rogers.satchell" : 0., "gk.yz" : 0., "yang.zhang" : 0.}

    dclose = np.array(C)
    dopen  = np.array(O)
    dhigh  = np.array(H)
    dlow   = np.array(L)

    r  = np.log(dclose[1:]/dclose[:n-2])
    HL = np.log(dhigh/dlow)
    HC = np.log(dhigh / dclose)
    HO = np.log(dhigh / dopen)
    LC = np.log(dlow  / dclose)
    LO = np.log(dlow  / dopen)
    CO = np.log(dclose/dopen)
    OLm = np.log(dopen[1:] / dclose[:n-2])
    OLm.insert(0,0)

    data["close"]        = np.std(r)*np.sqrt(N)
    data["garman.klass"] = np.sqrt ( N/n * np.sum( 0.5*HL*HL - (2*np.log(2.)-1)*CO*CO) )
    data["parkinson"]    = np.sqrt ( N/(4*n*np.log(2.)) * np.sum( HL*HL) )
    data["rogers.satchell"] = np.sqrt ( N/n * np.sum( HC*HO + LC/LO ) )
    data["gk.yz"]           = np.sqrt ( N/n * np.sum( OCm*OCm + 0.5*HL*HL - (2*np.log(2.)-1)*CO*CO) )

    alpha = 1.34
    k = (alpha-1)/(alpha+ (n+1)/(n-1))
    mu0 = 1/n * p.sum(OCm)
    muc = 1/n * np.sum(CO)
    sig0 = N/(n-1) * np.sum((OCm - mu0)**2)
    sigc = N/(n-1) * np.sum((CO-muc)**2)
    sigrs = N/n*np.sum(HC*HO + LC/LO)

    data["yang.zhang"]      = np.sqrt ( sig0 * sig0 + k * sigc * sigc + (1-k) * sigrs * sigrs )

    return data


def calc_bound(x2, index, iExp0, myExpDates,stock, myStrikes, data4D, iStrike, iRight, VoltFac, bound0):
    #
    limit1 = 0.1
    limit2 = 1.5
    bounds = [np.NaN]*len(myExpDates)
    for iExp in range(iExp0, len(myExpDates)-2):
        try:
            T = next(k for k, v in enumerate(x2[index:]) if (v == myExpDates[iExp]))
            boundX = implied_volatility(stock[index],myStrikes[iStrike],T/253., data4D[iExp][iStrike][iRight][1][0])
            #print('boundX',boundX)
            if((boundX > limit1) and (boundX < limit2)):
                bounds[iExp] =  boundX
        except:
            pass
        #
    #print('bound',bounds)
    bounds = [x for x in bounds if ((x > limit1) and (x< limit2))]
    Volatilities = bounds
    if(len(bounds) == 0):
        bound2 = bound0
    else:
        #bound  = statistics.median(Volatilities)
        bound2 =  stock[index] * VoltFac / np.sqrt(253)  * statistics.median(Volatilities)

    return Volatilities, round(bound2,3)


# S = 27.14
# K = 27.00
# C0 = 0.59
# T = 3./253
# v1 = implied(S, K, T, C0)
#
# C1 = option (S, K, T, v1)
#
# #print('C0,C1,v1',C0,C1,v1)
#

