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
from random import seed
from random import random, gauss

savefig   = True

N = 10000
N0 = 0
T = 20
fac = 1.0
seed(1)
for i in range(N):
    s = 0.0
    for j in range(T):
        s = s + gauss(0,fac)
        if(s > 0.0):
            N0 = N0 +1
            break

print('N0,N,float(N0/N): ',N0,N,float(N0/N))
