import pandas
import os as os
import time as time
from myContract_Spec import *

#######################################################################
#  Below are what needs to be customized
mySymbol, myType, myHistDate = myHead()
myContracts = myContract_Specs(mySymbol, myType)
#######################################################################

NF = len(myContracts)
for i in range(NF):
    cmd2 = 'python my_getHistData.py'
    print(i,cmd2)
    os.system(cmd2)
    time.sleep(2)

# remove small files
print('Remove empty files:')
dir = 'data/'+mySymbol+'/'+myType +'/'
files = os.listdir(dir)
#print(files)
for file in files:
    file2 = dir+file
    if(os.stat(file2).st_size) < 200:
        os.remove(file2)

print('List of files in folder:', dir)
os.system('ls '+dir)