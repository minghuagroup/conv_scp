import os.path
from myContract_Spec import myHead

mySymbol, myType, myHistDate = myHead()

dir = 'data/'+mySymbol+'/'+myType+'/'
files = os.listdir(dir)
#print(files)

for file in files:
    file2 = dir+file
    if(os.stat(file2).st_size) < 200:
        os.remove(file2)
