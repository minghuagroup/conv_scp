Change the my_Head in myContract_Spec.py for each program!
############################################################


To do:
1. test my_testHistData and my_getHistory data for get_filename
2. test and run myLoop to get data



change the ContractSummary for 4 types
myHistData5.py works, but manually
myLoop.py is to control

Modules: myContractSpec.py for contract spec
Modules: myRead.py for read and plotting

1, Change specifications in myHead in myContractSpec.py
get_filename1 function to obtain one file for writing AWS data to local disk
get_filename2 to obtain data from local disk and plots, could be multiple data
read_data can be more than one file and merge security

To get TWS history data:
2. my_getHistData.py
3. myLoop.py
    remove_small.py

4. plotSTK.py
5. plotOPT.py

get_tws_df.py  : get table, convert csv file, plot, overplot: run my_testHist and myContract_Specs to get raw data

4. plotSTK_bb.py
5. plotOPT_bb.py

6. algo_bbopt.py

git commit - m "fixed plotSTK and plotOPT.py x axises file merge and labels
git commit - m "added plotSTK_bb.py and plotOPT_bb"
git commit - m " made plotSTK.py plotOPT.py, plotSTK_bb.py plotOPT_bb.py work for TWS and bb database"
git commit - m " made algo_sell_options.py work for both algo_sell_put.py and algo_sell_call.py"


git --version
conda --version

install conda, git
mkdir py

git --version
conda --version

git init .
git remote add origin URL to get from GitHub tekuate

conda create --name py python # environment
conda activate py             #environment
conda install pandas
conda install ipython
conda install anaconda
conda install pip
pip install django



