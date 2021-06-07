import os as os

# find the latest update of files
# grab files
# unzip and move to the right folders
cmd = "wget --ftp-user=tekuate.yahoo.com --ftp-password=Pazzword24$ ftp://l2.deltaneutral.net/classic/options_20200803.zip"
os.system(cmd)
cmd2 = "unzip '*.zip'"
os.system(cmd2)
