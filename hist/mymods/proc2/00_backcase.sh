#!/bin/csh -f
set casename = F2000MG1
echo "backup $casename"
mkdir backup/{$casename}
mkdir backup/{$casename}/infiles
mkdir backup/{$casename}/mymods

cp runlog backup/{$casename}/infiles/
cp -L *_in backup/{$casename}/infiles/
cp -L docn.stream* backup/{$casename}/infiles/
cp -L ../mymods/*.F90 backup/{$casename}/mymods/

echo "done in backup!"
