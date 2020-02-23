#/bin/sh

path='/storage1/jhlee/NMSC_2018/Caliop/'
file=$path'CAL_LID_L1-Standard-V4-10.2017-11-03T04-35-45ZD.hdf'
type='calipso532'
output='calipso532.png'

ccplot -i $file
ccplot -y 0..20000 -x +0:00..+1:00 -o $output $type $file
