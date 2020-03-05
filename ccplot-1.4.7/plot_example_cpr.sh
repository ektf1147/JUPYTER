#/bin/sh

path='/storage1/jhlee/NMSC_2018/Cloudsat/'
file=$path'2017307041756_61267_CS_2B-GEOPROF_GRANULE_P_R04_E06.hdf'
type='cloudsat-reflec'
output='cloudsat.png'
cmap='./cmap/cloudsat-reflectivity.cmap'

ccplot -i $file
ccplot -x 05:13:50..05:15:45 -y 0..20000 -o $output -c $cmap $type $file
