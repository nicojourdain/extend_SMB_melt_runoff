#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_RCM
#SBATCH -e extended_RCM.e%j
#SBATCH -o extended_RCM.o%j
#SBATCH --time=09:59:00
ulimit -s unlimited

date

echo " "
echo "====================================================================================="
echo " Calculating SMB, melt and runoff yearly data from other scenarios of the same model"
echo "====================================================================================="
echo " "

RCM='MAR'
SCENARA='ssp585'
PERIOD_RCM='2015-2100' # '2015-2100' or '2015-2200'

if [ ${PERIOD_RCM} == '2015-2100' ]; then
  PERIOD_GCM='2015_2100'
else
  PERIOD_GCM='2015_2300'
fi

for GCM in 'UKESM1-0-LL' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'ACCESS1-3' 'CNRM-CM6-1' 'CESM2' 'NorESM1-M'; do

for SCENARB in 'ssp126' 'ssp245'; do

if [ $GCM == 'UKESM1-0-LL' ]; then
  MEMBER='r1i1p1f2'
  COEFSMB=0.065
  COEFMLT=0.313
elif [ $GCM == 'IPSL-CM6A-LR' ]; then
  MEMBER='r1i1p1f1'
  COEFSMB=0.069
  COEFMLT=0.330
elif [ $GCM == 'MPI-ESM1-2-HR' ]; then
  MEMBER='r1i1p1f1'
  COEFSMB=0.071
  COEFMLT=0.335
elif [ $GCM == 'ACCESS1-3' ]; then
  MEMBER='r1i1p1'
  COEFSMB=0.058
  COEFMLT=0.320
elif [ $GCM == 'CNRM-CM6-1' ]; then
  MEMBER='r1i1p1f2'
  COEFSMB=0.067
  COEFMLT=0.284
elif [ $GCM == 'CESM2' ]; then
  MEMBER='r11i1p1f1'
  COEFSMB=0.065
  COEFMLT=0.303
elif [ $GCM == 'NorESM1-M' ]; then
  MEMBER='r1i1p1'
  COEFSMB=0.080
  COEFMLT=0.358
fi

rm -f spx.f90
sed -e "s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g ; s/<rcm>/$RCM/g ; s/<gcm>/$GCM/g ; s/<member>/$MEMBER/g ; s/<scenarA>/$SCENARA/g ; s/<scenarB>/$SCENARB/g ; s/<period_rcm>/${PERIOD_RCM}/g ; s/<period_gcm>/${PERIOD_GCM}/g" calculate_extended_RCM_data_from_scenarA_to_scenarB.f90 > spx.f90
ifort -c $NC_INC spx.f90
ifort -o spx spx.o $NC_LIB
./spx

rm -f spx.f90 spx.o spx

done
done

date
