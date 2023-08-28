#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_RCM_2200
#SBATCH -e extended_RCM_2200.e%j
#SBATCH -o extended_RCM_2200.o%j
#SBATCH --time=11:59:00
ulimit -s unlimited

MODELA='IPSL-CM6A-LR'
MEMBERA='r1i1p1f1'
SCENARA='ssp585'
COEFSMB=0.069
COEFMLT=0.330

for MODELB in 'UKESM1-0-LL' 'CESM2-WACCM' 'MRI-ESM2-0' 'ACCESS-CM2' 'ACCESS-ESM1-5' 'CanESM5' 'GISS-E2-1-H'; do

for SCENARB in 'ssp126' 'ssp585'; do

if [ $MODELB == 'UKESM1-0-LL' ]; then
  MEMBERB='r4i1p1f2'
elif [ $MODELB == 'CESM2-WACCM' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'MRI-ESM2-0' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'ACCESS-CM2' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'ACCESS-ESM1-5' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'CanESM5' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'GISS-E2-1-H' ]; then
  MEMBERB='r1i1p1f2'
fi

echo "$MODELA $MEMBERA $SCENARA  -->  $MODELB $MEMBERB $SCENARB"
rm -f jta.f90
sed -e "s/<modelA>/$MODELA/g ; s/<memberA>/$MEMBERA/g ; s/<scenarA>/$SCENARA/g ; s/<modelB>/$MODELB/g ; s/<memberB>/$MEMBERB/g ; s/<scenarB>/$SCENARB/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g" calculate_extended_RCM_data_from_model_to_other_model_2200.f90 > jta.f90
ifort -c $NC_INC jta.f90
ifort -o jta jta.o $NC_LIB
./jta

done # SCENARB

done # MODELB

rm -f jta.f90 jta.o jta

date

