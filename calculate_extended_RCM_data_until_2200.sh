#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_2200
#SBATCH -e extended_2200.e%j
#SBATCH -o extended_2200.o%j
#SBATCH --time=47:59:00
ulimit -s unlimited

date

for GCM in 'IPSL-CM6A-LR' 'UKESM1-0-LL' 'CESM2-WACCM' 'MRI-ESM2-0' 'ACCESS-CM2' 'ACCESS-ESM1-5' 'CanESM5' 'GISS-E2-1-H'; do

 for SCENAR in 'ssp126' 'ssp585'; do

  if [ $GCM == 'IPSL-CM6A-LR' ]; then
    MEMBER='r1i1p1f1'
    COEFSMB=0.069
    COEFMLT=0.330
    if [ $SCENAR == 'ssp585' ]; then
      SUFFIX=""
    else
      SUFFIX="_FROM_ssp585"
    fi
    XX=""
    SUFFIX2=""
  elif [ $GCM == 'UKESM1-0-LL' ]; then
    MEMBER='r4i1p1f2'
    COEFSMB=0.065
    COEFMLT=0.313
    SUFFIX="_FROM_UKESM1-0-LL-r1i1p1f2-${SCENAR}"
    SUFFIX2="_FROM_UKESM1-0-LL-r1i1p1f2"
    XX="-$MEMBER"
  else
    # average coefficients:
    COEFSMB=0.068
    COEFMLT=0.320
    if [ $GCM == 'GISS-E2-1-H' ]; then
      MEMBER='r1i1p1f2'
    else
      MEMBER='r1i1p1f1'
    fi
    SUFFIX="_FROM_6_MODELS"
    SUFFIX2="_FROM_6_MODELS"
    XX="-$MEMBER"
  fi

  if [ -f /scratchu/njourdain/RCMs_PROTECT/MAR-${GCM}${XX}_asmb_2015-2100_${SCENAR}_regrid_04000m${SUFFIX}.nc ]; then
  
    sed -e "s/<gcm>/$GCM/g ; s/<member>/$MEMBER/g ; s/<scenar>/${SCENAR}/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g ; s/<suffix>/$SUFFIX/g ; s/<suffix2>/${SUFFIX2}/g ; s/<xx>/$XX/g" calculate_extended_RCM_data_until_2200.f90  > szp.f90
  
    ifort -c $NC_INC szp.f90 
    ifort -o szp szp.o $NC_LIB
    ./szp
  
    rm -f szp*

  else

    echo "MAR-${GCM}${XX}_asmb_2015-2100_${SCENAR}_regrid_04000m${SUFFIX}.nc DOES NOT EXIST >>> SKIPPED !"

  fi

 done #SCENAR

done #GCM

date
