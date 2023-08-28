#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_RCM
#SBATCH -e extended_RCM.e%j
#SBATCH -o extended_RCM.o%j
#SBATCH --time=05:59:00
ulimit -s unlimited

date

#for GCM in 'IPSL-CM6A-LR' 'UKESM1-0-LL' 'CESM2-WACCM' 'ACCESS-CM2'; do
for GCM in 'ACCESS-CM2' ; do
for SCENAR in 'ssp126' 'ssp585'; do

  if [ $GCM == 'UKESM1-0-LL' ]; then
    MEMBER='r4i1p1f2'
    COEFSMB=0.065
    COEFMLT=0.313
    SUFFIX="_FROM_UKESM1-0-LL-r1i1p1f2-${SCENAR}"
    SUFFIX2="_FROM_UKESM1-0-LL-r1i1p1f2"
    XX="-$MEMBER"
  elif [ $GCM == 'IPSL-CM6A-LR' ]; then
    MEMBER='r1i1p1f1'
    COEFSMB=0.069
    COEFMLT=0.330
    XX=""
    if [ $SCENAR == 'ssp585' ]; then
      SUFFIX=""
      SUFFIX2=""
    else
      SUFFIX="_FROM_ssp585"
      SUFFIX2=""
    fi
  else
    COEFSMB=0.068
    COEFMLT=0.320
    MEMBER='r1i1p1f1'
    SUFFIX="_FROM_6_MODELS"
    SUFFIX2="_FROM_6_MODELS"
    XX="-$MEMBER"
  fi

  if [ -f /scratchu/njourdain/RCMs_PROTECT/MAR-${GCM}${XX}_asmb_2015-2100_${SCENAR}_regrid_04000m${SUFFIX}.nc ]; then
  
    sed -e "s/<gcm>/$GCM/g ; s/<member>/$MEMBER/g ; s/<scenar>/$SCENAR/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g ; s/<suffix>/$SUFFIX/g ; s/<suffix2>/${SUFFIX2}/g ; s/<xx>/$XX/g" calculate_extended_RCM_data_new.f90 > gjk.f90
  
    ifort -c $NC_INC gjk.f90 
    ifort -o gjk gjk.o $NC_LIB
    ./gjk
  
    rm -f gjk*

  fi

done
done

date
