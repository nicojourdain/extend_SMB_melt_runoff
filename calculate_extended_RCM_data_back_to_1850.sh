#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_1850
#SBATCH -e extended_1850.e%j
#SBATCH -o extended_1850.o%j
#SBATCH --time=47:59:00
ulimit -s unlimited

date

TOCKEN=0

# NB: 2 time UKESM1-0-LL because 2 ensemble members :
for GCM in 'IPSL-CM6A-LR' 'CNRM-CM6-1' 'UKESM1-0-LL' 'UKESM1-0-LL' 'CESM2-WACCM' 'MRI-ESM2-0' 'NorESM2-MM' 'ACCESS-CM2' 'ACCESS-ESM1-5' 'CanESM5' 'CNRM-ESM2-1' 'GFDL-CM4' 'GFDL-ESM4' 'CESM2' 'MPI-ESM1-2-HR' 'INM-CM5-0' 'GISS-E2-1-H'; do

  if [ $GCM == 'IPSL-CM6A-LR' ]; then
    MEMBER='r1i1p1f1'
    COEFSMB=0.069
    COEFMLT=0.330
    XX=""
    SUFFIX=""
    SUFFIX2=""
  elif [ $GCM == 'CNRM-CM6-1' ]; then
    MEMBER='r1i1p1f2'
    COEFSMB=0.067
    COEFMLT=0.284
    XX=""
    SUFFIX=""
    SUFFIX2=""
  elif [ $GCM == 'UKESM1-0-LL' ] && [ $TOCKEN -eq 0 ]; then
    MEMBER='r1i1p1f2'
    COEFSMB=0.065
    COEFMLT=0.313
    SUFFIX=""
    SUFFIX2=""
    XX=""
  elif [ $GCM == 'UKESM1-0-LL' ] && [ $TOCKEN -eq 1 ]; then
    MEMBER='r4i1p1f2'
    COEFSMB=0.065
    COEFMLT=0.313
    SUFFIX="_FROM_UKESM1-0-LL-r1i1p1f2-histo"
    SUFFIX2="_FROM_UKESM1-0-LL-r1i1p1f2"
    XX="-$MEMBER"
  elif [ $GCM == 'CESM2' ]; then
    MEMBER='r11i1p1f1'
    COEFSMB=0.065
    COEFMLT=0.303
    XX=""
    SUFFIX=""
    SUFFIX2=""
  elif [ $GCM == 'MPI-ESM1-2-HR' ]; then
    MEMBER='r1i1p1f1'
    COEFSMB=0.071
    COEFMLT=0.335
    XX=""
    SUFFIX=""
    SUFFIX2=""
  else
    # average coefficients:
    COEFSMB=0.068
    COEFMLT=0.320
    if [ $GCM == 'CNRM-ESM2-1' ] || [ $GCM == 'GISS-E2-1-H' ]; then
      MEMBER='r1i1p1f2'
    else
      MEMBER='r1i1p1f1'
    fi
    SUFFIX="_FROM_6_MODELS"
    SUFFIX2="_FROM_6_MODELS"
    XX="-$MEMBER"
  fi

  if [ -f /scratchu/njourdain/RCMs_PROTECT/MAR-${GCM}${XX}_asmb_1980-2014_histo_regrid_04000m${SUFFIX}.nc ]; then
  
    sed -e "s/<gcm>/$GCM/g ; s/<member>/$MEMBER/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g ; s/<suffix>/$SUFFIX/g ; s/<suffix2>/${SUFFIX2}/g ; s/<xx>/$XX/g" calculate_extended_RCM_data_back_to_1850.f90 > kgj.f90
  
    ifort -c $NC_INC kgj.f90 
    ifort -o kgj kgj.o $NC_LIB
    ./kgj
  
    rm -f kgj*

  else

    echo "MAR-${GCM}${XX}_asmb_1980-2014_histo_regrid_04000m${SUFFIX}.nc DOES NOT EXIST >>> SKIPPED !"

  fi

  if [ $GCM == 'UKESM1-0-LL' ]; then
    TOCKEN=1
  fi

done

date
