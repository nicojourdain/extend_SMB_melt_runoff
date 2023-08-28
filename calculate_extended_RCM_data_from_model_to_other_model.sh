#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_RCM
#SBATCH -e extended_RCM.e%j
#SBATCH -o extended_RCM.o%j
#SBATCH --time=47:59:00
ulimit -s unlimited

echo " "
echo "========================================================================="
echo " Calculating SMB, melt and runoff yearly data from other RCM simulations"
echo "========================================================================="
echo " "

for MODELA in 'UKESM1-0-LL' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'ACCESS1-3' 'CNRM-CM6-1' 'CESM2' 'NorESM1-M'; do

TOCKEN=0

# Two times UKESM1-0-LL because two members :
for MODELB in 'CNRM-CM6-1' 'UKESM1-0-LL' 'UKESM1-0-LL' 'CESM2-WACCM' 'MRI-ESM2-0' 'NorESM2-MM' 'ACCESS-CM2' 'ACCESS-ESM1-5' 'CanESM5' 'CNRM-ESM2-1' 'GISS-E2-1-H' 'GFDL-CM4' 'GFDL-ESM4' 'CESM2' 'MPI-ESM1-2-HR' 'ACCESS1-3' 'INM-CM5-0'; do

for SCENARB in 'historical' 'ssp126' 'ssp245' 'ssp585'; do

SCENARB2=$SCENARB

if [ $MODELA == 'UKESM1-0-LL' ]; then
  MEMBERA='r1i1p1f2'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  elif [  $SCENARB == 'ssp126' ]; then
    SCENARA='ssp126'
  elif [  $SCENARB == 'ssp245' ]; then
    SCENARA='ssp245'
  elif [  $SCENARB == 'ssp585' ]; then
    SCENARA='ssp585'
  fi
  COEFSMB=0.065
  COEFMLT=0.313
elif [ $MODELA == 'IPSL-CM6A-LR' ]; then
  MEMBERA='r1i1p1f1'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  else
    SCENARA='ssp585'
  fi
  COEFSMB=0.069
  COEFMLT=0.330
elif [ $MODELA == 'MPI-ESM1-2-HR' ]; then
  MEMBERA='r1i1p1f1'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  elif [  $SCENARB == 'ssp126' ]; then
    SCENARA='ssp245' # taking higher scenario as relatively low ECS for this model
  elif [  $SCENARB == 'ssp245' ]; then
    SCENARA='ssp585' # taking higher scenario as relatively low ECS for this model
  elif [  $SCENARB == 'ssp585' ]; then
    SCENARA='ssp585'
  fi
  COEFSMB=0.071
  COEFMLT=0.335
elif [ $MODELA == 'ACCESS1-3' ]; then
  MEMBERA='r1i1p1'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  else
    SCENARA='rcp85'
  fi
  COEFSMB=0.058
  COEFMLT=0.320
elif [ $MODELA == 'CNRM-CM6-1' ]; then
  MEMBERA='r1i1p1f2'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  else
    SCENARA='ssp585'
  fi
  COEFSMB=0.067
  COEFMLT=0.284
elif [ $MODELA == 'CESM2' ]; then
  MEMBERA='r11i1p1f1'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  elif [  $SCENARB == 'ssp126' ]; then
    SCENARA='ssp126'
  elif [  $SCENARB == 'ssp245' ]; then
    SCENARA='ssp245'
  elif [  $SCENARB == 'ssp585' ]; then
    SCENARA='ssp585'
  fi
  COEFSMB=0.065
  COEFMLT=0.303
elif [ $MODELA == 'NorESM1-M' ]; then
  MEMBERA='r1i1p1'
  if [ $SCENARB == 'historical' ]; then
    SCENARA='historical'
  else
    SCENARA='rcp85'
  fi
  COEFSMB=0.080
  COEFMLT=0.358
fi

if [ $MODELB == 'CNRM-CM6-1' ]; then
  MEMBERB='r1i1p1f2'
elif [ $MODELB == 'UKESM1-0-LL' ]; then
  if [ $TOCKEN -eq 0 ]; then
    MEMBERB='r4i1p1f2'
  else
    MEMBERB='r1i1p1f2'
  fi
elif [ $MODELB == 'CESM2-WACCM' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'MRI-ESM2-0' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'NorESM2-MM' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'ACCESS-CM2' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'ACCESS-ESM1-5' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'CanESM5' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'CNRM-ESM2-1' ]; then
  MEMBERB='r1i1p1f2'
elif [ $MODELB == 'GISS-E2-1-H' ]; then
  MEMBERB='r1i1p1f2'
elif [ $MODELB == 'GFDL-CM4' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'GFDL-ESM4' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'CESM2' ]; then
  MEMBERB='r11i1p1f1'
elif [ $MODELB == 'MPI-ESM1-2-HR' ]; then
  MEMBERB='r1i1p1f1'
elif [ $MODELB == 'ACCESS1-3' ] || [ $MODELB == 'NorESM1-M' ]; then
  MEMBERB='r1i1p1'
  if [ $SCENARB == 'ssp126' ]; then
    SCENARB2='rcp26'
  elif [ $SCENARB == 'ssp245' ]; then
    SCENARB2='rcp45'
  elif [ $SCENARB == 'ssp585' ]; then
    SCENARB2='rcp85'
  fi
fi

if [ $MODELB == 'GFDL-CM4' ] && [ $SCENARB == 'ssp126' ]; then
  echo "Skip $MODELB $SCENARB (not available)"
else
  echo "$MODELA $MEMBERA $SCENARA  -->  $MODELB $MEMBERB ${SCENARB2}"
  rm -f yld.f90
  sed -e "s/<modelA>/$MODELA/g ; s/<memberA>/$MEMBERA/g ; s/<scenarA>/$SCENARA/g ; s/<modelB>/$MODELB/g ; s/<memberB>/$MEMBERB/g ; s/<scenarB>/${SCENARB2}/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g" calculate_extended_RCM_data_from_model_to_other_model.f90 > yld.f90
  ifort -c $NC_INC yld.f90
  ifort -o yld yld.o $NC_LIB
  ./yld
fi

done # SCENARB

if [ $MODELB == 'UKESM1-0-LL' ]; then
  echo 'switching to other UKESM member'
  TOCKEN=1
fi

done # MODELB
done # MODELA

rm -f yld.f90 yld.o yld

date

