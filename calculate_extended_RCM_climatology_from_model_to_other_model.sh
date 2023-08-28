#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=32000
#SBATCH --threads-per-core=1
#SBATCH -J extended_RCM
#SBATCH -e extended_RCM.e%j
#SBATCH -o extended_RCM.o%j
#SBATCH --time=02:59:00
ulimit -s unlimited

date

echo " "
echo "========================================================================="
echo "Calculating SMB, melt and runoff climatologies from other RCM simulations"
echo "========================================================================="
echo " "

for MODELA in 'UKESM1-0-LL' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'ACCESS1-3' 'CNRM-CM6-1' 'CESM2' 'NorESM1-M'; do

TOCKEN=0

# Two times UKESM1-0-LL because two scenarios :
for MODELB in 'CNRM-CM6-1' 'UKESM1-0-LL' 'UKESM1-0-LL' 'CESM2-WACCM' 'MRI-ESM2-0' 'NorESM2-MM' 'ACCESS-CM2' 'ACCESS-ESM1-5' 'CanESM5' 'CNRM-ESM2-1' 'GISS-E2-1-H' 'GFDL-CM4' 'GFDL-ESM4' 'CESM2' 'MPI-ESM1-2-HR' 'ACCESS1-3' 'INM-CM5-0'; do

if [ $MODELA == 'UKESM1-0-LL' ]; then
  MEMBERA='r1i1p1f2'
  COEFSMB=0.065
  COEFMLT=0.313
elif [ $MODELA == 'IPSL-CM6A-LR' ]; then
  MEMBERA='r1i1p1f1'
  COEFSMB=0.069
  COEFMLT=0.330
elif [ $MODELA == 'MPI-ESM1-2-HR' ]; then
  MEMBERA='r1i1p1f1'
  COEFSMB=0.071
  COEFMLT=0.335
elif [ $MODELA == 'ACCESS1-3' ]; then
  MEMBERA='r1i1p1'
  COEFSMB=0.058
  COEFMLT=0.320
elif [ $MODELA == 'CNRM-CM6-1' ]; then
  MEMBERA='r1i1p1f2'
  COEFSMB=0.067
  COEFMLT=0.284
elif [ $MODELA == 'CESM2' ]; then
  MEMBERA='r11i1p1f1'
  COEFSMB=0.065
  COEFMLT=0.303
elif [ $MODELA == 'NorESM1-M' ]; then
  MEMBERA='r1i1p1'
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
elif [ $MODELB == 'ACCESS1-3' ]; then
  MEMBERB='r1i1p1'
fi

echo "$MODELA $MEMBERA  -->  $MODELB $MEMBERB"

rm -f kgf.f90
sed -e "s/<modelA>/$MODELA/g ; s/<memberA>/$MEMBERA/g ; s/<modelB>/$MODELB/g ; s/<memberB>/$MEMBERB/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g" calculate_extended_RCM_climatology_from_model_to_other_model.f90 > kgf.f90
ifort -c $NC_INC kgf.f90
ifort -o kgf kgf.o $NC_LIB
./kgf

for VAR in smb ru me; do
 file="/scratchu/njourdain/RCMs_PROTECT/MAR-${MODELB}-${MEMBERB}_${VAR}_1995-2014_clim_regrid_04000m_FROM_${MODELA}-${MEMBERA}.nc"
 if [ -f $file ]; then
   ncra -O -F -d time,1,20 $file $file
   ncwa -O -F -a time,1 $file $file
   ncks -O -x -v time $file $file
 else
   echo "~!@#$%^&* PROBLEM WITH FILE $file"
   echo "  >>>>>> STOP !!"
   exit
 fi
done

if [ $MODELB == 'UKESM1-0-LL' ]; then
  echo 'switching to other UKESM member'
  TOCKEN=1
fi

done
done

rm -f kgf.f90 kgf kgf.o

date
