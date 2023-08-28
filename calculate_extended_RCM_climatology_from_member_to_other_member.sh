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

MODEL='IPSL-CM6A-LR'
MEMBERA='r1i1p1f1'
COEFSMB=0.069
COEFMLT=0.330

for MEMBERB in 'r2i1p1f1' 'r3i1p1f1' 'r4i1p1f1' 'r5i1p1f1' 'r6i1p1f1' 'r10i1p1f1' 'r11i1p1f1' 'r14i1p1f1' 'r22i1p1f1' 'r25i1p1f1'; do

echo "$MODEL    $MEMBERA --> $MEMBERB"

rm -f lsk.f90
sed -e "s/<model>/$MODEL/g ; s/<memberA>/$MEMBERA/g ; s/<memberB>/$MEMBERB/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g" calculate_extended_RCM_climatology_from_member_to_other_member.f90 > lsk.f90
ifort -c $NC_INC lsk.f90
ifort -o lsk lsk.o $NC_LIB
./lsk

for VAR in smb ru me; do
 file="/scratchu/njourdain/RCMs_PROTECT/MAR-${MODEL}-${MEMBERB}_${VAR}_1995-2014_clim_regrid_04000m_FROM_${MODEL}-${MEMBERA}.nc"
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

done

rm -f lsk.f90 lsk lsk.o

date
