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

MODEL='IPSL-CM6A-LR'
MEMBERA='r1i1p1f1'

for MEMBERB in 'r2i1p1f1' 'r3i1p1f1' 'r4i1p1f1' 'r5i1p1f1' 'r6i1p1f1' 'r10i1p1f1' 'r11i1p1f1' 'r14i1p1f1' 'r22i1p1f1' 'r25i1p1f1'; do

  echo "$MODEL    $MEMBERA --> $MEMBERB"

  for SCENARB in 'historical' 'ssp245'; do

    if [ $SCENARB == 'historical' ]; then
      SCENARA='historical'
    else
      SCENARA='ssp585'
    fi
    COEFSMB=0.069
    COEFMLT=0.330

    SCENARB2=$SCENARB

    rm -f ldy.f90
    sed -e "s/<model>/$MODEL/g ; s/<memberA>/$MEMBERA/g ; s/<scenarA>/$SCENARA/g ; s/<memberB>/$MEMBERB/g ; s/<scenarB>/${SCENARB2}/g ; s/<coefSMB>/$COEFSMB/g ; s/<coefMLT>/$COEFMLT/g" calculate_extended_RCM_data_from_member_to_other_member.f90 > ldy.f90
    ifort -c $NC_INC ldy.f90
    ifort -o ldy ldy.o $NC_LIB
    ./ldy

  done

done

rm -f ldy.f90 ldy.o ldy

date

