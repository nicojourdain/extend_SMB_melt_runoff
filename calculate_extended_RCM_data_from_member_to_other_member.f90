program modif

! script to estimate an pseudo MAR projection for modelB over 2015-2100
! based on the existing MAR projection for modelA.
!
! **NB** You first need to run calculate_extended_RCM_climatology_from_member_to_other_member.f90
!        to generate the extended climatology of modelB !!

USE netcdf
 
IMPLICIT NONE
 
INTEGER :: fidinXXX, status, dimID_x, dimID_y, dimID_time, mx, my, mtime, time_ID, asmb_ID, fidoutSMB, tas_modelB_ID,         & 
&          fidinTmodelB, dimID_time_ext_modelB, mtime_ext_modelB, time_ext_modelB_ID, tas_ID, ame_ID, aru_ID, dimID_tb,       &
&          fidinTmodelA, Tref_ID, mtime_Tref, fidoutMLT, fidoutRUN, ki, kj, kt, ame_modelA_ID, asmb_modelA_ID, aru_modelA_ID, &
&          tas_modelA_ID, fidinSMBmodelA, fidinRUmodelA, fidinMEmodelA, ktref, kr1, kr2, asmb_err_ID, ame_err_ID, aru_err_ID, &
&          fidinTmodelAhis, fidinTmodelBhis, tashis_modelA_ID, tashis_modelB_ID, lag

CHARACTER(LEN=20) :: rcm, modelA, modelB, memberA, memberB, scenarA, scenarB, scenarAx, scenarBx, periodRCM, periodGCM
 
CHARACTER(LEN=150) :: file_in_smb_modelA, file_in_me_modelA, file_in_ru_modelA, file_in_TmodelB, &
&                     file_out_smb_ext, file_out_me_ext, file_out_ru_ext, file_in_TmodelA,       &
&                     file_in_smb_modelA_clim, file_in_me_modelA_clim, file_in_ru_modelA_clim,   &
&                     file_in_smb_modelB_clim, file_in_me_modelB_clim, file_in_ru_modelB_clim,   &
&                     file_in_TmodelA_hist, file_in_TmodelB_hist, time_uni, time_cal

REAL*4 :: coefSMB, coefMLT, moa, zme, zsn, zru, ratio, deltaT, maxmelt, deltaTclim
 
REAL*8,ALLOCATABLE,DIMENSION(:) :: time, time_ext_modelB

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: asmb_anom_modelA, ame_anom_modelA, aru_anom_modelA, tas_modelB, tas_modelA, &
&                                    smb_climA, ru_climA, me_climA, tmp, tas_modelA_clim, tas_modelB_clim,       &
&                                    smb_climB, ru_climB, me_climB

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: asmb_ext_modelB, ame_ext_modelB, aru_ext_modelB, txp

LOGICAL :: ll_cal

!---------------------------------------

rcm='MAR'
modelA='<model>' ; memberA='<memberA>' ; scenarA='<scenarA>'
modelB='<model>' ; memberB='<memberB>' ; scenarB='<scenarB>'

if ( TRIM(scenarA) == 'historical' ) then
  scenarAx='histo'
  periodRCM='1980-2014'
  periodGCM='1850_2014'
  lag=1980-1850
else
  scenarAx=scenarA
  periodRCM='2015-2100'
  periodGCM='2015_2100'
  lag=0
endif

if ( TRIM(scenarB) == 'historical' ) then
  scenarBx='histo'
else
  scenarBx=scenarB
endif

! RCM anomalies for modelA
101 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_asmb_',a,'_',a,'_regrid_04000m.nc')
write(file_in_smb_modelA,101) TRIM(rcm), TRIM(modelA), TRIM(periodRCM), TRIM(scenarAx)
102 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_ame_',a,'_',a,'_regrid_04000m.nc')
write(file_in_me_modelA,102) TRIM(rcm), TRIM(modelA), TRIM(periodRCM), TRIM(scenarAx)
103 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_aru_',a,'_',a,'_regrid_04000m.nc')
write(file_in_ru_modelA,103) TRIM(rcm), TRIM(modelA), TRIM(periodRCM), TRIM(scenarAx)

! RCM climatology for modelA
104 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_smb_1995-2014_clim_regrid_04000m.nc')
write(file_in_smb_modelA_clim,104) TRIM(rcm), TRIM(modelA)
105 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_me_1995-2014_clim_regrid_04000m.nc')
write(file_in_me_modelA_clim,105) TRIM(rcm), TRIM(modelA)
106 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'_ru_1995-2014_clim_regrid_04000m.nc')
write(file_in_ru_modelA_clim,106) TRIM(rcm), TRIM(modelA)

! RCM climatology for modelB extended from modelA (generated from previous script)
114 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_smb_1995-2014_clim_regrid_04000m_FROM_',a,'-',a,'.nc')
write(file_in_smb_modelB_clim,114) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(modelA), TRIM(memberA)
115 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_me_1995-2014_clim_regrid_04000m_FROM_',a,'-',a,'.nc')
write(file_in_me_modelB_clim,115) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(modelA), TRIM(memberA)
116 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_ru_1995-2014_clim_regrid_04000m_FROM_',a,'-',a,'.nc')
write(file_in_ru_modelB_clim,116) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(modelA), TRIM(memberA)

! Surface air temperature in given scenario for related CMIP models
107 FORMAT('/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_',a,'_',a,'_',a,'_',a,'.nc')
write(file_in_TmodelA,107) TRIM(modelA), TRIM(scenarA), TRIM(memberA), TRIM(periodGCM)
108 FORMAT('/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_',a,'_',a,'_',a,'_',a,'.nc')
write(file_in_TmodelB,108) TRIM(modelB), TRIM(scenarB), TRIM(memberB), TRIM(periodGCM)

! Historical surface air temperature for related CMIP models (used to calculate climatology) :
109 FORMAT('/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_',a,'_historical_',a,'_1850_2014.nc')
write(file_in_TmodelA_hist,109) TRIM(modelA), TRIM(memberA)
110 FORMAT('/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_',a,'_historical_',a,'_1850_2014.nc')
write(file_in_TmodelB_hist,110) TRIM(modelB), TRIM(memberB)

! outputs:
121 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_asmb_',a,'_',a,'_regrid_04000m_FROM_',a,'-',a,'-',a,'.nc')
write(file_out_smb_ext,121) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(periodRCM), TRIM(scenarBx), TRIM(modelA), TRIM(memberA), TRIM(scenarAx)
122 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_ame_',a,'_',a,'_regrid_04000m_FROM_',a,'-',a,'-',a,'.nc')
write(file_out_me_ext,122) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(periodRCM), TRIM(scenarBx), TRIM(modelA), TRIM(memberA), TRIM(scenarAx)
123 FORMAT('/scratchu/njourdain/RCMs_PROTECT/',a,'-',a,'-',a,'_aru_',a,'_',a,'_regrid_04000m_FROM_',a,'-',a,'-',a,'.nc')
write(file_out_ru_ext,123) TRIM(rcm), TRIM(modelB), TRIM(memberB), TRIM(periodRCM), TRIM(scenarBx), TRIM(modelA), TRIM(memberA), TRIM(scenarAx)

! parameters obtained from fit on original MAR grid
! for modelA 1980-2100 w.r.t. 2040-2060.
coefSMB = <coefSMB>
coefMLT = <coefMLT>
moa = 0.60 ! melt over accumulation ratio
maxmelt = 1.80e-4 ! maximum melt rate (kg/m2/s) [99.99th percentile of 2080-2100 melt rates]
 
!-------------------------------------------
! Read modelA SMB anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_smb_modelA)
status = NF90_OPEN(TRIM(file_in_smb_modelA),0,fidinSMBmodelA); call erreur(status,.TRUE.,"read")
 
status = NF90_INQ_DIMID(fidinSMBmodelA,"x",dimID_x); call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidinSMBmodelA,"y",dimID_y); call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidinSMBmodelA,"time",dimID_time); call erreur(status,.TRUE.,"inq_dimID_time")
 
status = NF90_INQUIRE_DIMENSION(fidinSMBmodelA,dimID_x,len=mx); call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidinSMBmodelA,dimID_y,len=my); call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidinSMBmodelA,dimID_time,len=mtime); call erreur(status,.TRUE.,"inq_dim_time")
  
ALLOCATE(  time(mtime)  ) 
 
status = NF90_INQ_VARID(fidinSMBmodelA,"time",time_ID); call erreur(status,.TRUE.,"inq_time_ID")
status = NF90_INQ_VARID(fidinSMBmodelA,"asmb",asmb_modelA_ID); call erreur(status,.TRUE.,"inq_asmb_ID")

ll_cal=.false.
status = NF90_GET_ATT(fidinSMBmodelA,time_ID,"units",time_uni); call erreur(status,.TRUE.,"get_att_time1")
status = NF90_GET_ATT(fidinSMBmodelA,time_ID,"calendar",time_cal)
if ( status .eq. 0 ) ll_cal=.true.
 
status = NF90_GET_VAR(fidinSMBmodelA,time_ID,time); call erreur(status,.TRUE.,"getvar_time")
 
!---------------------------------------------
! Read modelA melt anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_me_modelA)
status = NF90_OPEN(TRIM(file_in_me_modelA),0,fidinMEmodelA); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinMEmodelA,"ame",ame_modelA_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
!---------------------------------------
! Read modelA runoff anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_ru_modelA)
status = NF90_OPEN(TRIM(file_in_ru_modelA),0,fidinRUmodelA); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinRUmodelA,"aru",aru_modelA_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
!----------------------------------------
! Read modelA 1995-2014 SMB climatology for RCM-modelA :
 
write(*,*) 'Reading ', TRIM(file_in_smb_modelA_clim)
 
status = NF90_OPEN(TRIM(file_in_smb_modelA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  smb_climA(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"smb",asmb_ID); call erreur(status,.TRUE.,"inq_asmb_ID")
 
status = NF90_GET_VAR(fidinXXX,asmb_ID,smb_climA)
call erreur(status,.TRUE.,"getvar_smb_climA")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-----------------------------------------
! Read modelA 1995-2014 melt climatology for RCM-modelA :
 
write(*,*) 'Reading ', TRIM(file_in_me_modelA_clim)
 
status = NF90_OPEN(TRIM(file_in_me_modelA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  me_climA(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"me",ame_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
status = NF90_GET_VAR(fidinXXX,ame_ID,me_climA)
call erreur(status,.TRUE.,"getvar_me_climA")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-------------------------------------------
! Read modelA 1995-2014 runoff climatology for RCM-modelA :
 
write(*,*) 'Reading ', TRIM(file_in_ru_modelA_clim)
 
status = NF90_OPEN(TRIM(file_in_ru_modelA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  ru_climA(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"ru",aru_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
status = NF90_GET_VAR(fidinXXX,aru_ID,ru_climA)
call erreur(status,.TRUE.,"getvar_ru_climA")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")

!----------------------------------------
! Read modelB 1995-2014 SMB climatology for RCM-modelB :
 
write(*,*) 'Reading ', TRIM(file_in_smb_modelB_clim)
 
status = NF90_OPEN(TRIM(file_in_smb_modelB_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  smb_climB(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"smb",asmb_ID); call erreur(status,.TRUE.,"inq_asmb_ID")
 
status = NF90_GET_VAR(fidinXXX,asmb_ID,smb_climB)
call erreur(status,.TRUE.,"getvar_smb_climB")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-----------------------------------------
! Read modelB 1995-2014 melt climatology for RCM-modelB :
 
write(*,*) 'Reading ', TRIM(file_in_me_modelB_clim)
 
status = NF90_OPEN(TRIM(file_in_me_modelB_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  me_climB(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"me",ame_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
status = NF90_GET_VAR(fidinXXX,ame_ID,me_climB)
call erreur(status,.TRUE.,"getvar_me_climB")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-------------------------------------------
! Read modelB 1995-2014 runoff climatology for RCM-modelB :
 
write(*,*) 'Reading ', TRIM(file_in_ru_modelB_clim)
 
status = NF90_OPEN(TRIM(file_in_ru_modelB_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  ru_climB(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"ru",aru_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
status = NF90_GET_VAR(fidinXXX,aru_ID,ru_climB)
call erreur(status,.TRUE.,"getvar_ru_climB")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")

!----------------------------------------------------------------------
! Read CMIP modelA climatology of surface air temperature (1995-2015):

ALLOCATE( txp(mx,my,20) )
ALLOCATE( tas_modelA_clim(mx,my) )
ALLOCATE( tas_modelB_clim(mx,my) )

write(*,*) 'Reading ', TRIM(file_in_TmodelA_hist)
 
status = NF90_OPEN(TRIM(file_in_TmodelA_hist),0,fidinTmodelAhis); call erreur(status,.TRUE.,"readxA")
status = NF90_INQ_VARID(fidinTmodelAhis,"tas",tashis_modelA_ID); call erreur(status,.TRUE.,"inq_TAhis_ID")
status = NF90_GET_VAR(fidinTmodelAhis,tashis_modelA_ID,txp,start=(/1,1,165-20+1/),count=(/mx,my,20/)); call erreur(status,.TRUE.,"getvar_tas_climA")
status = NF90_CLOSE(fidinTmodelAhis); call erreur(status,.TRUE.,"close_file_xA") 
tas_modelA_clim = SUM( txp, 3 ) / 20.
write(*,*) 'tas_modelA_clim = ', tas_modelA_clim(INT(mx/2),INT(my/2))

!----------------------------------------------------------------------
! Read CMIP modelB climatology of surface air temperature (1995-2015):

write(*,*) 'Reading ', TRIM(file_in_TmodelB_hist)

status = NF90_OPEN(TRIM(file_in_TmodelB_hist),0,fidinTmodelBhis); call erreur(status,.TRUE.,"readxB")
status = NF90_INQ_VARID(fidinTmodelBhis,"tas",tashis_modelB_ID); call erreur(status,.TRUE.,"inq_TBhis_ID")
status = NF90_GET_VAR(fidinTmodelBhis,tashis_modelB_ID,txp,start=(/1,1,165-20+1/),count=(/mx,my,20/)); call erreur(status,.TRUE.,"getvar_tas_climB")
status = NF90_CLOSE(fidinTmodelBhis); call erreur(status,.TRUE.,"close_file_xB")
tas_modelB_clim = SUM( txp, 3 ) / 20.
write(*,*) 'tas_modelB_clim = ', tas_modelB_clim(INT(mx/2),INT(my/2))
DEALLOCATE( txp )

!----------------------------------------------------------------
! Read CMIP modelA surface air temperature :

write(*,*) 'Reading ', TRIM(file_in_TmodelA)
 
status = NF90_OPEN(TRIM(file_in_TmodelA),0,fidinTmodelA); call erreur(status,.TRUE.,"read")

status = NF90_INQ_DIMID(fidinTmodelA,"time",dimID_tb); call erreur(status,.TRUE.,"inq_dimID_time")
status = NF90_INQUIRE_DIMENSION(fidinTmodelA,dimID_tb,len=mtime_Tref); call erreur(status,.TRUE.,"inq_dim_time")
 
status = NF90_INQ_VARID(fidinTmodelA,"tas",tas_modelA_ID); call erreur(status,.TRUE.,"inq_Tref_ID")
 
!---------------------------------------------------
! Read CMIP modelB surface air temperature :
 
write(*,*) 'Reading ', TRIM(file_in_TmodelB)
 
status = NF90_OPEN(TRIM(file_in_TmodelB),0,fidinTmodelB); call erreur(status,.TRUE.,"read")
 
status = NF90_INQ_DIMID(fidinTmodelB,"time",dimID_time_ext_modelB); call erreur(status,.TRUE.,"inq_dimID_time_ext_modelB")
 
status = NF90_INQUIRE_DIMENSION(fidinTmodelB,dimID_time_ext_modelB,len=mtime_ext_modelB); call erreur(status,.TRUE.,"inq_dim_time")
  
ALLOCATE(  time_ext_modelB(mtime_ext_modelB)  ) 
 
status = NF90_INQ_VARID(fidinTmodelB,"time",time_ext_modelB_ID); call erreur(status,.TRUE.,"inq_time_ext_modelB_ID")
status = NF90_INQ_VARID(fidinTmodelB,"tas",tas_modelB_ID); call erreur(status,.TRUE.,"inq_tas_modelB_ID")
 
status = NF90_GET_VAR(fidinTmodelB,time_ext_modelB_ID,time_ext_modelB); call erreur(status,.TRUE.,"getvar_time")

!-----------------------------------------------
! Writing reconstructed SMB for modelB :
 
write(*,*) 'Creating ', TRIM(file_out_smb_ext)
 
status = NF90_CREATE(TRIM(file_out_smb_ext),NF90_NETCDF4,fidoutSMB); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutSMB,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutSMB,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutSMB,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutSMB,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutSMB,"asmb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),asmb_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_asmb_ID")
status = NF90_DEF_VAR(fidoutSMB,"asmb_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),asmb_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_asmb_err_ID")
 
if ( ll_cal ) status = NF90_PUT_ATT(fidoutSMB,time_ID,"calendar",TRIM(time_cal))
status = NF90_PUT_ATT(fidoutSMB,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,time_ID,"units",TRIM(time_uni)); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"long_name","surface mass balance anomaly"); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"long_name","surface mass balance anomaly error (stddev)"); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
 
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_member_to_other_member.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutSMB); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutSMB,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!------------------------------------------------
! Writing reconstructed MELT for modelB :
 
write(*,*) 'Creating ', TRIM(file_out_me_ext)
 
status = NF90_CREATE(TRIM(file_out_me_ext),NF90_NETCDF4,fidoutMLT); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutMLT,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutMLT,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutMLT,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutMLT,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutMLT,"ame",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),ame_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_ame_ID")
status = NF90_DEF_VAR(fidoutMLT,"ame_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),ame_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_ame_err_ID")
 
if ( ll_cal ) status = NF90_PUT_ATT(fidoutMLT,time_ID,"calendar",TRIM(time_cal))
status = NF90_PUT_ATT(fidoutMLT,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,time_ID,"units",TRIM(time_uni)); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"long_name","surface melt rate anomaly"); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_ame_err_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_ame_err_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"long_name","surface melt rate anomaly error (stddev)"); call erreur(status,.TRUE.,"put_att_ame_err_ID")
 
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_member_to_other_member.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutMLT); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutMLT,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!--------------------------------------------------
! Writing reconstructed RUNOFF for modelB :
 
write(*,*) 'Creating ', TRIM(file_out_ru_ext)
 
status = NF90_CREATE(TRIM(file_out_ru_ext),NF90_NETCDF4,fidoutRUN); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutRUN,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutRUN,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutRUN,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutRUN,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutRUN,"aru",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),aru_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_aru_ID")
status = NF90_DEF_VAR(fidoutRUN,"aru_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),aru_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_aru_err_ID")
 
if ( ll_cal ) status = NF90_PUT_ATT(fidoutRUN,time_ID,"calendar",TRIM(time_cal))
status = NF90_PUT_ATT(fidoutRUN,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,time_ID,"units",TRIM(time_uni)); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"long_name","runoff anomaly"); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_aru_err_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_aru_err_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"long_name","runoff anomaly error (stddev)"); call erreur(status,.TRUE.,"put_att_aru_err_ID")
 
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_member_to_other_member.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutRUN); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutRUN,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!---------------------------------------

ALLOCATE( tas_modelB(mx,my), tas_modelA(mx,my), tmp(mx,my) )
ALLOCATE( asmb_anom_modelA(mx,my), ame_anom_modelA(mx,my), aru_anom_modelA(mx,my)  ) 
ALLOCATE( asmb_ext_modelB(mx,my,20), ame_ext_modelB(mx,my,20), aru_ext_modelB(mx,my,20) )

DO kt=1,mtime

  kr1 = MAX(1,kt-10)
  kr2 = kr1+19
  if ( kr2 .gt. mtime ) then
     kr2 = mtime
     kr1 = kr2-19
  endif

  status = NF90_GET_VAR(fidinTmodelB,tas_modelB_ID,tas_modelB,start=(/1,1,kt+lag/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"getvar_tas_modelB")

  write(*,*) 'kt = ', kt, kr1, kr2

  DO ktref=kr1,kr2

    status = NF90_GET_VAR(fidinSMBmodelA,asmb_modelA_ID,asmb_anom_modelA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_asmb_modelA")
    status = NF90_GET_VAR(fidinMEmodelA,ame_modelA_ID,ame_anom_modelA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_ame_modelA")
    status = NF90_GET_VAR(fidinRUmodelA,aru_modelA_ID,aru_anom_modelA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_aru_modelA")
  
    status = NF90_GET_VAR(fidinTmodelA,tas_modelA_ID,tas_modelA,start=(/1,1,ktref+lag/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_tas_modelA")
  
    do ki=1,mx
    do kj=1,my
  
      ! set zero anomaly where missing value
      if (      tas_modelA(ki,kj) .gt. 1.e2 .and.  tas_modelA(ki,kj) .lt. 350. &
      &   .and. tas_modelB(ki,kj) .gt. 1.e2 .and.  tas_modelB(ki,kj) .lt. 350. ) then
        deltaT = tas_modelB(ki,kj) - tas_modelA(ki,kj)
      else
        deltaT = 0.0000000
      endif
  
      ! SMB anomaly (with respect to 1995-2014):
      ! NB: here anomaly of accumulation (SMB - RUNOFF) [NB: ru>0]
      ! snf_clim_B + snf_anom_B = ( snf_clim_A + snf_anom_A ) exp(a*deltaT)
      ! snf_clim_B = snf_clim_A exp(a*deltaTclim)
      asmb_ext_modelB(ki,kj,ktref-kr1+1) = (asmb_anom_modelA(ki,kj)+aru_anom_modelA(ki,kj)) * exp(coefSMB*deltaT)  &
      &                                   + ( smb_climA(ki,kj)+ru_climA(ki,kj) ) * exp(coefSMB*deltaT)             &
      &                                   - ( smb_climB(ki,kj)+ru_climB(ki,kj) )
      asmb_ext_modelB(ki,kj,ktref-kr1+1) = max( asmb_ext_modelB(ki,kj,ktref-kr1+1), -smb_climB(ki,kj)-ru_climB(ki,kj) )  ! total accu >= 0 
    
      ! MELT anomaly (with respect to 1995-2014):
      ! NB: absolute melt rate should not exceed 2.65e-4 kg/m2/s (99.9th percentile of MAR until 2200)
      ame_ext_modelB(ki,kj,ktref-kr1+1) =  ame_anom_modelA(ki,kj) * exp(coefMLT*deltaT)  &
      &                                  + me_climA(ki,kj) * exp(coefMLT*deltaT) - me_climB(ki,kj)
      ame_ext_modelB(ki,kj,ktref-kr1+1) = min( ame_ext_modelB(ki,kj,ktref-kr1+1) , maxmelt - me_climB(ki,kj) )
      ame_ext_modelB(ki,kj,ktref-kr1+1) = max( ame_ext_modelB(ki,kj,ktref-kr1+1), -me_climB(ki,kj) ) ! total melt >= 0
    
      zme = ame_ext_modelB(ki,kj,ktref-kr1+1) + me_climB(ki,kj)
      zsn = asmb_ext_modelB(ki,kj,ktref-kr1+1) + smb_climB(ki,kj) + ru_climB(ki,kj) ! total accumulation (SMB - RUNOFF)
      ratio = zme/zsn
    
      ! runoff anomaly (with respect to 1995-2014):
      if ( ratio .ge. moa ) then 
        ! From this relationship : ru_climA(ki,kj)+aru_ext_modelB(ki,kj,ktref-kr1+1) = ame_ext_modelB(ki,kj,ktref-kr1+1)+me_climA (ki,kj) - moa* zsn, we get :
        aru_ext_modelB(ki,kj,ktref-kr1+1) = ame_ext_modelB(ki,kj,ktref-kr1+1) + me_climB (ki,kj) - moa*zsn - ru_climB(ki,kj)
      else
        aru_ext_modelB(ki,kj,ktref-kr1+1) = 0.e0
      endif
  
      ! Full SMB anomaly, i.e. accounting for both accumulation and runoff [NB: ru>0]  
      asmb_ext_modelB(ki,kj,ktref-kr1+1) = asmb_ext_modelB(ki,kj,ktref-kr1+1) - aru_ext_modelB(ki,kj,ktref-kr1+1)
  
    enddo
    enddo
   
  ENDDO ! ktref

  ! -- mean --

  tmp = SUM(asmb_ext_modelB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutSMB,asmb_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb")

  tmp = SUM(ame_ext_modelB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutMLT,ame_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame")

  tmp = SUM(aru_ext_modelB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutRUN,aru_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru")

  ! -- stddev --

  tmp = SQRT( SUM(asmb_ext_modelB**2,DIM=3)/20 - (SUM(asmb_ext_modelB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutSMB,asmb_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb_err")

  tmp = SQRT( SUM(ame_ext_modelB**2,DIM=3)/20 - (SUM(ame_ext_modelB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutMLT,ame_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame_err")

  tmp = SQRT( SUM(aru_ext_modelB**2,DIM=3)/20 - (SUM(aru_ext_modelB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutRUN,aru_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru_err")

ENDDO ! kt=1,mtime

!---------------------------------------
status = NF90_CLOSE(fidinTmodelB); call erreur(status,.TRUE.,"close_file1")
status = NF90_CLOSE(fidinTmodelA); call erreur(status,.TRUE.,"close_file2")
status = NF90_CLOSE(fidoutSMB); call erreur(status,.TRUE.,"close smb")
status = NF90_CLOSE(fidoutMLT); call erreur(status,.TRUE.,"close melt")
status = NF90_CLOSE(fidoutRUN); call erreur(status,.TRUE.,"close runoff")


end program modif

!=========================================================

SUBROUTINE erreur(iret, lstop, chaine)
  ! pour les messages d'erreur
  USE netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
    WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERREUR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
