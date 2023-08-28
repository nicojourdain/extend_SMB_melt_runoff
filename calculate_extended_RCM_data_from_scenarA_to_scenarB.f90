program modif

! script to estimate an pseudo MAR projection for scenarB over 2015-2100
! based on the existing MAR projection for scenarA over the same period.

USE netcdf
 
IMPLICIT NONE
 
INTEGER :: fidinXXX, status, dimID_x, dimID_y, dimID_time, mx, my, mtime, time_ID, asmb_ID, fidoutSMB, tas_scenarB_ID,         & 
&          fidinTscenarB, tas_ID, ame_ID, aru_ID, dimID_tb,       &
&          fidinTscenarA, Tref_ID, fidoutMLT, fidoutRUN, ki, kj, kt, ame_scenarA_ID, asmb_scenarA_ID, aru_scenarA_ID, &
&          tas_scenarA_ID, fidinSMBscenarA, fidinRUscenarA, fidinMEscenarA, ktref, kr1, kr2, asmb_err_ID, ame_err_ID, aru_err_ID
 
CHARACTER(LEN=150) :: file_in_smb_scenarA, file_in_me_scenarA, file_in_ru_scenarA, file_in_TscenarB,   &
&                     file_out_smb_ext, file_out_me_ext, file_out_ru_ext, file_in_TscenarA, time_uni,  &
&                     file_in_smb_scenarA_clim, file_in_me_scenarA_clim, file_in_ru_scenarA_clim, time_cal

REAL*4 :: coefSMB, coefMLT, moa, zme, zsn, zru, ratio, deltaT, maxmelt
 
REAL*8,ALLOCATABLE,DIMENSION(:) :: time

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: asmb_anom_scenarA, ame_anom_scenarA, aru_anom_scenarA, tas_scenarB, tas_scenarA, &
&                                    smb_clim, ru_clim, me_clim, tmp

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: asmb_ext_scenarB, ame_ext_scenarB, aru_ext_scenarB

LOGICAL:: ll_cal

!---------------------------------------

file_in_smb_scenarA   = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_asmb_<period_rcm>_<scenarA>_regrid_04000m.nc'
file_in_me_scenarA    = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_ame_<period_rcm>_<scenarA>_regrid_04000m.nc'
file_in_ru_scenarA    = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_aru_<period_rcm>_<scenarA>_regrid_04000m.nc'

file_in_smb_scenarA_clim = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_smb_1995-2014_clim_regrid_04000m.nc'
file_in_me_scenarA_clim  = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_me_1995-2014_clim_regrid_04000m.nc'
file_in_ru_scenarA_clim  = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_ru_1995-2014_clim_regrid_04000m.nc'

file_in_TscenarB = '/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_<gcm>_<scenarB>_<member>_<period_gcm>.nc'
file_in_TscenarA  = '/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_<gcm>_<scenarA>_<member>_<period_gcm>.nc'

file_out_smb_ext  = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_asmb_<period_rcm>_<scenarB>_regrid_04000m_FROM_<scenarA>.nc'
file_out_me_ext  = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_ame_<period_rcm>_<scenarB>_regrid_04000m_FROM_<scenarA>.nc'
file_out_ru_ext  = '/scratchu/njourdain/RCMs_PROTECT/<rcm>-<gcm>_aru_<period_rcm>_<scenarB>_regrid_04000m_FROM_<scenarA>.nc'

! parameters obtained from fit on original MAR grid
! for scenarA 1980-2100 w.r.t. 2040-2060.
coefSMB = <coefSMB>
coefMLT = <coefMLT>
moa = 0.60 ! melt over accumulation ratio
maxmelt = 1.80e-4 ! maximum melt rate (kg/m2/s) [99.99th percentile of 2080-2100 melt rates]
 
!-------------------------------------------
! Read scenarA 2041-2060 mean SMB anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_smb_scenarA)
status = NF90_OPEN(TRIM(file_in_smb_scenarA),0,fidinSMBscenarA); call erreur(status,.TRUE.,"read")
 
status = NF90_INQ_DIMID(fidinSMBscenarA,"x",dimID_x); call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidinSMBscenarA,"y",dimID_y); call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidinSMBscenarA,"time",dimID_time); call erreur(status,.TRUE.,"inq_dimID_time")
 
status = NF90_INQUIRE_DIMENSION(fidinSMBscenarA,dimID_x,len=mx); call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidinSMBscenarA,dimID_y,len=my); call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidinSMBscenarA,dimID_time,len=mtime); call erreur(status,.TRUE.,"inq_dim_time")
  
ALLOCATE(  time(mtime)  ) 
 
status = NF90_INQ_VARID(fidinSMBscenarA,"time",time_ID); call erreur(status,.TRUE.,"inq_time_ID")
status = NF90_INQ_VARID(fidinSMBscenarA,"asmb",asmb_scenarA_ID); call erreur(status,.TRUE.,"inq_asmb_ID")

ll_cal=.false.
status = NF90_GET_ATT(fidinSMBscenarA,time_ID,"units",time_uni); call erreur(status,.TRUE.,"get_att_time1")
status = NF90_GET_ATT(fidinSMBscenarA,time_ID,"calendar",time_cal)
if ( status .eq. 0 ) ll_cal=.true.
 
status = NF90_GET_VAR(fidinSMBscenarA,time_ID,time); call erreur(status,.TRUE.,"getvar_time")
 
!---------------------------------------------
! Read scenarA 2041-2060 melt anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_me_scenarA)
status = NF90_OPEN(TRIM(file_in_me_scenarA),0,fidinMEscenarA); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinMEscenarA,"ame",ame_scenarA_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
!---------------------------------------
! Read scenarA 2041-2060 runoff anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_ru_scenarA)
status = NF90_OPEN(TRIM(file_in_ru_scenarA),0,fidinRUscenarA); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinRUscenarA,"aru",aru_scenarA_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
!----------------------------------------
! Read 1995-2014 SMB climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_smb_scenarA_clim)
 
status = NF90_OPEN(TRIM(file_in_smb_scenarA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  smb_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"smb",asmb_ID); call erreur(status,.TRUE.,"inq_asmb_ID")
 
status = NF90_GET_VAR(fidinXXX,asmb_ID,smb_clim)
call erreur(status,.TRUE.,"getvar_smb_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-----------------------------------------
! Read 1995-2014 melt climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_me_scenarA_clim)
 
status = NF90_OPEN(TRIM(file_in_me_scenarA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  me_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"me",ame_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
status = NF90_GET_VAR(fidinXXX,ame_ID,me_clim)
call erreur(status,.TRUE.,"getvar_me_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-------------------------------------------
! Read 1995-2014 runoff climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_ru_scenarA_clim)
 
status = NF90_OPEN(TRIM(file_in_ru_scenarA_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  ru_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"ru",aru_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
status = NF90_GET_VAR(fidinXXX,aru_ID,ru_clim)
call erreur(status,.TRUE.,"getvar_ru_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!----------------------------------------------------------------
! Read scenarA 2041-2060 surface air temperature anomaly from CMIP :

write(*,*) 'Reading ', TRIM(file_in_TscenarA)
 
status = NF90_OPEN(TRIM(file_in_TscenarA),0,fidinTscenarA); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinTscenarA,"tas",tas_scenarA_ID); call erreur(status,.TRUE.,"inq_Tref_ID")
 
!---------------------------------------------------
! Read scenarB 2015-2100 CMIP surface air temperature :
 
write(*,*) 'Reading ', TRIM(file_in_TscenarB)
 
status = NF90_OPEN(TRIM(file_in_TscenarB),0,fidinTscenarB); call erreur(status,.TRUE.,"read")
status = NF90_INQ_VARID(fidinTscenarB,"tas",tas_scenarB_ID); call erreur(status,.TRUE.,"inq_tas_scenarB_ID")
 
!-----------------------------------------------
! Writing extended SMB for scenarB over 2015-2100 :
 
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
 
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_scenarA_to_scenarB.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutSMB); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutSMB,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!------------------------------------------------
! Writing extended MELT for scenarB over 2015-2100 :
 
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
 
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_scenarA_to_scenarB.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutMLT); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutMLT,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!--------------------------------------------------
! Writing extended RUNOFF for scenarB over 2015-2100 :
 
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
 
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_from_scenarA_to_scenarB.f90")
call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
 
status = NF90_ENDDEF(fidoutRUN); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutRUN,time_ID,time); call erreur(status,.TRUE.,"var_time_ID")

!---------------------------------------

ALLOCATE( tas_scenarB(mx,my), tas_scenarA(mx,my), tmp(mx,my) )
ALLOCATE( asmb_anom_scenarA(mx,my), ame_anom_scenarA(mx,my), aru_anom_scenarA(mx,my)  ) 
ALLOCATE( asmb_ext_scenarB(mx,my,20), ame_ext_scenarB(mx,my,20), aru_ext_scenarB(mx,my,20) )

DO kt=1,mtime

  kr1 = MAX(1,kt-10)
  kr2 = kr1+19
  if ( kr2 .gt. mtime ) then
     kr2 = mtime
     kr1 = kr2-19
  endif

  status = NF90_GET_VAR(fidinTscenarB,tas_scenarB_ID,tas_scenarB,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"getvar_tas_scenarB")

  write(*,*) 'kt = ', kt, kr1, kr2

  DO ktref=kr1,kr2

    status = NF90_GET_VAR(fidinSMBscenarA,asmb_scenarA_ID,asmb_anom_scenarA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_asmb_scenarA")
    status = NF90_GET_VAR(fidinMEscenarA,ame_scenarA_ID,ame_anom_scenarA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_ame_scenarA")
    status = NF90_GET_VAR(fidinRUscenarA,aru_scenarA_ID,aru_anom_scenarA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_aru_scenarA")
  
    status = NF90_GET_VAR(fidinTscenarA,tas_scenarA_ID,tas_scenarA,start=(/1,1,ktref/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_tas_scenarA")
  
    do ki=1,mx
    do kj=1,my
  
      ! deltaT = tas_scenarB - tas_scenarA
      ! set zero anomaly where missing value
      if ( tas_scenarA(ki,kj) .gt. 1.e2 .and.  tas_scenarA(ki,kj) .lt. 350. ) then
        deltaT = tas_scenarB(ki,kj) - tas_scenarA(ki,kj)
      else
        deltaT = 0.0000000
      endif
  
      ! SMB anomaly (with respect to 1995-2014):
      ! NB: here anomaly of accumulation (SMB - RUNOFF) [NB: ru>0]
      asmb_ext_scenarB(ki,kj,ktref-kr1+1) = (asmb_anom_scenarA(ki,kj)+aru_anom_scenarA(ki,kj)) * exp( coefSMB * deltaT )  &
      &                                   + ( smb_clim(ki,kj)+ru_clim(ki,kj) ) * ( exp( coefSMB * deltaT ) - 1.e0 )
      asmb_ext_scenarB(ki,kj,ktref-kr1+1) = max( asmb_ext_scenarB(ki,kj,ktref-kr1+1), -smb_clim(ki,kj)-ru_clim(ki,kj) )  ! total accu >= 0 
    
      ! MELT anomaly (with respect to 1995-2014):
      ! NB: absolute melt rate should not exceed 2.65e-4 kg/m2/s (99.9th percentile of MAR until 2200)
      ame_ext_scenarB(ki,kj,ktref-kr1+1) =  ame_anom_scenarA(ki,kj) * exp( coefMLT * deltaT )  &
      &                                  + me_clim(ki,kj) * ( exp( coefMLT * deltaT ) - 1.e0 ) 
      ame_ext_scenarB(ki,kj,ktref-kr1+1) = min( ame_ext_scenarB(ki,kj,ktref-kr1+1) , maxmelt - me_clim(ki,kj) )
      ame_ext_scenarB(ki,kj,ktref-kr1+1) = max( ame_ext_scenarB(ki,kj,ktref-kr1+1), -me_clim(ki,kj) ) ! total melt >= 0
    
      zme = ame_ext_scenarB(ki,kj,ktref-kr1+1) + me_clim(ki,kj)
      zsn = asmb_ext_scenarB(ki,kj,ktref-kr1+1) + smb_clim(ki,kj) + ru_clim(ki,kj) ! total accumulation (SMB - RUNOFF)
      ratio = zme/zsn
    
      ! runoff anomaly (with respect to 1995-2014):
      if ( ratio .ge. moa ) then 
        ! From this relationship : ru_clim(ki,kj)+aru_ext_scenarB(ki,kj,ktref-kr1+1) = ame_ext_scenarB(ki,kj,ktref-kr1+1)+me_clim (ki,kj) - moa* zsn, we get :
        aru_ext_scenarB(ki,kj,ktref-kr1+1) = ame_ext_scenarB(ki,kj,ktref-kr1+1) + me_clim (ki,kj) - moa*zsn - ru_clim(ki,kj)
      else
        aru_ext_scenarB(ki,kj,ktref-kr1+1) = 0.e0
      endif
  
      ! Full SMB anomaly, i.e. accounting for both accumulation and runoff [NB: ru>0]  
      asmb_ext_scenarB(ki,kj,ktref-kr1+1) = asmb_ext_scenarB(ki,kj,ktref-kr1+1) - aru_ext_scenarB(ki,kj,ktref-kr1+1)
  
    enddo
    enddo
   
  ENDDO ! ktref

  ! -- mean --

  tmp = SUM(asmb_ext_scenarB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutSMB,asmb_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb")

  tmp = SUM(ame_ext_scenarB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutMLT,ame_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame")

  tmp = SUM(aru_ext_scenarB,DIM=3)/20
  status = NF90_PUT_VAR(fidoutRUN,aru_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru")

  ! -- stddev --

  tmp = SQRT( SUM(asmb_ext_scenarB**2,DIM=3)/20 - (SUM(asmb_ext_scenarB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutSMB,asmb_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb_err")

  tmp = SQRT( SUM(ame_ext_scenarB**2,DIM=3)/20 - (SUM(ame_ext_scenarB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutMLT,ame_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame_err")

  tmp = SQRT( SUM(aru_ext_scenarB**2,DIM=3)/20 - (SUM(aru_ext_scenarB,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutRUN,aru_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru_err")

ENDDO ! kt=1,mtime

!---------------------------------------
status = NF90_CLOSE(fidinTscenarB); call erreur(status,.TRUE.,"close_file1")
status = NF90_CLOSE(fidinTscenarA); call erreur(status,.TRUE.,"close_file2")
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
