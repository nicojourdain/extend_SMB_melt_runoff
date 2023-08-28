program modif
 
USE netcdf
 
IMPLICIT NONE

INTEGER :: fidinXXX, status, dimID_x, dimID_y, dimID_time, mx, my, mtime, time_ID, asmb_ID, fidoutSMB, &
&          fidinTcmip, dimID_time_ext, mtime_ext, time_ext_ID, tas_ID, ame_ID, aru_ID, dimID_tb,       &
&          Tref_ID, mtime_Tref, fidoutMLT, fidoutRUN, ki, kj, kt, ktref, state_size,          &
&          asmb_err_ID, ame_err_ID, aru_err_ID

INTEGER,ALLOCATABLE,DIMENSION(:) :: state
 
CHARACTER(LEN=150) :: file_in_smb, file_in_me, file_in_ru, file_in_Tcmip,   &
&                     file_out_smb, file_out_me, file_out_ru, &
&                     file_in_smb_clim, file_in_me_clim, file_in_ru_clim

REAL*4 :: coefSMB, coefMLT, moa, zme, zsn, zru, ratio, maxmelt, rr
 
REAL*8,ALLOCATABLE,DIMENSION(:) :: time, time_ext

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: asmb_ref_1981_2000, ame_ref_1981_2000, aru_ref_1981_2000, tas_1981_2000

REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: asmb_ext, ame_ext, aru_ext

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: tas_anom, smb_clim, ru_clim, me_clim, tmp

!---------------------------------------
 
file_in_smb   = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_asmb_1980-2014_histo_regrid_04000m<suffix>.nc'
file_in_me    = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_ame_1980-2014_histo_regrid_04000m<suffix>.nc'
file_in_ru    = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_aru_1980-2014_histo_regrid_04000m<suffix>.nc'

file_in_smb_clim = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_smb_1995-2014_clim_regrid_04000m<suffix2>.nc'
file_in_me_clim  = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_me_1995-2014_clim_regrid_04000m<suffix2>.nc'
file_in_ru_clim  = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_ru_1995-2014_clim_regrid_04000m<suffix2>.nc'

file_in_Tcmip = '/data/njourdain/DATA_PROTECT/TAS/tas_Ayr_<gcm>_historical_<member>_1850_2014.nc'

file_out_smb  = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_asmb_1850-1979_histo_regrid_04000m<suffix>_EXTENDED.nc'
file_out_me  = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_ame_1850-1979_histo_regrid_04000m<suffix>_EXTENDED.nc'
file_out_ru  = '/scratchu/njourdain/RCMs_PROTECT/MAR-<gcm><xx>_aru_1850-1979_histo_regrid_04000m<suffix>_EXTENDED.nc'

coefSMB = <coefSMB>
coefMLT = <coefMLT>
moa = 0.60 ! melt over accumulation ratio
maxmelt = 1.80e-4 ! maximum melt rate (kg/m2/s) [99.99th percentile of 2080-2100 melt rates]
 
!---------------------------------------
! Read SMB anomaly from RCM over 2081-2100:
 
write(*,*) 'Reading ', TRIM(file_in_smb)
 
status = NF90_OPEN(TRIM(file_in_smb),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
status = NF90_INQ_DIMID(fidinXXX,"x",dimID_x); call erreur(status,.TRUE.,"inq_dimID_x")
status = NF90_INQ_DIMID(fidinXXX,"y",dimID_y); call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidinXXX,"time",dimID_time); call erreur(status,.TRUE.,"inq_dimID_time")
 
status = NF90_INQUIRE_DIMENSION(fidinXXX,dimID_x,len=mx); call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidinXXX,dimID_y,len=my); call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidinXXX,dimID_time,len=mtime); call erreur(status,.TRUE.,"inq_dim_time")
  
ALLOCATE(  time(mtime)  ) 
ALLOCATE(  asmb_ref_1981_2000(mx,my,20)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"time",time_ID); call erreur(status,.TRUE.,"inq_time_ID")
status = NF90_INQ_VARID(fidinXXX,"asmb",asmb_ID); call erreur(status,.TRUE.,"inq_asmb_ID")
 
status = NF90_GET_VAR(fidinXXX,time_ID,time); call erreur(status,.TRUE.,"getvar_time")
status = NF90_GET_VAR(fidinXXX,asmb_ID,asmb_ref_1981_2000,start=(/1,1,2/),count=(/mx,my,20/))
call erreur(status,.TRUE.,"getvar_asmb")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!---------------------------------------
! Read 2100 melt anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_me)
 
status = NF90_OPEN(TRIM(file_in_me),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  ame_ref_1981_2000(mx,my,20)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"ame",ame_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
status = NF90_GET_VAR(fidinXXX,ame_ID,ame_ref_1981_2000,start=(/1,1,2/),count=(/mx,my,20/))
call erreur(status,.TRUE.,"getvar_ame")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!---------------------------------------
! Read 2100 runoff anomaly from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_ru)
 
status = NF90_OPEN(TRIM(file_in_ru),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  aru_ref_1981_2000(mx,my,20)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"aru",aru_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
status = NF90_GET_VAR(fidinXXX,aru_ID,aru_ref_1981_2000,start=(/1,1,2/),count=(/mx,my,20/))
call erreur(status,.TRUE.,"getvar_aru")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!----------------------------------------
! Read 1995-2014 SMB climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_smb_clim)
 
status = NF90_OPEN(TRIM(file_in_smb_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  smb_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"smb",asmb_ID); call erreur(status,.TRUE.,"inq_asmb_ID")
 
status = NF90_GET_VAR(fidinXXX,asmb_ID,smb_clim)
call erreur(status,.TRUE.,"getvar_smb_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-----------------------------------------
! Read 1995-2014 melt climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_me_clim)
 
status = NF90_OPEN(TRIM(file_in_me_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  me_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"me",ame_ID); call erreur(status,.TRUE.,"inq_ame_ID")
 
status = NF90_GET_VAR(fidinXXX,ame_ID,me_clim)
call erreur(status,.TRUE.,"getvar_me_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!-------------------------------------------
! Read 1995-2014 runoff climatology from RCM :
 
write(*,*) 'Reading ', TRIM(file_in_ru_clim)
 
status = NF90_OPEN(TRIM(file_in_ru_clim),0,fidinXXX); call erreur(status,.TRUE.,"read")
 
ALLOCATE(  ru_clim(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinXXX,"ru",aru_ID); call erreur(status,.TRUE.,"inq_aru_ID")
 
status = NF90_GET_VAR(fidinXXX,aru_ID,ru_clim)
call erreur(status,.TRUE.,"getvar_ru_clim")
 
status = NF90_CLOSE(fidinXXX); call erreur(status,.TRUE.,"close_file")
 
!---------------------------------------------
! Read 1850-2014 CMIP surface air temperature :
 
write(*,*) 'Reading ', TRIM(file_in_Tcmip)
 
status = NF90_OPEN(TRIM(file_in_Tcmip),0,fidinTcmip); call erreur(status,.TRUE.,"read")
 
status = NF90_INQ_DIMID(fidinTcmip,"time",dimID_time_ext); call erreur(status,.TRUE.,"inq_dimID_time_ext")
 
!status = NF90_INQUIRE_DIMENSION(fidinTcmip,dimID_time_ext,len=mtime_ext); call erreur(status,.TRUE.,"inq_dim_time") 
mtime_ext=1979-1850+1

ALLOCATE(  time_ext(mtime_ext)  ) 
ALLOCATE(  tas_1981_2000(mx,my,20) )
ALLOCATE(  tas_anom(mx,my)  ) 
 
status = NF90_INQ_VARID(fidinTcmip,"time",time_ext_ID); call erreur(status,.TRUE.,"inq_time_ext_ID")
status = NF90_INQ_VARID(fidinTcmip,"tas",Tref_ID); call erreur(status,.TRUE.,"inq_Tref_ID")
status = NF90_INQ_VARID(fidinTcmip,"tas",tas_ID); call erreur(status,.TRUE.,"inq_tas_ID")
 
status = NF90_GET_VAR(fidinTcmip,time_ext_ID,time_ext,start=(/1/),count=(/mtime_ext/)); call erreur(status,.TRUE.,"getvar_time")
status = NF90_GET_VAR(fidinTcmip,Tref_ID,tas_1981_2000,start=(/1,1,1981-1850+1/),count=(/mx,my,20/)); call erreur(status,.TRUE.,"getvar_tas")

!---------------------------------------
! Writing extended SMB over 1850-1979 :
 
write(*,*) 'Creating ', TRIM(file_out_smb)
 
status = NF90_CREATE(TRIM(file_out_smb),NF90_NETCDF4,fidoutSMB); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutSMB,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutSMB,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutSMB,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutSMB,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutSMB,"asmb",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),asmb_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_asmb_ID")
status = NF90_DEF_VAR(fidoutSMB,"asmb_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),asmb_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_asmb_err_ID")
 
status = NF90_PUT_ATT(fidoutSMB,time_ID,"calendar","gregorian"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,time_ID,"units","days since 1850-01-01"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_ID,"long_name","surface mass balance anomaly"); call erreur(status,.TRUE.,"put_att_asmb_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
status = NF90_PUT_ATT(fidoutSMB,asmb_err_ID,"long_name","surface mass balance error (stddev)"); call erreur(status,.TRUE.,"put_att_asmb_err_ID")
 
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_back_to_1850.f90"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL1_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
status = NF90_PUT_ATT(fidoutSMB,NF90_GLOBAL,"member",'<member>');  call erreur(status,.TRUE.,"put_att_GLOBAL6_ID")
 
status = NF90_ENDDEF(fidoutSMB); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutSMB,time_ID,time_ext); call erreur(status,.TRUE.,"var_time_ID")

!---------------------------------------
! Writing extended MELT over 1850-1979 :
 
write(*,*) 'Creating ', TRIM(file_out_me)
 
status = NF90_CREATE(TRIM(file_out_me),NF90_NETCDF4,fidoutMLT); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutMLT,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutMLT,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutMLT,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutMLT,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutMLT,"ame",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),ame_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_ame_ID")
status = NF90_DEF_VAR(fidoutMLT,"ame_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),ame_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_ame_err_ID")
 
status = NF90_PUT_ATT(fidoutMLT,time_ID,"calendar","gregorian"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,time_ID,"units","days since 1850-01-01"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_ID,"long_name","surface melt rate anomaly"); call erreur(status,.TRUE.,"put_att_ame_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_ame_err_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_ame_err_ID")
status = NF90_PUT_ATT(fidoutMLT,ame_err_ID,"long_name","surface melt rate anomaly error (stddev)"); call erreur(status,.TRUE.,"put_att_ame_err_ID")
 
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_back_to_1850.f90"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
status = NF90_PUT_ATT(fidoutMLT,NF90_GLOBAL,"member",'<member>');  call erreur(status,.TRUE.,"put_att_GLOBAL6_ID")
 
status = NF90_ENDDEF(fidoutMLT); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutMLT,time_ID,time_ext); call erreur(status,.TRUE.,"var_time_ID")

!---------------------------------------
! Writing extended RUNOFF over 1850-1979 :
 
write(*,*) 'Creating ', TRIM(file_out_ru)
 
status = NF90_CREATE(TRIM(file_out_ru),NF90_NETCDF4,fidoutRUN); call erreur(status,.TRUE.,'create')
 
status = NF90_DEF_DIM(fidoutRUN,"x",mx,dimID_x); call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidoutRUN,"y",my,dimID_y); call erreur(status,.TRUE.,"def_dimID_y")
status = NF90_DEF_DIM(fidoutRUN,"time",NF90_UNLIMITED,dimID_time); call erreur(status,.TRUE.,"def_dimID_time")
  
status = NF90_DEF_VAR(fidoutRUN,"time",NF90_DOUBLE,(/dimID_time/),time_ID); call erreur(status,.TRUE.,"def_var_time_ID")
status = NF90_DEF_VAR(fidoutRUN,"aru",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),aru_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_aru_ID")
status = NF90_DEF_VAR(fidoutRUN,"aru_err",NF90_DOUBLE,(/dimID_x,dimID_y,dimID_time/),aru_err_ID,deflate_level=1); call erreur(status,.TRUE.,"def_var_aru_err_ID")
 
status = NF90_PUT_ATT(fidoutRUN,time_ID,"calendar","gregorian"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,time_ID,"axis","T"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,time_ID,"units","days since 1850-01-01"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,time_ID,"standard_name","time"); call erreur(status,.TRUE.,"put_att_time_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_ID,"long_name","runoff anomaly"); call erreur(status,.TRUE.,"put_att_aru_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"units","kg m-2 s-1"); call erreur(status,.TRUE.,"put_att_aru_err_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"missing_value",-99999.); call erreur(status,.TRUE.,"put_att_aru_err_ID")
status = NF90_PUT_ATT(fidoutRUN,aru_err_ID,"long_name","runoff anomaly error (stddev)"); call erreur(status,.TRUE.,"put_att_aru_err_ID")
 
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"history","Created by N. Jourdain using calculate_extended_RCM_data_back_to_1850.f90"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"project","EU-H2020-PROTECT"); call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefSMB",coefSMB);  call erreur(status,.TRUE.,"put_att_GLOBAL2_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"coefMLT",coefMLT);  call erreur(status,.TRUE.,"put_att_GLOBAL3_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"moa",moa);  call erreur(status,.TRUE.,"put_att_GLOBAL4_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"maxmelt",maxmelt);  call erreur(status,.TRUE.,"put_att_GLOBAL5_ID")
status = NF90_PUT_ATT(fidoutRUN,NF90_GLOBAL,"member",'<member>');  call erreur(status,.TRUE.,"put_att_GLOBAL6_ID")
 
status = NF90_ENDDEF(fidoutRUN); call erreur(status,.TRUE.,"fin_definition") 
 
status = NF90_PUT_VAR(fidoutRUN,time_ID,time_ext); call erreur(status,.TRUE.,"var_time_ID")

!---------------------------------------

ALLOCATE( asmb_ext(mx,my,20), ame_ext(mx,my,20), aru_ext(mx,my,20), tmp(mx,my) )

DO kt=1,mtime_ext

  DO ktref=1,20

    status = NF90_GET_VAR(fidinTcmip,tas_ID,tas_anom,start=(/1,1,kt/),count=(/mx,my,1/))
    call erreur(status,.TRUE.,"getvar_tas")
  
    do ki=1,mx
    do kj=1,my
  
      ! set zero anomaly where missing value
      if ( tas_1981_2000(ki,kj,ktref) .gt. 1.e2 .and.  tas_1981_2000(ki,kj,ktref) .lt. 350. ) then
        tas_anom(ki,kj) = tas_anom(ki,kj) - tas_1981_2000(ki,kj,ktref) ! ∆T with respect to 2100
      else
        tas_anom(ki,kj) = 0.0000000
      endif
  
      ! SMB anomaly (with respect to 1995-2014):
      ! NB: here anomaly of accumulation (SMB - RUNOFF) [NB: ru>0]
      asmb_ext(ki,kj,ktref) = (asmb_ref_1981_2000(ki,kj,ktref)+aru_ref_1981_2000(ki,kj,ktref)) * exp( coefSMB * tas_anom(ki,kj) )  &
      &                       + ( smb_clim(ki,kj)+ru_clim(ki,kj) ) * ( exp( coefSMB * tas_anom(ki,kj) ) - 1.e0 )
      asmb_ext(ki,kj,ktref) = max( asmb_ext(ki,kj,ktref), -smb_clim(ki,kj)-ru_clim(ki,kj) )  ! total accu >= 0 
  
      ! MELT anomaly (with respect to 1995-2014):
      ! NB: absolute melt rate should not exceed 2.65e-4 kg/m2/s (99.9th percentile of MAR until 2200)
      ame_ext(ki,kj,ktref) =  ame_ref_1981_2000(ki,kj,ktref) * exp( coefMLT * tas_anom(ki,kj) )  &
      &                 + me_clim(ki,kj) * ( exp( coefMLT * tas_anom(ki,kj) ) - 1.e0 ) 
      ame_ext(ki,kj,ktref) = min( ame_ext(ki,kj,ktref) , maxmelt - me_clim(ki,kj) )
      ame_ext(ki,kj,ktref) = max( ame_ext(ki,kj,ktref), -me_clim(ki,kj) ) ! total melt >= 0
  
      zme = ame_ext(ki,kj,ktref) + me_clim(ki,kj)
      zsn = asmb_ext(ki,kj,ktref) + smb_clim(ki,kj) + ru_clim(ki,kj) ! total accumulation (SMB - RUNOFF)
      ratio = zme/zsn
  
      ! runoff anomaly (with respect to 1995-2014):
      if ( ratio .ge. moa ) then 
        ! From this relationship : ru_clim(ki,kj)+aru_ext(ki,kj,ktref) = ame_ext(ki,kj,ktref)+me_clim (ki,kj) - moa* zsn, we get :
        aru_ext(ki,kj,ktref) = ame_ext(ki,kj,ktref) + me_clim (ki,kj) - moa*zsn - ru_clim(ki,kj)
      else
        aru_ext(ki,kj,ktref) = 0.e0
      endif
  
      asmb_ext(ki,kj,ktref) = asmb_ext(ki,kj,ktref) - aru_ext(ki,kj,ktref) ! Full SMB anomaly, i.e. accounting for both accumulation and runoff [NB: ru>0]
  
    enddo
    enddo

  ENDDO ! ktref
 
  ! -- mean --
 
  tmp = SUM(asmb_ext,DIM=3)/20
  status = NF90_PUT_VAR(fidoutSMB,asmb_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb")

  tmp = SUM(ame_ext,DIM=3)/20
  status = NF90_PUT_VAR(fidoutMLT,ame_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame")

  tmp = SUM(aru_ext,DIM=3)/20
  status = NF90_PUT_VAR(fidoutRUN,aru_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru")

  ! -- stddev --

  tmp = SQRT( SUM(asmb_ext**2,DIM=3)/20 - (SUM(asmb_ext,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutSMB,asmb_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_asmb_err") 

  tmp = SQRT( SUM(ame_ext**2,DIM=3)/20 - (SUM(ame_ext,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutMLT,ame_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_ame_err")

  tmp = SQRT( SUM(aru_ext**2,DIM=3)/20 - (SUM(aru_ext,DIM=3)/20)**2 )
  status = NF90_PUT_VAR(fidoutRUN,aru_err_ID,tmp,start=(/1,1,kt/),count=(/mx,my,1/))
  call erreur(status,.TRUE.,"putvar_aru_err")

ENDDO ! kt=1,mtime_ext

!---------------------------------------
status = NF90_CLOSE(fidinTcmip); call erreur(status,.TRUE.,"close_file")
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
