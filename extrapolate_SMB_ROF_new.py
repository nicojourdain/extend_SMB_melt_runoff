import numpy as np
import xarray as xr
from scipy.optimize import curve_fit
import matplotlib
matplotlib.use('pdf')
import matplotlib.pyplot as plt
import os

# set scat=True to plot a classical scatter plot or =False to plot a PDF (long calculation)
scat=True

lastyr = '2100'  # '2100' or '2200' (string)

CMIPmod = 'CESM2'       # 'IPSL-CM6A-LR', 'CNRM-CM6-1', 'MPI-ESM1-2-HR' , 'UKESM1-0-LL', 'CESM2'
                        # 'ACCESS1-3' , 'NorESM1-M'

scenar = 'SSP585'  # 'SSP585' or 'RCP8.5'

# period over which the reference year is taken
#t1 =  99 # 2080
#t2 = 119 # 2100
t1 =  59 # 2040
t2 =  79 # 2060

#--------------------------------------------------------------------------------
fig, axs = plt.subplots(nrows=1,ncols=2,figsize=(21.0,7.0))

dir_in = '/data/ckan/storage/modelling_results/Forcing/AIS/Atmosphere/ORIGINAL/MARv3.11/'

grd = xr.open_dataset('MARgrid-35km_nico.nc')
msk = grd.MASK.where( (grd.MASK>50.),np.nan)

np.seterr(divide='ignore')

#--------------------------------------------------------------------------------
# a - SMB vs delta T

TEM_clim = dir_in+'T2m_AIS_'+CMIPmod+'_MAR-v3.11_climato_1995-2014.nc'
TEM_anom_list=[dir_in+'aT2m_AIS_'+CMIPmod+'_MAR-v3.11_histo_yearly_1980-2014.nc',dir_in+'aT2m_AIS_'+CMIPmod+'_MAR-v3.11_'+scenar+'_yearly_2015-'+lastyr+'.nc']
dsTEMclim = xr.open_dataset(TEM_clim)
dsTEManom = xr.open_mfdataset(TEM_anom_list,decode_times=False)

print('toto1')

SMB_clim = dir_in+'SMB_AIS_'+CMIPmod+'_MAR-v3.11_climato_1995-2014.nc'
SMB_anom_list=[dir_in+'aSMB_AIS_'+CMIPmod+'_MAR-v3.11_histo_yearly_1980-2014.nc',dir_in+'aSMB_AIS_'+CMIPmod+'_MAR-v3.11_'+scenar+'_yearly_2015-'+lastyr+'.nc']
dsSMBclim = xr.open_dataset(SMB_clim)
dsSMBanom = xr.open_mfdataset(SMB_anom_list,decode_times=False)
print('toto2')

ROF_clim = dir_in+'Runoff_AIS_'+CMIPmod+'_MAR-v3.11_climato_1995-2014.nc'
ROF_anom_list=[dir_in+'aRunoff_AIS_'+CMIPmod+'_MAR-v3.11_histo_yearly_1980-2014.nc',dir_in+'aRunoff_AIS_'+CMIPmod+'_MAR-v3.11_'+scenar+'_yearly_2015-'+lastyr+'.nc']
dsROFclim = xr.open_dataset(ROF_clim)
dsROFanom = xr.open_mfdataset(ROF_anom_list,decode_times=False)
print('toto3')

all_SMBmROF_fac=np.empty((0))
all_delta_TEM=np.empty((0))

for tref in np.arange(t1,t2):
   
   print(tref)

   delta_TEM=dsTEManom.t2m_anomaly - dsTEManom.t2m_anomaly.isel(time=tref)
   delta_TEM_vec = np.reshape(delta_TEM.values,delta_TEM.size)
   print(np.size(delta_TEM_vec))  
 
   SMBref = dsSMBanom.smb_anomaly.isel(time=tref) + dsSMBclim.smb_climatology
   SMB = dsSMBanom.smb_anomaly + dsSMBclim.smb_climatology
   print(np.size(SMB))  
   
   ROFref = dsROFanom.runoff_anomaly.isel(time=tref) + dsROFclim.runoff_climatology
   ROF = dsROFanom.runoff_anomaly + dsROFclim.runoff_climatology
   print(np.size(ROF))  
   
   # SMB >0 if accumulation, Runoff < 0 if mass loss
   SMBmROF = ( SMB - ROF ) * msk / msk
   SMBmROF_vec = np.reshape( SMBmROF.values, SMBmROF.size)
   
   SMBmROFref = ( SMBref + ROFref ) * msk/msk
   SMBmROFref_vec = np.reshape( SMBmROFref.values, SMBmROFref.size)
   
   SMBmROF_fac = SMBmROF / SMBmROFref
   SMBmROF_fac_vec = np.reshape( SMBmROF_fac.values, SMBmROF_fac.size)
  
   print(np.size(SMBmROF_vec), np.size(SMBmROF_fac_vec))
 
   # remove outliers and reference year 
   print(np.nansum((SMBmROF_vec>=np.nanpercentile(SMBmROF,5))))
   print(np.nansum((SMBmROF_vec<=np.nanpercentile(SMBmROF,95))))
   print(np.nansum((SMBmROF_fac_vec>=1.e-9)))
   print(np.nansum((SMBmROF_fac_vec>=np.nanpercentile(SMBmROF_fac_vec,5))))
   print(np.nansum((SMBmROF_fac_vec<=np.nanpercentile(SMBmROF_fac_vec,95))))
   print(np.nansum(( ~( (delta_TEM_vec==0.) & (SMBmROF_fac_vec==1.) ) )))
   print(np.nansum((~np.isinf(SMBmROF_fac_vec))))
   print(np.nansum((~np.isnan(SMBmROF_fac_vec))))
   cond =   (SMBmROF_vec>=np.nanpercentile(SMBmROF,5)) & (SMBmROF_vec<=np.nanpercentile(SMBmROF,95)) &(SMBmROF_fac_vec>=1.e-9) \
          & (SMBmROF_fac_vec>=np.nanpercentile(SMBmROF_fac_vec,5)) & (SMBmROF_fac_vec<=np.nanpercentile(SMBmROF_fac_vec,95)) \
          & ( ~( (delta_TEM_vec==0.) & (SMBmROF_fac_vec==1.) ) ) \
          & (~np.isinf(SMBmROF_fac_vec)) & (~np.isnan(SMBmROF_fac_vec))
   SMBmROF_fac_vec = SMBmROF_fac_vec[cond ]
   delta_TEM_vec = delta_TEM_vec[ cond ]

   all_delta_TEM = np.append(all_delta_TEM,delta_TEM_vec)
   all_SMBmROF_fac = np.append(all_SMBmROF_fac,SMBmROF_fac_vec)

# Fit the function y = exp(a*x) with x = TEMvec and y = SMBmROF_fac
def func(x, a):
     return np.exp(a * x)
popt, pcov = curve_fit(func, all_delta_TEM, all_SMBmROF_fac,p0=[0.07])
print(popt,np.sqrt(np.diag(pcov)))

TTmin=np.amin(all_delta_TEM)
TTmax=np.amax(all_delta_TEM)
deltaTT=(TTmax-TTmin)/300.
TT=np.arange(TTmin,TTmax+deltaTT,deltaTT)
  
RRmin=np.amin(all_SMBmROF_fac)
RRmax=np.amax(all_SMBmROF_fac)
deltaRR=(RRmax-RRmin)/302.
RR=np.arange(RRmin,RRmax+deltaRR,deltaRR)
 
# scatter plot or PDF :
if scat:

  axs[0].scatter(all_delta_TEM,all_SMBmROF_fac,s=10,c='red',marker='o',alpha=0.01)

else:
 
  if os.path.exists('PDF_SMB_'+CMIPmod+'_'+lastyr+'.npz'):
    print ('WARNING : using existing PDF_SMB_'+CMIPmod+'_'+lastyr+'.npz !')
    npzfile = np.load('PDF_SMB_'+CMIPmod+'_'+lastyr+'.npz')
    PDF_SMB = npzfile['PDF_SMB']
  else:
    NT=np.size(TT)
    NR=np.size(RR)
    PDF_SMB = np.zeros((NT,NR))
    TT2d = np.repeat(TT[:, np.newaxis], NR, axis=1)
    RR2d = np.repeat(RR[np.newaxis, :], NT, axis=0)
    sigT = (TTmax-TTmin)/100.
    sigR = (RRmax-RRmin)/100.
    for kk in np.arange(np.size(delta_TEM_vec)):
      PDF_SMB = PDF_SMB + np.exp( -0.5*( (TT2d-all_delta_TEM[kk])**2/sigT**2 + (RR2d-all_SMBmROF_fac[kk])**2/sigR**2 ) )
    PDF_SMB = PDF_SMB / (np.sum(PDF_SMB)*deltaTT*deltaRR)
    np.savez('PDF_SMB_'+CMIPmod+'_'+lastyr+'.npz',sigT=sigT,sigR=sigR,PDF_SMB=PDF_SMB)
  
  cax0=axs[0].contourf(TT,RR,np.transpose(PDF_SMB),12,cmap='YlOrBr')
  cbar0=fig.colorbar(cax0,ax=axs[0])
  cbar0.ax.set_title('PDF',size=14)
  cbar0.ax.tick_params(labelsize=14)

axs[0].plot([0,0],[RRmin,RRmax],color='k',linewidth=0.5)
axs[0].plot([-10,10],[1,1],color='k',linewidth=0.5)

Yf=func(TT,popt[0])
string=np.array2string(popt[0],precision=4,floatmode='fixed')
axs[0].plot(TT,Yf,linewidth=1.5,color='k',label=r'RCM fit : exp('+string+' $\Delta$T)')

Ycc=func(TT,0.072) #Clausius Clapeyron
axs[0].plot(TT,Ycc,linewidth=1.5,linestyle='--',color='k',label='Clausius-Clapeyron')

axs[0].set_title('(a) SMB - Runoff',fontsize=16,fontweight='bold')
axs[0].set_xlabel(r'$\Delta$ T ($^\circ$C)',fontsize=16)
axs[0].set_ylabel(r'Multiplicative factor',fontsize=16)
axs[0].set_yticks(np.arange(0.4,1.8,0.2))
axs[0].tick_params(axis='both', which='both', labelsize=14)
axs[0].set_xlim([-6,6])
axs[0].set_ylim([0.4,1.8])
axs[0].legend(loc='upper center',fontsize=14)

#--------------------------------------------------------------------------------
# b - melt vs delta T

MLT_clim = dir_in+'Melt_AIS_'+CMIPmod+'_MAR-v3.11_climato_1995-2014.nc'
MLT_anom_list=[dir_in+'aMelt_AIS_'+CMIPmod+'_MAR-v3.11_histo_yearly_1980-2014.nc',dir_in+'aMelt_AIS_'+CMIPmod+'_MAR-v3.11_'+scenar+'_yearly_2015-'+lastyr+'.nc']
dsMLTclim = xr.open_dataset(MLT_clim)
dsMLTanom = xr.open_mfdataset(MLT_anom_list,decode_times=False)
print('toto5')

all_MLT_fac=np.empty((0))
all_delta_TEMP=np.empty((0))

for tref in np.arange(t1,t2):

   delta_TEMP=dsTEManom.t2m_anomaly - dsTEManom.t2m_anomaly.isel(time=tref)
   delta_TEMP_vec = np.reshape(delta_TEMP.values,delta_TEMP.size)
   
   MLTref = ( dsMLTanom.melt_anomaly.isel(time=tref) + dsMLTclim.melt_climatology ) * msk/msk
   MLTref = MLTref.where( (MLTref>0.), np.nan)
   MLT = ( dsMLTanom.melt_anomaly + dsMLTclim.melt_climatology ) * msk/msk
   MLT_vec = np.reshape( MLT.values, MLT.size)
   MLTref_vec = np.reshape( MLTref.values, MLTref.size)
   
   MLT_fac = MLT / MLTref
   MLT_fac_vec = np.reshape( MLT_fac.values, MLT_fac.size)
   
   # remove points with no melt, outliers and reference year
   cond2 =   ( MLT_vec >= np.nanpercentile(MLT_vec,75) ) & (MLT_fac_vec > 0.001) & (MLT_fac_vec <= 10) \
           & ( ~( (MLT_fac_vec==1.) & (delta_TEMP_vec==0.) ) ) \
           & (~np.isnan(MLT_vec)) &  (~np.isnan(MLT_fac_vec)) & (~np.isinf(MLT_vec)) &  (~np.isinf(MLT_fac_vec))
   MLT_fac_vec = MLT_fac_vec[cond2]
   delta_TEMP_vec = delta_TEMP_vec[cond2]

   all_MLT_fac = np.append(all_MLT_fac,MLT_fac_vec)
   all_delta_TEMP = np.append(all_delta_TEMP,delta_TEMP_vec)

   #print(np.nanpercentile(MLT_vec,95),np.nanpercentile(MLT_vec,99),np.nanpercentile(MLT_vec,99.9))

ropt, rcov = curve_fit(func, all_delta_TEMP, all_MLT_fac,p0=[0.4])
print(ropt,np.sqrt(np.diag(rcov)))

XXmin=np.amin(all_delta_TEMP)
XXmax=np.amax(all_delta_TEMP)
deltaXX=(XXmax-XXmin)/300.
XX=np.arange(XXmin,XXmax+deltaXX,deltaXX)

MMmin=np.amin(all_MLT_fac)
MMmax=np.amax(all_MLT_fac)
deltaMM=(MMmax-MMmin)/302.
MM=np.arange(MMmin,MMmax+deltaMM,deltaMM)

if scat:

  axs[1].scatter(all_delta_TEMP,all_MLT_fac,s=10,c='red',marker='o',alpha=0.02)

else:
 
  if os.path.exists('PDF_MLT_'+CMIPmod+'_'+lastyr+'.npz'):
    print ('WARNING : using existing PDF_MLT.npz !')
    npzfile = np.load('PDF_MLT_'+CMIPmod+'_'+lastyr+'.npz')
    PDF_MLT = npzfile['PDF_MLT']
  else:
    NX=np.size(XX)
    NM=np.size(MM)
    PDF_MLT = np.zeros((NX,NM))
    XX2d = np.repeat(XX[:, np.newaxis], NM, axis=1)
    MM2d = np.repeat(MM[np.newaxis, :], NX, axis=0)
    sigX = (XXmax-XXmin)/100.
    sigM = (MMmax-MMmin)/100.
    for kk in np.arange(np.size(all_delta_TEMP)):
      PDF_MLT = PDF_MLT + np.exp( -0.5*( (XX2d-all_delta_TEMP[kk])**2/sigX**2 + (MM2d-all_MLT_fac[kk])**2/sigM**2 ) )
    PDF_MLT = PDF_MLT / (np.sum(PDF_MLT)*deltaXX*deltaMM)
    np.savez('PDF_MLT_'+CMIPmod+'_'+lastyr+'.npz',sigX=sigX,sigM=sigM,PDF_MLT=PDF_MLT)
  
  cax1=axs[1].contourf(XX,MM,np.transpose(PDF_MLT),12,cmap='YlOrBr')
  cbar1=fig.colorbar(cax1,ax=axs[1])
  cbar1.ax.set_title('PDF',size=14)
  cbar1.ax.tick_params(labelsize=14)

axs[1].plot([0,0],[-10,MMmax],color='k',linewidth=0.5)
axs[1].plot([-10,10],[1,1],color='k',linewidth=0.5)

Zf=func(XX,ropt[0])
string=np.array2string(ropt[0],precision=4,floatmode='fixed')
axs[1].plot(XX,Zf,linewidth=1.5,color='k',label=r'RCM fit : exp('+string+' $\Delta$T)')

axs[1].set_title('(b) Melt rate',fontsize=16,fontweight='bold')
axs[1].set_xlabel(r'$\Delta$ T ($^\circ$C)',fontsize=16)
axs[1].set_ylabel(r'Multiplicative factor',fontsize=16)
axs[1].set_yticks(np.arange(0,8,1))
axs[1].tick_params(axis='both', which='both', labelsize=14)
axs[1].set_xlim([-6,6])
axs[1].set_ylim([0,8])
axs[1].legend(loc='upper center',fontsize=14)

#--------------------------------------------------------------------------------
fig.savefig('fit_SMB_MLT_'+CMIPmod+'_'+lastyr+'.jpg')
fig.savefig('fit_SMB_MLT_'+CMIPmod+'_'+lastyr+'.pdf')
