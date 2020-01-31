	INTEGER max_nc		! max number of ncols in dem map
	INTEGER max_nr		! max number of nrows in dem map
	INTEGER max_stn		! max number of all gauged weather stations
	INTEGER max_stno	! max number of all output stations
	INTEGER max_soilo	! max number of all output soil moisture grids
	INTEGER max_time	! max time steps of model input data 

	INTEGER max_sub		! max number of sub-catchments 
	INTEGER max_flow    ! max number of flow interval in a sub-catchment
	INTEGER max_grid    ! max number of grids in a flow-interval
	INTEGER max_irrg		! max number of irrigated grids for one flow interval
	INTEGER max_year	! max number of years in modeling
	
	INTEGER	ndvi_nc		! max number of ncols in NDVI map (After patched up)
	INTEGER ndvi_nr		! max number of nrows in NDVI map (After patched up)
	INTEGER ndvi_nt		! max number of Times in one NDVI file

	INTEGER fpar_nc		! max number of nrows in fpar map 
	INTEGER fpar_nr		! max number of nrows in one fpar file

	INTEGER LAI_nc		! max number of nrows in LAI map 
	INTEGER LAI_nr		! max number of nrows in one LAI file

	INTEGER nc_s			! max number of nrows in SIB2 vegetaion map 
	INTEGER nr_s			! max number of nrows in SIB2 vegetaion map
			
	real pi				! pi=3.141592653589793238462643

	parameter (max_stn=300,max_time=366)
	parameter (max_stno=30,max_soilo=30)
	parameter (max_nc=221,max_nr=126)	
c In Yellow River (before HuaYuanKou station, 3 level)
c max_sub=481, max_flow=23, max_grid=25	
	parameter (max_sub=500, max_flow=30, max_grid=30,max_year=50)
c max_irrg=87	
	parameter (max_irrg=90)
	parameter (pi=3.141592653589793238462643)
	 
	parameter (ndvi_nc = 400, ndvi_nr = 300, ndvi_nt = 31)	
	parameter (fpar_nc = 1000, fpar_nr = 400)		
	parameter (LAI_nc = 1000, LAI_nr = 400)			
	parameter (nc_s=2630,nr_s=1820)

c icho1 - file channel for SIB_DHM system running information
c icho2 - file channel for SiB2 model warning information
c icho3 - file channel for SIB_DHM system warning information
	INTEGER icho1,icho2,icho3,icho4,icho5,icho6
	common icho1,icho2,icho3,icho4,icho5,icho6

c icho32 - file channel for output check information
	INTEGER icho32,icho33,icho34
	common icho32,icho33,icho34

c file channel for output
	INTEGER inpt_lai,inpt_fpar,inpt_tm,inpt_tmax,inpt_min
	INTEGER inpt_um,inpt_et,inpt_rsum,inpt_sun,inpt_fsm
	common inpt_lai,inpt_fpar,inpt_tm,inpt_tmax,inpt_min
	common inpt_um,inpt_et,inpt_rsum,inpt_sun,inpt_fsm

c control parameters
c re6000: reconstruct 1960-2000 (with change)
c nc_cl: no climate change; (=0, no change, other, change)
c nc_ia: no irrigated area change; (=0, no change, other, change)
c nc_vg: no vegetation change; (=0, no change, other, change)
	integer nc_cl,nc_ia,nc_vg
	common nc_cl,nc_ia,nc_vg