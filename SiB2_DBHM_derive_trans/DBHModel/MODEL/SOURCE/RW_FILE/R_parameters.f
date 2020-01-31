c Read all the parameters in 'Input_para'
c Set them as common paramters
c Call this subroutine in the main programe
	subroutine Read_parameters(KER)
	implicit none
	integer	KER

	character*80	code_file,dis_file,dir_file,slope_file,dem_file
	character*80	area_file,frac_file,riverway_file,Outstn,Output_D
	character*80	Derive_D       
      integer	levels,startyear,endyear,startmont,startday,endmont,endday
	real			dx_max,dt,rivlen
	common			/Input_para/ levels,startyear,startmont,startday,
     $				endyear,endmont,endday,dx_max,
     $		code_file,dis_file,dir_file,slope_file,dem_file,area_file,
     $frac_file,riverway_file,Outstn,Output_D,Derive_D,dt,rivlen
	integer			ETID
	character*80	DEM_FD,FRAC_FD,ETPATH
	common			/Get_ET_para/ETID,DEM_FD,FRAC_FD,ETPATH

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	
	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1

	integer			SHOWTIP,NP,NPPR
	real			R0,CM
	character*80	stn_info_file,stn_data_dir
	common			/Read_foring_para/SHOWTIP,stn_info_file,
     $				stn_data_dir,NP,R0,CM,NPPR

	character*80	DAnciDir,DLat,DLon,DSuffix,D_dir,DPre
	integer			Col0,Col1,Row0,Row1,DN_div
	common			/Read_NDVI_para/DAnciDir,DLat,DLon,DSuffix,
     $				Col0,Col1,Row0,Row1,D_dir,DPre,DN_div

	character*80	TMAnciDir,TMLat,TMLon,Ten_dir,TenPre,M_dir,MPre
	integer			D_nc,D_nr,S_c,S_r,S_nc,S_nr,TMN_div
	common			/Read_NDVI_para_TenDM/TMAnciDir,TMLat,TMLon,
     $				D_nc,D_nr,S_c,S_r,S_nc,S_nr,Ten_dir,TenPre,
     $				M_dir,MPre,TMN_div

	character*80	FPAR_AnciDir,FPAR_Lat,FPAR_Lon,FPAR_dir,FPAR_Pre
	integer			FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,FPAR_S_nc
	integer			FPAR_S_nr,FPAR_N_div
	common			/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR_dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div

	character*80	LAI_AnciDir,LAI_Lat,LAI_Lon,LAI_dir,LAI_Pre
	integer			LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,LAI_S_nc
	integer			LAI_S_nr,LAI_N_div
	common			/Read_LAI_para/LAI_AnciDir,LAI_Lat,LAI_Lon,
     $				LAI_dir,LAI_Pre,LAI_D_nc,LAI_D_nr,LAI_S_c,
     $				LAI_S_r,LAI_S_nc,LAI_S_nr,LAI_N_div
		
	namelist		/Input_para/ levels,startyear,startmont,startday,
     $				endyear,endmont,endday,dx_max,
     $	code_file,dis_file,dir_file,slope_file,dem_file,area_file,
     $	frac_file,riverway_file,Outstn,Output_D,Derive_D,dt,rivlen
	Namelist		/Get_ET_para/ETID,DEM_FD,FRAC_FD,ETPATH
	Namelist		/Get_Grid_ATM_para/ 
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	Namelist		/Lambert_para/ r,lambda0,phi1
	Namelist		/Read_foring_para/ SHOWTIP,stn_info_file,
     $				stn_data_dir,NP,R0,CM,NPPR
	Namelist		/Read_NDVI_para/ DAnciDir,DLat,DLon,DSuffix,
     $				Col0,Col1,Row0,Row1,D_dir,DPre,DN_div
	Namelist		/Read_NDVI_para_TenDM/ TMAnciDir,TMLat,TMLon,
     $				D_nc,D_nr,S_c,S_r,S_nc,S_nr,Ten_dir,TenPre,
     $				M_dir,MPre,TMN_div
	Namelist		/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR_dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div
	Namelist		/Read_LAI_para/LAI_AnciDir,LAI_Lat,LAI_Lon,
     $				LAI_dir,LAI_Pre,LAI_D_nc,LAI_D_nr,LAI_S_c,
     $				LAI_S_r,LAI_S_nc,LAI_S_nr,LAI_N_div


	KER=0

	open(1, file='../PARAMETER/Input.par' ,status='old')
		read (1, NML=Input_para, err=5101)
		read (1, NML=Get_ET_para,err=5102)
		read (1, NML=Get_Grid_ATM_para,err=5103)
		read (1, NML=Lambert_para,err=5104)
		read (1, NML=Read_foring_para,err=5105)
		read (1, NML=Read_NDVI_para,err=5106)
		read (1, NML=Read_NDVI_para_TenDM,err=5107)
		read (1, NML=Read_FPAR_para,err=5108)
		read (1, NML=Read_LAI_para,err=5109)
	close(1)
	RETURN

5101	KER=1
	PRINT *, 'ERROR IN READING Input_para.'
	RETURN
5102	KER=2
	PRINT *, 'ERROR IN READING Get_ET_para.'
	RETURN
5103	KER=3
	PRINT *, 'ERROR IN READING Get_Grid_ATM_para.'
	RETURN
5104	KER=4
	PRINT *, 'ERROR IN READING Lambert_para.'
	RETURN
5105	KER=5
	PRINT *, 'ERROR IN READING Read_foring_para.'
	RETURN
5106	KER=6
	PRINT *, 'ERROR IN READING Read_NDVI_para.'
	RETURN
5107	KER=7
	PRINT *, 'ERROR IN READING Read_NDVI_para_TenDM.'
	RETURN
5108	KER=8
	PRINT *, 'ERROR IN READING Read_FPAR_para.'
	RETURN
5109	KER=9
	PRINT *, 'ERROR IN READING Read_LAI_para.'
	RETURN

	end
