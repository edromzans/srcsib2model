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
     
	character*80	vege_file,soil_file,soil_para,slpsib_f,sibpp
	common			/SIB2_LSM/ vege_file,soil_file,soil_para,slpsib_f,sibpp

	character*80	irriratio_file,irrcode_file,irrpro_file
	common			/Water_withdraw/ irriratio_file,irrcode_file,irrpro_file 
	
	character*80	obv_dir,out_soil
	real			layo1         
	common			/Verf_OBV/ obv_dir,out_soil,layo1
	
	character*80	NDVI_ANA         
	common			/NDVI_ANALYSIS/ NDVI_ANA	

	integer	GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,GMT_DDY,GMT_IDX,GMT_IDY
	real	GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,
     $		GMT_cxx,GMT_cy,GMT_cyy,GMT_tyy
	common		/GMT_SET_VER/ GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,
     $	GMT_DDY,GMT_IDX,GMT_IDY,
     $	GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,
     $	GMT_cxx,GMT_cy,GMT_cyy,GMT_tyy
	