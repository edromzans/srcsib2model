c Analysis the trend of LAI, FPAR etc
c NOTE:
c	Pay attention to ND
c	In ANA_LAI_TREND etc.
c	D_M_lai(2200,max_nr,max_nc), here 2200 should larger than ND
	include './CHARACTER/Strlen.f'
	include './CHARACTER/ConvMMYYYY.f'
	include './RW_FILE/R_parameters.f'
	include './RW_FILE/R_ANA_para.f'
	include './RW_FILE/R_GEO_hrd.f'
	include './RW_FILE/RW_ArcInfo_Ascii.f'
	include './GEO_REFER/Convert_Lambert_latlon.f'
	include './INTERPOLATE/CUBICSPLINE.F'
	include './INTERPOLATE/HPINT.F'
      include './INTERPOLATE/HPINT_2.F'	
	include	'./INTERPOLATE/ThinPlateSplines2.f'
	include './INTERPOLATE/InverseDistance.f'
	include './INTERPOLATE/ThiessenPolygon.f'
	include './INTERPOLATE/Interpolate_tm.f'
	include './INTERPOLATE/Interpolate_tmax.f'
	include './INTERPOLATE/Interpolate_tmin.f'
	include './INTERPOLATE/Interpolate_um.f'
	include './INTERPOLATE/Interpolate_n_summ.f'
	include './INTERPOLATE/Interpolate_fsm.f'
	include './INTERPOLATE/Interpolate_rsum.f'
	include './INTERPOLATE/Interpolate_sun.f'
	INCLUDE './SUBROUTINE/GET_LAI.f'
	INCLUDE './SUBROUTINE/GET_FPAR.f'
	include	'./SUBROUTINE/FAO_PMON.f'
	include	'./RW_FILE/R_Interpolate_data.f'
	include	'./RW_FILE/RW_Real_Binary.f'
	include	'./RW_FILE/R_ATM_data.f'
	INCLUDE './RW_FILE/R_ancillary_file.f'
	include './TEMPORAL/Days_S_E.f'
	include './TEMPORAL/Month_D.f'
	include './ANALYSIS/ANA_LAI_TREND.f'
	include './ANALYSIS/ANA_FPAR_TREND.f'
	INCLUDE './ANALYSIS/ANA_ArcInfo_Ascii.f'
	INCLUDE './GEO_REFER/Abstract_SA.f'
	INCLUDE './GEO_REFER/Get_Goode_coordinate.f'
	INCLUDE './CHARACTER/ConvYYMM.f'
	INCLUDE './RW_FILE/R_FPAR_SA.f'
	INCLUDE './RW_FILE/R_SIB2_para.f'


	implicit none
	include '../INCLUDE/common.inc'
	Integer StartY,EndY,ND,KER,StartM,EndM
	real	LAIa(max_nr,max_nc),LAIb(max_nr,max_nc)
	real	LAIr2(max_nr,max_nc),LAIavy(max_nr,max_nc)
	character*80 tmpname
	integer	nr,nc,i,j
	real	x0,y0,s,nodata
	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
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
	character*80	NDVI_ANA         
	common			/NDVI_ANALYSIS/ NDVI_ANA
	integer			l1,l2
	character*80	irriratio_file,irrcode_file,irrpro_file 
	common		/Water_withdraw/ irriratio_file,irrcode_file,irrpro_file
	namelist	/Water_withdraw/ irriratio_file,irrcode_file,irrpro_file

	real gridarea(max_nr,max_nc),fracnd(max_nr,max_nc),demnd(max_nr,max_nc)
	real fracdd(max_nr,max_nc),demd(max_nr,max_nc),irrcode(max_nr,max_nc)
	common gridarea,fracnd,demnd,fracdd,demd,irrcode
	real	LAIa1,LAIb1,LAIr21,LAIavy1
	real	LAIa2,LAIb2,LAIr22,LAIavy2
	real	LAIa3,LAIb3,LAIr23,LAIavy3


	call Read_parameters(KER)
	call Read_SIB2_para(KER)
	call Read_ANA_para(KER)
	call strlen(NDVI_ANA,l1,l2)

	CALL readfile_float (area_file,nr,nc,x0,y0,s,
     $	nodata,gridarea,max_nr,max_nc)	   
	CALL readfile_float (frac_file,nr,nc,x0,y0,s,
     $	nodata,fracnd,max_nr,max_nc)	
	CALL readfile_float (dem_file,nr,nc,x0,y0,s,
     $	nodata,demnd,max_nr,max_nc)
	CALL readfile_float (FRAC_FD,nr,nc,x0,y0,s,
     $	nodata,fracdd,max_nr,max_nc)	
	CALL readfile_float (DEM_FD,nr,nc,x0,y0,s,
     $	nodata,demd,max_nr,max_nc)
	CALL readfile_float(irrcode_file,nr,nc,x0,y0,s,nodata,
     $		irrcode,max_nr,max_nc)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	StartY=1982
	StartM=1
	EndY=2000
	EndM=12
	ND=(EndY-StartY+1)*366
	Print *,ND

	PRINT *, 'TREND OF LAI'
	CALL 	ANA_LAI_TREND(StartY,StartM,
     $	EndY,EndM,ND,LAIa,LAIb,LAIr2,LAIavy,
     $	LAIa1,LAIb1,LAIr21,LAIavy1,
     $	LAIa2,LAIb2,LAIr22,LAIavy2,
     $	LAIa3,LAIb3,LAIr23,LAIavy3) !Y = a + bX

	do i=1,nr
		do j=1,nc
			if (LAIb(i,j).ne.-9999)
     $			LAIb(i,j)=LAIb(i,j)*365.
		enddo
	enddo	

	tmpname=NDVI_ANA(l1:l2)//'LAIa.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'LAIb.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'LAIr2.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'LAIavy.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'LAISUMMARY.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) ' ','LAIa1','LAIb1','LAIr21','LAIavy1','StartY','ENDY'
	write(1,101) 'Non-IRR',LAIa1,LAIb1*365.,LAIr21,LAIavy1,StartY,ENDY
     	write(1,101) 'WithIRR',LAIa2,LAIb2*365.,LAIr22,LAIavy2,StartY,ENDY
     	write(1,101) 'IRR',LAIa3,LAIb3*365.,LAIr23,LAIavy3,StartY,ENDY
101	format(A15,4F15.3,2I8)
102	format(5A15,2A8)
	close(1)

	PRINT *, 'TREND OF FPAR'
	CALL 	ANA_FPAR_TREND(StartY,StartM,
     $	EndY,EndM,ND,LAIa,LAIb,LAIr2,LAIavy,
     $	LAIa1,LAIb1,LAIr21,LAIavy1,
     $	LAIa2,LAIb2,LAIr22,LAIavy2,
     $	LAIa3,LAIb3,LAIr23,LAIavy3) !Y = a + bX

	do i=1,nr
		do j=1,nc
			if (LAIb(i,j).ne.-9999)
     $			LAIb(i,j)=LAIb(i,j)*365.
		enddo
	enddo	

	tmpname=NDVI_ANA(l1:l2)//'FPARa.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'FPARb.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'FPARr2.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'FPARavy.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,LAIavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'FPARSUMMARY.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $	' ','FPARa1','FPARb1','FPARr21','FPARavy1','StartY','ENDY'
	write(1,101) 'Non-IRR',LAIa1,LAIb1*365.,LAIr21,LAIavy1,StartY,ENDY
     	write(1,101) 'WithIRR',LAIa2,LAIb2*365.,LAIr22,LAIavy2,StartY,ENDY
     	write(1,101) 'IRR',LAIa3,LAIb3*365.,LAIr23,LAIavy3,StartY,ENDY
	close(1)
	
	END



