c Analysis the trend of Reference ET, Precipitation, etc
c NOTE:
c	Pay attention to ND
c	In ANA_ReferET_TREND.f etc.
c	ReferET(2200,max_nr,max_nc), here 2200 should larger than ND
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
	include './SUBROUTINE/Get_potential_ET.f'
	include	'./SUBROUTINE/Get_Grid_ATM.f'
	include	'./SUBROUTINE/FAO_PMON.f'
	include	'./RW_FILE/R_Interpolate_data.f'
	include	'./RW_FILE/RW_Real_Binary.f'
	include	'./RW_FILE/R_ATM_data.f'
	include './TEMPORAL/Days_S_E.f'
	include './TEMPORAL/Month_D.f'
	include './ANALYSIS/ANA_ReferET_TREND.f'
	include './ANALYSIS/ANA_Prec_TREND.f'
	INCLUDE './ANALYSIS/ANA_ArcInfo_Ascii.f'
	include './ANALYSIS/ANA_nsumm_TREND.f'
	include './ANALYSIS/ANA_Tm_TREND.f'
	include './ANALYSIS/ANA_Tmax_TREND.f'
	include './ANALYSIS/ANA_Tmin_TREND.f'
	include './ANALYSIS/ANA_um_TREND.f'
	include './ANALYSIS/ANA_sun_TREND.f'

	implicit none
	include '../INCLUDE/common.inc'
	Integer StartY,EndY,ND,KER
	real	RETa(max_nr,max_nc),RETb(max_nr,max_nc)
	real	RETr2(max_nr,max_nc),RETavy(max_nr,max_nc)
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
	integer			l1,l2,m1,m2
	character*80	WAY

	real gridarea(max_nr,max_nc),fracnd(max_nr,max_nc),demnd(max_nr,max_nc)
	real fracdd(max_nr,max_nc),demd(max_nr,max_nc)
	common gridarea,fracnd,demnd,fracdd,demd
	real	RETa1,RETb1,RETr21,RETavy1
	real	RETa2,RETb2,RETr22,RETavy2


	call Read_parameters(KER)
	call Read_ANA_para(KER)
	call strlen(NDVI_ANA,l1,l2)
	INTPLT=3
	if (INTPLT.eq.1) then
		WAY='IDW'
	else if (INTPLT.eq.2) then
		WAY='TPS'
	else if (INTPLT.eq.3) then	
		WAY='TS'
	else
		WAY='UNKNOWN'
	endif
	call strlen(WAY,m1,m2)

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
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	StartY=1960
	EndY=2000
	ND=(EndY-StartY+1)*366
	Print *,ND

	PRINT *, 'TREND OF Reference ET (FAO Penman)'
	CALL 	ANA_ReferET_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)*365.
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)*365.
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'RETa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'RETb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'RETr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'RETavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'RETSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) ' ','RETa1','RETb1','RETr21','RETavy1','StartY','ENDY'
	write(1,101) 'Non-IRR',
     $RETa1*365.,RETb1*365.*365.,RETr21,RETavy1*365.,StartY,ENDY
     	write(1,101) 'WithIRR',
     $RETa2*365.,RETb2*365.*365.,RETr22,RETavy2*365.,StartY,ENDY
101	format(A15,4F15.3,2I8)
102	format(5A15,2A8)
	close(1)

	PRINT *, 'TREND OF Precipitation'
	CALL 	ANA_Prec_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)*365.
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)*365.
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'PRECa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'PRECb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'PRECr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'PRECavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'PRECSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','PRECa1','PRECb1','PRECr21','PRECavy1','StartY','ENDY'
	write(1,101) 'Non-IRR',
     $RETa1*365.,RETb1*365.*365.,RETr21,RETavy1*365.,StartY,ENDY
     	write(1,101) 'WithIRR',
     $RETa2*365.,RETb2*365.*365.,RETr22,RETavy2*365.,StartY,ENDY
	close(1)

	PRINT *, 'TREND OF mean Temperature'
	CALL 	ANA_Tm_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'TMa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','TMa1','TMb1','TMr21','TMavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)

	PRINT *, 'TREND OF Max Temperature'
	CALL 	ANA_Tmax_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'TMAXa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMAXb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMAXr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMAXavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMAXSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','TMAXa1','TMAXb1','TMAXr21','TMAXavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)


	PRINT *, 'TREND OF Min Temperature'
	CALL 	ANA_Tmin_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'TMINa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMINb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMINr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMINavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'TMINSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','TMINa1','TMINb1','TMINr21','TMINavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)


	PRINT *, 'TREND OF Relative Humidity'
	CALL 	ANA_um_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'UMa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'UMb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'UMr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'UMavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'UMSUMMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','UMa1','UMb1','UMr21','UMavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)

	PRINT *, 'TREND OF Sunshine duration'
	CALL 	ANA_sun_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'SUNa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'SUNb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'SUNr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'SUNavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'SUNSUMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','SUNa1','SUNb1','SUNr21','SUNavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)

	PRINT *, 'TREND OF Cloud Amount'
	CALL 	ANA_nsumm_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2) !Y = a + bX
	
	do i=1,nr
		do j=1,nc
			if (RETa(i,j).ne.-9999)
     $			RETa(i,j)=RETa(i,j)
			if (RETb(i,j).ne.-9999)
     $			RETb(i,j)=RETb(i,j)*365.
			if (RETavy(i,j).ne.-9999)
     $			RETavy(i,j)=RETavy(i,j)
		enddo
	enddo

	tmpname=NDVI_ANA(l1:l2)//'CLOUDa'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETa,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'CLOUDb'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETb,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'CLOUDr2'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETr2,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'CLOUDavy'//WAY(m1:m2)//'.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,RETavy,max_nr,max_nc)
	tmpname=NDVI_ANA(l1:l2)//'CLOUDSUMARY'//WAY(m1:m2)//'.asc'
	open (1, file=tmpname, status='unknown')
	write(1,102) 
     $' ','CLOUDa1','CLOUDb1','CLOUDr21','CLOUDavy1','StartY','ENDY'
	write(1,101) 
     $'Non-IRR',RETa1,RETb1*365.,RETr21,RETavy1,StartY,ENDY
     	write(1,101) 
     $'WithIRR',RETa2,RETb2*365.,RETr22,RETavy2,StartY,ENDY
	close(1)

	END



