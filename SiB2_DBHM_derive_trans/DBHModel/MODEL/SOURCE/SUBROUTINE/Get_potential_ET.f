c Get daily potential ET
c Input:
c	Year: the year the daily potential ET is required
c Output:
c	ET:  The potential ET value

c	if ETID = 1 calculate reference potential ET (FAO_Penman_Monteith reference crop)
c	if ETID<>1	the parameters is required (not finished, 2004/12/15)
	subroutine	Get_potential_ET(year,ET)
	implicit none
	include '../INCLUDE/common.inc'
	Real	Lat,Alt			!Latitude (DD), Altitude (m)
	Real	as,bs,alpha		!Fraction of extraterrestrial radiation, Albedo
	Integer	DJ,DN			!Number of the day in the year between 1 and 365/6
	real	r_a,d,h,zom,zoh								!(E4)
	real	r_s,r_l,LAIact,LAI							!(E5)
	real	lambda,phi,lam_0,phi_1
	real*4	AVGtmp(max_nr,max_nc)
	real	x0,y0,s,znodata_f,demd(max_nr,max_nc),x(max_nc),y(max_nr)
	integer	nr,nc,mm,nn,ll,days_fromstart,l1,l2,m1,m2 !,Imkdir,makedirqq
	character*80	WAY,ET_file
	character*4		YYYY
	integer i,j,k,year,nodata
	real tm(max_time,max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_time,max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_time,max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_time,max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_time,max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_time,max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_time,max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_time,max_nr,max_nc)	!Sunshine Time data after interpolate
	real ET(max_time,max_nr,max_nc)
	integer			ETID
	character*80	DEM_FD,FRAC_FD,ETPATH
	common			/Get_ET_para/ETID,DEM_FD,FRAC_FD,ETPATH
	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI	
	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1

	call readfile_float(DEM_FD,nr,nc,x0,y0,s,znodata_f,
     $	demd,max_nr,max_nc)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	DN=days_fromstart(year,1,1,year,12,31)

	Write (YYYY,'(i4)') year
	Call Strlen(ETPATH,l1,l2)
c	Imkdir= makedirqq(ETPATH(l1:l2))
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
	IF (ETID.eq.1) then
	ET_file=ETPATH(l1:l2)//YYYY//'_ReferET_'//WAY(m1:m2)//'.bin'
	ELSE
	ET_file=ETPATH(l1:l2)//YYYY//'_PTET_'//WAY(m1:m2)//'.bin'
	ENDIF

	open(1, file=ET_file ,status='old',ERR=2011)
	close(1)
	GOTO 2022

2011	Print *, 'File not exist',ET_file
	PRINT *, 'Try to generate ET_file'
	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0
c Get Lambert coordinate from geographic information head file
	do j=1,nc
		x(j)=x0+(real(j)-0.5)*s
	end do		
      do i=1,nr
		y(i)=y0+0.5*s+real(nr-i)*s
      end do

	call Get_Grid_ATM(year,tm,tmax,tmin,um,n_summ,fsm,rsum,sun)
	do ll=1,DN
		do j=1,nr
			do k=1,nc
				ET(ll,j,k)=-9999
			enddo
		enddo
	enddo

	do j=1,DN
		do mm=1,nr
			do nn=1,nc
	if (demd(mm,nn).ne.-9999) then
	call Convert_Lambert_latlon(x(nn),y(mm),lambda,phi,lam_0,phi_1,r,1)
	phi=phi	*180.0/pi
		IF (ETID.eq.1) then
			call FAO_PMON_day(phi,demd(mm,nn),j,DN,as,bs,alpha,r_a,r_s,
     $		tm(j,mm,nn),tmax(j,mm,nn),tmin(j,mm,nn),um(j,mm,nn),
     $		n_summ(j,mm,nn),fsm(j,mm,nn),rsum(j,mm,nn),sun(j,mm,nn),
     $		1,ET(j,mm,nn))
		else
			PRINT *,'NOT reference ET!'
			STOP
		endif
	Endif
			enddo
		enddo
	enddo
	call WR_binary(ET_file,0,DN,nr,nc,ET,max_time,max_nr,max_nc)	
2022	call WR_binary(ET_file,1,DN,nr,nc,ET,max_time,max_nr,max_nc)

	IF (ETID.eq.1) then
	ET_file=ETPATH(l1:l2)//'AVG_'//YYYY//'_ReferET_'//WAY(m1:m2)//'.asc'
	ELSE
	ET_file=ETPATH(l1:l2)//'AVG_'//YYYY//'_PTET_'//WAY(m1:m2)//'.asc'
	ENDIF

	open(1, file=ET_file ,status='old',ERR=2033)
	close(1)
	GOTO 2044

2033	Print *, 'Save ET results to file:',ET_file
	do j=1,nr
		do k=1,nc
			AVGtmp(j,k)=0.0
		enddo
	enddo
	do j=1,nr
		do k=1,nc
			nodata=1
			do ll=1,DN
				if (nodata.eq.1) then
					if (ET(ll,j,k).ne.-9999) then
					AVGtmp(j,k)=AVGtmp(j,k)+ET(ll,j,k)/real(DN)
					else
					AVGtmp(j,k)=-9999
					nodata=0
					endif
				endif
			enddo
		enddo
	enddo
	call writefile_float(ET_file,nr,nc,x0,
     $	y0,s,-9999.0,AVGtmp,max_nr,max_nc)

2044	RETURN	
	end
