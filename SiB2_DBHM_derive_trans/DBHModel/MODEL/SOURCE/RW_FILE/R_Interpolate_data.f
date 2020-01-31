	subroutine	Read_Interpolate_data(startyear,endyear,GEO_hrd,n_stn,stn,
     $	INTPLT,nr,nc,tm,tmax,tmin,um,n_summ,fsm,rsum,sun)
	implicit none
	include '../INCLUDE/Def_Read_foring.inc'
      real			x0,y0,znodata_f,s
      integer			nc,nr,KER,count
c      integer			dem(max_nr,max_nc)
	character*80	GEO_hrd
	integer			i,j
	real			distance,long_tmp,lat_tmp,lam_0,phi_1
	real			x(max_nc),y(max_nr)
	integer			n_stn,startyear,endyear
	character*30	DATE_TIME  

	integer		INTPLT !=1, InverseDistance; =2, ThinPlateSpiline; =3, TS
c	arguments for ThinPlateSplines2
	INTEGER		NIWK,NWK

	integer			SHOWTIP,NP,NPPR
	real			R0,CM
	character*80	stn_info_file,stn_data_dir
	common			/Read_foring_para/SHOWTIP,stn_info_file,
     $				stn_data_dir,NP,R0,CM,NPPR
	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1

	real tm(max_time,max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_time,max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_time,max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_time,max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_time,max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_time,max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_time,max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_time,max_nr,max_nc)	!Sunshine Time data after interpolate

	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0

c Get Lambert coordinate from geographic information head file
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	do j=1,nc
		x(j)=x0+(real(j)-0.5)*s
	end do		
      do i=1,nr
		y(i)=y0+0.5*s+real(nr-i)*s
      end do

	R0=R0*s

c Get the gauge station information: (stn_id, Lambert_x, Lambert_y, elevation)
	IF (SHOWTIP) Print *, 'YEAR:',startyear
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, 'Read Station Infomation...',DATE_TIME
	n_stn=0
	open(1, file=stn_info_file, status='old')
	do i=1,max_stn
		read(1,*,end=1) stn(i).id,long_tmp,lat_tmp,stn(i).elev
		long_tmp=long_tmp*pi/180.0
		lat_tmp=lat_tmp*pi/180.0	
		call Convert_Lambert_latlon(stn(i).x,stn(i).y,
     $		long_tmp,lat_tmp,lam_0,phi_1,r,0)
		n_stn=n_stn+1
	end do
1	close(1)

c Read observation data into the structure of station /only the data in calculate period are saved/
c e.g. if calculated 1999-, then 1999/1/1 is saved in stn(i).record(1) 1999/1/2 is saved in stn(i).record(2)...
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, 'Read input data files...',DATE_TIME
	call read_data_file(stn_data_dir,n_stn,startyear,endyear,stn)

c Interpolate	the observation data into grids
	NIWK=7*n_stn
	NWK=7*n_stn+360+100

	IF (SHOWTIP) Print *, 'Begin interpolate...'
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	tm	',DATE_TIME
	call Interpolate_tm(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,tm,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	tmax	',DATE_TIME
	call Interpolate_tmax(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,tmax,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	tmin	',DATE_TIME
	call Interpolate_tmin(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,tmin,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	um	',DATE_TIME
	call Interpolate_um(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,um,KER)	
	IF (SHOWTIP) call fdate(DATE_TIME)
      IF (SHOWTIP) Print *, '	n_summ	',DATE_TIME	
	call Interpolate_n_summ(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,n_summ,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	fsm	',DATE_TIME
	call Interpolate_fsm(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,fsm,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	rsum	',DATE_TIME
	call Interpolate_rsum(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,rsum,KER)
	IF (SHOWTIP) call fdate(DATE_TIME)
	IF (SHOWTIP) Print *, '	sun	',DATE_TIME
	call Interpolate_sun(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,sun,KER)
	IF (SHOWTIP) Print *, 'End of interpolate.'
	
	end

