c Output and compare with observationss
	subroutine OUT_CMP_OBV()
	include '../INCLUDE/common.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	integer	nday,nmon,nyear,days_fromstart,i,istn
	real	AXj,AFj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj,rmse,rrmse
	real	em_t,ea_t,ep_t
	real	Xj(7320),Fj(7320)
	integer	o1,o2
	character*80 cmpday,cmpmonth,cmpyear,sumday,summon,sumyear,sum_spc

	CALL strlen(Output_D,o1,o2)
	cmpday	= Output_D(o1:o2)//'OBV_SIM_day.txt'
	cmpmonth= Output_D(o1:o2)//'OBV_SIM_month.txt'
	cmpyear	= Output_D(o1:o2)//'OBV_SIM_year.txt'
	sumday	= Output_D(o1:o2)//'Ver_day.txt'
	summon	= Output_D(o1:o2)//'Ver_month.txt'
	sum_spc	= Output_D(o1:o2)//'Ver_month_spe.txt'
	sumyear	= Output_D(o1:o2)//'Ver_year.txt'

	OPEN(1,file=cmpday,status='unknown')
	write(1,'(4A15)') 'DAY','PREC','OBV','SIM'
	do istn=1,n_stno
		write(1,'(A45,$)') outstnn(istn)
	enddo
	write(1,*)
	do istn=1,n_stno
		write(1,'(3A15,$)') 'PREC','OBV','SIM'
	enddo
	write(1,*)
	nday= days_fromstart
     $	(startyear,startmont,startday,endyear,endmont,endday)
	do i=1,nday
		do istn=1,n_stno
		if (DayOBV(i,istn).eq.-9999.) then
		write(1,'(3F15.3,$)') 
     $	DayPRE(i,istn),-9999.,
     $    DaySIM(i,istn)*24.*3600./ctlacca(istn)/1000.
		else
		write(1,'(3F15.3,$)') 
     $	DayPRE(i,istn),DayOBV(i,istn)*24.*3600./ctlacca(istn)/1000.,
     $    DaySIM(i,istn)*24.*3600./ctlacca(istn)/1000.
		endif
		enddo
		write(1,*)	
	enddo
	CLOSE(1)

	OPEN(1,file=cmpmonth,status='unknown')
	write(1,'(4A15)') 'MONTH','PREC','OBV','SIM'
	do istn=1,n_stno
		write(1,'(A45,$)') outstnn(istn)
	enddo
	write(1,*)
	do istn=1,n_stno
		write(1,'(3A15,$)') 'PREC','OBV','SIM'
	enddo
	write(1,*)
	nmon= (endyear - startyear) * 12 +endmont
	do i=1,nmon
		do istn=1,n_stno
		if (MonOBV(i,istn).eq.-9999.) then
		write(1,'(3F15.3,$)') 
     $	 MonPRE(i,istn),
     $     -9999.,
     $     MonSIM(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
		else
		write(1,'(3F15.3,$)') 
     $	 MonPRE(i,istn),
     $     MonOBV(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.,
     $     MonSIM(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
		endif
		enddo
		write(1,*)	
	enddo
	CLOSE(1)

	OPEN(1,file=cmpyear,status='unknown')
	write(1,'(4A15)') 'YEAR','PREC','OBV','SIM'
	do istn=1,n_stno
		write(1,'(A45,$)') outstnn(istn)
	enddo
	write(1,*)
	do istn=1,n_stno
		write(1,'(3A15,$)') 'PREC','OBV','SIM'
	enddo
	write(1,*)
	nyear= endyear - startyear + 1
	do i=1,nyear
		do istn=1,n_stno
		if (YearOBV(i,istn).eq.-9999.) then
		write(1,'(3F15.3,$)') 
     $	 YearPRE(i,istn),
     $     -9999. ,
     $     YearSIM(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. 
		else
		write(1,'(3F15.3,$)') 
     $	 YearPRE(i,istn),
     $     YearOBV(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. ,
     $     YearSIM(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. 
		endif
		enddo
		write(1,*)	
	enddo
	CLOSE(1)

	OPEN(1,file=sumday,status='unknown')
	write (1,'(A20)') 'Day'
	write (1,'(A20,6A12)') 
     $	'Station','MSSS','rmse','rrmse','mean','amplitude','phase'
	nday= days_fromstart
     $	(startyear,startmont,startday,endyear,endmont,endday)
	do istn=1,n_stno
		do i=1,nday
		if (DayOBV(i,istn).eq.-9999.) then
		Xj(i)=-9999.	
		else
		Xj(i)=DayOBV(i,istn)*24.*3600./ctlacca(istn)/1000.	!(m3/s->mm/day)
		endif
		Fj(i)=DaySIM(i,istn)*24.*3600./ctlacca(istn)/1000.
		enddo
		CALL MSSS_Murphy		!daily
     $(nday,Xj,Fj,AFj,AXj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj,em_t,ea_t,ep_t)
		if (MSEj.ge.0.0) then
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,sqrt(MSEj),sqrt(MSEj)/AXj,em_t,ea_t,ep_t
		else
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,-9999.,-9999.,em_t,ea_t,ep_t
		endif
	enddo
	close(1)

	OPEN(1,file=summon,status='unknown')
	write (1,'(A20)') 'Month'
	write (1,'(A20,6A12)') 
     $	'Station','MSSS','rmse','rrmse','mean','amplitude','phase'
	nmon= (endyear - startyear) * 12 +endmont
	do istn=1,n_stno
		do i=1,nmon
		if (MonOBV(i,istn).eq.-9999.) then
		Xj(i)=-9999.	
		else
		Xj(i)=MonOBV(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000. 
		endif
		Fj(i)=MonSIM(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
		enddo
		CALL MSSS_Murphy		!daily
     $(nmon,Xj,Fj,AFj,AXj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj,em_t,ea_t,ep_t)
		if (MSEj.ge.0.0) then
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,sqrt(MSEj),sqrt(MSEj)/AXj,em_t,ea_t,ep_t
		else
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,-9999.,-9999.,em_t,ea_t,ep_t
		endif
	enddo
	close(1)

	OPEN(1,file=sum_spc,status='unknown')
	write (1,'(A20)') 'Annual Month Special'
	write (1,'(A20,6A12)') 
     $	'Station','MSSS','rmse','rrmse','mean','amplitude','phase'
	nyear= endyear - startyear + 1
	do istn=1,n_stno
		write (1,'(A20)')  outstnn(istn)
		do i=1,nyear
			nmon=12
			do j=1 ,nmon
			ntmp = (i-1)*12+j 
		if (MonOBV(i,istn).eq.-9999.) then
		Xj(i)=-9999.	
		else
		Xj(j)=MonOBV(ntmp,istn)*24.*3600.*30.4375/ctlacca(istn)/1000. 
		endif
		Fj(j)=MonSIM(ntmp,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
			enddo
			CALL MSSS_Murphy		!daily
     $			(nmon,Xj,Fj,AFj,AXj,Sfj,Sxj,rfxj,MSEj,MSEcj,
     $			MSSSj,em_t,ea_t,ep_t)
			if (MSEj.ge.0.0) then
			write (1,'(I20,6F12.4)') 
     $			nyear+startyear-1,MSSSj,sqrt(MSEj),
     $			sqrt(MSEj)/AXj,em_t,ea_t,ep_t
			else
			write (1,'(I20,6F12.4)') 
     $			i+startyear-1,MSSSj,-9999.,
     $			-9999.,em_t,ea_t,ep_t
			endif
		enddo
	enddo
	close(1)

	OPEN(1,file=sumyear,status='unknown')
	write (1,'(A20)') 'Year'
	write (1,'(A20,6A12)') 
     $	'Station','MSSS','rmse','rrmse','mean','amplitude','phase'
	nyear= endyear - startyear + 1
	do istn=1,n_stno
		do i=1,nyear
		if (YearOBV(i,istn).eq.-9999.) then
		Xj(i)=-9999.	
		else
		Xj(i)=YearOBV(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. 
		endif
		Fj(i)=YearSIM(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000.
		enddo
		CALL MSSS_Murphy		!daily
     $		(nyear,Xj,Fj,AFj,AXj,Sfj,Sxj,rfxj,MSEj,
     $	     MSEcj,MSSSj,em_t,ea_t,ep_t)
		if (MSEj.ge.0.0) then
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,sqrt(MSEj),sqrt(MSEj)/AXj,em_t,ea_t,ep_t
		else
     		write (1,'(A20,6F12.4)') 
     $	outstnn(istn),MSSSj,-9999.,-9999.,em_t,ea_t,ep_t
		endif
	enddo
	close(1)

	end !subroutine OUT_CMP_OBV

c Verfication scores, refer to Murphy, 1988
c recommendate by WMO
c also refer to Arora 1999
c xij,fij (i=1, 2, .. Ni) denote time series of observations and forecasts
c Axj, AFj: averages
c Sfj, Sxj: sqrt ( sample variances)
c rfxj:	  product moment correlations of the forecasts and observations
c MSEj:	  mean squared error of the forcasts
c MSEcj:	  mean squared error of climatology forecasts (Murphy, 1988)
c MSSSj:	  Verfication scores
c em_t,ea_t,ep_t: error associated with overall mean, amplitude, and phase
c 
	subroutine MSSS_Murphy
     $(Ni,Xj,Fj,AFj,AXj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj,em_t,ea_t,ep_t)
	integer	Ni,Na
	DIMENSION Xj(*), Fj(*), Xa(Ni), Fa(Ni)
	real	AXj,AFj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj
	real	em_t,ea_t,ep_t

	integer i,j
	real	sumx,sumf,sumxx,sumff,sumfx,sumfx2
	real	em2,ea2,ep2,etot2
	
	Na=0
	do i=1,Ni
		if (abs(Xj(i)+9999.).gt.0.1.and.abs(Fj(i)+9999.).gt.0.1) then
			Na=Na+1
			Xa(Na)=Xj(i)
			Fa(Na)=Fj(i)
		endif
	enddo

	if (Na.lt.3) then
		AFj	=-9999.
		AXj	=-9999.
		Sfj	=-9999.
		Sxj	=-9999.
		rfxj	=-9999.
		MSEj	=-9999.
		MSEcj	=-9999.
		MSSSj	=-9999.
		em_t	=-9999.
		ea_t	=-9999.
		ep_t	=-9999.
	else
		sumx=0.
		sumf=0.
		do i=1,Na
			sumx=sumx+Xa(i)
			sumy=sumy+Fa(i)
		enddo
		AXj = sumx / Na
		AFj = sumy / Na

		sumxx=0.
		sumff=0.
		do i=1,Na
			sumxx=sumxx+ ( Xa(i)-AXj )*( Xa(i)-AXj )
			sumff=sumff+ ( Fa(i)-AFj )*( Fa(i)-AFj )
		enddo
		Sxj = sqrt( sumxx / (Na - 1) )
		Sfj = sqrt( sumff / (Na - 1) )

		sumfx=0.
		do i=1,Na
			sumfx=sumfx+ ( Xa(i)-AXj )*( Fa(i)-AFj )
		enddo
		rfxj = sumfx / Na / (Sfj*Sxj)

		sumfx2=0.
		do i=1,Na
			sumfx2=sumfx2+ ( Fa(i)-Xa(i) )*( Fa(i)-Xa(i) )
		enddo
		MSEj = sumfx2 / Na

		MSEcj = sumxx / Na
		MSSSj = 1 - MSEj / MSEcj
		
		em2 = (AXj-AFj)*(AXj-AFj)
		ea2 = ( sqrt(sumff/Na)-sqrt(sumxx/Na) )**2.
		ep2 = 2.*sqrt(sumff/Na)*sqrt(sumxx/Na)
     $		*(1.-sumfx/Na/(sqrt(sumff/Na)*sqrt(sumxx/Na))  )
		etot2=sumfx2/ Na
		em_t	= em2 / etot2 * 100.
		ea_t	= ea2 / etot2 * 100.
		ep_t	= ep2 / etot2 * 100.	
	endif

	end !subroutine MSSS_Murphy