c Output and compare with observationss
	subroutine OUT_CMP_OBV(niter)
	include '../INCLUDE/common.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	integer	nday,nmon,nyear,days_fromstart,i,istn
	real	AXj,AFj,Sfj,Sxj,rfxj,MSEj,MSEcj,MSSSj,rmse,rrmse
	real	em_t,ea_t,ep_t
c	real	Xj(7320),Fj(7320) !7320 for 20 year running !18300 for 50 yrs
	real	Xj(18300),Fj(18300)
	integer	o1,o2,s1,s2,niter
	real	c1,c2,c3,ymin,ymax
	character*80 cmpday,cmpmonth,cmpyear,sumday,summon,sumyear,sum_spc
	character*80 fsw1,fsw2,fsw3
	integer	GMT_Nfig
	character*80 GMT_fname,GMT_psname,GMT_stname(30),GMT_dname(30)

	CALL strlen(Output_D,o1,o2)
	sumday	= Output_D(o1:o2)//'Ver_day.txt'
	summon	= Output_D(o1:o2)//'Ver_month.txt'
	sum_spc	= Output_D(o1:o2)//'Ver_month_spe.txt'
	sumyear	= Output_D(o1:o2)//'Ver_year.txt'
	fsw1	= Output_D(o1:o2)//'SW1.txt'
	fsw2	= Output_D(o1:o2)//'SW2.txt'
	fsw3	= Output_D(o1:o2)//'SW3.txt'

	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	sw1 = sw1 / niter
	sw2 = sw2 / niter
	sw3 = sw3 / niter
	CALL writefile_float (fsw1,max_nr,max_nc,x0,y0,s,
     $	-9999.,sw1,max_nr,max_nc)
	CALL writefile_float (fsw2,max_nr,max_nc,x0,y0,s,
     $	-9999.,sw2,max_nr,max_nc)
	CALL writefile_float (fsw3,max_nr,max_nc,x0,y0,s,
     $	-9999.,sw3,max_nr,max_nc)

	ymin=9999.
	ymax=-9999.
	do istn=1,n_stno
		CALL strlen(outstnn(istn),s1,s2)
		GMT_stname(istn)=""
		GMT_stname(istn)=outstnn(istn)(s1:s2)
		GMT_dname(istn)=""
		GMT_dname(istn)=outstnn(istn)(s1:s2)//'P_DOS_D.txt'
		cmpday = Output_D(o1:o2)//outstnn(istn)(s1:s2)//'P_DOS_D.txt'
		OPEN(1,file=cmpday,status='unknown')
		nday= days_fromstart
     $	(startyear,startmont,startday,endyear,endmont,endday)
		do i=1,nday
			if (DayOBV(i,istn).eq.-9999.) then
			c1=DayPRE(i,istn)
			c2=-9999.
			c3=DaySIM(i,istn)*24.*3600./ctlacca(istn)/1000.
			else
			c1=DayPRE(i,istn)
			c2=DayOBV(i,istn)*24.*3600./ctlacca(istn)/1000.
			c3=DaySIM(i,istn)*24.*3600./ctlacca(istn)/1000.
			endif
			write(1,'(3F15.3)') c1,c2,c3
			ymin= min(ymin,c1,c2,c3)
			ymax= max(ymax,c1,c2,c3)
		enddo
		CLOSE(1)
	enddo
	GMT_Nfig=n_stno
	GMT_fname=Output_D(o1:o2)//"P_DOS_D.csh"
	GMT_psname=Output_D(o1:o2)//"P_DOS_D.ps"	
	GMT_r1=0.0
	GMT_r2=nday
	GMT_r3=ymin
	if (GMT_r3.eq.-9999.) GMT_r3=0.0
	GMT_r4=ymax
	CALL write_csh(GMT_fname,GMT_psname,
     $	GMT_Nfig,GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,GMT_DDY,GMT_IDX,
     $	GMT_IDY,GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,GMT_cxx,GMT_tyy,
     $    GMT_cy,GMT_cyy,GMT_stname,GMT_dname)

	ymin=9999.
	ymax=-9999.
	do istn=1,n_stno
		CALL strlen(outstnn(istn),s1,s2)
		GMT_stname(istn)=""
		GMT_stname(istn)=outstnn(istn)(s1:s2)
		GMT_dname(istn)=""
		GMT_dname(istn)=outstnn(istn)(s1:s2)//'P_DOS_M.txt'
		cmpmonth = Output_D(o1:o2)//outstnn(istn)(s1:s2)//'P_DOS_M.txt'
		OPEN(1,file=cmpmonth,status='unknown')
		nmon= (endyear - startyear) * 12 +endmont
		do i=1,nmon
			if (MonOBV(i,istn).eq.-9999.) then
			c1=MonPRE(i,istn)
			c2=-9999.
			c3=MonSIM(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
			else
			c1=MonPRE(i,istn)
			c2=MonOBV(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
			c3=MonSIM(i,istn)*24.*3600.*30.4375/ctlacca(istn)/1000.
			endif
			write(1,'(3F15.3)') c1,c2,c3
			ymin= min(ymin,c1,c2,c3)
			ymax= max(ymax,c1,c2,c3)
		enddo
		CLOSE(1)
	enddo
	GMT_Nfig=n_stno
	GMT_fname=Output_D(o1:o2)//"P_DOS_M.csh"
	GMT_psname=Output_D(o1:o2)//"P_DOS_M.ps"	
	GMT_r1=0.0
	GMT_r2=nmon
	GMT_r3=ymin
	if (GMT_r3.eq.-9999.) GMT_r3=0.0
	GMT_r4=ymax
	CALL write_csh(GMT_fname,GMT_psname,
     $	GMT_Nfig,GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,GMT_DDY,GMT_IDX,
     $	GMT_IDY,GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,GMT_cxx,GMT_tyy,
     $    GMT_cy,GMT_cyy,GMT_stname,GMT_dname)

	ymin=9999.
	ymax=-9999.
	do istn=1,n_stno
		CALL strlen(outstnn(istn),s1,s2)
		cmpyear = Output_D(o1:o2)//outstnn(istn)(s1:s2)//'P_DOS_Y.txt'
		OPEN(1,file=cmpyear,status='unknown')
		nyear= endyear - startyear + 1
		do i=1,nyear
			if (YearOBV(i,istn).eq.-9999.) then
			c1=YearPRE(i,istn)
			c2=-9999.
			c3=YearSIM(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. 
			else
			c1=YearPRE(i,istn)
			c2=YearOBV(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000. 
			c3=YearSIM(i,istn)*24.*3600.*365.25/ctlacca(istn)/1000.
			endif
			write(1,'(3F15.3)') c1,c2,c3
			ymin= min(ymin,c1,c2,c3)
			ymax= max(ymax,c1,c2,c3)
		enddo
	CLOSE(1)
	enddo
	GMT_Nfig=n_stno
	GMT_fname=Output_D(o1:o2)//"P_DOS_Y.csh"
	GMT_psname=Output_D(o1:o2)//"P_DOS_Y.ps"
	GMT_r1=0.0
	GMT_r2=nyear
	GMT_r3=ymin
	if (GMT_r3.eq.-9999.) GMT_r3=0.0
	GMT_r4=ymax		
	CALL write_csh(GMT_fname,GMT_psname,
     $	GMT_Nfig,GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,GMT_DDY,GMT_IDX,
     $	GMT_IDY,GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,GMT_cxx,GMT_tyy,
     $    GMT_cy,GMT_cyy,GMT_stname,GMT_dname)

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
		if (MonOBV(ntmp,istn).eq.-9999.) then
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