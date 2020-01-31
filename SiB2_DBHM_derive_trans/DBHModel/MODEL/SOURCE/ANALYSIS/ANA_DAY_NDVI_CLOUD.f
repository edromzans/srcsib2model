c	This subroutine try to get relationship between NDVI and CLOUD
c	if the data is not daily, interpolated to daily using cubic spline
c	Input:
c		StarY,StratM,EndY,EndM: The period start year, strat month, end year, end month
c	Output:
c		FILES:

c		SUN_CLDI_XXXXXX_YYYYYY_r2/a/b.asc  where SUN_CLDI means NCAI compare with duration of sunshine
c		CLD_CLDI_XXXXXX_YYYYYY_r2/a/b.asc  where SUN_CLDI means NCAI compare with observed cloud amount
c			XXXXXX,YYYYYY is StarY,StratM,EndY,EndM; r2,a,b is Y = a + bX  r2 is R_squared value
c		GET daily NDVI, composite monthly 'true' NDVI, 
c		then the 'true' monthly NDVI is interpolated to corrected daily NDVI
c		Compare the corrected daily NDVI to original NDVI, get NDVI Cloud and Aerosols Index (NCAI)
c		Compare the NACI to observed cloud amount and SCIA (i.e. n/N, where n is duration of sunshine)
c		
c		The Trendence of daily NDVI is also calucated and output
c		NDVI_011995_122000_D_r2/a/b.asc		Daily trendence
c		NDVI_011995_122000_M_r2/a/b.asc		Composite monthly trendence
c		Ten_011995_122000_r2/a/b.asc		Mean Month Sunshine compare with Mean Month NCAI (not used)	
	
	Subroutine ANA_DAY_NDVI_CLOUD(StartY,StartM,
     $	EndY,EndM)
	implicit none
	include '../INCLUDE/common.inc'
	integer	ND
	parameter (ND=8000)	
	integer	StartY,StartM,EndY,EndM,DOIT,year,month,days_fromstart
	integer month_days,id_mon
	integer	KER,DN !DN:total days in the year
	real	MNDVI(ND,max_nc)
	real	DNDVI(ND,max_nc)
	real	CLDI(ND,max_nc)
	real	DSUN(ND,max_nc)
	real	DCLD(ND,max_nc)
	real	DSUN2(ND),DCLD2(ND),CLDI2(ND),DNDVI2(ND),MNDVI2(ND)

	real	TenCLD(ND,max_nc),TenSUN(ND,max_nc),TenN(ND)
	real	Tena(max_nr,max_nc),Tenb(max_nr,max_nc)
	real	Tenr2(max_nr,max_nc)

	real a,b,r2,err,mbe
	real	MNDVIa(max_nr,max_nc),MNDVIb(max_nr,max_nc)
	real	MNDVIr2(max_nr,max_nc)
	real	DNDVIa(max_nr,max_nc),DNDVIb(max_nr,max_nc)
	real	DNDVIr2(max_nr,max_nc),DNDVImin,Dmin(max_nr,max_nc)
	real	SUNCLDa(max_nr,max_nc),SUNCLDb(max_nr,max_nc)
	real	SUNCLDr2(max_nr,max_nc)
	real	CLDCLDa(max_nr,max_nc),CLDCLDb(max_nr,max_nc)
	real	CLDCLDr2(max_nr,max_nc)
	real	OCSa(max_nr,max_nc),OCSb(max_nr,max_nc)
	real	OCSr2(max_nr,max_nc)
	
c	revised: output r.m.s.error, and m.b.e.
      real SUNCLDerr(max_nr,max_nc),CLDCLDerr(max_nr,max_nc)
      real SUNCLDmbe(max_nr,max_nc),CLDCLDmbe(max_nr,max_nc)

	integer	nr,nc,I,J,K,IY,IYD,II,JJ !,makedirqq,Imkdir
	real	x0,y0,s
	real	ndvi_ij(ndvi_nt,max_nc)	!ndvi at (i,j)
	real	n_summ(max_time,max_nc)	!Cloud cover data after interpolate
	real	sun(max_time,max_nc)	!Sunshine Time data after interpolate

	integer	DayM(12)
	data	DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	integer	ID1,ID2,NF,NT,SAVENDVIID,NNF,l1,l2,IM
	real	days(ND),Mdays(ND),XF(ND),FX(ND),sunN,Mdays2(ND)
	CHARACTER*6		SMY,EMY
	CHARACTER*2		IDM
	CHARACTER*80	DATAa,DATAb,DATAr2,DATAerr,DATAmbe
	real	lam_0,phi_1,x(max_nc),y(max_nr),lambda,phi,delta,varphi,ws
	real	N,max_daily,sumd,sumcd

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	NDVI_ANA         
	common			/NDVI_ANALYSIS/ NDVI_ANA
	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1	

	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NNF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	IF (ND.lt.NNF) GOTO 44044
	do i=1,NNF
		XF(i)=i
	enddo
	
	call Read_ANA_para(KER)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	call Strlen(NDVI_ANA,l1,l2)
	Call ConvMMYYYY(StartM,StartY,SMY)
	Call ConvMMYYYY(EndM,EndY,EMY)

c	Imkdir=makedirqq(NDVI_ANA(l1:l2))

	SAVENDVIID=NDVIID
	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0
c Get Lambert coordinate from geographic information head file
	do j=1,nc
		x(j)=x0+(real(j)-0.5)*s
	end do		
      do i=1,nr
		y(i)=y0+0.5*s+real(nr-i)*s
      end do

	do 	id_mon=1,12 !id_mon
	Print *, "ID_Mon= ",id_mon

	DO I=1,nr
		DOIT=1
		year=StartY
		month=StartM
		IM=0
		DO While (DOIT.EQ.1) 
			IM=IM+1
			NDVIID=2	!Get Daily ndvi_ij
			NT=DayM(month)
			if (mod(year,4).eq.0.and.month.eq.2) NT=29
			if (mod(year,4).ne.0.and.month.eq.2) NT=28	
			call GET_NDVI_ij(year,month,ndvi_ij,I,KER)	
c Get max daily as Tendays data
			Mdays(IM)=days_fromstart(StartY,StartM,1,year,month,15)
			do J=1,nc
				max_daily=-9999.1
				DO k=1,NT
					if (ndvi_ij(k,J).gt.max_daily) then
						max_daily=ndvi_ij(k,J)
					endif
				ENDDO
				MNDVI(IM,J)=max_daily				
			enddo					
c End Get Tendays data
			do k=1,NT
				NF=days_fromstart(
     $				StartY,StartM,1,year,month,k)
				DO J=1,nc
					DNDVI(NF,J)=ndvi_ij(k,J)
				ENDDO
			enddo		
			if (month.EQ.EndM.and.year.EQ.EndY) DOIT=0
			month=month+1
			if (month.gt.12) then
				month=1
				year=year+1
			endif
		ENDDO	
	
		PRINT *, 'RUN AT ROW:',I
		DO J=1,nc
c			PRINT *, 'RUN AT (ROW, COL):',I,J
			ID2=0	!Interpolate Monthly data to daily and calculated trend
			do k=1,IM
				if (MNDVI(k,J).lt.1.0.and.
     $				MNDVI(k,J).gt.-1.0) then
					ID2=ID2+1
					Mdays2(ID2)=Mdays(k)
					MNDVI2(ID2)=MNDVI(k,J)
				endif		
			enddo
			if (ID2.gt.3) then
				call CUBSPL(ID2,Mdays2,MNDVI2,NF,XF,FX)
				call HPINT(ID2,Mdays2,MNDVI2,a,b,r2)
				MNDVIa(I,J)=a
				MNDVIb(I,J)=b
				MNDVIr2(I,J)=r2
			else
				do k=1,NNF
					FX(k)=-9999.0
					MNDVIa(I,J)=-9999.0
					MNDVIb(I,J)=-9999.0
					MNDVIr2(I,J)=-9999.0	
				enddo
			endif
			do k=1,NNF
				if (FX(k).gt.1.0) FX(k)=1.0
				if (FX(k).lt.-1.0.and.FX(k).ne.-9999.0) FX(k)=-1.0
				MNDVI(k,J)=FX(k)
			enddo

			ID2=0		!Calculated Trend of Daily data
			do k=1,NNF
				if (DNDVI(k,J).lt.1.0.and.
     $					DNDVI(k,J).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=k
					DNDVI2(ID2)=DNDVI(k,J)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,days,DNDVI2,a,b,r2)
				DNDVIa(I,J)=a
				DNDVIb(I,J)=b
				DNDVIr2(I,J)=r2
				DNDVImin=1.1
				do k=1,ID2		!Search for DNDVImin		
					if (DNDVI2(k).lt.DNDVImin) then
						DNDVImin=DNDVI2(k)
					endif			
				enddo
				IF (DNDVImin.lt.-1.0.or.DNDVImin.gt.1.0)PRINT*,'Min/ERROR'
			else
				DNDVImin=-9999.0
				DNDVIa(I,J)=-9999.0
				DNDVIb(I,J)=-9999.0
				DNDVIr2(I,J)=-9999.0	
			endif

			Dmin(I,J)=DNDVImin
c			DNDVImin=0.0
			do k=1,NNF !Calculate CloudIndex=(Dndvi-DNDVImin)/(Mndvi-DNDVImin)
				If (DNDVImin.lt.-2.or.
     $				DNDVI(k,J).lt.-2.or.MNDVI(k,J).lt.-2) then
					CLDI(k,J)=-9999.0
				else
				CLDI(k,J)=(DNDVI(k,J)-Dmin(I,J))/(MNDVI(k,J)-Dmin(I,J))
					IF (CLDI(k,J).gt.1.0) then
						CLDI(k,J)=1.0
					ELSE IF (CLDI(k,J).lt.0.0) then
						CLDI(k,J)=0.0
					ENDIF
				endif	
			enddo	
cccccccccccccccccccccccccccccccccccc
			DOIT=1
			year=StartY
			month=StartM
			II=0
			DO While (DOIT.EQ.1) 
				II=II+1
				NT=DayM(month)
				if (mod(year,4).eq.0.and.month.eq.2) NT=29
				if (mod(year,4).ne.0.and.month.eq.2) NT=28	
				IYD=days_fromstart(StartY,StartM,1,year,month,1)

				sumd=0.0
				sumcd=0.0
				DO k=1,NT
					if (DNDVI(IYD+k-1,J).ne.-9999.0.and.
     $					MNDVI(IYD+k-1,J).ne.-9999.0) then
					sumd=sumd+DNDVI(IYD+k-1,J)-Dmin(I,J)
					sumcd=sumcd+MNDVI(IYD+k-1,J)-Dmin(I,J)
					endif
				ENDDO
				if (sumcd.ne.0) then
					if (sumd/sumcd.lt.0.0.or.sumd/sumcd.gt.1.0) then
						TenCLD(II,J)=-9999
					else
						TenCLD(II,J)=sumd/sumcd
					endif
				else
					TenCLD(II,J)=-9999.0
				endif

				if (month.EQ.EndM.and.year.EQ.EndY) DOIT=0
				month=month+1
				if (month.gt.12) then
					month=1
					year=year+1
				endif
			ENDDO
ccccccccccccccccccccccccccccccccccccc	
			call Convert_Lambert_latlon(x(J),y(I),
     $			lambda,phi,lam_0,phi_1,r,1)
			phi=phi	*180.0/pi

			do IY=StartY,EndY	!Get Observated CLOUD and SUNSHINE/N
				ID1=days_fromstart(StartY,1,1,IY,1,1)
				call Get_Grid_cld_sun_ij(IY,n_summ,sun,I)
				DN=days_fromstart(IY,1,1,IY,12,31)
				do IYD=1,DN
					delta=0.406*sin(2*pi*IYD/DN-1.39)		!(E24)
					varphi=phi*pi/180.0						!(E22)
					ws=acos(-tan(varphi)*tan(delta))		!(E25)
					N=24.0/pi*ws							!(E34)
					TenN(IYD)=N
				if (n_summ(IYD,J).lt.0.0.or.n_summ(IYD,J).gt.1.0) THEN
						DCLD(ID1+IYD-1,J)=-9999.0
					ELSE
						DCLD(ID1+IYD-1,J)=n_summ(IYD,J)
					endif
					sunN=sun(IYD,J)/N
					if (sunN.lt.-100.0) THEN
						DSUN(ID1+IYD-1,J)=-9999.0
					ELSE if (sunN.lt.0.0) THEN
						DSUN(ID1+IYD-1,J)=0.0
					ELSE if (sunN.gt.1.0) THEN
						DSUN(ID1+IYD-1,J)=1.0
					ELSE
						DSUN(ID1+IYD-1,J)=sunN
					endif
				enddo
cccccccccccccccccccccccccccccccccccccccc
				JJ=IY-StartY
				DO II=1,12
					NT=DayM(month)
					if (mod(year,4).eq.0.and.month.eq.2) NT=29
					if (mod(year,4).ne.0.and.month.eq.2) NT=28	
					IYD=days_fromstart(IY,1,1,IY,II,1)					
					sumd=0.0
					sumcd=0.0
					DO k=1,NT
				if (sun(IYD+k-1,J).ge.0.and.sun(IYD+k-1,J).le.12) then
						sumd=sumd+sun(IYD+k-1,J)
						sumcd=sumcd+TenN(IYD+k-1)
						endif
					ENDDO 
					if (sumcd.ne.0) then
						TenSUN(JJ*12+II,J)=sumd/sumcd
					else
						TenSUN(JJ*12+II,J)=-9999.0
					endif	
									
				ENDDO
cccccccccccccccccccccccccccccccccccccccc
			enddo
cccccccccccccccccccccccccccccccccccccccccccc
			ID2=0	!Calculated Relationship between TenSUN and TenCloudI
			do k=1,IM
				if (TenCLD(k,J).ne.-9999.0.and.TenSUN(k,J).ne.-9999.0) then
					ID2=ID2+1
					DSUN2(ID2)=TenSUN(k,J)
					CLDI2(ID2)=TenCLD(k,J)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,CLDI2,DSUN2,a,b,r2)
				Tena(I,J)=a
				Tenb(I,J)=b
				Tenr2(I,J)=r2
			else
				Tena(I,J)=-9999.0
				Tenb(I,J)=-9999.0
				Tenr2(I,J)=-9999.0
			endif
cccccccccccccccccccccccccccccccccccccccccccc
			ID2=0	!Calculated Relationship between SUN and CloudIndex
			do k=1,NNF
				if (month_days(StartY,StartM,1,k-1).eq.id_mon) then
				if (CLDI(k,J).ne.-9999.0.and.DSUN(k,J).ne.-9999.0) then
					ID2=ID2+1
					DSUN2(ID2)=DSUN(k,J)
					CLDI2(ID2)=CLDI(k,J)
				endif
				endif		
			enddo
			if (ID2.gt.3) then
c				call HPINT(ID2,DSUN2,CLDI2,a,b,r2)
				call HPINT_2(ID2,CLDI2,DSUN2,a,b,r2,err,mbe)				
				SUNCLDa(I,J)=a
				SUNCLDb(I,J)=b
				SUNCLDr2(I,J)=r2
				SUNCLDerr(I,J)=err
				SUNCLDmbe(I,J)=mbe
			else
				SUNCLDa(I,J)=-9999.0
				SUNCLDb(I,J)=-9999.0
				SUNCLDr2(I,J)=-9999.0
				SUNCLDerr(I,J)=-9999.0
				SUNCLDmbe(I,J)=-9999.0
			endif
	
			ID2=0	!Calculated Relationship between CLOUD and CloudIndex
			do k=1,NNF
				if (month_days(StartY,StartM,1,k-1).eq.id_mon) then
				if (CLDI(k,J).ne.-9999.0.and.DCLD(k,J).ne.-9999.0)then
					ID2=ID2+1
					DCLD2(ID2)=DCLD(k,J)
					CLDI2(ID2)=CLDI(k,J)
				endif	
				endif	
			enddo
			if (ID2.gt.3) then
c				call HPINT(ID2,DCLD2,CLDI2,a,b,r2)
				call HPINT_2 (ID2,CLDI2,DCLD2,a,b,r2,err,mbe)				
				CLDCLDa(I,J)=a
				CLDCLDb(I,J)=b
				CLDCLDr2(I,J)=r2
				CLDCLDerr(I,J)=err
				CLDCLDmbe(I,J)=mbe
			else
				CLDCLDa(I,J)=-9999.0
				CLDCLDb(I,J)=-9999.0
				CLDCLDr2(I,J)=-9999.0
				CLDCLDerr(I,J)=-9999.0
				CLDCLDmbe(I,J)=-9999.0
			endif

			ID2=0	!Calculated Relationship between CLOUD and SUNSHINE
			do k=1,NNF
				if (DSUN(k,J).ne.-9999.0.and.DCLD(k,J).ne.-9999.0)then
					ID2=ID2+1
					DCLD2(ID2)=DCLD(k,J)
					DSUN2(ID2)=DSUN(k,J)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,DCLD2,DSUN2,a,b,r2)
				OCSa(I,J)=a
				OCSb(I,J)=b
				OCSr2(I,J)=r2
			else
				OCSa(I,J)=-9999.0
				OCSb(I,J)=-9999.0
				OCSr2(I,J)=-9999.0
			endif

c	if (J.eq.150) then
c	open (1, file='OUT.txt')
c		do k=1,NNF
c			Write(1,'(7f12.5)'),DSUN(k,J),DCLD(k,J),DNDVI(k,J),MNDVI(k,J),
c     $			CLDI(k,J),TenSUN(k,J),TenCLD(k,J)
c		enddo
c	close(1)
c	endif
			
c			PAUSE '1'
c			PRINT*,	'NDVI Month:',MNDVIa(I,J),MNDVIb(I,J),MNDVIr2(I,J)				
c			PRINT*,	'Dmin:',DNDVImin
c			PRINT*,	'NDVI Daily:',DNDVIa(I,J),DNDVIb(I,J),DNDVIr2(I,J)			
c			PRINT*,	'SUN CLOUDI:',SUNCLDa(I,J),SUNCLDb(I,J),SUNCLDr2(I,J)				
c			PRINT*,	'CLD CLOUDI:',CLDCLDa(I,J),CLDCLDb(I,J),CLDCLDr2(I,J)
c			PRINT*,	'Obs.SUNCLD:',OCSa(I,J),OCSb(I,J),OCSr2(I,J)
c			PAUSE '2'
		ENDDO
	ENDDO

	if (id_mon.lt.10) then
		IDM(1:1)='0'
		write (IDM(2:2),'(i1)') id_mon
	else
		write (IDM,'(i2)') id_mon
	endif

	PRINT*,'PRINT ANALYSIS RESULTS'
	DATAa=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_M_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_M_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_M_r2.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,MNDVIa,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,MNDVIb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,MNDVIr2,max_nr,max_nc)
	DATAa=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_D_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_D_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'NDVI_'//SMY//'_'//EMY//'_D_r2.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,DNDVIa,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,DNDVIb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,DNDVIr2,max_nr,max_nc)
	DATAa=NDVI_ANA(l1:l2)//'SUN_CLDI_'//IDM//'_'//EMY//'_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'SUN_CLDI_'//IDM//'_'//EMY//'_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'SUN_CLDI_'//IDM//'_'//EMY//'_r2.asc'
	DATAerr=NDVI_ANA(l1:l2)//'SUN_CLDI_'//IDM//'_'//EMY//'_err.asc'
	DATAmbe=NDVI_ANA(l1:l2)//'SUN_CLDI_'//IDM//'_'//EMY//'_mbe.asc'	
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,SUNCLDa,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,SUNCLDb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,SUNCLDr2,max_nr,max_nc)
	call writefile_float (DATAerr,nr,nc,x0,y0,s,
     $		-9999.0,SUNCLDerr,max_nr,max_nc)     
	call writefile_float (DATAmbe,nr,nc,x0,y0,s,
     $		-9999.0,SUNCLDmbe,max_nr,max_nc)          
	DATAa=NDVI_ANA(l1:l2)//'CLD_CLDI_'//IDM//'_'//EMY//'_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'CLD_CLDI_'//IDM//'_'//EMY//'_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'CLD_CLDI_'//IDM//'_'//EMY//'_r2.asc'
	DATAerr=NDVI_ANA(l1:l2)//'CLD_CLDI_'//IDM//'_'//EMY//'_err.asc'
	DATAmbe=NDVI_ANA(l1:l2)//'CLD_CLDI_'//IDM//'_'//EMY//'_mbe.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,CLDCLDa,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,CLDCLDb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,CLDCLDr2,max_nr,max_nc)
	call writefile_float (DATAerr,nr,nc,x0,y0,s,
     $		-9999.0,CLDCLDerr,max_nr,max_nc)
	call writefile_float (DATAmbe,nr,nc,x0,y0,s,
     $		-9999.0,CLDCLDmbe,max_nr,max_nc)
	DATAa=NDVI_ANA(l1:l2)//'Obs_SUNCLD_'//SMY//'_'//EMY//'_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'Obs_SUNCLD_'//SMY//'_'//EMY//'_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'Obs_SUNCLD_'//SMY//'_'//EMY//'_r2.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,OCSa,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,OCSb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,OCSr2,max_nr,max_nc)
	DATAa=NDVI_ANA(l1:l2)//'Ten_'//SMY//'_'//EMY//'_a.asc'
	DATAb=NDVI_ANA(l1:l2)//'Ten_'//SMY//'_'//EMY//'_b.asc'
	DATAr2=NDVI_ANA(l1:l2)//'Ten_'//SMY//'_'//EMY//'_r2.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,Tena,max_nr,max_nc)
	call writefile_float (DATAb,nr,nc,x0,y0,s,
     $		-9999.0,Tenb,max_nr,max_nc)
	call writefile_float (DATAr2,nr,nc,x0,y0,s,
     $		-9999.0,Tenr2,max_nr,max_nc)

	DATAa=NDVI_ANA(l1:l2)//'Dmin_'//SMY//'_'//EMY//'.asc'
	call writefile_float (DATAa,nr,nc,x0,y0,s,
     $		-9999.0,Dmin,max_nr,max_nc)

	end do !	do 	id_mon=1,12
	NDVIID=SAVENDVIID
	RETURN
44044 PRINT *, 'ND LESS THAN THE DAYS BETWEEN START TO END'
	PRINT *, 'ND=',ND,'DAYS=',NF
	STOP
	RETURN
	end
