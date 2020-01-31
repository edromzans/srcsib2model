c	This subroutine try to get trend of FPAR
c	if the data is not daily, interpolated to daily using cubic spline
c	Input:
c		StarY,StratM,EndY,EndM: The period start year, strat month, end year, end month
c		ND: The size of D_M_FPAR, if ND.lt.days between Startday to Endday, Error occurs
c	Output:
c		D_M_FPAR: the array saves daily FPAR
c		FPARa,FPARb,FPARr2: the array saves a,b,r2 of the period (before interpolated)
	Subroutine ANA_FPAR_TREND(StartY,StartM,
     $	EndY,EndM,ND,FPARa,FPARb,FPARr2,FPARavy,
     $	FPARa1,FPARb1,FPARr21,FPARavy1,
     $	FPARa2,FPARb2,FPARr22,FPARavy2,
     $	FPARa3,FPARb3,FPARr23,FPARavy3)
	implicit none
	include '../INCLUDE/common.inc'
	integer	StartY,StartM,EndY,EndM,DOIT,year,month,days_fromstart
	integer	KER,ND
	real	D_M_FPAR(7400,max_nr,max_nc)
	real	a,FPARa(max_nr,max_nc),b,FPARb(max_nr,max_nc)
	real	r2,FPARr2(max_nr,max_nc),AVy
	real	FPARavy(max_nr,max_nc)
	real	FPARa1,FPARb1,FPARr21,FPARavy1
	real	FPARa2,FPARb2,FPARr22,FPARavy2
	real	FPARa3,FPARb3,FPARr23,FPARavy3

	integer	nr,nc
	real	x0,y0,s
	real	aFPAR(max_nr,max_nc),fave,fmin,fmax,areasum
	real	AFPAR1(ND),AFPAR2(ND),AFPAR3(ND)


	integer	DayM(12)
	data	DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	integer	i,j,k,ID1,ID2,NF,NT,SAVEINTPLT
	real	days(ND),DFPAR(ND),dayi(ND)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI,tmpname
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	FPAR_AnciDir,FPAR_Lat,FPAR_Lon,FPAR_dir,FPAR_Pre
	integer			FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,FPAR_S_nc
	integer			FPAR_S_nr,FPAR_N_div
	common			/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR_dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div

	character*80	NDVI_ANA,FName         
	common			/NDVI_ANALYSIS/ NDVI_ANA
	CHARACTER*6		MMYYYY
	integer			l1,l2
	integer			month_days,mon1,mon2,nmon,nyear
	real			sum1,sum2,sumy1,sumy2,sum3,sumy3

	real gridarea(max_nr,max_nc),fracnd(max_nr,max_nc),demnd(max_nr,max_nc)
	real fracdd(max_nr,max_nc),demd(max_nr,max_nc),irrcode(max_nr,max_nc)
	common gridarea,fracnd,demnd,fracdd,demd,irrcode


	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	IF (ND.lt.NF) GOTO 44044

	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	DOIT=1
	year=StartY
	month=StartM
	ID1=0
	call strlen(NDVI_ANA,l1,l2)
	Call ConvMMYYYY(month,year,MMYYYY)
	FName=NDVI_ANA(l1:l2)//"FPAR"//MMYYYY//".txt"
	open(11,file=FName,status='unknown')
	FName=NDVI_ANA(l1:l2)//"FPAR_Y"//MMYYYY//".txt"
	open(33,file=FName,status='unknown')
	sumy1=0.
	sumy2=0.
	sumy3=0.
	nyear=0
	DO While (DOIT.EQ.1) 
c		print *,year,month
		ID1=ID1+1
		call Get_FPAR(year,month,aFPAR,KER)
		dayi(ID1)=days_fromstart(StartY,StartM,1,year,month,15)
		do i=1,nr
		do j=1,nc
			D_M_FPAR(ID1,i,j)=aFPAR(i,j)
		enddo
		enddo
		CALL ana_DATAF(aFPAR,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			AFPAR1(ID1)=fave
		CALL ana_DATAF(aFPAR,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			AFPAR2(ID1)=fave
		CALL ana_DATAF(aFPAR,gridarea,
     $		fracdd,irrcode,max_nr,max_nc,fave,fmin,fmax,areasum)
			AFPAR3(ID1)=fave
		Write(11,'(2I6,2F10.3)') 
     $		year,month,AFPAR1(ID1),AFPAR2(ID1),AFPAR3(ID1)
		sumy1 = sumy1 + AFPAR1(ID1)
		sumy2 = sumy2 + AFPAR2(ID1)
		sumy3 = sumy3 + AFPAR3(ID1)
		nyear = nyear + 1
		IF (month.EQ.12) THEN
			WRITE(33,'(I6,4F10.3)') 
     $		year,sumy1,sumy2,sumy3,sumy1/nyear,sumy2/nyear,sumy3/nyear
			sumy1 = 0.
			sumy2 = 0.
			sumy3 = 0.
			nyear = 0			
		ENDIF
		if (month.EQ.EndM.and.year.EQ.EndY) DOIT=0
		month=month+1
		if (month.gt.12) then
			month=1
			year=year+1
		endif
	ENDDO
	close(33)
	close(11)	
	close(11)	

	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	do i=1,nr
	do j=1,nc
			ID2=0
			do k=1,ID1
				if (D_M_FPAR(k,i,j).lt.100.0.and.
     $				D_M_FPAR(k,i,j).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=dayi(k)
					DFPAR(ID2)=D_M_FPAR(k,i,j)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,days,DFPAR,a,b,r2,AVy)
				FPARa(i,j)=a
				FPARb(i,j)=b
				FPARr2(i,j)=r2
				FPARAVy(i,j)=AVy
			else
				do k=1,NF
					FPARa(i,j)=-9999.0
					FPARb(i,j)=-9999.0
					FPARr2(i,j)=-9999.0
					FPARAVy(i,j)=-9999.0	
				enddo
			endif

	enddo
	enddo

		ID2=0
		do k=1,ID1
			if(AFPAR1(k).lt.100.0.and.
     $			AFPAR1(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				DFPAR(ID2)=AFPAR1(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,DFPAR,a,b,r2,AVy)
			FPARa1=a
			FPARb1=b
			FPARr21=r2
			FPARavy1=AVy		
		else
			FPARa1=-9999.0
			FPARb1=-9999.0
			FPARr21=-9999.0				
			FPARavy1=-9999.0		
		endif

		ID2=0
		do k=1,ID1
			if(AFPAR2(k).lt.100.0.and.
     $			AFPAR2(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				DFPAR(ID2)=AFPAR2(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,DFPAR,a,b,r2,AVy)
			FPARa2=a
			FPARb2=b
			FPARr22=r2
			FPARavy2=AVy		
		else
			FPARa2=-9999.0
			FPARb2=-9999.0
			FPARr22=-9999.0				
			FPARavy2=-9999.0		
		endif

		ID2=0
		do k=1,ID1
			if(AFPAR3(k).lt.100.0.and.
     $			AFPAR3(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				DFPAR(ID2)=AFPAR3(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,DFPAR,a,b,r2,AVy)
			FPARa3=a
			FPARb3=b
			FPARr23=r2
			FPARavy3=AVy		
		else
			FPARa2=-9999.0
			FPARb2=-9999.0
			FPARr22=-9999.0				
			FPARavy2=-9999.0		
		endif

	RETURN
44044 PRINT *, 'ND LESS THAN THE DAYS BETWEEN START TO END'
	PRINT *, 'ND=',ND,'DAYS=',NF
	STOP
	RETURN
	end