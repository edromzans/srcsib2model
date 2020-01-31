c	This subroutine try to get trend of LAI
c	if the data is not daily, interpolated to daily using cubic spline
c	Input:
c		StarY,StratM,EndY,EndM: The period start year, strat month, end year, end month
c		ND: The size of D_M_lai, if ND.lt.days between Startday to Endday, Error occurs
c	Output:
c		D_M_lai: the array saves daily lai
c		laia,laib,lair2: the array saves a,b,r2 of the period (before interpolated)
	Subroutine ANA_LAI_TREND(StartY,StartM,
     $	EndY,EndM,ND,LAIa,LAIb,LAIr2,LAIavy,
     $	LAIa1,LAIb1,LAIr21,LAIavy1,
     $	LAIa2,LAIb2,LAIr22,LAIavy2,
     $	LAIa3,LAIb3,LAIr23,LAIavy3)
	implicit none
	include '../INCLUDE/common.inc'
	integer	StartY,StartM,EndY,EndM,DOIT,year,month,days_fromstart
	integer	KER,ND
	real	D_M_lai(7400,max_nr,max_nc)
	real	a,laia(max_nr,max_nc),b,laib(max_nr,max_nc)
	real	r2,lair2(max_nr,max_nc),AVy
	real	LAIavy(max_nr,max_nc)
	real	LAIa1,LAIb1,LAIr21,LAIavy1
	real	LAIa2,LAIb2,LAIr22,LAIavy2
	real	LAIa3,LAIb3,LAIr23,LAIavy3

	integer	nr,nc
	real	x0,y0,s
	real	alai(max_nr,max_nc),fave,fmin,fmax,areasum
	real	Alai1(ND),Alai2(ND),Alai3(ND)


	integer	DayM(12)
	data	DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	integer	i,j,k,ID1,ID2,NF,NT,SAVEINTPLT
	real	days(ND),Dlai(ND),dayi(ND)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI,tmpname
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	LAI_AnciDir,LAI_Lat,LAI_Lon,LAI_dir,LAI_Pre
	integer			LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,LAI_S_nc
	integer			LAI_S_nr,LAI_N_div
	common			/Read_LAI_para/LAI_AnciDir,LAI_Lat,LAI_Lon,
     $				LAI_dir,LAI_Pre,LAI_D_nc,LAI_D_nr,LAI_S_c,
     $				LAI_S_r,LAI_S_nc,LAI_S_nr,LAI_N_div
	
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
	FName=NDVI_ANA(l1:l2)//"LAI"//MMYYYY//".txt"
	open(11,file=FName,status='unknown')
	FName=NDVI_ANA(l1:l2)//"LAI_Y"//MMYYYY//".txt"
	open(33,file=FName,status='unknown')
	sumy1=0.
	sumy2=0.
	sumy3=0.
	nyear=0
	DO While (DOIT.EQ.1) 
c		print *,year,month
		ID1=ID1+1
		call Get_LAI(year,month,alai,KER)
		dayi(ID1)=days_fromstart(StartY,StartM,1,year,month,15)
		do i=1,nr
		do j=1,nc
			D_M_lai(ID1,i,j)=alai(i,j)
		enddo
		enddo
		CALL ana_DATAF(alai,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1(ID1)=fave
		CALL ana_DATAF(alai,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2(ID1)=fave
		CALL ana_DATAF(alai,gridarea,
     $		fracdd,irrcode,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai3(ID1)=fave
			Write(11,'(2I6,4F10.3)') 
     $			year,month,dayi(ID1),Alai1(ID1),Alai2(ID1),Alai3(ID1)
		sumy1 = sumy1 + Alai1(ID1)
		sumy2 = sumy2 + Alai2(ID1)
		sumy3 = sumy3 + Alai3(ID1)
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

	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	do i=1,nr
	do j=1,nc
			ID2=0
			do k=1,ID1
				if (D_M_lai(k,i,j).lt.100.0.and.
     $				D_M_lai(k,i,j).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=dayi(k)
					Dlai(ID2)=D_M_lai(k,i,j)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,days,Dlai,a,b,r2,AVy)
				laia(i,j)=a
				laib(i,j)=b
				lair2(i,j)=r2
				laiAVy(i,j)=AVy
			else
				do k=1,NF
					laia(i,j)=-9999.0
					laib(i,j)=-9999.0
					lair2(i,j)=-9999.0
					laiAVy(i,j)=-9999.0	
				enddo
			endif

	enddo
	enddo

		ID2=0
		do k=1,ID1
			if(Alai1(k).lt.100.0.and.
     $			Alai1(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dlai(ID2)=Alai1(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dlai,a,b,r2,AVy)
			LAIa1=a
			LAIb1=b
			LAIr21=r2
			LAIavy1=AVy		
		else
			LAIa1=-9999.0
			LAIb1=-9999.0
			LAIr21=-9999.0				
			LAIavy1=-9999.0		
		endif

		ID2=0
		do k=1,ID1
			if(Alai2(k).lt.100.0.and.
     $			Alai2(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dlai(ID2)=Alai2(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dlai,a,b,r2,AVy)
			LAIa2=a
			LAIb2=b
			LAIr22=r2
			LAIavy2=AVy		
		else
			LAIa2=-9999.0
			LAIb2=-9999.0
			LAIr22=-9999.0				
			LAIavy2=-9999.0		
		endif

		ID2=0
		do k=1,ID1
			if(Alai3(k).lt.100.0.and.
     $			Alai3(k).gt.-1.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dlai(ID2)=Alai3(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dlai,a,b,r2,AVy)
			LAIa3=a
			LAIb3=b
			LAIr23=r2
			LAIavy3=AVy		
		else
			LAIa2=-9999.0
			LAIb2=-9999.0
			LAIr22=-9999.0				
			LAIavy2=-9999.0		
		endif

	RETURN
44044 PRINT *, 'ND LESS THAN THE DAYS BETWEEN START TO END'
	PRINT *, 'ND=',ND,'DAYS=',NF
	STOP
	RETURN
	end