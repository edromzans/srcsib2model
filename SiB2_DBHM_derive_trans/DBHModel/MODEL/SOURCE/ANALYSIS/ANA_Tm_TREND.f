c	This subroutine analysis the Trend of mean Temperature
c	if the data is not daily, interpolated to daily using cubic spline
c	Input:
c		StarY,EndY: The period start year, strat month, end year, end month
c		ND: The size of ReferET, if ND.lt.days between Startday to Endday, Error occurs
c	Output:
c		ReferET: the array saves daily ReferET
c		RETa,RETb,RETr2: the array saves a,b,r2 of the period (before interpolated)
	Subroutine ANA_Tm_TREND(StartY,
     $	EndY,ND,RETa,RETb,RETr2,RETavy,
     $	RETa1,RETb1,RETr21,RETavy1,
     $	RETa2,RETb2,RETr22,RETavy2)
	implicit none
	include '../INCLUDE/common.inc'
	integer	StartY,EndY,DOIT,year,month,days_fromstart
	integer	KER,ND
	real	ReferET(7320,max_nr,max_nc)
	real	AReferET1(ND),AReferET2(ND)
	real	a,RETa(max_nr,max_nc),b,RETb(max_nr,max_nc)
	real	r2,RETr2(max_nr,max_nc),AVy,RETavy(max_nr,max_nc)
	real	RETa1,RETb1,RETr21,RETavy1
	real	RETa2,RETb2,RETr22,RETavy2

	integer	nr,nc
	real	x0,y0,s
	real tm(max_time,max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_time,max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_time,max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_time,max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_time,max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_time,max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_time,max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_time,max_nr,max_nc)	!Sunshine Time data after interpolate
	real	aret(max_nr,max_nc),fave,fmin,fmax,areasum

	integer	DayM(12)
	data	DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	integer	i,j,k,ID1,ID2,NF,NT
	real	days(ND),Dndvi(ND),dayi(ND)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

	real gridarea(max_nr,max_nc),fracnd(max_nr,max_nc),demnd(max_nr,max_nc)
	real fracdd(max_nr,max_nc),demd(max_nr,max_nc)
	common gridarea,fracnd,demnd,fracdd,demd

	character*80	NDVI_ANA,FName         
	common			/NDVI_ANALYSIS/ NDVI_ANA
	CHARACTER*4		YYYY
	CHARACTER*1		WAY
	integer			l1,l2
	integer			month_days,mon1,mon2,nmon,nyear
	real			sum1,sum2,sumy1,sumy2

	NF= days_fromstart(StartY,1,1,EndY,12,3)
	IF (ND.lt.NF) GOTO 44044

	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	DOIT=1
	year=StartY
	ID1=0
	call strlen(NDVI_ANA,l1,l2)
	write(YYYY,'(I4)') year
	write(WAY,'(I1)') INTPLT
	FName=NDVI_ANA(l1:l2)//"TM"//YYYY//"_"//WAY//".txt"
	open(11,file=FName,status='unknown')
	FName=NDVI_ANA(l1:l2)//"TM"//YYYY//"M_"//WAY//".txt"
	open(22,file=FName,status='unknown')
	FName=NDVI_ANA(l1:l2)//"TM"//YYYY//"Y_"//WAY//".txt"
	open(33,file=FName,status='unknown')
	sum1=0.
	sum2=0.
	sumy1=0.
	sumy2=0.
	nmon=0
	nyear=0
	DO While (DOIT.EQ.1) 
		ID1=ID1+1
		call Get_Grid_ATM(year,tm,tmax,tmin,um,n_summ,fsm,rsum,sun)
		NT=days_fromstart(
     $			year,1,1,year,12,31)
		NF=days_fromstart(
     $			StartY,1,1,year-1,12,31)
		if (NF.lt.0) NF=0
		do k=1,NT
c			print *,NF,k,NF+k
			dayi(NF+k)=NF+k
			do i=1,nr
			do j=1,nc
				ReferET(NF+k,i,j)=tm(k,i,j)
				aret(i,j)=tm(k,i,j)
			enddo
			enddo
			CALL ana_DATAF(aret,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			AReferET1(NF+k)=fave
			CALL ana_DATAF(aret,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			AReferET2(NF+k)=fave
			Write(11,'(2I6,2F10.3)') 
     $		year,NF+k,AReferET1(NF+k)*365.25,AReferET2(NF+k)*365.25
			mon1=month_days(StartY,1,1,NF+k-1)
			mon2=month_days(StartY,1,1,NF+k)
			if (mon1.ne.mon2) then
				sum1=sum1+AReferET1(NF+k)
				sum2=sum2+AReferET2(NF+k)
				nmon=nmon+1
				write(22,'(2I6,4F10.3)') 
     $				year,mon1,sum1,sum2,sum1/nmon,sum2/nmon
				sum1=0.
				sum2=0.
				nmon=0
			else
				sum1=sum1+AReferET1(NF+k)
				sum2=sum2+AReferET2(NF+k)
				nmon=nmon+1
			endif		
			sumy1=sumy1+AReferET1(NF+k)
			sumy2=sumy2+AReferET2(NF+k)	
			nyear=nyear+1
		enddo
		write(33,'(I6,4F10.3)') 
     $		year,sumy1,sumy2,sumy1/nyear,sumy2/nyear
    			sumy1=0.
			sumy2=0.
			nyear=0 	
		year=year+1
		if (year.gt.EndY) DOIT=0
	ENDDO
	close(33)
	close(22)
	close(11)
	NF= days_fromstart(StartY,1,1,EndY,12,31)
	do i=1,nr
	do j=1,nc
		ID2=0
		do k=1,NF
			if(ReferET(k,i,j).lt.1000.0.and.
     $			ReferET(k,i,j).gt.-100.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dndvi(ID2)=ReferET(k,i,j)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dndvi,a,b,r2,AVy)
			RETa(i,j)=a
			RETb(i,j)=b
			RETr2(i,j)=r2
			RETavy(i,j)=AVy		
		else
			RETa(i,j)=-9999.0
			RETb(i,j)=-9999.0
			RETr2(i,j)=-9999.0				
			RETavy(i,j)=-9999.0		
		endif
	enddo
	enddo

		ID2=0
		do k=1,NF
			if(AReferET1(k).lt.1000.0.and.
     $			AReferET1(k).gt.-100.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dndvi(ID2)=AReferET1(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dndvi,a,b,r2,AVy)
			RETa1=a
			RETb1=b
			RETr21=r2
			RETavy1=AVy		
		else
			RETa1=-9999.0
			RETb1=-9999.0
			RETr21=-9999.0				
			RETavy1=-9999.0		
		endif

		ID2=0
		do k=1,NF
			if(AReferET2(k).lt.1000.0.and.
     $			AReferET2(k).gt.-100.0) then
				ID2=ID2+1
				days(ID2)=dayi(k)
				Dndvi(ID2)=AReferET2(k)
			endif		
		enddo
		if (ID2.gt.3) then
			call HPINT(ID2,days,Dndvi,a,b,r2,AVy)
			RETa2=a
			RETb2=b
			RETr22=r2
			RETavy2=AVy		
		else
			RETa2=-9999.0
			RETb2=-9999.0
			RETr22=-9999.0				
			RETavy2=-9999.0		
		endif

	RETURN
44044 PRINT *, 'ND LESS THAN THE DAYS BETWEEN START TO END'
	PRINT *, 'ND=',ND,'DAYS=',NF
	STOP
	RETURN
	end