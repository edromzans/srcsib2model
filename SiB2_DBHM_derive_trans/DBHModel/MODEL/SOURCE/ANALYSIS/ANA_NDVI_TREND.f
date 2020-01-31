c	This subroutine try to get trend of NDVI
c	if the data is not daily, interpolated to daily using cubic spline
c	Input:
c		ID: if ID=1, monthly NDVI is interpolated using cubic spline and return
c			if ID=3, ten days NDVI isinterpolated using cubic spline and return
c			if ID=2, daily NDVI is return
c		StarY,StratM,EndY,EndM: The period start year, strat month, end year, end month
c		ND: The size of D_M_NDVI, if ND.lt.days between Startday to Endday, Error occurs
c	Output:
c		D_M_NDVI: the array saves daily NDVI
c		NDVIa,NDVIb,NDVIr2: the array saves a,b,r2 of the period (before interpolated)
	Subroutine ANA_NDVI_TREND(ID,StartY,StartM,
     $	EndY,EndM,ND,D_M_NDVI,NDVIa,NDVIb,NDVIr2)
	implicit none
	include '../INCLUDE/common.inc'
	integer	ID,StartY,StartM,EndY,EndM,DOIT,year,month,days_fromstart
	integer	KER,ND
	real	D_M_NDVI(ND,max_nr,max_nc)
	real	a,NDVIa(max_nr,max_nc),b,NDVIb(max_nr,max_nc)
	real	r2,NDVIr2(max_nr,max_nc)

	integer	nr,nc
	real	x0,y0,s
	real	ndvi(ndvi_nt,max_nr,max_nc)

	integer	DayM(12)
	data	DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	integer	i,j,k,ID1,ID2,NF,NT,SAVEINTPLT
	real	days(ND),Dndvi(ND),XF(ND),FX(ND)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	IF (ND.lt.NF) GOTO 44044

	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	SAVEINTPLT=INTPLT
	INTPLT=ID

	DOIT=1
	year=StartY
	month=StartM
	ID1=0
	DO While (DOIT.EQ.1) 
	ID1=ID1+1
	if (NDVIID.EQ.1) then
		call GET_NDVI(year,month,ndvi,KER)
		days(ID1)=days_fromstart(StartY,StartM,1,year,month,15)
		do i=1,nr
		do j=1,nc
			D_M_NDVI(ID1,i,j)=ndvi(1,i,j)
		enddo
		enddo
	endif
	if (NDVIID.EQ.3) then
		call GET_NDVI(year,month,ndvi,KER)
		do k=1,3
			days(3*(ID1-1)+k)=days_fromstart(
     $			StartY,StartM,1,year,month,10*k-5)
			do i=1,nr
			do j=1,nc
				D_M_NDVI(3*(ID1-1)+k,i,j)=ndvi(k,i,j)
			enddo
			enddo
		enddo
	endif
	if (NDVIID.EQ.2) then
		call GET_NDVI(year,month,ndvi,KER)
		NT=DayM(month)
		if (mod(year,4).eq.0.and.month.eq.2) NT=29
		if (mod(year,4).ne.0.and.month.eq.2) NT=28	
		do k=1,NT
			NF=days_fromstart(
     $			StartY,StartM,1,year,month,k)
			do i=1,nr
			do j=1,nc
				D_M_NDVI(NF,i,j)=ndvi(k,i,j)
			enddo
			enddo
		enddo
	endif
	if (month.EQ.EndM.and.year.EQ.EndY) DOIT=0
	month=month+1
	if (month.gt.12) then
		month=1
		year=year+1
	endif
	ENDDO

	NT=DayM(EndM)
	if (mod(EndY,4).eq.0.and.EndM.eq.2) NT=29
	if (mod(EndY,4).ne.0.and.EndM.eq.2) NT=28	
	NF= days_fromstart(StartY,StartM,1,EndY,EndM,NT)
	do i=1,NF
		XF(i)=i
	enddo
	do i=1,nr
	do j=1,nc
	PRINT*, NDVIID, i,j
		if (NDVIID.EQ.1) then
			ID2=0
			do k=1,ID1
				if (D_M_NDVI(k,i,j).lt.1.0.and.
     $				D_M_NDVI(k,i,j).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=days(k)
					Dndvi(ID2)=D_M_NDVI(k,i,j)
				endif		
			enddo
			if (ID2.gt.3) then
				call CUBSPL(ID2,days,Dndvi,NF,XF,FX)
				call HPINT(ID2,days,Dndvi,a,b,r2)
				NDVIa(i,j)=a
				NDVIb(i,j)=b
				NDVIr2(i,j)=r2
			else
				do k=1,NF
					FX(k)=-9999.0
					NDVIa(i,j)=-9999.0
					NDVIb(i,j)=-9999.0
					NDVIr2(i,j)=-9999.0	
				enddo
			endif
			do k=1,NF
				if (FX(k).gt.1.0) FX(k)=1.0
				if (FX(k).lt.-1.0.and.FX(k).ne.-9999.0) FX(k)=-1.0
				D_M_NDVI(k,i,j)=FX(k)
			enddo
		endif
		if (NDVIID.EQ.3) then
			ID2=0
			do k=1,ID1*3
				if(D_M_NDVI(k,i,j).lt.1.0.and.
     $				D_M_NDVI(k,i,j).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=days(k)
					Dndvi(ID2)=D_M_NDVI(k,i,j)
				endif		
			enddo
			if (ID2.gt.3) then
				call CUBSPL(ID2,days,Dndvi,NF,XF,FX)
				call HPINT(ID2,days,Dndvi,a,b,r2)
				NDVIa(i,j)=a
				NDVIb(i,j)=b
				NDVIr2(i,j)=r2
			else
				do k=1,NF
					FX(k)=-9999.0
					NDVIa(i,j)=-9999.0
					NDVIb(i,j)=-9999.0
					NDVIr2(i,j)=-9999.0	
				enddo
			endif
			do k=1,NF
				if (FX(k).gt.1.0) FX(k)=1.0
				if (FX(k).lt.-1.0.and.FX(k).ne.-9999.0) FX(k)=-1.0
				D_M_NDVI(k,i,j)=FX(k)
			enddo
		endif
		if (NDVIID.EQ.2) then
			ID2=0
			do k=1,NF
				if(D_M_NDVI(k,i,j).lt.1.0.and.
     $				D_M_NDVI(k,i,j).gt.-1.0) then
					ID2=ID2+1
					days(ID2)=k
					Dndvi(ID2)=D_M_NDVI(k,i,j)
				endif		
			enddo
			if (ID2.gt.3) then
				call HPINT(ID2,days,Dndvi,a,b,r2)
				NDVIa(i,j)=a
				NDVIb(i,j)=b
				NDVIr2(i,j)=r2
			else
				NDVIa(i,j)=-9999.0
				NDVIb(i,j)=-9999.0
				NDVIr2(i,j)=-9999.0				
			endif
		endif
	enddo
	enddo

	INTPLT=SAVEINTPLT
	RETURN
44044 PRINT *, 'ND LESS THAN THE DAYS BETWEEN START TO END'
	PRINT *, 'ND=',ND,'DAYS=',NF
	STOP
	RETURN
	end