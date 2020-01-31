c Interpolate precipitation data
c Include simple data check: the precipitaion data cannot be less than zero
	subroutine Interpolate_rsum(nr,nc,n_stn,startyear,endyear,
     $	stn,x,y,INTPLT,NP,R0,CM,NPPR,NIWK,NWK,rsum,KER)
	include	'../INCLUDE/Def_Read_foring.inc'
	integer		nr,nc,n_stn,startyear,endyear
	integer		i,j,npi,ii,jj,days_fromstart
	INTEGER		NIWK,NIWKU,NWK,NWKU,KER,NPPR
	DIMENSION	IWK(NIWK), WK(NWK),fo(nc,nr)
	real		x(nc),y(nr)
	real		xi(n_stn),yi(n_stn),fi(n_stn)
	real rsum(max_time,max_nr,max_nc)	!Precipitation data after interpolate
	integer		INTPLT !=1, InverseDistance; =2, ThinPlateSpiline
c	arguments for InverseDistance
	INTEGER		NP
	REAL		R0,CM
	KER=0
	do i=1, days_fromstart(startyear,1,1,endyear,12,31)
		npi=0
		do j=1, n_stn
			if (stn(j).nodata(i).ne.0) then
				if (stn(j).rsum(i).ne.-99999.0) then
					npi=npi+1
					xi(npi)=stn(j).x
					yi(npi)=stn(j).y
					fi(npi)=stn(j).rsum(i)
					if (fi(npi).lt.0.0) then
					PRINT *,'INPUT ERROR.'
					PRINT *,'DAY:',i,'AT',xi(npi),yi(npi),'P:',fi(npi)
					STOP 'INPUT PRECIPITATION CAN NOT BE LESS THAN ZERO'
					endif
				endif
			endif	
		enddo

		if (INTPLT.eq.1) then
			call IDW(npi,xi,yi,fi,nc,x,nr,y,CM,NP,R0,FO,KER)
		else if (INTPLT.eq.2) then
1004			call LOTPS (1,NPPR,npi,xi,yi,fi,nc,x,nr,y,IWK,NIWK,
     $                NIWKU,WK,NWK,NWKU,fo,KER)
			IF (KER.eq.4) then
				PRINT *,'REVISE NIWK',NIWK,'	TO',NIWKU
				PRINT *,'REVISE NWK',NWK,'	TO',NWKU
				NIWK=max(NIWK,NIWKU) 
				NWK=max(NWK,NWKU)
				GOTO 1004
			ENDIF
		else if (INTPLT.eq.3) then
			call TS (npi,xi,yi,fi,nc,x,nr,y,FO,KER)
		else 
			STOP 'THE INTERPOLATE METHOD SHOULD BE 1, 2 OR 3.'
		endif

		if (KER.eq.0) then
			do ii=1,nr
				do jj=1,nc
					if (fo(jj,ii).ge.0.0) then
						rsum(i,ii,jj)=fo(jj,ii)
					else
						rsum(i,ii,jj)=0.0
					endif
				enddo
			enddo		
		else
			GOTO 1007
		endif

	enddo
	RETURN
1007	PRINT *, 'ERROR OCCUR IN INTERPOLATE!'
	RETURN
	end