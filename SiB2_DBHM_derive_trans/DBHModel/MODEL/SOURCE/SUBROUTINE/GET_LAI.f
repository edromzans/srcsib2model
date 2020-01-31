c This subroutine is used to get LAI (monthly LAI)_extend to 1960
	SUBROUTINE	Get_LAI_ext(year,month,aLAI,KER)
	implicit none
	include '../INCLUDE/common.inc'		
	include '../SIB2/SiB2River.inc'
	integer		year,month,KER !,ID
	real		aLAI(max_nr,max_nc)
	real			ftnd,Alai1,Alai2,Alai3
	real			fave,fmin,fmax,areasum
	integer		i,j

	if (year.lt.1982) then
		CALL Get_LAI_nodata(year,month,aLAI,KER)
	else
		CALL Get_LAI(year,month,aLAI,KER)
		IF (nc_vg.eq.0) THEN !Set NO Vegetation change scenario
		do i=1,max_nr
		do j=1,max_nc
			ftnd = ( (1960+1969)*0.5 -1982+1)*fm_LAI(i,j) + fb_LAI(i,j)
			if (alai(i,j).ge.0. .and. alai(i,j).le.100.) then
				alai(i,j) = alai(i,j)-( fm_LAI(i,j)*(year-1982+1+month/12.) 
     $			+fb_LAI(i,j) )/12.+ftnd/12.
				alai(i,j) = max ( alai(i,j) , 0. )
				alai(i,j) = min ( alai(i,j) , 6. )
			else
				alai(i,j) = -9999.
			endif
		enddo
		enddo
		ENDIF
	endif

		CALL ana_DATAF(alai,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
		CALL ana_DATAF(alai,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
		CALL ana_DATAF(alai,gridarea,
     $		fracdd,irrcode,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai3=fave		
		write(inpt_lai,*) year,month,Alai1,Alai2,Alai3

	END  !SUBROUTINE Get_LAI_ext

	SUBROUTINE	Get_LAI_nodata(year,month,aLAI,KER)
	implicit none
	include '../INCLUDE/common.inc'
	include '../SIB2/SiB2River.inc'
	integer		year,month,KER !,ID
	real			aLAI(max_nr,max_nc)
	integer   i,j
	real			ftnd
	
	do i=1,max_nr
	do j=1,max_nc
		if (year.lt.1982) then
			IF (nc_vg.eq.0) THEN !Set NO Vegetation change scenario
				ftnd = ( (1960+1969)*0.5 -1982+1)*fm_LAI(i,j) + fb_LAI(i,j)
			ELSE
				ftnd = (year-1982+1)*fm_LAI(i,j) + fb_LAI(i,j)
			ENDIF
			if (fsave_lai(month,i,j).ge.0.) then
				alai(i,j) = fsave_lai(month,i,j)*ftnd 
				alai(i,j) = max ( alai(i,j) , 0. )
				alai(i,j) = min ( alai(i,j) , 6. )
			else
				alai(i,j) = -9999.
			endif
		endif
	enddo
	enddo

	END  !SUBROUTINE Get_LAI_nodata


c This subroutine is used to get LAI (monthly LAI)
	SUBROUTINE	Get_LAI(year,month,LAI,KER)
	implicit none
	include '../INCLUDE/common.inc'
		
	integer		year,month,KER !,ID
	real		LAI(max_nr,max_nc)

	real			reallat(LAI_nr,LAI_nc),reallon(LAI_nr,LAI_nc)
c	CHARACTER*80	LambF,FileLat
c	CHARACTER*80	NDVI_dir,NDVIPre,Dname,FileLon
	CHARACTER*4		YYMM
	integer			nr,nc,ll1,ll2,m1,m2,k1,k2,l11,l22 !,Imkdir,makedirqq
	integer			l1,l2,kk1,kk2 !,j,k
	real			x0,y0,s
	character*80	LambLAI,FileLat,FileLon,LambF,LAI_SA !,tmpname
c	integer			DayM(12),NT,l1,l2,Imkdir
c	data			DayM /31,28,31,30,31,30,31,31,30,31,30,31/

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	LAI_AnciDir,LAI_Lat,LAI_Lon,LAI_dir,LAI_Pre
	integer			LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,LAI_S_nc
	integer			LAI_S_nr,LAI_N_div
	common			/Read_LAI_para/LAI_AnciDir,LAI_Lat,LAI_Lon,
     $				LAI_dir,LAI_Pre,LAI_D_nc,LAI_D_nr,LAI_S_c,
     $				LAI_S_r,LAI_S_nc,LAI_S_nr,LAI_N_div
	KER=0
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	LambLAI=LAI_dir
	call strlen(LambLAI,l11,l22)
	Call Strlen(LAI_Pre,kk1,kk2)	

c READ LON/LAT FILE, Get the latitude and longitude
	Call Strlen(LAI_AnciDir,l1,l2)	
	Call Strlen(LAI_Lat,m1,m2)	
	Call Strlen(LAI_Lon,k1,k2)	
	FileLat = LAI_AnciDir(l1:l2)//LAI_Lat(m1:m2)//'_SA'
	FileLon = LAI_AnciDir(l1:l2)//LAI_Lon(k1:k2)//'_SA'

	open(1, file=FileLat ,status='old',ERR=1000)
	close(1)
	open(1, file=FileLon ,status='old',ERR=1000)
	close(1)
	GOTO 1001
1000	FileLat = LAI_AnciDir(l1:l2)//LAI_Lat(m1:m2)
	FileLon = LAI_AnciDir(l1:l2)//LAI_Lon(k1:k2)

	Call Abstract_SA(FileLat,LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,
     $	LAI_S_nc,LAI_S_nr,2,KER)
	IF (KER.NE.0) GOTO 5008
	Call Abstract_SA(FileLon,LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,
     $	LAI_S_nc,LAI_S_nr,2,KER)
	IF (KER.NE.0) GOTO 5008
	FileLat = LAI_AnciDir(l1:l2)//LAI_Lat(m1:m2)//'_SA'
	FileLon = LAI_AnciDir(l1:l2)//LAI_Lon(k1:k2)//'_SA'
1001	call Read_ancil_file_2(FileLat,FileLon,
     $		LAI_nc,LAI_nr,reallat,reallon,LAI_nc,LAI_nr,0.0,0.01)     	
c	tmpname='lat1.asc'	
c	call writefile_float (tmpname,LAI_nr,LAI_nc,x0,y0,s,
c     $	-9999.0,reallat,LAI_nr,LAI_nc)
c		stop

	Call ConvYYMM(month,year,YYMM)
	LambF=LambLAI(l11:l22)//'Lamb_'//YYMM//'_LAI.bin'

	open(1, file=LambF ,status='old',ERR=5000)
	close(1)
	GOTO 5001
5000	LAI_SA=LAI_dir(l11:l22)//LAI_Pre(kk1:kk2)//YYMM//'_SA'

	open(1, file=LAI_SA ,status='old',ERR=3005)
	close(1)
	GOTO 3006
c	Abstract Study Area from Asia or Global if it is not existing
3005	LAI_SA=LAI_dir(l11:l22)//LAI_Pre(kk1:kk2)//YYMM
	Call Abstract_SA(LAI_SA,LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,
     $	LAI_S_nc,LAI_S_nr,1,KER)
	LAI_SA=LAI_dir(l11:l22)//LAI_Pre(kk1:kk2)//YYMM//'_SA'
c	If the files are abstracted, use the existing files
c	Resample to Lambert projection
3006	call R_FPAR_SA(LAI_SA,nr,nc,x0,y0,s,LAI_N_div,
     $		reallat,reallon,LAI,0.04,KER)

	Call Strlen(LambF,ll1,ll2)
	If (KER.ne.0) then
	PRINT *, 'LAI DATA IN YEAR',YEAR,' MONTH',MONTH,' IS UNAVAILABLE'
	endif
	Call WR_binary_1(LambF,0,nr,nc,LAI,max_nr,max_nc)
5001	Call WR_binary_1(LambF,1,nr,nc,LAI,max_nr,max_nc)

	RETURN
5008	KER=2
	PRINT *,'ERROR IN GET STUDY AREA FROM ANCILLARY TMLat./TMLon. FILES.'
	RETURN

	END