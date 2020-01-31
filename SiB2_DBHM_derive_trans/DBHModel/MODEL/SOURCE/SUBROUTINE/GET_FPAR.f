c This subroutine is used to get FPAR (monthly FPAR)_extend to 1960
	SUBROUTINE	Get_FPAR_ext(year,month,aFPAR,KER)
	implicit none
	include '../INCLUDE/common.inc'		
	include '../SIB2/SiB2River.inc'
	integer		year,month,KER !,ID
	real		aFPAR(max_nr,max_nc)
	real			ftnd,Afpar1,Afpar2,Afpar3
	real			fave,fmin,fmax,areasum
	integer		i,j

	if (year.lt.1982) then
		CALL Get_FPAR_nodata(year,month,aFPAR,KER)
	else
		CALL Get_FPAR(year,month,aFPAR,KER)
		IF (nc_vg.eq.0) THEN !Set NO Vegetation change scenario
		do i=1,max_nr
		do j=1,max_nc
			ftnd = ( (1960+1969)*0.5 -1982+1)*fm_FPAR(i,j) + fb_FPAR(i,j)
			if (aFPAR(i,j).ge.0. .and. aFPAR(i,j).le.100.) then
				aFPAR(i,j) = aFPAR(i,j)-( fm_FPAR(i,j)*(year-1982+1+month/12.) 
     $			+fb_FPAR(i,j) )/12.+ftnd/12.
				aFPAR(i,j) = max ( aFPAR(i,j) , 0. )
				aFPAR(i,j) = min ( aFPAR(i,j) , 1. )
			else
				aFPAR(i,j) = -9999.
			endif
		enddo
		enddo
		ENDIF
	endif

		CALL ana_DATAF(afpar,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Afpar1=fave
		CALL ana_DATAF(afpar,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Afpar2=fave
		CALL ana_DATAF(afpar,gridarea,
     $		fracdd,irrcode,max_nr,max_nc,fave,fmin,fmax,areasum)
			Afpar3=fave		
		write(inpt_fpar,*) year,month,Afpar1,Afpar2,Afpar3

	END  !SUBROUTINE Get_LAI_ext

	SUBROUTINE	Get_FPAR_nodata(year,month,aFPAR,KER)
	implicit none
	include '../INCLUDE/common.inc'
	include '../SIB2/SiB2River.inc'
	integer		year,month,KER !,ID
	real			aFPAR(max_nr,max_nc)
	integer   i,j
	real			ftnd
	
	do i=1,max_nr
	do j=1,max_nc
		if (year.lt.1982) then
			IF (nc_vg.eq.0) THEN !Set NO Vegetation change scenario
				ftnd = ((1960+1969)*0.5-1982+1)*fm_FPAR(i,j) + fb_FPAR(i,j)
			ELSE
				ftnd = (year-1982+1)*fm_FPAR(i,j) + fb_FPAR(i,j)
			ENDIF
			if (fsave_fpar(month,i,j).ge.0.) then
				aFPAR(i,j) = fsave_fpar(month,i,j)*ftnd 
				afpar(i,j) = max ( afpar(i,j) , 0. )
				afpar(i,j) = min ( afpar(i,j) , 1. )
			else
				afpar(i,j) = -9999.
			endif
		endif
	enddo
	enddo

	END  !SUBROUTINE Get_LAI_nodata

c This subroutine is used to get FPAR (monthly FPAR)
	SUBROUTINE	Get_FPAR(year,month,fpar,KER)
	implicit none
	include '../INCLUDE/common.inc'
		
	integer		year,month,KER !,ID
	real		fpar(max_nr,max_nc)

	real			reallat(fpar_nr,fpar_nc),reallon(fpar_nr,fpar_nc)
c	CHARACTER*80	LambF,FileLat
c	CHARACTER*80	NDVI_dir,NDVIPre,Dname,FileLon
	CHARACTER*4		YYMM
	integer			nr,nc,ll1,ll2,m1,m2,k1,k2,l11,l22 !,Imkdir,makedirqq
	integer			l1,l2,kk1,kk2 !,j,k
	real			x0,y0,s
	character*80	LambFPAR,FileLat,FileLon,LambF,FPAR_SA !,tmpname
c	integer			DayM(12),NT,l1,l2,Imkdir
c	data			DayM /31,28,31,30,31,30,31,31,30,31,30,31/

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	FPAR_AnciDir,FPAR_Lat,FPAR_Lon,FPAR_dir,FPAR_Pre
	integer			FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,FPAR_S_nc
	integer			FPAR_S_nr,FPAR_N_div
	common			/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR_dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div
	KER=0
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	LambFPAR=FPAR_dir
	call strlen(LambFPAR,l11,l22)
	Call Strlen(FPAR_Pre,kk1,kk2)	

c READ LON/LAT FILE, Get the latitude and longitude
	Call Strlen(FPAR_AnciDir,l1,l2)	
	Call Strlen(FPAR_Lat,m1,m2)	
	Call Strlen(FPAR_Lon,k1,k2)	
	FileLat = FPAR_AnciDir(l1:l2)//FPAR_Lat(m1:m2)//'_SA'
	FileLon = FPAR_AnciDir(l1:l2)//FPAR_Lon(k1:k2)//'_SA'

	open(1, file=FileLat ,status='old',ERR=1000)
	close(1)
	open(1, file=FileLon ,status='old',ERR=1000)
	close(1)
	GOTO 1001
1000	FileLat = FPAR_AnciDir(l1:l2)//FPAR_Lat(m1:m2)
	FileLon = FPAR_AnciDir(l1:l2)//FPAR_Lon(k1:k2)

	Call Abstract_SA(FileLat,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,
     $	FPAR_S_nc,FPAR_S_nr,2,KER)
	IF (KER.NE.0) GOTO 5008
	Call Abstract_SA(FileLon,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,
     $	FPAR_S_nc,FPAR_S_nr,2,KER)
	IF (KER.NE.0) GOTO 5008
	FileLat = FPAR_AnciDir(l1:l2)//FPAR_Lat(m1:m2)//'_SA'
	FileLon = FPAR_AnciDir(l1:l2)//FPAR_Lon(k1:k2)//'_SA'
1001	call Read_ancil_file_2(FileLat,FileLon,
     $		fpar_nc,fpar_nr,reallat,reallon,fpar_nc,fpar_nr,0.0,0.01)     	
c	tmpname='lat1.asc'	
c	call writefile_float (tmpname,fpar_nr,fpar_nc,x0,y0,s,
c     $	-9999.0,reallat,fpar_nr,fpar_nc)
c		stop

	Call ConvYYMM(month,year,YYMM)
	LambF=LambFPAR(l11:l22)//'Lamb_'//YYMM//'_FPAR.bin'

	open(1, file=LambF ,status='old',ERR=5000)
	close(1)
	GOTO 5001
5000	FPAR_SA=FPAR_dir(l11:l22)//FPAR_Pre(kk1:kk2)//YYMM//'_SA'

	open(1, file=FPAR_SA ,status='old',ERR=3005)
	close(1)
	GOTO 3006
c	Abstract Study Area from Asia or Global if it is not existing
3005	FPAR_SA=FPAR_dir(l11:l22)//FPAR_Pre(kk1:kk2)//YYMM
	Call Abstract_SA(FPAR_SA,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,
     $	FPAR_S_nc,FPAR_S_nr,1,KER)
	FPAR_SA=FPAR_dir(l11:l22)//FPAR_Pre(kk1:kk2)//YYMM//'_SA'
c	If the files are abstracted, use the existing files
c	Resample to Lambert projection
3006	call R_FPAR_SA(FPAR_SA,nr,nc,x0,y0,s,FPAR_N_div,
     $		reallat,reallon,fpar,0.004,KER)

	Call Strlen(LambF,ll1,ll2)
	If (KER.ne.0) then
	PRINT *, 'FPAR DATA IN YEAR',YEAR,' MONTH',MONTH,' IS UNAVAILABLE'
	endif
	Call WR_binary_1(LambF,0,nr,nc,fpar,max_nr,max_nc)
5001	Call WR_binary_1(LambF,1,nr,nc,fpar,max_nr,max_nc)

	RETURN
5008	KER=2
	PRINT *,'ERROR IN GET STUDY AREA FROM ANCILLARY TMLat./TMLon. FILES.'
	RETURN

	END