c	This subroutine get Tendays/Month NDVI data under Lambert Projection
c	Input:
c		Year/Month:		The Year/Month when the NDVI is required
c		reallat,reallon:Ancillary TMLat/TMLon (from Get_Ancil_latlon)
c		NDVIID:				NDVIID=3, Tendays, NDVIID=1, Month, ELSE STOP
c	Output:
c		ndvi(T,nr,nc):	Tendays NDVI in the required month, T=1:1-10, T=2:11-20, T=3: 21-end of the month
c		KER:			KER=0, successful, KER.gt.0, Fail			
	subroutine Get_TenDM_NDVI(Year,month,ndvi,reallat,reallon,KER)
	implicit none
	include '../INCLUDE/common.inc'
	integer			Col0,Col1,Row0,Row1	
	CHARACTER*80	NDVI_file,Suffix
	integer			year,month
	character*6		MMYYYY
	integer			DayM(12)
	data			DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	CHARACTER*2		Ten(3)
	data			Ten /'01','11','21'/
	integer			l1,l2,ll1,ll2,s1,s2,KER
	real			reallat(ndvi_nr,ndvi_nc),reallon(ndvi_nr,ndvi_nc)
	real			ndvi_1(ndvi_nt,max_nr,max_nc)
	real			ndvi(ndvi_nt,max_nr,max_nc)
	CHARACTER*80	Dname

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	TMAnciDir,TMLat,TMLon,Ten_dir,TenPre,M_dir,MPre
	integer			D_nc,D_nr,S_c,S_r,S_nc,S_nr,TMN_div
	common			/Read_NDVI_para_TenDM/TMAnciDir,TMLat,TMLon,
     $				D_nc,D_nr,S_c,S_r,S_nc,S_nr,Ten_dir,TenPre,
     $				M_dir,MPre,TMN_div

	integer			i,j,k,nr,nc
	real			x0,y0,s

	KER=0
	if (NDVIID.ne.1.and.NDVIID.ne.3 )  GOTO 8008

	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	Call ConvMMYYYY(month,year,MMYYYY)
	Do i=1,NDVIID
	If (NDVIID.eq.1) then
		Call Strlen(M_dir,l1,l2)	
		Call Strlen(MPre,ll1,ll2)	
		Suffix=MMYYYY(5:6)//MMYYYY(1:2)	
		Call Strlen(Suffix,s1,s2)
		Dname=M_dir(l1:l2)//MPre(ll1:ll2)//Suffix(s1:s2)
		NDVI_file=M_dir(l1:l2)//MPre(ll1:ll2)//Suffix(s1:s2)//'_SA'			
	else
		Call Strlen(Ten_dir,l1,l2)	
		Call Strlen(TenPre,ll1,ll2)	
		Suffix=MMYYYY(5:6)//MMYYYY(1:2)//Ten(i)
		Call Strlen(Suffix,s1,s2)
		Dname=Ten_dir(l1:l2)//TenPre(ll1:ll2)//Suffix(s1:s2)
		NDVI_file=Ten_dir(l1:l2)//TenPre(ll1:ll2)//Suffix(s1:s2)//'_SA'	
	endif

	open(1, file=NDVI_file ,status='old',ERR=8005)
	close(1)

	GOTO 8006
c	Abstract Study Area from Asia or Global if it is not existing
8005	Call Abstract_SA(Dname,D_nc,D_nr,S_c,S_r,S_nc,S_nr,1,KER)
c	PRINT *, 'Abstract Study Area from Asia or Global'
	
c	If the files are abstracted, use the existing files
c	Resample to Lambert projection
8006	call R_NDVI_SA(NDVI_file,nr,nc,x0,y0,s,TMN_div,
     $		reallat,reallon,1,ndvi_1,KER)
	If (KER.eq.0) then
		do j=1,max_nr
			do k=1,max_nc
				ndvi(i,j,k)=ndvi_1(1,j,k)
			enddo
		enddo
	else
		do j=1,max_nr
			do k=1,max_nc
				ndvi(i,j,k)=-9999
			enddo
		enddo
	endif
	enddo
	RETURN
8008	KER=1
	PRINT *,'ERROR, NDVIID SHOULD BE 1 (MONTH DATA) OR 3 (TENDAYS DATA)'
	STOP
	RETURN
	end

