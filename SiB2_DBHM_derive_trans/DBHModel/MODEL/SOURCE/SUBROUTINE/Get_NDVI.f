	SUBROUTINE	Get_NDVI(year,month,ndvi,KER)
	implicit none
	include '../INCLUDE/common.inc'
	integer			year,month,KER
	real			ndvi(ndvi_nt,max_nr,max_nc)
	real			reallat(ndvi_nr,ndvi_nc),reallon(ndvi_nr,ndvi_nc)
	CHARACTER*80	LambF,FileLat
	CHARACTER*80	NDVI_dir,NDVIPre,Dname,FileLon
	CHARACTER*6		MMYYYY
	integer			nr,nc,ll1,ll2,m1,m2,k1,k2,l11,l22
	real			x0,y0,s
	integer			DayM(12),NT,l1,l2 !,Imkdir,makedirqq
	data			DayM /31,28,31,30,31,30,31,31,30,31,30,31/

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	TMAnciDir,TMLat,TMLon,Ten_dir,TenPre,M_dir,MPre
	integer			D_nc,D_nr,S_c,S_r,S_nc,S_nr,TMN_div
	common			/Read_NDVI_para_TenDM/TMAnciDir,TMLat,TMLon,
     $				D_nc,D_nr,S_c,S_r,S_nc,S_nr,Ten_dir,TenPre,
     $				M_dir,MPre,TMN_div
	KER=0
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	call strlen(LambNDVI,l11,l22)
c	Imkdir=makedirqq(LambNDVI(l11:l22))
	IF (NDVIID.lt.1.or.NDVIID.gt.3) GOTO 5007

	IF (NDVIID.eq.2) THEN	!For Daily
		Call Get_Ancil_latlon(reallat,reallon)
	ELSE	!For Monthly or Tendays
		Call Strlen(TMAnciDir,l1,l2)	
		Call Strlen(TMLat,m1,m2)	
		Call Strlen(TMLon,k1,k2)	
		FileLat = TMAnciDir(l1:l2)//TMLat(m1:m2)//'_SA'
		FileLon = TMAnciDir(l1:l2)//TMLon(k1:k2)//'_SA'
		open(1, file=FileLat ,status='old',ERR=1000)
		close(1)
		open(1, file=FileLon ,status='old',ERR=1000)
		close(1)
		GOTO 1001
1000		FileLat = TMAnciDir(l1:l2)//TMLat(m1:m2)
		FileLon = TMAnciDir(l1:l2)//TMLon(k1:k2)
		Call Abstract_SA(FileLat,D_nc,D_nr,S_c,S_r,S_nc,S_nr,2,KER)
		IF (KER.NE.0) GOTO 5008
		Call Abstract_SA(FileLon,D_nc,D_nr,S_c,S_r,S_nc,S_nr,2,KER)
		IF (KER.NE.0) GOTO 5008
		FileLat = TMAnciDir(l1:l2)//TMLat(m1:m2)//'_SA'
		FileLon = TMAnciDir(l1:l2)//TMLon(k1:k2)//'_SA'
1001		call Read_ancillary_file(FileLat,FileLon,
     $		ndvi_nc,ndvi_nr,reallat,reallon,ndvi_nc,ndvi_nr)     	
      ENDIF     	
		
	Call ConvMMYYYY(month,year,MMYYYY)
	IF (NDVIID.eq.2) THEN	!For Daily
		NT=DayM(month)
		if (mod(year,4).eq.0.and.month.eq.2) NT=29
		if (mod(year,4).ne.0.and.month.eq.2) NT=28	
		LambF=LambNDVI(l11:l22)//MMYYYY//'_NDVI.bin'
		open(1, file=LambF ,status='old',ERR=5020)
		close(1)
		GOTO 5021
5020		PRINT *, 'GET DAILY NDVI. YEAR:',YEAR,'MONTH:',MONTH
		Call Get_daily_NDVI(Year,month,NT,ndvi,reallat,reallon,KER)
		PRINT *, KER
		PRINT *, 'WRITE LAMF FOR NEXT USAGE.',LambF
		Call WR_binary(LambF,0,NT,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
5021		Call WR_binary(LambF,1,NT,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
	ELSE	!For Monthly or Tendays		
		if (NDVIID.eq.1) then
			LambF=LambNDVI(l11:l22)//MMYYYY//'Month_NDVI.bin'
		else
			LambF=LambNDVI(l11:l22)//MMYYYY//'TenD_NDVI.bin'
		endif
		Print *, LambF

		open(1, file=LambF ,status='old',ERR=5000)
		close(1)
		GOTO 5001
5000		if (NDVIID.eq.1) then
			PRINT *, 'GET Monthly NDVI. YEAR:',YEAR,'MONTH:',MONTH
		else
			PRINT *, 'GET Tendays NDVI. YEAR:',YEAR,'MONTH:',MONTH
		endif
		Call Get_TenDM_NDVI(Year,month,ndvi,reallat,reallon,KER)
		Call Strlen(LambF,ll1,ll2)
		If (KER.ne.0) then
		PRINT *, 'NDVI DATA IN YEAR',YEAR,' MONTH',MONTH,' IS UNAVAILABLE'
		endif
		Call WR_binary(LambF,0,NDVIID,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
5001		Call WR_binary(LambF,1,NDVIID,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
	ENDIF

	RETURN
5007	KER=1
	PRINT *,'NDVIID SHOUL BE 1(MONTHLY),2(DAILY), OR 3(Tendays).'
	RETURN
5008	KER=2
	PRINT *,'ERROR IN GET STUDY AREA FROM ANCILLARY TMLat./TMLon. FILES.'
	RETURN

	end

