	SUBROUTINE	Get_NDVI_ij(year,month,ndvi_ij,inr,KER)
	implicit none
	include '../INCLUDE/common.inc'
	integer			year,month,KER,inr,irecl,I,J
	real			ndvi_ij(ndvi_nt,max_nc)
	CHARACTER*80	LambF
	CHARACTER*6		MMYYYY
	integer			nr,nc,l11,l22
	real			x0,y0,s
	integer			DayM(12),NT
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
	
	Call ConvMMYYYY(month,year,MMYYYY)
	IF (NDVIID.eq.2) THEN	!For Daily
		NT=DayM(month)
		if (mod(year,4).eq.0.and.month.eq.2) NT=29
		if (mod(year,4).ne.0.and.month.eq.2) NT=28	
		LambF=LambNDVI(l11:l22)//MMYYYY//'_NDVI.bin'
		open(1, file=LambF ,status='old',ERR=5020)
		close(1)
		GOTO 5021
5020		PRINT *, 'GET DAILY NDVI. ERROR YEAR:',YEAR,'MONTH:',MONTH
		STOP
c		Call WR_binary(LambF,1,NT,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
5021		open(100,form='unformatted',file=LambF,access='direct',
     *		recl=4*nc,status='old')
		Do I=1,NT
			irecl=(I-1)*nr+inr
			READ(100,rec=irecl) (ndvi_ij(I,J),J=1,nc)
		Enddo
		CLOSE(100)		
	ELSE	!For Monthly or Tendays		
		if (NDVIID.eq.1) then
			LambF=LambNDVI(l11:l22)//MMYYYY//'Month_NDVI.bin'
		else
			LambF=LambNDVI(l11:l22)//MMYYYY//'TenD_NDVI.bin'
		endif
		open(1, file=LambF ,status='old',ERR=5000)
		close(1)
		GOTO 5001
5000		if (NDVIID.eq.1) then
			PRINT *, 'GET Monthly NDVI. ERROR YEAR:',YEAR,'MONTH:',MONTH
		else
			PRINT *, 'GET Tendays NDVI. ERROR YEAR:',YEAR,'MONTH:',MONTH
		endif
		STOP
c		Call WR_binary(LambF,1,NDVIID,nr,nc,ndvi,ndvi_nt,max_nr,max_nc)
5001		open(100,form='unformatted',file=LambF,access='direct',
     *		recl=4*nc,status='old')
		Do I=1,NDVIID
			irecl=(I-1)*nr+inr
			READ(100,rec=irecl) (ndvi_ij(I,J),J=1,nc)	
		Enddo
		CLOSE(100)
	ENDIF

	RETURN
5007	KER=1
	PRINT *,'NDVIID SHOUL BE 1(MONTHLY),2(DAILY), OR 3(Tendays).'
	RETURN

	end

