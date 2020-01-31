c Get daily potential ET (only get one day potential ET)
c Input:
c	Year:	the year the daily potential ET is required
c	Month:	The month
c	Day:	The date
c Output:
c	ET:  The potential ET value

c	if ETID = 1 calculate reference potential ET (FAO_Penman_Monteith reference crop)
c	if ETID<>1	the parameters is required (not finished, 2004/12/15)
	subroutine	Get_potential_ET_d(year,month,day,ETday_o)
	implicit none
	include '../INCLUDE/common.inc'
	include '../SIB2/SiB2River.inc'
	integer	days_fromstart,l1,l2,m1,m2 !,Imkdir,makedirqq !,mm,nn,ll
	real x0,y0,s
	character*80	WAY,ET_file
	character*4		YYYY
	integer year,month,day,nt,startyear ,i,j !,k,nodata
	real ETday_o(max_nr,max_nc)

	integer			ETID
	character*80	DEM_FD,FRAC_FD,ETPATH
	common			/Get_ET_para/ETID,DEM_FD,FRAC_FD,ETPATH
	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI	

	real			ftnd,Alai1,Alai2,Alai3,y11,alpha
	real			fave,fmin,fmax,areasum

	startyear=year	
	nt=days_fromstart(startyear,1,1,startyear,month,day)

	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	Write (YYYY,'(i4)') year
	Call Strlen(ETPATH,l1,l2)
c	Imkdir= makedirqq(ETPATH(l1:l2))
	if (INTPLT.eq.1) then
		WAY='IDW'
	else if (INTPLT.eq.2) then
		WAY='TPS'
	else if (INTPLT.eq.3) then	
		WAY='TS'
	else
		WAY='UNKNOWN'
	endif
	call strlen(WAY,m1,m2)
	IF (ETID.eq.1) then
	ET_file=ETPATH(l1:l2)//YYYY//'_ReferET_'//WAY(m1:m2)//'.bin'
	ELSE
	ET_file=ETPATH(l1:l2)//YYYY//'_PTET_'//WAY(m1:m2)//'.bin'
	ENDIF


	open(111, file=ET_file ,status='old',ERR=2011)
	close(111)
	call WR_binary_nt(ET_file,1,nt,nr,nc,ETday_o,max_nr,max_nc)
	nt=days_fromstart(1960,1,1,year,month,day)
	IF (nc_cl.eq.0) THEN !Set NO Climate change scenario
		do i=1,max_nr
		do j=1,max_nc
			if (fmm_et(i,j).ge.0.0) then
				ETday_o(i,j)=ETday_o(i,j)
     $			-(fm_et(i,j)*(nt)/365.+fb_et(i,j))/365.+fav_et(i,j)/365.	
			else
				y11 = ETday_o(i,j)
     $			-(fm_et(i,j)*(nt)/365.+fb_et(i,j))/365.+fav_et(i,j)/365.	
				alpha = ( fmm_et(i,j)-0.0 )/
     $				( fmm_et(i,j)- fav_et(i,j)/365.)
				ETday_o(i,j)=y11-alpha*(y11-fav_et(i,j)/365.)
			endif
		enddo
		enddo		
	ENDIF 

			CALL ana_DATAF(ETday_o,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(ETday_o,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_et,*) year,nt,Alai1*365.25,Alai2*365.25


	RETURN
2011	Print *, 'File NOT exist:',ET_file
	PRINT *,'You can not use subroutine Get_potential_ET_d,'
	PRINT *,'Please use subroutine Get_potential_ET first.'
	PRINT *,'Please run preprocess Refer_ET_old.f first.'
	STOP
2044	RETURN	
	end
