c	This subroutine try to get Atmospheric forcing data (One ROW) (in Lambert Projecion)
c	Input: Year
c			Parameters is read from '../PARAMETER/Input_para'
c	Output: tm ... etc. atmospheric foring data (for FAO Penman_M equation)
c			For the first runing, the results will be saved in ..ATM_ITP/
	subroutine	Get_Grid_cld_sun_ij(year,n_summ,sun,inr)
	implicit none
	include '../INCLUDE/common.inc'
	integer startyear,endyear,nt,nr,nc,year,irecl
	character*80	FOUT(2),YYYY,WAY
	real x0,y0,s
	integer	l1,l2,ll1,ll2,c1,c2
	integer	days_fromstart,i,j,inr

	real n_summ(max_time,max_nc)	!Cloud cover data after interpolate
	real sun(max_time,max_nc)		!Sunshine Time data after interpolate

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

	startyear=year	
	
c	Because the data size is huge, only calcaulate one year once
	endyear=startyear
	nt=days_fromstart(startyear,1,1,endyear,12,31)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	call strlen(ATM_ITP,c1,c2)
c	Imkdir=makedirqq(ATM_ITP(c1:c2))

	write(YYYY,*) startyear
	call strlen(YYYY,l1,l2)
	if (INTPLT.eq.1) then
		WAY='IDW'
	else if (INTPLT.eq.2) then
		WAY='TPS'
	else if (INTPLT.eq.3) then	
		WAY='TS'
	else
		WAY='UNKNOWN'
	endif
	call strlen(WAY,ll1,ll2)
	FOUT(1)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_n_summ_'//WAY(ll1:ll2)//'.bin'
	FOUT(2)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_sun_'//WAY(ll1:ll2)//'.bin'
	
c	Check the interpolate results, if the result files exist, read it, if not,ERROR
	do i=1,2
		open(1, file=FOUT(i) ,status='old',ERR=212)
		close(1)
	enddo

	open(100,form='unformatted',file=FOUT(1),access='direct',
     *		recl=4*nc,status='old')
	Do I=1,nt
		irecl=(I-1)*nr+inr
		READ(100,rec=irecl) (n_summ(I,J),J=1,nc)	
	Enddo
	CLOSE(100)	
	open(100,form='unformatted',file=FOUT(2),access='direct',
     *		recl=4*nc,status='old')
	Do I=1,nt
		irecl=(I-1)*nr+inr
		READ(100,rec=irecl) (sun(I,J),J=1,nc)
	Enddo
	CLOSE(100)	
	
	RETURN

212	PRINT *,'Data is unavailable! FILE MISS:',FOUT(i)
	STOP
	end