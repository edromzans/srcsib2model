c	This subroutine try to get Atmospheric forcing data (in Lambert Projecion)
c	Input: Year,month,day
c			Parameters is read from '../PARAMETER/Input_para'
c	Output: tm ... etc. atmospheric foring data
c			The annual interpolated data should be saved in ..ATM_ITP/
c			This subroutine only read the interpolated results
	subroutine	Get_Grid_ATM_d(year,month,day,tm,
     $	tmax,tmin,um,n_summ,fsm,rsum,sun)
	implicit none
	include '../INCLUDE/common.inc'
	include '../SIB2/SiB2River.inc'
c	include '../INCLUDE/Def_Read_foring.inc'
	integer startyear,nt,year,month,day !,n_stn
	character*80	FOUT(8),YYYY,WAY ,FName
	real x0,y0,s
	integer	l1,l2,ll1,ll2,c1,c2 !,Imkdir,makedirqq
	integer	days_fromstart,i, j !,k,ifile

	real tm(max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_nr,max_nc)	!Sunshine Time data after interpolate

	real			ftnd,Alai1,Alai2,Alai3,y11,alpha
	real			fave,fmin,fmax,areasum

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	real fnodata

	startyear=year	
	
	nt=days_fromstart(startyear,1,1,startyear,month,day)
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
	FOUT(1)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tm_'//WAY(ll1:ll2)//'.bin'
	FOUT(2)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tmax_'//WAY(ll1:ll2)//'.bin'
	FOUT(3)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tmin_'//WAY(ll1:ll2)//'.bin'
	FOUT(4)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_um_'//WAY(ll1:ll2)//'.bin'
	FOUT(5)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_n_summ_'//WAY(ll1:ll2)//'.bin'
	FOUT(6)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_fsm_'//WAY(ll1:ll2)//'.bin'
	FOUT(7)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_rsum_'//WAY(ll1:ll2)//'.bin'
	FOUT(8)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_sun_'//WAY(ll1:ll2)//'.bin'
	
c	Check the interpolate results, if the result files exist, read it, if not, STOP
	do i=1,8
		open(1, file=FOUT(i) ,status='old',ERR=2112)
		close(1)
	enddo

c	PRINT *,'Read existing Grid Atmospheric Data from'//ATM_ITP(c1:c2)
	call WR_binary_nt(FOUT(1),1,nt,nr,nc,tm,max_nr,max_nc)
	call WR_binary_nt(FOUT(2),1,nt,nr,nc,tmax,max_nr,max_nc)
	call WR_binary_nt(FOUT(3),1,nt,nr,nc,tmin,max_nr,max_nc)
	call WR_binary_nt(FOUT(4),1,nt,nr,nc,um,max_nr,max_nc)
	call WR_binary_nt(FOUT(5),1,nt,nr,nc,n_summ,max_nr,max_nc)
	call WR_binary_nt(FOUT(6),1,nt,nr,nc,fsm,max_nr,max_nc)
	call WR_binary_nt(FOUT(7),1,nt,nr,nc,rsum,max_nr,max_nc)
	call WR_binary_nt(FOUT(8),1,nt,nr,nc,sun,max_nr,max_nc)

	nt=days_fromstart(1960,1,1,year,month,day)
c	FName='asun'
c	call wr_fb(FName,max_nr,max_nc,x0,y0,s,
c     $	fnodata,fb_sun,max_nr,max_nc)
c	FName='bsun'
c	call wr_fb(FName,max_nr,max_nc,x0,y0,s,
c     $	fnodata,fm_sun,max_nr,max_nc)
c	FName='minsun'
c	call wr_fb(FName,max_nr,max_nc,x0,y0,s,
c     $	fnodata,fmm_sun,max_nr,max_nc)
c	FName='avysun'
c	call wr_fb(FName,max_nr,max_nc,x0,y0,s,
c     $	fnodata,fav_sun,max_nr,max_nc)
c	print *,'print out'
c	stop

	IF (nc_cl.eq.0) THEN !Set NO Climate change scenario
		if (nt.eq.1) print *, "Set NO Climate change scenario"
		do i=1,max_nr
		do j=1,max_nc
			tm(i,j)=tm(i,j)
     $			-(fm_tm(i,j)*(nt)/365.+fb_tm(i,j))+fav_tm(i,j)		

			tmax(i,j)=tmax(i,j)
     $			-(fm_tmax(i,j)*(nt)/365.+fb_tmax(i,j))+fav_tmax(i,j)		

			tmin(i,j)=tmin(i,j)
     $			-(fm_tmin(i,j)*(nt)/365.+fb_tmin(i,j))+fav_tmin(i,j)		

			if (fmm_um(i,j).ge.0.0) then
				um(i,j)=um(i,j)
     $			-(fm_um(i,j)*(nt)/365.+fb_um(i,j))+fav_um(i,j)	
			else
				y11 = um(i,j)
     $			-(fm_um(i,j)*(nt)/365.+fb_um(i,j))+fav_um(i,j)	
				alpha = ( fmm_um(i,j)-0.0 )/
     $				( fmm_um(i,j)- fav_um(i,j))
				um(i,j)=y11-alpha*(y11-fav_um(i,j))
			endif   
			um(i,j)=max(um(i,j),0.0)
			
			if (fmm_rsum(i,j).ge.0.0) then
				rsum(i,j)=rsum(i,j)
     $			-(fm_rsum(i,j)*(nt)/365.+fb_rsum(i,j))/365.+fav_rsum(i,j)/365.	
			else
				y11 = rsum(i,j)
     $			-(fm_rsum(i,j)*(nt)/365.+fb_rsum(i,j))/365.+fav_rsum(i,j)/365.	
				alpha = ( fmm_rsum(i,j)-0.0 )/
     $				( fmm_rsum(i,j)- fav_rsum(i,j)/365.)
				rsum(i,j)=y11-alpha*(y11-fav_rsum(i,j)/365.)
			endif
			rsum(i,j)=max(0.0,rsum(i,j))

c			sun(i,j)=sun(i,j)
c     $			-(fm_sun(i,j)*(nt)/365.+fb_sun(i,j))+fav_sun(i,j)	

			if (fmm_sun(i,j).ge.0.0) then
				sun(i,j)=sun(i,j)
     $			-(fm_sun(i,j)*(nt)/365.+fb_sun(i,j))+fav_sun(i,j)	
			else
				y11 = sun(i,j)
     $			-(fm_sun(i,j)*(nt)/365.+fb_sun(i,j))+fav_sun(i,j)	
				alpha = ( fmm_sun(i,j)-0.0 )/
     $				( fmm_sun(i,j)- fav_sun(i,j))
				sun(i,j)=y11-alpha*(y11-fav_sun(i,j))
			endif     
			sun(i,j) = max(sun(i,j), 0.0)
		enddo
		enddo

	ENDIF 

			CALL ana_DATAF(tm,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(tm,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_tm,*) year,nt,Alai1,Alai2

			CALL ana_DATAF(tmax,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(tmax,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_tmax,*) year,nt,Alai1,Alai2

			CALL ana_DATAF(tmin,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(tmin,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_min,*) year,nt,Alai1,Alai2

			CALL ana_DATAF(um,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(um,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_um,*) year,nt,Alai1,Alai2

			CALL ana_DATAF(rsum,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(rsum,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_rsum,*) year,nt,Alai1*365.25,Alai2*365.25

			CALL ana_DATAF(sun,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(sun,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_sun,*) year,nt,Alai1,Alai2
					
	RETURN

2112	PRINT *,'The interpoalted data does not exist!'
	PRINT *,'You can not use subroutine Get_Grid_ATM_d,'
	PRINT *,'Please use subroutine Get_Grid_ATM first.'
	STOP

	end
