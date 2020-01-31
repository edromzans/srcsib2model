c	This subroutine try to get Atmospheric forcing data (in Lambert Projecion)
c	Input: Year
c			Parameters is read from '../PARAMETER/Input_para'
c	Output: tm ... etc. atmospheric foring data (for FAO Penman_M equation)
c			For the first runing, the results will be saved in ..ATM_ITP/
	subroutine	Get_Grid_ATM(year,tm,tmax,tmin,um,n_summ,fsm,rsum,sun)
	implicit none
	include '../INCLUDE/Def_Read_foring.inc'
	integer startyear,endyear,nt,nr,nc,n_stn,year
	character*80	FOUT(8),YYYY,WAY,STN_F
	real x0,y0,s
	integer	l1,l2,ll1,ll2,c1,c2 !,Imkdir,makedirqq
	integer	days_fromstart,i,j,k,ifile,nrecord(8)

	real tm(max_time,max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_time,max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_time,max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_time,max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_time,max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_time,max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_time,max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_time,max_nr,max_nc)	!Sunshine Time data after interpolate

c	Argument for ASCII OUTPUT
	real tmp(max_time,max_nr,max_nc)	!TMP
	real AVGtmp(max_nr,max_nc),AVG(max_stn,8)
	
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
	FOUT(1)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tm_'//WAY(ll1:ll2)//'.bin'
	FOUT(2)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tmax_'//WAY(ll1:ll2)//'.bin'
	FOUT(3)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_tmin_'//WAY(ll1:ll2)//'.bin'
	FOUT(4)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_um_'//WAY(ll1:ll2)//'.bin'
	FOUT(5)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_n_summ_'//WAY(ll1:ll2)//'.bin'
	FOUT(6)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_fsm_'//WAY(ll1:ll2)//'.bin'
	FOUT(7)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_rsum_'//WAY(ll1:ll2)//'.bin'
	FOUT(8)=ATM_ITP(c1:c2)//YYYY(l1:l2)//'_sun_'//WAY(ll1:ll2)//'.bin'
	
c	Check the interpolate results, if the result files exist, read it, if not, calculate it
	do i=1,8
		open(1, file=FOUT(i) ,status='old',ERR=212)
		close(1)
	enddo

	PRINT *,'Read existing Grid Atmospheric Data from'//ATM_ITP(c1:c2)
	call WR_binary(FOUT(1),1,nt,nr,nc,tm,max_time,max_nr,max_nc)
	call WR_binary(FOUT(2),1,nt,nr,nc,tmax,max_time,max_nr,max_nc)
	call WR_binary(FOUT(3),1,nt,nr,nc,tmin,max_time,max_nr,max_nc)
	call WR_binary(FOUT(4),1,nt,nr,nc,um,max_time,max_nr,max_nc)
	call WR_binary(FOUT(5),1,nt,nr,nc,n_summ,max_time,max_nr,max_nc)
	call WR_binary(FOUT(6),1,nt,nr,nc,fsm,max_time,max_nr,max_nc)
	call WR_binary(FOUT(7),1,nt,nr,nc,rsum,max_time,max_nr,max_nc)
	call WR_binary(FOUT(8),1,nt,nr,nc,sun,max_time,max_nr,max_nc)
	
	RETURN

212	PRINT *,'Read Data and INTERPOLATE to Grid for the first runing.'
	PRINT *,'It might take a long time, please be patient...'

	call Read_Interpolate_data(startyear,endyear,GEO_hrd,n_stn,stn,
     $	INTPLT,nr,nc,tm,tmax,tmin,um,n_summ,fsm,rsum,sun)

c	Write Interpolate results into Binary Files
c	The Binary Files will be used as input to DHM is the Files are available
	PRINT *,'Save the interpolate results for Next Time Usage'
	call WR_binary(FOUT(1),0,nt,nr,nc,tm,max_time,max_nr,max_nc)
	call WR_binary(FOUT(2),0,nt,nr,nc,tmax,max_time,max_nr,max_nc)
	call WR_binary(FOUT(3),0,nt,nr,nc,tmin,max_time,max_nr,max_nc)
	call WR_binary(FOUT(4),0,nt,nr,nc,um,max_time,max_nr,max_nc)
	call WR_binary(FOUT(5),0,nt,nr,nc,n_summ,max_time,max_nr,max_nc)
	call WR_binary(FOUT(6),0,nt,nr,nc,fsm,max_time,max_nr,max_nc)
	call WR_binary(FOUT(7),0,nt,nr,nc,rsum,max_time,max_nr,max_nc)
	call WR_binary(FOUT(8),0,nt,nr,nc,sun,max_time,max_nr,max_nc)

c	Summary the Averaged interpolated value and output as ASCII file
c	The files is used to check the interpolation and ouput, will not be used in next step 
c	PRINT *, 'Save the Averaged interpolated data in this year for CHECK'
	do ifile=1,8
		call WR_binary(FOUT(ifile),1,nt,nr,nc,tmp,max_time,max_nr,max_nc)
		do j=1,nr
			do k=1,nc
				AVGtmp(j,k)=0.0
			enddo
		enddo
		do i=1,nt
			do j=1,nr
				do k=1,nc
					AVGtmp(j,k)=AVGtmp(j,k)+tmp(i,j,k)/real(nt)
				enddo
			enddo
		enddo
		call strlen(FOUT(ifile),ll1,ll2)
	FOUT(ifile)=ATM_ITP(c1:c2)//'AVG_'//FOUT(ifile)(c2-c1+2:ll2-3)//'asc'
		call writefile_float(FOUT(ifile),nr,nc,x0,
     $	y0,s,-9999.0,AVGtmp,max_nr,max_nc)
	enddo

c	Summary the averaged observation data of each station and save as ASCII file
c	The file is used to check the interpolation and output, will not be used in next step
c	PRINT *, 'Save the Averaged observation data in this year for CHECK'
	STN_F=ATM_ITP(c1:c2)//'AVG_'//YYYY(l1:l2)//'_STATION.asc'
	open(1, file=STN_F, status='unknown', ERR=313)
	write (1,*) 'ID  X  Y  tm  tmax  tmin  um  n_summ  fsm  rsum  sun'
	do i=1,n_stn
		do j=1,8
			AVG(i,j) = 0.
			nrecord(j)=0
		enddo
		do j=1,nt
			if (stn(i).nodata(j).ne.0) then
				if (stn(i).tm(j).ne.-99999.0) then
				AVG(i,1)=AVG(i,1)+stn(i).tm(j)
				nrecord(1)=nrecord(1)+1
				endif
				if (stn(i).tmax(j).ne.-99999.0) then
				AVG(i,2)=AVG(i,2)+stn(i).tmax(j)
				nrecord(2)=nrecord(2)+1
				endif
				if (stn(i).tmin(j).ne.-99999.0) then
				AVG(i,3)=AVG(i,3)+stn(i).tmin(j)
				nrecord(3)=nrecord(3)+1
				endif
				if (stn(i).um(j).ne.-99999.0) then
				AVG(i,4)=AVG(i,4)+stn(i).um(j)
				nrecord(4)=nrecord(4)+1
				endif
				if (stn(i).n_summ(j).ne.-99999.0) then
				AVG(i,5)=AVG(i,5)+stn(i).n_summ(j)
				nrecord(5)=nrecord(5)+1
				endif
				if (stn(i).fsm(j).ne.-99999.0) then
				AVG(i,6)=AVG(i,6)+stn(i).fsm(j)
				nrecord(6)=nrecord(6)+1
				endif
				if (stn(i).rsum(j).ne.-99999.0) then
				AVG(i,7)=AVG(i,7)+stn(i).rsum(j)
				nrecord(7)=nrecord(7)+1
				endif
				if (stn(i).sun(j).ne.-99999.0) then
				AVG(i,8)=AVG(i,8)+stn(i).sun(j)
				nrecord(8)=nrecord(8)+1
				endif
			endif
		enddo
		do j=1,8
			if (nrecord(j).ne.0) then
			AVG(i,j) = AVG(i,j)/nrecord(j)	
			else
			AVG(i,j) = -9999.
			endif
		enddo
		write (1,'(I8,10f15.3)') 
     $		stn(i).id,stn(i).x,stn(i).y, (AVG(i,j),j=1,8)
	enddo
	close(1)

313	PRINT *,'The Interpolation Results are Saved.'

	end


c	This subroutine try to get Atmospheric forcing data (in Lambert Projecion)
c	Input: Year,month,day
c			Parameters is read from '../PARAMETER/Input_para'
c	Output: tm ... etc. atmospheric foring data
c			The annual interpolated data should be saved in ..ATM_ITP/
c			This subroutine only read the interpolated results
	subroutine	Get_Grid_ATM_d(year,month,day,tm,
     $	tmax,tmin,um,n_summ,fsm,rsum,sun)
	implicit none
	include '../INCLUDE/Def_Read_foring.inc'
	include '../SIB2/SiB2River.inc'
	integer startyear,nt,nr,nc,n_stn,year,month,day
	character*80	FOUT(8),YYYY,WAY,STN_F
	real x0,y0,s
	integer	l1,l2,ll1,ll2,c1,c2 !,Imkdir,makedirqq
	integer	days_fromstart,i,j,k,ifile

	real tm(max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax(max_nr,max_nc)	!Max temperature data after interpolate
	real tmin(max_nr,max_nc)	!Min temperature data after interpolate
	real um(max_nr,max_nc)		!Relative humid data after interpolate
	real n_summ(max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm(max_nr,max_nc)	!Mean Wind Rate data after interpolate
	real rsum(max_nr,max_nc)	!Precipitation data after interpolate
	real sun(max_nr,max_nc)	!Sunshine Time data after interpolate

	real			ftnd,Alai1,Alai2,Alai3
	real			fave,fmin,fmax,areasum

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

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

	do i=1,max_nr
	do j=1,max_nc
		tm(i,j)=tm(i,j)
     $			-(fm_tm(i,j)*(nt)/365.+fb_tm(i,j))+fav_tm(i,j)		
	enddo
	enddo

			CALL ana_DATAF(tm,gridarea,
     $		fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai1=fave
			CALL ana_DATAF(tm,gridarea,
     $		fracdd,demd,max_nr,max_nc,fave,fmin,fmax,areasum)
			Alai2=fave
			write(inpt_tm,*) year,nt,Alai1,Alai2
			print*, year,nt,Alai1,Alai2
	
	RETURN

2112	PRINT *,'The interpoalted data does not exist!'
	PRINT *,'You can not use subroutine Get_Grid_ATM_d,'
	PRINT *,'Please use subroutine Get_Grid_ATM first.'
	STOP

	end