c This subroutine real data from data files
c Read observation data into the structure of station /only the data in calculate period are saved/
c e.g. if calculated 1999-, then 1999/1/1 is saved in stn(i).record(1) 1999/1/2 is saved in stn(i).record(2)...
	subroutine read_data_file(stn_data_dir,n_stn,startyear,endyear,stn)
	implicit none
	include '../INCLUDE/Def_Read_foring.inc'
	integer n_stn,startyear,endyear	
	character*80	stn_data_dir,stn_id,stn_data_file

	integer	i,j,l1,l2,ll1,ll2
	integer	id,year,month,day
	integer	days,days_e,days_fromstart
	real	tm1,tmax1,tmin1,um1,umin1,n_summ1,n_lowm1,fsm1,fmaxx1
	real	fmaxs1,rsum1,d0m1,sun1,E01,snow1
	real	alter
c	Ini 
	do i=1,n_stn
		do j=1,max_time
			stn(i).nodata(j)=0
			stn(i).tm(j)=-99999.0
			stn(i).tm(j)=-99999.0
			stn(i).tmax(j)=-99999.0
			stn(i).tmin(j)=-99999.0
			stn(i).um(j)=-99999.0
			stn(i).umin(j)=-99999.0
			stn(i).n_summ(j)=-99999.0
			stn(i).n_lowm(j)=-99999.0
			stn(i).fsm(j)=-99999.0
			stn(i).fmaxx(j)=-99999.0
			stn(i).fmaxs(j)=-99999.0
			stn(i).rsum(j)=-99999.0
			stn(i).d0m(j)=-99999.0
			stn(i).sun(j)=-99999.0
			stn(i).E0(j)=-99999.0
			stn(i).snow(j)=-99999.0
		enddo
	enddo

	do i=1,n_stn
		write(stn_id,*) stn(i).id
		call strlen(stn_id,ll1,ll2)
		call strlen(stn_data_dir,l1,l2)
		stn_data_file=stn_data_dir(l1:l2)//stn_id(ll1:ll2)//'.txt'		
		open(2, file=stn_data_file, status='old')
			do j=1,30000
				read(2,*,end=2) id,year,month,day,tm1,tmax1,tmin1,um1,
     $				umin1,n_summ1,n_lowm1,fsm1,fmaxx1,fmaxs1,
     $				rsum1,d0m1,sun1,E01,snow1
				days=days_fromstart(startyear,1,1,year,month,day)
				days_e=days_fromstart(endyear+1,1,1,year,month,day)
c				Only the data in calculate period are saved
				if (days.gt.0.and.days_e.eq.-1) then
					stn(i).nodata(days)=1	
					if(alter(tm1).ne.-99999.0) then
						stn(i).tm(days)=alter(tm1)*0.1  !Change Unit
					endif
					if(alter(tmax1).ne.-99999.0) then
						stn(i).tmax(days)=alter(tmax1)*0.1	
					endif	
					if(alter(tmin1).ne.-99999.0) then
						stn(i).tmin(days)=alter(tmin1)*0.1
					endif	
					if(alter(um1).ne.-99999.0) then
						stn(i).um(days)=alter(um1)*0.01
					endif		
					if(alter(umin1).ne.-99999.0) then
						stn(i).umin(days)=alter(umin1)*0.01
					endif
					if(alter(n_summ1).ne.-99999.0) then
						stn(i).n_summ(days)=alter(n_summ1)*0.01
					endif
					if(alter(n_lowm1).ne.-99999.0) then
						stn(i).n_lowm(days)=alter(n_lowm1)*0.01
					endif
					if(alter(fsm1).ne.-99999.0) then
						stn(i).fsm(days)=alter(fsm1)*0.1
					endif
					if(alter(fmaxx1).ne.-99999.0) then
						stn(i).fmaxx(days)=alter(fmaxx1)
					endif
					if(alter(fmaxs1).ne.-99999.0) then
						stn(i).fmaxs(days)=alter(fmaxs1)*0.1
					endif
					if(alter(rsum1).ne.-99999.0) then
						stn(i).rsum(days)=alter(rsum1)*0.1
					endif
					if(alter(d0m1).ne.-99999.0)	then
						stn(i).d0m(days)=alter(d0m1)*0.1
					endif
					if(alter(sun1).ne.-99999.0)	then
						stn(i).sun(days)=alter(sun1)*0.1
					endif
					if(alter(E01).ne.-99999.0)	then
						stn(i).E0(days)=alter(E01)
					endif
					if(alter(snow1).ne.-99999.0) then
						stn(i).snow(days)=alter(snow1)
					endif
c				Check the input data					
					call Check_data(i,days,stn)
				endif
			enddo
2		close(2)
	enddo
	end

c Check the observation data
c common id 32744,32766, shows nodata, set to -99999.0
c common id 32700, shows very small value, set to 0.1
c For some station (especially for precipitaiton data),3ABCD mean only BCD
c e.g. the record is 31017, but it means precipitaion is 17(*0.1)mm/day
	real function alter(observation)
	implicit none
	real observation
	alter=observation

	if (observation.ge.30000) then
		if (observation.lt.31000) then
			alter=observation-30000
		else if (observation.lt.32000) then
			alter=observation-31000
		else if (observation.lt.32700) then
			alter=observation-32000
		endif
	endif

	if(abs(observation-32744).lt.0.01.or.
     $	abs(observation-32766).lt.0.01)then
		alter=-99999.0
	endif
	if (abs(observation-32700).lt.0.01) then
		alter=0.1
	endif
	return
	end

c A simple subroutine to check the input data
c You are strongly recommended to confirm input file by yourself
c DO NOT rely on the subroutine to check all the errors!
c This subroutine can only point out the very obvious error in input files 
	subroutine Check_data(i,days,stn)
	implicit none
	include '../INCLUDE/Def_Read_foring.inc'
	integer i,days

	if (stn(i).rsum(days).gt.2900.0.or.
     $(stn(i).rsum(days).lt.0.0.and.stn(i).rsum(days).ne.-99999.0)) then
		Print*,'Data Error'
		print*,'Station',stn(i).id,days,'Precipitation:',stn(i).rsum(days)
c		stop
	endif

	if ((stn(i).tm(days).ne.-99999.0.and.stn(i).tm(days).lt.-90.0).or.
     $ (stn(i).tmax(days).ne.-99999.0.and.stn(i).tmax(days).lt.-90.0))
     $		then
		Print*,'Data Error'
		print*,'Station',i,stn(i).id,days
		print*,'    Tmean:',stn(i).tm(days)
		print*,'	Tmax:',stn(i).tmax(days)
		print*,'	Tmin:',stn(i).tmin(days)
c		stop
	endif
	if (stn(i).tmin(days).ne.-99999.0.and.stn(i).tmin(days).lt.-90.0
     $		.or.stn(i).tmin(days).gt.70.0)then
		Print*,'Data Error'
		print*,'Station',i,stn(i).id,days
		print*,'    Tmean:',stn(i).tm(days)
		print*,'	Tmax:',stn(i).tmax(days)
		print*,'	Tmin:',stn(i).tmin(days)
c		stop
	endif	
	if (stn(i).tm(days).gt.70.0
     $		.or.stn(i).tmax(days).gt.70.0)then
		Print*,'Data Error'
		print*,'Station',i,stn(i).id,days
		print*,'    Tmean:',stn(i).tm(days)
		print*,'	Tmax:',stn(i).tmax(days)
		print*,'	Tmin:',stn(i).tmin(days)
c		stop
	endif	
	if (stn(i).fmaxx(days).gt.17.0.or.(stn(i).fmaxx(days).ne.-99999.0
     $	.and.stn(i).fmaxx(days).le.0.0)) then
c		Print*,'Data Error'
c		print*,'Station',i,stn(i).id,days
c		print*,'WARNING:	fmaxx:',stn(i).fmaxx(days)
	endif
	end