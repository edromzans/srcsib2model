	INCLUDE './CHARACTER/ConvMMYYYY.f'
	INCLUDE './RW_FILE/RW_ArcInfo_Ascii.f'
	INCLUDE './RW_FILE/R_SIB2_para.f'
	INCLUDE './GEO_REFER/Convert_Lambert_latlon.f'
	INCLUDE './SIB2/SIB2sub.f'
	INCLUDE './SIB2/SiB2data.f'
	INCLUDE './SIB2/derive_trans.f'
	INCLUDE './RW_FILE/R_parameters.f'
	INCLUDE './RW_FILE/R_ANA_para.f'
	INCLUDE "./TEMPORAL/Year_days_YYMMDD.f"
	INCLUDE './RW_FILE/R_GEO_hrd.f'
	INCLUDE './RW_FILE/RW_Real_Binary.f'
	INCLUDE './TEMPORAL/Days_S_E.f'
	INCLUDE './CHARACTER/Strlen.f'
	INCLUDE	'./SUBROUTINE/Get_Grid_ATM_d.f'
	INCLUDE './SUBROUTINE/GET_FPAR.f'
	INCLUDE './SUBROUTINE/GET_LAI.f'
	INCLUDE './RW_FILE/R_FPAR_SA.f'
	INCLUDE './CHARACTER/ConvYYMM.f'
	INCLUDE './RW_FILE/R_ancillary_file.f'
	INCLUDE './GEO_REFER/Abstract_SA.f'	
	INCLUDE './GEO_REFER/Get_Goode_coordinate.f'
	INCLUDE './SUBROUTINE/downscale.f'
	INCLUDE	'./SUBROUTINE/Radiation_YANG.f'
	INCLUDE	'./SUBROUTINE/Get_potential_ET_d.f'
	INCLUDE './SUBROUTINE/Get_Basin_structure.f'
	INCLUDE './SUBROUTINE/Get_river_var.f'
	INCLUDE './SUBROUTINE/SIB_River.f'		
	INCLUDE './ANALYSIS/ANA_ArcInfo_Ascii.f'
	INCLUDE './SUBROUTINE/OutISuBBal.f'		
	INCLUDE './SUBROUTINE/IrriWithdraw.f'	
	INCLUDE './SUBROUTINE/River_Route.f'	
	INCLUDE './SIB2/SiB2_grid.f'			
	INCLUDE './SUBROUTINE/Acc_Sub.f'
	INCLUDE './SUBROUTINE/Data_Pre.f'
	INCLUDE './RW_FILE/R_VER_para.f'
	INCLUDE './RW_FILE/W_GMT.f'
	INCLUDE './SUBROUTINE/Verificate.f'

	PROGRAM SIB_DHM
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'

      integer		i_y,i_m,i_d,i_h,iyd,iyc,iter
	integer		idum,lengrid,lenriv,idx,irx,lenrpa
	integer		NQSsf(12)
	real		QSsubflow(max_sub,max_flow,12),MWDsf(max_sub,max_flow)
	real		Rivpara(max_sub,max_flow,14)
      integer		idc,ihc,dayinmonth(12),i,j,days_fromstart,KER
      data		dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	INTEGER*4	TMA(3)
	CHARACTER*4	YYYY
	data	icho1,icho2,icho3,icho4,icho5,icho6 /21,22,23,24,25,26/
	data	icho32, icho33, icho34, intm,inshwv / 232,332,432,233,234/
      data	iout,iout1, iout2, iout3, iout4 / 35, 38, 42, 45, 48 /              
      data	ichmet,iu    / 7,  8/                  
      data	ipbl  / 1 /                                                          
      data	isnow / 0 / 
	data  inpt_lai,inpt_fpar,inpt_tm,inpt_tmax,inpt_min
     $ /660,661,662,663,664 /
	data  inpt_um,inpt_et,inpt_rsum,inpt_sun,inpt_fsm
     $ /665,666,667,668,669/
	data nc_cl,nc_ia,nc_vg /1,1,1/ !(=0, no change, other, change)

	CALL Read_parameters(KER)

	idx = 1	!idx=1, try to read initial para from GridSave.bin,RiverSave.bin
	irx = 1 !irx=1, try to read river width, etc from Riverpar.bin
	idum = 0

	CALL strlen(Output_D,l1,l2)
	OPEN(icho1,file=Output_D(l1:l2)//'SIB_DHM_info.txt',status='unknown')
	OPEN(icho2,file=Output_D(l1:l2)//'SIB2_warn.txt',status='unknown')
	OPEN(icho3,file=Output_D(l1:l2)//'SIB_DHM_warn.txt',status='unknown')
	OPEN(icho32,file=Output_D(l1:l2)//'OutInfo_roff.txt',status='unknown')
	OPEN(icho33,file=Output_D(l1:l2)//'OutInfo_www.txt',status='unknown')
	OPEN(icho34,file=Output_D(l1:l2)//'OutStn_www.txt',status='unknown')
	CALL strlen(ATM_ITP,n1,n2)
	lenin   = 4 * max_nr * max_nc
	lengrid = 4 * max_nr * max_nc
	lenriv  = 4 
	lenrpa  = 4 * max_sub * max_flow
	CALL strlen(Derive_D,l1,l2)
	OPEN(icho4,file=Derive_D(l1:l2)//'GridSave.bin',status='unknown',
     $	access='direct',recl=lengrid)
	OPEN(icho5,file=Derive_D(l1:l2)//'RiverSave.bin',status='unknown',
     $	access='direct',recl=lenriv)	
	OPEN(icho6,file=Derive_D(l1:l2)//'Riverpar.bin',status='unknown',
     $	access='direct',recl=lenrpa)	

	OPEN (inpt_lai,file=Derive_D(l1:l2)//'LAI_in.txt',status='unknown')
	OPEN (inpt_fpar,file=Derive_D(l1:l2)//'FPAR_in.txt',status='unknown')
	OPEN (inpt_tm,file=Derive_D(l1:l2)//'TM_in.txt',status='unknown')
	OPEN (inpt_tmax,file=Derive_D(l1:l2)//'TMAX_in.txt',status='unknown')
	OPEN (inpt_min,file=Derive_D(l1:l2)//'TMIN_in.txt',status='unknown')
	OPEN (inpt_um,file=Derive_D(l1:l2)//'UM_in.txt',status='unknown')
	OPEN (inpt_et,file=Derive_D(l1:l2)//'ET_in.txt',status='unknown')
	OPEN (inpt_rsum,file=Derive_D(l1:l2)//'RSUM_in.txt',status='unknown')
	OPEN (inpt_sun,file=Derive_D(l1:l2)//'SUN_in.txt',status='unknown')
	OPEN (inpt_fsm,file=Derive_D(l1:l2)//'FSM_in.txt',status='unknown')

	CALL Get_Basin_structure(n_sub,sub)
	CALL Get_Base_Maps()
	CALL Get_VER_PARs()
	CALL Get_SIB_PARs(n_sub,sub)
	CALL Get_Irr_PARs(n_sub,sub)
	CALL Set_SIBDHM_INI()
	CALL Set_OUT_stn(n_sub,sub)
	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s) !to get the nr,nc	
	CALL Initial_cond(idx,sub)
	CALL River_para(irx,Rivpara,sub,n_sub)
	NQSsf		=0
	QSsubflow	=0.
	MWDsf		=0.

c	Loop from start time to end time 

	i_y=startyear
	i_m=startmont
	i_d=startday
	i_h=1	
	niter=days_fromstart(i_y,i_m,i_d,endyear,endmont,endday)*24
	if (niter.le.0) goto 101
	if(mod(i_y,4).eq.0 .and. i_m.eq.2) dayinmonth(i_m)=29	    
	if(mod(i_y,4).ne.0 .and. i_m.eq.2) dayinmonth(i_m)=28
	if(mod(i_y,4).eq.0) iyd=366
	if(mod(i_y,4).ne.0) iyd=365

	do iter =1,niter	

	if (i_h.eq.1.and.i_d.eq.1.and.i_m.eq.1) then	!update per year
		if(mod(i_y,4).eq.0) iyd=366
		if(mod(i_y,4).ne.0) iyd=365
		write(YYYY,'(i4)')  i_y
		CALL CHANGE_IRR_RATIO(i_y)
	endif

	if (i_h.eq.1.and.i_d.eq.1) then					!update per month
c		if (irx.eq.1) CALL UPDATARV(Rivpara,sub,n_sub,i_m)
		imd = dayinmonth(i_m)
		CALL Get_LAI_ext(i_y,i_m,LAI,KER)
		CALL GET_FPAR_ext(i_y,i_m,fpar,KER)
		CALL WR_BAL_INI(sub,n_sub)
	endif

	if (i_h.eq.1) then								!update per day
		CALL ITIME (TMA)
		WRITE(icho1,10), i_y,i_m,i_d,TMA
		CALL Get_potential_ET_d(i_y,i_m,i_d,ETday)
		CALL Get_Grid_ATM_d(i_y,i_m,i_d,tm_o,
     $		tmax_o,tmin_o,um_o,n_summ_o,fsm_o,rsum_o,sun_o)
	endif

	CALL Get_river_var(n_sub,sub,Drw)

	Iyear  = i_y
	Imonth = i_m
	Iday   = i_d
	Ihour  = i_h-1

	irec = (days_fromstart(i_y,1,1,i_y,i_m,i_d)-1)*24+i_h
201		CALL Downscale_tm2(Ihour,tm_sib,lon0,Iyear,Imonth,Iday,
     $		long_10,lat_10,tmax_o,tmin_o,tm_o,demd)
	CALL Downscale_em(Ihour,em_sib,tmax_o,tmin_o,um_o,demd,precm,ETday)
	CALL Downscale_zlwd(Ihour,zlwd_sib,tm_sib,em_sib,demd)
	CALL Downscale_um(Ihour,um_sib,fsm_o,demd)
	CALL Downscale_tprec(Ihour,randihour,tprec_sib,rsum_o,demd,idum)
203		CALL Downscale_swdown(Ihour,swdown_sib,lon0,Iyear,Imonth,Iday,
     $		long_10,lat_10,tm_o,um_o,sun_o,demd)

	CALL SIB2_grid(dt,i_y,i_m,i_d,i_h)

	CALL SIB_River(dt,levels,n_sub,sub,
     $	i_y,i_m,i_d,i_h,imd,iyd)

	CALL OUT_SIM(dt,levels,n_sub,sub,
     $	i_y,i_m,i_d,i_h,imd,iyd)

	CALL WR_BAL(sub,n_sub,i_y,i_m,i_d,i_h,imd,iyd)

	CALL Q_derived(sub,irx,i_m,NQSsf,QSsubflow,MWDsf)

	if (iter.eq.niter) CALL OUT_CMP_OBV(niter)

	if (iter.eq.niter.and.idx.eq.0) then 
		CALL Save_cond(sub)	
	endif

	if (iter.eq.niter.and.irx.eq.0) then 
		CALL Save_derived(n_sub,sub,NQSsf,QSsubflow,MWDsf)	
	endif

	i_h = i_h + 1
	if (i_h.gt.24) then
		i_h = 1
		i_d = i_d +1
		if (i_d.gt.dayinmonth(i_m)) then
			i_d = 1
			i_m = i_m +1
			if(mod(i_y,4).eq.0 .and. i_m.eq.2) dayinmonth(i_m)=29	    
			if(mod(i_y,4).ne.0 .and. i_m.eq.2) dayinmonth(i_m)=28
			if (i_m.gt.12) then
				i_m = 1
				i_y = i_y +1
			endif
		endif
	endif
	enddo

10	FORMAT('Year',I5,' MONTH',I3,' DAY',I3,
     $			' COMP TIME ',I2,':',I2,':',I2)

101	CLOSE(icho1)
	CLOSE(icho2)
	CLOSE(icho3)
	CLOSE(icho32)
	CLOSE(icho33)
	CLOSE(icho34)
	CLOSE(icho4)
	CLOSE(icho5)
	CLOSE(icho6)

	CLOSE(inpt_lai)
	CLOSE(inpt_fpar)
	CLOSE(inpt_tm)
	CLOSE(inpt_tmax)
	CLOSE(inpt_min)
	CLOSE(inpt_um)
	CLOSE(inpt_et)
	CLOSE(inpt_rsum)
	CLOSE(inpt_sun)
	CLOSE(inpt_fsm)

      END !PROGRAM  
	

