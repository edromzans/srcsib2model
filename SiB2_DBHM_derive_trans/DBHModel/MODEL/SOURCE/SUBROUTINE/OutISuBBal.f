c Output simulation results
	subroutine OUT_SIM(dt,levels,n_sub,sub,
     $	i_y,i_m,i_d,i_h,imd,iyd)		
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/COMSIBC.H' 
	character*20	stnname
	real	dt
	integer isub,iflow,insub1,insub2,levels,igrid

	do 414 isub=1,n_sub
		do 313 iflow=sub(isub).nflow,1,-1
			do 3113 i=1, n_stno
			 if (sub(isub).code.eq.subcode(i)) then
			 subisub(i)=isub
			 if (flowid(i).eq.0) flowid(i)=sub(isub).nflow
			 if (iflow.eq.flowid(i)) then
c			  print *,sub(isub).code,iflow,isub
			  call strlen(outstnn(i),len1,len2)
			  stnname=outstnn(i)(len1:len2)
			  CALL WR_ISUB(sub,i_y,i_m,i_d,i_h,imd,iyd,isub,iflow,i,stnname)
			 endif
			 endif
3113			continue
313		continue
414	continue

	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN
		rirr  = irriratio(inr,inc)/100.0
		IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
		  www1(inr,inc) =www1_i(inr,inc)*rirr+www1_ni(inr,inc)*(1.-rirr)
		  www2(inr,inc) =www2_i(inr,inc)*rirr+www2_ni(inr,inc)*(1.-rirr)
		  www3(inr,inc) =www3_i(inr,inc)*rirr+www3_ni(inr,inc)*(1.-rirr)
		  sura(inr,inc) =surdep_i(inr,inc)*rirr+surdep_ni(inr,inc)*(1.-rirr)
		ELSE
		  www1(inr,inc) =www1_ni(inr,inc)
		  www2(inr,inc) =www2_ni(inr,inc)
		  www3(inr,inc) =www3_ni(inr,inc)
		  sura(inr,inc) =surdep_ni(inr,inc)
		ENDIF		
	ENDIF
	ENDDO
	ENDDO

	if (iter.eq.1) then
		write(icho34,'(A4,1x,3(A3),$)') 'i_y','i_m','i_d','i_h'
		DO i=1,nsoilg
			write(icho34,'(A18,$)') slstnn(i)
		ENDDO
		write(icho34,*)
	endif
	write(icho34,'(I4,1x,3(I2,1x),$)') 	i_y,i_m,i_d,i_h
	DO i=1,nsoilg
		inr=soilgr(i,1)
		inc=soilgr(i,2)
		rirr  = irriratio(inr,inc)/100.0
		poros = slpar1(inr,inc)
		zdep12ni= zdepth1_ni(inr,inc)+ zdepth2_ni(inr,inc)
		if (zdepth1_ni(inr,inc).lt.laydep(1)) then
		Wlay1ni = zdepth1_ni(inr,inc)*poros*www1_ni(inr,inc) 
     $	+(laydep(1)-zdepth1_ni(inr,inc))*poros*www2_ni(inr,inc)
		  if (zdep12ni.lt.1.0) then
		  Wlay2ni = (zdep12ni-laydep(1))*poros*www2_ni(inr,inc)
     $	  +(1.0-zdep12ni)*poros*www3_ni(inr,inc)
		  else
		  Wlay2ni = laydep(2)*poros*www2_ni(inr,inc)
		  endif 
		else
		  Wlay1ni = laydep(1)*poros*www1_ni(inr,inc) 
		  if (zdep12ni.lt.1.0) then
			Wlay2ni = (zdepth1_ni(inr,inc)-laydep(1))*poros*www1_ni(inr,inc)
     $		+zdepth2_ni(inr,inc)*poros*www2_ni(inr,inc)
     $		+(1.0-zdep12ni)*poros*www3_ni(inr,inc)
		  else
			Wlay2ni = (zdepth1_ni(inr,inc)-laydep(1))*poros*www1_ni(inr,inc)
     $		+(1.0-zdepth1_ni(inr,inc))*poros*www2_ni(inr,inc)
		  endif 
		endif
		IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
			zdep12i= zdepth1_i(inr,inc)+ zdepth2_i(inr,inc)
			if (zdepth1_i(inr,inc).lt.laydep(1)) then
			Wlay1i = zdepth1_i(inr,inc)*poros*www1_i(inr,inc) 
     $		+(laydep(1)-zdepth1_i(inr,inc))*poros*www2_i(inr,inc)
			  if (zdep12i.lt.1.0) then
				Wlay2i = (zdep12i-laydep(1))*poros*www2_i(inr,inc)
     $			+(1.0-zdep12i)*poros*www3_i(inr,inc)
			  else
				Wlay2i = laydep(2)*poros*www2_i(inr,inc)
			  endif 
			else
			Wlay1i = laydep(1)*poros*www1_i(inr,inc) 
			  if (zdep12i.lt.1.0) then
				Wlay2i = (zdepth1_i(inr,inc)-laydep(1))*poros*www1_i(inr,inc)
     $			+zdepth2_i(inr,inc)*poros*www2_i(inr,inc)
     $			+(1.0-zdep12i)*poros*www3_i(inr,inc)
			  else
				Wlay2i = (zdepth1_i(inr,inc)-laydep(1))*poros*www1_i(inr,inc)
     $			+(1.0-zdepth1_i(inr,inc))*poros*www2_i(inr,inc)
			  endif 
			endif
			Wlay1 =(Wlay1i*rirr+Wlay1ni*(1.-rirr))*100.	!(UNIT: cm)
			Wlay2 =(Wlay2i*rirr+Wlay2ni*(1.-rirr))*100.
		ELSE
		Wlay1 =Wlay1ni*100.	!(UNIT: cm)
		Wlay2 =Wlay2ni*100.
		ENDIF
		write(icho34,'(2(F8.3,1x),$)') Wlay1,Wlay2		
	Enddo
	write(icho34,*)

	sw1 = sw1 + www1
	sw2 = sw2 + www2
	sw3 = sw3 + www3
	CALL ana_DATAF(www1,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fwww1 = fave
	CALL ana_DATAF(www2,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fwww2 = fave
	CALL ana_DATAF(www3,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fwww3 = fave
	write(icho33,'(3(f,1x))') fwww1,fwww2,fwww3

	end !subroutine OUT_SIM


c Output hydrograph at isub
c Input:
c	sub:	save state of river 
c	i_y,i_m,i_d,i_h: year, month, day, hour
c	imd,iyd: the days in the month, the days in the year
c	isub:	 the sub basin id for output
c	stnname: the control station name of the sub basin
	Subroutine WR_ISUB(sub,i_y,i_m,i_d,i_h,imd,iyd,isub,iflow,i,stnname)
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
      INCLUDE './SIB2/COMSIBC.H' 	

	character*80	rofy,rofm,rofd,rofh
	character*20	CH,stnname
	integer			l1,l2,o1,o2,i,days_fromstart,nday,nmon,nyear
	integer			irofy,irofm,irofd,irofh 
	data			irofy,irofm,irofd,irofh / 91, 92, 93, 94 /

	Ch=stnname
	CALL strlen(Ch,l1,l2)
	CALL strlen(Output_D,o1,o2)
	rofy =Output_D(o1:o2)//Ch(l1:l2)//'_y.txt'
	rofm =Output_D(o1:o2)//Ch(l1:l2)//'_m.txt'
	rofd =Output_D(o1:o2)//Ch(l1:l2)//'_d.txt'
	rofh =Output_D(o1:o2)//Ch(l1:l2)//'_h.txt'
	if (iter.eq.1) then
c	Imkdir= makedirqq(Output_D(o1:o2))
	OPEN(irofy,file=rofy,status='unknown')
	OPEN(irofm,file=rofm,status='unknown')
	OPEN(irofd,file=rofd,status='unknown')
	OPEN(irofh,file=rofh,status='unknown')
	write (irofy,'(3(A12,1x))')'i_y',	'Qy','Qy_obv'
	write (irofm,'(4(A12,1x))')'i_y',	'i_m','Qm','Qm_obv'
	write (irofd,'(5(A12,1x))')'i_y',	'i_m',	'i_d',	'Qd','Qd_obv'
	write (irofh,'(5A12)')'i_y',	'i_m',	'i_d',	'i_h',	'Qh'
      CLOSE(irofy)
      CLOSE(irofm)
      CLOSE(irofd)
      CLOSE(irofh)
	endif

	if (i_m.eq.1.and.i_d.eq.1.and.i_h.eq.1) Qy(i) = 0.0 
	if (i_m.eq.1.and.i_d.eq.1.and.i_h.eq.1) Qyobv(i) = 0.0 
	if (i_d.eq.1.and.i_h.eq.1) Qm(i) = 0.0
	if (i_d.eq.1.and.i_h.eq.1) Qmobv(i) = 0.0
	if (i_h.eq.1) Qd(i) = 0.0

	Qh=sub(isub).flow(iflow).Qi1_j1
	Qd(i)=Qd(i)+sub(isub).flow(iflow).Qi1_j1/24.0
	Qm(i)=Qm(i)+sub(isub).flow(iflow).Qi1_j1/24.0/ imd
	Qy(i)=Qy(i)+sub(isub).flow(iflow).Qi1_j1/24.0/ iyd

	OPEN(irofh,file=rofh,form='formatted',access='APPEND',status='OLD')
	write (irofh,'(4i12,f12.3)'),i_y,i_m,i_d,i_h,Qh
      CLOSE(irofh)

	if (i_h.eq.24) then
	nday= days_fromstart(startyear,startmont,startday,i_y,i_m,i_d)
	DaySIM(nday,i)=Qd(i)
	Qdobv = DayOBV(nday,i)
	if (Qdobv.ge.0..and.Qmobv(i).ge.0.) then
		Qmobv(i)=Qmobv(i)+Qdobv/imd
	else
		Qmobv(i)=-9999
	endif
	if (Qdobv.ge.0..and.Qyobv(i).ge.0.) then
		Qyobv(i)=Qyobv(i)+Qdobv/iyd
	else
		Qyobv(i)=-9999
	endif
	OPEN(irofd,file=rofd,form='formatted',access='APPEND',status='OLD')
		write (irofd,9999),i_y,i_m,i_d,Qd(i),Qdobv
	CLOSE(irofd)
	endif
9999	format(3(i12,1x),2(f12.3,1x))

	if (i_d.eq.imd.and.i_h.eq.24) then
	nmon= (i_y - startyear) * 12 +i_m
	MonSIM(nmon,i)=Qm(i)
	OPEN(irofm,file=rofm,form='formatted',access='APPEND',status='OLD')
		write (irofm,9998),i_y,i_m,Qm(i),Qmobv(i)
	CLOSE(irofm)	
	endif
9998	format(2(i12,1x),2(f12.3,1x))

	if (i_m.eq.12.and.i_d.eq.imd.and.i_h.eq.24) then
	nyear= (i_y - startyear) +1
	YearSIM(nyear,i)=Qy(i)
	OPEN(irofy,file=rofy,form='formatted',access='APPEND',status='OLD')
		write (irofy,9997),i_y,Qy(i),Qyobv(i)
	CLOSE(irofy)
	endif
9997	format(1(i12,1x),2(f12.3,1x))
	
	END !Subroutine WR_ISUB()


c Output water balance and energy balance of the river basin
c Input:
c	sub:	save state of river 
c	n_sub:	number of sub basins
c	i_y,i_m,i_d,i_h: year, month, day, hour
c	imd,iyd: the days in the month, the days in the year		
	Subroutine WR_BAL(sub,n_sub,i_y,i_m,i_d,i_h,imd,iyd)
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	

	integer			ETID
	character*80	DEM_FD,FRAC_FD,ETPATH
	common			/Get_ET_para/ETID,DEM_FD,FRAC_FD,ETPATH
	character*80	code_file,dis_file,dir_file,slope_file,dem_file
	character*80	area_file,frac_file,riverway_file,Outstn,Output_D
	character*80	Derive_D       
      integer	levels,startyear,endyear,startmont,startday,endmont,endday
	real			dx_max,dt,rivlen
	common			/Input_para/ levels,startyear,startmont,startday,
     $				endyear,endmont,endday,dx_max,
     $		code_file,dis_file,dir_file,slope_file,dem_file,area_file,
     $frac_file,riverway_file,Outstn,Output_D,Derive_D,dt,rivlen
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

	character*80	tmpname
	character*6		MMYYYY
	integer			o1,o2
	REAL			fave,fmin,fmax,areasum,FP,FE,FR,FB,FG,FW,DS
	real			dtotwb,balnt,dovwb,dgwwb
	real			OF,OR,OD,OB,GF,GG,GR,GD,GB
	real			rirr,B,WDj,dx
	real			totwbm(max_nr,max_nc)

	real			BHend(max_stno),FLQR(max_stno),FLQQ(max_stno)
	real			WITHD(max_stno),FOUTQ(max_stno),RIVBAL(max_stno)
	real			RETNFW(max_stno)

	integer			codeid2(max_nr,max_nc),l1,l2
	real			rcodeid(max_nr,max_nc),fshort(max_nr,max_nc)
	character*80	StnCS,StnOG,StnRV,MainRiv,StnEN

c Energy balance items
	DO inr=1,nr
	DO inc=1,nc
		if (radlong(inr,inc).gt.0.) then
		radl_m(inr,inc) = radl_m(inr,inc) + radlong(inr,inc)/imd/24.
		raul_m(inr,inc) = raul_m(inr,inc) + raulong(inr,inc)/imd/24.
		rads_m(inr,inc) = rads_m(inr,inc) + radshor(inr,inc)/imd/24.
		rans_m(inr,inc) = rans_m(inr,inc) + ranshor(inr,inc)/imd/24.
		radt_m(inr,inc) = radt_m(inr,inc) + radutot(inr,inc)/imd/24.
		raet_m(inr,inc) = raet_m(inr,inc) + raet(inr,inc)/imd/24.
		raht_m(inr,inc) = raht_m(inr,inc) + raht(inr,inc)/imd/24.
		rast_m(inr,inc) = rast_m(inr,inc) + rast(inr,inc)/imd/24.
		tc_m  (inr,inc) = tc_m  (inr,inc) + tcts(inr,inc)/imd/24.
		tg_m  (inr,inc) = tg_m  (inr,inc) + tgts(inr,inc)/imd/24.
		td_m  (inr,inc) = td_m  (inr,inc) + tdts(inr,inc)/imd/24.
		assimn_m(inr,inc)=assimn_m(inr,inc)+assts(inr,inc)/imd/24.
		else
		radl_m(inr,inc) = -9999.
		raul_m(inr,inc) = -9999.
		rads_m(inr,inc) = -9999.
		rans_m(inr,inc) = -9999.
		radt_m(inr,inc) = -9999.
		raet_m(inr,inc) = -9999.
		raht_m(inr,inc) = -9999.
		rast_m(inr,inc) = -9999.
		tc_m  (inr,inc) = -9999.
		tg_m  (inr,inc) = -9999.
		td_m  (inr,inc) = -9999.
		assimn_m(inr,inc)=-9999.
		endif
	ENDDO
	ENDDO

c Water balance check for each time step
	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN
	rirr  = irriratio(inr,inc)/100.0
	IF (rirr.gt.0.0.and.rirr.le.1.0) THEN !irrgation value is reliable
	wbi=(www1_i(inr,inc)*sporos(inr,inc)*zdepth1_i(inr,inc) 
     & + www2_i(inr,inc) * sporos(inr,inc) * zdepth2_i (inr,inc)  
     & + www3_i(inr,inc) * sporos(inr,inc) * zdepth3_i (inr,inc)  
     & + capac1_i(inr,inc) + capac2_i(inr,inc) + snoww1_i(inr,inc) 
     & + snoww2_i(inr,inc) + surdep_i(inr,inc)	)*1000.
	wbni=(	www1_ni(inr,inc) * sporos(inr,inc)*zdepth1_ni(inr,inc) 
     & + www2_ni(inr,inc) * sporos(inr,inc) * zdepth2_ni (inr,inc)       
     & + www3_ni(inr,inc) * sporos(inr,inc) * zdepth3_ni (inr,inc)       
     & + capac1_ni(inr,inc) + capac2_ni(inr,inc) + snoww1_ni(inr,inc) 
     & + snoww2_ni(inr,inc) + surdep_ni(inr,inc)	)*1000.
	dtotwb=(wbi - totwbmi(inr,inc) )* rirr + 
     &	   (wbni - totwbmni(inr,inc) )*(1 - rirr)  !mm 
	ELSE
	wbi= 0.
	wbni=(	www1_ni(inr,inc) * sporos(inr,inc)*zdepth1_ni(inr,inc) 
     & + www2_ni(inr,inc) * sporos(inr,inc) * zdepth2_ni (inr,inc)       
     & + www3_ni(inr,inc) * sporos(inr,inc) * zdepth3_ni (inr,inc)       
     & + capac1_ni(inr,inc) + capac2_ni(inr,inc) + snoww1_ni(inr,inc) 
     & + snoww2_ni(inr,inc) + surdep_ni(inr,inc)	)*1000.
	dtotwb=wbni - totwbmni(inr,inc)   !mm 
	ENDIF !irrigation
	balnt = tprec_h(inr,inc)-ETmass_h(inr,inc)+gwsoil_h(inr,inc)+
     &	wthdr_h(inr,inc) - retnfw_h(inr,inc) - tgwir_h(inr,inc)
     &  -roff_h(inr,inc)-dtotwb
	IF (abs(balnt).gt.1.0E-1) THEN
		WRITE(icho3, *) 'WATER INBALANCE AT CANOPY/SOIL (mm)'
		WRITE(icho3,32101) ITER, i_y,i_m,i_d,i_h,inr,inc
32101	format('Tstep,Year,Month,Day,Hour,LOCA(inr,inc):',I7,I5,3I3,2I5)
		WRITE(icho3,32102) tprec_h(inr,inc),gwsoil_h(inr,inc),
     &		wthdr_h(inr,inc),retnfw_h(inr,inc),tgwir_h(inr,inc)
32102 format('PREC,GSOIL,WTHDR,RTNFW,TGWIR',5(F10.2,1X))
		WRITE(icho3,32103) ETmass_h(inr,inc),roff_h(inr,inc),dtotwb,balnt
32103 format('ET,ROFF,DS,BAL',4(F10.2,1X))
	ENDIF

c	dovwb = (SWdepA(inr,inc) - over_h(inr,inc))*1000.	!(m->mm)
c	balnt = runoff1_h(inr,inc) - riv_rof_h(inr,inc) -dovwb 
c	IF (abs(balnt).gt.1.0E-1) THEN
c		WRITE(icho3, *) 'WATER INBALANCE AT OVERLAND FLOW (mm)'
c		WRITE(icho3,32101) ITER, i_y,i_m,i_d,i_h,inr,inc
c		WRITE(icho3,32104) runoff1_h(inr,inc), riv_rof_h(inr,inc),dovwb,balnt
c32104 format('ROFF1,RORIV,DOH,BAL',4F10.2)
c	ENDIF

	dgwwb =(GWdepA(inr,inc)-gwd_h(inr,inc))*Speyield(inr,inc)*1000.	!(m->mm)
	balnt =runoff2_h(inr,inc)-gwsoil_h(inr,inc)-QreA_h(inr,inc)
     ^	+tgwir_h(inr,inc)+dgwwb
	IF (abs(balnt).gt.1.0E-1) THEN
		WRITE(icho3, *) 'WATER INBALANCE AT Ground Water (mm)'
		WRITE(icho3,32101) ITER, i_y,i_m,i_d,i_h,inr,inc
		WRITE(icho3,32105) runoff2_h(inr,inc), gwsoil_h(inr,inc),
     ^		QreA_h(inr,inc), tgwir_h(inr,inc),dgwwb,balnt
		WRITE(icho3,32109) gwd_h(inr,inc)*1000.,GWdepA(inr,inc)*1000.
32105 format('ROFF2,GSOIL,GWRIV,IRRGW,DGH,BAL',/,6(F10.2,1X))
32109 format('DEP_END, DEP_STA',/,2(F10.2,1X))
	ENDIF

	ENDIF !in river basin
	ENDDO !inc
	ENDDO !inr

	CALL strlen(Output_D,o1,o2)
	CALL strlen(Derive_D,ll1,ll2)
	IF (iter.eq.1) THEN
c	Imkdir= makedirqq(Output_D(o1:o2))
	DO i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	IF (KID.ne.0) THEN
		CALL strlen(outstnn(i),l1,l2)
		StnCS =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALCS.txt'
		StnOG =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALOG.txt'
		StnRV =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALRV.txt'
		StnEN =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALEN.txt'
		OPEN(9992,file=StnRV,status='unknown')
		WRITE (9992,'(10A12)')'YEAR','MONTH','BHini','FLQQ','FLQR','BHend',
     $	'FOUTQ','WITHD','RTN_FW','BALR'
		CLOSE(9992)
		OPEN(9992,file=StnCS,status='unknown')
		WRITE (9992,'(13A12)')'YEAR','MONTH','PREC','ET','GWSOIL','IRR',
     $	'RTN_FLOW','IRR_GW','ROFF','DSiB','BAL','DIS','AREA'
		CLOSE(9992)
		OPEN(9992,file=StnOG,status='unknown')
		WRITE (9992,'(12A12)')'YEAR','MONTH','ROFF1','RORIV','DOH','BALO',
     $	'ROFF2','IRR_GW','GSOIL','GWRIV','DGH','BALG'
		CLOSE(9992)
		OPEN(9992,file=StnEN,status='unknown')
		WRITE (9992,'(11A12)')'YEAR','MONTH','radl','raul','rads','rans',
     $	'radt','raet','raht','rast','BALE'
		CLOSE(9992)
	ENDIF 
	ENDDO
	MainRiv =Derive_D(ll1:ll2)//'MainRiv.txt'
	OPEN(9992,file=MainRiv,status='unknown')
	totlen =0.0
	do  isub=1,n_sub
		imain=0
		do icode=1,levels
			if (mod(sub(isub).digit(icode),2).eq.0.and.
     $			sub(isub).digit(icode).ne.0) imain=1
		enddo
		if (imain.eq.0) then
			do  iflow=sub(isub).nflow,1,-1
				totlen =totlen + sub(isub).flow(iflow).dis
			enddo
		endif
	enddo
	Write (9992,'(A15,$)') 'DIS2MOUTH'
	acclen =0.0
	do  isub=1,n_sub
		imain=0
		do icode=1,levels
			if (mod(sub(isub).digit(icode),2).eq.0.and.
     $			sub(isub).digit(icode).ne.0) imain=1
		enddo
		if (imain.eq.0) then
			do  iflow=sub(isub).nflow,1,-1
				acclen =acclen +  sub(isub).flow(iflow).dis
			enddo
			dis2mouth = totlen-acclen
			Write (9992,'(I4,f10.0,$)') sub(isub).code,dis2mouth
		endif
	enddo
	CLOSE(9992)
	ENDIF

	MainRiv =Derive_D(ll1:ll2)//'MainRiv.txt'
	OPEN(9992,file=MainRiv,access='APPEND',status='old')
	Write (9992,*)
	Write (9992,'(I15,$)')  iter
	do  isub=1,n_sub
		imain=0
		do icode=1,levels
			if (mod(sub(isub).digit(icode),2).eq.0.and.
     $			sub(isub).digit(icode).ne.0) imain=1
		enddo
		if (imain.eq.0) then
c			do  iflow=sub(isub).nflow,1,-1
			Write (9992,'(f15.3,$)') sub(isub).flow(1).Qi1_j1
c			enddo
		endif
	enddo
	CLOSE(9992)


	if (i_d.eq.1.and.i_h.eq.1) then	
		Qmout= 0.0
		FOUTQ= 0.0
	endif
	Do i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
	Qmout(i)=Qmout(i)+sub(subisub(i)).flow(flowid(i)).Qi1_j1/24.0/real(imd)
	FOUTQ(i)=FOUTQ(i)+sub(subisub(i)).flow(flowid(i)).Qi1_j1*dt/(1.0E6)	!(Mm3)
	endif 
	Enddo

	BHend =0.0
	FLQR  =0.0
	FLQQ  =0.0
	WITHD =0.0
	RETNFW=0.0
	do i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
c	Save precipitation in to DayPRE
		if (i_h.eq.24) then !End of the day
			rcodeid=real(codeid2)
			CALL ana_DATAF(tprec_h,gridarea,
     $			fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
			iiday = iter /24
			DayPRE(iiday,i) = fave - BHPrec(i)
			BHPrec(i)      = fave
		endif
		if (i_d.eq.imd.and.i_h.eq.24) then !End of the month
			iimon = (i_y- startyear) * 12 + i_m
			iiyear = i_y - startyear + 1
			MonPRE(iimon,i)=fave
			YearPRE(iiyear,i)=YearPRE(iiyear,i) +fave/12.
		endif

	do 44452 isub=1,n_sub
		do 33352 iflow=sub(isub).nflow,1,-1
			B=sub(isub).flow(iflow).width
			dx=sub(isub).flow(iflow).dis
			WDj=sub(isub).flow(iflow).WDj1
			inr = sub(isub).flow(iflow).gridxy(1,1)
			inc = sub(isub).flow(iflow).gridxy(1,2)	
			if (codeid2(inr,inc).ne.-9999) then
			BHend(i)= BHend(i) + WDj*B*dx*0.001/(1.0E6)			!(million m3)
			do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)	
				FLQR(i) = FLQR(i)+ riv_rof_h(inr,inc)*gridarea(inr,inc)
     $				*fracnd(inr,inc)/100./1000.0				!(million m3)
				FLQQ(i) = FLQQ(i)+ QreA_h(inr,inc)*gridarea(inr,inc)
     $				*fracnd(inr,inc)/100./1000.0				!(million m3)
				percent =1.0- (irrriper(inr,inc)+irresper(inr,inc))/100.
				percent = max( min (percent, 1.0) , 0.0)
				WITHD(i)=WITHD(i)+wthdr_h(inr,inc)*gridarea(inr,inc)
     $				*percent*fracdd(inr,inc)/100./1000.0		!(million m3)
				rirr = irriratio(inr,inc)/100.0
				IF (rirr.gt.0.0.and.rirr.le.1.0) THEN						 !Return flow
				RETNFW(i)=RETNFW(i)+retnfw_h(inr,inc)*
     $					gridarea(inr,inc)*fracdd(inr,inc)/100./1000. !(million m3)
				ENDIF
			enddo
			do igrid = 1, sub(isub).flow(iflow).nirrg
				inr = sub(isub).flow(iflow).irrxy(igrid,1)
				inc = sub(isub).flow(iflow).irrxy(igrid,2)	
				percent =irrriper(inr,inc)/100.
				WITHD(i)=WITHD(i)+wthdr_h(inr,inc)*gridarea(inr,inc)
     $			*percent*fracdd(inr,inc)/100./1000.0			!(million m3)
			enddo
			endif !(codeid2(inr,inc).ne.-9999)
33352		continue
44452	continue
	RIVBAL(i)=BHim(i)+FLQQ(i)+FLQR(i)-BHend(i)-FOUTQ(i)
     $	-WITHD(i)+RETNFW(i)
	RIVRA = 100.0 * RIVBAL(i) / amax1 ( ( BHim(i)+BHend(i) )*0.5 , 1.)
	IF (ABS(RIVBAL(i)).gt.1.0.and.RIVRA.gt.0.01) THEN
		WRITE(icho3, *) 'WATER INBALANCE AT RIVER ROUTING (10^6 m^3)'
		WRITE(icho3, '("AT SUB",I5," FLOW",I3)') subcode(i),flowid(i)
		WRITE(icho3,32106) ITER, i_y,i_m,i_d,i_h
		WRITE(icho3,32107) BHim(i),FLQQ(i),FLQR(i),BHend(i),FOUTQ(i),
     $		WITHD(i),RETNFW(i),RIVBAL(i)
32106	FORMAT('Tstep,Year,Month,Day,Hour:',I7,1X,I5,1X,3(I3,1X))
32107	FORMAT('BHini,FLQQ,FLQR,BHend,FOUTQ,WITHD,RIVBAL',/,8(F10.1,1X))
	ENDIF			
	endif 
	end do

	if (i_d.eq.imd.and.i_h.eq.24) then

	do i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
		CALL strlen(outstnn(i),l1,l2)
		StnRV =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALRV.txt'
		OPEN(9992,file=StnRV,form='formatted',access='APPEND',status='OLD')
			WRITE (9992,5456)i_y,i_m,BHim(i),FLQQ(i),FLQR(i),BHend(i),
     $		FOUTQ(i),WITHD(i),RETNFW(i),RIVBAL(i)
		CLOSE(9992)
	endif 
	end do
5456	FORMAT (2i12,8f12.3)

	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN
	rirr  = irriratio(inr,inc)/100.0
	IF (rirr.gt.0.0.and.rirr.le.1.0) THEN !irrgation value is reliable
	wbi=(www1_i(inr,inc)*sporos(inr,inc)*zdepth1_i(inr,inc) 
     & + www2_i(inr,inc) * sporos(inr,inc) * zdepth2_i (inr,inc)  
     & + www3_i(inr,inc) * sporos(inr,inc) * zdepth3_i (inr,inc)  
     & + capac1_i(inr,inc) + capac2_i(inr,inc) + snoww1_i(inr,inc) 
     & + snoww2_i(inr,inc) + surdep_i(inr,inc)	)*1000.
	wbni=(	www1_ni(inr,inc) * sporos(inr,inc)*zdepth1_ni(inr,inc) 
     & + www2_ni(inr,inc) * sporos(inr,inc) * zdepth2_ni (inr,inc)       
     & + www3_ni(inr,inc) * sporos(inr,inc) * zdepth3_ni (inr,inc)       
     & + capac1_ni(inr,inc) + capac2_ni(inr,inc) + snoww1_ni(inr,inc) 
     & + snoww2_ni(inr,inc) + surdep_ni(inr,inc)	)*1000.
	totwbm(inr,inc)=(wbi - totwbmi(inr,inc) )* rirr + 
     &	   (wbni - totwbmni(inr,inc) )*(1 - rirr)  !mm 
	ELSE
	wbi= 0.
	wbni=(	www1_ni(inr,inc) * sporos(inr,inc)*zdepth1_ni(inr,inc) 
     & + www2_ni(inr,inc) * sporos(inr,inc) * zdepth2_ni (inr,inc)       
     & + www3_ni(inr,inc) * sporos(inr,inc) * zdepth3_ni (inr,inc)       
     & + capac1_ni(inr,inc) + capac2_ni(inr,inc) + snoww1_ni(inr,inc) 
     & + snoww2_ni(inr,inc) + surdep_ni(inr,inc)	)*1000.
	totwbm(inr,inc)=wbni - totwbmni(inr,inc)   !mm 
	ENDIF !irrigation

	over_h(inr,inc) = (SWdepA(inr,inc) - over_h(inr,inc))*1000.	!(m->mm)
	gwd_h(inr,inc)=(GWdepA(inr,inc)-gwd_h(inr,inc))*Speyield(inr,inc)*1000.	

	ENDIF !in river basin
	ENDDO !inc
	ENDDO !inr

	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	CALL ConvMMYYYY(i_m,i_y,MMYYYY)

	iprint =1
	if (iprint.eq.1) then
	tmpname=Output_D(o1:o2)//MMYYYY//'_ETmass_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,ETmass_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_roff_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,roff_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_tprec_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,tprec_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_GWdep_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	0.0000000,GWdep_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_wthdr_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	0.0000000,wthdr_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_gwsoil_h.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,gwsoil_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_dSiB.asc'
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,totwbm,max_nr,max_nc)

	endif

	tmpname=Output_D(o1:o2)//MMYYYY//'_raet.asc' !Latent heat fluxes
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,raet_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_raht.asc' !Sensible heat fluxes
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,raht_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_tc.asc' !Canopy Temperature
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,tc_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_tg.asc' !Ground Temperature
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,tg_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_td.asc' !Deep soil teemperature
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,td_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_assimn.asc' !canopy assim
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,assimn_m,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_wreq.asc' !water withdrawal requirement
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,wthreq_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_wthd.asc' !actual water withdrawals
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,wthdr_h,max_nr,max_nc)

	do i=1,max_nr
		do j=1,max_nc
			if (wthreq_h(i,j).ne.0.) then
				fshort(i,j) = wthdr_h(i,j) / wthreq_h(i,j)
			else
				fshort(i,j) = -9999
			endif
		enddo
	enddo
	tmpname=Output_D(o1:o2)//MMYYYY//'_fshort.asc' !water shortage
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,fshort,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_retnfw.asc' !return flow
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,retnfw_h,max_nr,max_nc)

	tmpname=Output_D(o1:o2)//MMYYYY//'_tgwir' !irrigation recharge GW
	CALL writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
     $	-9999.0,tgwir_h,max_nr,max_nc)

	Do i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
		rcodeid=real(codeid2)
		CALL strlen(outstnn(i),l1,l2)
		StnCS =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALCS.txt'
		StnOG =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALOG.txt'
		StnEN =Output_D(o1:o2)//outstnn(i)(l1:l2)//'_BALEN.txt'

		CALL ana_DATAF(tprec_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FP=fave
		CALL ana_DATAF(ETmass_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FE=fave
		CALL ana_DATAF(gwsoil_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FG=fave
		CALL ana_DATAF(wthdr_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FW=fave
		CALL ana_DATAF(retnfw_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FWr=fave
		CALL ana_DATAF(tgwir_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FWg=fave
		CALL ana_DATAF(roff_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FR=fave
		CALL ana_DATAF(totwbm,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		DS=fave

		DIS = Qmout(i)*imd*24.*3600./areasum/1000.

		FB= FP-FE+FG+FW-FWr-FWg-FR-DS
		
		OPEN(9992,file=StnCS,form='formatted',access='APPEND',status='OLD')
	write (9992,5455)i_y,i_m,FP,FE,FG,FW,FWr,FWg,FR,DS,FB,DIS,areasum
		CLOSE(9992)

		CALL ana_DATAF(over_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		OD = fave
		CALL ana_DATAF(runoff1_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		OF = fave
		CALL ana_DATAF(riv_rof_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		OR = fave

		CALL ana_DATAF(gwd_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		GD = fave
		CALL ana_DATAF(runoff2_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		GF = fave
		CALL ana_DATAF(tgwir_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		FWg = fave
		CALL ana_DATAF(gwsoil_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		GG = fave
		CALL ana_DATAF(QreA_h,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		GR = fave

		OB= OF-OR-OD 
		GB= GF+FWg-GG-GR+GD

		OPEN(9992,file=StnOG,form='formatted',access='APPEND',status='OLD')
		WRITE (9992,5451)  i_y,i_m,OF,OR,OD,OB,GF,FWg,GG,GR,GD,GB
		CLOSE(9992)

		CALL ana_DATAF(radl_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R1 = fave
		CALL ana_DATAF(raul_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R2 = fave
		CALL ana_DATAF(rads_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R3 = fave
		CALL ana_DATAF(rans_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R4 = fave
		CALL ana_DATAF(radt_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R5 = fave
		CALL ana_DATAF(raet_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R6 = fave
		CALL ana_DATAF(raht_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R7 = fave
		CALL ana_DATAF(rast_m,gridarea,
     $	fracnd,rcodeid,max_nr,max_nc,fave,fmin,fmax,areasum)
		R8 = fave

		GB = R1-R2+R4-R6-R7-R8
		OPEN(9992,file=StnEN,form='formatted',access='APPEND',status='OLD')
		WRITE (9992,5452)  i_y,i_m,R1,R2,R3,R4,R5,R6,R7,R8,GB
		CLOSE(9992)
	endif 
	Enddo
	
5451	format (2i12,10f12.3)
5452	format (2i12,9f12.3)
5454	format (2i12,4f12.3)
5455	format (2i12,11f12.3)
	endif

	END !Subroutine WR_BAL()

c Save total water amount at the first time step 
	Subroutine WR_BAL_INI(sub,n_sub)
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	
	real	rirr,B,WDj,dx
	integer codeid2(max_nr,max_nc), KID

	GWdep_h		= 0.0
	ETmass_h	= 0.0
	roff_h		= 0.0
	tprec_h		= 0.0
	wthdr_h		= 0.0
	retnfw_h	= 0.0
	tgwir_h		=	0.0
	wthreq_h	=	0.0
	gwsoil_h	= 0.0
	runoff1_h	= 0.0
	riv_rof_h	= 0.0
	runoff2_h	= 0.0
	QreA_h		= 0.0
	over_h		= SWdepA
	gwd_h			= GWdepA

	radl_m	= 0.0
	raul_m	= 0.0
	rads_m	= 0.0
	rans_m	= 0.0
	radt_m	= 0.0
	raet_m	= 0.0
	raht_m	= 0.0
	rast_m	= 0.0
	tc_m    = 0.0
	tg_m    = 0.0
	td_m    = 0.0
	assimn_m= 0.0

	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN	

	rirr  = irriratio(inr,inc)/100.0
	IF (rirr.gt.0.0.and.rirr.le.1.0) THEN !irrgation value is reliable
	totwbmi(inr,inc)=(www1_i(inr,inc)*sporos(inr,inc)*zdepth1_i(inr,inc)                                      
     & + www2_i(inr,inc) * sporos(inr,inc) * zdepth2_i (inr,inc)                                        
     & + www3_i(inr,inc) * sporos(inr,inc) * zdepth3_i (inr,inc)                                        
     & + capac1_i(inr,inc) + capac2_i(inr,inc) + snoww1_i(inr,inc) 
     & + snoww2_i(inr,inc) + surdep_i(inr,inc)	)*1000. !mm 
      totwbmni(inr,inc)=( www1_ni(inr,inc)*sporos(inr,inc)                                      
     &*zdepth1_ni(inr,inc) + www2_ni(inr,inc) * sporos(inr,inc)                                        
     &* zdepth2_ni (inr,inc)  + www3_ni(inr,inc) * sporos(inr,inc)                                         
     &* zdepth3_ni (inr,inc) + capac1_ni(inr,inc) + capac2_ni(inr,inc) 
     &+ snoww1_ni(inr,inc)  + snoww2_ni(inr,inc) 
     &+ surdep_ni(inr,inc)	) *1000. !mm
	ELSE
	totwbmi(inr,inc) =0
	totwbmni(inr,inc)=
     &		(	www1_ni(inr,inc) *sporos(inr,inc)*zdepth1_ni (inr,inc)                                      
     & + www2_ni(inr,inc) * sporos(inr,inc) * zdepth2_ni (inr,inc)                                        
     & + www3_ni(inr,inc) * sporos(inr,inc) * zdepth3_ni (inr,inc)                                        
     & + capac1_ni(inr,inc) + capac2_ni(inr,inc) + snoww1_ni(inr,inc) 
     & + snoww2_ni(inr,inc) + surdep_ni(inr,inc)	) *1000.			!mm
	ENDIF !irrigation value 

	ENDIF !in river basin
	ENDDO !inc
	ENDDO !inr

	BHim =0.0
	BHPrec=0.0
	do i=1,n_stno
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
	do 44451 isub=1,n_sub
		do 33351 iflow=sub(isub).nflow,1,-1
			B=sub(isub).flow(iflow).width
			dx=sub(isub).flow(iflow).dis
			WDj=sub(isub).flow(iflow).WDj1
			inr = sub(isub).flow(iflow).gridxy(1,1)
			inc = sub(isub).flow(iflow).gridxy(1,2)	
			if (codeid2(inr,inc).ne.-9999) then
			BHim(i)	= BHim(i) + WDj*B*dx*0.001/(10.0**6)		!(million m3)
			endif
33351		continue
44451	continue		
	endif 
	end do
	End !Subroutine WR_BAL_INI()


