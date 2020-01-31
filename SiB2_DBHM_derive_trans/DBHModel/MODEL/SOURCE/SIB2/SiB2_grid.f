	subroutine SIB2_grid(dt,i_y,i_m,i_d,i_h)
	INCLUDE '../INCLUDE/common.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 
	character*80	tmpname,tmpname2
	real	r1(max_nr,max_nc),r2(max_nr,max_nc)
	real	r3(max_nr,max_nc),r4(max_nr,max_nc)
	real	roff1_i,roff2_i,roff3_i,roff4_i
	real	roff1_ni,roff2_ni,roff3_ni,roff4_ni
	real	sur_i,sur_ni

	year = real(i_y)
	day  = i_d+	(i_h-1.)/24.
	time = real(i_h-2)
	if (time.lt.0.0) time = 23.0

c	nymdij2 =(mod(Iyear,100)*10000+Imonth*100+Iday)*100+Ihour
	nymdij2 =(mod(i_y,100)*10000+i_m*100+i_d)*100+i_h-1

	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN
	nymd		=	nymdij2
	swdown		=	swdown_sib(inr,inc)
	rnetm		=	-9999.0
	zlwd		=	zlwd_sib(inr,inc)
	em			=	em_sib(inr,inc)
	tm			=	tm_sib(inr,inc)
	um			=	um_sib(inr,inc) !Wind speed
	um			=	min(15.0,um)
	um			=	max(0.20,um)
	tprec		=	tprec_sib(inr,inc)
	tprec		= max(0.0,tprec)
	em			=	max(0.0,em)
cccccccccccccccccccccccdel
	item01=inr
	item02=inc
cccccccccccccccccccccccdel	print *,zlwd,em,tm,um,tprec
	tprec_h(inr,inc) =tprec_h(inr,inc)+ tprec
	app = appcoff1(inr,inc)
	bpp = bppcoff1(inr,inc)
	cpp = cppcoff1(inr,inc)

c	get constants
	CALL const2
	zlat  =lat_10(inr,inc)		
	zlong =long_10(inr,inc)
	
	istype	=	soil_sib(inr,inc)
	bee		=	slpar4(inr,inc) !soilpara(istype,4)
	phsat	=	slpar2(inr,inc)	!soilpara(istype,2)
	satco	=	slpar3(inr,inc) !soilpara(istype,3)
	poros	=	slpar1(inr,inc) !soilpara(istype,1)
	slpp	=	slpcoff (inr,inc)
	slope	=	slpar5(inr,inc)*slpp !soilpara(istype,5) from FAO soil map 
c	slope	=	slope_sib(inr,inc)	!slope from DEM (1km avg)

c	get phenologically-varying aparc (from NDVI)
	fparc = max( fpar(inr,inc), 0.001)
	gmudmu= 1.0			!Simple used, should be changed
c	green = 0.7979		!INPUT(changed by TANG, in subroutine varcal)
	zlt	  = max( lai(inr,inc), 0.01 )				
	green = 0.8 !amin1 ( zlt/7. , 1. )
c     print *,zlwd,em,tm,um,tprec,fparc,zlt
	rirr  = irriratio(inr,inc)/100.0
c	PRINT *,'CHECK ZLT FPARC',inr,inc,zlt,fparc

c	Paramaters for surface runoff
	fn    = 1.0								!Manning's roughness parameters
	idir  = griddir(inr,inc)	!River Direction
	if (idir.eq.2.or.idir.eq.8.or.idir.eq.32.or.idir.eq.128) then
		fLm = 1.0E4*sqrt(2.0)		!River length
	else
		fLm = 1.0E4
	endif	
	fAm2  = gridarea(inr,inc)*1.0E6	!Grid area (m2)

cccccccccccccccccc Calculate Irrigation area in one grid   ccccccccccccccc
	idirr = 0 !=0 for non-irrigation, =1 for irrigation area
	IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
	   idirr = 1
		CALL Read_SIB_status(inr,inc,1)
		GWdep=GWdepA(inr,inc)
		ivtype=9
		green = 1. 
		CALL vegpar
		CALL varcal(varcal_para)
		CALL driver(iu, icho2, ichmet, isnow, nymd )
		CALL balan( 1, icho2, nymd )
		CALL inter2 
		CALL rada2  
		CALL begtem 
		CALL endtem (ipbl)  
		CALL updat2 
		CALL balan( 2, icho2, nymd ) 
		CALL outer( iout, iout1, iout2, iout3, iout4, nymd ) 
		fimm_i	= (roff1 + roff2 + roff4)*1000.
		osur		=	surdep
		CALL overland(surdep,qsmm,fimm_i,fn,slope,fLm,fAm2,dt)	
		dmm_i		=	(surdep - osur)*1000.
		qsmm_i	= qsmm				
		etmass_i= etmass
		roff_i	= roff3+qsmm/1000.
		roff1_i = roff1
		roff2_i = roff2
		roff3_i = roff3
		roff4_i = roff4
		gwsoil_i= gwsoil
		sur_i		= surdep

		radlong_i	=	radn(3,2)
		raulong_i	=	zlwup
		radshor_i	=	radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2) 
		ranshor_i	=	(1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2)
     $		+ (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2)
		radutot_i	=	radt(1) + radt(2)
		raet_i		=	etmass / dtt * hlat
		raht_i		=	hflux
		rast_i		=	chf + shf
		CALL Record_SIB_status(inr,inc,1)
		ass_i(inr,inc) = assimn

c	To calculate irrigation requirement
c	IF Root Layer < 0.3*saturated, Irrigation
c	Irrigation amount: -> surface layer saturated and root layer 0.8*saturated
		faci1=0.7
		faci2=1.0 
		faci =www(2) 
		if (irriter(inr,inc).eq.0) then
		   if (faci.lt.faci1) then
			wbl1req = (1.0 - www(1)) * poros * zdepth(1) * 1000.	!(mm)
			wbl2req = (faci2 - faci) * poros * zdepth(2) * 1000.	!(mm)
			irrireq(inr,inc) = (wbl1req + wbl2req) * rirr /coffirr	!(mm)
			wthreq_h(inr,inc)= wthreq_h(inr,inc) + irrireq(inr,inc)
			irriter(inr,inc) = iter		!Record irrigation start time step
		   endif
		else
			!Because Precipiation, Irrigation stop
			wbl1req = (1.0 - www(1)) * poros * zdepth(1) * 1000.	!(mm)
			wbl2req = (faci2 - faci) * poros * zdepth(2) * 1000.	!(mm)
			fmaxirr  = (wbl1req + wbl2req) * rirr /coffirr
			if (fmaxirr .gt.0. ) then
				dirrireq = amin1( 0. , fmaxirr - irrireq(inr,inc) )
				irrireq(inr,inc) = irrireq(inr,inc) + dirrireq
				wthreq_h(inr,inc)= wthreq_h(inr,inc) + dirrireq
			else
				wthreq_h(inr,inc)= wthreq_h(inr,inc) - irrireq(inr,inc)
				irrireq(inr,inc) = 0.0
				irriter(inr,inc) = 0
			endif
c		   if (faci.gt.faci2) then		!Because Precipiation, Irrigation stop
c			irrireq(inr,inc) = 0.0
c			irriter(inr,inc) = 0
c		   endif
		endif
	ENDIF
		
cccccccccccccccccc Calculate SIB2 Land USE in one grid   cccccccccccccccccccc
	idirr=0
	CALL Read_SIB_status(inr,inc,0)
	GWdep=GWdepA(inr,inc)
	ivtype=vege_sib(inr,inc)
	green = 0.8!amin1 ( zlt/7. , 1. )
	IF (ivtype.eq.10) THEN !water surface
		if (GWdep.lt.0.) then 
			etmass = ETday(inr,inc) /24.
		else
			etmass = ETday(inr,inc) /24. /2.
		endif
		roff	  = (tprec -etmass)/1000.
		etmass_ni = etmass
c		roff_ni	  = roff
		roff1_ni  = 0.0
		roff2_ni  = max(roff,0.0)
		roff3_ni  = min(roff,0.0)
		roff4_ni  = 0.0
		www(1)	  = 1.0
		www(2)	  = 1.0
		www(3)	  = 1.0
		gwsoil_ni = 0.0 
		fimm_ni		= (roff1_ni + roff2_ni + roff4_ni)*1000.
		osur			=	surdep
		CALL overland(surdep,qsmm,fimm_ni,fn,slope,fLm,fAm2,dt)	
		dmm_ni		=	(surdep - osur)*1000.
		qsmm_ni		= qsmm				
		sur_ni		= surdep
		roff_ni	  = roff3_ni+ qsmm/1000.

		radlong_ni=	-9999.
		raulong_ni=	-9999.
		radshor_ni=	-9999.
		ranshor_ni=	-9999.
		radutot_ni=	-9999.
		raet_ni		=	-9999.
		raht_ni		=	-9999.
		rast_ni		=	-9999.
		ass_ni(inr,inc) = -9999.
	ELSE
		CALL vegpar
		CALL varcal(varcal_para)
		CALL driver(iu, icho2, ichmet, isnow, nymd )
		CALL balan( 1, icho2, nymd )
		CALL inter2   
		CALL rada2   
		CALL begtem 
		CALL endtem (ipbl)  
		CALL updat2 
		CALL balan( 2, icho2, nymd ) 
		CALL outer( iout, iout1, iout2, iout3, iout4, nymd )
		ass_ni(inr,inc) = assimn
		fimm_ni		= (roff1 + roff2 + roff4)*1000.
		osur			=	surdep
		CALL overland(surdep,qsmm,fimm_ni,fn,slope,fLm,fAm2,dt)	
		dmm_ni		=	(surdep - osur)*1000.
		qsmm_ni		= qsmm				
		etmass_ni	= etmass
		roff_ni		= roff3+qsmm/1000.
		roff1_ni	= roff1
		roff2_ni	= roff2
		roff3_ni	= roff3
		roff4_ni	= roff4
		gwsoil_ni	= gwsoil
		sur_ni		= surdep

		radlong_ni=	radn(3,2)
		raulong_ni=	zlwup
		radshor_ni=	radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2) 
		ranshor_ni=	(1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2) 
     $		+ (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2)
		radutot_ni=	radt(1) + radt(2)
		raet_ni		=	etmass / dtt * hlat
		raht_ni		=	hflux
		rast_ni		=	chf + shf

	ENDIF
	CALL Record_SIB_status(inr,inc,0)

	IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
	  r1(inr,inc)  = (roff1_i*rirr + roff1_ni* (1.-rirr))*1000.0
	  r2(inr,inc)  = (roff2_i*rirr + roff2_ni* (1.-rirr))*1000.0	
	  r3(inr,inc)  = (roff3_i*rirr + roff3_ni* (1.-rirr))*1000.0	
	  r4(inr,inc)  = (roff4_i*rirr + roff4_ni* (1.-rirr))*1000.0	
	  gwrsa(inr,inc)=(gwsoil_i*rirr+ gwsoil_ni* (1.-rirr))*1000.0	
		riv_rof(inr,inc)=(qsmm_i*rirr+ qsmm_ni * (1.-rirr))
		SWdepA(inr,inc) =sur_i*rirr	 + sur_ni* (1.-rirr)
	  ETmass_h(inr,inc) = ETmass_h(inr,inc)+
     $	etmass_i*rirr + etmass_ni*(1.- rirr)
	  roff_h(inr,inc)	= roff_h(inr,inc)+
     $	(roff_i*rirr  + roff_ni* (1.-rirr))*1000.0

		if (radlong_ni.ge.0.) then	!to avoid Water surface
     			radlong(inr,inc)=	radlong_i*rirr + radlong_ni* (1.-rirr)	
     			raulong(inr,inc)=	raulong_i*rirr + raulong_ni* (1.-rirr)	
     			radshor(inr,inc)=	radshor_i*rirr + radshor_ni* (1.-rirr)	
     			ranshor(inr,inc)=	ranshor_i*rirr + ranshor_ni* (1.-rirr)	
     			radutot(inr,inc)=	radutot_i*rirr + radutot_ni* (1.-rirr)	
     			raet(inr,inc)		=	raet_i*rirr + raet_ni* (1.-rirr)	
     			raht(inr,inc)		=	raht_i*rirr + raht_ni* (1.-rirr)	
     			rast(inr,inc)		=	rast_i*rirr + rast_ni* (1.-rirr)	
			tcts(inr,inc)		=	tc_i(inr,inc)*rirr + tc_ni(inr,inc)* (1.-rirr)
			tgts(inr,inc)		=	tg_i(inr,inc)*rirr + tg_ni(inr,inc)* (1.-rirr)
			tdts(inr,inc)		=	td_i(inr,inc)*rirr + td_ni(inr,inc)* (1.-rirr)
			assts(inr,inc)=ass_i(inr,inc)*rirr + ass_ni(inr,inc)* (1.-rirr)
		else
     			radlong(inr,inc)=	-9999.
     			raulong(inr,inc)=	-9999.
     			radshor(inr,inc)=	-9999.
     			ranshor(inr,inc)=	-9999.
     			radutot(inr,inc)=	-9999.
     			raet(inr,inc)		=	-9999.
     			raht(inr,inc)		=	-9999.
     			rast(inr,inc)		=	-9999.
			tcts(inr,inc)		=	-9999.
			tgts(inr,inc)		=	-9999.
			tdts(inr,inc)		=	-9999.
			assts(inr,inc)  = -9999.
		endif
	ELSE
	  r1(inr,inc)	= roff1_ni*1000.0
	  r2(inr,inc)	= roff2_ni*1000.0
	  r3(inr,inc)	= roff3_ni*1000.0
	  r4(inr,inc)	= roff4_ni*1000.0
	  gwrsa(inr,inc)		= gwsoil_ni*1000.0
		riv_rof(inr,inc)	= qsmm_ni
		SWdepA(inr,inc)		= sur_ni
	  ETmass_h(inr,inc)	= ETmass_h(inr,inc)+etmass_ni
	  roff_h(inr,inc)		= roff_h(inr,inc)+roff_ni*1000.0
		if (radlong_ni.ge.0.) then
     			radlong(inr,inc)=	radlong_ni
     			raulong(inr,inc)=	raulong_ni
     			radshor(inr,inc)=	radshor_ni
     			ranshor(inr,inc)=	ranshor_ni
     			radutot(inr,inc)=	radutot_ni
     			raet(inr,inc)		=	raet_ni
     			raht(inr,inc)		=	raht_ni
     			rast(inr,inc)		=	rast_ni
			tcts(inr,inc)		=	tc_ni(inr,inc)
			tgts(inr,inc)		=	tg_ni(inr,inc)
			tdts(inr,inc)		=	td_ni(inr,inc)
			assts(inr,inc)  = ass_ni(inr,inc)
		else
     			radlong(inr,inc)=	-9999.
     			raulong(inr,inc)=	-9999.
     			radshor(inr,inc)=	-9999.
     			ranshor(inr,inc)=	-9999.
     			radutot(inr,inc)=	-9999.
     			raet(inr,inc)		=	-9999.
     			raht(inr,inc)		=	-9999.
     			rast(inr,inc)		=	-9999.
			tcts(inr,inc)		=	-9999.
			tgts(inr,inc)		=	-9999.
			tdts(inr,inc)		=	-9999.
			assts(inr,inc)  = -9999.
		endif
	ENDIF

	runoff1(inr,inc)   = r1(inr,inc) + r2(inr,inc) + r4(inr,inc)
	runoff2(inr,inc)   = r3(inr,inc)
	runoff1_h(inr,inc) = runoff1_h(inr,inc)+ runoff1(inr,inc)
	gwsoil_h(inr,inc)  = gwsoil_h(inr,inc) + gwrsa(inr,inc)
	runoff2_h(inr,inc) = runoff2_h(inr,inc)+ runoff2(inr,inc)
	riv_rof_h(inr,inc) = riv_rof_h(inr,inc)+ riv_rof(inr,inc)

c	r4(inr,inc) = otest1*1000.

c Estimate Groundwater-surface water exchange (GWriver_RT)
	if (demnd(inr,inc).ne.-9999) then
		GWdep =GWdepA(inr,inc)
		GHc   = satco
		Ght_1 =	GWdep
		GRg   =	Drw(inr,inc)
		GRc   =	runoff2(inr,inc)-gwrsa(inr,inc)		!(mm)
		GHb	  =	Dimp(inr,inc)-GWdep
		GS	  =	Speyield(inr,inc)
		GWD	  = gridarea(inr,inc)*(10.**6.)/fLm/1.0	!(km2*10^6/m ->m)
		IF (ivtype.ne.10) Then
			CALL  GWriver_RTDupuit(Gqt,dt,Ght_1,
     $			GRg,GRc,GHc,GHb,GS,GWD,slope,sodep)
			Qre   = Gqt
		Else
			Ght_1 = GWdepA(inr,inc) - GRc/GS/1000.
			Qre   = GRc	
			if (Ght_1.gt.0.0) Qre = -Ght_1*GS*1000.
		Endif
		QreA(inr,inc)   = Qre
		GWdepA(inr,inc) = Ght_1
	else
		QreA(inr,inc)   = runoff2(inr,inc)-gwrsa(inr,inc)
	endif

	QreA_h(inr,inc) = QreA_h(inr,inc) + QreA(inr,inc)
	
	ENDIF !IF DEM.NE.-9999
	ENDDO !DO INC
	ENDDO !DO INR

	CALL ana_DATAF(r1,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fr1 = fave
	CALL ana_DATAF(r2,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fr2 = fave
	CALL ana_DATAF(r3,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fr3 = fave
	CALL ana_DATAF(r4,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fr4 = fave
	CALL ana_DATAF(tprec_sib,gridarea,
     $	fracnd,demnd,max_nr,max_nc,fave,fmin,fmax,areasum)
	fprec = fave
	write(icho32,'(i,1x,5(f18.12,1x))') iter,fr1,fr2,fr3,fr4,fprec

	END	! END SUBROUTINE SIB2_grid

	SUBROUTINE Record_SIB_status(inr,inc,ID_irr)
	INCLUDE '../INCLUDE/common.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 		

	IF (ID_irr.ne.0.and.ID_irr.ne.1) PRINT*,"ERROR IN ID_irr"
	if (ID_irr.eq.0) then
		tc_ni(inr,inc) = tc
		tg_ni(inr,inc) = tg
		td_ni(inr,inc) = td
		capac1_ni(inr,inc) = capac(1)
		capac2_ni(inr,inc) = capac(2)
		snoww1_ni(inr,inc) = snoww(1)
		snoww2_ni(inr,inc) = snoww(2)
		www1_ni(inr,inc) = www(1)
		www2_ni(inr,inc) = www(2)
		www3_ni(inr,inc) = www(3)
		surdep_ni(inr,inc) = surdep
	else
		tc_i(inr,inc) = tc
		tg_i(inr,inc) = tg
		td_i(inr,inc) = td
		capac1_i(inr,inc) = capac(1)
		capac2_i(inr,inc) = capac(2)
		snoww1_i(inr,inc) = snoww(1)
		snoww2_i(inr,inc) = snoww(2)
		www1_i(inr,inc) = www(1)
		www2_i(inr,inc) = www(2)
		www3_i(inr,inc) = www(3)
		surdep_i(inr,inc) = surdep
	endif

	END !

c	READ initial status parameters for each grid
c	ID_irr: IF ID_irr =0 then normal vegetation from SIB2 Land use map
c			IF ID_irr =1 then Land USE is Irrigation area (SIB2 Legend: 9 Agriculture)
	SUBROUTINE Read_SIB_status(inr,inc,ID_irr)
	INCLUDE '../INCLUDE/common.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 		
	
	IF (ID_irr.ne.0.and.ID_irr.ne.1) PRINT*,"ERROR IN ID_irr"

	if (ID_irr.eq.0) then
		tc = tc_ni(inr,inc)
		tg = tg_ni(inr,inc)
		td = td_ni(inr,inc)
		capac(1) = capac1_ni(inr,inc)
		capac(2) = capac2_ni(inr,inc)
		snoww(1) = snoww1_ni(inr,inc)
		snoww(2) = snoww2_ni(inr,inc)
		www(1) = www1_ni(inr,inc)
		www(2) = www2_ni(inr,inc)
		www(3) = www3_ni(inr,inc)
		surdep = surdep_ni(inr,inc)
	else
		tc = tc_i(inr,inc)
		tg = tg_i(inr,inc)
		td = td_i(inr,inc)
		capac(1) = capac1_i(inr,inc)
		capac(2) = capac2_i(inr,inc)
		snoww(1) = snoww1_i(inr,inc)
		snoww(2) = snoww2_i(inr,inc)
		www(1) = www1_i(inr,inc)
		www(2) = www2_i(inr,inc)
		www(3) = www3_i(inr,inc)
		surdep = surdep_i(inr,inc)
	endif
	 DO i=1,3
		if (www(i).lt.0.0.or.www(i).gt.1.0) then
			www(i)=www_ini(i)
		endif
		if (i.ne.3) then
		  if (snoww(i).lt.0.0)then
			snoww(i)=0.0
		  endif
		  if (capac(i).lt.0.0)then
			capac(i)=0.0
		  endif
		endif
	 ENDDO		
	END ! END SUBROUTINE Read_SIB_status

c	Overland flow, surface flow along the hillslop elments
c	fi  : net input (net surface runoff) (mm)
c	slp : friction slope gradient S0(-)
c	htm  : surface water depth (m)
c	n  : manning roughness parameters
c	L  : river lengh in this grid (m)
c	A  : Grid area (m2)
c	qs = sqrt(slp)* h**(5/3) /n
c	qs : discharge per unit width (m3 s-1 m-1)
c	qsmm: qs -> qs (unit: mm)
	SUBROUTINE overland(htm,qsmm,fimm,n,slp,fLm,fAm2,dt)
	real htm,qsmm,fimm,n,slp,fLm,fAm2,dt
c	qs = sqrt(slp)* htm**(5/3) /n	!(m3 s-1 m-1)
	qsmm= sqrt(slp)* htm**(5/3) /n*dt*fLm/fAm2*1000./10.		!(mm)
	if ((htm +fimm/1000.0 -qsmm/1000.0).gt.0.0) then
		htm = htm +fimm/1000.0 -qsmm/1000.0 
	else
		qsmm = htm*1000. +fimm
		htm = 0.0
	endif
	END ! END SUBROUTINE overland

c Groundwater Surface water interaction
c Based on Dupuit equ.
c Assumeing flow lines are spproximately parallel to the bed
c according to Dupuit-Fprchheimer approximation, then, by means of Darcy's law
c q=-KH(dZ/ds)= -KH(dH/ds*cosa+sina)
c Refer: Childs (1971)
c Childs, E.C. 1971. Drainage of groundwater resting on a sloping bed. Water Resour. Res.
	SUBROUTINE GWriver_RTDupuit(qt,dt,ht_1,Rg,Rc,Hc,Hb,S,Len,
     $	slope,sodep)
c Output
c	qt: leakage(mm/h)
c Input
c	dt:		time step length (1h = 3600 s)
c	ht_1:	water table depth (m)
c	Rg:		river water depth (from ground to river stage, m)
c	Rc:		Groundwater recharge/discharge (mm/h)
c	Hc:		hydraulic conductivity ( m/s)
c	Hb:		Hb: depth of saturated layer (m)
c	S:		Specific yield (-)
c	Len:	Length of the grid
c	slope:  Slope of the hillslope ( sin (a), a is the slope in radians)
c	sodep:	Soil depth (m)
	real	qt,dt,Rc,Hc,Hb,S,ht_1,Len,HafLen,DEP,qtt
	real	kh, hg2,ds,tana,cosa,ht_tmp,Hb_tmp

	HafLen = Len *0.5	!(Lengh of the hillslope)
	DEP    = ht_1 + Hb  !(Depth from impermeable layer to ground)
	if (abs(slope).lt.1.0) then
	tana   = slope / sqrt( 1 - slope * slope)
	cosa   = sqrt( 1 - slope * slope)
	else
	tana   = 2.0
	cosa   = 1./sqrt(5.0)
	endif
	tana = min (tana , 2.0 )
	cosa = max (cosa , 1./sqrt(5.0) )

	ht_1 = ht_1 - Rc/S/1000.			!(m)
	Hb	 = Hb + Rc/S/1000.				!(m)

	hg2 = DEP-Rg*cosa					!(m)
	kh  = Hc*(Hb+hg2)/2.				!(m2/s)	
	ds  = HafLen/2./cosa+Rg*slope		!(m)
	qt  = kh*(slope+cosa*(Hb-hg2)/ds )	!(qt >0, from gw to river, m2/s)
	qt  = qt/HafLen						!(m/s)
	qt  = qt* dt*1000.					!(m/s -> mm/h)

	!If groundwater is too deep
	!Water recharged from river
	if ( ht_1.gt.(0.8*DEP) ) then
		qtt = ( 0.8*DEP - ht_1 )*S*1000.	!(m -> mm)  
		qt  = amin1 (qtt , qt)
	endif

	!If Groundwater is shallow than soil layer depth
	!Water flow to River
	if (ht_1.lt.sodep) then	
		qtt = (sodep - ht_1)*S*1000.		!(m -> mm)
		qt  = amax1 (qtt , qt)
	endif

	END 

c Groundwater Surface water interaction
c Simplified one (not used)
	SUBROUTINE GWriver_RT_sim(qt,dt,ht_1,Rg,Rc,Hc,Hb,S,Len,slope)
c Output
c	qt: leakage(mm/h)
c Input
c	dt:		time step length (1h = 3600 s)
c	ht_1:	water table depth (m)
c	Rg:		river water depth (from ground to river stage, m)
c	Rc:		Groundwater recharge/discharge (mm/h)
c	Hc:		hydraulic conductivity ( m/s)
c	Hb:		Hb: depth of saturated layer (m)
c	S:		Specific yield (-)
c	Len:	Length of the grid
c	slope:  Slope of the hillslope ( sin (a), a is the slope in radians)
	real	qt,dt,Rc,Hc,Hb,S,ht_1,Len,HafLen
	real	k, tana

	HafLen = Len*0.5
	if (abs(slope).lt.1.0) then
	tana   = slope / sqrt( 1 - slope * slope)
	else
	tana   = 2.0
	endif
	tana = min (tana , 2.0 )
	k=Hc*(2.0*Hb+ht_1-Rg)/2.0/HafLen/HafLen		!(1/s)	
	ht_1 = ht_1 - Rc/S/1000.	!(m)

	if (Hb.gt.(0.2*(Hb+ht_1))) then
		qt = k * (HafLen*tana+ Rg - ht_1)	!(qt >0, from gw to river, m/s)
		qt = qt* dt*1000.					!(m/s -> mm/h)
	else
		qt = Rc
	endif

	END 

c Groundwater Surface water interaction
c Rushton and Tomlinson (1979)
c Also refer to Marios Sophocleous (2002) Hydrogeology Journal
	SUBROUTINE GWriver_RT(qt,dt,ht_1,Rg,Rc,Hc,Hb,S)
c Output
c	qt: leakage(mm/h)
c Input
c	dt:		time step length (1h = 3600 s)
c	ht_1:	water table depth (m)
c	Rg:		river water depth (from ground to river stage, m)
c	Rc:		Groundwater recharge/discharge (mm/h)
c	Hc:		hydraulic conductivity ( m/s)
c	Hb:		Hb: depth of saturated layer (m)
c	S:		Specific yield (-)
	real	qt,dt,Rc,Hc,Hb,S,ht_1
	real	k

	ht_1 = ht_1 - Rc/S/1000.	!(m)
	k=Hc / Hb					!(m/s /m -> 1/s)	
	qt = k * (Rg - ht_1)		!(qt >0, from gw to river, m/s)
	qt = qt* dt*1000.			!(m/s -> mm/h)
	
	END 

c Groundwater Surface water interaction
c Smedema and Ryacroft (1983)
c Used in SWIM by Hattermann (2005)
c BE PROVED CAN NOT WORK (TANG, 2005/9/7)
c IT IS NOT SUITABLE IN CURRENT VERSION]
c IMPROVE IT OR DO NOT USE IT
	SUBROUTINE GWriver_SWIM(qt,qt_1,ht_1,dt,Rc,K,Hb,S,L)
c Output	
c	qt: return flow (mm/h)
c	ht: water table depth from ground (m)
c Input
c	qt_1:	return flow (mm/h)
c	ht_1:	water table depth (m)	
c	dt:		time step length (1h = 3600 s)
c	Rc:		Groundwater recharge (mm/h)
c	T:		Hydraulic transmissivity (m2/s)
c		T=K*Hb K: hydraulic conductivity ( m/s) Hb: depth of saturated layer (m)
c	S:		Specific yield (-)
c	L:		slope lenght (m)
	real	qt,qt_1,ht_1,dt,Rc,T,S,L,K,Hb
	real	a, adt, exp_adt
	
	T=K*Hb			!(m2 /s)
	a=10.0*T/S/L/L	!(1/s)
	adt=a * dt		
	exp_adt = exp (- adt)
	qt =qt_1 * exp_adt + Rc * (1 - exp_adt)

	END 
