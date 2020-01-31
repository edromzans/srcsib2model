	real lon0
	real long_10(max_nr,max_nc),lat_10(max_nr,max_nc)
	common lon0,long_10,lat_10

	integer	vege_sib(max_nr,max_nc),soil_sib(max_nr,max_nc)
	real slope_sib(max_nr,max_nc),distomon(max_nr,max_nc)	
	real fpar(max_nr,max_nc)		
	real lai(max_nr,max_nc)		
	common vege_sib,soil_sib,distomon	
	common slope_sib,fpar,lai	
	
	real varcal_para(9,585)				!derived parameters for SIB2
	common 	varcal_para		
	
	real tm_o(max_nr,max_nc)		!Mean temperature data after interpolate
	real tmax_o(max_nr,max_nc)		!Max temperature data after interpolate
	real tmin_o(max_nr,max_nc)		!Min temperature data after interpolate
	real um_o(max_nr,max_nc)			!Relative humid data after interpolate
	real n_summ_o(max_nr,max_nc)	!Cloud cover data after interpolate
	real fsm_o(max_nr,max_nc)			!Mean Wind Rate data after interpolate
	real rsum_o(max_nr,max_nc)		!Precipitation data after interpolate
	real sun_o(max_nr,max_nc)			!Sunshine Time data after interpolate
	common	tm_o,tmax_o,tmin_o,um_o,n_summ_o,fsm_o,rsum_o,sun_o
	
	real ETday(max_nr,max_nc)			!Mean temperature data after interpolate
	common	ETday
	real ETmass_h(max_nr,max_nc)
	common 	ETmass_h
	real roff_h(max_nr,max_nc)
	common 	roff_h		
	real tprec_h(max_nr,max_nc)
	common 	tprec_h		
	real GWdep_h(max_nr,max_nc)
	common	GWdep_h
	real wthdr_h(max_nr,max_nc)			!record water withdrawal
	common	wthdr_h	
	real gwsoil_h(max_nr,max_nc)		!record GW to soil water
	common 	gwsoil_h		
	real	wthreq_h(max_nr,max_nc)			!record water withdrawal requirement
	common	wthreq_h
	
	real Dimp(max_nr,max_nc)			!impervious layer depth (m)
	real Drw(max_nr,max_nc)				!river water depth (m)(the distance from river water level to ground)
	real Speyield(max_nr,max_nc)		!Specific yield (m3 m-3)
	common 	Dimp,Drw,Speyield
	
	real tm_sib(max_nr,max_nc)			!temperature data for sib2 input
	real em_sib(max_nr,max_nc)			!vapor pressure data for sib2 input
	real zlwd_sib(max_nr,max_nc)		!vapor pressure data for sib2 input
	real um_sib(max_nr,max_nc)			!wind speed data for sib2 input 
	integer randihour(max_nr,max_nc)!random ihour (3, 20)
	real tprec_sib(max_nr,max_nc)		!precipitaion data for sib2 input
	real swdown_sib(max_nr,max_nc)	!solar radiation data for sib2 input
	real	runoff1(max_nr,max_nc)		!roff1: surface runoff (mm)
	real	riv_rof(max_nr,max_nc)		!riv_rof: real time surface runoff to river way (mm)
	real  runoff2(max_nr,max_nc)		!roff2: sub-surface runoff (mm)
	real  riv_roff(max_nr,max_nc)		!riv_roff: real time total runoff to river way (mm)
	integer griddir(max_nr,max_nc) !flow direction map
	real	irriratio(max_nr,max_nc) 	!irrigation ratio of the grid (km2)
	real	irriratioo(max_nr,max_nc) 	!irrigation ratio of the grid (km2) from digit map
	real	irrireq(max_nr,max_nc) 		!irrigation water reqired	(mm)
	integer irriter(max_nr,max_nc)	!irrigation start time step
	real	sporos(max_nr,max_nc)			!sporos: soil porosity (-)
	real	zdepth1_i(max_nr,max_nc)	!zdepth1_i: soil layer 1 depth of irrigation zone 
	real	zdepth2_i(max_nr,max_nc)
	real	zdepth3_i(max_nr,max_nc)
	real	zdepth1_ni(max_nr,max_nc)	!zdepth1_ni: soil layer 1 depth of non-irrigation zone 
	real	zdepth2_ni(max_nr,max_nc)
	real	zdepth3_ni(max_nr,max_nc)	
	real  gwrsa(max_nr,max_nc)			!gwrsa: groundwater to soil water (mm)

	common 	tm_sib,em_sib,zlwd_sib,um_sib,randihour,tprec_sib,swdown_sib
	common	runoff1,riv_rof,runoff2,riv_roff,irriratio,irrireq,irriter
	common	sporos,zdepth1_i,zdepth2_i,zdepth3_i,gwrsa,griddir
	common	zdepth1_ni,zdepth2_ni,zdepth3_ni
	common	irriratioo

	real	slpar1(max_nr,max_nc)
	real	slpar2(max_nr,max_nc)
	real	slpar3(max_nr,max_nc)
	real	slpar4(max_nr,max_nc)
	real	slpar5(max_nr,max_nc)
	common	slpar1,slpar2,slpar3,slpar4,slpar5
	
	real	appcoff1(max_nr,max_nc)
	real	bppcoff1(max_nr,max_nc)
	real	cppcoff1(max_nr,max_nc)
	real	slpcoff(max_nr,max_nc)
	common appcoff1,bppcoff1,cppcoff1,slpcoff
	
	integer irrcode(max_nr,max_nc)	!irrcode map (code shows irrigation area id)
	integer irr_riv(max_nr,max_nc) 	!irr_riv map of river and intervals code
	real		irrriper(max_nr,max_nc) !irrriver map of ratio supplied by river
	integer irr_res(max_nr,max_nc) 	!irr_res map of reservoir code
	real		irresper(max_nr,max_nc) !irresper map of ratio supplied by reservoir
c	real		wthdr(max_nr,max_nc)		!water withdraw
	common irrcode,irr_riv,irrriper,irr_res,irresper
			
	real	gridarea(max_nr,max_nc) 	!area of grid (km2)
	real 	demd(max_nr,max_nc)			  !DEM data which includes dissipative area		
	real	fracdd(max_nr,max_nc) 			!fraction inside study area includes dissipative area
	real	demnd(max_nr,max_nc) 			!DEM data which does NOT includes dissipative area
	real	fracnd(max_nr,max_nc)			!fraction inside study area does NOT includes dissipative area
	common	gridarea,demd,fracdd,demnd,fracnd
	
	real totwbmi(max_nr,max_nc)	!total water balance of soil+canopy+ground (monthly check, start) 
	real totwbmni(max_nr,max_nc)	!total water balance of soil+canopy+ground ('non-irrigation' part) 	
	common	totwbmi,totwbmni
	
	real over_h(max_nr,max_nc)		!Overland flow (monthly check, start)
	real runoff1_h(max_nr,max_nc)
	real riv_rof_h(max_nr,max_nc)
	common over_h,runoff1_h,riv_rof_h
	
	real gwd_h(max_nr,max_nc)			!GroundWater depth (monthly check, start)
	real runoff2_h(max_nr,max_nc)
	real QreA_h(max_nr,max_nc)
	common gwd_h,runoff2_h,QreA_h
	
	integer nymdij2,nr,nc,isnow,ipbl
	common  nymdij2,nr,nc,isnow,ipbl
	
ccc  Save tc,tg,td,capac,snoww,www,SWdep,GWdep,Qre cccc	
	real	tc_ni(max_nr,max_nc),tc_i(max_nr,max_nc)
	real	tg_ni(max_nr,max_nc),tg_i(max_nr,max_nc)	
	real	td_ni(max_nr,max_nc),td_i(max_nr,max_nc)
	real	ass_ni(max_nr,max_nc),ass_i(max_nr,max_nc)
	real	capac1_ni(max_nr,max_nc),capac1_i(max_nr,max_nc)
	real	capac2_ni(max_nr,max_nc),capac2_i(max_nr,max_nc)
	real	snoww1_ni(max_nr,max_nc),snoww1_i(max_nr,max_nc)
	real	snoww2_ni(max_nr,max_nc),snoww2_i(max_nr,max_nc)
	real	www1_ni(max_nr,max_nc),www1_i(max_nr,max_nc)
	real	www2_ni(max_nr,max_nc),www2_i(max_nr,max_nc)
	real	www3_ni(max_nr,max_nc),www3_i(max_nr,max_nc)
	real	SWdepA(max_nr,max_nc)		!(m)
	real	GWdepA(max_nr,max_nc)		!(m)							
	real	QreA(max_nr,max_nc)		
	common tc_ni,tc_i,tg_ni,tg_i,td_ni,td_i,capac1_ni,capac1_i
	common capac2_ni,capac2_i,snoww1_ni,snoww1_i,snoww2_ni,snoww2_i
	common www1_ni,www1_i,www2_ni,www2_i,www3_ni,www3_i
	common SWdepA,GWdepA,QreA
	common ass_ni,ass_i

	real	surdep_i(max_nr,max_nc)		!surdep_i: surface runoff depth (m) of irrigation zone
	real	surdep_ni(max_nr,max_nc)	!surdep_ni: surface runoff depth (m) of non-irrigation zone
	real	sura(max_nr,max_nc)			!sura: surface runoff depth (m) _avg
	common  surdep_i,surdep_ni,sura

	real	retnfw_i(max_nr,max_nc)		!return flow !retnfw_i: return flow depth(mm)
	real	retnfw_h(max_nr,max_nc)		!summary return flow every month (mm)
	real	tgw_irr(max_nr,max_nc)		!irrigation recharge to groundwater (mm)
	real	tgwir_h(max_nr,max_nc)		!summary irrigation recharge to groundwater (mm)
	common	retnfw_i,tgw_irr,retnfw_h,tgwir_h
	
c	engegy balance check
	real radlong(max_nr,max_nc)	!downward longwave (w m-2)
	real raulong(max_nr,max_nc)	!upward longwave (w m-2)
	real radshor(max_nr,max_nc) !downward shortwave (w m-2)
	real ranshor(max_nr,max_nc) !net shortwave (w m-2)
	real radutot(max_nr,max_nc) !total net radiation (w m-2)
	real raet(max_nr,max_nc)	!radiation for evaporation
	real raht(max_nr,max_nc)	!radiation for sensible heating
	real rast(max_nr,max_nc)	!radiation for storage

	real radl_m(max_nr,max_nc)	!downward longwave (w m-2) (avg month)
	real raul_m(max_nr,max_nc)	!upward longwave (w m-2) (avg month)
	real rads_m(max_nr,max_nc) !downward shortwave (w m-2) (avg month)
	real rans_m(max_nr,max_nc) !net shortwave (w m-2) (avg month)
	real radt_m(max_nr,max_nc) !total net radiation (w m-2) (avg month)
	real raet_m(max_nr,max_nc)	!radiation for evaporation (avg month)
	real raht_m(max_nr,max_nc)	!radiation for sensible heating (avg month)
	real rast_m(max_nr,max_nc)	!radiation for storage (avg month)
	common radlong,raulong,radshor,ranshor,radutot,raet,raht,rast
	common radl_m,raul_m,rads_m,rans_m,radt_m,raet_m,raht_m,rast_m
	
	real tcts(max_nr,max_nc)	!canopy temperature (K) (time step)
	real tgts(max_nr,max_nc)	!ground surface temperature (K) (time step)
	real tdts(max_nr,max_nc)	!deep soil temperature (K) (time step)	
	real tc_m(max_nr,max_nc)	!canopy temperature (K) (avg month)
	real tg_m(max_nr,max_nc)	!ground surface temperature (K) (avg month)
	real td_m(max_nr,max_nc)	!deep soil temperature (K) (avg month)	
	common 	tcts,tgts,tdts, tc_m,tg_m,td_m

	real assts(max_nr,max_nc)	!canopy assimilaion flux (mol m-2 s-1) 
	real assimn_m(max_nr,max_nc) !canopy assimilaion flux (mol m-2 s-1) (avg month)
	common assts,assimn_m

cc Save output sation information
	integer n_stno		!  number of all output stations		
	integer subcode(max_stno),flowid(max_stno),subisub(max_stno)
	real		ctlacca(max_stno)	!station control area
	character*20 outstnn(max_stno)
	common n_stno,subcode,flowid,outstnn,subisub,ctlacca
	
	real Qh,Qd(max_stno),Qm(max_stno),Qy(max_stno),Qmout(max_stno)
	common Qh,Qd,Qm,Qy,Qmout

	real Qmobv(max_stno),Qyobv(max_stno)
	common Qmobv,Qyobv

c	Save output soil grids information
	integer nsoilg					!number of all output grids		
	integer soilgr(max_soilo,2)	!inr=soilgr(i,1);inc=soilgr(i,2)
	real	laydep(2)
	character*20 slstnn(max_soilo)	!soil station name
	common nsoilg,soilgr,laydep,slstnn
	
	
c	BHim:  Width(B)*Height(H) water stored in river cannal	
c	BHPrec:Precipitation save
	real BHim(max_stno)	,BHPrec(max_stno)						
	common BHim, BHPrec
	
	
c	real FINQ,FBE,FLQ,FEN,FOUTQ_,FBAL
c	common FINQ,FBE,FLQ,FEN,FOUTQ_,FBAL

c	the observation data !7320 for 20 year running !18300 for 50 yrs
c	real DayOBV(7320,max_stno),MonOBV(240,max_stno),YearOBV(20,max_stno)
c	real DaySIM(7320,max_stno),MonSIM(240,max_stno),YearSIM(20,max_stno)
	real DayOBV(18300,max_stno),MonOBV(600,max_stno),YearOBV(50,max_stno)
	real DaySIM(18300,max_stno),MonSIM(600,max_stno),YearSIM(50,max_stno)
	common DayOBV,MonOBV,YearOBV,DaySIM,MonSIM,YearSIM

c	the precipitation	!7320 for 20 year running !18300 for 50 yrs
c	real DayPRE(7320,max_stno),MonPRE(240,max_stno),YearPRE(20,max_stno)
	real DayPRE(18300,max_stno),MonPRE(600,max_stno),YearPRE(50,max_stno)
	common DayPRE,MonPRE,YearPRE

	real www1(max_nr,max_nc),www2(max_nr,max_nc),www3(max_nr,max_nc)
	common www1,www2,www3

	real sw1(max_nr,max_nc),sw2(max_nr,max_nc),sw3(max_nr,max_nc)
	common sw1,sw2,sw3

c	annual precipiation (m) 
	real precm(max_nr,max_nc)
	common precm
	
	real coffirr	!Coff of irrigation
	data coffirr / 0.3 /
	common coffirr
	
	real fsave_LAI(12,max_nr,max_nc) !save monthly averaged value of LAI
	real fm_LAI(max_nr,max_nc),fb_LAI(max_nr,max_nc)  ! fm: slope; fb: intercep of LAI (1982-2000)
	real fsave_FPAR(12,max_nr,max_nc) !save monthly averaged value of FPAR
	real fm_FPAR(max_nr,max_nc),fb_FPAR(max_nr,max_nc)  ! fm: slope; fb: intercep of FPAR (1982-2000)
	real fm_tm(max_nr,max_nc),fb_tm(max_nr,max_nc)
	real fav_tm(max_nr,max_nc)   ! fm: slope; fb: intercep; fav: averaged value of mean temperature (1960-2000)
	real fm_tmax(max_nr,max_nc),fb_tmax(max_nr,max_nc)
	real fav_tmax(max_nr,max_nc)   ! fm: slope; fb: intercep of max temperature (1960-2000)
	real fm_tmin(max_nr,max_nc),fb_tmin(max_nr,max_nc)
	real fav_tmin(max_nr,max_nc)   ! fm: slope; fb: intercep of min temperature (1960-2000)
	real fm_um(max_nr,max_nc),fb_um(max_nr,max_nc)
	real fav_um(max_nr,max_nc), fmm_um(max_nr,max_nc)		! fm: slope; fb: intercep of relative humity (1960-2000)
	real fm_et(max_nr,max_nc),fb_et(max_nr,max_nc)
	real fav_et(max_nr,max_nc), fmm_et(max_nr,max_nc)   ! fm: slope; fb: intercep of reference ET (1960-2000)
	real fm_rsum(max_nr,max_nc),fb_rsum(max_nr,max_nc) ! fm: slope; fb: intercep of precipiation (1960-2000)
	real fav_rsum(max_nr,max_nc), fmm_rsum(max_nr,max_nc) !fmm: min prec   
	real fm_sun(max_nr,max_nc),fb_sun(max_nr,max_nc)
	real fav_sun(max_nr,max_nc), fmm_sun(max_nr,max_nc)   ! fm: slope; fb: intercep of sunshine time (1960-2000)
	real fm_fsm(max_nr,max_nc),fb_fsm(max_nr,max_nc)
	real fav_fsm(max_nr,max_nc), fmm_fsm(max_nr,max_nc)   ! fm: slope; fb: intercep of mean wind speed (1960-2000)
	common fsave_LAI,fm_LAI,fb_LAI
	common fsave_FPAR,fm_FPAR,fb_FPAR
	common fm_tm,fb_tm,fav_tm,fm_tmax,fb_tmax,fav_tmax
	common fm_tmin,fb_tmin,fav_tmin,fm_um,fb_um,fav_um,fmm_um
	common fm_et,fb_et,fav_et,fm_rsum,fb_rsum,fav_rsum,fmm_et,fmm_rsum
	common fm_sun,fb_sun,fav_sun,fm_fsm,fb_fsm,fav_fsm,fmm_sun,fmm_fsm
	
	integer iumd(max_nr,max_nc) !id for upstream (1), middle stream (2) and down stream (3)
	real	areairr(3)	!save total irrigated area for up, mid, down stream
	common iumd,areairr
	
		
	
	
	
