c	downscale Daily data to hourly data
c	downscale temperature
c	input:
c		tmax_o,tmin_o: observed (actually interpolated data)
c		Ihour:		 local time in one day 0, 1, 2, 3 ... 23
c	output:
c		tm_sib:		output temperature at Ihour (K)
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out
	SUBROUTINE Downscale_tm(Ihour,tm_sib,tmax_o,tmin_o,demd)
c	implicit none
	INCLUDE '../INCLUDE/common.inc' 
      INCLUDE './SIB2/COMSIBC.H'	     
	integer	Ihour
	real	tm_sib(max_nr,max_nc)	!Mean temperature data 
	real	tmax_o(max_nr,max_nc)	!Max temperature data
	real	tmin_o(max_nr,max_nc)	!Min temperature data
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
	real	C2K !,x0,y0,s,znodata_f

	integer i,j
	real	fhour

	C2K = 273.16

	IF (Ihour.lt.3) THEN
		fhour = abs(Ihour+24.0-14.0)
	ELSE
		fhour = abs(14.0 - Ihour)
	ENDIF
c	print *, Ihour,fhour,sin(pi*(1-fhour/12.)/2)
	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
			tmax00 = tmax_o(i,j)
			tmin00 = tmin_o(i,j)
			tmax_o(i,j) = max (tmax00, tmin00) 
			tmin_o(i,j) = min (tmax00, tmin00)
c			IF (tmax_o(i,j).ge.tmin_o(i,j)) THEN
				tm_sib(i,j) = C2K + tmin_o(i,j)+ 
     $				(tmax_o(i,j)- tmin_o(i,j))*sin(pi*(1-fhour/12.)/2)
c			ELSE
c				tm_sib(i,j) = C2K + tmin_o(i,j)+ 
c     $				(tmax_o(i,j)- tmin_o(i,j))*sin(pi*(1-fhour/12.)/2)
c				write(icho,*) iter, Ihour, I,J, 
c     $					"WARNING: Tmax .lt. Tmin, INPUT ERROR", 
c     $				tmax_o(i,j),tmin_o(i,j)
c			ENDIF
		ELSE
c	If out the DEM map, will not check the data
				tm_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END

c	downscale Daily data to hourly data
c	downscale vapor pressure
c	input:
c		tmax_o,tmin_o: observed (actually interpolated data)
c		Ihour:		 local time in one day 0, 1, 2, 3 ... 23
c	output:
c		em_sib:		output vapor pressure at Ihour
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out
c	EF: see reference
c	J.S. Kimball et al / Agricultural and Forest Meteorology 85 (1997) 87-98
c	EF = lEp,day / lp,ann
	SUBROUTINE Downscale_em
     $	(Ihour,em_sib,tmax_o,tmin_o,um_o,demd,precm,ETday)
	implicit none
	INCLUDE '../INCLUDE/common.inc'      
	integer	Ihour
	real	em_sib(max_nr,max_nc)	!Actual vapor pressure
	real	tmax_o(max_nr,max_nc)	!Max temperature data
	real	tmin_o(max_nr,max_nc)	!Min temperature data
	real	um_o(max_nr,max_nc)		!Mean relative humid
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
	real	precm(max_nr,max_nc)	!annual precipiation (meter)
	real	ETday(max_nr,max_nc)	!ET from FAO Penman (mm/day)
	real	eoTmax,eoTmin,Tmax,Tmin,em01 !,x0,y0,s,znodata_f
	integer i,j
	real	Tdest,EF !Estimated T following J.S. Kimball et al.
	real	emoo
	real	C2K		 

	C2K = 273.16

	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
		Tmax=amax1(tmax_o(i,j),tmin_o(i,j))
		Tmin=amin1(tmax_o(i,j),tmin_o(i,j))
c		um_o(i,j) = max (um_o(i,j), 0.01)
c		eoTmax=0.6108*exp(17.27*Tmax/(Tmax+237.3))			!(E11)
c		eoTmin=0.6108*exp(17.27*Tmin/(Tmin+237.3))
c		em_sib(i,j)=um_o(i,j)*(eoTmax+eoTmin)/2.0				!(E19)	
c	Change Unit from KPa to mb
c		em_sib(i,j)=em_sib(i,j)*10.0
c		em01 = em_sib(i,j)
		if (precm(i,j).gt.0..and.ETday(i,j).ne.-9999) then
			EF = ETday(i,j)/precm(i,j)/1000.
			Tdest = (Tmin+C2K)*( -0.127												!See reference
     $			+1.121*(1.003-1.444*EF+12.312*EF*EF-32.766*EF*EF*EF)
     $			+0.0006*(Tmax-Tmin)) -C2K
			em01 = 6.1078*exp(17.269*Tdest/(Tdest+237.3))	!E(14)
			emoo = 6.1078*exp(17.269*Tmin/(Tmin+237.3))		!E(14)
			emoo = amin1(emoo,em01)
		else
			emoo = 6.1078*exp(17.269*Tmin/(Tmin+237.3))		!E(14)
		endif
c	if (i.eq.88 .and. j.eq.154) then
c		if (ihour.eq.1) print *,em01,emoo,Tdest,Tmin
c	endif
		em_sib(i,j)	=	emoo
ccccccccccccccccccccdel
c	if (em_sib(i,j).lt.5.0) then
c	print*, i,j,em_sib(i,j)
c	print*, Tmax,Tmin,eoTmax,eoTmin,6.1078*exp(17.269*Tmin/(Tmin+237.3))
c	pause
c	endif
ccccccccccccccccccccdel	

		ELSE
c	If out the DEM map, will not check the data
	em_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END

c	downscale Daily data to hourly data
c	downscale zlwd downward long wave radiation
c	input:
c		tm_sib:		 temperature from Downscale_tm
c		Ihour:		 local time in one day 0, 1, 2, 3 ... 23
c		em_sib:		 vapor pressure from Downscale_em
c	output:
c		zlwd_sib:		output downward long wave radiation at Ihour
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out			   
	SUBROUTINE Downscale_zlwd(Ihour,zlwd_sib,tm_sib,em_sib,demd)
	INCLUDE '../INCLUDE/common.inc'      
      INCLUDE './SIB2/COMSIBC.H'	 
	integer	Ihour
	real	zlwd_sib(max_nr,max_nc)	!Actual vapor pressure
	real	tm_sib(max_nr,max_nc)	!temperature data
	real	em_sib(max_nr,max_nc)	!Actual vapor pressure
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
c	real	x0,y0,s,znodata_f
c	integer nr,nc
	integer i,j

	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
c	The incoming longwave radiation comes from two sources: the sky and the environment. 
c	Similar to the diffuse radiation, it is assumed in this study that the incoming 
c	longwave radiation comes from the sky only: zldw = omiga*siga*Ta^4
c	where Ta is the shelter height air temperature and ea is the vapor pressure. 
c	This formula was developed by Brunt (1932), which was found to be more stable 
c	with temperature change than other methods (Jim¨¦nez et al, 1987). 
c	Brunt, D., (1932) Notes on radiation in the atmosphere. Quart. J. 
c	Roy. Meteor. Soc., 58: 389-418. 
c	Jim¨¦nez, J. I. , Alados-Arboledas, L., Castro-Diez, Y. and Ballester, G., (1987)
c	On the estimation of long-wave radiation flux from clear skies. 
c	Theor. Appl. Climatol., 38: 37-42.
c		omiga = 4.903E-9 !Stefan-Boltzmann constant(4.903E-9MJ/K4m2 day-1)
c		em_sib(i,j) (mb) -->change unit to Kpa
c		result: MJ m-2/h -> W m-2
c			if (em_sib(i,j).ge.0) then
				zlwd_sib(i,j)=4.903E-3/24.0*(tm_sib(i,j)**4)*
     $			(0.66+0.039*sqrt(em_sib(i,j)/10.0))/3600.
c			ELSE
c			  write(icho,*)   
c     $			  "ERROR: vapor pressure lt 0.0",em_sib(i,j),i,j,iter
c			ENDIF
		ELSE
c	If out the DEM map, will not check the data
		zlwd_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END


c	downscale Daily data to hourly data
c	downscale wind speed
c	input:
c		fsm_o: observed wind speed(actually interpolated data)
c		Ihour:		 local time in one day 0, 1, 2, 3 ... 23
c	output:
c		um_sib:		output wind speed at Ihour (K)
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out
	SUBROUTINE Downscale_um(Ihour,um_sib,fsm_o,demd)
	implicit none
	INCLUDE '../INCLUDE/common.inc'      
	integer	Ihour
	real	um_sib(max_nr,max_nc)	!wind speed for Sib2 
	real	fsm_o(max_nr,max_nc)	!wind speed data
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
c	real	x0,y0,s,znodata_f
	integer i,j

	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
			um_sib(i,j) = fsm_o(i,j)
		ELSE
c	If out the DEM map, will not check the data
			um_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END


c	downscale Daily data to hourly data
c	downscale precipitaion
c	input:
c		fsm_o: observed precipitaion(actually interpolated data)
c		Ihour:		 local time in one day 0, 1, 2, 3 ... 23
c	output:
c		um_sib:		output precipitaion at Ihour (K)
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out
	SUBROUTINE Downscale_tprec(Ihour,randihour,tprec_sib,rsum_o,demd,
     *	idum)
	implicit none
	INCLUDE '../INCLUDE/common.inc'      
	integer	Ihour
	real	tprec_sib(max_nr,max_nc)!precipitaion for Sib2 
	integer randihour(max_nr,max_nc)!random ihour (3,20)
	real	rsum_o(max_nr,max_nc)	!precipitaion data (mm/day)
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
c	real	x0,y0,s,znodata_f
	real	randfhour(max_nr,max_nc)!random hour (float point) (0,1)
	real	rand,ran0,plim,avp
	integer i,j,idum

	IF (Ihour.eq.-10) THEN
c		Under windows or Under Solar Unix f90
c		CALL RANDOM_NUMBER(randfhour)
		DO i=1,max_nr
			DO j=1,max_nc
c	Under Solar Unix system, f77 ,use rand(0)
c				randfhour(i,j)= rand(0)
				randfhour(i,j)= ran0(idum)
c				randfhour(i,j)= 0.65  !used to test
c				print *, i,j,randfhour(i,j)
c				pause
				randihour(i,j)=3+Int(randfhour(i,j)*18)
c				if (randihour(i,j).lt.3.or.randihour(i,j).gt.20) then
c				print *, randihour(i,j),randfhour,randfhour(i,j)*18
c				endif
			ENDDO
		ENDDO
	ENDIF
	plim = 4.
	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN

c	tprec_sib(i,j)=0.0
c	if (rsum_o(i,j).le.4.0) then
c		if (Ihour.eq.randihour(i,j)) tprec_sib(i,j)=rsum_o(i,j)
c	else if (rsum_o(i,j).le.12.0) then
c		if (Ihour.eq.randihour(i,j)-1) tprec_sib(i,j)=rsum_o(i,j)*0.3
c		if (Ihour.eq.randihour(i,j)) tprec_sib(i,j)=rsum_o(i,j)*0.4
c		if (Ihour.eq.randihour(i,j)+1) tprec_sib(i,j)=rsum_o(i,j)*0.3
c	else if (rsum_o(i,j).le.48.0) then
c		if (Ihour.eq.randihour(i,j)-3) tprec_sib(i,j)=rsum_o(i,j)*0.1
c		if (Ihour.eq.randihour(i,j)-2) tprec_sib(i,j)=rsum_o(i,j)*0.15
c		if (Ihour.eq.randihour(i,j)-1) tprec_sib(i,j)=rsum_o(i,j)*0.15
c		if (Ihour.eq.randihour(i,j)) tprec_sib(i,j)=rsum_o(i,j)*0.2
c		if (Ihour.eq.randihour(i,j)+1) tprec_sib(i,j)=rsum_o(i,j)*0.15
c		if (Ihour.eq.randihour(i,j)+2) tprec_sib(i,j)=rsum_o(i,j)*0.15
c		if (Ihour.eq.randihour(i,j)+3) tprec_sib(i,j)=rsum_o(i,j)*0.1
c	else
c		tprec_sib(i,j)=rsum_o(i,j)/24.0
c	endif
		avp = rsum_o(i,j)/24.0
		if (avp.lt.plim) then
		     if (Ihour.lt.INT(rsum_o(i,j)/plim)) then
			tprec_sib(i,j)=plim
		     else if (Ihour.eq.INT(rsum_o(i,j)/plim)) then
			tprec_sib(i,j)=rsum_o(i,j)-plim*INT(rsum_o(i,j)/plim)
		     else
			tprec_sib(i,j)=0.
		     endif
		else
		      tprec_sib(i,j)=avp
		endif

		ELSE
c	If out the DEM map, will not check the data
			tprec_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END

c	Use it to get rand number
      FUNCTION ran0(idum)
      INTEGER idum,IA,IM,IQ,IR,MASK
      REAL ran0,AM
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *MASK=123459876)
      INTEGER k
      idum=ieor(idum,MASK)
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      ran0=AM*idum
      idum=ieor(idum,MASK)
      return
      END
c	downscale Daily data to hourly data
c	downscale solar radiation
c	input:
c		tm_o: observed temperature(actually interpolated data)
c		um_o: observed Relative humid (actually interpolated data)
c		sun_o:observed sun shine time (actually interpolated data)
c		Ihour:		local time in one day 0, 1, 2, 3 ... 23
c		yy,mm,dd:	year, time , day
c		lon0:		the longitude where the standard time is defined
               !input time is UTC,lon0=0; BST,lon0=120; JST,lon0=135
c	output:
c		swdown_sib:		output solar radiation at Ihour (K)
c	requirement:
c		USE dem, Only where the dem data is valid, the data is taken out
	SUBROUTINE Downscale_swdown(Ihour,swdown_sib,
     $	lon0,yy,mm,dd,long_10,lat_10,tm_o,um_o,sun_o,demd)
	implicit none
	INCLUDE '../INCLUDE/common.inc'      
	integer	Ihour
	real	swdown_sib(max_nr,max_nc)	!solar radiation for Sib2 
	real	long_10(max_nr,max_nc)		!longitude
	real	lat_10(max_nr,max_nc)		!latitude
	real	tm_o(max_nr,max_nc)			!temperature data 
	real	um_o(max_nr,max_nc)			!Relative humid data 
	real	sun_o(max_nr,max_nc)		!sun shine time data 
	real	demd(max_nr,max_nc)			!only take data where DEM is not -9999
	real	lon0
	integer	yy,mm,dd

c	variables for Dr. Yang Kun 's subroutines
      real radb    ! daily or hourly solar beam radiation in clear day (w/m2)
      real radd    ! daily or hourly solar diffuse radiation in clear day (w/m2)
c      real radday0 ! daily global solar radiation in clear day (w/m2)
      real daylen   ! daytime length (hour)
c      real radday     ! estimated daily solar radiaiton (J m^-2)
      real ratio      ! realtive sunshien duration
	real lon,lat,alt,shine,pa,ta,rh

      integer hh   ! hour
      real trise    ! sunrise time (hour)
      real tset     ! sunset  time (hour)
      real halfday  ! half of daylength in radian
      real hshine   ! hourly fraction of sunshine duration
      real hh0      ! soalr hour angle in radian, =0 if noon 
      real k        !
      real solarhour ! function to calculate solar hour angle

c	temporary variables
	integer i,j
	real	C2K

	C2K = 273.16

	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
			lon = long_10(i,j)
			lat = lat_10(i,j)
			alt = demd(i,j)
			pa = 101.3*(((293.-0.0065*demd(i,j))/293.)**5.26)*1000. !FAO E(7)
			ta = tm_o(i,j) +  C2K
			rh = um_o(i,j)*100
			shine = sun_o(i,j)

c     calculate sunrise, sunset, and length of a day
			CALL sunrise(trise,lon0,lon,lat,yy,mm,dd)
			CALL sunset (tset, lon0,lon,lat,yy,mm,dd)
			daylen  = tset - trise
			shine	= min(shine, daylen)
			ratio   = shine/daylen
c     parameters in the function of sunshine fraction distribution
			halfday = daylen/ 2 * 2*3.1416 / 24
			k       = 2*ratio*(1-ratio)*3.1416/halfday
c     calculate sunshine fraction in each hour
			hh= Ihour +1
			hh0 = solarhour(lon0,lon,yy,mm,dd,hh-1,30,0)
			IF(abs(hh0).le.halfday)THEN
c     you may select bell-shaped or dome-shaped form
c     bell-shaped distribution of sunshine fraction
c               hshine = 2*k*halfday*ratio*
c     :              (1+cos(2*k*hh0))/(2*k*halfday+sin(2*k*halfday))

c     dome-shaped distribution of sunshine fraction
				hshine = k*halfday*ratio*cos(k*hh0)/sin(k*halfday)
			ELSE
				hshine = 0.
			END IF
c     radiative transfer due to cloud extinction
			IF(hshine.gt.0)THEN 
				swdown_sib(i,j) = (0.4560+0.3566*hshine+0.1873*hshine**2)
			ELSE  
				swdown_sib(i,j) = 0.2640
			END IF

c     clear-sky surface radiation: radb+radd
			CALL Rhclr(radb,radd,lon0,lon,lat,alt,pa,ta,rh, 
     :                  yy,mm,dd,hh)

c     cloudy-sky surface radiation: 
			swdown_sib(i,j) = (radb+radd) * swdown_sib(i,j)
			swdown_sib(i,j) = swdown_sib(i,j)/3600. ! J m^-2 --> W m^-2
ccccccccccccccccccdel
c	if (swdown_sib(i,j).gt.1500.) then
c	print *, i,j,swdown_sib(i,j)
c	print *, radb,radd,hshine,shine
c	print *, lon,lat,alt,pa,ta,rh
c	print *, k, halfday, ratio, hh0
c	pause
c	endif
ccccccccccccccccccdel	

		ELSE
c	If out the DEM map, will not check the data
			swdown_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END

c	downscale Daily data to hourly data
c	downscale temperature
c	There are many empirical model to determine the Temperauture curve
c	e.g.WAVE model Wit et al(1978), Hoogenboom and Huck (1986)
c		P&L Parton and Logan (1981)
c		WK Wilkerson et al. (1981)
c		TM Carla Cesaraccio et al (2001) 
c	The method is used in SIB_DHM is following Carla Cesaraccio et al (2001) 
c		Cesaraccio C, Spano D, Duce P, Snyder RL.
c		Int. J. Biometeorol. (2001) 45 : 161-169
c		An improved model for determining degree-day values from daily temperature data.
c	A revise: to fit the observated Tm value
	SUBROUTINE Downscale_tm2(Ihour,tm_sib,
     $	lon0,yy,mm,dd,long_10,lat_10,tmax_o,tmin_o,tm_o,demd)
c	implicit none
	INCLUDE '../INCLUDE/common.inc' 
	integer	Ihour,II
	real	tm_sib(max_nr,max_nc)	!Mean temperature data 
	real	tmax_o(max_nr,max_nc)	!Max temperature data
	real	tmin_o(max_nr,max_nc)	!Min temperature data
	real	tm_o(max_nr,max_nc)		!Mean temperature data
	real	demd(max_nr,max_nc)		!only take data where DEM is not -9999
	real	long_10(max_nr,max_nc)	!longitude
	real	lat_10(max_nr,max_nc)	!latitude
	real	lon0
	integer	yy,mm,dd
	real	lon,lat
      real	trise    ! sunrise time (hour)
      real	tset     ! sunset  time (hour)
	real	ttx		 ! max temperature time (tset - 4.)
      real	Txx		 ! max temperature ()
      real	Tnn		 ! min temperature (at sunrise time)
      real	Too		 ! temperature at sunset time
      real	Tpp		 ! min temperature (at sunrise time of next day)
	real	C2K		 
	integer i,j
	real	t,alpha,RR,bb,tm01,avtm,dtm

	C2K = 273.16

	Do i=1,max_nr
		DO j=1,max_nc
		IF (demd(i,j).ne.-9999) THEN
			lon = long_10(i,j)
			lat = lat_10(i,j)
			CALL sunrise(trise,lon0,lon,lat,yy,mm,dd)
			CALL sunset (tset, lon0,lon,lat,yy,mm,dd)
			ttx = tset -4.
			tmax00	= tmax_o(i,j)
			tmin00	= tmin_o(i,j)
			Txx		= max (tmax00, tmin00) 
			Tnn		= min (tmax00, tmin00)
			Tpp		= tnn	
			Too		= Txx - 0.39*(Txx - Tpp)
			alpha	= Txx - Tnn
			RR		= Txx - Too
			bb		= (Tpp-Too)/sqrt(trise+24.-tset)

			avtm = 0.
			do II=1,24
				IF (II.lt.trise) THEN
					t = II + 24.
				ELSE
					t = II
				ENDIF				
				if (t.le.ttx) then
				 tm01=Tnn+alpha*sin((t-trise)/(ttx-trise)*pi/2.)
				else if (t.lt.tset) then
				 tm01=Too+RR*sin( (1.+ (t-ttx)/4. )*pi/2.)
				else if (t.le.(trise+24.)) then
				 tm01=Too+bb*sqrt(t-tset)
				endif
				avtm = avtm + tm01
			enddo
c			dtm = tm_o(i,j) - avtm /24.
			dtm = 0.
			IF (Ihour.lt.trise) THEN
				t = Ihour + 24.
			ELSE
				t = Ihour
			ENDIF
			if (t.le.ttx) then
			 tm_sib(i,j)=C2K+Tnn+alpha*sin((t-trise)/(ttx-trise)*pi/2.)+dtm
			else if (t.lt.tset) then
			 tm_sib(i,j)=C2K+Too+RR*sin( (1.+ (t-ttx)/4. )*pi/2.)+dtm
			else if (t.le.(trise+24.)) then
			 tm_sib(i,j)=C2K+Too+bb*sqrt(t-tset)+dtm
			endif
			tmax_o(i,j) = Txx
			tmin_o(i,j) = Tnn
		ELSE
c	If out the DEM map, will not check the data
			tm_sib(i,j) = -9999.0
		ENDIF
		ENDDO
	ENDDO
	RETURN
	END
