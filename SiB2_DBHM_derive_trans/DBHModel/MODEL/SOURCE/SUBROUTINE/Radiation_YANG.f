c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE  Rdclr                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     River and Environmental Engineering Laboratory   ######
c     ######                University of Tokyo                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE  Rdclr(radb,radd,lon0,lon,lat,alt,
     :                  pa,ta,rh, yy,mm,dd,daylen)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate daily global solar radiation in clear skies
c
c#######################################################################
c      
      implicit none
c
c  INPUT:
c
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      real    alt        ! Local surface elevation (m)
      integer	yy,mm,dd   !calendar year:month:day		
      real pa         ! surface air pressure (Pa)
      real ta         ! air temperature (K)
      real rh         ! relative humidity
c
c  OUTPUT:
c
      real radb,radd ! daily solar radiation in clear day (w/m2)
c
c  Temporary:
c
      integer jday   ! julian day
      real hsun      ! the height of the sun (90-zenith angle)
	real R0	       ! horizontal extraterrestial solar insolation
				   !(w/m^2)	
      real taub      ! Beam radiation transmittance
      real taud      ! Diffuse radiation transmittance
      real trise    ! sunrise time (hour)
      real tset     ! sunset  time (hour)
      real daylen   ! daytime length (hour)
      real time     ! time (hour)
      real dt       ! time step for integrating daily solar radiation (hour)
 
      integer hh,mn,ss
      integer k  !, k1, k2  
      integer cycl
      parameter ( cycl = 20)
c      real t(cycl), tb(cycl),tb0,R00
c      real tbgn, tend          
      real pi
      parameter ( pi=3.1415927/180.0 )
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      radb = 0
      radd = 0

      CALL JULDAY( yy, mm, dd, jday )

!     calculate trise and tset
      CALL sunrise(trise,lon0,lon,lat,yy,mm,dd)
      CALL sunset (tset, lon0,lon,lat,yy,mm,dd)

      daylen = tset - trise  ! (hour) 
      dt = daylen/cycl

!     Integrate daily beam and diffuse radiation for clear-sky
      DO k = 1, cycl 
         time = trise + (k-1+0.5)*dt
         hh   = int(time)
         mn   = int(time * 60  - hh * 60) 
         ss   = int(time * 3600- hh * 3600 - mn * 60) 

         CALL solarR0(R0,hsun,lon0,lon,lat,yy,mm,dd,hh,mn,ss)

         CALL  trans(jday,lat,lon,alt,hsun,pa,ta,rh,taub,taud)

         Radb   = Radb + R0*taub * dt * 3600
         Radd   = Radd + R0*taud * dt * 3600 

      END DO  

      RETURN
      END !SUBROUTINE Rdclr

c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE  Rhclr                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     River and Environmental Engineering Laboratory   ######
c     ######                University of Tokyo                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE  Rhclr(radb,radd,lon0,lon,lat,alt,
     :                  pa,ta,rh, yy,mm,dd,hh)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate hourly global solar radiation in clear skies
c
c#######################################################################
c      
      implicit none
c
c  INPUT:
c
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      real    alt        ! Local surface elevation (m)
      integer	yy,mm,dd   !calendar year:month:day		
      integer	hh		   !standard time hour:minute:second in long0
      real pa         ! surface air pressure (Pa)
      real ta         ! air temperature (K)
      real rh         ! relative humidity
c
c  OUTPUT:
c
      real radb,radd ! hourly solar radiation in clear day (w/m2)
c
c  Temporary:
c
      integer jday   ! julian day
      real hsun      ! the height of the sun (90-zenith angle)
	real R0	       ! horizontal extraterrestial solar insolation
				   !(w/m^2)	
      real taub      ! Beam radiation transmittance
      real taud      ! Diffuse radiation transmittance
      real time      ! time (hour)
      real dt        ! time step for integrating hourly solar radiation (hour)
 
      integer k, cycl   
      integer mn,ss
      real pi
      parameter (pi=3.1415927/180.0)
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      radb   = 0
      radd   = 0

      CALL JULDAY( yy, mm, dd, jday )

      cycl = 10
      dt   = 3600.0/cycl 
      time = 0
      DO k=1,cycl
         time = - dt * (k-0.5) 
         mn = time / 60
         ss = 0

         CALL solarR0(R0,hsun,lon0,lon,lat,yy,mm,dd,hh,mn,ss)

         IF(R0.gt.0)THEN 
            CALL  trans(jday,lat,lon,alt,hsun,pa,ta,rh,taub,taud)
			
            Radb = Radb    + R0*taub * dt 
            Radd = Radd    + R0*taud * dt
         END IF
      END DO

      END !SUBROUTINE  Rhclr


c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE trans                      ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     River and Environmental Engineering Laboratory   ######
c     ######                University of Tokyo                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE  trans(jday,lat,lon,alt,hsun,pa,ta,rh,taub,taud)
c
c#######################################################################
c
c     PURPOSE:
c
c     Calculate the transmittance in clear skies
c
c#######################################################################
c      
c
c     AUTHOR: Kun Yang
c     10/11/2004 
c                                                                               
c     MODIFICATION HISTORY:
c
c#######################################################################
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
c
c     INPUT:
c
      integer jday    ! julian day
      real lat        ! latitdue of each site (deg.)
                      ! > 0 in North Hemesphere
                      ! < 0 in South Hemesphere
      real lon        ! longitude of each site (deg.)
                      ! > 0 in East Hemesphere
                      ! < 0 in West Hemesphere
      real alt        ! Local surface elevation (m)
      real hsun       ! the height of the sun (90-zenith angle)
      real pa         ! surface air pressure (Pa)
      real ta         ! air temperature (K)
      real rh         ! relative humidity
c
c  OUTPUT:
c
      real taub     ! Beam radiation transmittance  Eq. (9a)
      real taud     ! Diffuse radiation transmittance	Eq. (9b)
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      real mass     ! air mass 
      real beta        !  Angstrom turbidity coefficient           
      real beta_annual ! Annual mean Angstrom turbidity coefficient           
                       ! =0.0-0.15               
      real beta_season ! Seasonal variability of turbidity coefficient 
                       ! =0.02-0.04               
      real water    ! precipitable water (cm)
      real pp0      ! pa/p0
      real loz      ! the thickness of ozone layer (cm)

      real mp       ! mass * pa/p0
      real mb       ! mass * beta
      real moz      ! mass * loz
      real mw       ! mass * water

      real koz	  ! Parameter of ozone absorption,		   Eq. (8a)
      real wv		  ! Parameter of water vapor absorption,   Eq. (8b)
      real lamr     ! Parameter of Rayleigh acttering,	   Eq. (8d)
      real lama     ! Parameter of aerosol extinction,	   Eq. (8e)
      real gas	  ! Parameter of permanent gas absorption, Eq. (8c)

      real tauoz	  ! transmittance due to ozone absorption		   Eq. (7a)
      real tauw	  ! transmittance due to water vapor absorption	   Eq. (7b)
      real taur     ! transmittance due to Rayleigh acttering		   Eq. (7c)
      real taua     ! transmittance due to aerosol extinction		   Eq. (7d)
      real taug	  ! transmittance due to permanent gas absorption  Eq. (7e)

      real tema, temb    

      real p0       ! standard atmospheric pressure (Pa) 
      parameter (p0=101300.)
      real pi
      parameter (pi=3.1415927/180)
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c     calculate air mass
      mass =1/(sin(hsun)+0.15*(57.3*hsun+3.885)**(-1.253))
      mass=max(0.0,mass)

c     calculate relative air mass
      pp0    = pa / p0

c     calculate turbidity
      beta_annual = (0.025+0.1*(cos(lat*pi))**2)*exp(-0.7*alt/1000)
      beta_season = 0.30 * beta_annual
      beta   = beta_annual+beta_season*cos((jday-135)/365*2*3.142)**2 

c     calculate ozone thickness
      tema = jday 
      if(tema.gt.303)tema=tema-365
      temb  = ((lat-80)/60.)**2+((tema-120)/(183.0+(80-lat)))**2
      loz   = 0.44-0.16*sqrt(temb)

c     calculate precipitable water amount
      water = 0.493*rh/100/ta*exp(26.23-5416/ta) 
		
      
c     calculate transmittance

      mp  = mass * pp0
      mb  = mass * beta
      moz = mass * loz
      mw  = mass * water

      lamr = 0.5474+0.01424*mp-0.0003834*mp**2+4.59e-6*mp**3
      lama = 0.6777+0.1464*mb-0.00626*mb**2
      koz  = 0.0365*moz**(-0.2864)
      gas  = 0.0117*mp**0.3139
	if (mw.le.0.0.or.min(1.0,-0.036*log(mw)+0.909).le.0.0) then
	print *,"Radiation_YANG",mw,min(1.0,-0.036*log(mw)+0.909)
	endif
      wv   = -log(min(1.0,-0.036*log(mw)+0.909))

      taur = exp(-0.008735*mp*lamr**(-4.08))
      taua = exp(-mb*lama**(-1.3))
      tauoz= exp(-moz*koz)
      taug = exp(-gas)
      tauw = exp(-wv)					

      taub = max (0.0,taur*taua*tauoz*taug*tauw-0.013)
      taud = 0.5 * (tauoz*taug*tauw*(1-taur*taua)+0.013)

      RETURN
      END !SUBROUTINE trans

c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   SUBROUTINE solarR0                         #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      SUBROUTINE solarR0(R0,hsun,lon0,lon,lat,yy,mm,dd,hh,mn,ss)

c#############################################################################
c
c     PURPOSE:
c	Calculate horizontal extraterrestial solar insolation (w/s)
c	
c#############################################################################
	implicit none

c	INPUT:
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      integer	yy,mm,dd			!calendar year:month:day		
      integer	hh,mn,ss			!standard time hour:minute:second in lon0
c
c	OUTPUT:
	real	R0	         		!horizontal extraterrestial solar insolation
								!(w/m^2)	
	real	hsun       ! the height of the sun (rad)
c
c	WORK:
	real i00
	parameter	(i00=1353.0)		 ! solar constant
	real pai, pi
	parameter (pai=3.1415926, pi=pai/180.0)
c	integer	julian				!Julian day
	integer jday,jday0   		!day number starting with 1 on Jan.1	
	real	eta	
	real	d0d2				!(a^2/r^2):earth-sun distance factor
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
      CALL JULDAY( yy, mm, dd, jday )
      CALL JULDAY( yy, 12, 31, jday0)
	eta=2*pai*jday/jday0
	d0d2=1.00011+0.034221*cos(eta)+0.00128*sin(eta)+
     :	 0.000719*cos(2*eta)+0.000077*sin(2*eta)	  
	CALL hsolar(hsun,lon0,lon,lat,yy,mm,dd,hh,mn,ss)

	R0=i00*d0d2*sin(hsun)

      RETURN
      END	!SUBROUTINE solarR0

c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   SUBROUTINE sunrise                         #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      SUBROUTINE sunrise(trise,lon0,lon,lat,yy,mm,dd)

c#############################################################################
c
c     PURPOSE:
c	Calculate sun rise time
c	
c#############################################################################

	implicit none

c	INPUT:
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      integer	yy,mm,dd   !calendar year:month:day		
c	OUTPUT:
      real	trise      !standard time hours (defined at lon0)
c	WORK:
	real pi
	parameter(	pi=3.1415926/180.0)
	real	delta				!solar declination(rad)
	real	t					!hour angle
	real	cost				!cosine of hour angle
	real	eta					!daily averaged hour error
      							!(min)		
      real   solardelta, solardte  
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
	delta=solardelta(yy,mm,dd)
	cost=-tan(delta)*tan(lat*pi)
	cost=max(-1.,min(1.,cost))
	t=ACOS(COSt)
	t=-t/pi
	eta=solardte(yy,mm,dd)						!daily averaged hour error
												!(min)		
	trise=max(0.,(t-(lon-lon0))/15-eta/60.+12)  	!(hour)

      RETURN
      END	!SUBROUTINE sunrise

c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   SUBROUTINE sunset                          #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      SUBROUTINE sunset(tset,lon0,lon,lat,yy,mm,dd)

c#############################################################################
c
c     PURPOSE:
c	Calculate sun set time
c	
c#############################################################################
c
	implicit none

c	INPUT:
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      integer	yy,mm,dd   !calendar year:month:day		
c	OUTPUT:
      real	tset            	!standard time hours (defined at lon0)
c	WORK:
	real pi
	parameter(	pi=3.1415926/180.0 )
	real	delta				!solar declination(rad)
	real	t					!hour angle
	real	eta					! daily averaged time eeror due to elliptic 
								!shape of earth rorbit(min)
	real	cost				!cosine of hour angle
c
      real   solardelta,solardte  
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
	delta=solardelta(yy,mm,dd)
	cost=-tan(delta)*tan(lat*pi)
	cost=max(-1.,min(1.,cost))
	t=ACOS(COSt)
	t=t/pi
	eta=solardte(yy,mm,dd)						!daily averaged hour error
												!(min)		
	tset=min(24.,(t-(lon-lon0))/15-eta/60.+12)

      RETURN
      END	!SUBROUTINE sunset


c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   SUBROUTINE hsolar                          #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      SUBROUTINE hsolar(hsun,lon0,lon,lat,yy,mm,dd,hh,mn,ss)

c#############################################################################
c
c     PURPOSE:
c	Calculate the height of the sun (rad)
c	
c#############################################################################
	implicit none

c	INPUT:
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      real    lat        ! latitdue of each site (deg.)
                         ! > 0 in North Hemesphere
                         ! < 0 in South Hemesphere
      integer	yy,mm,dd   !calendar year:month:day		
      integer	hh,mn,ss   !standard time hour:minute:second in lon0
c	OUTPUT:
	real	hsun	! The height of the sun (rad)
c	WORK:
	real pi
	parameter ( pi=3.141593/180.0)
	real	delta				!solar declination(rad)
	real	hrangle				!hour angle(rad)
	real	sinh				!
      real    solardelta,solarhour 
c           				
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
	delta=solardelta(yy,mm,dd)
	hrangle=solarhour(lon0,lon,yy,mm,dd,hh,mn,ss)
	sinh=sin(lat*pi)*sin(delta)+cos(lat*pi)*cos(delta)*cos(hrangle)
	if(sinh.le.0)then	
		hsun = 0			    ! rad
	else	
		hsun = asin(sinh)       ! rad
	end if

      RETURN
      END	!SUBROUTINE hsolar
c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   FUNCTION solardelta                        #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      FUNCTION solardelta(yy,mm,dd)

c#############################################################################
c
c     PURPOSE:
c	Calculate solar declination angle delta from calendar
c	
c#############################################################################
	
	implicit none

c	INPUT:
      integer	yy,mm,dd			! calendar year:month:day
c	OUTPUT:
	real	solardelta			! solar declination angle	(rad)
c	WORK:
	integer	jday,jday0
	real pi
	parameter (pi=3.1415926)
	real	w	
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      CALL JULDAY( yy, mm, dd, jday )
      CALL JULDAY( yy, 12, 31, jday0)

	w=2.*pi*jday/jday0

	solardelta=0.3622133-23.24763*COS(w+0.153231)-0.3368908
     :	   *COS(2.*w+0.2070988)-0.1852646*COS(3.*w+0.6201293)

      solardelta = solardelta * pi / 180

      RETURN
      END	!FUNCTION solardelta

c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   FUNCTION solardte                          #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      FUNCTION solardte(yy,mm,dd)

c#############################################################################
c
c     PURPOSE:
c	Calculate daily averaged time error due to elliptic shape of earth 
c	orbit (min)
c	
c#############################################################################

	implicit none

c	INPUT:
      integer	yy,mm,dd			! calendar year:month:day		
c	OUTPUT:
	real	solardte			! daily averaged time eeror due to elliptic 
								!shape of earth rorbit(min)
c	WORK:
	integer	jday,jday0			! Julian day
	real pi
	parameter (pi=3.1415926)
	real	w	
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      CALL JULDAY( yy, mm, dd, jday )
      CALL JULDAY( yy, 12, 31, jday0)

	w=2.*pi*jday/jday0

	solardte =60.*(-0.0002786409+0.1227715*COS(w+1.498311)
     :		  -0.1654575*COS(2.*w-1.261546)-0.00535383	 
     :	      *COS(3.*w-1.1571))				!daily averaged time 
												!error(min) 
      RETURN
      END	!FUNCTION solardte

c     ########################################################################
c     ########################################################################
c     #####                                                              #####
c     #####                                                              #####
c     #####                   FUNCTION solarhour                         #####
c     #####                                                              #####
c     #####                                                              #####
c     ########################################################################
c     ########################################################################

      FUNCTION solarhour(lon0,lon,yy,mm,dd,hh,mn,ss)

c#############################################################################
c
c     PURPOSE:
c	Calculate solar hour angle from calendar and standard time
c	
c#############################################################################
	implicit none

c	INPUT:
      real    lon0       ! the longitude where the standard time is defined. 
                         ! If input time is UTC, lon0 = 0
                         ! If input time is BST, lon0 = 120
                         ! If input time is JST, lon0 = 135
      real    lon        ! longitude of each site (deg.)
                         ! > 0 in East Hemesphere
                         ! < 0 in West Hemesphere
      integer	yy,mm,dd			! calendar year:month:day		
      integer	hh,mn,ss			! standard time hour:minute:second in long0
c	OUTPUT:
	real	solarhour			! hour angle (rad)
c	WORK:
	real  eta,ts,t	
      real  solardte 
	real pi
	parameter (pi=3.1415926)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!	Begining of executable code......
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
	eta=solardte(yy,mm,dd)						!daily averaged hour
												!error(min) 
      ts=FLOAT(hh)+FLOAT(mn)/60.+FLOAT(ss)/3600 !convert standard time
	t=15.*(ts-12.+eta/60.)+lon-lon0 			!convert hour and longitude 
												!difference to hour angle
	solarhour=t	* pi /180					    !(rad)	
      RETURN
      END	!FUNCTION solarhour

c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE JULDAY                     ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     Center for Analysis and Prediction of Storms     ######
c     ######                University of Oklahoma                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE JULDAY( year, month, day, jday )
c
c#######################################################################
c
c     PURPOSE:
c
c     Compute Julian day from year, month, and day
c
c     Start from 1 (Jan. 1) to 365, or 366 for leap year (Dec. 31)
c
c     The rule is that a year will be a leap year if
c
c       the year can be divided by 400, or
c       the year can by divided by 4, but not by 100
c
c     Form this rule year 1972, 1984, 1996, and 2000 are leap years,
c     but 1700, 1800 and 1900 are not.
c
c#######################################################################
c
c     AUTHOR: Yuhe Liu
c     07/29/93
c
c     MODIFICATIONS:
c
c     05/06/1998 (Yuhe Liu)
c     Corrected the leap year calculation.
c
c#######################################################################
c
c     INPUT:
c
c       year       Reference calendar year
c       month      Reference monthe of the year
c       day        Reference day of the month
c
c    OUTPUT:
c
c       jday       Julian day, start from 1 -- Jan. 1 to 365 -- Dec. 31
c
c#######################################################################
c
      implicit none
 
      integer year, month, day, jday
      integer lpyear, lp

      integer mndys(12)     ! Day numbers for each month
      DATA mndys/0,31,59,90,120,151,181,212,243,273,304,334/
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF ( mod(year,400).eq.0 .or. 
     :     (mod(year,4).eq.0 .and. mod(year,100).ne.0 ) ) THEN
        lpyear = 1
      ELSE
        lpyear = 0
      ENDIF

      lp = 0
      IF ( month.gt.2 ) lp = lpyear

      jday = mndys(month) + day + lp
 
      RETURN
      END