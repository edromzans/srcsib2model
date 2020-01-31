INPUT FILEs
DATA1 (SUBROUTINE veginc)
  SIB 0-D PARAMETERS  FOR TEST RUNS OF SIB-2C ( VEGINC )
  VEGN. TYPE------------------------------------------------(I,J)
   ivtype=99   !ivtype: vegetation type
     
  VEGN. TYPE-DEPENDENT STATIC PARAMETERS--------------------(IVTYPE)
   z2=35.0     !z2: canopy top height
   z1=1.0      !z1: canopy base height
   vcover=0.98 !vcover: vegetation cover fraction
   chil=0.1    !chil: leaf angle distribution factor

   rootd=0.5   !rootd: rooting depth
   phc=-200.0  !phc: 1/2 critical leaf water potential limit
   
   tran(1,1)=0.0500  !leaf transmittance
   tran(2,1)=0.2500  !
   tran(1,2)=0.0010  !
   tran(2,2)=0.0010  !
   
   ref (1,1)=0.1000  !leaf reflectance
   ref (2,1)=0.4500  !
   ref (1,2)=0.1600  !
   ref (2,2)=0.3900  !   

   effcon=0.08    !effcon: quantum efficiency
   gradm=9.00     !gradm: conductance-photosynthesis slope parameter
   binter=0.01    !binter: conductance-photosynthesis intercept 
   respcp=0.015   !respcp: respiration fraction of vmax 
   atheta=0.95    !atheta: wc, we coupling parameter
   btheta=0.95    !btheta: wc & we, ws coupling parameter
  
   trda=1.30    !trda: temperature coefficient in gs-a model
   trdm=328.16  !trdm: temperature coefficient in gs-a model
   trop=298.16  !trop: temperature coefficient in gs-a model   
   slti=0.2     !slti: slope of low temperature inhibition function 
   hlti=288.16  !hlti: 1/2 point of low temperature inhibition function
   shti=0.3     !shti: slope of high temperature inhibition function  
   hhti=313.16  !hhti: 1/2 point of high temperature inhibition function     
         
  SOIL TYPE, SOIL DEPTH, SOIL REFLECTANCES(VIS,NIR)---------(I,J)
   istype=99      !istype: soil type
   sodep=2.0      !sodep: total depth of 3 soil moisture layers
   soref(1)=0.10  !soref: soil reflectance
   soref(2)=0.20  !soref: soil reflectance
   
  SOIL TYPE-DEPENDENT STATIC PARAMETERS---------------------(ISTYPE)
   bee=7.797      !bee: soil wetness exponent
   phsat=-0.2     !phsat: soil tension at saturation 
   satco=3.5E-06  !satco: hydraulic conductivity at saturation 
   poros=0.458    !poros: soil porosity  
   slope=0.08     !slope: cosine of mean slope  
              
  VEGN. : PHENOLOGICALLY-VARYING APARC ( FROM NDVI )--------(I,J,T)
  fparc= 0.999
  0.999  X 0.601 0.29263
  
  DERIVED OR SECONDARY PARAMETERS---------------------------(I,J,T)
   vmax0=0.00006   !vmax0: rubisco velocity of sun-leaf 
   gmudmu=1.0      !gmudmu: time-mean leaf projection ( g(mu)/ mu )
   green=0.7979    !green: green leaf fraction
   zlt=-99.9       !zlt: leaf area index  
      
   z0d=2.02        !z0d: roughness length
   dd=28.81        !dd: zero plane displacement 
   cc1=5.59        !cc1: rb coefficient (c1)
   cc2=1177.14     !cc2: rd coefficient (c2)  
            
  PARAMETERS REQUIRED FOR RASITE OPERATION ONLY
   corb1=0.111     !corb1: - 
   corb2=19.112    !corb2: -
   ha=24.81        !ha: canopy source height for heat
   g1=1.449        !g1: ratio of km(actual) to km(log-linear) at z = z2        
   g2=0.801        !g2: ratio of ra(actual) to ra(log-linear) for momentum between: z = z2 and z = zx, where zx = min(zl,zwind)
   g3=0.801        !g3: ratio of ra(actual) to ra(log-linear) for heat between: z = z2 and z = zx, where zx = min(zl,zmet)  
   ztz0=11.785     !ztz0: parameter to determine depth of transition layer above canopy, zl. zl = z2 + ztz0 * z0 
   zwind=45.00     !zwind: reference height for wind measurement    
   zmet=45.00      !zmet: reference height for temperature, humidity measurement 
c      corb1  : non-neutral correction for calculation of aerodynamic           
c               resistance between ha and z2. when multiplied by                
c               h*rbb/(tm*u2*u2*(z2-ha)) gives bulk estimate of local 
c               richardson number.        
c               rbb = ra for heat between ha and z2. 
c               Rib = corb1*h*rbb/(tm*u2*u2*(z2-ha))                                           
c               corb1 = 9*g/( rhoair*cpair* (du/dz)**2 )                       
c      corb2  : neutral value of rbb*u2 ( squared ), equivalent to              
c               rdc**2 for upper canopy      
                          
  SITE LOCATION, NUMBER OF ITERATIONS, INITIAL CONDITIONS (NON-VEGINC)
   dtt=3600.     !dtt: - 
   itrunk=20     !itrunk: -
   ilw=1         !ilw:   
c			if ilw=1
c     downward long-wave assumed to be provided as radn(3,2) 
c     no calculation is needed
c			if ilw=2
c     downward long-wave from brunt's equation, Monteith(1973), p37.    
c			if ilw=3
c     downward long-wave flux calculated as residual from measured              
c     net radiation and outgoing longwave radiation.                        
c     calculation of absorbed fractions of radiation ( expendable )       
  
  zlat, zlong, time, month, day, year, niter    
  -3.0  -59.0  23.5   9   242.   1983.  18264
  
  tc, tg, td !tc: canopy temperature (K); tg: ground surface temperature (K)
  300.0    300.0    298.0  300.0  300.0    0.   5.5
  
  www(1), www(2), www(3)
  0.6259  0.6259   0.6259  
  
  0.0     0.0      0.0

DATA2 (Forcing data)
nymd, iqcaws, swdown, rnetm, zlwd, em,  tm,   um,   tprec, iqchyd, mevap, msensh, mustar  
83090300 2    0.      -36.   423.1 29.8 300.4 1.60  0.00   0       4      -6      6
83090301 2    0.      -31.   421.5 28.9 300.1 1.70  0.00   0       5      -5      6

 nymd:    date and time (yymmddhh)
 iqcaws:  data quality check indices
 swdown:  shortwave downward radiation 
 rnetm:   measured net radiation(W /m2 )
 zlwd:    longwave  downward radiation 
 em:      vapor pressure
 tm:      temperature
 um:      wind speed
 tprec:   precipitation
 iqchyd:  data quality check indices
 mevap:   latent heat flux / measured data for comparison
 msensh:  sensible heat flux / measured
 mustar:  friction velocity  / measured

OUTPUT FILEs
moisture.txt
nymd, www(1), www(2), www(3), ppl
 nymd:   date and time (yymmddhh)
 www(1): ground wetness of surface layer (--)
 www(2): ground wetness of the second layer (--) 
 www(3): ground wetness of the third layer (--)
 ppl:    large scale precipitation (mm)
 
waterbalance.txt
nymd,       roff,        trant,    canil,    evapg,    etmass,   ppl
 nymd:   date and time (yymmddhh)
 roff:   runoff (mm)
 trant:  =ect/hlat; ect:canopy transpiration (j m-2) 
 canil:  =eci/hlat; eci:canopy interception loss (j m-2)
 evapg:  =(egs+egi)/hlat; 
           egs:ground evaporation (j m-2)
           egi:ground interception loss (j m-2)
 etmass: evapotranspiration (mm)
 ppl:    large scale precipitation (mm)
 
temperature.txt
nymd, tc, tg, td, tm, tgeff, capac(1), capac(2), snoww(1), snoww(2)
 nymd:   	date and time (yymmddhh)
 tc:   		canopy temperature (K)
 tg:  		ground surface temperature (K) 
 td:  		deep soil temperature (K)
 tm:  		air temperature at zmet 
 tgeff:		effective surface radiative temperature (k)
 capac(1):canopy liquid interception store (m) 
 capac(2):ground liquid interception store (m)
 snoww(1):canopy snow interception store (m) 
 snoww(2):ground snow interception store (m) 
 
energy.txt
nymd, radn(3,2), zlwup, radswd, radswa, radtot, elat, hflux, gcstor
 nymd:			date and time (yymmddhh)
 radn(3,2):	downward longwave
 zlwup:			total upward long wave 
 radswd:		=radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2)
 							radn(1,1): beam visible
 							radn(1,2): diffuse visble
 							radn(2,1): direct near infrared
 							radn(2,2): diffuse near infrared
 radswa:		=(1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2) + (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2) 
 							salb: surface albedo
 radtot:		=radt(1) + radt(2)
 							radt(1): net radiation absorbed by vegetaion
 							radt(2): net radiation absorbed by ground
 elat:			=etmass / dtt * hlat
 							 etmass: evapotranspiration (mm)
 hflux:			(fss) sensible heat flux (w m-2)
 gcstor:		=chf + shf
 							chf: canopy heat storage flux (j m-2)
 							shf: soil heat storage flux (j m-2)
 							 
physiology.txt
nymd, rst,rstfac(1),rstfac(2),rstfac(3),rstfac(4),gsh2o,assimn
 nymd:			date and time (yymmddhh)
 rst:				1/rst = canopy conductance
 rstfac(1-4)canopy resistance stress factors
 rstfac(1):	f(h-s): equation (17,18), SE-92A 
 rstfac(2):	f(soil): equation (12 mod), SE-89 
 rstfac(3):	f(temp): equation (5b)   , CO-92
 rstfac(4):	f(h-s)*f(soil)*f(temp)
 gsh2o:			canopy conductance (mol m-2 s-1)
 assimn:		canopy net assimilation rate    