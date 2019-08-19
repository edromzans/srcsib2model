c=======================================================================
c
c
      program sib2
c
c=======================================================================
c
c...  SiB driver offline version, forced by meteorological data
c
c	core of routines for sib2 model
c
c	**** to use SiB2 in stand-by mode ****
c
c=======================================================================
c
c
c     last revision:          dec. 15, 1993                              
c
c     P. Sellers, J. Collatz, L. Bounoua                                  
c
c
c     last update:      04 fev 97   Humberto Rocha
c	soil multi-layer model
c	
c=======================================================================   
c                                                                         
c     subroutines called from this segment  :    veginc                  
c                                                cntrol                   
c                                                const2                         
c                                                driver                         
c                                                balan                          
c                                                inter2                         
c                                                rada2                          
c                                                begtem                         
c                                                endtem                         
c                                                updat2                         
c                                                balan                          
c                                                                              
c-----------------------------------------------------------------------        
c     ipbl = 0 : dtc, dtg only     ; as per SE-86                               
c     ipbl = 1 : dtc, dtg, dth, dqm; as per Randall-Sellers, SA-89b             
c                                                                               
c     isnow= 0 : conventional run using amazon forcings.                        
c     isnow= 1 : snow shock test, warming to normal over 5 days.               
c-----------------------------------------------------------------------        

c...	variables only important to the optimization module
      integer itero, maxit
c...
      include 'pardif.h'                                                      
      include 'comsibc.h'                                                     
      data ichi, icho, iu /1,6,8/                  
      data itmp1/88/,itmp2/78/,itmp3/79/,itmp4/80/,itmp5/81/,
     &     itero/0/, ipbl/0/, isnow/0/   
                                         

c... output files opening

      open(98,file='sib2diag.dat',status='unknown')
      open(itmp1,file='sib2dt.dat',status='unknown')	
      open(itmp2,file='sib2dt_SM.dat',status='unknown')		! umidade do solo
      
cH      open(itmp3,file='sib2dt_Qu.dat',status='unknown')		! vazão Q upward
cH      open(itmp4,file='sib2dt_Qd.dat',status='unknown')		! vazão Q downward            
cH      open(itmp5,file='sib2dt_Qh.dat',status='unknown')		! vazão Q Hortoniana

      open(ichi, file='data1', status='old')         ! parameters file                                         

c      write(98,'(20a11)')
c     & 'xs','P','D','Pinf','Dc','Dd','Ri','R','Pi','q0'

      call veginc(ichi)       
      
      call cntrol(ichi,icho,maxit,nylast,nyfirst)
      
C... simulation start: at nyfirst (data1)                              
C... simulation end  : at maxit or nylast (most limiting out of two)
             

      do 1000 iter = 1, maxit    

c      write( *,'(a20,2i10)')'iter nymd:',iter,nymd

      call const2                                              

c       write(98,'(A20)')'  to call driver'
      call driver (iu, icho, isnow, ichi, itero, nyfirst)

      if (mod(iter,int(86400./dtt)).eq.0) then	 ! display screen cada dia
      write( *,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd
      write(98,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd
      endif

c      if (mod(iter,365*int(86400./dtt)).eq.0) then  !display screen cada ano
c      write( *,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd
c      write(98,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd
c      endif

c				!display screen cada passo de tempo
c      write(98,'(a20,i5,i12)')'  iter  nymd:',iter,nymd 
c      write(*,'(a20,i5,i12)')'  iter nymd:',iter,nymd

c       write(98,'(A20)')'   passed driver'

      if (nymd.gt.nylast) goto 1001


c	write(98,'(A20)')'  to call optima'
c	if (lopt) call optima (xx, mo, no, ff, iqc, itero)
c	write(98,'(A20)')'  passed optima'

c       write(98,'(A20)')'  to call balan'
         call balan( 1, totwb )                                          
c       write(98,'(A20)')'   passed balan'
c      print*,maxit
      
c       write(98,'(A20)')'  to call inter2'
        call inter2                                                               
c       write(98,'(A20)')'   passed inter2'

c       write(98,'(A20)')'  to call rada2'
         call rada2                                                                
c       write(98,'(A20)')'   passed rada2'

c       write(98,'(A20)')'  to call begtem'
         call begtem                                                               
c       write(98,'(A20)')'   passed begtem'

c       write(98,'(A20)')'  to call endtem'
         call endtem (ipbl)                                                        
c       write(98,'(A20)')'   passed endtem'

c       write(98,'(A20)')'  to call updat2'
         call updat2                                                               
c       write(98,'(A20)')'   passed updat2'

c       write(98,'(A20)')'  to call balan'
         call balan( 2, totwb )      
c       write(98,'(A20)')'   passed balan'

c       write(98,'(A20)')'  to call outer'
      call outer(itero)                 
c       write(98,'(A20)')'   passed outer'
                           
      if (nymd.eq.nylast) goto 1001

       
 1000 continue                                   

c----------------------------------------------------------------------

 1001 continue

      close(ichi)
      close(iu)
      close(itmp1)
      close(itmp2)
cH      close(itmp3)
cH      close(itmp4)
cH      close(itmp5)      
      close(98) 
c
      stop ' SiB2 HAS DONE. '                                                                      
      end                                                                       
c
c=======================================================================        
c
c   SUBROUTINES
c                                                                                
c=======================================================================        

      subroutine outer (itero)               

c----------------------------------------------------------------------
c writes out selected variables (unformatted) select in data1 namelist
c----------------------------------------------------------------------

      include    'comsibc.h'       
      dimension  rvar(170)

c...
      trant  = ect/hlat                                                         
      canil  = eci/hlat                                                         
      evapg  = (egs+egi)/hlat                                                   
      radswd = radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2)                    
      radswa = (1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2)              
     &       + (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2)              
      calbe = 1. - (radswa/radswd)
      cgsto = shf - gflux     

c..   fco2 = co2 flux in umol m-2 s-1
      fco2    = (respg - assimn) * 1.e06                          !Rs calculado
      
      if (xmrsco2.ne.-9999.) fco2 = xmrsco2 - assimn*1.e06        !Rs observado
      
      radtot = radt(1) + radt(2)                                                
      elat   = etmass / dtt * hlat                                              
      gcstor = chf + shf                                                        
c                                                                               
      tgs  = amin1(tf,tg)*areas + tg*(1.-areas)                                 
      tc4  = tc  * tc  * tc  * tc                                               
      tg4  = tgs * tgs * tgs * tgs                                              
      zlwf =  tc4 * stefan * vcover * ( 1. - thermk )                           
     &     + (1. - vcover * ( 1. - thermk ) ) * stefan * tg4                    
      tgeff= sqrt ( sqrt ( zlwf/stefan ) )                                      


c......................................................................

c=======================================================================
c... list of all variables to write out
c=======================================================================
c... water balance
c1   elat            latent heat flux (Wm-2)         
c2   ect/dtt         canopy transpiration (Wm-2)             
c3   eci/dtt         canopy interception loss (Wm-2)         
c4   egs/dtt         soil evaporation (Wm-2)         
c5   egi/dtt         ground cover interception loss (Wm-2)           
c6   q0              top soil infiltration flux (ms-1)
c7   qng             deep drainage flux (ms-1)
c8   croff           surface runoff (m) 
c9   roff            total runoff (m)
c...	sensible heat balance
c10  hflux           sensible heat flux (Wm-2)               
c11  hc/dtt          canopy sensible heat flux (Wm-2)                
c12  hg/dtt          ground cover sensible heat flux (Wm-2)          
c13  chf             canopy storage energy (Wm-2)
c14  cgsto           ground storage energy (Wm-2)
c15  gflux           soil heat flux (Wm-2)
c16  heaten/dtt      snowmelt heat loss (Wm-2)           
c...    radiation balance ................................................
c17  radtot          calculated net radiation (Wm-2)
c18  zlwf            long wave upward radiation (Wm-2)
c19  calbe           surface albedo (dimensionless)
c20  tgeff           radiative surface temperature (K)
c21  td              deep soil temperature (K)
c22  radt(1)         canopy net radiation (Wm-2)
c23  radt(2)         ground cover net radiation (Wm-2)
c24  radn(1,1)       direct par incoming radiation (Wm-2)
c25  radn(1,2)       difuse par incoming radiation (Wm-2)
c26  radn(2,1)       direct nir incoming radiation (Wm-2)
c27  radn(2,2)       direct nir incoming radiation (Wm-2)
c28  radn(3,2)       tir incoming radiation (Wm-2)
c29  salb(1,1)       direct par albedo
c30  salb(1,2)       difuse par albedo
c31  salb(2,1)       direct nir albedo
c32  salb(2,2)       direct nir albedo
c...  carbon balance
c33  assimn          net assimilation (mimol m-2 s-1) 
c34  fco2            net ecosystem exchange (mimol m-2 s-1) 
c35  respg           soil respiration (mimol m-2 s-1) 
c36  respc           canopy (plant) respiration (mimol m-2 s-1) 
c37  pco2a           co2 atmospheric partial pressure (Pa)
c... conductance calculation variables
c38  rst             canopy (leaf) conductance (m s-1)
c39  gsh2o           canopy (leaf) conductance (molm-2s-1)
c40  rstfac(1)       conductance stress factor vpd
c41  rstfac(2)       conductance stress factor soil moisture
c42  rstfac(3)       conductance stress factor temperature
c43  rstfac(4)       total conductance stress factor 
c...  other calculated variables (FILL IT IF NECESSARY)
c44                  blank
c45                  blank
c46                  blank
c47                  blank
c48                  blank
c49                  blank
c50                  blank
c...    observed (forcing or not) variables
c51  swdown          downward shortwave radiation (Wm-2)
c52  rnetm           net radiation (Wm-2)
c53  em              water vapour pressure (HPa)
c54  tm              surface air temperature (K)
c55  um              horizontal wind speed (ms-1)
c56  udm             surface wind direction (degree)
c57  ppl+ppc         total precipitation (mm)
c58  pco2m           atmospheric co2 concentration (Pa)
c59  xmevap          observed latent heat flux (Wm-2)
c60 xmsensh          observed sensible heat flux (Wm-2)
c61 xmgheat           observed soil heat flux (Wm-2)
c62 xmco2           observed co2 flux (Wm-2)
c63 xalbedo          observed surface albedo
c...  other observed variables (FILL IT IF NECESSARY)
c64                  blank
c65                  blank
c66 ustar            calculated friction velocity (ms-1)
c67                  blank
c68                  blank
c69                  blank
c70                  blank
c.. soil variables (up to nlayer = 50 MAXIMUM)
c71..120     qqq(i) interlayer i to i+1 water flux (m s-1) (+ downward)
c121..170    www(i) soil wetness at layer i

c------------------------------------------------------------------
c	variables atribution
c------------------------------------------------------------------

 700	continue

      rvar(1) = elat
      rvar(2) = ect / hlat	! mm
      rvar(3) = eci / hlat	! mm
      rvar(4) = egs / hlat	! mm
      rvar(5) = egi / hlat 	! mm
      rvar(6) = q0  * 1000. * dtt   ! mm
      rvar(7) = qng * 1000. * dtt   ! mm		
      rvar(8) = croff * 1000.       ! mm
      rvar(9) = roff * 1000.        ! mm
      rvar(10) = hflux
      rvar(11) = hc
      rvar(12) = hg
      rvar(13) = chf
      rvar(14) = cgsto
      rvar(15) = gflux
      rvar(16) = heaten
      rvar(17) = radtot
      rvar(18) = zlwf
      rvar(19) = calbe
      rvar(20) = tgeff
      rvar(21) = td -273.15
      rvar(22) = radt(1)
      rvar(23) = radt(2)
      rvar(24) = radn(1,1)
      rvar(25) = radn(1,2)
      rvar(26) = radn(2,1)
      rvar(27) = radn(2,2)
      rvar(28) = radn(3,2)
      rvar(29) = salb(1,1)
      rvar(30) = salb(1,2)
      rvar(31) = salb(2,1)
      rvar(32) = salb(2,2)
      rvar(33) = 1.e+6 * assimn 
      rvar(34) = fco2
      rvar(35) = respg * 1.e+6
      rvar(36) = respc * 1.e+6
      rvar(37) = pco2a
      rvar(38) = rst
      rvar(39) = gsh2o * 18.    ! stomat conductance mm /s
      rvar(40) = rstfac(1)
      rvar(41) = rstfac(2)
      rvar(42) = rstfac(3)
      rvar(43) = rstfac(4)
c
	rvar(44) = rvar(3) + rvar(5)		! total interception loss
	rvar(45) = rvar(13) + rvar(14)		! column energy storage
c	rvar(46) = rvar(1)/hlat*dtt		! Evptrans (mm)
	rvar(46) = rvar(2)+rvar(3)+rvar(4)+rvar(5)	! Evptrans (mm)	
c	rvar(47) = blank
c	rvar(48) = blank
c	rvar(49) = blank
c	rvar(50) = blank
c
      rvar(51) = swdown
      rvar(52) = rnetm                 ! Rn observada (for Ld calculation)
      rvar(53) = em
      rvar(54) = tm -273.15		! Celsius
      rvar(55) = um
      rvar(56) = udm
      rvar(57) = ppl+ppc
      rvar(58) = pco2m
      rvar(59) = xmevap
      rvar(60) = xmsensh
      rvar(61) = xmgheat
      rvar(62) = xmfco2
      rvar(63) = xalbedo
c
      rvar(64) = xmrsco2
      rvar(65) = rvar(35) + rvar(36)       ! Rsc calculado
      rvar(66) = ustar
      rvar(67) = xmustar
c	rvar(69) = blank
c	rvar(70) = blank

      do 40 in=1,nlayer
      rvar( 70+in) = qqq(in)
 40	rvar(120+in) = www(in) 

c--------------------------------------------------------------------
c	write out variables
c--------------------------------------------------------------------

      if (itero.eq.1) then                            ! write labels
      
      write(itmp1,'(a8,100(1x,a9))')'NYMD',
     & 'Tm', 'em','um',   
     & 'Ki','Rn_m','alb','Ldwn','Lupw', 'Rn_C','H_C',
     & 'LE_C','G_C','J_C','Fc_C','Rsc_C','An_C','u*_C',       
     & 'Td','W1_C','W2_C','W3_C','gcond',
     & 'Evpt','Trans','Esoil','Einterc','Prec','Rss','Rs','Runoff',
     & 'PARidir','PARidif','albPARdir','albPARdif'
     
      write(itmp2,'(a8,9(7x,a2,i1),90(7x,a1,i2))')
     & 'NYMD',('W_',i,i=1,9),('W',i,i=10,nlayer)
      
      endif
           
           
      write(itmp1,'(i8.8,50(1x,f9.3))') nymd,
      
     & rvar(54), rvar(53), rvar(55), 		    	! tm, em, um 
     & rvar(51), rvar(52), rvar(19),rvar(28),rvar(18),  ! Ki,Rn, alb,Ldwn,Lupw
     & rvar(17), rvar(10), rvar(01),rvar(15),rvar(45), 	! Rn,H,LE,G,J
     & rvar(34), rvar(65), rvar(33),rvar(66),		! Fc,Rsc,An ,u*
     & rvar(21), rvar(121),rvar(122),rvar(123),         ! Td, W1,W2,W3
     & rvar(39), rvar(46),rvar(2),rvar(4),rvar(44),    	! gc, Evptran, Transp,Esoil,Einterc
     & rvar(57), rvar(7), rvar(8), rvar(9),             ! precip, Qng  , Croff, Runoff
     & rvar(24), rvar(25), rvar(29), rvar(30)           ! PARidir, PARidif, albPARdir, albPARdif
             
      write(itmp2,'(i8.8,50(1x,f9.3))') nymd,(www(i),i=1,nlayer)

c      write(*,'(i8.8,11(1x,f6.3))') nymd,(www(i),i=1,nlayer)
 
      return                                                                    
      end                                                     
                       
            
c=======================================================================        
c                                                                               
      subroutine driver (iu, icho, isnow, ichi, itero, nyfirst)
c
c=======================================================================        
c                                                                               
c     forcing meteo data:
c     data required to run sib :                                                
c
c     nymd     : date and time (yymmddhh)                                       
c     swdown   : shortwave downward radiation.                                  
c     zlwd     : longwave  downward radiation.                                  
c     em,tm,um : vapor pressure,temperature and wind speed                      
c                at measurement height ( or lowest model level.)                
c     tprec    : precipitation.                                                 
c
c     subroutines called : radc2
c                                                                               
      include 'comsibc.h'                                                       
      character*30 cfinp
      real         vchec(6), feno(12)
      integer      diames(12)     
      data diames/31,28,31,30,31,30,31,31,30,31,30,31/
       
      e(x) = exp( 21.18123 - 5418. / x ) / .622
c-----------------------------------------------------------------------        
c...    open monthly input forcing met data / read green phenology
c-----------------------------------------------------------------------        
  
      if (itero.eq.0) then   
      
      read(ichi,*)
      read(ichi,*) (greex(mm),mm=1,12)
      read(ichi,*) (zltex(mm),mm=1,12)
      read(ichi,*) ( vmex(mm),mm=1,12)

      write(98,*) 'Greex' ,(greex(mm),mm=1,12)
      write(98,*) 'Zltes' ,(zltex(mm),mm=1,12)
c      write(*,*) 'passei..' ,(zltex(mm),mm=1,12)

      read(ichi,*)
      read(ichi,*) cfinp                
      write( *,'(a25,a40)')' # opening file counting ',cfinp
      write(98,'(a25,a40)')' # opening file counting ',cfinp
      open(iu, file= cfinp, status='old')
      read(iu,*)
      
      endif

 851  CONTINUE      
C...
c---------------------------------------------------------------------
c       read in meteorological forcing data
c---------------------------------------------------------------------

      xmevap  = -9999.
      xmsensh = -9999.
      xmgheat = -9999.
      xmfco2  = -9999.
      xmrsco2 = -9999.
      xco2m   = -9999.
      xmustar = -9999.
      xalbedo = -9999.
      zlwd    = -9999.                                 
                                       
 100  continue

*-----------------------------------------------------------------------
*     Defining a default format to read data2
*     and check iwl flag to read
*     Evandro M Anselmo 16/08/2019       
 201  continue
      if (ilw.ne.3) then
         read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec
      else
         read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec, rnetm
      end if
*-----------------------------------------------------------------------

      if (nymd.ge.nyfirst) then
      itero = itero + 1           ! incremento iteracoes apos nyfirst
      else
      goto 201 
      endif

c...  checa qualidade forcantes
      iqc  =  1
      vchec(1) = swdown
      vchec(2) = rnetm
      vchec(3) = em
      vchec(4) = tm
      vchec(5) = um
      vchec(6) = tprec
c      if (tprec.eq.-9999.0) tprec = 0.

      do 55 k=1,6
 55   if (vchec(k).eq.-9999.) iqc = 0
 
      if (iqc.eq.0) then
      write(* ,*) ' Forcante com erro -9999: iqc = 0 at', nymd
      write(98,*) ' Forcante com erro -9999: iqc = 0 at', nymd
      stop
      endif
 


c... vegetation phenology	and co2 concentration diurnal cycle
      ihour = mod(nymd,100)
      imont = int(mod(nymd,1000000)/10000)
      idddd = int(mod(nymd,10000)/100)


c... ico2m=  0:  pco2m constant = 34 (set in const2 every time step)
c            1:  pco2m read in as xco2m (have to be a forcing data)
c            2:  pco2m com ciclo diurno forcado

      if (ico2m.eq.0) co2amp = 0. 

      if (ico2m.eq.2) co2amp = 100.       ! amplitude (ppm)

      pco2m = facco2*(340 + co2amp/2.*cos(2.*pie * real(ihour - 8)/24.))
      
      if(ico2m.eq.1.and.xco2m.ne.-9999.) pco2m = xco2m     !xco2m in ppm  

      pco2m = pco2m * 0.1      !0.1 ppm


c	write(*,'(2(a10,f12.7))') 'pco2m=',pco2m,'real(ihour)',real(ihour)

c..   interpola mes a mes (N - green fraction, L - LAI)


c     write(*,*) 'antes N ' ,(greex(mm),mm=1,12)
c     write(*,*) 'antes L' ,KK,(zltex(mm),mm=1,12)
      
      do 360 kk = 1,3

      do 350 jj = 1,12
      if (kk.eq.1) feno(jj) = greex (jj)
      if (kk.eq.2) feno(jj) = zltex (jj)
 350  if (kk.eq.3) feno(jj) =  vmex (jj) * 1.e-06
      
      gprev = feno(imont-1)
      gpres = feno(imont)
      gpost = feno(imont+1)
      if (imont.eq.01) gprev = feno(12)
      if (imont.eq.12) gpost = feno(01)

      if (idddd.gt.15) then	    
      gfrac = 0.5* (real(idddd-15)/real(diames(imont) - idddd + 15))
      fenot = (1.-gfrac) * gpres + gfrac * gpost
      else
      gfrac = 0.5* (real(15 - idddd)/ real(idddd + 15))
      fenot = (1.-gfrac) * gpres + gfrac * gprev
      endif

      if (kk.eq.1) green = fenot
      if (kk.eq.2) zlt   = fenot
 360  if (kk.eq.3) vmax0 = fenot
      
 361  continue
 

c ---------------------------------------------------------------------
c                                                                               
c     isnow = 0 : conventional run with received met data.                      
c                                                                               
c     isnow = 1 : shock test with initial snow dump and freezing                
c                 temperatures at beginning of run warming to                   
c                 normal over  5-day period.                                    
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      if( isnow .eq. 0) go to 200                                               
      if ( iter .gt. 1) go to 210                                               
      tc = 270.                                                                 
      tg = 270.                                                                 
      snoww(2) = 0.1                                                            
 210  cold = amax1 ( 0., (120. - (1.*iter)) / 120. )
      rhair = em/e(tm)                                                          
      tm = tm * ( 1. - cold ) + (tm - 30.) * cold                               
      em = e(tm)*rhair                                                          
      if(em .lt. 0.)em = 0.1                                                    
 200  continue                                                                   
c                                                                               
      um = amax1(um,0.25)                                                       
ch    ustarm = mustar/100.                                                      
      swdown = amax1(swdown,0.1)                                                
      ihour = mod(nymd,100)                                                     
      ptot = ptot + tprec                                                       
      ppl = tprec                                                               
      ppc = tprec-ppl                                                           

      call radc2                                                                

      cloud = (1160.*sunang - swdown) / (963. * sunang)                         
      cloud = amax1(cloud,0.)                                                   
      cloud = amin1(cloud,1.)                                                   
      cloud = amax1(0.58,cloud)                                                 
c                                                                               
      difrat = 0.0604 / ( sunang-0.0223 ) + 0.0683                              
      if ( difrat .lt. 0. ) difrat = 0.                                         
      if ( difrat .gt. 1. ) difrat = 1.                                         
c                                                                               
      difrat = difrat + ( 1. - difrat ) * cloud                                 
      vnrat = ( 580. - cloud*464. ) / ( ( 580. - cloud*499. )                   
     &        + ( 580. - cloud*464. ) )                                         
c                                                                               
      radn(1,1) = (1.-difrat)*vnrat*swdown                                      
      radn(1,2) = difrat*vnrat*swdown                                           
      radn(2,1) = (1.-difrat)*(1.-vnrat)*swdown                                 
      radn(2,2) = difrat*(1.-vnrat)*swdown                                      
      if (ilw.eq.3) radn(3,2) = 0.0                                                         
      if (ilw.eq.1) radn(3,2) = zlwd                                                         

      if (ilw.eq.1.and.zlwd.le.100.)
     & stop 'warning: checar ilw: incompativel'

      return

 1000 write(icho, 90)iu, nymd                                            
 90   format(5x,'eof encountered for unit= ',i2,' eof date = ',i8)        
      stop

      end

c=======================================================================        
c                                                                               
      subroutine balan ( iplace, totwb )                                   
c                                                                               
c=======================================================================        
c                                                                               
c     energy and water balance check.                                           
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                       
c     
      if( iplace .eq. 2 ) go to 100                                             
c                                                                               
      etmass = 0.                                                               
      roff   = 0.                                                               
c 
c	write(98,*)'====================================================='
      totwb = 0.
      do 20 i=1,nlayer                                     
c	write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i)
 20   totwb = totwb + ( www(i) * poros(i) * zdepth(i) )
      totwb = totwb + (capac(1) + capac(2) + snoww(1) + snoww(2))

c      totwb1 = dble(totwb)

c      write(98,'(1x,a10,2a13,/,1x,f12.5,1x,f12.10,1x,f12.5)')
c     .    ' S ',' M ',' To1',
c     .   totwb1*1000.,
c     .   (capac(1)+capac(2)+snoww(1)+snoww(2))*1000., totwb*1000.

c                                                                               
      goto 200                                                                 
c                                                                               
 100  continue                                                                  
c                                                                               
      endwb = 0. 
      do 40 i=1,nlayer                                                                              
 40   endwb = endwb +  (www(i) * poros(i) * zdepth(i))

      endwb = endwb +  (capac(1) + capac(2) + snoww(1) + snoww(2))                         
     .    -  (ppl+ppc)/1000. +  (etmass)/1000. + roff                             

c      errorw= totwb1 - dble(endwb)                                                     
      errorw= totwb - endwb                                                     
      
      
      pmeter= (ppl+ppc)/1000.                                                   
      emeter= etmass/1000.                                                      
c                                                                               
      if (abs(errorw) .gt. 0.0001) then
      write( *,'(a40)') ' ---> warning: water balance violation'
      write(*,*) 'nymd , abs(errorw)= ',nymd ,abs(errorw)      
      write(98,'(a40)') ' ---> warning: water balance violation'
      write(98,*) 'nymd , abs(errorw)= ',nymd ,abs(errorw)
c	do 35 i=1,nlayer
c 35	write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i)
c      write(98,'(1x,a10,5a13,/,1x,f12.5,2(1x,f12.10),3(1x,f12.5))')
c     .  ' S ',' M ',' E',' R',' -Pr',
c     .' To2', endwb1*1000.,(capac(1)+capac(2)+snoww(1)+snoww(2))*1000., 
c     . dble(etmass), dble(roff)*1000.,-dble(ppl+ppc),endwb*1000.
c      write(98,'(1x,a20,1x,f20.10)')'Error To1-To2(mm):',errorw*1000.d0
c      write(98,*)

      endif
c                                                                               
      cbal = radt(1) - chf - (ect+hc+eci)/dtt                                   
      gbal = radt(2) - shf - (egs+hg+egi)/dtt - heaten/dtt                      
      zlhs = radt(1) + radt(2) - chf - shf                                      
      zrhs = hflux + (ect + eci + egi + egs)/dtt + heaten/dtt                   
c                                                                               
      errore= zlhs - zrhs                                                       
c
      if(abs(errore) .gt. 1.) then
      write(*,'(a40)') ' ---> warning: energy balance violation'
      write(98,910) nymd, zlhs, zrhs, radt(1), radt(2), chf, shf,            
     &       hflux, ect/dtt, eci/dtt, egi/dtt, egs/dtt, hc/dtt, hg/dtt,
     &       heaten/dtt, cbal, gbal                    
      endif
910   format(//,10x,'---> warning: energy balance violation **',//,               
     & /,1x,'date ', i8,                                                        
     & /,1x,'rhs, lhs              ', 2g12.5,                                   
     & /,1x,'rn1, rn2, chf, shf, h ', 5g12.5,                                   
     & /,1x,'ect, eci, egi, egs    ', 4g12.5,                                   
     & /,1x,'hc        hg          ',  g12.5, 12x, g12.5,                       
     & /,1x,'heaten, c-bal, g-bal  ', 3g12.5 )                                  
c                                                                               
200   continue                                                                  
      return                                                                    
      end                                                                     
c                                                                               
c=======================================================================        
c                                                                               
c                                                                               
      subroutine veginc(ichi)                                                   
c                                                                               
c=======================================================================        
c                                                                               
c    read vegetation and soil parameters for SIB2                             
c-----------------------------------------------------------------------        
c    subroutines called :  vegpar
c                          soipar
c                          dynveg
c                          varcal
c
c-----------------------------------------------------------------------        
c                                                                               
c                                                                               
c        subscripts (iv, iw, il) :                                              
c                                                                               
c              iv  : surface layer ;                                            
c                1 = canopy                                                     
c                2 = ground                                                     
c              iw  : radiation wavelength;                                      
c                1 = visible                                                    
c                2 = near infrared                                              
c              il  : vegetation state;                                          
c                1 = live (green) and                                           
c                2 = dead (stems and trunk)                                     
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c                 input parameter set                                           
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c   ivtype        : vegetation type                                             
c                                                                               
c        static parameters associated with vegetation type                      
c        -------------------------------------------------                      
c                                                                               
c   z2            : canopy top height                                           
c   z1            : canopy base height                                          
c   vcover        : vegetation cover fraction                                   
c   chil          : leaf angle distribution factor                              
c   rootd         : rooting depth                                               
c   phc           : 1/2 critical leaf water potential limit                     
c   tran(iw,il)   : leaf transmittance                                          
c   ref (iw,il)   : leaf reflectance                                            
c   effcon        : quantum efficiency                                          
c   gradm         : conductance-photosynthesis slope parameter                  
c   binter        : conductance-photosynthesis intercept                        
c   respcp        : respiration fraction of vmax                                
c   atheta        : wc, we coupling parameter                                   
c   btheta        : wc & we, ws coupling parameter                              
c   trda          : temperature coefficient in gs-a model                       
c   trdm          : temperature coefficient in gs-a model                       
c   trop          : temperature coefficient in gs-a model                       
c   slti          : slope of low temperature inhibition function                
c   hlti          : 1/2 point of low temperature inhibition function            
c   shti          : slope of high temperature inhibition function               
c   hhti          : 1/2 point of high temperature inhibition function           
c                                                                               
c   istype        : soil type                                                   
c                                                                               
c        static parameters associated with soil type                            
c        -------------------------------------------                            
c                                                                               
c   sodep         : total depth of 3 soil moisture layers                       
c   soref(iw)     : soil reflectance                                            
c   bee           : soil wetness exponent                                       
c   phsat         : soil tension at saturation                                  
c   satco         : hydraulic conductivity at saturation                        
c   poros         : soil porosity                                               
c   slope         : cosine of mean slope                                        
c                                                                               
c        time-space varying vegetation parameters, from                         
c        spectral vegetation indice (svi).                                      
c        --------------------------------------------------                     
c                                                                               
c   zlt           : leaf area index                                             
c   green         : green leaf fraction                                         
c   fparc         : canopy absorbed fraction of photosynthetically              
c                 : active radiation (par)  from svi                            
c                                                                               
c        parameters derived from the above                                      
c        ---------------------------------                                      
c                                                                               
c   vmax0         : rubisco velocity of sun-leaf                                
c   gmudmu        : time-mean leaf projection ( g(mu)/ mu )                     
c   z0d           : roughness length                                            
c   dd            : zero plane displacement                                     
c   cc1           : rb coefficient (c1)                                         
c   cc2           : rd coefficient (c2)                                         
c   zdepth        : individual depths of 3 soil moisture layers                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c        other variables                                                        
c        ---------------                                                        
c                                                                               
c      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet                          
c                                                                               
c   g1            : ratio of km(actual) to km(log-linear) at z = z2             
c   g2            : ratio of ra(actual) to ra(log-linear) for momentum          
c                   between: z = z2 and z = zx, where zx = min(zl,zwind)        
c   g3            : ratio of ra(actual) to ra(log-linear) for heat              
c                   between: z = z2 and z = zx, where zx = min(zl,zmet)         
c   ztz0          : parameter to determine depth of transition layer            
c                   above canopy, zl. zl = z2 + ztz0 * z0                       
c   corb1         : non-neutral correction for calculation of aerodynami        
c                   resistance between ha and z2. when multiplied by            
c                   h*rbb/tm gives bulk estimate of local richardson            
c                   number                                                      
c                   rbb = ra for heat between ha and z2.                        
c                   corb2 = 9*g/( rhoair*cpair* (du/dz)**2 )                    
c   corb2         : neutral value of rbb*u2 ( squared ), equivalent to          
c                   rdc**2 for upper canopy                                     
c   ha            : canopy source height for heat                               
c   zwind         : reference height for wind measurement                       
c   zmet          : reference height for temperature, humidity                  
c                   measurement                                                 
c                                                                               
c        the above are generated from sibx + momopt output                      
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                      
      logical pfirst
      data pfirst/.true./
c                                                                               
      read(ichi, *)                                                             
      read(ichi, *)                                                             
      read(ichi, *)                                                             
c                                                                               
      read(ichi, *) ivtype                                                      
      call vegpar (ichi)                                                        
c                                                                               
      read(ichi, *)
      read(ichi, *)
      read(ichi, *) istype, sodep,  (soref(iwave),iwave=1,2)                    

      if (pfirst) then
      pfirst=.false.
 50	format(/,6(1x,e12.6),/)
      write(98,'(a72)')' ivtype'                                                      
      write(98,*) ivtype                                                      
      write(98,'(a72)') ' istype, sodep,  (soref(iwave),iwave=1,2)'                    
      write(98,*) istype, sodep,  (soref(iwave),iwave=1,2)                    
      endif

      call soipar (ichi)                                                        
c                                                                               
      call dynveg (ichi)                                                        
c                                                                               
      call varcal (ichi)                                                        
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine vegpar (ichi)                                                  
c                                                                               
c=======================================================================        
c                                                                               
c     reading/setting of vegetation-type dependent static parameters.           
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                      
      logical pfirst
      data pfirst/.true./
c                                                                               
      read(ichi,*)                                                            
      read(ichi,*)
      read(ichi, *) z2, z1, vcover, chil                                        
      read(ichi,*)
      read(ichi, *) rootd, phc                                                  
c
      read(ichi,*)                                                                               
      read(ichi,*)                                                                               
      read(ichi,*)(tran(iw,1), iw=1,2), (tran(iw,2), iw=1,2)   !(iw,ilive)                                                                                         
      read(ichi,*)(ref (iw,1), iw=1,2), (ref (iw,2), iw=1,2)                               
c
      read(ichi,*)                                                                               
      read(ichi, *) effcon, gradm, binter, respcp, atheta, btheta 
      read(ichi,*)
      read(ichi, *) trda, trdm, trop, slti, hlti, shti, hhti
      read(ichi,*)
      read(ichi,*)  acoef, bcoef, ccoef
c                                                                               
      if (pfirst) then
      pfirst=.false.
 50	format(/,6(1x,e12.6,/))
      write(98,'(a72)') ' z2, z1, vcover, chil'                                        
      write(98,*) z2, z1, vcover, chil                                        
      write(98,'(a72)') ' rootd, phc '                                                  
      write(98,*) rootd, phc                                                  
      write(98,'(a72)') ' (tran(iw,2), iw=1,2)'                              
      write(98,*) (tran(iw,1), iw=1,2)
      write(98,'(a72)') ' (ref (iw,2), iw=1,2)'                               
      write(98,*) (ref (iw,2), iw=1,2)                               
      write(98,'(a72)') 'effcon, gradm, binter, respcp, atheta, btheta' 
      write(98,*) effcon, gradm, binter, respcp, atheta, btheta 
      write(98,'(a72)') ' trda, trdm, trop, slti, hlti, shti, hhti'
      write(98,*) trda, trdm, trop, slti, hlti, shti, hhti
      write(98,'(a72)') ' acoef, bcoef, ccoef'                                                                             
      write(98,*) acoef, bcoef, ccoef                                                                             
      endif

      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine soipar (ichi)                                                  
c                                                                               
c=======================================================================        
c                                                                               
c     reading/setting of soil-type dependent static parameters.                 
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                      
      logical pfirst
      data pfirst/.true./
c                                                                               
      read(ichi, *)                                                             
      read(ichi, *)
      read(ichi, *) iinf, slope, xcs, jqng, xkb,
     &               jesq, jdpsi, jkcon, jsys, jjgs, jqini
      read(ichi, *)
      do 10 i=1,nlayer                                                         
 10   read(ichi,*)
     . zdepth(i),extfrac(i),bee(i),phsat(i),satco(i),poros(i), www(i)

      extmax = 0.
      do 20 il=2,nlayer
 20     extmax = extmax + extfrac(il)

      if (pfirst) then
      pfirst=.false.
      write(98,'(a72)') ' iinf  slope '
      write(98,*)          iinf, slope
      write(98,'(a60)')
     .'zdepth(i) extfrac(i) bee(i) phsat(i) satco(i) poros(i) www(i)'                                 
      do 19 i=1,nlayer                                                         
  19  write(98, '(3(1x,f6.4),2(1x,e10.3),3(1x,f6.4),/)')
     .zdepth(i),extfrac(i),bee(i),phsat(i),satco(i),poros(i),www(i)                                 
      write(98,*) ' extmax(%): ',extmax*100.
      
      if (extmax.gt.1.00001.or.extfrac(1).lt.1.) then
      write(*,29) '!!! warning: extfrac violation ', extmax*100.,' %'
      write(98,29)'!!! warning: extfrac violation ', extmax*100.,' %'
 29	format(a32,f20.10,a3)
      stop 
      endif
      endif			!if pfirst
C...
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine dynveg (ichi)                                                  
c                                                                               
c=======================================================================        
c                                                                               
c     reading of phenological vegetation parameters.                            
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                      
      logical pfirst
      data pfirst/.true./
c                                                                               
      read(ichi, *)                                                             
      read(ichi, *)                                                             
      read(ichi, *) fparc                                                       
      if (pfirst) then
      pfirst=.false.
      write(98,'(a72)') ' fparc' 
      write(98,*) fparc
 50	format(6(1x,e12.6))
      endif
c                                                                               
      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine varcal (ichi)                                                  
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of secondary parameters from input data                       
c                                                                               
c-----------------------------------------------------------------------        
      include 'comsibc.h'                                                      
      logical pfirst
      data pfirst /.true./
c                                                                               
      read(ichi,*)                                                             
      read(ichi,*)                                                             
      read(ichi,*) vmax0, gmudmu, green, zlt
      read(ichi,*)                                                             
      read(ichi,*) z0d, dd, cc1, cc2                                           
      read(ichi,*)                                                             
      read(ichi,*)                                                             
      read(ichi,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet             

c      write(*,'(a72)') ' vmax0, gmudmu, green, zlt'                                   
c      write(*,*) vmax0, gmudmu, green, zlt                                   
c      write(*,'(a72)') ' z0d, dd, cc1, cc2'                                           
c      write(*,*) z0d, dd, cc1, cc2                                           
c      write(*,'(a72)')'corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet'             
c      write(*,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet             

         
cnew...              
      sodep1=0.
      do 10 i=1,nlayer             
 10     sodep1 = sodep1 + zdepth(i)                                             
cnew...

      rootd = amin1( rootd, sodep1*0.75 )                                        

      scatp    =     green   * ( tran(1,1) + ref(1,1) )                         
     &         +( 1.-green ) * ( tran(1,2) + ref(1,2) )                         
      park = sqrt(1.-scatp) * gmudmu                                            
c     fparc = 1. - exp ( -park*zlt )                                            

      fparc = 1. - exp ( -park*zlt )                                            

ch007      zlt = -1./park*alog( 1.-fparc )
      
      
c      write(*,'(a72)') ' rootd '
c      write(*,*) rootd 
c      write(*,'(a72)') ' scatp '
c      write(*,*) scatp 
c      write(*,'(a72)') ' park '
c      write(*,*) park 
c      write(*,'(a72)') ' zlt'
c      write(*,*) zlt
 
      
      if (pfirst) then
      pfirst=.false.
 50	format(/,6(1x,e12.6),/)
      write(98,'(a72)') ' vmax0, gmudmu, green, zlt'                                   
      write(98,*) vmax0, gmudmu, green, zlt                                   
      write(98,'(a72)') ' z0d, dd, cc1, cc2'                                           
      write(98,*) z0d, dd, cc1, cc2                                           
      write(98,'(a72)')'corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet'             
      write(98,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet             
      write(98,'(a72)') ' rootd '
      write(98,*) rootd 
      write(98,'(a72)') ' scatp '
      write(98,*) scatp 
      write(98,'(a72)') ' park '
      write(98,*) park 
      write(98,'(a72)') ' zlt'
      write(98,*) zlt
      endif

      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine cntrol(ichi,icho,maxit,nylast,nyfirst)
c                                                                               
c=======================================================================        
c                                                                               
c      initialisation and switches.                                             
c                                                                               
c-----------------------------------------------------------------------        
c
      include 'comsibc.h'  
      real ydep(nlayer), eacum(nlayer) 
      logical  pfirst                                                   
      data pfirst/.true./
c
c       write(98,*)'  entrando cntrol'                                                                               
c       write(*,*)'  entrando cntrol'                                                                               
                     
  900 format(18a4)                                                              
      read(ichi,*)                                                            
      read(ichi,*)  
      read(ichi, * )dtt, itrunk, ilw, ico2m,facco2, irespg                                            

      read(ichi,*)  
      read(ichi,*) zlat, zlong, time, month, day, year,
     .             maxit, nyfirst, nylast
      read(ichi,*)  
      read(ichi, * )tc, tg, td, ta, tm, ht, qa                                  

 48	format(a24,1x,i2,1x,a5)
 49	format(/,a24,1x,i2,1x,a5,/)

      if (pfirst) then
      pfirst=.false.
      write(98,*)
     . zlat, zlong,time,month,day,year,maxit,nyfirst,nylast
 50	format(
     .'zlat, zlong,time,month,day,year,maxit,nyfirst,nylast',/,
     . 3f8.2,i3,1x,2f5.0,i3,4(1x,i8) )                                                                               
      write(icho,800) zlat, zlong,nyfirst,nylast,maxit                    
  800 format(10x,32('*')/10x,'*       SiB2 off-line run      *'/10x,            
     &32('*')/5x,'latitude : ',f6.2,5x,' longitude : ',f7.2/,
     & 4x,' run length: nyfirst - nylast',4x,i8,4x,i8,/,
     & 4x,'             maxit           ',4x,i8,/)

      if(itrunk .eq. 1) write(icho,801)                                         
      if(itrunk .ge. 2) write(icho,802) itrunk                                  
  801 format(5x,'resistances calculated from initial fluxes')                   
  802 format(5x,'resistances calculated by iteration, itrunk=',i4)              
      if(ilw .eq. 1) write(icho,816)                                            
      if(ilw .eq. 2) write(icho,817)                                            
      if(ilw .eq. 3) write(icho,818)                                            
  816 format(5x,'downward longwave radiation read in as data')               
  817 format(5x,'downward longwave radiation computed from brunts',             
     & ' equation' )                                                        
  818 format(5x,'downward longwave radiation computed as residual in ene        
     &rgy balance', /, 5x,'net radiation read in as data ')                 

c      if (iinf.eq.1) write(*,*)'Infiltration=1 top layer diffusion'
c      if (iinf.eq.2) write(*,*)   
c     &           'Infiltration=2 wave front, w/  overland flow'      
c      if (iinf.eq.3) write(*,*)
c     &           'Infiltration=3 wave front, w/o overland flow'      
c      if (iinf.eq.4) write(*,*)
c     &  'Infiltration=4 wave front w/ sat impeding, w/o overland flow'      

      write(*,'(a28,/,3(5x,a25,2x,f9.4,/),10(5x,a25,2x,i2,/))')
     &  ' Soil hydrology parameters: ', 
     &  ' slope qng     - slope  ', slope, 
     &  ' qstar Horton  - xcs    ', xcs  ,
     &  ' Liston qng    - xkb    ', xkb  , 
     &  ' ativar qng    - jqng   ', jqng ,
     &  ' infiltration  - iinf   ', iinf , 
     &  ' numer scheme  - jesq   ', jesq , 
     &  ' C(psi) calc   - jdpsi  ', jdpsi, 
     &  ' K(psi) calc   - jkcon  ', jkcon,
     &  ' linear system - jsys   ', jsys ,
     &  ' Jacobi/Gauss  - jjgs   ', jjgs ,
     &  ' Q initial     - jqini  ', jqini
     

      ydep(1) = zdepth(1)/2.
      eacum(2) = extfrac(2)
      eacum(1) = extfrac(1)

      do i=2,nlayer
      ydep(i) = zdepth(i-1) + zdepth(i)/2.
      if (i.ge.3) eacum(i) = eacum(i-1) + extfrac(i)
      enddo

      write (*,'(a44,/,50(i3,3x,f6.0,3x,f7.2,3x,f7.2,3x,f7.3,/))')
     & 'Layer Depth_cm  Root % Root_acum %  Winicial',
     & (i, 100.*ydep(i), extfrac(i)*100.,eacum(i),www(i),i=1,nlayer)

      endif

       write(98,*)'  saindo cntrol'                                                                               
c       write(*,*)'  saindo cntrol'                                                                               
      return                                                                    
      end                                                                       
c-----------------------------------------------------------------------------------------------                                                                               

                                                                     
