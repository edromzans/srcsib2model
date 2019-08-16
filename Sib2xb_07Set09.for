c=======================================================================
c                                                                               
      subroutine updat2                                                         
c                                                                               
c=======================================================================        
c                                                                               
c     updating of all prognostic variables.                                     
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     subroutines called   : updat2                                        
c     ------------------     snow2                                         
c                            run2n                                          
c                                                                               
c++++++++++++++++++++++++++++++output from this block++++++++++++++++++++       
c                                                                               
c       dtc            canopy temperature increment (K)                         
c       dtd            deep soil temperature increment (K)                      
c       dtg            ground surface temperature increment (K)                 
c       www(3)         ground wetness                                           
c       capac(2)       canopy/ground liquid interception store (m)              
c       snoww(2)       canopy/ground snow interception store (m)                
c       roff           runoff (m)                                              
c       etmass (fws)   evapotranspiration (mm)                                  
c       hflux (fss)    sensible heat flux (w m-2)                               
c                                                                               
c++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++        
c                                                                               
c       ecmass         canopy evapotranspiration (mm)                           
c       egmass         ground evapotranspiration (mm)                           
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
      include 'comsibc.h'     
      
      real extra2p(nlayer)
                                                        
c       write(98,*)' updat2: begin'
c       write(98,*)' updat2: to call snow1'


      call snow1                                                                

c       write(98,*)' updat2: snow1 passed'
c                                                                               
c                                                                               
c----------------------------------------------------------------------         
c    interception losses.                                                       
c    evaporation losses are expressed in j m-2 : when divided by                
c    ( hlat*1000.) loss is in m m-2. mass terms are in kg m-2 dt-1              
c    interception and drainage treated in inter2.                               
c                                                                               
c----------------------------------------------------------------------         
c                                                                               

c       write(98,*)' updat2: interception losses begin'

      rsnow = snoww(1)/(snoww(1)+capac(1)+1.e-10)                               
      facks = 1. + rsnow * ( snofac-1. )                                        
      if ( (ect+eci) .gt. 0.) go to 100                                         
      eci = ect+eci                                                             
      ect = 0.                                                                  
      facks = 1. / facks                                                        
100   capac(1) = capac(1) - ( 1.-rsnow )*eci*facks/hlat/1.e3                    
      snoww(1) = snoww(1) -      rsnow  *eci*facks/hlat/1.e3                    
      ecmass = eci*facks / hlat                                                 
c                                                                               
      rsnow = snoww(2)/(snoww(2)+capac(2)+1.e-10)                               
      facks = 1. + rsnow * ( snofac-1. )                                        
      if ( (egs+egi) .gt. 0. ) go to 200                                        
      egi = egs+egi                                                             
      egs = 0.                                                                  
      facks = 1. / facks                                                        
200   capac(2) = capac(2) - ( 1.-rsnow )*egi*facks/hlat/1.e3                    
      snoww(2) = snoww(2) -      rsnow  *egi*facks/hlat/1.e3                    
      egmass = egi*facks / hlat                                                 

c       write(98,*)' updat2: interception losses passed'

c                                                                               
c----------------------------------------------------------------------         
c    dumping of small capac values onto soil surface store                      
c----------------------------------------------------------------------         
c                                                                               
c       write(98,*)' updat2: dump snow to w1 begin'


      do 1000 iveg = 1, 2                                                       
      if ( (snoww(iveg)+capac(iveg)) .gt. 0.00001 ) go to 300                   
cnew  www(1) = www(1) + (snoww(iveg)+capac(iveg)) / ( poros*zdepth(1) )         
      www(1) = www(1) + (snoww(iveg)+capac(iveg))/(poros(1)*zdepth(1) )         
      capac(iveg) = 0.                                                          
      snoww(iveg) = 0.                                                          
300   continue                                                                  
1000  continue                                                                  

c       write(98,*)' updat2: dump snow to w1 passed'

c                                                                               
c----------------------------------------------------------------------         
c    snowmelt / refreeze calculation                                            
c----------------------------------------------------------------------         
c                                                                               

c       write(98,*)' updat2: call snow2  begin'

      call snow2                                                                

c       write(98,*)' updat2: call snow2  passed'

c                                                                               
c----------------------------------------------------------------------         
c    evapotranspiration losses,                                                 
c    extraction of transpiration loss from root zone, soil evaporation.         
c                                                                               
c      ect         (e-dc)  : equation (5,6), SE-86                              
c      egs         (e-s)   : equation (5)  , SE-86                              
c----------------------------------------------------------------------  

ch.. preferential multilayer extraction 
      ectnew = 0.

c      write(98,'22f8.7') (www(i),i= 1, nlayer)
      do 40 il= 1, nlayer
      if (extfrac(il).gt.0.) then
      facl    = 1./ hlat / 1.e3 / (poros(il)*zdepth(il))                                   
      if (il.eq.1) then
c     write(98,*)' updat2: prefer extraction il=1'
      extrak     = amin1 (www(1), egs*extfrac(1)*facl)                                          
      egsdif     = egs - extrak/facl                                                
      egs        = extrak/facl
      hg         = hg + egsdif                                                      
      egmass     = egmass + egs/hlat                                                
      www(1)     = www(1) - egs*facl                                                
      else
c     write(98,*)' updat2: prefer extraction il=',il
      ectil     = ect * facl * extfrac(il) 
      extrak    = amin1 ( www(il), ectil )                                          
      ectdif    = (ectil - extrak)/facl
      ectnew    = ectnew + extrak/facl
      hc        = hc + ectdif                                                      
      ecmass    = ecmass + extrak/facl/hlat
      www(il)   = www(il) - extrak
      endif          
      endif
      
cHR..211108     
      extra2p(il) = extrak
     
      
  40  continue

      ect = ectnew


cHR..211108     

c      write(98,'2(11(1x,f8.7))')(www(il),il=1,nlayer),
c     &      (1.e+02*extra2p(il),il= 1, nlayer)


ch ...
c-----------------------------------------------------------------------        
c    calculation of total moisture and sensible heat fluxes from surface.       
c-----------------------------------------------------------------------        
c                                                                               
      etmass = ecmass + egmass                                                  
      hflux  = (hc+hg) / dtt                                                    
c                                                                               
c----------------------------------------------------------------------         
c    calculation of interflow, infiltration excess and loss to                  
c    groundwater .  all losses are assigned to variable 'roff' .                
c----------------------------------------------------------------------         
									       

	 call run2n

c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c    update of temperatures and pbl variables. note that tc and tg              
c    are modified in interc as a result of precipitation inputs.                
c    update of interception stores.                                             
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      tc  = tc + dtc                                                            
      tg  = tg + dtg                                                            
      td  = td + dtd                                                            
      th  = th + dth                                                            
      qm  = qm + dqm                                                            
c                                                                               
      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine inter2                                                         
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of  interception and drainage of rainfall and snow            
c     incorporating effects of patchy snow cover and temperature                
c     adjustments.                                                              
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     (1) non-uniform precipitation                                             
c         convective ppn. is described by area-intensity                        
c         relationship :-                                                       
c                                                                               
c                   f(x) = a*exp(-b*x)+c                                        
c                                                                               
c         throughfall, interception and infiltration                            
c         excess are functional on this relationship                            
c         and proportion of large-scale ppn.                                    
c         reference: sato et al.(1989b), appendix.                              
c                                                                               
c     (2) reorganisation of snowmelt and rain-freeze procedures.                
c               subroutine adjust                                               
c                                                                               
c     (3) additional calculation for partial snow-cover case.                   
c               subroutine patchs                                               
c                                                                               
c     (4) reorganisation of overland flow.                                      
c         reference: SA-89B, appendix.                              
c                                                                               
c     (5) modified calaculation of soil heat capacity and                       
c         conductivity.                                                         
c                                                                               
c=======================================================================        
c                                                                               
c     subroutines in this block : snow1                                        
c     -------------------------   adjust                                        
c                                 patchs                                        
c                                 snow1                                         
c
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       roff           runoff (m)                                              
c       tc             canopy temperature (K)                                   
c       tg             ground surface temperature (K)                           
c       www(1)         ground wetness of surface layer                          
c       capac(2)       canopy/ground liquid interception store (m)              
c       snoww(2)       canopy/ground snow interception store (m)                
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
      include 'comsibc.h'                                                      
c                                                                               
      dimension  pcoefs(2,2)                                                    
      data pcoefs(1,1)/ 20. /, pcoefs(1,2)/ .206e-8 /,                          
     &     pcoefs(2,1)/ 0.0001 /, pcoefs(2,2)/ 0.9999 /, bp /20. /              
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      call snow1                                                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     prec ( pi-x )   : equation (c.3), SA-89B                                  
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      ap = pcoefs(2,1)                                                          
      cp = pcoefs(2,2)                                                          
      totalp = ppc + ppl                                                        
      if( snoww(1) .gt. 0. .or. snoww(2) .gt. 0. .or. tm .lt. tf )              
     &     ppc = 0.                                                             
      ppl = totalp - ppc                                                        
      if(totalp.lt.1.e-8) go to 100                                             
      ap = ppc/totalp * pcoefs(1,1) + ppl/totalp * pcoefs(2,1)                  
      cp = ppc/totalp * pcoefs(1,2) + ppl/totalp * pcoefs(2,2)                  
100   continue                                                                  
c                                                                               
csml...
	croff  = 0.			! surface runoff
	cthru  = 0.			! incoming thrufall to soil surface
csml...
      roff = 0.                                                                 
      thru = 0.                                                                 
      fpi  = 0.                                                                 
c                                                                               
c----------------------------------------------------------------------         
c     heat capacity of the soil, as used in force-restore heat flux             
c     description. dependence of csoil on porosity and wetness is               
c     based on CS-81.                       
c----------------------------------------------------------------------         
c
csml...	porosity is taken of the layer 2

      slamda = ( 1.5*(1.-poros(2)) + 1.3*www(1)*poros(2) ) /                          
     &         ( 0.75 + 0.65*poros(2) - 0.4*www(1)*poros(2) ) * 0.4186                
      shcap  = ( 0.5*(1.-poros(2)) + www(1)*poros(2) ) * 4.186 * 1.e6                 
      csoil  = sqrt( slamda * shcap * 86400./pie ) / 2.                         
c                                                                               
c----------------------------------------------------------------------         
c     input precipitation is given in mm, converted to m to give p0.            
c----------------------------------------------------------------------         
c                                                                               
      p0 = totalp * 0.001                                                       
c                                                                               
      do 1000 iveg = 1, 2                                                       
c                                                                               
      realc = 2. - iveg                                                         
      realg = iveg - 1.                                                         
c                                                                               
      capacp = capac(iveg)                                                      
      snowwp = snoww(iveg)                                                      
c                                                                               
      xsc = amax1(0., capac(iveg) - satcap(iveg) )                              
      capac(iveg) = capac(iveg) - xsc                                           
      xss = amax1(0., snoww(iveg) - satcap(iveg) ) * realc                      
      snoww(iveg) = snoww(iveg) - xss                                           
      roff = roff + xsc + xss                                                   
c                                                                               
      spechc = amin1( 0.05, ( capac(iveg) + snoww(iveg) ) ) * cw                
     &       + realc * zlt * clai + realg * csoil                               
      ts = tc * realc + tg * realg                                              
c                                                                               
c----------------------------------------------------------------------         
c     proportional saturated area (xs) and leaf drainage(tex)                    
c                                                                               
c     tex ( d-c )     : equation (c.8), SA-89B                                 
c     xs  ( x-s )     : equation (c.7), SA-89B                                  
c     tex ( d-c )     : equation (c.8), SA-89B                                  
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      chiv = chil                                                               
      if ( abs(chiv) .le. 0.01 ) chiv = 0.01                                    
      aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv                              
      bb = 0.877 * ( 1. - 2. * aa )                                             
      exrain = aa + bb                                                          
c                                                                               
      zload = capac(iveg) + snoww(iveg)                                         
      fpi = ( 1.-exp( - exrain*zlt/vcover ) )*vcover*realc + realg              
      tti = p0 * ( 1.-fpi )                                                     
      xs = 1.                                                                   
      if ( p0 .lt. 1.e-9 ) go to 200                                            
      arg =  ( satcap(iveg)-zload )/( p0*fpi*ap ) -cp/ap                        
      if ( arg .lt. 1.e-9 ) go to 200                                           
      xs = -1./bp * alog( arg )                                                 
      xs = amin1( xs, 1. )                                                      
      xs = amax1( xs, 0. )                                                      
200   tex = p0*fpi * ( ap/bp*( 1.- exp( -bp*xs )) + cp*xs ) -                   
     &      ( satcap(iveg) - zload ) * xs                                       
      tex = amax1( tex, 0. )                                                    
c                                                                               
c----------------------------------------------------------------------         
c     total throughfall (thru) and store augmentation                            
c----------------------------------------------------------------------         
c                                                                               
      if ( iveg .eq. 2 ) go to 300                                              
c                                                                               
      thru = tti + tex                                                          
      pinf = p0 - thru                                                          
      if( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf                         
      if( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf                         
c                                                                               
      call adjust ( tc, spechc, capacp, snowwp, iveg )                          
c                                                                               
      p0 = thru                                                                 
      go to 700                                                                 
c                                                                               
300   continue                                                                  
c                                                                               
      if ( tg .gt. tf .and. snoww(2) .gt. 0. ) then                             
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      call patchs ( p0 )                                                     
      go to 700                                                              
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      endif                                                                     
c                                                                               
      thru = tti + tex                                                          
      if ( tg .le. tf .or. tm .le. tf ) thru = 0.                               
      pinf = p0 - thru                                                          
      if( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf                         
      if( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf                         
      if( tm .le. tf ) go to 500                                                

c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     instantaneous overland flow contribution ( roff )                          
c                                                                               
c     roff( r-i )     : equation (c.13), SA-89B                                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      equdep = satco(1) * dtt                                                      
c                                                                               
      xs = 1.                                                                   
      if ( thru .lt. 1.e-9 ) go to 400                                          
      arg = equdep / ( thru * ap ) -cp/ap                                       
      if ( arg .lt. 1.e-9 ) go to 400                                           
      xs = -1./bp * alog( arg )                                                 
      xs = amin1( xs, 1. )                                                      
      xs = amax1( xs, 0. )                                                      
400   roffo = thru * ( ap/bp * ( 1.-exp( -bp*xs )) + cp*xs )                    
     &       -equdep*xs                                                         
      roffo = amax1 ( roffo, 0. )                                               

cHR..
c      write(98,'(20a10)')
c    & 'xs','P','D','P-D','Dc','Dd','Ri','R','Pi','q0' (no Main)
c      write(98,'(30(1x,f10.5))') 
c     & xs,p0*1000.,thru*1000.,pinf*1000.,tex*1000.,
c     & tti*1000.,roffo*1000.,roff*1000.,
c     & amax1 (0., (1.0 - www(1))*zdepth(1)*poros(1) )*1000.,
c     & q0*1000.*dtt,
c     & (www(i),i=1,nlayer)

c....................................................................
cHR..Jan2008	infiltration/surface runoff options: iinf =
c....................................................................
c
c  =1  capac. infiltracao = disponibilidade top layer
c
c  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente onda
c  =6  id. 2, frente de onda cessa encontro saturacao
c
c  =3  all thrufall infiltrates; infiltracao frente onda;
c  =5  id. 3, frente de onda cessa encontro saturacao
c
c  =4  all thrufall infiltrates; frente onda limitada R-Horton
c	qstar (f* de EN-89) 
c

	if (iinf.eq.1) then
	q0 = amax1 (0., (1.0 - www(1))*zdepth(1)*poros(1) )
        q0 = ( amin1 (q0, thru-roffo) ) / dtt		! m/s
	roff = roff + amax1 ( 0., thru - q0*dtt)	
	endif
	
	if (iinf.eq.2.or.iinf.eq.6) then
	q0 = amax1 (0., thru - roffo)/dtt  
	roff = roff +  roffo 
	endif

	if (iinf.eq.3.or.iinf.eq.4.or.iinf.eq.5) then
	q0 = amax1 (0., thru)/dtt  
	endif

        croff = croff + roff         
	cthru = thru

c====================================================================

500   continue                                                                  
c                                                                               
      call adjust ( tg, spechc, capacp, snowwp, iveg )                          
c                                                                               
700   continue                                                                  
c                                                                               
1000  continue                                                                  
c                                                                               
      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine begtem                                                      
c                                                                               
c-----------------------------------------------------------------------        
c     core routine: calculation of canopy and ground temperature                
c     increments over time step, fluxes derived.                                
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c                                                                               
c     subroutinescalled : snow1                                         
c     -----------------                                                
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       rstfac(2)      soil moisture stress factor                              
c       rsoil          soil surface resistance (s m-1)                          
c       hr             soil surface relative humidity                           
c       wc             canopy wetness fraction                                  
c       wg             ground wetness fraction                                  
c       ccx            canopy heat capacity (j m-2 k-1)                         
c       cg             ground heat capacity (j m-2 k-1)                         
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
c----------------------------------------------------------------------         
      include 'comsibc.h'                                                      
c
c----------------------------------------------------------------------         
c     e(x) is vapour pressure in mbars as a function of temperature             
c     ge(x) is d e(x) / d ( temp )                                              
c----------------------------------------------------------------------         
c                                                                               
      e(x) = exp( 21.18123 - 5418. / x ) / .622                                 
      ge(x) = exp( 21.18123 - 5418. / x ) * 5418.                               
     $        / (x*x) / .622                                                    
c
c----------------------------------------------------------------------         
c                                                                               
      call snow1                                                                
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      hlat     = ( 3150.19 - 2.378 * tm ) * 1000.                               
      psy      = cpair / hlat * psur / .622                                     
      snofac   = hlat/ ( hlat + snomel / 1.e3 )                                 
c                                                                               
c----------------------------------------------------------------------         
c    calculation of canopy and ground heat capacities.                          
c    n.b. this specification does not necessarily conserve energy when          
c    dealing with very large snowpacks.                                         
c----------------------------------------------------------------------         
c                                                                               
      ccx = zlt * clai + (snoww(1)+capac(1))*cw                                 
      cg  = csoil + amin1 ( 0.05, (snoww(2)+capac(2)) ) * cw                    
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c----------------------------------------------------------------------         
c      calculation of ground surface temperature and wetness fractions          
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      tsnow = amin1 ( tf-0.01, tg )                                             
      rsnow = snoww(2) / (snoww(2)+capac(2)+1.e-10)                             
c                                                                               
      tgs = tsnow*areas + tg*(1.-areas)                                         
c                                                                               
      etc   = e(tc)                                                             
      etgs  = e(tgs)                                                            
      getc  = ge(tc)                                                            
      getgs = ge(tgs)                                                           
c                                                                               
      wc = amin1( 1.,( capac(1) + snoww(1))/satcap(1) )                         
      wg = amax1( 0.,  capac(2)/satcap(2) )*0.25                                
c                                                                               
c-----------------------------------------------------------------------        
c     calculation of soil moisture stress factor.                               
c     average soil moisture potential in root zone (layer-2) used as            
c     source for transpiration.                                                 
c                                                                               
c      phroot      (psi-r) : equation (47) , SE-86, (22), SE-89
c      rstfac(2)  f(psi-l) :    "     (C17), SE-96             
c-----------------------------------------------------------------------        
c                      
ch      phroot = phsath * amax1( 0.02, www(2) ) ** ( - beeh )                       
ch      phroot = amax1 ( phroot, -2.e3 )                                          

ch...
cnew..equation 22 (SE-89) used as potential in root zone
cnew    rdepth= 0.
cnew    phroot= 0.
cnew    do 40 i= 1, nlayer
cnew    if (rdepth.gt.rootd) goto 40
cnew    phroot= phroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee))
cnew    rdepth= rdepth + zdepth(i)
cnew 40 continue
cnew    phroot = amax1 ( phroot*phsat, -2.e3 )                                          
cnew ..

ctes..equation 22 (SE-89) used as potential in root zone
c	wdepth= 0.
c	whroot= 0.
	!write(98,*)nymd
c	do 40 i= 2, nlayer

c	if (wdepth.gt.rootd) goto 40
cnew    whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee))
c      whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee(i)))
cml05_nov      whroot= whroot + extfrac(i)*((amax1(0.02,www(i)))**(-bee(i)))
c  	wdepth= wdepth + zdepth(i)

c	if (nymd.eq.40101512.or.nymd.eq.40041512) then
c	write (98,*)
c     &  ' i,zdepth(i),www(i),psi(i),whroot,wdepth'  	
c        write(*,*)   ' aqui nymd 40101512 ou 40041512'
c        write (98,*)i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)),whroot,
c     &	wdepth
c	endif

c 40     continue
c	whroot = amax1 ( whroot*phsat(3), -2.e3 )                                          

c	phroot = whroot
ch...
	
c       write(98,'(a15,2(1x,e10.4))')' phroot, whroot:', phroot, whroot
ctes ..

c      rstfac(2) = 1./( 1 + exp( 0.02*( phc-phroot) ) )                          
c      rstfac(2) = amax1( 0.0001, rstfac(2) )                                    
c      rstfac(2) = amin1( 1.,     rstfac(2) )                                    
cHR alteracao feita dia 06_nov 
        phroot=0.
	rstfac2p=0.
c 	 write(98,'22f8.7') (www(i),i= 1, nlayer)
	do 40 i= 2, nlayer

	phroot=phsat(i)*((amax1(0.02,www(i)))**(-bee(i)))
	phroot=amax1(phroot,-2.e4)
	rstfac2p = rstfac2p + extfrac(i)*(1./(1+exp(0.02*(phc-phroot))))
	
c	if (nymd.eq.40101512.or.nymd.eq.40041512) then
c	write (98,'a60')
c     &  ' i,zdepth(i),www(i),psi(i),phroot,rstfac2p,extfrac,fpsi'  	
c        write(*,*)   ' aqui nymd 40101512 ou 40041512'
c        write (98,'i3,7f7.4')
c     &  i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)),
c     &	phroot,rstfac2p, extfrac(i), (1./(1+exp(0.02*(phc-phroot))))
c	endif

 40     continue
    
      rstfac(2)=rstfac2p	
      rstfac(2) = amax1( 0.0001, rstfac(2) )                                    
      rstfac(2) = amin1( 1.,     rstfac(2) )  

c        write(98,'22f8.7') (www(i),i= 1, nlayer)

c	if (nymd.eq.40101512.or.nymd.eq.40041512) 
c     &	write (98,*)rstfac(2),phroot
                                                                               
c----------------------------------------------------------------------         
c                                                                               
c      rsoil function from fit to FIFE-87 data.  soil surface layer             
c      relative humidity.                                                       
c                                                                               
c      rsoil      (rsoil) : Heiser 1992 (personal communication)                
c      hr         (fh)    : equation (66) , SE-86                               
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      fac = amin1( www(1), 1. )                                                 
      fac = amax1( fac, 0.02  )                                                 
      rsoil =  amax1 (0.1, 694. - fac*1500.) + 23.6                             
c                                                                               
      psit = phsat(1) * fac ** (- bee(1) )                                            
      argg = amax1(-10.,(psit*g/461.5/tgs))                                     
      hr = exp(argg)                                                            
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c====================================================================  
c
      subroutine run2n
c
c     modified multi-layer scheme:  Humberto da Rocha 03/04/1996
c
c====================================================================
c
cHR..Dez2008	infiltration/surface runoff options: iinf =
c....................................................................
c
c  =1  capac. infiltracao = disponibilidade top layer
c
c  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente onda
c  =6  id. 2, frente de onda cessa encontro saturacao
c
c  =3  all thrufall infiltrates; frente onda;
c  =5  id. 3, frente de onda cessa encontro saturacao
c
c  =4  all thrufall infiltrates; frente onda limitada R-Horton
c	qstar (f* de EN-89) 
c
c ALL (see routine inter2 for q0 calculation)
c....................................................................

        include 'comsibc.h'
C
      real temw(nlayer), temwp(nlayer), temwpp(nlayer),
     &  a(nlayer), b(nlayer), c(nlayer), d(nlayer),
     &  da(nlayer), db(nlayer), dc(nlayer)
      logical wavend
cHR
      dimension qdowrd(nlayer), qupwrd(nlayer), qhorton(nlayer)
      

      do 1000 i= 1, nlayer
	temw(i)= amax1( 0.03, www(i) )
	temwp(i)= temw(i) ** (-bee(i))
	temwpp(i)= amin1( 1., temw(i) ) ** (2.*bee(i)+3.)
cH
      qdowrd (i) = 0.	! fluxos downward (drenagem rapida + difusiva), i=1 (infiltracao superficie)
      qupwrd (i) = 0.	! fluxos upward (capilar)
      qhorton(i) = 0.	! infiltração maxima Hortoniana EN-89 
 1000 continue

c-----------------------------------------------------------------------
c	iinf  = 1
c-----------------------------------------------------------------------
	if (iinf.eq.1) then 
	www(1) =  www(1) + q0*dtt/(poros(1)*zdepth(1))
	q0 = 0.
	
	qdowrd(1) = qdowrd(1) + q0	! infiltração superficie (m/s)
	endif
	
c-----------------------------------------------------------------------
c	iinf  > 1
c-----------------------------------------------------------------------

	if (iinf.gt.1) then	
	q0now = q0
	wavend = .false.

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	if (q0now.gt.0.) then		! throughfall event

c.............................................................................
c	capacidade infiltração da camada i
c.............................................................................

	do while (.not.wavend)

	do 75 i = 1, nlayer
	
c	write(*,*) ' do while (.not.wavend), E wavend = ', wavend
c	pause

c	jdpsi = 2
c	call retec ( jdpsi, dpdw, i)
c	call hydcon (avk, i,temwp(i),temwp(i+1),temwpp(i),temwpp(i+1))
c	avk = sqrt( satco(i)*satco(i+1) )
c	qstar = - 2.*avk*(dpdw*(1.-www(i+1))/(zdepth(i)+zdepth(i+1))-1.)	

	zpond = amax1(0., (1.-www(i)) * zdepth(i)*poros(i))
	call retec ( dpdw, 1, i)	! dpsi/dw at saturation threshold p/ capac infiltracao
        xni = dpdw / zdepth(i)	
        qstar = satco(i)*( xni*www(i) + 1. - xni) ! infiltração Horton 
	qstar = amin1 (qstar, 0.)	! only negative flux is downward eq(13) EN-89
	qstar = abs(qstar)		! physical drainage
		 		
	if (iinf.ne.4)   	! infiltração ilimitada,  sem Runoff-Horton (iinf=2,6,3,5)
     &       qstar = amax1(cs * qstar, 1.e+20)
		
	q0 = q0now					! oferta sempre>0
        q0now = amin1( q0now, zpond/dtt, qstar)		! infiltrado >=0
        qdowrd (i) = qdowrd(i) + q0now		! diagnostico m/s	
	qhorton(i) = qstar			! diagnostico m/s

	if (q0now.eq.0.) then		! há oferta (mas não infiltra em alguns casos iinf)
	if (iinf.eq.4.or.iinf.eq.5.or.iinf.eq.6) wavend = .true.				
	endif
	
	www(i) = www(i) + q0now*dtt/(zdepth(i)*poros(i))! absorvido (ou não, se infiltrado=0)
	wover = amax1( 0., (www(i)-1.))		! check arredon/to
	www(i)= www(i) - wover			!..
	roff= roff + wover*poros(i)*zdepth(i)	!..

	q0now = amax1( 0., q0 - q0now )			! atualiza oferta de infiltração

	if (q0now.eq.0..or.			! oferta cessou
     &	   (q0now.gt.0..and.i.eq.nlayer)) then  ! onda atingiu camada profunda saturada
        wavend =.true.   
	endif	
	
	
c	write(*,*) ' i layer,  q0now =' , i, q0now
c	qexcess =  amax1 (0., ( q0now -  amin1 ( q0now, qstar ) ) )
c	roff = roff + qexcess*dtt	  
c	croff = croff + qexcess*dtt	  

 75	continue

	end do				!  wavend = F
 
	endif				! throughfall event

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	croff = croff + q0now*dtt	!...excess to surface runoff
	roff  =  roff + q0now*dtt	


c-----------------------------------------------------------------------	

	endif				!  option iinf > 1

C----------------------------------------------------------------------
C
c    calculation of inter-layer exchanges of water due to gravitation
c    and matric gradient. the values of w(x) + dw(x) are used to calculate
c    the potential gradients between layers.
c    modified calculation of mean conductivies follows me-82, which
C    reduces recharge flow to top layer
C        
C    dpdw : estimated derivative of soil moisture potential with respect
C           to soil wetness. assumption of gravitational drainage used
C           to estimate likely minimum wetness over the time step.
C
C     qqq (q    )  equation (61) , se 86     i = 1 , nlayer   (m/s)
C             I , I + 1                
C
C      avk  (k)     equation (4.14), me 82
C             I , I + 1
C
C-----------------------------------------------------------------------

	do 49 i= 1, nlayer
	temw(i)= amax1( 0.03, www(i) )				! W
	temwp(i)= temw(i) ** (-bee(i))				! Psi
	temwpp(i)= amin1( 1., temw(i) ) ** (2.*bee(i)+3.)	
   49 continue

c-------------------------------------------------------------------
c	 Flow between soil layers, diffusion equation in multi-layer scheme
c         backward implicit calculation, or Crank-Nicholson  method
c         linear system of (nlayer-1 x nlayer -1 ) dimension
c         tridiagonal matrix solution - Thomas algorithm
c
c	b = diagonal principal
c	c = superior diagonal principal
c	a = sub (inferior) diagonal principal
c	d = coeficientes independentes (direita igualdade)
c				HRocha 15.12.96, atualizado 07.01.09
c-------------------------------------------------------------------

c..	esquema discretização numérica ................................
c	jesq = 1		! backward
c	jesq = 2		! Crank-Nicolson
c	jesq = 3		! desativa difusão de agua

c..	método calculo dpsi/dteta .....................................
c	jdpsi = 1		!  SiB2 original 
c	jdpsi = 2		!  derivada CH-78 media ponderada espessura
c       jdpsi = 3		!  derivada CH-78 media geometrica 
c       jdpsi = 4		!  derivada CH-78 media aritmetica

c..	método calculo condutividade hidraulica .......................
c	jkcon = 1		! SiB2 original ME-82
c	jkcon = 2		! media ponderada espessura
c       jkcon = 3		! media geometrica 
c       jkcon = 4		! media aritmetica

c...	método solução sistema eqs lineares
c	jsys=	1    matriz tridiagonal (HR-1996, revisado-2009)
c		3    método iterativo
c			3-definir jjgs = 1   método Jacobi
c			3-definir jjgs = 2   método Gauss-Seidel

c......................................................................

	do 50 i= 1, nlayer-1

c...	calculo dpsi/dw  ......................................

	call retec (dpdw, 0, i)

c...	calculo condutividade e difusividade hidraulica .......................

       call hydcon (avk, i)

       avdif = avk * dpdw / poros(i)

c...	valor inicial de qqq(i)
	if (jqini.eq.0) qqq(i) = 0.

	if (jqini.eq.1) then
	dpsidz = phsat(i) * temwp(i)
	dpsidz = dpsidz - phsat(i+1) * temwp(i+1)
	dpsidz = 2.* dpsidz / (zdepth(i) + zdepth(i+1))
	qqq(i) = -avk * (dpsidz + 1.)	
	endif
	
	jqini = -999.


c... 	matriz multi-camada: esquema backward
	if (jesq.eq.1) then
	
	a(i) = -2.*dtt*avdif / (zdepth(i) * (zdepth(i) + zdepth(i+1)))

	b(i) =  2.*dtt*avdif / (zdepth(i) * zdepth(i+1)) + 1.
	
	c(i) = -2.*dtt*avdif / (zdepth(i+1) * (zdepth(i) + zdepth(i+1)))	

        d(i) = qqq(i)

	if (i.eq.1) 	   a(i) = 0.
	if (i.eq.nlayer-1) c(i) = 0.

c        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))')
c     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif',
c     &   'log(avdif1)','log(avdif2)','log(avdif'  
c        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))')            
c     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,  ! x 3.6e5 m/s p/ cm2/h
c     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,),
c     &    log(avdif* 3.6e5)

	endif
	
c... 	matriz multi-camada: esquema Crank-Nicolson
	if (jesq.eq.2) then	
	
	a(i) = -dtt*avdif / (zdepth(i) * (zdepth(i) + zdepth(i+1)))

	b(i) =  1. + dtt*avdif / (zdepth(i) * zdepth(i+1))
	
	c(i) = -dtt*avdif / (zdepth(i+1) * (zdepth(i) + zdepth(i+1)))	

	if (i.eq.1) 	   a(i) = 0.
	if (i.eq.nlayer-1) c(i) = 0.

        da(i) = -a(i)
        db(i) = 1. - dtt*avdif / (zdepth(i) * zdepth(i+1))
        dc(i) = -c(i)        

	if (i.ge.2.and.i.le.(nlayer-2)) 
     &  d(i) = da(i)*qqq(i-1) + db(i)*qqq(i) + dc(i)*qqq(i+1)

	if (i.eq.1) 
     &  d(i) =  db(i)*qqq(i) + dc(i)*qqq(i+1)
	
	if (i.eq.(nlayer-1)) 
     &  d(i) = da(i)*qqq(i-1) + db(i)*qqq(i)
			

c        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))')
c     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif',
c     &   'log(avdif1)','log(avdif2)','log(avdif'  
c        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))')            
c     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,	! x 3.6e5 m/s p/ cm2/h
c     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,),
c     &    log(avdif* 3.6e5)

	endif

c..


cHHH 	qng incrementado em W no final apenas
cHH	if (i.eq.nlayer-1) d(i) = d(i) + qng

cHHH

cH       if(i.eq.1) then
cH       write(itmp5,*) ' Run2n: sistema eqs montado '
cH       write(itmp5,'(a8,50(2x,a9,2x))')
cH     &    'i','a(i)','b(i)','c(i)','d(i)','d(i)_mm/d'
cH       endif
cH       write(itmp5,'(i8.8,50(4x,e9.3))') i, a(i),b(i),c(i),d(i),
cH     &                                   d(i)*1000.*dtt*24.


  50   continue


c..	solucao sistema eqs lineares

	if(jsys.eq.1) call tridia2 (nlayer-1, d, c, a, b )
	
	if(jsys.eq.3) call eqlin (itmp5,jjgs,nlayer-1, d, c, a, b)

c       write(itmp5,'(/,i8.8,50(1x,e9.3),/)')
c     &   nymd, (d(i),i=1,nlayer-1)

cH	write(itmp5,*)

C---------------------------------------------------------------------
C   update wetness of soil layers due to layer interflow and base flow
C---------------------------------------------------------------------

	do 3000 i= 1, nlayer-1

        qqq(i)= d(i)
        if (jesq.eq.3) qqq(i) = 0.	

	if (qqq(i).lt.0.) qdowrd(i+1) = qdowrd(i+1) + abs(qqq(i))
	if (qqq(i).gt.0.) qupwrd(i+1) = qupwrd(i+1) + qqq(i)	

	qmin=  -www(i)   * (poros(i)  *zdepth(i)  /dtt)	
	qmax=   www(i+1) * (poros(i+1)*zdepth(i+1)/dtt)
	qqq(i)= amin1( qqq(i), qmax )
	qqq(i)= amax1( qqq(i), qmin )
	www(i)  = www(i)   + qqq(i)/(poros(i)  *zdepth(i)  /dtt)
	www(i+1)= www(i+1) - qqq(i)/(poros(i+1)*zdepth(i+1)/dtt)

c...	qqq(i): fluxo base camada i (+downward) = 
c	qdowrd,qupwrd(i+1): fluxo topo camada i+1 	
c	if (qqq(i).gt.0.) qdowrd(i+1) = qdowrd(i+1) + qqq(i)
c	if (qqq(i).lt.0.) qupwrd(i+1) = qupwrd(i+1) + qqq(i)	


 3000 continue


C----------------------------------------------------------------------
c      calculation of gravitationally driven drainage (qng) from
c      w(n): taken as an integral of time varying conductivity;
c      addition of liston baseflow term to original qng to insure
c      flow in dry season (modified liston baseflow constant
c      scaled by available water)
C
C     QNG: equation of q3g (SE 86)
C
C-----------------------------------------------------------------------
C

	pows= 2.*bee(nlayer)+2.
	qng= temw(nlayer)**(-pows) +
     & satco(nlayer)/zdepth(nlayer)/poros(nlayer)*slope*pows*dtt
	qng= qng ** (1./pows)
	qng= -( 1./qng-www(nlayer) )* poros(nlayer) * zdepth(nlayer)/dtt

cHRDez2008... ajuste coeficiente de Liston
c_original_1994      qng = qng + 0.002 * poros(nlayer)*zdepth(nlayer) * 0.5 / 86400. 
c     &      * www (nlayer)
cHR...  kb = escoamento de base se W3=1 ( em m/d)

      qng = qng + xkb/1000. * poros(nlayer)*zdepth(nlayer)/86400. 
     &      * www (nlayer)

	qng= amax1( 0., qng)
	qng= amin1( qng, www(nlayer)*poros(nlayer)*zdepth(nlayer)/dtt)

cH...	teste de difusao interna
c	if (jqng.eq.0) qng = 0.

	www(nlayer)= www(nlayer)- 
     &               qng*dtt/(poros(nlayer)*zdepth(nlayer))


	roff= roff + qng*dtt

	do 401 i= 1, nlayer			! arredondamento ...
	excess= amax1( 0., (www(i)-1.))
	www(i)= www(i) - excess
	roff= roff + excess*poros(i)*zdepth(i)
 401    continue

C -----------------------------------------------------------------
C       prevent negative values of soil wetness
C -----------------------------------------------------------------

	do 402 i = 1 , nlayer-1
	deficit = amax1 (0.,(1.e-12 - www(i)))
	www(i) = www(i) + deficit
	www(i+1) = www(i+1) - deficit * zdepth(i)/zdepth(i+1)
 402    continue
	www(nlayer) = amax1 (www(nlayer),1.e-12)

c... diagnostico escoamento de agua do solo

      d10 = dtt * 1000.
cH      write(itmp3,'(i8.8,50(1x,f10.3))')nymd,(qupwrd (i)*d10,i=1,nlayer)
cH      write(itmp4,'(i8.8,50(1x,f10.5))')nymd,(qdowrd (i)*d10,i=1,nlayer)
c      write(itmp5,'(i8.8,50(1x,e9.3))')nymd,(qhorton(i)*d10,i=1,nlayer)
      
	return
	end

c=======================================================================
	subroutine retec (dpdw, jhort, i)
c=======================================================================
c
c..	output: cdpdw (derivada curva retencao) dpsi / dw
c
c----------------------------------------------------------------------
	include 'comsibc.h'

c...
      j = i + 1	
      
      if (i.gt.1) then
      w0 = amin1(www(i-1),1.)
      w0 = amax1(www(i-1),0.05)
      endif    
      w1 = amin1(www(i),1.)
      w1 = amax1(www(i),0.05)
      w2 = amin1(www(j),1.)
      w2 = amax1(www(j),0.05)   
c...	jhorton = 1  (calcula proximo saturacao; Runoff Horton)
      if (jhort.eq.1) then
      w1 = 1.0
      w2 = 0.95
      endif

      psi1 = phsat(i)* (w1**(-bee(i)))
      psi2 = phsat(j)* (w2**(-bee(j)))      

c... calculo método original SiB2 SE-96
	if (jdpsi.eq.1) then

	wmax= amax1 (www(i), www(i+1))
	wmax= amax1 ( amin1(wmax,1.0),  0.05)
	pmax= wmax**(-bee(i))
	
	if(i.eq.1) then
      wmin= (pmax-2./(phsat(1)*(zdepth(1)+2.*zdepth(2)+zdepth(3))))
ch    wmin= (pmax-2./(phsat(1)*(2.*zdepth(1)+zdepth(2))))
      wmin= amin1(w1,w2,wmin)
      wmin= amax1(wmin,1.0e-02)
	endif
	if(i.gt.1) then
      wmin=(pmax-2./(phsat(i)*(zdepth(i-1)+2.*zdepth(i)+zdepth(i+1))))
      wmin= amin1(w0,w1,w2,wmin)
      wmin= amax1(wmin,1.0e-02)
	endif
	if (wmin.eq.wmax) then
      wmin= (pmax-1./(phsat(i)*(zdepth(i-1)+2.*zdepth(i)+zdepth(i+1))))
      wmin= amax1(wmin,1.0e-03)
	endif
	pmin= wmin**(-bee(i))
	dpdw= phsat(i) * ( pmax-pmin )/( wmax-wmin )
	return
	endif
	
c...  calculo derivada psi(W) nas camada i,i+1 - CH-78 

      dpdw1 = - bee(i) * phsat(i) / (w1 ** (bee(i) + 1.))
      dpdw2 = - bee(j) * phsat(j) / (w2 ** (bee(j) + 1.))

      if (jdpsi.eq.2) then	! media ponderada espessura	
      dpdw = ( dpdw1  * zdepth(i) +  dpdw2  * zdepth(i+1) ) /
     &      ( zdepth(i+1) + zdepth(i) ) 
      endif	
c...	
      if (jdpsi.eq.3) then	! media geometrica	
      dpdw = sqrt( dpdw1 * dpdw2 )
      endif	
c
      if (jdpsi.eq.4) then	! media aritmetica	
      dpdw = ( dpdw1 + dpdw2 )/2.
      endif	

	return
	end

c====================================================================
	subroutine hydcon (avk, i)	
c====================================================================
c
c     avk (condutividade hidraulica segmento camadas i,i+1)
c
c--------------------------------------------------------------------
	include 'comsibc.h'

      j = i + 1	
      w1 = amin1(www(i),1.)
      w1 = amax1(www(i),0.05)
      w2 = amin1(www(j),1.)
      w2 = amax1(www(j),0.05)
      
      psi1 = phsat(i)* (w1**(-bee(i)))
      psi2 = phsat(j)* (w2**(-bee(j)))      
      avk1 = satco(i)* (w1**(2.*bee(i)+3.))      	
      avk2 = satco(j)* (w2**(2.*bee(j)+3.))
c...
      if (jkcon.eq.1) then	! metodo ME-82
      rsame= 0.
      avb = (bee(i) + bee(j)) / 2.            	
      div= psi2 - psi1
      if ( abs(div) .lt. 1.e-6 ) rsame=1.
      avk = (psi1*avk1 - psi2*avk2) / 
     &      ( ( 1. +3./avb ) * div + rsame )       

      avkmin = amin1 (avk1, avk2)
      avk    = amax1 (avk, avkmin)
      avkmax = amax1 (avk1, avk2 )
      avk    = amin1 (avk, avkmax )
      endif
c...
      if (jkcon.eq.2) then	! media ponderada espessura	
      avk = ( avk1  * zdepth(i) +  avk2  * zdepth(i+1) ) /
     &      ( zdepth(i+1) + zdepth(i) ) 
      endif	
c...	
      if (jkcon.eq.3) then	! media geometrica	
      avk = sqrt( avk1 * avk2 )
      endif	
c
      if (jkcon.eq.4) then	! media aritmetica	
      avk = ( avk1 + avk2 )/2.
      endif	


c----------------------------------------------------------------
c      conductivites and baseflow reduced when temperature drops
c      below freezing
C----------------------------------------------------------------
c
	tsnow = amin1( tf-0.01,tg)
	tgs = tsnow * areas + tg* (1. - areas)
	ts = tgs * (2 - j)  + td*(j-1)
	props = ( ts - (tf - 10.) ) / 10.
	props = amax1(0.05,amin1(1.0,props))
	avk = avk * props
	qng = qng * props

	return
	end
c
c=======================================================================
	subroutine tridia2 (n, d, c, a, b)
c=======================================================================
c
c  	HR  algoritmo original 1996, corrigido 2009
c   
c             solution of linear system of n x n dimension
c             for a tridiagonal matrix
c             d    : independent coefficients (input)
c                         vector solution      (output)
c             b  : main diagonal coefs
c             c   :  suprior diagonal coefs
c             a   : inferior diagonal coefs
c--------------------------------------------------------------

	dimension c(n+1), a(n+1), b(n+1), d(n+1)

	c(1) = c(1)/b(1)
	d(1) = d(1)/b(1)
	
	do 10 i = 2, (n-1), 1
	ii = (i-1)
       
	b(i) = b(i) - c(ii)*a(i)
	
	if (i .eq. n) go to 10
	c(i) = c(i) / b(i)		! calculado
   10   d(i) = (d(i) - d(ii)*a(i)) / b(i)

	do 20 k = 1, (n-1), 1
	i = n -k		! back substitution
   20	d(i) = d(i) - c(i) * d(i+1)

	return
	end
c=======================================================================
	subroutine eqlin (ifi, inum, n, d, sup, sub, diag)
c=======================================================================
c	solução sistema eqs lineares m x n por metodo iterativo 
c	HRocha Jan 2009
c	inum =  1 (método de Jacobi)
c		2 (método de Gauss-Seidel)
c
c	elements of soil multi-layer scheme in SiB:
c             d     : independent coefficients (inputs)
c			 vector solution (output)
c             diag  : main diagonal coefs
c             sup   :  suprior diagonal coefs
c             sub   : inferior diagonal coefs
c             
c--------------------------------------------------------------
	parameter (nmax=30, mmax=30, kmax = 100)
	real d(n), sup(n), sub(n), diag(n)
	real a(mmax,nmax), b(mmax), x(nmax,2), dif(nmax)
	parameter (eps=1.e-06,itmax=50)	! critérios convergencia
	character aconv(3)*20
	logical conv

	do 1 k= 1,3
   1	aconv(k) = ' '

	m = n		! este caso matriz n x n

	do 10 i= 1,n
  	x(i,2) = d(i)	! cond.inicial column vector X(n)
  	dif (i) = 0.
	do 20 j= 1,m
   20   a(j,i) = 0.	! matrix A(m,n)
   10   continue

c...	atribuicao matriz eqs Jacobi c/ input esquema soil multi-layer

	a(1,1)   = diag(1)
	a(1,2)   = sup(1)
	b(1)     = d(1)
	a(m,n-1) = sub(n)
	a(m,n)   = diag(n)
	b(m)     = d(n)

c	inserir aqui goto caso matriz 2x2

	do 30 i= 2,m-1
	a(i,i-1) = sub(i)
	a(i,i)   = diag(i)
   	a(i,i+1) = sup(i)
   30  	b(i)     = d(i)

	write(ifi,*) ' Eqlin: sistema montado'
      write(ifi,'(a3,a10,30(1x,a2,i2,5x))') 'i',' b ',('a_',i,i=1,n)
	do i = 1, m
        write(ifi,'(i3,30(1x,e9.3))') i, b(i), (a(i,j),j=1,n)
	enddo

c... 	solução do sistema
	it = 0
	conv = .false.

	do 35 i = 1,n
   	if (a(i,i).eq.0.)  then
   	conv = .true.
   	aconv(1)= ' a(i,i)=0'
   	endif
   35  	continue

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
	do while (.not.conv)

	do 50 i= 1, n		! recupera passo anterior
	x(i,1) = x(i,2)	
   50	x(i,2) = 0.
c----------------------------------------------------------------------
	do 56 i= 1, n
	
	do 60 j= 1, (i-1)
   	if(inum.eq.2) x(i,2) = x(i,2) + a(i,j)*x(j,2)	!G-Seidel
   60	if(inum.eq.1) x(i,2) = x(i,2) + a(i,j)*x(j,1)	!Jacobi

	do 65 j= (i+1), n
   65	x(i,2) = x(i,2) + a(i,j)*x(j,1)

   	x(i,2) = (b(i)-x(i,2))/a(i,i)

   56	continue
c----------------------------------------------------------------------

c       if (it.eq.0) write(ifi,'(a3,50(1x,a4,i3,1x,a4,i3))') 
c     &   'IT',  ( 'xt0_',i,  'xt1_',i   , i= 1,n )      
   
c       write(ifi,'(i3,50(1x,e7.1))') it, (x(i,1),x(i,2),i=1,n)   

	isum = 0
	do 58 i= 1, n
	if (x(i,1).ne.0.) then
	dif(i) = abs(x(i,2)-x(i,1))/x(i,1)
	if (dif(i).lt.eps) isum = isum + 1
	endif
   58	d(i) = x(i,2)

	if (isum.eq.n) then
	conv = .true. 
	aconv(2) = ' eps<0 '
	endif
	
	it = it + 1		
	if (it.eq.itmax) then
	conv = .true.
	aconv(3) = ' it=itmax '
	endif
	
      	end do
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	

	write(ifi,*)

      write(ifi,'(4a17)') 'Convergencia:', (aconv(i),i=1,3)
      write(ifi,'(a3,5a13,i3,a20)') 'i','x(i,1)','x(i,2)','d(i)',
     &            'd(i)_mm/d','it final=',it,' sistema resolvido'
	do i = 1, n
        write(ifi,'(i3,4(1x,e12.3))')
     &   i, x(i,1), x(i,2), d(i), d(i)*1000.*3600*24.
	enddo
      
        write(ifi,*)
	
	return
	end   
   
   
