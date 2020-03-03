!=======================================================================
!                                                                       
      subroutine updat2 
!                                                                       
!=======================================================================
!                                                                       
!     updating of all prognostic variables.                             
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     subroutines called   : updat2                                     
!     ------------------     snow2                                      
!                            run2n                                      
!                                                                       
!++++++++++++++++++++++++++++++output from this block+++++++++++++++++++
!                                                                       
!       dtc            canopy temperature increment (K)                 
!       dtd            deep soil temperature increment (K)              
!       dtg            ground surface temperature increment (K)         
!       www(3)         ground wetness                                   
!       capac(2)       canopy/ground liquid interception store (m)      
!       snoww(2)       canopy/ground snow interception store (m)        
!       roff           runoff (m)                                       
!       etmass (fws)   evapotranspiration (mm)                          
!       hflux (fss)    sensible heat flux (w m-2)                       
!                                                                       
!++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++
!                                                                       
!       ecmass         canopy evapotranspiration (mm)                   
!       egmass         ground evapotranspiration (mm)                   
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      implicit none
      real (kind=8) :: extra2p(nlayer)
      !
      real (kind=8) :: ectdif
      real (kind=8) :: ectil
      real (kind=8) :: ectnew
      real (kind=8) :: egsdif
      real (kind=8) :: extrak
      real (kind=8) :: facks
      real (kind=8) :: facl
      integer :: il
      integer :: iveg
      real (kind=8) :: qm = 0d0
      real (kind=8) :: rsnow
      real (kind=8) :: th = 0d0

!       write(98,*)' updat2: begin'                                     
!       write(98,*)' updat2: to call snow1'                             
                                                                        
                                                                        
      call snow1 
                                                                        
!       write(98,*)' updat2: snow1 passed'                              
!                                                                       
!                                                                       
!---------------------------------------------------------------------- 
!    interception losses.                                               
!    evaporation losses are expressed in j m-2 : when divided by        
!    ( hlat*1000.) loss is in m m-2. mass terms are in kg m-2 dt-1      
!    interception and drainage treated in inter2.                       
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
                                                                        
!       write(98,*)' updat2: interception losses begin'                 
                                                                        
      rsnow = snoww(1)/(snoww(1)+capac(1)+1.e-10) 
      facks = 1. + rsnow * ( snofac-1. ) 
      if ( (ect+eci) .gt. 0.) go to 100 
      eci = ect+eci 
      ect = 0. 
      facks = 1. / facks 
  100 capac(1) = capac(1) - ( 1.-rsnow )*eci*facks/hlat/1.e3 
      snoww(1) = snoww(1) -      rsnow  *eci*facks/hlat/1.e3 
      ecmass = eci*facks / hlat 
!                                                                       
      rsnow = snoww(2)/(snoww(2)+capac(2)+1.e-10) 
      facks = 1. + rsnow * ( snofac-1. ) 
      if ( (egs+egi) .gt. 0. ) go to 200 
      egi = egs+egi 
      egs = 0. 
      facks = 1. / facks 
  200 capac(2) = capac(2) - ( 1.-rsnow )*egi*facks/hlat/1.e3 
      snoww(2) = snoww(2) -      rsnow  *egi*facks/hlat/1.e3 
      egmass = egi*facks / hlat 
                                                                        
!       write(98,*)' updat2: interception losses passed'                
                                                                        
!                                                                       
!---------------------------------------------------------------------- 
!    dumping of small capac values onto soil surface store              
!---------------------------------------------------------------------- 
!                                                                       
!       write(98,*)' updat2: dump snow to w1 begin'                     
                                                                        
                                                                        
      do 1000 iveg = 1, 2 
         if ( (snoww(iveg)+capac(iveg)) .gt. 0.00001 ) go to 300 
    !new www(1) = www(1) + (snoww(iveg)+capac(iveg)) / ( poros*zdepth(1) ) 
         www(1) = www(1) + (snoww(iveg)+capac(iveg))/(poros(1)*zdepth(1) ) 
         capac(iveg) = 0. 
         snoww(iveg) = 0. 
  300 continue 
 1000 continue 
                                                                        
!       write(98,*)' updat2: dump snow to w1 passed'                    
                                                                        
!                                                                       
!---------------------------------------------------------------------- 
!    snowmelt / refreeze calculation                                    
!---------------------------------------------------------------------- 
!                                                                       
                                                                        
!       write(98,*)' updat2: call snow2  begin'                         
                                                                        
      call snow2 
                                                                        
!       write(98,*)' updat2: call snow2  passed'                        
                                                                        
!                                                                       
!---------------------------------------------------------------------- 
!    evapotranspiration losses,                                         
!    extraction of transpiration loss from root zone, soil evaporation. 
!                                                                       
!      ect         (e-dc)  : equation (5,6), SE-86                      
!      egs         (e-s)   : equation (5)  , SE-86                      
!---------------------------------------------------------------------- 
                                                                        
!h.. preferential multilayer extraction                                 
      ectnew = 0. 
                                                                        
!      write(98,'22f8.7') (www(i),i= 1, nlayer)                         
      do 40 il= 1, nlayer 
         if (extfrac(il).gt.0.) then 
            facl    = 1./ hlat / 1.e3 / (poros(il)*zdepth(il)) 
            if (il.eq.1) then 
!     write(98,*)' updat2: prefer extraction il=1'                      
               extrak     = amin1 (www(1), egs*extfrac(1)*facl) 
               egsdif     = egs - extrak/facl 
               egs        = extrak/facl 
               hg         = hg + egsdif 
               egmass     = egmass + egs/hlat 
               www(1)     = www(1) - egs*facl 
            else 
!     write(98,*)' updat2: prefer extraction il=',il                    
               ectil     = ect * facl * extfrac(il) 
               extrak    = amin1 ( www(il), ectil ) 
               ectdif    = (ectil - extrak)/facl 
               ectnew    = ectnew + extrak/facl 
               hc        = hc + ectdif 
               ecmass    = ecmass + extrak/facl/hlat 
               www(il)   = www(il) - extrak 
            endif
         endif
                                                                        
!HR..211108                                                             
         extra2p(il) = extrak 
                                                                                      
   40 continue 
                                                                        
      ect = ectnew 
                                                                                      
!HR..211108                                                             
                                                                        
!      write(98,'2(11(1x,f8.7))')(www(il),il=1,nlayer),                 
!     &      (1.e+02*extra2p(il),il= 1, nlayer)                         
                                                                              
!h ...                                                                  
!-----------------------------------------------------------------------
!    calculation of total moisture and sensible heat fluxes from surface
!-----------------------------------------------------------------------
!                                                                       
      etmass = ecmass + egmass 
      hflux  = (hc+hg) / dtt 
!                                                                       
!---------------------------------------------------------------------- 
!    calculation of interflow, infiltration excess and loss to          
!    groundwater .  all losses are assigned to variable 'roff' .        
!---------------------------------------------------------------------- 
      call run2n 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!    update of temperatures and pbl variables. note that tc and tg      
!    are modified in interc as a result of precipitation inputs.        
!    update of interception stores.                                     
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      tc  = tc + dtc 
      tg  = tg + dtg 
      td  = td + dtd 
      th  = th + dth 
      qm  = qm + dqm 
!                                                                       
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine inter2 
!                                                                       
!=======================================================================
!                                                                       
!     calculation of  interception and drainage of rainfall and snow    
!     incorporating effects of patchy snow cover and temperature        
!     adjustments.                                                      
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     (1) non-uniform precipitation                                     
!         convective ppn. is described by area-intensity                
!         relationship :-                                               
!                                                                       
!                   f(x) = a*exp(-b*x)+c                                
!                                                                       
!         throughfall, interception and infiltration                    
!         excess are functional on this relationship                    
!         and proportion of large-scale ppn.                            
!         reference: sato et al.(1989b), appendix.                      
!                                                                       
!     (2) reorganisation of snowmelt and rain-freeze procedures.        
!               subroutine adjust                                       
!                                                                       
!     (3) additional calculation for partial snow-cover case.           
!               subroutine patchs                                       
!                                                                       
!     (4) reorganisation of overland flow.                              
!         reference: SA-89B, appendix.                                  
!                                                                       
!     (5) modified calaculation of soil heat capacity and               
!         conductivity.                                                 
!                                                                       
!=======================================================================
!                                                                       
!     subroutines in this block : snow1                                 
!     -------------------------   adjust                                
!                                 patchs                                
!                                 snow1                                 
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       roff           runoff (m)                                       
!       tc             canopy temperature (K)                           
!       tg             ground surface temperature (K)                   
!       www(1)         ground wetness of surface layer                  
!       capac(2)       canopy/ground liquid interception store (m)      
!       snoww(2)       canopy/ground snow interception store (m)        
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      implicit none
      !                                                                       
      ! dimension  pcoefs(2,2) 
      ! data pcoefs(1,1)/ 20. /, pcoefs(1,2)/ .206e-8 /,                  &
      !&     pcoefs(2,1)/ 0.0001 /, pcoefs(2,2)/ 0.9999 /, bp /20. /      

      real (kind=8) :: pcoefs(2,2)
      real (kind=8) :: bp
      real (kind=8) :: aa
      real (kind=8) :: ap
      real (kind=8) :: arg
      real (kind=8) :: bb
      real (kind=8) :: capacp
      real (kind=8) :: chiv
      real (kind=8) :: cp
      real (kind=8) :: equdep
      real (kind=8) :: fpi
      integer :: iveg
      real (kind=8) :: p0
      real (kind=8) :: pinf
      real (kind=8) :: realc
      real (kind=8) :: realg
      real (kind=8) :: roffo
      real (kind=8) :: shcap
      real (kind=8) :: slamda
      real (kind=8) :: snowwp
      real (kind=8) :: spechc
      real (kind=8) :: tex
      real (kind=8) :: thru
      real (kind=8) :: totalp
      real (kind=8) :: ts
      real (kind=8) :: tti
      real (kind=8) :: xs
      real (kind=8) :: xsc
      real (kind=8) :: xss
      real (kind=8) :: zload
      ! 
      pcoefs(1,1) = 20.0
      pcoefs(1,2) = 0.206e-8
      pcoefs(2,1) = 0.0001
      pcoefs(2,2) = 0.9999
      bp = 20.      
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      call snow1 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     prec ( pi-x )   : equation (c.3), SA-89B                          
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      ap = pcoefs(2,1) 
      cp = pcoefs(2,2) 
      totalp = ppc + ppl 
      if( snoww(1) .gt. 0. .or. snoww(2) .gt. 0. .or. tm .lt. tf )      &
     &     ppc = 0.                                                     
      ppl = totalp - ppc 
      if(totalp.lt.1.e-8) go to 100 
      ap = ppc/totalp * pcoefs(1,1) + ppl/totalp * pcoefs(2,1) 
      cp = ppc/totalp * pcoefs(1,2) + ppl/totalp * pcoefs(2,2) 
  100 continue
!
!sml...
      croff = 0. ! surface runoff
      cthru = 0. ! incoming thrufall to soil surface
!sml...
      roff = 0.
      thru = 0.
      fpi = 0.
!
!---------------------------------------------------------------------- 
!     heat capacity of the soil, as used in force-restore heat flux     
!     description. dependence of csoil on porosity and wetness is       
!     based on CS-81.                                                   
!---------------------------------------------------------------------- 
!                                                                       
!sml...	porosity is taken of the layer 2                                
                                                                        
      slamda = ( 1.5*(1.-poros(2)) + 1.3*www(1)*poros(2) ) /            &
     &         ( 0.75 + 0.65*poros(2) - 0.4*www(1)*poros(2) ) * 0.4186  
      shcap  = ( 0.5*(1.-poros(2)) + www(1)*poros(2) ) * 4.186 * 1.e6 
      csoil  = sqrt( slamda * shcap * 86400./pie ) / 2. 
!                                                                       
!---------------------------------------------------------------------- 
!     input precipitation is given in mm, converted to m to give p0.    
!---------------------------------------------------------------------- 
!                                                                       
      p0 = totalp * 0.001 
!                                                                       
      do 1000 iveg = 1, 2
         
         realc = 2. - iveg 
         realg = iveg - 1. 
!                                                                       
         capacp = capac(iveg) 
         snowwp = snoww(iveg) 
!                                                                       
         xsc = amax1(0., capac(iveg) - satcap(iveg) ) 
         capac(iveg) = capac(iveg) - xsc 
         xss = amax1(0., snoww(iveg) - satcap(iveg) ) * realc 
         snoww(iveg) = snoww(iveg) - xss 
         roff = roff + xsc + xss 
!                                                                       
         spechc = amin1( 0.05, ( capac(iveg) + snoww(iveg) ) ) * cw    &
     &                 + realc * zlt * clai + realg * csoil                       
         ts = tc * realc + tg * realg 
!                                                                       
!---------------------------------------------------------------------- 
!     proportional saturated area (xs) and leaf drainage(tex)           
!                                                                       
!     tex ( d-c )     : equation (c.8), SA-89B                          
!     xs  ( x-s )     : equation (c.7), SA-89B                          
!     tex ( d-c )     : equation (c.8), SA-89B                          
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
         chiv = chil 
         if ( abs(chiv) .le. 0.01 ) chiv = 0.01 
         aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv 
         bb = 0.877 * ( 1. - 2. * aa ) 
         exrain = aa + bb 
!                                                                       
         zload = capac(iveg) + snoww(iveg) 
         fpi = ( 1.-exp( - exrain*zlt/vcover ) )*vcover*realc + realg 
         tti = p0 * ( 1.-fpi ) 
         xs = 1. 
         if ( p0 .lt. 1.e-9 ) go to 200 
         arg =  ( satcap(iveg)-zload )/( p0*fpi*ap ) -cp/ap 
         if ( arg .lt. 1.e-9 ) go to 200 
         xs = -1./bp * log( arg ) 
         xs = amin1( xs, 1. ) 
         xs = amax1( xs, 0. ) 
  200    tex = p0*fpi * ( ap/bp*( 1.- exp( -bp*xs )) + cp*xs ) -       &
     &         ( satcap(iveg) - zload ) * xs                               
         tex = amax1( tex, 0. ) 
!                                                                       
!---------------------------------------------------------------------- 
!     total throughfall (thru) and store augmentation                   
!---------------------------------------------------------------------- 
!                                                                       
         if ( iveg .eq. 2 ) go to 300 
!                                                                       
         thru = tti + tex 
         pinf = p0 - thru 
         if( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf 
         if( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf 
!                                                                       
         call adjust ( tc, spechc, capacp, snowwp, iveg ) 
!                                                                       
         p0 = thru 
         go to 700 
!                                                                       
  300    continue 
!                                                                       
         if ( tg .gt. tf .and. snoww(2) .gt. 0. ) then 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
            call patchs ( p0 ) 
            go to 700 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
         endif
!                                                                       
         thru = tti + tex 
         if ( tg .le. tf .or. tm .le. tf ) thru = 0. 
         pinf = p0 - thru 
         if( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf 
         if( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf 
         if( tm .le. tf ) go to 500 
                                                                        
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     instantaneous overland flow contribution ( roff )                 
!                                                                       
!     roff( r-i )     : equation (c.13), SA-89B                         
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
         equdep = satco(1) * dtt 
!                                                                       
         xs = 1. 
         if ( thru .lt. 1.e-9 ) go to 400 
         arg = equdep / ( thru * ap ) -cp/ap 
         if ( arg .lt. 1.e-9 ) go to 400 
         xs = -1./bp * log( arg ) 
         xs = amin1( xs, 1. ) 
         xs = amax1( xs, 0. ) 
  400    roffo = thru * ( ap/bp * ( 1.-exp( -bp*xs )) + cp*xs )        &
     &           -equdep*xs                                                 
         roffo = amax1 ( roffo, 0. ) 
                                                                        
!HR..                                                                   
!      write(98,'(20a10)')                                              
!    & 'xs','P','D','P-D','Dc','Dd','Ri','R','Pi','q0' (no Main)        
!      write(98,'(30(1x,f10.5))')                                       
!     & xs,p0*1000.,thru*1000.,pinf*1000.,tex*1000.,                    
!     & tti*1000.,roffo*1000.,roff*1000.,                               
!     & amax1 (0., (1.0 - www(1))*zdepth(1)*poros(1) )*1000.,           
!     & q0*1000.*dtt,                                                   
!     & (www(i),i=1,nlayer)                                             
                                                                        
!....................................................................   
!HR..Jan2008	infiltration/surface runoff options: iinf =                
!....................................................................   
!                                                                       
!  =1  capac. infiltracao = disponibilidade top layer                   
!                                                                       
!  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente o
!  =6  id. 2, frente de onda cessa encontro saturacao                   
!                                                                       
!  =3  all thrufall infiltrates; infiltracao frente onda;               
!  =5  id. 3, frente de onda cessa encontro saturacao                   
!                                                                       
!  =4  all thrufall infiltrates; frente onda limitada R-Horton          
!	qstar (f* de EN-89)                                                   
!                                                                       
                                                                        
         if (iinf.eq.1) then 
            q0 = amax1 (0., (1.0 - www(1))*zdepth(1)*poros(1) )                     
            q0 = ( amin1 (q0, thru-roffo) ) / dtt                 ! m/s
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
                                                                        
!====================================================================   
                                                                        
  500 continue 
!                                                                       
      call adjust ( tg, spechc, capacp, snowwp, iveg ) 
!                                                                       
  700 continue 
!                                                                       
 1000 continue 
!                                                                       
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine begtem 
!                                                                       
!-----------------------------------------------------------------------
!     core routine: calculation of canopy and ground temperature        
!     increments over time step, fluxes derived.                        
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!                                                                       
!     subroutinescalled : snow1                                         
!     -----------------                                                 
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       rstfac(2)      soil moisture stress factor                      
!       rsoil          soil surface resistance (s m-1)                  
!       hr             soil surface relative humidity                   
!       wc             canopy wetness fraction                          
!       wg             ground wetness fraction                          
!       ccx            canopy heat capacity (j m-2 k-1)                 
!       cg             ground heat capacity (j m-2 k-1)                 
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
!---------------------------------------------------------------------- 
      use comsibc
      implicit none  
      real (kind=8) :: x
      !
      real (kind=8) :: argg
      real (kind=8) :: fac
      integer :: i
      real (kind=8) :: phroot
      real (kind=8) :: psit
      real (kind=8) :: rsnow
      real (kind=8) :: rstfac2p
      real (kind=8) :: tsnow
      real (kind=8) :: e
      real (kind=8) :: ge
!                                                                       
!---------------------------------------------------------------------- 
!     e(x) is vapour pressure in mbars as a function of temperature     
!     ge(x) is d e(x) / d ( temp )                                      
!---------------------------------------------------------------------- 
!                                                                       
      e(x) = exp( 21.18123 - 5418. / x ) / .622 
      ge(x) = exp( 21.18123 - 5418. / x ) * 5418.                       &
     &        / (x*x) / .622                                            
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      call snow1 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      hlat     = ( 3150.19 - 2.378 * tm ) * 1000. 
      psy      = cpair / hlat * psur / .622 
      snofac   = hlat/ ( hlat + snomel / 1.e3 ) 
!                                                                       
!---------------------------------------------------------------------- 
!    calculation of canopy and ground heat capacities.                  
!    n.b. this specification does not necessarily conserve energy when  
!    dealing with very large snowpacks.                                 
!---------------------------------------------------------------------- 
!                                                                       
      ccx = zlt * clai + (snoww(1)+capac(1))*cw 
      cg  = csoil + amin1 ( 0.05, (snoww(2)+capac(2)) ) * cw 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!---------------------------------------------------------------------- 
!      calculation of ground surface temperature and wetness fractions  
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      tsnow = amin1 ( tf-0.01, tg ) 
      rsnow = snoww(2) / (snoww(2)+capac(2)+1.e-10) 
!                                                                       
      tgs = tsnow*areas + tg*(1.-areas) 
!                                                                       
      etc   = e(tc) 
      etgs  = e(tgs) 
      getc  = ge(tc) 
      getgs = ge(tgs) 
!                                                                       
      wc = amin1( 1.,( capac(1) + snoww(1))/satcap(1) ) 
      wg = amax1( 0.,  capac(2)/satcap(2) )*0.25 
!                                                                       
!-----------------------------------------------------------------------
!     calculation of soil moisture stress factor.                       
!     average soil moisture potential in root zone (layer-2) used as    
!     source for transpiration.                                         
!                                                                       
!      phroot      (psi-r) : equation (47) , SE-86, (22), SE-89         
!      rstfac(2)  f(psi-l) :    "     (C17), SE-96                      
!-----------------------------------------------------------------------
!                                                                       
!h      phroot = phsath * amax1( 0.02, www(2) ) ** ( - beeh )           
!h      phroot = amax1 ( phroot, -2.e3 )                                
!h...                                                                   
!new..equation 22 (SE-89) used as potential in root zone                
!new    rdepth= 0.                                                      
!new    phroot= 0.                                                      
!new    do 40 i= 1, nlayer                                              
!new    if (rdepth.gt.rootd) goto 40                                    
!new    phroot= phroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee)) 
!new    rdepth= rdepth + zdepth(i)                                      
!new 40 continue                                                        
!new    phroot = amax1 ( phroot*phsat, -2.e3 )                          
!new ..                                                                 
!tes..equation 22 (SE-89) used as potential in root zone                
!	wdepth= 0.                                                            
!	whroot= 0.                                                            
!	!write(98,*)nymd                                                      
!	do 40 i= 2, nlayer                                                    
!                                                                       
!	if (wdepth.gt.rootd) goto 40                                          
!new    whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee)) 
!      whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee(i))
!ml05_nov      whroot= whroot + extfrac(i)*((amax1(0.02,www(i)))**(-bee(
!  	wdepth= wdepth + zdepth(i)                                          
!                                                                       
!	if (nymd.eq.40101512.or.nymd.eq.40041512) then                        
!	write (98,*)                                                          
!     &  ' i,zdepth(i),www(i),psi(i),whroot,wdepth'  	                  
!        write(*,*)   ' aqui nymd 40101512 ou 40041512'                 
!        write (98,*)i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)),whroot
!     &	wdepth                                                          
!	endif                                                                 
!                                                                       
! 40     continue                                                       
!	whroot = amax1 ( whroot*phsat(3), -2.e3 )                             
!                                                                       
!	phroot = whroot                                                       
!h...                                                                   
!	                                                                      
!       write(98,'(a15,2(1x,e10.4))')' phroot, whroot:', phroot, whroot 
!tes ..                                                                 
!                                                                       
!      rstfac(2) = 1./( 1 + exp( 0.02*( phc-phroot) ) )                 
!      rstfac(2) = amax1( 0.0001, rstfac(2) )                           
!      rstfac(2) = amin1( 1.,     rstfac(2) )                           
!HR alteracao feita dia 06_nov                                          
      phroot=0. 
      rstfac2p=0. 
!     write(98,'22f8.7') (www(i),i= 1, nlayer)                          
      do 40 i= 2, nlayer 
         phroot=phsat(i)*((amax1(0.02,www(i)))**(-bee(i))) 
         phroot=amax1(phroot,-2.e4) 
         rstfac2p = rstfac2p + extfrac(i)*(1./(1+exp(0.02*(phc-phroot)))) 
!	if (nymd.eq.40101512.or.nymd.eq.40041512) then                        
!	write (98,'a60')                                                      
!     &  ' i,zdepth(i),www(i),psi(i),phroot,rstfac2p,extfrac,fpsi'  	   
!        write(*,*)   ' aqui nymd 40101512 ou 40041512'                 
!        write (98,'i3,7f7.4')                                          
!     &  i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)),                  
!     &	phroot,rstfac2p, extfrac(i), (1./(1+exp(0.02*(phc-phroot))))    
!	endif                                                                 
                                                                        
   40 continue 
                                                                        
      rstfac(2)=rstfac2p
      rstfac(2) = amax1( 0.0001, rstfac(2) )
      rstfac(2) = amin1( 1.,     rstfac(2) )

!        write(98,'22f8.7') (www(i),i= 1, nlayer)                       
                                                                        
!	if (nymd.eq.40101512.or.nymd.eq.40041512)                             
!     &	write (98,*)rstfac(2),phroot                                    
                                                                        
!---------------------------------------------------------------------- 
!                                                                       
!      rsoil function from fit to FIFE-87 data.  soil surface layer     
!      relative humidity.                                               
!                                                                       
!      rsoil      (rsoil) : Heiser 1992 (personal communication)        
!      hr         (fh)    : equation (66) , SE-86                       
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      fac = amin1( www(1), 1. ) 
      fac = amax1( fac, 0.02  ) 
      rsoil =  amax1 (0.1, 694. - fac*1500.) + 23.6                    
                                                   !Cinthia_cerrado16Out
      !rsoil =  amax1 (0.1, 1001. - exp(fac*6.686)) 
!                                                                       
      psit = phsat(1) * fac ** (- bee(1) ) 
      argg = amax1(-10.,(psit*gx/461.5/tgs)) 
      hr = exp(argg) 
!                                                                       
      return 
      END                                           
!                                                                       
!====================================================================   
!                                                                       
      subroutine run2n 
!                                                                       
!     modified multi-layer scheme:  Humberto da Rocha 03/04/1996        
!                                                                       
!====================================================================   
!                                                                       
!HR..Dez2008	infiltration/surface runoff options: iinf =                
!....................................................................   
!                                                                       
!  =1  capac. infiltracao = disponibilidade top layer                   
!                                                                       
!  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente o
!  =6  id. 2, frente de onda cessa encontro saturacao                   
!                                                                       
!  =3  all thrufall infiltrates; frente onda;                           
!  =5  id. 3, frente de onda cessa encontro saturacao                   
!                                                                       
!  =4  all thrufall infiltrates; frente onda limitada R-Horton          
!	qstar (f* de EN-89)                                                   
!                                                                       
! ALL (see routine inter2 for q0 calculation)                           
!....................................................................   
                                                                        
      use comsibc
      implicit none  
!                                                                       
     !  real temw(nlayer), temwp(nlayer), temwpp(nlayer),                 &
     ! &  a(nlayer), b(nlayer), c(nlayer), d(nlayer),                     &
     ! &  da(nlayer), db(nlayer), dc(nlayer)                              
     !  logical wavend
!
      real (kind=8) :: temw(nlayer)
      real (kind=8) :: temwp(nlayer)
      real (kind=8) :: temwpp(nlayer)
      real (kind=8) :: a(nlayer)
      real (kind=8) :: b(nlayer)
      real (kind=8) :: c(nlayer)
      real (kind=8) :: d(nlayer)
      real (kind=8) :: da(nlayer)
      real (kind=8) :: db(nlayer)
      real (kind=8) :: dc(nlayer)
      logical :: wavend                                                       
      !dimension qdowrd(nlayer), qupwrd(nlayer), qhorton(nlayer)------
      real (kind=8) :: qdowrd(nlayer)
      real (kind=8) :: qupwrd(nlayer)
      real (kind=8) :: qhorton(nlayer)
      !---------------------------------------------------------------
      real (kind=8) :: avdif
      real (kind=8) :: avk
      real (kind=8) :: cs = 0d0
      real (kind=8) :: d10
      real (kind=8) :: deficit
      real (kind=8) :: dpdw
      real (kind=8) :: dpsidz
      real (kind=8) :: excess
      integer :: i
      real (kind=8) :: pows
      real (kind=8) :: q0now
      real (kind=8) :: qmax
      real (kind=8) :: qmin
      real (kind=8) :: qstar
      real (kind=8) :: wover
      real (kind=8) :: xni
      real (kind=8) :: zpond
      !
      do 1000 i= 1, nlayer 
         temw(i)= amax1( 0.03, www(i) ) 
         temwp(i)= temw(i) ** (-bee(i)) 
         temwpp(i)= amin1( 1., temw(i) ) ** (2.*bee(i)+3.) 
!H                                                                      
         qdowrd (i) = 0. !fluxos downward (drenagem rapida + difusiva),
                                          !i=1 (infiltracao superficie)
         qupwrd (i) = 0. !fluxos upward (capilar)
         qhorton(i) = 0. !infiltração maxima Hortoniana EN-89	 
 1000 continue 
                                                                        
!-----------------------------------------------------------------------
!	iinf  = 1                                                             
!-----------------------------------------------------------------------
      if (iinf.eq.1) then 
         www(1) =  www(1) + q0*dtt/(poros(1)*zdepth(1)) 
         q0 = 0.                
         qdowrd(1) = qdowrd(1) + q0 ! infiltração superficie (m/s)     
      endif 
!-----------------------------------------------------------------------
!	iinf  > 1                                                             
!-----------------------------------------------------------------------
      if (iinf.gt.1) then
         q0now = q0
         wavend = .false.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         if (q0now.gt.0.) then ! throughfall event                     
!.......................................................................
!	capacidade infiltração da camada i                                  
!.......................................................................
            do while (.not.wavend) 
               do 75 i = 1, nlayer 
!	write(*,*) ' do while (.not.wavend), E wavend = ', wavend             
!	pause                                                                 
                                                                        
!	jdpsi = 2                                                             
!	call retec ( jdpsi, dpdw, i)                                          
!	call hydcon (avk, i,temwp(i),temwp(i+1),temwpp(i),temwpp(i+1))        
!	avk = sqrt( satco(i)*satco(i+1) )                                     
!	qstar = - 2.*avk*(dpdw*(1.-www(i+1))/(zdepth(i)+zdepth(i+1))-1.)	     
               zpond = amax1(0., (1.-www(i)) * zdepth(i)*poros(i)) 
               call retec ( dpdw, 1, i)! dpsi/dw at saturation
                                       ! threshold p/ capac infiltracao 
               xni = dpdw / zdepth(i)
               qstar = satco(i)*( xni*www(i) + 1. - xni) ! infiltração
                                                         !      Horton 
               qstar = amin1 (qstar, 0.) ! only negative flux is
                                         ! downward eq(13) EN-89
               qstar = abs(qstar) ! physical drainage

               if (iinf.ne.4)                                          &
      &             qstar = amax1(cs * qstar, 1.e+20) ! infiltração                  
                           ! ilimitada, sem Runoff-Horton (iinf=2,6,3,5) 
                                      
               q0 = q0now ! oferta sempre>0                                        
               q0now = amin1( q0now, zpond/dtt, qstar) ! infiltrado >=0
               qdowrd (i) = qdowrd(i) + q0now ! diagnostico m/s                    
               qhorton(i) = qstar ! diagnostico m/s
                                  
               if (q0now.eq.0.) then ! há oferta (mas não infiltra em
                                     ! alguns casos iinf)
                  if (iinf.eq.4.or.iinf.eq.5.or.iinf.eq.6) wavend =  &
     &                 .true.
               endif

               www(i) = www(i) + q0now*dtt/(zdepth(i)*poros(i)) !
                                    !absorvido (ou não, se infiltrado=0)
               wover = amax1( 0., (www(i)-1.)) ! check arredon/to
               www(i)= www(i) - wover 
               roff= roff + wover*poros(i)*zdepth(i)
               
               q0now = amax1( 0., q0 - q0now ) ! atualiza oferta de
                                                       !infiltração
               if (q0now.eq.0..or.                                   &
     &              (q0now.gt.0..and.i.eq.nlayer)) then              
                  wavend =.true.
                  ! oferta cessou
                  ! onda atingiu camada profunda saturada
               endif
!	write(*,*) ' i layer,  q0now =' , i, q0now                            
!	qexcess =  amax1 (0., ( q0now -  amin1 ( q0now, qstar ) ) )           
!	roff = roff + qexcess*dtt	                                            
!	croff = croff + qexcess*dtt	                                          
   75          continue 
            end do       !  wavend = F
         endif           ! throughfall event
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         croff = croff + q0now*dtt !...excess to surface runoff
         roff  =  roff + q0now*dtt
!-----------------------------------------------------------------------
      endif              !  option iinf > 1
!---------------------------------------------------------------------- 
!                                                                       
!    calculation of inter-layer exchanges of water due to gravitation   
!    and matric gradient. the values of w(x) + dw(x) are used to calcula
!    the potential gradients between layers.                            
!    modified calculation of mean conductivies follows me-82, which     
!    reduces recharge flow to top layer                                 
!                                                                       
!    dpdw : estimated derivative of soil moisture potential with respect
!           to soil wetness. assumption of gravitational drainage used  
!           to estimate likely minimum wetness over the time step.      
!                                                                       
!     qqq (q    )  equation (61) , se 86     i = 1 , nlayer   (m/s)     
!             I , I + 1                                                 
!                                                                       
!      avk  (k)     equation (4.14), me 82                              
!             I , I + 1                                                 
!                                                                       
!-----------------------------------------------------------------------
      do 49 i= 1, nlayer 
         temw(i)= amax1( 0.03, www(i) )        ! W 
         temwp(i)= temw(i) ** (-bee(i))        ! Psi
         temwpp(i)= amin1( 1., temw(i) ) ** (2.*bee(i)+3.)
   49 continue 
!-------------------------------------------------------------------    
!	 Flow between soil layers, diffusion equation in multi-layer scheme   
!         backward implicit calculation, or Crank-Nicholson  method     
!         linear system of (nlayer-1 x nlayer -1 ) dimension            
!         tridiagonal matrix solution - Thomas algorithm                
!                                                                       
!	b = diagonal principal                                                
!	c = superior diagonal principal                                       
!	a = sub (inferior) diagonal principal                                 
!	d = coeficientes independentes (direita igualdade)                    
!				HRocha 15.12.96, atualizado 07.01.09                               
!-------------------------------------------------------------------    
                                                                        
!..	esquema discretização numérica ................................  
!	jesq = 1		! backward                                                  
!	jesq = 2		! Crank-Nicolson                                            
!	jesq = 3		! desativa difusão de agua                                 
                                                                        
!..	método calculo dpsi/dteta .....................................    
!	jdpsi = 1		!  SiB2 original                                           
!	jdpsi = 2		!  derivada CH-78 media ponderada espessura                
!       jdpsi = 3		!  derivada CH-78 media geometrica                   
!       jdpsi = 4		!  derivada CH-78 media aritmetica                   
                                                                        
!..	método calculo condutividade hidraulica .......................    
!	jkcon = 1		! SiB2 original ME-82                                      
!	jkcon = 2		! media ponderada espessura                                
!       jkcon = 3		! media geometrica                                   
!       jkcon = 4		! media aritmetica                                   
                                                                        
!...	método solução sistema eqs lineares                             
!	jsys=	1    matriz tridiagonal (HR-1996, revisado-2009)                
!		3    método iterativo                                               
!			3-definir jjgs = 1   método Jacobi                                 
!			3-definir jjgs = 2   método Gauss-Seidel                           
                                                                        
!...................................................................... 
      do 50 i= 1, nlayer-1 
!...	calculo dpsi/dw  ......................................            
         call retec (dpdw, 0, i) 
!...	calculo condutividade e difusividade hidraulica ...................
         call hydcon (avk, i) 
         avdif = avk * dpdw / poros(i) 
!...	valor inicial de qqq(i)                                            
         if (jqini.eq.0) qqq(i) = 0. 
         if (jqini.eq.1) then 
            dpsidz = phsat(i) * temwp(i) 
            dpsidz = dpsidz - phsat(i+1) * temwp(i+1) 
            dpsidz = 2.* dpsidz / (zdepth(i) + zdepth(i+1)) 
            qqq(i) = -avk * (dpsidz + 1.)
         endif
         jqini = -999
!... 	matriz multi-camada: esquema backward                             
         if (jesq.eq.1) then 
            a(i) = -2.*dtt*avdif / (zdepth(i) * (zdepth(i) +            &
     &        zdepth(i+1)))                                             
            b(i) =  2.*dtt*avdif / (zdepth(i) * zdepth(i+1)) + 1. 
            c(i) = -2.*dtt*avdif / (zdepth(i+1) * (zdepth(i) +          &
     &        zdepth(i+1)))
            d(i) = qqq(i)
            if (i.eq.1) a(i) = 0. 
            if (i.eq.nlayer-1) c(i) = 0. 
!        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))')   
!     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif',   
!     &   'log(avdif1)','log(avdif2)','log(avdif'                       
!        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))')       
!     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,  ! x 3
!     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,),          
!     &    log(avdif* 3.6e5)                                            
         endif
!...     matriz multi-camada: esquema Crank-Nicolson                    
         if (jesq.eq.2) then
            a(i) = -dtt*avdif / (zdepth(i) * (zdepth(i) + zdepth(i+1))) 
            b(i) =  1. + dtt*avdif / (zdepth(i) * zdepth(i+1)) 
            c(i) = -dtt*avdif / (zdepth(i+1) * (zdepth(i) +             &
     &        zdepth(i+1)))
            if (i.eq.1) a(i) = 0. 
            if (i.eq.nlayer-1) c(i) = 0. 
            da(i) = -a(i) 
            db(i) = 1. - dtt*avdif / (zdepth(i) * zdepth(i+1)) 
            dc(i) = -c(i) 
            if (i.ge.2.and.i.le.(nlayer-2))                             &
     &      d(i) = da(i)*qqq(i-1) + db(i)*qqq(i) + dc(i)*qqq(i+1)       
            if (i.eq.1)                                                 &
     &      d(i) =  db(i)*qqq(i) + dc(i)*qqq(i+1)                       
            if (i.eq.(nlayer-1))                                        &
     &      d(i) = da(i)*qqq(i-1) + db(i)*qqq(i)                        
!        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))')   
!     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif',   
!     &   'log(avdif1)','log(avdif2)','log(avdif'                       
!        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))')       
!     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,	! x 3.
!     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,),          
!     &    log(avdif* 3.6e5)                                            
         endif
!..                                                                     
!HHH 	qng incrementado em W no final apenas                             
!HH	if (i.eq.nlayer-1) d(i) = d(i) + qng                                
!HHH                                                                    
!H       if(i.eq.1) then                                                
!H       write(itmp5,*) ' Run2n: sistema eqs montado '                  
!H       write(itmp5,'(a8,50(2x,a9,2x))')                               
!H     &    'i','a(i)','b(i)','c(i)','d(i)','d(i)_mm/d'                 
!H       endif                                                          
!H       write(itmp5,'(i8.8,50(4x,e9.3))') i, a(i),b(i),c(i),d(i),      
!H     &                                   d(i)*1000.*dtt*24.           
   50 continue 
!..	solucao sistema eqs lineares                                        
      if(jsys.eq.1) call tridia2 (nlayer-1, d, c, a, b ) 
      if(jsys.eq.3) call eqlin (itmp5,jjgs,nlayer-1, d, c, a, b) 
!                                                                       
!     write(itmp5,'(/,i8.8,50(1x,e9.3),/)')                             
!     &   nymd, (d(i),i=1,nlayer-1)                                     
!H    write(itmp5,*)                                                    
!---------------------------------------------------------------------  
!   update wetness of soil layers due to layer interflow and base flow  
!---------------------------------------------------------------------  
                                                                        
      do 3000 i= 1, nlayer-1 
                                                                        
         qqq(i)= d(i) 
         if (jesq.eq.3) qqq(i) = 0.

         if (qqq(i).lt.0.) qdowrd(i+1) = qdowrd(i+1) + abs(qqq(i))
         if (qqq(i).gt.0.) qupwrd(i+1) = qupwrd(i+1) + qqq(i)

         qmin = -www(i)*(poros(i)*zdepth(i)/dtt)
         qmax = www(i+1)*(poros(i+1)*zdepth(i+1)/dtt)
         qqq(i) = amin1(qqq(i), qmax )
         qqq(i) = amax1(qqq(i), qmin )
         www(i) = www(i)+qqq(i)/(poros(i)*zdepth(i)/dtt)
         www(i+1) = www(i+1)-qqq(i)/(poros(i+1)*zdepth(i+1)/dtt) 

!...  qqq(i): fluxo base camada i (+downward) =                         
!     qdowrd,qupwrd(i+1): fluxo topo camada i+1 	                       
!     if (qqq(i).gt.0.) qdowrd(i+1) = qdowrd(i+1) + qqq(i)              
!     if (qqq(i).lt.0.) qupwrd(i+1) = qupwrd(i+1) + qqq(i)	             
                                                                        
                                                                        
 3000 continue 
                                                                        
                                                                        
!---------------------------------------------------------------------- 
!      calculation of gravitationally driven drainage (qng) from        
!      w(n): taken as an integral of time varying conductivity;         
!      addition of liston baseflow term to original qng to insure       
!      flow in dry season (modified liston baseflow constant            
!      scaled by available water)                                       
!                                                                       
!     QNG: equation of q3g (SE 86)                                      
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      pows= 2.*bee(nlayer)+2. 
      qng= temw(nlayer)**(-pows) +                                      &
     &        satco(nlayer)/zdepth(nlayer)/poros(nlayer)*slope*pows*dtt        
      qng= qng ** (1./pows) 
      qng= -( 1./qng-www(nlayer) )* poros(nlayer) * zdepth(nlayer)/dtt 
                                                                        
!HRDez2008... ajuste coeficiente de Liston                              
!_original_1994      qng = qng + 0.002 * poros(nlayer)*zdepth(nlayer) * 
!     &      * www (nlayer)                                             
!HR...  kb = escoamento de base se W3=1 ( em m/d)                       
                                                                        
      qng = qng + xkb/1000. * poros(nlayer)*zdepth(nlayer)/86400.       &
     &      * www (nlayer)                                              
                                                                        
      qng= amax1( 0., qng) 
      qng= amin1( qng, www(nlayer)*poros(nlayer)*zdepth(nlayer)/dtt) 
                                                                        
!H...	teste de difusao interna                                          
!	if (jqng.eq.0) qng = 0.                                               
                                                                        
      www(nlayer)= www(nlayer)-                                         &
     & qng*dtt/(poros(nlayer)*zdepth(nlayer))                           
      roff= roff + qng*dtt 
                                                   
      do 401 i= 1, nlayer ! arredondamento ... 
         excess= amax1( 0., (www(i)-1.)) 
         www(i)= www(i) - excess 
         roff= roff + excess*poros(i)*zdepth(i) 
  401 continue 
! -----------------------------------------------------------------     
!       prevent negative values of soil wetness                         
! -----------------------------------------------------------------     
                                                                        
      do 402 i = 1 , nlayer-1 
         deficit = amax1 (0.,(1.e-12 - www(i))) 
         www(i) = www(i) + deficit 
         www(i+1) = www(i+1) - deficit * zdepth(i)/zdepth(i+1) 
  402 continue 
      www(nlayer) = amax1 (www(nlayer),1.e-12) 
!... diagnostico escoamento de agua do solo                             
      d10 = dtt * 1000. 
!                                                                       
!H    write(itmp3,'(i8.8,50(1x,f10.3))')nymd,(qupwrd (i)*d10,i=1,nlayer)
!H    write(itmp4,'(i8.8,50(1x,f10.5))')nymd,(qdowrd (i)*d10,i=1,nlayer)
!     write(itmp5,'(i8.8,50(1x,e9.3))')nymd,(qhorton(i)*d10,i=1,nlayer) 
      return 
      END                                           
                                                                        
!=======================================================================
      subroutine retec (dpdw, jhort, i) 
!=======================================================================
!                                                                       
!..	output: cdpdw (derivada curva retencao) dpsi / dw                   
!                                                                       
!---------------------------------------------------------------------- 
      use comsibc 
      implicit none
      real (kind=8) :: dpdw
      integer :: jhort
      integer :: i
      real (kind=8) :: dpdw1
      real (kind=8) :: dpdw2
      integer :: j
      real (kind=8) :: pmax
      real (kind=8) :: pmin
      real (kind=8) :: psi1
      real (kind=8) :: psi2
      real (kind=8) :: w0
      real (kind=8) :: w1
      real (kind=8) :: w2
      real (kind=8) :: wmax
      real (kind=8) :: wmin = 0d0
!...                                                                    
      j = i + 1
!                                                                       
      if (i.gt.1) then 
         w0 = amin1(www(i-1),1.) 
         w0 = amax1(www(i-1),0.05) 
      endif 
      w1 = amin1(www(i),1.) 
      w1 = amax1(www(i),0.05) 
      w2 = amin1(www(j),1.) 
      w2 = amax1(www(j),0.05) 
!...	jhorton = 1  (calcula proximo saturacao; Runoff Horton)            
      if (jhort.eq.1) then 
         w1 = 1.0 
         w2 = 0.95 
      endif 
                                                                        
      psi1 = phsat(i)* (w1**(-bee(i))) 
      psi2 = phsat(j)* (w2**(-bee(j))) 
                                                                        
!... calculo método original SiB2 SE-96                                
      if (jdpsi.eq.1) then 
         wmax= amax1 (www(i), www(i+1)) 
         wmax= amax1 ( amin1(wmax,1.0),  0.05) 
         pmax= wmax**(-bee(i)) 
         if(i.eq.1) then 
            wmin= (pmax-2./(phsat(1)*(zdepth(1)+2.*zdepth(2)+           &
     &        zdepth(3))))                                              
!h           wmin= (pmax-2./(phsat(1)*(2.*zdepth(1)+zdepth(2))))        
            wmin= amin1(w1,w2,wmin) 
            wmin= amax1(wmin,1.0e-02) 
         endif 
         if(i.gt.1) then 
            wmin=(pmax-2./(phsat(i)*(zdepth(i-1)+2.*zdepth(i)+          &
     &        zdepth(i+1))))                                            
            wmin= amin1(w0,w1,w2,wmin) 
            wmin= amax1(wmin,1.0e-02) 
         endif 
         if (wmin.eq.wmax) then
            wmin= (pmax-1./(phsat(i)*(zdepth(i-1)+2.*zdepth(i)+         &
     &        zdepth(i+1))))                                            
            wmin= amax1(wmin,1.0e-03) 
         endif 
         pmin= wmin**(-bee(i)) 
         dpdw= phsat(i) * ( pmax-pmin )/( wmax-wmin ) 
         return 
      endif 
!...  calculo derivada psi(W) nas camada i,i+1 - CH-78                  
      dpdw1 = - bee(i) * phsat(i) / (w1 ** (bee(i) + 1.)) 
      dpdw2 = - bee(j) * phsat(j) / (w2 ** (bee(j) + 1.)) 
!...                           
      if (jdpsi.eq.2) then ! media ponderada espessura	                 	 
         dpdw = ( dpdw1  * zdepth(i) +  dpdw2  * zdepth(i+1) ) /        &
     &   ( zdepth(i+1) + zdepth(i) )                                    
      endif
!...	                                                                   
      if (jdpsi.eq.3) then ! media geometrica	                          	 
         dpdw = sqrt( dpdw1 * dpdw2 ) 
      endif
!...                                                                    
      if (jdpsi.eq.4) then ! media aritmetica	                          	 
         dpdw = ( dpdw1 + dpdw2 )/2. 
      endif
      return 
      END                                           
                                                                        
!====================================================================   
      subroutine hydcon (avk, i)
!====================================================================   
!                                                                       
!     avk (condutividade hidraulica segmento camadas i,i+1)             
!                                                                       
!--------------------------------------------------------------------   
      use comsibc
      implicit none
      integer :: i
      real (kind=8) :: avb
      real (kind=8) :: avk
      real (kind=8) :: avk1
      real (kind=8) :: avk2
      real (kind=8) :: avkmax
      real (kind=8) :: avkmin
      real (kind=8) :: div
      integer :: j
      real (kind=8) :: props
      real (kind=8) :: psi1
      real (kind=8) :: psi2
      real (kind=8) :: rsame
      real (kind=8) :: ts
      real (kind=8) :: tsnow
      real (kind=8) :: w1
      real (kind=8) :: w2
      
      j = i + 1
      w1 = amin1(www(i),1.) 
      w1 = amax1(www(i),0.05) 
      w2 = amin1(www(j),1.) 
      w2 = amax1(www(j),0.05) 

      psi1 = phsat(i)*(w1**(-bee(i)))
      psi2 = phsat(j)*(w2**(-bee(j)))
      avk1 = satco(i)*(w1**(2.*bee(i)+3.))
      avk2 = satco(j)*(w2**(2.*bee(j)+3.))
!...
      if (jkcon.eq.1) then ! metodo ME-82                               	 
         rsame= 0. 
         avb = (bee(i) + bee(j)) / 2.
         div= psi2 - psi1 
      if ( abs(div) .lt. 1.e-6 ) rsame=1. 
      avk = (psi1*avk1 - psi2*avk2) /                                   &
     & ( ( 1. +3./avb ) * div + rsame )                                 
!...                                                                    
      avkmin = amin1 (avk1, avk2) 
      avk    = amax1 (avk, avkmin) 
      avkmax = amax1 (avk1, avk2 ) 
      avk    = amin1 (avk, avkmax ) 
      endif 
!...                                                                    
      if (jkcon.eq.2) then ! media ponderada espessura	                 	 
         avk = ( avk1  * zdepth(i) +  avk2  * zdepth(i+1) ) /           &
     &   ( zdepth(i+1) + zdepth(i) )                                    
      endif
!...	                                                                   
      if (jkcon.eq.3) then ! media geometrica	                          	 
         avk = sqrt( avk1 * avk2 ) 
      endif
!...                                                                       
      if (jkcon.eq.4) then ! media aritmetica	                          	 
         avk = ( avk1 + avk2 )/2. 
      endif
!----------------------------------------------------------------       
!      conductivites and baseflow reduced when temperature drops        
!      below freezing                                                   
!----------------------------------------------------------------       
      tsnow = amin1( tf-0.01,tg) 
      tgs = tsnow * areas + tg* (1. - areas) 
      ts = tgs * (2 - j)  + td*(j-1) 
      props = ( ts - (tf - 10.) ) / 10. 
      props = amax1(0.05,amin1(1.0,props)) 
      avk = avk * props 
      qng = qng * props 
      return 
      END                                           
!                                                                       
!=======================================================================
      subroutine tridia2 (n, d, c, a, b) 
!=======================================================================
!                                                                       
!  	HR  algoritmo original 1996, corrigido 2009                         
!                                                                       
!             solution of linear system of n x n dimension              
!             for a tridiagonal matrix                                  
!             d    : independent coefficients (input)                   
!                         vector solution      (output)                 
!             b  : main diagonal coefs                                  
!             c   :  suprior diagonal coefs                             
!             a   : inferior diagonal coefs                             
!--------------------------------------------------------------
      implicit none
      ! dimension c(n+1), a(n+1), b(n+1), d(n+1) 
      ! c(1) = c(1)/b(1)
      ! d(1) = d(1)/b(1)
      integer :: n
      real (kind=8) :: c(n+1)
      real (kind=8) :: a(n+1)
      real (kind=8) :: b(n+1)
      real (kind=8) :: d(n+1)
      !
      integer :: i
      integer :: ii
      integer :: k
      !
      c(1) = c(1)/b(1)
      d(1) = d(1)/b(1) 

      
      do 10 i = 2, (n-1), 1 
         ii = (i-1) 
         b(i) = b(i) - c(ii)*a(i) 
         if (i .eq. n) go to 10 
         c(i) = c(i) / b(i) ! calculado
   10    d(i) = (d(i) - d(ii)*a(i)) / b(i) 
         do 20 k = 1, (n-1), 1 
            i = n - k ! back substitution                          		 
   20       d(i) = d(i) - c(i) * d(i+1) 
      return 
      END                                           
!=======================================================================
      subroutine eqlin (ifi, inum, n, d, sup, sub, diag) 
!=======================================================================
!	solução sistema eqs lineares m x n por metodo iterativo             
!	HRocha Jan 2009                                                       
!	inum =  1 (método de Jacobi)                                         
!		2 (método de Gauss-Seidel)                                          
!                                                                       
!	elements of soil multi-layer scheme in SiB:                           
!             d     : independent coefficients (inputs)                 
!			 vector solution (output)                                           
!             diag  : main diagonal coefs                               
!             sup   :  suprior diagonal coefs                           
!             sub   : inferior diagonal coefs                           
!                                                                       
!--------------------------------------------------------------         
      implicit none
      !parameter (nmax=30, mmax=30, kmax = 100)
      integer, parameter :: nmax=30
      integer, parameter :: mmax=30
      integer, parameter :: kmax = 100
      !real d(n), sup(n), sub(n), diag(n)
      integer :: n
      real (kind=8) :: d(n)
      real (kind=8) :: sup(n)
      real (kind=8) :: sub(n)
      real (kind=8) :: diag(n)
      !real a(mmax,nmax), b(mmax), x(nmax,2), dif(nmax) 
      real (kind=8) :: a(mmax,nmax)
      real (kind=8) :: b(mmax)
      real (kind=8) :: x(nmax,2)
      real (kind=8) :: dif(nmax)
      !parameter (eps=1.e-06,itmax=50) ! critérios convergencia         
      real (kind=8), parameter :: eps = 1d-06
      integer, parameter :: itmax = 50
      !character aconv(3)*20
      character (len=20) :: aconv(3) 
      !logical conv
      logical :: conv
      !
      integer :: ifi
      integer :: inum
      integer :: i
      integer :: isum
      integer :: it
      integer :: j
      integer :: k
      integer :: m
      !
      do 1 k= 1,3 
    1    aconv(k) = ' ' 

      m = n                     ! este caso matriz n x n                    

      do 10 i= 1,n 
         x(i,2) = d(i)          ! cond.inicial column vector X(n)       
         dif (i) = 0. 
         do 20 j= 1,m 
   20       a(j,i) = 0.         ! matrix A(m,n)
   10 continue 
                                                                        
!...	atribuicao matriz eqs Jacobi c/ input esquema soil multi-layer     
      a(1,1)   = diag(1) 
      a(1,2)   = sup(1) 
      b(1)     = d(1) 
      a(m,n-1) = sub(n) 
      a(m,n)   = diag(n) 
      b(m)     = d(n) 
!     inserir aqui goto caso matriz 2x2                                 
      do 30 i= 2,m-1 
         a(i,i-1) = sub(i) 
         a(i,i)   = diag(i) 
         a(i,i+1) = sup(i) 
   30    b(i)     = d(i) 
      write(ifi,*) ' Eqlin: sistema montado' 
      write(ifi,'(a3,a10,30(1x,a2,i2,5x))')                             &
     &        'i',' b ',('a_',i,i=1,n)                                  
      do i = 1, m 
         write(ifi,'(i3,30(1x,e9.3))') i, b(i), (a(i,j),j=1,n) 
      enddo 
!...  solução do sistema                                              
      it = 0 
      conv = .false. 
      do 35 i = 1,n 
         if (a(i,i).eq.0.)  then 
            conv = .true. 
            aconv(1)= ' a(i,i)=0' 
         endif 
   35 continue 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
      do while (.not.conv) 
                                ! recupera passo anterior               
         do 50 i= 1, n 
            x(i,1) = x(i,2)
   50       x(i,2) = 0. 
!---------------------------------------------------------------------- 
         do 56 i= 1, n 
            do 60 j= 1, (i-1) 
                                                             !G-Seidel  
               if(inum.eq.2) x(i,2) = x(i,2) + a(i,j)*x(j,2) 
                                                             !Jacobi    
   60          if(inum.eq.1) x(i,2) = x(i,2) + a(i,j)*x(j,1) 
            do 65 j= (i+1), n 
   65          x(i,2) = x(i,2) + a(i,j)*x(j,1) 
               x(i,2) = (b(i)-x(i,2))/a(i,i) 
   56    continue 
!---------------------------------------------------------------------- 
!       if (it.eq.0) write(ifi,'(a3,50(1x,a4,i3,1x,a4,i3))')            
!       &   'IT',  ( 'xt0_',i,  'xt1_',i   , i= 1,n )                   
!       write(ifi,'(i3,50(1x,e7.1))') it, (x(i,1),x(i,2),i=1,n)         
         isum = 0 
         do 58 i= 1, n 
            if (x(i,1).ne.0.) then 
               dif(i) = abs(x(i,2)-x(i,1))/x(i,1) 
               if (dif(i).lt.eps) isum = isum + 1 
            endif 
   58       d(i) = x(i,2) 
         if (isum.eq.n) then 
            conv = .true. 
            aconv(2) = ' eps<0 ' 
         endif 
!...                                                                    
         it = it + 1
         if (it.eq.itmax) then 
            conv = .true. 
            aconv(3) = ' it=itmax ' 
         endif 
!...                                                                    
      end do 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
      write(ifi,*) 
                                                                        
      write(ifi,'(4a17)') 'Convergencia:', (aconv(i),i=1,3) 
      write(ifi,'(a3,5a13,i3,a20)') 'i','x(i,1)','x(i,2)','d(i)',       &
     &            'd(i)_mm/d','it final=',it,' sistema resolvido'       
      do i = 1, n 
         write(ifi,'(i3,4(1x,e12.3))')                                  &
     &        i, x(i,1), x(i,2), d(i), d(i)*1000.*3600*24.              
      end do 
      write(ifi,*) 
      return 
      END                                           
