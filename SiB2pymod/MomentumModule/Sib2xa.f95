!                                                                       
      subroutine const2 
!                                                                       
!=======================================================================
!                                                                       
!     initialization of physical constants                              
!                                                                       
!     subroutine const2 is called at every time step                    
!     because in some applications constants may depend on              
!     environmental conditions.                                         
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      use comsibc
      implicit none
!                                                                       
      asnow    = 13.2 
      bps      = 1. 
      clai     = 4.2 * 1000. * 0.2 
      cpair    = 1010. 
      cw       = 4.2 * 1000. * 1000. 
      epsfac   = 0.622 
      gx       = 9.81 
      kappa    = 0.286 
      pie      = 3.14159265 
      po2m     = 20900. 
      pco2m    = 34. 
      psur     = 1000. 
      rhoair   = 1.225 
      snomel   = 370518.5 * 1000. 
      stefan   = 5.669 * 10e-9 
      tf       = 273.16 
      vkc      = 0.41 
      rcp      = rhoair * cpair 
      timcon   = pie/86400. 
!                                                                       
!-----------------------------------------------------------------------
!     n.b. :  snomel is expressed in j m-1                              
!-----------------------------------------------------------------------
!                                                                       
      return 
      END                                           
                                                                        
!=======================================================================
!                                                                       
      subroutine adjust ( ts, spechc, capacp, snowwp, iveg ) 
!                                                                       
!=======================================================================
!                                                                       
!     temperature change due to addition of precipitation               
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!     roff           runoff (m)                                         
!     tc             canopy temperature (K)                             
!     tg             ground surface temperature (K)                     
!     www(1)         ground wetness of surface layer                    
!     capac(2)       canopy/ground liquid interception store (m)        
!     snoww(2)       canopy/ground snow interception store (m)          
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      implicit none
      real (kind=8) :: ts
      real (kind=8) :: spechc
      real (kind=8) :: capacp
      real (kind=8) :: snowwp
      integer :: iveg
      real (kind=8) :: cca
      real (kind=8) :: ccb
      real (kind=8) :: ccc
      real (kind=8) :: ccp
      real (kind=8) :: cct
      real (kind=8) :: diff
      real (kind=8) :: freeze
      real (kind=8) :: tsd
      real (kind=8) :: tta
      real (kind=8) :: ttb
      real (kind=8) :: xs            
!                                                                       
      freeze = 0. 
      diff = ( capac(iveg)+snoww(iveg) - capacp-snowwp )*cw 
      ccp = spechc 
      cct = spechc + diff 
!                                                                       
      tsd = ( ts * ccp + tm * diff ) / cct 
!                                                                       
      if ( ts .gt. tf .and. tm .gt. tf ) go to 200 
      if ( ts .le. tf .and. tm .le. tf ) go to 200 
!                                                                       
      tta = ts 
      ttb = tm 
      cca = ccp 
      ccb = diff 
      if ( tsd .gt. tf ) go to 100 
!                                                                       
!---------------------------------------------------------------------- 
!    freezing of water on canopy or ground                              
!---------------------------------------------------------------------- 
!                                                                       
      ccc = capacp * snomel 
      if ( ts .lt. tm ) ccc = diff * snomel / cw 
      tsd = ( tta * cca + ttb * ccb + ccc ) / cct 
!                                                                       
      freeze = ( tf * cct - ( tta * cca + ttb * ccb ) ) 
      freeze = (min ( ccc, freeze )) / snomel 
      if(tsd .gt. tf)tsd = tf - 0.01 
!                                                                       
      go to 200 
!                                                                       
  100 continue 
!                                                                       
!---------------------------------------------------------------------- 
!    melting of snow on canopy or ground, water infiltrates.            
!---------------------------------------------------------------------- 
!                                                                       
      ccc = - snoww(iveg) * snomel 
      if ( ts .gt. tm ) ccc = - diff * snomel / cw 
!                                                                       
      tsd = ( tta * cca + ttb * ccb + ccc ) / cct 
!                                                                       
      freeze = ( tf * cct - ( tta * cca + ttb * ccb ) ) 
      freeze = (max( ccc, freeze )) / snomel 
      if(tsd .le. tf)tsd = tf - 0.01 
!                                                                       
  200 snoww(iveg) = snoww(iveg) + freeze 
      capac(iveg) = capac(iveg) - freeze 
!                                                                       
      xs = max( 0., ( capac(iveg) - satcap(iveg) ) ) 
      if( snoww(iveg) .ge. 0.0000001 ) xs = capac(iveg) 
      www(1) = www(1) + xs / ( poros(1) * zdepth(1) ) 
      capac(iveg) = capac(iveg) - xs 
      ts = tsd 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine patchs ( p0 ) 
!                                                                       
!=======================================================================
!                                                                       
!     marginal situation: snow exists in patches at temperature tf      
!     with remaining area at temperature tg > tf.                       
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     calculation of effect of intercepted snow and rainfall on ground. 
!     patchy snowcover situation involves complex treatment to keep     
!     energy conserved.                                                 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      implicit none
!                                                                       
      real (kind=8) :: p0
      real (kind=8) :: dareas
      real (kind=8) :: dcap
      real (kind=8) :: ex
      real (kind=8) :: pinf
      real (kind=8) :: rhs
      real (kind=8) :: snowhc
      real (kind=8) :: thru
      real (kind=8) :: tsd
      real (kind=8) :: zmelt
!
      pinf = p0 
      thru = 0. 
      snowhc = min( 0.05, snoww(2) ) * cw 
      areas = min( 1.,(asnow*snoww(2)) ) 
      if( tm .gt. tf ) go to 400 
!                                                                       
!---------------------------------------------------------------------- 
!     snow falling onto area                                            
!---------------------------------------------------------------------- 
!                                                                       
      rhs = tm*pinf*cw + tf*(snowhc + csoil*areas)                      &
     &    + tg*csoil*(1.-areas)                                         
      dareas = min( asnow*pinf, ( 1.-areas ) ) 
      ex = rhs - tf*pinf*cw - tf*(snowhc + csoil*(areas + dareas))      &
     &   - tg*csoil*(1.-areas-dareas)                                   
      if( (areas+dareas) .ge. 0.999 ) tg = tf - 0.01 
      if( ex .lt. 0. ) go to 200 
!                                                                       
!---------------------------------------------------------------------- 
!     excess energy is positive, some snow melts and infiltrates.       
!---------------------------------------------------------------------- 
!                                                                       
      zmelt = ex/snomel 
      if( asnow*(snoww(2) + pinf - zmelt) .gt. 1. ) go to 100 
      zmelt = 0. 
      if( asnow*(snoww(2) + pinf) .ge. 1. )                             &
     &    zmelt = ( asnow*(snoww(2) + pinf) - 1. ) / asnow              
      zmelt = ( ex - zmelt*snomel )/( snomel + asnow*csoil*(tg-tf) )    &
     &      + zmelt                                                     
  100 snoww(2) =  snoww(2) + pinf - zmelt 
      www(1) = www(1) + zmelt/(poros(1)*zdepth(1)) 
      go to 600 
!                                                                       
!---------------------------------------------------------------------- 
!     excess energy is negative, bare ground cools to tf, then whole    
!     area cools together to lower temperature.                         
!---------------------------------------------------------------------- 
!                                                                       
  200 tsd = 0. 
      if( (areas+dareas) .le. 0.999 )                                   &
     &        tsd = ex/(csoil*( 1.-areas-dareas)) + tg                  
      if( tsd .gt. tf ) go to 300 
      tsd = tf + ( ex - (tf-tg)*csoil*(1.-areas-dareas) )               &
     &            /(snowhc+pinf*cw+csoil)                               
  300 tg = tsd 
      snoww(2) = snoww(2) + pinf 
      go to 600 
!                                                                       
!---------------------------------------------------------------------- 
!     rain falling onto area                                            
!---------------------------------------------------------------------- 
!                                                                       
  400 continue 
!                                                                       
!---------------------------------------------------------------------- 
!     rain falls onto snow-free sector first.                           
!---------------------------------------------------------------------- 
!                                                                       
      tsd = tf - 0.01 
      if ( areas .lt. 0.999 ) tsd = ( tm*pinf*cw + tg*csoil )           &
           /( pinf*cw + csoil )                
      tg = tsd 
      www(1)= www(1)+pinf*(1.-areas)/(poros(1)*zdepth(1)) 
!                                                                       
!---------------------------------------------------------------------- 
!     rain falls onto snow-covered sector next.                         
!---------------------------------------------------------------------- 
!                                                                       
      ex = ( tm - tf )*pinf*cw*areas 
      dcap = -ex / ( snomel + ( tg-tf )*csoil*asnow ) 
      if( (snoww(2) + dcap) .lt. 0. ) go to 500 
      www(1) = www(1)+(pinf*areas-dcap)/(poros(1)*zdepth(1)) 
      snoww(2) = snoww(2) + dcap 
      go to 600 
  500 tg = ( ex - snomel*snoww(2) - ( tg-tf )*csoil*areas ) / csoil     &
     &      + tg                                                        
      www(1)=www(1)+(snoww(2)+pinf*areas)/(poros(1)*zdepth(1)) 
      capac(2) = 0. 
      snoww(2) = 0. 
!                                                                       
  600 continue 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine snow1 
!                                                                       
!=======================================================================
!                                                                       
!     calculation of effects of snow cover on surface morphology and    
!     maximum water storage values.                                     
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       z0             roughness length (m)                             
!       xdx              zero plane displacement (m)                    
!       rbc            rb coefficient (c1) (s m-1)**1/2                 
!       rdc            rd coefficient (c2)                              
!       satcap(2)      interception capacities (m)                      
!       canex          fraction of exposed canopy (snow-free)           
!       areas          ground snow cover fraction                       
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
!    alteration of aerodynamic transfer properties in case of snow      
!    accumulation. calculation of maximum water storage values.         
!                                                                       
!      canex       (fraction of canopy not covered by snow)             
!      xdx           (snow-modified value of dd, used in all calculation
!      z0          (snow-modified value of z0d, used in all calculations
!      rbc         (snow-modified value of cc1, used in all calculations
!      rdc         (snow-modified value of cc2, used in all calculations
!      areas       (fraction of ground covered by snow)                 
!      satcap(1)   (s-c)   : equation (56) , SE-86, page 741 se-89      
!      satcap(2)   (s-g)   : 0.002, surface interception store          
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      implicit none
!                                                                       
      canex  = 1.-( snoww(2)*5.-z1)/(z2-z1) 
      canex  = max( 0.1, canex ) 
      canex  = min( 1.0, canex ) 
      xdx    = z2 - ( z2-dd ) * canex 
      z0     = z0d/( z2-dd ) * ( z2-xdx ) 
      rbc    = cc1/canex 
      rdc    = cc2*canex 
      areas    = min(1., asnow*snoww(2)) 
      satcap(1) = zlt*0.0001 * canex 
      satcap(2) = 0.002 
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine rada2 
!                                                                       
!=======================================================================
!                                                                       
!     calculation of albedos via two stream approximation( direct       
!     and diffuse ) and partition of radiant energy                     
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     subroutines  called  : snow1                                      
!     --------------------   longrn                                     
!                                                                       
!                                                                       
!++++++++++++++++++++++++++++++output++++++++++++++++++++++++++++++++   
!                                                                       
!       salb(2,2)      surface albedos                                  
!       tgeff          effective surface radiative temperature (k)      
!       radfac(2,2,2)  radiation absorption factors                     
!       thermk         canopy gap fraction for tir radiation            
!                                                                       
!++++++++++++++++++++++++++diagnostics+++++++++++++++++++++++++++++++   
!                                                                       
!       albedo(2,2,2)  component reflectances                           
!       closs          tir emission from the canopy (w m-2)             
!       gloss          tir emission from the ground (w m-2)             
!                                                                       
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
!                                                                       
      use comsibc 
      implicit none
!                                                                       
      !dimension tranc1(2), tranc2(2), tranc3(2)
      real (kind=8) :: tranc1(2)
      real (kind=8) :: tranc2(2)
      real (kind=8) :: tranc3(2) 
      !
      real (kind=8) :: aa
      real (kind=8) :: acss
      real (kind=8) :: bb
      real (kind=8) :: be
      real (kind=8) :: betao
      real (kind=8) :: bot
      real (kind=8) :: ce
      real (kind=8) :: chiv
      real (kind=8) :: closs
      real (kind=8) :: de
      real (kind=8) :: den
      real (kind=8) :: ek
      real (kind=8) :: epsi
      real (kind=8) :: extkb
      real (kind=8) :: f1
      real (kind=8) :: fac2
      real (kind=8) :: facs
      real (kind=8) :: fe
      real (kind=8) :: fmelt
      real (kind=8) :: ge
      real (kind=8) :: gloss
      real (kind=8) :: hh1
      real (kind=8) :: hh10
      real (kind=8) :: hh2
      real (kind=8) :: hh3
      real (kind=8) :: hh4
      real (kind=8) :: hh5
      real (kind=8) :: hh6
      real (kind=8) :: hh7
      real (kind=8) :: hh8
      real (kind=8) :: hh9
      integer :: irad
      integer :: iveg
      integer :: iwave
      real (kind=8) :: power1
      real (kind=8) :: power2
      real (kind=8) :: proj
      real (kind=8) :: psi
      real (kind=8) :: reff1
      real (kind=8) :: reff2
      real (kind=8) :: scat
      real (kind=8) :: scov
      real (kind=8) :: tg4
      real (kind=8) :: tran1
      real (kind=8) :: tran2
      real (kind=8) :: upscat
      real (kind=8) :: xfx
      real (kind=8) :: zat
      real (kind=8) :: zkat
      real (kind=8) :: zmew
      real (kind=8) :: zmk
      real (kind=8) :: zp
      real (kind=8) :: fac1
      real (kind=8) :: tc4
!
      xfx = sunang 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!                                                                       
!     modification for effect of snow on upper story albedo             
!         snow reflectance   = 0.80, 0.40 . multiply by 0.6 if melting  
!         snow transmittance = 0.20, 0.54                               
!                                                                       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      call snow1 
!                                                                       
      facs  = ( tg-tf ) * 0.04 
      facs  = max( 0. , facs) 
      facs  = min( 0.4, facs) 
      fmelt = 1. - facs 
!                                                                       
      do 1000 iwave = 1, 2 
!                                                                       
      scov =  min( 0.5, snoww(1)/satcap(1) ) 
      reff1 = ( 1. - scov ) * ref(iwave,1) + scov * ( 1.2 -             &
     &        iwave * 0.4 ) * fmelt                                     
      reff2 = ( 1. - scov ) * ref(iwave,2) + scov * ( 1.2 -             &
     &        iwave * 0.4 ) * fmelt                                     
      tran1 = tran(iwave,1) * ( 1. - scov )                             &
     &        + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt )            &
     &        * tran(iwave,1)                                           
      tran2 = tran(iwave,2) * ( 1. - scov )                             &
     &        + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt ) * 0.9      &
     &        * tran(iwave,2)                                           
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     calculate average scattering coefficient, leaf projection and     
!     other coefficients for two-stream model.                          
!                                                                       
!      scat  (omega)        : equation (1,2) , SE-85                    
!      proj  (g(mu))        : equation (13)  , SE-85                    
!      extkb (k, g(mu)/mu)  : equation (1,2) , SE-85                    
!      zmew  (int(mu/g(mu)) : equation (1,2) , SE-85                    
!      acss  (a-s(mu))      : equation (5)   , SE-85                    
!      extk  (k, various)   : equation (13)  , SE-85                    
!      upscat(omega-beta)   : equation (3)   , SE-85                    
!      betao (beta-0)       : equation (4)   , SE-85                    
!      psi   (h)            : appendix       , SE-85                    
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      scat = green*( tran1 + reff1 ) +( 1. - green ) *                  &
     &     ( tran2 + reff2)                                             
      chiv = chil 
!                                                                       
      if ( abs(chiv) .le. 0.01 ) chiv = 0.01 
      aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv 
      bb = 0.877 * ( 1. - 2. * aa ) 
!                                                                       
      proj = aa + bb * xfx 
      extkb = ( aa + bb * xfx ) / xfx 
      zmew = 1. / bb * ( 1. - aa / bb * log ( ( aa + bb ) / aa ) ) 
      acss = scat / 2. * proj / ( proj + xfx * bb ) 
      acss = acss * ( 1. - xfx * aa / ( proj + xfx * bb ) *             &
     &     log ( ( proj + xfx * bb + xfx * aa ) / ( xfx * aa ) ) )     
!                                                                       
      upscat = green * tran1 + ( 1. - green ) * tran2 
      upscat = 0.5 * ( scat + ( scat - 2. * upscat ) *                  &
     &     (( 1. - chiv ) / 2. ) ** 2 )                                 
      betao = ( 1. + zmew * extkb ) / ( scat * zmew * extkb ) * acss 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     intermediate variables identified in appendix of SE-85.           
!                                                                       
!      be          (b)     : appendix      , SE-85                      
!      ce          (c)     : appendix      , SE-85                      
!      bot         (sigma) : appendix      , SE-85                      
!      hh1         (h1)    : appendix      , SE-85                      
!      hh2         (h2)    : appendix      , SE-85                      
!      hh3         (h3)    : appendix      , SE-85                      
!      hh4         (h4)    : appendix      , SE-85                      
!      hh5         (h5)    : appendix      , SE-85                      
!      hh6         (h6)    : appendix      , SE-85                      
!      hh7         (h7)    : appendix      , SE-85                      
!      hh8         (h8)    : appendix      , SE-85                      
!      hh9         (h9)    : appendix      , SE-85                      
!      hh10        (h10)   : appendix      , SE-85                      
!      psi         (h)     : appendix      , SE-85                      
!      zat         (l-t)   : appendix      , SE-85                      
!      epsi        (s1)    : appendix      , SE-85                      
!      ek          (s2)    : appendix      , SE-85                      
!--------------------------------------------------------------------   
!                                                                       
      be = 1. - scat + upscat 
      ce = upscat 
      bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 ) 
      if ( abs(bot) .gt. 1.e-10) go to 100 
      scat = scat* 0.98 
      be = 1. - scat + upscat 
      bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 ) 
!                                                                       
  100 continue 
!                                                                       
      de = scat * zmew * extkb * betao 
      fe = scat * zmew * extkb * ( 1. - betao ) 
      hh1 = - de * be + zmew * de * extkb - ce * fe 
      hh4 = - be * fe - zmew * fe * extkb - ce * de 
!                                                                       
      psi = sqrt(be**2 - ce**2)/zmew 
!                                                                       
      zat = zlt/vcover*canex 
!                                                                       
      power1 = min( psi*zat, 50. ) 
      power2 = min( extkb*zat, 50. ) 
      epsi = exp( - power1 ) 
      ek = exp ( - power2 ) 
!                                                                       
      albedo(2,iwave,1) = soref(iwave)*(1.-areas)                       &
     &                  + ( 1.2-iwave*0.4 )*fmelt * areas               
      albedo(2,iwave,2) = soref(iwave)*(1.-areas)                       &
     &                  + ( 1.2-iwave*0.4 )*fmelt * areas               
      ge = albedo(2,iwave,1)/albedo(2,iwave,2) 
!                                                                       
!-----------------------------------------------------------------------
!     calculation of diffuse albedos                                    
!                                                                       
!      albedo(1,ir,2) ( i-up ) : appendix , SE-85                       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      f1 = be - ce / albedo(2,iwave,2) 
      zp = zmew * psi 
!                                                                       
      den = ( be + zp ) * ( f1 - zp ) / epsi -                          &
     &      ( be - zp ) * ( f1 + zp ) * epsi                            
      hh7   = ce * ( f1 - zp ) / epsi / den 
      hh8  = -ce * ( f1 + zp ) * epsi / den 
      f1 = be - ce * albedo(2,iwave,2) 
      den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi 
!                                                                       
      hh9   = ( f1 + zp ) / epsi / den 
      hh10  = - ( f1 - zp ) * epsi / den 
      tranc2(iwave) = hh9 * epsi + hh10 / epsi 
!                                                                       
      albedo(1,iwave,2) =  hh7 + hh8 
!                                                                       
!-----------------------------------------------------------------------
!     calculation of direct albedos and canopy transmittances.          
!                                                                       
!      albedo(1,iw,1) ( i-up ) : equation(11)   , SE-85                 
!      tranc (iw)   ( i-down ) : equation(10)   , SE-85                 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      f1 = be - ce / albedo(2,iwave,2) 
      zmk = zmew * extkb 
!                                                                       
      den = ( be + zp ) * ( f1 - zp ) / epsi -                          &
     &      ( be - zp ) * ( f1 + zp ) * epsi                            
      hh2 = ( de - hh1/bot * ( be + zmk ) ) * ( f1 - zp ) / epsi -      &
     &        ( be - zp ) * ( de - ce*ge - hh1/bot * ( f1 + zmk ) ) * ek
      hh2 = hh2 / den 
      hh3 = ( be + zp ) * (de - ce*ge - hh1/bot * ( f1 + zmk ))* ek -   &
     &       ( de - hh1/bot * ( be + zmk ) ) * ( f1 + zp ) * epsi       
      hh3 = hh3 / den 
      f1 = be - ce * albedo(2,iwave,2) 
      den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi 
      hh5 = - hh4/bot * ( f1 + zp ) / epsi -                            &
     &        ( fe + ce*ge*albedo(2,iwave,2) + hh4/bot*( zmk-f1 ) ) * ek
      hh5 = hh5 / den 
      hh6 = hh4/bot * ( f1 - zp ) * epsi +                              &
     &        ( fe + ce*ge*albedo(2,iwave,2) + hh4/bot*( zmk-f1 ) ) * ek
      hh6 = hh6 / den 
      tranc1(iwave) = ek 
      tranc3(iwave) = hh4/bot * ek + hh5 * epsi + hh6 / epsi 
!                                                                       
      albedo(1,iwave,1) = hh1/bot + hh2 + hh3 
!                                                                       
!---------------------------------------------------------------------- 
!     calculation of terms which multiply incoming short wave fluxes    
!     to give absorption of radiation by canopy and ground              
!                                                                       
!      radfac      (f(il,imu,iv)) : equation (19,20) , SE-86            
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      radfac(2,iwave,1) = ( 1.-vcover ) * ( 1.-albedo(2,iwave,1) )      &
     &       + vcover * ( tranc1(iwave) * ( 1.-albedo(2,iwave,1) )      &
     &       + tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )               
!                                                                       
      radfac(2,iwave,2) = ( 1.-vcover ) * ( 1.-albedo(2,iwave,2) )      &
     &       + vcover *  tranc2(iwave) * ( 1.-albedo(2,iwave,2) )       
!                                                                       
      radfac(1,iwave,1) = vcover * ( ( 1.-albedo(1,iwave,1) )           &
     &       - tranc1(iwave) * ( 1.-albedo(2,iwave,1) )                 &
     &       - tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )               
!                                                                       
      radfac(1,iwave,2) = vcover * ( ( 1.-albedo(1,iwave,2) )           &
     &       - tranc2(iwave) * ( 1.-albedo(2,iwave,2) ) )               
!                                                                       
!---------------------------------------------------------------------- 
!     calculation of total surface albedos ( salb ) with weighting      
!     for cover fractions.                                              
!---------------------------------------------------------------------- 
!                                                                       
      do 3000 irad = 1, 2 
!                                                                       
      salb(iwave,irad) = ( 1.-vcover ) * albedo(2,iwave,irad) +         &
     &                   vcover * albedo(1,iwave,irad)                  
!                                                                       
 3000 continue 
!                                                                       
 1000 continue 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     calculation of long-wave flux terms from canopy and ground        
!                                                                       
!      closs ( fc - rnc )     : equation (21),  SE-86                   
!      gloss ( fg - rng )     : equation (22),  SE-86                   
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      tgs = min(tf,tg)*areas + tg*(1.-areas) 
      tc4 = tc  * tc  * tc  * tc 
      tg4 = tgs * tgs * tgs * tgs 
!                                                                       
      zkat = 1./zmew * zlt / vcover 
      zkat = min( 50. , zkat ) 
      zkat = max( 1.e-5, zkat ) 
      thermk = exp(-zkat) 
!                                                                       
      fac1 =  vcover * ( 1.-thermk ) 
      fac2 =  1. 
      closs =  2. * fac1 * stefan * tc4 
      closs =  closs - fac2 * fac1 * stefan * tg4 
      gloss =  fac2 * stefan * tg4 
      gloss =  gloss - fac1 * fac2 * stefan * tc4 
      zlwup =  fac1 * stefan * tc4 + (1. - fac1 ) * fac2 * stefan * tg4 
!                                                                       
      call longrn( tranc1, tranc2, tranc3 ) 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     calculation of absorption of radiation by surface                 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      radt(1) = 0. 
      radt(2) = 0. 
!                                                                       
      do 2000 iveg  = 1, 2 
      do 2000 iwave = 1, 2 
      do 2000 irad  = 1, 2 
!                                                                       
      radt(iveg) = radt(iveg)+radfac(iveg,iwave,irad)*radn(iwave,irad) 
!                                                                       
 2000 continue 
!                                                                       
      radt(1) = radt(1) + radn(3,2)*vcover*(1.- thermk) - closs 
      radt(2) = radt(2) + radn(3,2)*( 1.-vcover*(1.-thermk) ) - gloss 
!                                                                       
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine longrn( tranc1, tranc2, tranc3 ) 
!                                                                       
!=======================================================================
!                                                                       
!     calculation of downward longwave. this is not required in gcm if  
!     downward longwave is provided by gcm-radiation code as radn(3,2). 
!                                                                       
!-----------------------------------------------------------------------
      use comsibc 
      implicit none
!                                                                       
      ! dimension tranc1(2), tranc2(2), tranc3(2)
      real (kind=8) :: tranc1(2)
      real (kind=8) :: tranc2(2)
      real (kind=8) :: tranc3(2)
!                                                                       
      real (kind=8) :: esky
      integer :: iwave
      real (kind=8) :: swab
      real (kind=8) :: swup
!                                                                       
      if(ilw .eq. 1)go to 101 
      if(ilw .eq. 2)go to 102  ! Brunts
      if(ilw .eq. 3)go to 103  ! residuo Bal radiacao
      if(ilw .eq. 4)go to 104  ! Brutsaert(1975)
      if(ilw .eq. 5)go to 105  ! Idso and Jackson
      if(ilw .eq. 6)go to 106  ! Swinbank
      if(ilw .eq. 7)go to 107  ! Duarte
      if(ilw .eq. 8)go to 108  ! Kruk 
!                                                                       
!---------------------------------------------------------------------- 
!     downward long-wave assumed to be provided as radn(3,2)            
!---------------------------------------------------------------------- 
  101 continue 
      go to 200 
!                                                                       
!---------------------------------------------------------------------- 
!     downward long-wave from brunt's equation, Monteith(1973), p37.    
!---------------------------------------------------------------------- 
!                                                                       
  102 esky = 0.53 + 0.06*sqrt(em) !brunts(1932) com correcao de Jacobs(1978)(SiB&_original) 
      radn(3,2) = esky*(1.+0.2*(cloud*cloud))*stefan*tm**4
      go to 200
  104 esky = 1.24*((em/tm)**0.1428) !Brutsaert(1975)com correcao de Jacobs(1978)	
      radn(3,2) = esky*stefan*tm**4*(1.+0.2*(cloud*cloud))
      go to 200
  105 esky =(0.26*exp(-0.00077*((273-tm)**2))) !Idso&Jackson(1969)c/correcaoJacobs (1978)
      radn(3,2) = stefan*tm**4*(1-esky)*(1.+0.2*(cloud*cloud))
      go to 200
  106 esky = 0.000009 !Swinbank (1963) com correcao de Jacobs (1978)	 
      radn(3,2) = esky*stefan*tm**6*(1.+0.2*(cloud*cloud))
      go to 200
  107 esky = 0.625*(((em*100)/tm)**0.131) !Duarte (2006)com correcao de Duarte (2006)
      radn(3,2)  =  esky*(1.+0.242*((cloud)**0.583))*stefan*tm**4
      go to 200
  108 esky = 0.576*(((em*100)/tm)**0.202) !Kruk(2008) com correcao de Kruk(2008)
      radn(3,2) = esky*(1.+0.1007*((cloud)**0.9061))*stefan*tm**4
      go to 200 
!                                                                       
  103 continue 
!                                                                       
!---------------------------------------------------------------------- 
!     downward long-wave flux calculated as residual from measured      
!     net radiation and outgoing longwave radiation.                    
!                                                                       
!     calculation of absorbed fractions of radiation ( expendable )     
!---------------------------------------------------------------------- 
!                                                                       
      do 2000 iwave = 1, 2 
!                                                                       
      rab(2,iwave,1) =  ( 1. - vcover ) *                               &
     &  ( radn(iwave,1) * ( 1. - albedo(2,iwave,1) ) )                  
      rab(2,iwave,2) =  ( 1. - vcover ) *                               &
     &    radn(iwave,2) * ( 1. - albedo(2,iwave,2) )                    
!                                                                       
      rab(2,iwave,1) = rab(2,iwave,1) + vcover *                        &
     &  ( radn(iwave,1) * ( tranc1(iwave) * ( 1. - albedo(2,iwave,1) ) +&
     &    tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) ) )                
      rab(2,iwave,2) = rab(2,iwave,2) + vcover *                        &
     &    radn(iwave,2) * tranc2(iwave) * ( 1. - albedo(2,iwave,2) )    
!                                                                       
      rab(1,iwave,1) =  vcover *                                        &
     &    radn(iwave,1) * ( ( 1. - albedo(1,iwave,1) ) -                &
     &    tranc1(iwave) * ( 1. - albedo(2,iwave,1) ) -                  &
     &    tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) )                  
      rab(1,iwave,2) =  vcover *                                        &
     &    radn(iwave,2) * ( ( 1. - albedo(1,iwave,2) ) -                &
     &    tranc2(iwave) * ( 1. - albedo(2,iwave,2) ) )                  
 2000 continue 
!                                                                       
      swab = rab(1,1,1) + rab(1,1,2) + rab(1,2,1) + rab(1,2,2) +        &
     &       rab(2,1,1) + rab(2,1,2) + rab(2,2,1) + rab(2,2,2)          
      swup = swdown - swab 
      radn(3,2) = rnetm - swab + zlwup 
!                                                                       
  200 continue 
!                                                                       
      return 
      END                                           
                                                                        
!====================================================================== 
!                                                                       
      subroutine endtem (ipbl) 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!      calculation of ea, ta, ra, rb, rd and soil moisture stress       
!      for the beginning of the time step                               
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!                        modifications                                  
!                                                                       
!     (1)     : change in cog1,cog2,cogs1 to allow soil evaporation     
!               from beneath ground cover canopy.                       
!                                                                       
!     (2)     : change in temperature tgs to account for patchy snow.   
!               a bulk (area-weighted) temperature is substituted for   
!               all energy budget calculations. energy used to warm     
!               exposed patches is subtracted from snow evaporation.    
!                                                                       
!     (3)     : inclusion of randall-sellers backward implicit scheme   
!               for calculating dtc, dtg, dth, dqm. option remains to   
!               use original dtc, dtg scheme only using parameter ipbl. 
!                                                                       
!     (4)     : inclusion of integrated canopy  photosynthesis -        
!               conductance model. note that soil moisture stress is    
!               modelled as a chronic condition, while the relative     
!               humidity term is solved within a feedback loop.         
!               reference : SE-92                                       
!                                                                       
!=======================================================================
!                                                                       
!     subroutines  called : rasite --> unstab,stab,rafcal               
!     -------------------   rbrd                                        
!                           phosib --> cycalc-sortin                    
!                           delrn                                       
!                           delhf                                       
!                           delef                                       
!                           sibslv --> gauss                            
!                           dtcdtg                                      
!                           newton                                      
!-----------------------------------------------------------------------
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!       ect            canopy transpiration (j m-2)                     
!       eci            canopy interception loss (j m-2)                 
!       egs            ground evaporation (j m-2)                       
!       egi            ground interception loss (j m-2)                 
!       ec             ect + eci                                        
!       eg             egs + egi                                        
!       hc             canopy sensible heat flux (j m-2)                
!       hg             ground sensible heat flux (j m-2)                
!       chf            canopy heat storage flux (j m-2)                 
!       shf            soil heat storage flux (j m-2)                   
!       fc             canopy dew indicator                             
!       fg             ground dew indicator                             
!       heaten         heat loss to snow melt process (j m-2)           
!                                                                       
!++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++
!                                                                       
!       tsnow          snow temperature (k)                             
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      use comsibc 
      use pardif
      implicit none
!                                                                       
      integer :: ipbl
      real (kind=8) :: arean
      real (kind=8) :: aven
      real (kind=8) :: coct
      real (kind=8) :: cogs1
      real (kind=8) :: cogs2
      real (kind=8) :: d1
      real (kind=8) :: darea
      real (kind=8) :: dewc
      real (kind=8) :: dewg
      real (kind=8) :: ecf
      real (kind=8) :: ecidif
      real (kind=8) :: ecpot
      real (kind=8) :: egf
      real (kind=8) :: egidif
      real (kind=8) :: egit
      real (kind=8) :: egpot
      real (kind=8) :: egsmax
      real (kind=8) :: finc
      real (kind=8) :: hrr
      integer :: i
      integer :: icount
      integer :: ifirst
      integer :: iwalk
      integer :: lx
      integer :: nonpos
      integer :: nox
      real (kind=8) :: rsnow
      real (kind=8) :: t1
      real (kind=8) :: t2
      real (kind=8) :: taen
      real (kind=8) :: tsnow
      real (kind=8) :: y
      real (kind=8) :: hend
!            
!hrr  introduzido p/ evitar warning compilacao (variable used and not defined)
      rsnow = 0. 
      tsnow = tg 
!hrr                                                                    
      ifirst = 1 
      icount = 0 
!                                                                       
      fc = 1. 
      fg = 1. 
      ta = (tgs+tc)/2. 
      ea = em 
      ht = 0. 
!                                                                       
 1000 continue 
      icount = icount + 1 
!                                                                       
      call rasite 
!                                                                       
      call rbrd 
!                                                                       
      call phosib 
!                                                                       
      if ( icount .le. 4 ) go to 1000 
!                                                                       
      ifirst = 0 
      call delrn 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     dew calculation : dew condition is set at beginning of time step. 
!     if surface changes state during time step, latent heat flux is    
!     set to zero.                                                      
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      if ( ea .gt. etc ) fc = 0. 
      if ( ea .gt. etgs) fg = 0. 
!                                                                       
!---------------------------------------------------------------------- 
!     start of non-neutral resistance calculation loop                  
!---------------------------------------------------------------------- 
!                                                                       
      i = 0 
!                                                                       
!---------------------------------------------------------------------- 
!         initialize newton-raphson iterative routine                   
!                    for ra calculation                                 
!---------------------------------------------------------------------- 
!                                                                       
                                                                        
                    nox = 0 
                 nonpos = 0 
                  iwalk = 0 
                     lx = 2 
                   finc = 50. 
 2000 continue 
!                                                                       
      call rasite 
!                                                                       
      call delhf 
!                                                                       
      call delef 
!                                                                       
      if (ipbl .eq. 0 ) call dtcdtg 
!                                                                       
      if (ipbl .eq. 1 ) call sibslv 
!                                                                       
!-----------------------------------------------------------------------
!     calculation of evaporative potentials (ecpot, egpot) and          
!        interception losses; fluxes in j m**-2.  ecidif and egidif     
!        hold the excess energy if all intercepted water is evaporated  
!        during the timestep.  these energies are later added to the    
!        sensible heat fluxes.                                          
!                                                                       
!      eci         (e-wc)  : equation (59) , SE-86                      
!      egi         (e-wg)  : equation (60) , SE-86                      
!-----------------------------------------------------------------------
!                                                                       
!     check if interception loss term has exceeded canopy storage       
!---------------------------------------------------------------------- 
!                                                                       
      ecpot = ( (etc-ea) + (  getc-deadtc)*dtc-deadtg*dtg-deadqm*dqm ) 
      eci = ecpot * wc /(2.*rb) * rcp/psy * dtt 
      ecidif=max(0.0,(eci-(snoww(1)+capac(1))*1.e3*hlat)) 
      eci   =min(eci,(    (snoww(1)+capac(1))*1.e3*hlat)) 
!                                                                       
      egpot = ( (etgs-ea) + (getgs-deadtg)*dtg-deadtc*dtc-deadqm*dqm ) 
      egi = egpot/rd*rcp/psy*dtt * ( wg*(1.-areas) + areas ) 
      egidif=                                                           &
     &  max(0.0, egi-(snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)       
      egit  =                                                           &
     &  min(egi,     (snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)       
!                                                                       
!---------------------------------------------------------------------- 
!     calculation of interception loss from ground-snow. if snow patch  
!     shrinks, energy is taken from egi to warm exposed soil to tgs.    
!---------------------------------------------------------------------- 
!                                                                       
      t1 = snoww(2) - 1./asnow 
      t2 = max( 0., t1 ) 
      aven = egi - t2*hlat*1.e3/snofac 
      if ( (t1-t2)*egi .gt. 0. ) aven = egi 
      darea = aven/( (tsnow-tg)*csoil - 1./asnow*hlat*1.e3/snofac) 
      arean = areas + darea 
      egidif = egidif - min( 0., arean )*asnow*hlat*1.e3/snofac*rsnow 
      darea = max( darea, -areas ) 
      darea = min( 1.-areas, darea ) 
      heaten = (tsnow-tg)*csoil*darea*rsnow 
      egit = egit + ( egi - heaten - egidif )*rsnow 
      egi = egit 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      d1 = 1./ra + 1./rb + 1./rd 
      taen = ( (tgs+dtg)/rd + (tc+dtc)/rb + tm/ra ) / d1 
      hend = ( taen - tm ) * rcp / ra + (ecidif + egidif)/dtt 
      y = ht - hend 
      i = i + 1 
      if ( i .gt. itrunk ) go to 200 
!                                                                       
      call newton(ht,y,finc,nox,nonpos,iwalk,lx) 
      if(nox.eq.0)go to 2000 
!                                                                       
  200  continue 
!                                                                       
!---------------------------------------------------------------------- 
!     exit from non-neutral calculation                                 
!     evapotranspiration fluxes calculated first ( j m-2 )              
!                                                                       
!---------------------------------------------------------------------- 
!     calculation of transpiration and soil evaporation fluxes for the  
!        end of the timestep. see figure (2) of se-86.                  
!                                                                       
!      ect         (e-c)   : equation (64) , SE-86                      
!      egs         (e-s)   : equation (66) , SE-86                      
!---------------------------------------------------------------------- 
!                                                                       
      hrr = hr 
      if ( fg .lt. .5 ) hrr = 1. 
!                                                                       
      coct = (1.-wc)/ ( rst*fc + 2.*rb ) 
      cogs1 = (1.-areas)/(rd+rsoil*fg)*(1.-wg)*hrr 
      cogs2 = cogs1 / hrr 
!                                                                       
      ect = ecpot * coct * rcp/psy * dtt 
      egs = (etgs + getgs*dtg ) * cogs1                                 &
     &      - ( ea + deadtg*dtg + deadtc*dtc + deadqm*dqm ) * cogs2     
      egs = egs * rcp/psy * dtt 
      egsmax = www(1) / 2. * zdepth(1) * poros(1) * hlat * 1000. 
      egidif = egidif + max( 0., egs - egsmax ) 
      egs = min ( egs, egsmax ) 
!                                                                       
!---------------------------------------------------------------------- 
!     sensible heat flux calculated with latent heat flux correction    
!---------------------------------------------------------------------- 
!                                                                       
!     calculation of sensible heat fluxes for the end of the timestep.  
!        see figure (2) of se-86.  note that interception loss excess   
!        energies (ecidif, egidif) are added.                           
!                                                                       
!      hc          (hc)    : equation (63) , SE-86                      
!      hg          (hgs)   : equation (65) , SE-86                      
!---------------------------------------------------------------------- 
!                                                                       
      hc = hc + (hcdtc*dtc + hcdtg*dtg + hcdth*dth)*dtt + ecidif 
      hg = hg + (hgdtc*dtc + hgdtg*dtg + hgdth*dth)*dtt + egidif 
!                                                                       
!---------------------------------------------------------------------- 
!     test of dew condition. latent heat fluxes set to zero if sign     
!     of flux changes over time step.excess of energy donated to sensibl
!     heat flux.                                                        
!     calculation of total latent heat fluxes,  see figure (2), se-86.  
!                                                                       
!      ec          (ec)    : equation (63) , SE-86                      
!      eg          (eg)    : equation (65) , SE-86                      
!---------------------------------------------------------------------- 
!                                                                       
      ecf = sign( 1d0, ecpot ) 
      egf = sign( 1d0, egpot ) 
      dewc = fc * 2d0 - 1d0 
      dewg = fg * 2d0 - 1d0
!                                                                       
      if(dewc*ecf.gt.0.0) go to 300 
      hc = hc + eci + ect 
      eci = 0. 
      ect = 0. 
  300 if(dewg*egf.gt.0.0) go to 400 
      hg = hg + egs + egi 
      egs = 0. 
      egi = 0. 
  400 continue 
!                                                                       
      ec = eci + ect 
      eg = egi + egs 
!                                                                       
!---------------------------------------------------------------------- 
!     adjustment of : temperatures and vapor pressure                   
!                     net radiation terms                               
!                     storage heat fluxes                               
!                     longwave loss and effective surface temperature   
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      ta  = taen 
      ea = ea + deadtc*dtc + deadtg*dtg 
!                                                                       
      radt(1) = radt(1) + rncdtc*dtc + rncdtg*dtg 
      radt(2) = radt(2) + rngdtc*dtc + rngdtg*dtg 
!                                                                       
!---------------------------------------------------------------------- 
!     calculation of storage heat fluxes                                
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      chf = ccx / dtt * dtc 
      shf = cg / dtt * dtg + timcon*cg*2. * ( tgs+dtg - td ) 
!H                                                                      
      gflux = timcon*cg*2. * ( tgs+dtg - td ) 
!H                                                                      
!                                                                       
      zlwup = zlwup - rncdtc * dtc / 2.                                 &
     &              - rngdtg * dtg * (1.-vcover*(1.-thermk) )           
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      return 
      END                                           
!====================================================================== 
!                                                                       
!        *********    auxiliary subroutine     **********               
!                                                                       
!=======================================================================
!                                                                       
       subroutine rbrd 
!                                                                       
!=======================================================================
!                                                                       
!      calculation of rb and rd as functions of u2 and temperatures     
!                                                                       
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       rb (grb)       canopy to cas aerodynamic resistance (s m-1)     
!       rd (grd)       ground to cas aerodynamic resistance (s m-1)     
!       ta (gta)       cas temperature (k)                              
!                      cas : canopy air space                           
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      implicit none
      !
      real (kind=8) :: cogr
      real (kind=8) :: cogs
      real (kind=8) :: d1
      real (kind=8) :: fac
      real (kind=8) :: fih
      real (kind=8) :: temdif
!                                                                       
!-----------------------------------------------------------------------
!     rb : equation (a9), SE-86                                         
!-----------------------------------------------------------------------
!                                                                       
      temdif = max( 0.1,  tc-tm ) 
      fac = zlt/890.* sqrt( sqrt(temdif/0.05)) 
      rb  = 1.0/(sqrt(u2)/rbc+fac) 
!                                                                       
!-----------------------------------------------------------------------
!     rd : equation (a15), se-86                                        
!-----------------------------------------------------------------------
!                                                                       
      temdif = max( 0.1, tgs-tm ) 
      fih = sqrt( 1. + 9. * gx * temdif * z2 / tgs / ( u2*u2) ) 
      rd  = rdc / u2 / fih 
!                                                                       
!-----------------------------------------------------------------------
!     calculation of ta, ht and effective soil aero-surface conductance,
!        see equation (66) of SE-86 with snow area term added           
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      d1 = 1./ra + 1./rb + 1./rd 
      ta = ( tgs/rd + tc/rb + tm/ra ) / d1 
      ht = ( ta - tm ) * rcp / ra 
!                                                                       
      cogr = (1.-wg)/(rsoil*fg+rd) 
      cogs =  wg/rd 
      cog1 = (cogs + cogr*hr) * (1.-areas) + areas/rd 
      cog2 = (cogs + cogr   ) * (1.-areas) + areas/rd 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine phosib 
!                                                                       
!                                                                       
!=======================================================================
!                                                                       
!     calculation of canopy photosynthetic rate using the integrated    
!     model relating assimilation and stomatal conductance.             
!     method uses numerical solution based on extrapolation from error  
!     versus co2i line.                                                 
!     units are converted from mks to biological units in this routine. 
!     base reference :  SE-92                                           
!                                                                       
!                          units                                        
!                         -------                                       
!                                                                       
!      pco2m, pco2a, pco2i, po2m                : pascals               
!      co2a, co2s, co2i, h2oa, h2os, h2oa       : mol mol-1             
!      vmax0, respn, assim, gs, gb, ga, pfd     : mol m-2 s-1           
!      effcon                                   : mol co2 mol quanta-1  
!      gcan, 1/rb, 1/ra, 1/rst                  : m s-1                 
!      evapkg                                   : kg m-2 s-1            
!      q                                        : kg kg-1               
!                                                                       
!                       conversions                                     
!                      -------------                                    
!                                                                       
!      1 mol h2o           = 0.018 kg                                   
!      1 mol co2           = 0.044 kg                                   
!      h2o (mol mol-1)     = ea / psur ( mb mb-1 )                      
!      h2o (mol mol-1)     = q*mm/(q*mm + 1)                            
!      gs  (co2)           = gs (h2o) * 1./1.6                          
!      gs  (mol m-2 s-1 )  = gs (m s-1) * 44.6*tf/t*p/po                
!      par (mol m-2 s-1 )  = par(w m-2) * 4.6*1.e-6                     
!      mm  (molair/molh2o) = 1.611                                      
!                                                                       
!                                                                       
!                         output                                        
!                      -------------                                    
!                                                                       
!      assimn              = canopy net assimilation rate               
!      ea                  = canopy air space vapor pressure            
!      1/rst               = canopy conductance                         
!      pco2i               = internal co2 concentration                 
!      respc               = canopy respiration                         
!      respg               = ground respiration                         
!      rstfac(4)      canopy resistance stress factors                  
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!         rstfac(1) ( f(h-s) )               : equation (17,18), SE-92  
!         rstfac(2) ( f(soil) )              : equation (12 mod), SE-89 
!         rstfac(3) ( f(temp) )              : equation (5b)   , CO-92  
!         rstfac(4) ( f(h-s)*f(soil)*f(temp))                           
!                                                                       
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      implicit none
!                                                                       
      !dimension pco2y(6), eyy(6)
      real (kind=8) :: pco2y(6)
      real (kind=8) :: eyy(6)
!
      real (kind=8) :: bintc
      real (kind=8) :: c3
      real (kind=8) :: c4
      real (kind=8) :: fparkk
      real (kind=8) :: gah2o
      real (kind=8) :: gammas
      real (kind=8) :: gbh2o
      real (kind=8) :: gog2
      real (kind=8) :: h2oi
      real (kind=8) :: h2om
      real (kind=8) :: h2os
      real (kind=8) :: h2osl
      integer :: ic
      integer :: ic2
      real (kind=8) :: omss
      real (kind=8) :: par
      real (kind=8) :: park
      real (kind=8) :: pfd
      real (kind=8) :: qt
      real (kind=8) :: range
      real (kind=8) :: respn
      real (kind=8) :: rrkk
      real (kind=8) :: scatg
      real (kind=8) :: scatp
      real (kind=8) :: spfy
      real (kind=8) :: temph
      real (kind=8) :: templ
      real (kind=8) :: tprcor
      real (kind=8) :: vm
      real (kind=8) :: xdry
      real (kind=8) :: xwet
      real (kind=8) :: zko
      real (kind=8) :: gog1
      real (kind=8) :: h2oa
      real (kind=8) :: zkc
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!HR      respg = 0.                                                     
                                                                        
      if (irespg.eq.0) respg = 0. 
                                                                        
                                                                        
!...  Tropical forest RJA - Patrick Meir (pers com 1994) (molCO2 m-2 s-1
      if (irespg.eq.1) respg = 1.e-06 * exp (0.084*(td-273.15) - 0.23) 
                                                                        
!HR..  Tropical forest k83 - ajuste sazonal: aumento est.chuvosa, reduz est.seca 
!	(molCO2 m-2 s-1)                                                      
      if (irespg.eq.3) then 
      xwet = 1. 
      if (www(1).lt.0.95) xwet = www(1) 
      if (www(2).lt.0.9) xwet = (min(www(1),0.8)**3.) 
      xdry = 1. - xwet 
                                                                        
      respg = 1.e-06 *                                                  &
     &  (    xwet * exp(0.090*(td-273.15)-0.01)+                        &
     &       xdry * exp(0.080*(td-273.15)-0.30)   - 2.0 )               
                                                          ! (avg ~ 4a8 K
      endif 
                                                                        
!...  Cerrado ss PDG - Rocha 2002 (molCO2 m-2 s-1)                      
      if (irespg.eq.2) respg = 1.e-06 * exp (0.159*(td-273.15) - 1.886) 
                                                                        
!H                                                                      
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      c3     = 0. 
      if( effcon .gt. 0.07 ) c3 = 1. 
      c4     = 1. - c3 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!                                                                       
!     calculation of canopy par use parameter.                          
!                                                                       
!     fparkk      (pi)     : equation (31) , SE-92                      
!-----------------------------------------------------------------------
!                                                                       
      scatp    =     green   * ( tran(1,1) + ref(1,1) )                 &
     &         +( 1.-green ) * ( tran(1,2) + ref(1,2) )                 
!                                                                       
      scatg    = tran(1,1) + ref(1,1) 
      park = sqrt(1.-scatp) * gmudmu 
      fparc = 1. - exp ( -park*zlt ) 
      fparkk   = fparc / park * green 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     q-10 temperature effects :                                        
!      qt          (qt)    : table (2)     , SE-92                      
!      qt for vm changed to 2.1                                         
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      qt = 0.1*( tc - trop ) 
      respn = respcp * vmax0 * rstfac(2) 
      respc = respn * 2.0**qt / ( 1. + exp( trda*(tc-trdm )) ) 
      vm = vmax0 * 2.1**qt 
!                                                                       
      templ = 1. + exp(slti*(hlti-tc)) 
      temph = 1. + exp(shti*(tc-hhti)) 
      rstfac(3) = 1./( templ*temph) 
      vm    = vm/temph * rstfac(2) * c3                                 &
     &      + vm * rstfac(2)*rstfac(3) * c4                             
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     Michaelis-Menten constants for co2 and o2, co2/o2 specificity,    
!     compensation point                                                
!                                                                       
!      zkc          (kc)     : table (2)     , SE-92                    
!      zko          (ko)     : table (2)     , SE-92                    
!      spfy         (s)      : table (2)     , SE-92                    
!      gammas       (gamma-*): table (2)     , SE-92                    
!      omss         (omega-s): equation (13) , SE-92                    
!      bintc        (b*zlt)  : equation (35) , SE-92                    
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      zkc = 30. * 2.1**qt 
      zko = 30000. * 1.2**qt 
      spfy = 2600. * 0.57**qt 
      gammas = 0.5 * po2m/spfy * c3 
      pfd    = 4.6*1.e-6 * gmudmu * ( radn(1,1)+radn(1,2) ) 
!                                                                       
      h2oi   = etc/psur 
      h2oa   =  ea/psur 
      h2om   =  em/psur 
      h2osl  =etgs/psur 
!                                                                       
      tprcor = tf*psur*100./1.013e5 
!                                                                       
      gbh2o  = 0.5/rb * 44.6*tprcor/tc 
      gah2o  = 1.0/ra * 44.6*tprcor/tm 
      gog1   = cog1   * 44.6*tprcor/tgs 
      gog2   = cog2   * 44.6*tprcor/tgs 
!                                                                       
      rrkk   = zkc*( 1. + po2m/zko ) * c3 + vmax0/5.* ( 1.8**qt) * c4 
      par    = pfd*effcon*( 1.-scatg ) 
      bintc  = binter*zlt*green*max(0.1,rstfac(2)) 
!                                                                       
      omss  = ( vmax0/2.0 ) * ( 1.8**qt )/templ * rstfac(2) * c3        &
     &                                   + rrkk * rstfac(2) * c4        
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     first guess is midway between compensation point and maximum      
!     assimilation rate.                                                
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      range    = pco2m * ( 1. - 1.6/gradm ) - gammas 
!                                                                       
      do 1000 ic = 1, 6 
      pco2y(ic) = 0. 
      eyy(ic) = 0. 
 1000 continue 
!                                                                       
      do 2000 ic = 1, 6 
!                                                                       
      ic2 = ic 
!                                                                       
      call       sortin( eyy, pco2y, range, gammas, ic ) 
!                                                                       
      call       cycalc( fparkk, vm, gradm, bintc, atheta, btheta,      &
     &                   gah2o, gbh2o, gog1, gog2, wc,                  &
     &                   h2oi, h2om, h2osl, par, pco2m, psur,           &
     &                   gammas, respc, respg, rrkk, omss, c3, c4,      &
     &                   pco2y(ic), eyy(ic), gsh2o, assimn, h2os, h2oa )
!                                                                       
      if( abs(eyy(ic)) .lt. 0.1 ) go to 100 
!                                                                       
 2000 continue 
!                                                                       
  100 pco2i = pco2y(ic2) 
!                                                                       
      rstfac(1) = h2os/h2oi 
      rstfac(4) = rstfac(1)*rstfac(2)* rstfac(3) 
      rst   = min( 1.e6, 1./( gsh2o*tc/( 44.6*tprcor) ) ) 
      ea    = h2oa*psur 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine cycalc( fparkk, vm, gradm, bintc, atheta, btheta,      &
     &                   gah2o, gbh2o, gog1, gog2, wc,                  &
     &                   h2oi, h2om, h2osl, par, pco2m, psur,           &
     &                   gammas, respc, respg, rrkk, omss, c3, c4,      &
     &                   pco2i, eyy, gsh2o, assimn, h2os, h2oa )        
!                                                                       
!=======================================================================
!                                                                       
!     calculation equivalent to steps in figure 4 of SE-92              
!     c4 calculation based on CO-92.                                    
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       pco2i          canopy internal co2 concentration (mol mol-1)    
!       gsh2o          canopy conductance (mol m-2 s-1)                 
!       h2os           canopy surface h2o concentration (mol mol-1)     
!                                                                       
!++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++
!                                                                       
!       omc            rubisco limited assimilation (mol m-2 s-1)       
!                        (omega-c): equation (11) , SE-92               
!       ome            light limited assimilation (mol m-2 s-1)         
!                        (omega-e): equation (12) , SE-92               
!       oms            sink limited assimilation (mol m-2 s-1)          
!       co2s           canopy surface co2 concentration (mol mol-1)     
!                        equation (18c) , SE-92                         
!       assimn         (a-n)    :  equation (14,15), SE-92              
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      implicit none
      real (kind=8) :: fparkk
      real (kind=8) :: vm
      real (kind=8) :: gradm
      real (kind=8) :: bintc
      real (kind=8) :: atheta
      real (kind=8) :: btheta
      real (kind=8) :: gah2o
      real (kind=8) :: gbh2o
      real (kind=8) :: gog1
      real (kind=8) :: gog2
      real (kind=8) :: wc
      real (kind=8) :: h2oi
      real (kind=8) :: h2om
      real (kind=8) :: h2osl
      real (kind=8) :: par
      real (kind=8) :: pco2m
      real (kind=8) :: psur
      real (kind=8) :: gammas
      real (kind=8) :: respc
      real (kind=8) :: respg
      real (kind=8) :: rrkk
      real (kind=8) :: omss
      real (kind=8) :: c3
      real (kind=8) :: c4
      real (kind=8) :: pco2i
      real (kind=8) :: eyy
      real (kind=8) :: gsh2o
      real (kind=8) :: assimn
      real (kind=8) :: h2os
      real (kind=8) :: h2oa
      real (kind=8) :: alpha
      real (kind=8) :: aquad
      real (kind=8) :: assim
      real (kind=8) :: assmt
      real (kind=8) :: beta
      real (kind=8) :: bquad
      real (kind=8) :: co2s
      real (kind=8) :: co2st
      real (kind=8) :: cquad
      real (kind=8) :: div2
      real (kind=8) :: div3
      real (kind=8) :: hcdma
      real (kind=8) :: omc
      real (kind=8) :: ome
      real (kind=8) :: omp
      real (kind=8) :: oms
      real (kind=8) :: pco2a
      real (kind=8) :: pco2in
      real (kind=8) :: sqrtin
!        
      omc = vm  * ( pco2i-gammas )/( pco2i + rrkk ) * c3 + vm * c4 
      ome = par * ( pco2i-gammas )/( pco2i+2.*gammas ) * c3 + par * c4 
      sqrtin= max( 0., ( (ome+omc)**2 - 4.*atheta*ome*omc ) ) 
      omp   = ( ( ome+omc ) - sqrt( sqrtin ) ) / ( 2.*atheta ) 
      oms   = omss * c3 + omss*pco2i * c4 
      sqrtin= max( 0., ( (omp+oms)**2 - 4.*btheta*omp*oms ) ) 
      assim = ( ( oms+omp ) - sqrt( sqrtin ) ) / ( 2.*btheta ) 
      assimn= ( assim - respc) * fparkk 
!                                                                       
!-----------------------------------------------------------------------
!     gah2o bottom stopped to prevent negative values of pco2a          
!-----------------------------------------------------------------------
!                                                                       
        pco2a = pco2m - (1.4/max(0.446,gah2o) *                       &
     &             (assimn - respg)* psur*100.)                         
                                                                        
      co2s  = pco2a/(psur*100.) - 1.4*assimn/gbh2o 
!                                                                       
      assmt = max( 1.e-12, assimn ) 
      co2st = min( co2s, pco2a/(psur*100.) ) 
      co2st = max( co2st,1./(psur*100.) ) 
!                                                                       
      div2  = gah2o + gbh2o + gog2 
      hcdma = h2oi*co2st / ( gradm*assmt ) 
      alpha = hcdma*gbh2o*(1.-wc) / div2 
      beta  = ( -hcdma*bintc*gbh2o*(1.-wc) + h2oi*gbh2o*wc              &
     &        + h2osl*gog1 + h2om*gah2o ) / div2                        
      aquad = hcdma 
      bquad = gbh2o*( hcdma-alpha ) - h2oi - bintc*hcdma 
      cquad = -gbh2o*( hcdma*bintc + beta ) 
!                                                                       
      sqrtin= max( 0., ( bquad**2 - 4.*aquad*cquad ) ) 
      gsh2o = ( -bquad + sqrt ( sqrtin ) ) / (2.*aquad) 
      h2os  = ( gsh2o-bintc ) * hcdma 
      h2os  = min( h2os, h2oi ) 
      h2os  = max( h2os, 1.e-7) 
      gsh2o = h2os/hcdma + bintc 
      div3  = gbh2o*gsh2o/(gbh2o+gsh2o)*(1.-wc) + gbh2o*wc + gog2       &
     &        + gah2o                                                   
      h2oa  = ( ( h2oi - h2oi*gsh2o/(gbh2o+gsh2o) )*gsh2o*(1.-wc)       &
     &        + h2oi*gbh2o*wc + h2osl*gog1 + h2om*gah2o ) / div3        
      h2os  = ( h2oa*gbh2o + h2oi*gsh2o )/( gbh2o + gsh2o ) 
!                                                                       
!-----------------------------------------------------------------------
!     implied value of co2i derived from assimilation rate and stomatal 
!     conductance.                                                      
!-----------------------------------------------------------------------
!                                                                       
      pco2in = ( co2s - 1.6 * assimn / gsh2o )*psur*100. 
      eyy = pco2i - pco2in 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine sortin( eyy, pco2y, range, gammas, ic ) 
!                                                                       
!=======================================================================
!                                                                       
!-----------------------------------------------------------------------
                                                                        
!     arranges successive pco2/error pairs in order of increasing pco2. 
!     estimates next guess for pco2 using combination of linear and     
!     quadratic fits.                                                   
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!                                                                       
      implicit none
      !dimension eyy(6), pco2y(6) 
      real (kind=8) :: eyy(6)
      real (kind=8) :: pco2y(6)
      !
      real (kind=8) :: range
      real (kind=8) :: gammas
      integer :: ic
      real (kind=8) :: a
      real (kind=8) :: ac1
      real (kind=8) :: ac2
      real (kind=8) :: aterm
      real (kind=8) :: b
      real (kind=8) :: bc1
      real (kind=8) :: bc2
      real (kind=8) :: bterm
      real (kind=8) :: emin
      integer :: i
      integer :: i1
      integer :: i2
      integer :: i3
      integer :: is
      integer :: isp
      integer :: j
      integer :: n
      real (kind=8) :: pco2b
      real (kind=8) :: pco2yl
      real (kind=8) :: pco2yq
      real (kind=8) :: pmin
      real (kind=8) :: cc1
      real (kind=8) :: cc2
      real (kind=8) :: cterm
      integer :: ix
      !                                                                       
      if( ic .ge. 4 ) go to 500 
      pco2y(1) = gammas + 0.5*range 
      pco2y(2) = gammas + range*( 0.5 - 0.3*sign(1d0,eyy(1)) ) 
      pco2y(3) = pco2y(1)                                               &
     &          - (pco2y(1)-pco2y(2))/(eyy(1)-eyy(2)+1.e-10)*eyy(1)     
!                                                                       
      pmin = min( pco2y(1), pco2y(2) ) 
      emin = min(   eyy(1),   eyy(2) ) 
      if ( emin .gt. 0. .and. pco2y(3) .gt. pmin ) pco2y(3) = gammas 
      go to 200 
  500 continue 
!                                                                       
      n = ic - 1 
      do 1000 j = 2, n 
      a = eyy(j) 
      b = pco2y(j) 
      do 2000 i = j-1,1,-1 
      if(eyy(i) .le. a ) go to 100 
      eyy(i+1) = eyy(i) 
      pco2y(i+1) = pco2y(i) 
 2000 continue 
      i = 0 
  100 eyy(i+1) = a 
      pco2y(i+1) = b 
 1000 continue 
!                                                                       
      pco2b = 0. 
      is    = 1 
      do 3000 ix = 1, n 
      if( eyy(ix) .lt. 0. ) pco2b = pco2y(ix) 
      if( eyy(ix) .lt. 0. ) is = ix 
 3000 continue 
      i1 = is-1 
      i1 = max0(1, i1) 
      i1 = min0(n-2, i1) 
      i2 = i1 + 1 
      i3 = i1 + 2 
      isp   = is + 1 
      isp = min0( isp, n ) 
      is = isp - 1 
!                                                                       
      pco2yl=pco2y(is)                                                  &
     &      - (pco2y(is)-pco2y(isp))/(eyy(is)-eyy(isp))*eyy(is)         
!                                                                       
!---------------------------------------------------------------------- 
!   method using a quadratic fit                                        
!---------------------------------------------------------------------- 
!                                                                       
      ac1 = eyy(i1)*eyy(i1) - eyy(i2)*eyy(i2) 
      ac2 = eyy(i2)*eyy(i2) - eyy(i3)*eyy(i3) 
      bc1 = eyy(i1) - eyy(i2) 
      bc2 = eyy(i2) - eyy(i3) 
      cc1 = pco2y(i1) - pco2y(i2) 
      cc2 = pco2y(i2) - pco2y(i3) 
      bterm = (cc1*ac2-cc2*ac1)/(bc1*ac2-ac1*bc2) 
      aterm = (cc1-bc1*bterm)/ac1 
      cterm = pco2y(i2) - aterm*eyy(i2)*eyy(i2) - bterm*eyy(i2) 
      pco2yq= cterm 
      pco2yq= max( pco2yq, pco2b ) 
      pco2y(ic) = ( pco2yl+pco2yq)/2. 
!                                                                       
  200 continue 
!                                                                       
      pco2y(ic) = max ( pco2y(ic), 0.01 ) 
!                                                                       
      return 
      END                                           
!                                                                       
!====================================================================== 
!                                                                       
      subroutine delrn 
!                                                                       
!====================================================================== 
!                                                                       
!     partial derivatives of radiative and sensible heat fluxes         
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      use pardif
      implicit none
!                                                                       
      real (kind=8) :: fac1
      real (kind=8) :: fac2
      real (kind=8) :: tc3
      real (kind=8) :: tg3
!
      tc3 = tc * tc * tc 
      tg3 = tgs * tgs * tgs 
      fac1 = ( 1.-thermk ) * vcover 
      fac2 = 1. 
!                                                                       
      rncdtc = - 2. * 4. * fac1 * stefan * tc3 
      rncdtg = 4. * fac1 * fac2 * stefan * tg3 
!                                                                       
      rngdtg = - 4. * fac2 * stefan * tg3 
      rngdtc = 4. * fac1 * fac2 * stefan * tc3 
!                                                                       
      return 
      END                                           
!====================================================================== 
!                                                                       
      subroutine delhf 
!                                                                       
!====================================================================== 
!                                                                       
!     calculation of partial derivatives of canopy and ground sensible  
!     heat fluxes with respect to tc, tgs, and theta-m.                 
!     calculation of initial sensible heat fluxes.                      
!                                                                       
!=======================================================================
!                                                                       
!                                                                       
!       hc             canopy sensible heat flux (j m-2)                
!       hg             ground sensible heat flux (j m-2)                
!       hcdtc          dhc/dtc                                          
!       hcdtg          dhc/dtgs                                         
!       hcdth          dhc/dth                                          
!       hgdtc          dhg/dtc                                          
!       hgdtg          dhg/dtgs                                         
!       hgdth          dhg/dth                                          
!       aac            dh/dtc                                           
!       aag            dh/dtgs                                          
!       aam            dh/dth                                           
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
!     fluxes expressed in joules m-2                                    
!                                                                       
!      hc  ( h-c ) : equation(63), SE-86                                
!      hg  ( h-g ) : equation(65), SE-86                                
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      use pardif
      implicit none
!
      real (kind=8) :: d1
      real (kind=8) :: hcdqm
      real (kind=8) :: hgdqm
!                                                                       
      d1 = 1./ra + 1./rb + 1./rd 
      ta = ( tgs/rd + tc/rb + tm/ra ) / d1 
!                                                                       
      hc = rcp * ( tc - ta ) / rb * dtt 
      hg = rcp * ( tgs - ta ) / rd * dtt 
!---------------------------------------------------------------------- 
!                                                                       
!      n.b.      fluxes expressed in joules m-2                         
!                                                                       
!      hcdtc     (dhc/dtc) : equation (14) , SA-89B                     
!      hcdtg     (dhc/dtgs): equation (14) , SA-89B                     
!      hcdth     (dhc/dth) : equation (14) , SA-89B                     
!      hgdtc     (dhg/dtc) : equation (15) , SA-89B                     
!      hgdtg     (dhg/dtgs): equation (15) , SA-89B                     
!      hgdth     (dhg/dth) : equation (15) , SA-89B                     
!      aac       (dh/dtc)  : equation (12) , SA-89B                     
!      aag       (dh/dtgs) : equation (12) , SA-89B                     
!      aam       (dh/dth)  : equation (12) , SA-89B                     
!---------------------------------------------------------------------- 
!                                                                       
      hcdtc = rcp / rb * ( 1./ra + 1./rd ) / d1 
      hcdtg = - rcp / ( rb * rd ) / d1 
!                                                                       
      hgdtg = rcp / rd * ( 1./ra + 1./rb ) / d1 
      hgdtc = - rcp / ( rd * rb ) / d1 
!                                                                       
      hcdth=- rcp/( rb*ra )/d1*bps 
      hcdqm=0.0 
!                                                                       
      hgdth=-rcp/(rd*ra)/d1*bps 
      hgdqm=0.0 
!                                                                       
      aag=1.0/(rd*d1) 
      aac=1.0/(rb*d1) 
      aam=1.0/(ra*d1)*bps 
!                                                                       
      return 
      END                                           
!====================================================================== 
!                                                                       
      subroutine delef 
!                                                                       
!====================================================================== 
!                                                                       
!     calculation of partial derivatives of canopy and ground latent    
!     heat fluxes with respect to tc, tgs, theta-m, and qm.             
!     calculation of initial latent heat fluxes.                        
!                                                                       
!      ec  ( e-c ) : equation(64), SE-86                                
!      eg  ( e-gs) : equation(66), SE-86                                
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       ec             ect + eci                                        
!       eg             egs + egi                                        
!       ecdtc          dec/dtc                                          
!       ecdtg          dec/dtgs                                         
!       ecdqm          dec/dqm                                          
!       egdtc          deg/dtc                                          
!       egdtg          deg/dtgs                                         
!       egdqm          deg/dqm                                          
!       bbc            de/dtc                                           
!       bbg            de/dtgs                                          
!       bbm            de/dqm                                           
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      use pardif
      implicit none
!                                                                       
!---------------------------------------------------------------------- 
!     modification for soil dryness : hr = rel. humidity in top layer   
!---------------------------------------------------------------------- 
      !
      real (kind=8) :: coc
      real (kind=8) :: cogr
      real (kind=8) :: cogs
      real (kind=8) :: d2
      real (kind=8) :: deadem
      real (kind=8) :: hrr
      real (kind=8) :: rcc
      real (kind=8) :: top
      real (kind=8) :: sh
      !      
      hrr = hr 
      if ( fg .lt. .5 ) hrr = 1. 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     calculation of surface resistance components, see equations (64,66
!       of SE-86                                                        
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      rcc = rst*fc + 2. * rb 
      coc = (1.-wc)/rcc + wc/(2.*rb) 
!                                                                       
      cogr = (1.-wg)/(rsoil*fg+rd) 
      cogs =  wg/rd 
      cog1 = (cogs + cogr*hrr) * (1.-areas) + areas/rd 
      cog2 = (cogs + cogr    ) * (1.-areas) + areas/rd 
!                                                                       
      d2 = 1./ra + coc + cog2 
      top = coc * etc + cog1 * etgs + em/ra 
      ea = top / d2 
!                                                                       
      ec = ( etc - ea ) * coc * rcp/psy * dtt 
      eg = ( etgs*cog1 - ea*cog2 ) * rcp/psy * dtt 
!                                                                       
      deadtc = getc * coc / d2 
      deadtg = getgs * cog1 / d2 
!                                                                       
!-----------------------------------------------------------------------
!      ecdtc     (dec/dtc) : equation (14) , SA-89B                     
!      ecdtg     (dec/dtgs): equation (14) , SA-89B                     
!      ecdqm     (dec/dqm) : equation (14) , SA-89B                     
!      egdtc     (deg/dtc) : equation (15) , SA-89B                     
!      egdtg     (deg/dtgs): equation (15) , SA-89B                     
!      egdqm     (deg/dqm) : equation (15) , SA-89B                     
!      bbc       (de/dtc)  : equation (13) , SA-89B                     
!      bbg       (de/dtgs) : equation (13) , SA-89B                     
!      bbm       (de/dqm)  : equation (13) , SA-89B                     
!-----------------------------------------------------------------------
!                                                                       
      ecdtc = ( getc - deadtc ) * coc * rcp / psy 
      ecdtg = - deadtg * coc * rcp / psy 
!                                                                       
      egdtg = ( getgs*cog1 - deadtg*cog2 ) * rcp / psy 
      egdtc = - deadtc * cog2 * rcp / psy 
!                                                                       
      sh    = epsfac * em / (psur -em ) 
      deadem=1.0/( ra*d2 ) 
      demdqm=epsfac * psur/(epsfac+sh)**2 
      deadqm=deadem*demdqm 
!                                                                       
      ecdqm=- deadqm*coc*rcp/psy 
      egdqm=-deadqm*cog2*rcp/psy 
!                                                                       
      bbg=(cog1/d2) * getgs*epsfac * psur/(psur-etgs)**2 
      bbc=(coc /d2) * getc *epsfac * psur/(psur-etc )**2 
      bbm=1.0/(ra*d2) 
!                                                                       
      return 
      END                                           
!====================================================================== 
!                                                                       
      subroutine sibslv 
!                                                                       
!                                                                       
!====================================================================== 
!                                                                       
!     solve for time changes of pbl and sib variables,                  
!     using a semi-implicit scheme.                                     
!                                                                       
!      dtc, dtg, dth, dqm  : equations(12-15) , SA-89B + radiation terms
!                                                                       
!                                                                       
!++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++
!                                                                       
!       dtc            canopy temperature increment (K)                 
!       dtg            ground surface temperature increment (K)         
!       dth            mixed layer potential temperature increment (K)  
!       dqm            mixed layer mixing ratio increment (kg kg-1)     
!                                                                       
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                       
      use comsibc 
      use pardif 
      implicit none
      !dimension pblsib(4,4),cvec(4),solvec(4),chin(4,5),work(4,5)
      
      real (kind=8) :: pblsib(4,4)
      real (kind=8) :: cvec(4)
      real (kind=8) :: solvec(4)
      real (kind=8) :: chin(4,5)
      real (kind=8) :: work(4,5)
!
      real (kind=8) :: dthb
      real (kind=8) :: dwb
      real (kind=8) :: etem
      real (kind=8) :: fths
      real (kind=8) :: fws
      real (kind=8) :: grav2
      real (kind=8) :: gv
      real (kind=8) :: psb
      integer :: i
      integer :: j
!                                                                       
      grav2 = 0.01*gx 
      gv    = grav2*rhoair/ra 
      psb   = 100. 
!                                                                       
      etem=0. 
      dthb = 0. 
      dwb = 0. 
!                                                                       
!     cvec uses provisional values of the fluxes.                       
!                                                                       
      fths=(hc+hg) / (dtt*cpair*bps) 
      fws =(ec+eg) / (dtt*hlat     ) 
!                                                                       
!     tg equation                                                       
!                                                                       
      pblsib(1,1)= cg/dtt + hgdtg+egdtg-rngdtg + timcon*cg*2.0 
      pblsib(1,2)=        + hgdtc+egdtc-rngdtc 
      pblsib(1,3)=        + hgdth 
      pblsib(1,4)=                egdqm 
!                                                                       
!     tc equation                                                       
!                                                                       
      pblsib(2,1)=        + hcdtg+ecdtg-rncdtg 
      pblsib(2,2)= ccx/dtt+ hcdtc+ecdtc-rncdtc 
      pblsib(2,3)=        + hcdth 
      pblsib(2,4)=                ecdqm 
!                                                                       
!     theta equation                                                    
!                                                                       
      pblsib(3,1)=-gv* aag 
      pblsib(3,2)=-gv* aac 
      pblsib(3,3)=-gv*(aam-1.0) + etem + psb/dtt 
      pblsib(3,4)= 0.0 
!                                                                       
!     sh equation                                                       
!                                                                       
      pblsib(4,1)=-gv*bbg 
      pblsib(4,2)=-gv*bbc 
      pblsib(4,3)= 0.0 
      pblsib(4,4)=-gv*(bbm-1.0) + etem + psb/dtt 
!                                                                       
      cvec(1) = radt(2) - hg/dtt - eg/dtt - timcon*(tgs-td)*cg*2. 
      cvec(2) = radt(1) - hc/dtt - ec/dtt 
      cvec(3) = grav2*fths + etem*dthb 
      cvec(4) = grav2*fws  + etem*dwb 
!                                                                       
!     solve 4 x 4 matrix equation                                       
!                                                                       
      do 4000 j=1,4 
      do 4000 i=1,4 
 4000 chin(i,j)=pblsib(i,j) 
      do 4100 i=1,4 
 4100 chin(i,5)=cvec(i) 
!                                                                       
      call gauss(chin,4,5,solvec,work) 
!                                                                       
      dtg=solvec(1) 
      dtc=solvec(2) 
      dth=solvec(3) 
      dqm=solvec(4) 
!                                                                       
      return 
      END                                           
!                                                                       
!====================================================================== 
!                                                                       
      subroutine dtcdtg 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     calculation of temperature tendencies assuming no interaction     
!     with the pbl : equations(69,70), SE-86                            
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
      use comsibc 
      use pardif
      implicit none
      !
      real (kind=8) :: ccodtc
      real (kind=8) :: ccodtg
      real (kind=8) :: ccorhs
      real (kind=8) :: denom
      real (kind=8) :: gcodtc
      real (kind=8) :: gcodtg
      real (kind=8) :: gcorhs
      !                                                                       
      ccodtc = ccx / dtt - rncdtc + hcdtc + ecdtc 
      ccodtg = - rncdtg + hcdtg + ecdtg 
      ccorhs = radt(1) - ( hc + ec ) / dtt 
!                                                                       
      gcodtg = cg / dtt + timcon*cg*2. - rngdtg + hgdtg + egdtg 
      gcodtc = - rngdtc + hgdtc + egdtc 
      gcorhs = radt(2) - timcon*cg*2. * ( tgs -td ) - ( hg + eg ) / dtt 
!                                                                       
      denom = ccodtc * gcodtg - ccodtg * gcodtc 
!                                                                       
      dtc = ( ccorhs * gcodtg - ccodtg * gcorhs ) / denom 
      dtg = ( ccodtc * gcorhs - ccorhs * gcodtc ) / denom 
!                                                                       
      return 
      END                                           
                                                                        
!=======================================================================
!                                                                       
      subroutine snow2 
!                                                                       
!=======================================================================
!                                                                       
!    snowmelt / refreeze calculation                                    
!---------------------------------------------------------------------- 
!                                                                       
!     calculation of snowmelt and modification of temperatures          
!                                                                       
!     modification deals with snow patches:                             
!          ts < tf, tsnow = ts                                          
!          ts > tf, tsnow = tf                                          
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none  
      !
      real (kind=8) :: avex
      real (kind=8) :: avheat
      real (kind=8) :: avmelt
      real (kind=8) :: cct
      real (kind=8) :: cctt
      real (kind=8) :: cool
      real (kind=8) :: dts
      real (kind=8) :: dtsg
      real (kind=8) :: dtsg3
      real (kind=8) :: exheat
      real (kind=8) :: exmelt
      real (kind=8) :: flux
      real (kind=8) :: fluxef
      real (kind=8) :: freeze
      real (kind=8) :: heat
      integer :: iveg
      real (kind=8) :: realc
      real (kind=8) :: realg
      real (kind=8) :: safe
      real (kind=8) :: snowhc
      real (kind=8) :: tbulk
      real (kind=8) :: tn
      real (kind=8) :: ts
      real (kind=8) :: tsnow
      real (kind=8) :: zmelt
      real (kind=8) :: zmelt2
      real (kind=8) :: dtsg2
      !
      do 1000 iveg = 1, 2 
!                                                                       
      realc = (2 - iveg)*1. 
      realg = (iveg - 1)*1. 
!                                                                       
      cctt = realc*ccx  +  realg*cg 
      cct  = realc*ccx  +  realg*csoil 
      ts   = realc*tc   +  realg*tg 
      dts  = realc*dtc  +  realg*dtg 
      flux = realc*chf  +  realg*dtg/dtt*cg 
!                                                                       
      tsnow = min ( tf-0.01, ts ) 
      snowhc = min( 0.05, snoww(iveg) ) * cw * realg 
      zmelt = 0. 
!                                                                       
      if ( snoww(iveg) .gt. 0. ) go to 100 
      if ( ( ts+dts) .gt. tf ) go to 500 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     no snow  present, simple thermal balance with possible freezing.  
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      freeze = min ( 0., (flux*dtt - ( tf-0.01 - ts )*cctt ) ) 
      snoww(iveg) = min( capac(iveg), - freeze/snomel ) 
      zmelt = capac(iveg) - snoww(iveg) 
      capac(iveg) = 0. 
      dts = dts + snoww(iveg)*snomel/cctt 
      go to 500 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     snow present                                                      
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
  100 continue 
!                                                                       
      if ( ts .lt. tf .and. (ts+dts) .lt. tf ) go to 500 
      if ( ts .gt. tf ) go to 200 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     snow present : ts < tf,  ts+dts > tf                              
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      avex = flux - ( tf-0.01 - ts ) * cctt/dtt 
      avmelt = ( avex/snomel * (areas*realg + realc ) )*dtt 
      zmelt = min( avmelt, snoww(iveg) ) 
      snoww(iveg) = snoww(iveg) - zmelt 
      avheat = avex*( 1.-areas )*realg + ( avmelt-zmelt )*snomel/dtt 
!                                                                       
      safe = max( ( 1.-areas*realg ), 1.e-8 ) 
      dts = tf-0.01 - ts + avheat / ( cctt*safe )*dtt 
      go to 500 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     snow present and ts > tf : ground only.                           
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
  200 continue 
!                                                                       
      tbulk = tsnow*areas + ts*( 1. - areas ) 
      tn = tbulk + dts 
      exheat = cct*( 1.001-max(0.1,areas)) * dts 
      exmelt = flux*dtt - exheat 
      heat = exheat 
      dtsg = exheat / ( cct*(1.001-areas )) 
      if ( (ts+dtsg) .gt. tf ) go to 300 
      heat = ( tf-0.01 - ts ) * ( cct*(1.-areas) ) 
      dtsg = tf-0.01 - ts 
!                                                                       
  300 exmelt = exmelt + exheat - heat 
!                                                                       
      if( exmelt .lt. 0. ) go to 400 
      zmelt = exmelt/snomel 
      if( asnow*(snoww(iveg)-zmelt) .lt. 1. )                           &
     &           zmelt = max( 0., snoww(iveg) - 1./asnow )            
      snoww(iveg) = snoww(iveg) - zmelt 
      exmelt = exmelt - zmelt*snomel 
      zmelt2 = exmelt/ ( cct*( ts-tf )*asnow + snomel ) 
      zmelt2 = min( zmelt2, snoww(iveg) ) 
      zmelt = zmelt + zmelt2 
      snoww(iveg) = snoww(iveg) - zmelt2 
      exmelt = exmelt - zmelt2*( cct*( ts-tf )*asnow + snomel ) 
      dts  = dtsg + exmelt/cct 
      go to 500 
!                                                                       
  400 cool = min( 0., tf-0.01 - (ts+dtsg) ) * cct*(1.-areas) 
      dtsg2 = max ( cool, exmelt ) / ( cct*( 1.001-areas ) ) 
      exmelt = exmelt - dtsg2*cct*(1.-areas) 
      dtsg3 =exmelt/cctt 
      dts = dtsg + dtsg2 + dtsg3 
!                                                                       
  500 continue 
!                                                                       
      www(1) = www(1) + zmelt / ( poros(1) * zdepth(1) ) 
!                                                                       
      dtc = dtc*realg + dts*realc 
      dtg = dtg*realc + dts*realg 
!                                                                       
 1000 continue 
!                                                                       
      fluxef = shf - cg*dtg/dtt 
      dtd = fluxef / ( cg * 2. * sqrt ( pie*365. ) ) * dtt 
!                                                                       
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine radc2 
!                                                                       
!=======================================================================
!                                                                       
!     solar zenith angle computation; downcoming radiation at bottom.   
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      use comsibc 
      implicit none
      !
      real (kind=8) :: cosd
      real (kind=8) :: coshr
      real (kind=8) :: dawn
      real (kind=8) :: dayspy
      real (kind=8) :: dec
      real (kind=8) :: decmax
      real (kind=8) :: dusk
      real (kind=8) :: h
      real (kind=8) :: hac
      real (kind=8) :: rfd
      real (kind=8) :: season
      real (kind=8) :: sind
      real (kind=8) :: sols
      real (kind=8) :: sr
      real (kind=8) :: ss
      !
      dayspy = 365. 
      if ( mod( year, 4. ) .eq. 0. ) dayspy = 366. 
!                                                                       
!-----------------------------------------------------------------------
!    julian day and time update; skip on 1st time step (initialized)    
!-----------------------------------------------------------------------
      if(iter .eq. 1)go to 10 
      time = time + dtt / 3600. 
      if ( time .ge. 23.99 ) time = 0.0 
      day = day +  dtt / 86400. 
!                                                                       
   10 continue 
!                                                                       
      if ( day .gt. dayspy ) year = year + 1. 
      if ( day .gt. dayspy ) day = day - dayspy 
!                                                                       
!-----------------------------------------------------------------------
!    solar declination calculation                                      
!-----------------------------------------------------------------------
!                                                                       
      decmax = pie * ( 23.5 / 180.) 
      sols   = ( 4141./24. ) + mod( year+3., 4. ) * 0.25 
!                                                                       
      season = ( day - sols ) / 365.2 
      dec    = decmax * cos ( 2. * pie * season ) 
!                                                                       
      rfd  = pie / 180. 
      sind = sin( dec ) 
      cosd = cos( dec ) 
      hac  = -tan( zlat * rfd )*tan( dec ) 
      hac  = min(hac,1.0) 
      hac  = max(hac,-1.0) 
!                                                                       
!-----------------------------------------------------------------------
!     h is the half-day length (in radians)                             
!-----------------------------------------------------------------------
!                                                                       
      h   = acos(hac) 
      dawn= -h 
      dusk= +h 
      sr  = 12.-(h/(15.*rfd)) 
      ss  = 12.+(h/(15.*rfd)) 
      coshr = cos( - pie + (time + 0.5*dtt/3600.) / 24. * 2. * pie ) 
      sunang = sin( zlat*rfd ) * sind + cos ( zlat*rfd ) * cosd * coshr 
      sunang = max( 0.01, sunang ) 
!                                                                       
      return 
      END                                           
!====================================================================== 
!                                                                       
      subroutine rasite 
!                                                                       
!====================================================================== 
!                                                                       
!     calculation of ustar, u2, ra and drag using Paulson's method.     
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     (1) site parameters derived from momopt program suite.            
!                                                                       
!     (2) routine is not suitable for gcm applications; designed for    
!         use with forcing variables measured at a field site.          
!                                                                       
!     (3) paulson psi-coefficients are constrained under unsatble       
!         conditions to prevent unrealistically low ra values.          
!                                                                       
!     (4) wind speed (um) must be greater than or equal to 0.1 m/s      
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!     variables that must enter through comsibc                         
!                                                                       
!      tm     : air temperature at zmet                                 
!      um     : wind speed at zwind, um .ge. 0.1                        
!      ht     : sensible heat flux from surface                         
!                                                                       
!     parameters that must enter through comsibc                        
!                                                                       
!      z2     : height of canopy top                                    
!      z0     : roughness length                                        
!      xdx      : zero plane displacement                               
!      vkc    : von karmans constant = 0.41                             
!      rhoair : air density                                             
!      cpair  : air specific heat                                       
!                                                                       
!     other parameters                                                  
!     ----------------                                                  
!                                                                       
!      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet                  
!                                                                       
!      g1     : ratio of km(actual) to km(log-linear) at z = z2         
!      g2     : ratio of ra(actual) to ra(log-linear) for momentum      
!               between: z = z2 and z = zx, where zx = min(zl,zwind)    
!      g3     : ratio of ra(actual) to ra(log-linear) for heat          
!               between: z = z2 and z = zx, where zx = min(zl,zmet)     
!      ztz0   : parameter to determine depth of transition layer above  
!               canopy, zl. zl = z2 + ztz0 * z0                         
!      corb1  : non-neutral correction for calculation of aerodynamic   
!               resistance between ha and z2. when multiplied by        
!               h*rbb/tm gives bulk estimate of local richardson number.
!               rbb = ra for heat between ha and z2.                    
!               corb2 = 9*g/( rhoair*cpair* (du/dz)**2 )                
!      corb2  : neutral value of rbb*u2 ( squared ), equivalent to      
!               rdc**2 for upper canopy                                 
!      ha     : canopy source height for heat                           
!      zwind  : reference height for wind measurement                   
!      zmet   : reference height for temperature, humidity measurement  
!                                                                       
!        the above are generated from sibx + momopt output              
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     variables returned from this routine via comsibc                  
!                                                                       
!      ustar  : friction velocity                                       
!      u2     : wind speed at canopy top                                
!      ra     : aerodynamic resistance for heat flux between ha and zmet
!      drag   : shear stress at canopy top                              
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     references                                                        
!     ----------                                                        
!                                                                       
!         Paulson C.A. (1970) ' Mathematical representation of wind     
!         and temperature profiles in the unstable atmospheric surface  
!         layer', J. Appl. Met., 9, 129-861.                            
!                                                                       
!         SE-89                                                         
!-----------------------------------------------------------------------
      use comsibc
      implicit none
      !
      real (kind=8) :: arg1
      real (kind=8) :: bot
      real (kind=8) :: coef3
      real (kind=8) :: finc
      real (kind=8) :: gfac
      real (kind=8) :: hm1
      real (kind=8) :: hm2
      real (kind=8) :: hrb
      real (kind=8) :: hress
      real (kind=8) :: hss
      integer :: iwalk
      integer :: lx
      integer :: nonpos
      integer :: nox
      real (kind=8) :: ps1
      real (kind=8) :: ps2
      real (kind=8) :: raf
      real (kind=8) :: raf1
      real (kind=8) :: rafmax
      real (kind=8) :: ram
      real (kind=8) :: rbbest
      real (kind=8) :: top
      real (kind=8) :: uest
      real (kind=8) :: us1 = 0d0
      real (kind=8) :: us2
      real (kind=8) :: uss
      real (kind=8) :: y
      real (kind=8) :: zl
      real (kind=8) :: zx1
      real (kind=8) :: zx2
      !
      hress = ht 
      zl    = z2 + ztz0 * z0 
      uest  = vkc*um / log((zwind-xdx)/z0) 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     calculation of u2 assuming neutral conditions                     
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      if ( zwind .gt. zl ) go to 100 
      top = 0. 
      zx1 = zwind - xdx 
      zx2 = z2 - xdx 
      go to 200 
  100 zx1 = zwind - xdx 
      zx2 = zl - xdx 
      top = log( zx1 / zx2 ) 
      zx1 = zl - xdx 
      zx2 = z2 - xdx 
  200 bot = log( zx1 / zx2 ) 
      ram = 1. / ( vkc * uest ) * ( top + g2 * bot ) 
      u2 = um - ram * uest**2 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     calculation of ra for heat follows : non-neutrality assumed       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
       zx1 = zwind - xdx 
       zx2 = 0. 
       arg1 = log ( zx1 / z0 ) 
!                                                                       
!-----------------------------------------------------------------------
!         initialize newton-raphson iterative routine                   
!-----------------------------------------------------------------------
!                                                                       
                    nox = 0 
                 nonpos = 1 
                  iwalk = 0 
                     lx = 1 
                   finc = 0.2 
!                                                                       
       if( ht .le. 0. ) go to 300 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!     unstable case : calculation of ustar followed by ra               
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
 1000  continue 
!                                                                       
       call unstab ( uest, zx1, zx2, arg1, ht, ps1, ps2) 
!                                                                       
       y = um - uest/vkc * ( arg1 - ps1 ) 
!                                                                       
       call newton ( uest, y, finc, nox, nonpos, iwalk, lx ) 
       if( nox .eq. 0 ) go to 1000 
!                                                                       
       if( nox .eq. 2 ) write(6,900) 
  900  format( /,' convergence failure in rasite - unstable case' ) 
!                                                                       
       call rafcal ( zl, uest, ht, raf ) 
!                                                                       
       go to 500 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!      stable case : calculation of ustar                               
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
  300  continue 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!      interpolation zone is defined: this spans negative sensible      
!      heat fluxes from positive side ( hence factor 0.95 ) of the      
!      following conditions:                                            
!              y = 0,    dy/du* = 0.                                    
!      see notes for details                                            
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
       gfac = log((zwind - xdx)/z0) 
       hm1  = -0.95*tm*rhoair*cpair/(2.0*4.7*gx*(zwind-xdx))*           &
     &          (2.0*um/3.0)**3*(vkc/gfac)**2                           
       hm2  = 5.0*hm1 
       us2  = vkc*um/(gfac+4.7) 
       if( ht .lt. hm2 ) go to 310 
       ht = max ( hm1 , ht ) 
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
!      ustar calculated for slightly stable conditions : ht .ge. hm1    
!                                                                       
!---------------------------------------------------------------------- 
!                                                                       
 2000  continue 
!                                                                       
       call stab ( uest, zx1, zx2, ht, ps1, ps2) 
!                                                                       
       y = um - uest/vkc * ( arg1 - ps1 ) 
!                                                                       
       call newton ( uest, y, finc, nox, nonpos, iwalk, lx ) 
       if( nox .eq. 0 ) go to 2000 
!                                                                       
       if( nox .eq. 2 ) write(6,910) 
  910  format( /,' convergence failure in rasite - stable case' ) 
!                                                                       
       ht = hress 
!                                                                       
!-----------------------------------------------------------------------
!      ustar calculation in interpolation zone                          
!-----------------------------------------------------------------------
!                                                                       
       if ( ht .gt. hm1 ) go to 400 
       us1 = uest 
       uest = ( ht-hm2 ) / ( hm1-hm2 ) * ( us1-us2 ) + us2 
       go to 400 
!                                                                       
!-----------------------------------------------------------------------
!      ustar calculation for collapsed profiles                         
!-----------------------------------------------------------------------
!                                                                       
  310  continue 
       uest = us2 
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!      calculation of ra for heat transfer between z2 and zmet          
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
  400  raf = 1.e5 
!                                                                       
       call rafcal ( zl, us2, hm2, rafmax ) 
!                                                                       
       if ( ht .lt. hm2 ) go to 410 
       hss = max ( hm1, ht ) 
       uss = max ( us1, uest ) 
!                                                                       
       call rafcal ( zl, uss, hss, raf ) 
!                                                                       
       if ( ht .gt. hm1 ) go to 410 
       raf1 = raf 
       raf  = ( ht-hm2 ) / ( hm1-hm2 ) * ( raf1 - rafmax ) + rafmax 
!                                                                       
  410  raf = min ( raf , rafmax ) 
!                                                                       
!-----------------------------------------------------------------------
!     above canopy variables calculated.                                
!-----------------------------------------------------------------------
!                                                                       
  500 hrb = ( ht + sqrt(ht**2) ) / 2.0 + 0.1 
!                                                                       
!-----------------------------------------------------------------------
!     corb1 and corb2 are calculated for between ha and z2 only.        
!-----------------------------------------------------------------------
!                                                                       
                          rbbest = sqrt(corb2)/u2 
!                                                                       
!-----------------------------------------------------------------------
!           initialize newton-raphson iterative routine                 
!-----------------------------------------------------------------------
!                                                                       
                    nox = 0 
                 nonpos = 1 
                  iwalk = 0 
                     lx = 1 
                   finc = 0.2 
!                                                                       
 3000  continue 
!                                                                       
       coef3 = corb1 * hrb / tm / ( z2-ha ) 
!                                                                       
       y = coef3 * rbbest**3 + ( u2*rbbest )**2 - corb2 
!                                                                       
       call newton( rbbest , y, finc , nox, nonpos, iwalk, lx) 
       if( nox .ne. 1 ) go to 3000 
!                                                                       
       ra  = raf + rbbest 
!                                                                       
       ustar = uest 
       drag = rhoair * uest*uest 
!                                                                       
       return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
       subroutine unstab ( uest, a, b, argz, heat, psione , psitwo ) 
!                                                                       
!=======================================================================
!                                                                       
!      calculation of Paulson psi-function for unstable condition       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
       use comsibc 
       implicit none
       ! dimension x(2)
       real (kind=8) :: x(2)
       !
       real (kind=8) :: uest
       real (kind=8) :: a
       real (kind=8) :: b
       real (kind=8) :: argz
       real (kind=8) :: heat
       real (kind=8) :: psione
       real (kind=8) :: psitwo
       real (kind=8) :: fac
       integer :: i
       real (kind=8) :: zin
       real (kind=8) :: zml
       !
       zin = a 
!                                                                       
       do 1000 i=1,2 
          zml = -uest**3 * rhoair * cpair * tm 
          zml = zml / ( vkc*gx*heat ) 
          fac = 16.0 * zin/zml 
          x(i) = ( 1. - fac )**0.25 
          zin = b 
 1000  continue 
!                                                                       
       psione = 2.*log((1.+x(1))/(1.+x(2)))+log((1.+x(1)**2)/         &
     &         (1.+x(2)**2))-2.*atan(x(1))+2.*atan(x(2))                
       psione = min ( argz * 0.75, psione ) 
!                                                                       
       psitwo = 2.*log((1.+x(1)**2)/(1.+x(2)**2)) 
       psitwo = min ( argz * 0.75, psitwo ) 
!                                                                       
       return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
       subroutine stab ( uest, a, b, heat, psione , psitwo ) 
!                                                                       
!=======================================================================
!                                                                       
!      calculation of Paulson psi-function for stable condition         
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
       use comsibc
       implicit none
       !
       real (kind=8) :: uest
       real (kind=8) :: a
       real (kind=8) :: b
       real (kind=8) :: heat
       real (kind=8) :: psione
       real (kind=8) :: psitwo
       real (kind=8) :: zml
       !
       psione = 0. 
       psitwo = 0. 
       if ( abs(heat) .le. 1.e-4 ) go to 100 
!                                                                       
       zml = -uest**3. * rhoair * cpair * tm 
       zml = zml / ( vkc*gx*heat ) 
!                                                                       
       psione = -4.7 * ( a-b ) / zml 
       psione = max( -4.7, psione ) 
!                                                                       
       psitwo = psione 
!                                                                       
  100  continue 
!                                                                       
       return 
       END                                           
!                                                                       
!=======================================================================
!                                                                       
       subroutine rafcal ( zl, uest, heat, raf ) 
!                                                                       
!=======================================================================
!                                                                       
!      calculation of ra for heat between z2 and zmet                   
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
       use comsibc 
       implicit none
       !
       real (kind=8) :: zl
       real (kind=8) :: uest
       real (kind=8) :: heat
       real (kind=8) :: raf
       real (kind=8) :: arg
       real (kind=8) :: bot
       real (kind=8) :: ps1
       real (kind=8) :: ps2
       real (kind=8) :: top
       real (kind=8) :: zx1
       real (kind=8) :: zx2
       !
       if ( zmet .gt. zl ) go to 100 
!                                                                       
       top = 0. 
       zx1 = zmet - xdx 
       zx2 = z2 - xdx 
       go to 200 
!                                                                       
   100 zx1 = zmet - xdx 
       zx2 = zl - xdx 
       arg = log( zx1 / zx2 ) 
       if ( heat .gt. 0. )                                               &
           call unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)          
       if ( heat .le. 0. )                                               &
            call   stab ( uest, zx1, zx2,      heat, ps1, ps2)          
       top = arg - ps2 
!                                                                       
       zx1 = zl - xdx 
       zx2 = z2 - xdx 
!                                                                       
   200 arg = log ( zx1 / zx2 ) 
       if ( heat .gt. 0. )                                               &
           call unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)          
       if ( heat .le. 0. )                                               &
           call   stab ( uest, zx1, zx2,      heat, ps1, ps2)          
       bot = arg - ps2 
!                                                                       
       raf = 1. / ( vkc*uest ) * ( top + g3 * bot ) 
!                                                                       
       return 
       END                                           
!                                                                       
!=======================================================================
!                                                                       
       subroutine newton(a1,y,finc,nox,nonpos,iwolk,l) 
!                                                                       
!=======================================================================
!                                                                       
!      the newton raphson iterative routine will be used to generate new
!      values of a1 if dabsolute value of y is greater than ertol;      
!      a1 is estimate, y is resultant error                             
!      nex is exit condition  (0=no exit) or (1 when dabs(y) lt ertol)  
!      ertol is the dabsolute value of y necessary to obtain an exit    
!      finc is initial increment size for second estimate of a1         
!      nonpos=0 if quantity to be minimized can be less than zero;      
!      nonpos=1 if quantity can only be positive                        
!      l identifies which quantity is being calculated.                 
!                                                                       
!      control values: finc,ertol,nox,nonpos,l:must be set by user      
!-----------------------------------------------------------------------
!                                                                       
       ! dimension iter(3), iwalk(3), nex(3) 
       ! dimension zinc(3), a2(3), y1(3) 
!
       !importate salvar "save" as variaveis locais (iter, a2 e y2)
       !das subrotinas, fora do common, bem como
       !determinar os valores iniciais para evitar valores aleatorios
       !alocados na memoria. As opcoes de compilacao:
       !gfortran -fno-automatic -finit-local-zero tem resolvido isto
       !nas demais subrotinas (Evandro M Anselmo)
!
       ! save iter, a2, y1 
       ! data cons/1.0/ 
       ! data iter /0,0,0/, a2 /0.,0.,0./, y1 /0.,0.,0./

       implicit none
       integer, save :: iter(3) = (/0,0,0/)
       real (kind=8), save :: a2(3) = (/0d0,0d0,0d0/)
       real (kind=8), save :: y1(3) = (/0d0,0d0,0d0/)
       real (kind=8) :: cons = 1.0 
       integer :: iwalk(3)
       integer :: nex(3) 
       real (kind=8) :: zinc(3) = (/0d0,0d0,0d0/)
       !
       real (kind=8) :: a1
       real (kind=8) :: y
       real (kind=8) :: finc
       integer :: nox
       integer :: nonpos
       integer :: iwolk
       integer :: l
       real (kind=8) :: a
       real (kind=8) :: ertol
       real (kind=8) :: step
       !
       ! data cons/1.0/ 
       ! data iter /0,0,0/, a2 /0.,0.,0./, y1 /0.,0.,0./
       
       iter = (/0,0,0/)
       !a2 = (/0.,0.,0./)
       !y1 = (/0.,0.,0./)
       ! cons = 1.0
       ! print *, iter
       !                                                                       
       ertol = 0.05 * finc 
       iwalk(l) = iwolk 
       nex(l)=nox 
!                                                                       
       if ( iter(l) .ge. 490 ) go to 160 
       if (ertol .lt. 0.000001) ertol=0.000001 
       if (abs(y) .le. ertol) go to 150 
       if((abs(y-y1(l))).le.0.01*ertol .and. iwalk(l).eq.0 ) go to 8 
!                                                                       
       if(abs(y1(l)).gt.ertol) go to 1 
       a2(l)=a1 
!**    a1=a1-y                                                          
       step = min( abs(y), abs(10.*finc) ) * sign(cons,y) 
       a1=a1-step 
       nex(l)=0 
       y1(l)=y 
       iter(l)=1 
       if (iwalk(l) .eq. 3) go to 101 
       iwalk(l)=0 
       go to 101 
    1  iter(l)=iter(l)+1 
       if(iter(l) .eq. 20) iwalk(l)=1 
       if(iwalk(l) .ne. 0) go to 2 
       if(abs(y) .gt. ertol) go to 3 
       nex(l)=1 
       go to 150 
    3  a=a1-y*(a1-a2(l))/(y-y1(l)) 
       if(abs(a-a1).gt.(10.0*finc))                                     &
     &            a=a1+10.0*finc*sign(cons,(a-a1))                      
       a2(l)=a1 
       a1=a 
       y1(l)=y 
       go to 101 
    2  if(iwalk(l).eq.2)go to 4 
       if(iwalk(l).eq.3) go to 6 
       if(sign(cons,y).eq.sign(cons,y1(l))) go to  3 
       zinc(l)=(a1-a2(l))/4.0 
       a1=a2(l)+zinc(l) 
       iwalk(l)=2 
       nex(l)=0 
       go to 101 
    4  if(sign(cons,y) .eq.sign(cons,y1(l))) go to 5 
       zinc(l)=-zinc(l)/4.0 
       a2(l)=a1 
       a1=a1+zinc(l) 
       nex(l)=0 
       y1(l)=y 
       go to 101 
    5  a2(l)=a1 
       a1=a1+zinc(l) 
       y1(l)=y 
       nex(l)=0 
       go to 101 
    6  if(sign(cons,y).eq.sign(cons,y1(l))) go to 7 
       iwalk(l)=1 
       go to 2 
    7  a2(l) = a1 
       a1 = a1+finc 
       y1(l)=y 
       nex(l) = 0 
       go to 101 
    8  a1 = a1 + finc*2.0 
       nex(l)=0 
       go to 101 
  160  continue 
       write(6,900) y, l 
  900  format ( 3x,' failure to converge after 490 iterations',         &
     & /, 3x,' y = ',g12.5, ' lx =',i2 )                                
!                                                                       
  150  nex(l) = 1 
       if( iter(l) .ge. 490 ) nex(l) = 2 
       zinc(l)=0.0 
       iter(l) = 0 
       iwalk(l)=0 
       y1(l)=0.0 
       y=0.0 
       a2(l)=0.0 
  101  continue 
       if(nonpos.eq.1.and.a1.lt.0.0) a1=a2(l)/2.0 
       nox = nex(l) 
       iwolk = iwalk(l) 
!                                                                       
       return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine gauss ( a, n, np1, x, work)
!                                                                       
!=======================================================================
!                                                                       
!     solve a linear system by gaussian elimination.  developed by      
!     dr. chin-hoh moeng.  a is the matrix of coefficients, with the    
!     vector of constants appended as an extra column.  x is the vector 
!     containing the results.  the input matrix is not destroyed.       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      use comsibc 
      implicit none
      !dimension a(4,5),work(4,5),x(4)
      real (kind=8) :: a(4,5)
      real (kind=8) :: work(4,5)
      real (kind=8) :: x(4)
      integer :: n
      integer :: np1
      integer :: i
      integer :: j
      integer :: k
      integer :: l
      real (kind=8) :: r
!                                                                       
      do 1000 i=1,n 
         do 1000 j=1,np1 
 1000       work(i,j)=a(i,j) 
!                                                                       
      do 20 i=2,n 
         do 20 j=i,n 
            r=work(j,i-1)/work(i-1,i-1) 
!                                                                       
      do 20 k=1,np1 
   20    work(j,k)=work(j,k)-r*work(i-1,k) 
!                                                                       
      do 30 i=2,n 
         k=n-i+2 
         r=work(k,np1)/work(k,k) 
!                                                                       
      do 30 j=i,n 
         l=n-j+1 
   30    work(l,np1)=work(l,np1)-r*work(l,k) 
!                                                                       
      do 40 i=1,n 
   40    x(i)=work(i,np1)/work(i,i) 
!                                                                       
      return 
      END                                           
