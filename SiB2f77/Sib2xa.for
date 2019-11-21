c                                                                               
      subroutine const2                                                         
c
c=======================================================================        
c                                                                               
c     initialization of physical constants                                      
c                                                                               
c     subroutine const2 is called at every time step                            
c     because in some applications constants may depend on                      
c     environmental conditions.                                                 
c                                                                               
c-----------------------------------------------------------------------        
c
       include 'comsibc.h'                                                       
c                                                                               
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
c
c-----------------------------------------------------------------------        
c     n.b. :  snomel is expressed in j m-1                                      
c-----------------------------------------------------------------------        
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
c=======================================================================        
c                                                                               
      subroutine adjust ( ts, spechc, capacp, snowwp, iveg )                    
c                                                                               
c=======================================================================        
c                                                                               
c     temperature change due to addition of precipitation                       
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c     roff           runoff (m)                                              
c     tc             canopy temperature (K)                                   
c     tg             ground surface temperature (K)                           
c     www(1)         ground wetness of surface layer                          
c     capac(2)       canopy/ground liquid interception store (m)              
c     snoww(2)       canopy/ground snow interception store (m)                
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
      freeze = 0.                                                               
      diff = ( capac(iveg)+snoww(iveg) - capacp-snowwp )*cw                     
      ccp = spechc                                                              
      cct = spechc + diff                                                       
c                                                                               
      tsd = ( ts * ccp + tm * diff ) / cct                                      
c                                                                               
      if ( ts .gt. tf .and. tm .gt. tf ) go to 200                              
      if ( ts .le. tf .and. tm .le. tf ) go to 200                              
c                                                                               
      tta = ts                                                                  
      ttb = tm                                                                  
      cca = ccp                                                                 
      ccb = diff                                                                
      if ( tsd .gt. tf ) go to 100                                              
c                                                                               
c----------------------------------------------------------------------         
c    freezing of water on canopy or ground                                      
c----------------------------------------------------------------------         
c                                                                               
      ccc = capacp * snomel                                                     
      if ( ts .lt. tm ) ccc = diff * snomel / cw                                
      tsd = ( tta * cca + ttb * ccb + ccc ) / cct                               
c                                                                               
      freeze = ( tf * cct - ( tta * cca + ttb * ccb ) )                         
      freeze = (amin1 ( ccc, freeze )) / snomel                                 
      if(tsd .gt. tf)tsd = tf - 0.01                                            
c                                                                               
      go to 200                                                                 
c                                                                               
100   continue                                                                  
c                                                                               
c----------------------------------------------------------------------         
c    melting of snow on canopy or ground, water infiltrates.                    
c----------------------------------------------------------------------         
c                                                                               
      ccc = - snoww(iveg) * snomel                                              
      if ( ts .gt. tm ) ccc = - diff * snomel / cw                              
c                                                                               
      tsd = ( tta * cca + ttb * ccb + ccc ) / cct                               
c                                                                               
      freeze = ( tf * cct - ( tta * cca + ttb * ccb ) )                         
      freeze = (amax1( ccc, freeze )) / snomel                                  
      if(tsd .le. tf)tsd = tf - 0.01                                            
c                                                                               
200   snoww(iveg) = snoww(iveg) + freeze                                        
      capac(iveg) = capac(iveg) - freeze                                        
c                                                                               
      xs = amax1( 0., ( capac(iveg) - satcap(iveg) ) )                          
      if( snoww(iveg) .ge. 0.0000001 ) xs = capac(iveg)                         
      www(1) = www(1) + xs / ( poros(1) * zdepth(1) )                              
      capac(iveg) = capac(iveg) - xs                                            
      ts = tsd                                                                  
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine patchs ( p0 )                                                  
c                                                                               
c=======================================================================        
c                                                                               
c     marginal situation: snow exists in patches at temperature tf              
c     with remaining area at temperature tg > tf.                               
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     calculation of effect of intercepted snow and rainfall on ground.         
c     patchy snowcover situation involves complex treatment to keep             
c     energy conserved.                                                         
c                                                                               
c----------------------------------------------------------------------         
c
       include 'comsibc.h'                                                       
c                                                                               
      pinf = p0                                                                 
      thru = 0.                                                                 
      snowhc = amin1( 0.05, snoww(2) ) * cw                                     
      areas = amin1( 1.,(asnow*snoww(2)) )                                      
      if( tm .gt. tf ) go to 400                                                
c                                                                               
c----------------------------------------------------------------------         
c     snow falling onto area                                                    
c----------------------------------------------------------------------         
c                                                                               
      rhs = tm*pinf*cw + tf*(snowhc + csoil*areas)                              
     &    + tg*csoil*(1.-areas)                                                 
      dareas = amin1( asnow*pinf, ( 1.-areas ) )                                
      ex = rhs - tf*pinf*cw - tf*(snowhc + csoil*(areas + dareas))              
     &   - tg*csoil*(1.-areas-dareas)                                           
      if( (areas+dareas) .ge. 0.999 ) tg = tf - 0.01                            
      if( ex .lt. 0. ) go to 200                                                
c                                                                               
c----------------------------------------------------------------------         
c     excess energy is positive, some snow melts and infiltrates.               
c----------------------------------------------------------------------         
c                                                                               
      zmelt = ex/snomel                                                         
      if( asnow*(snoww(2) + pinf - zmelt) .gt. 1. ) go to 100                   
      zmelt = 0.                                                                
      if( asnow*(snoww(2) + pinf) .ge. 1. )                                     
     &    zmelt = ( asnow*(snoww(2) + pinf) - 1. ) / asnow                      
      zmelt = ( ex - zmelt*snomel )/( snomel + asnow*csoil*(tg-tf) )            
     &      + zmelt                                                             
100   snoww(2) =  snoww(2) + pinf - zmelt                                       
      www(1) = www(1) + zmelt/(poros(1)*zdepth(1))                                 
      go to 600                                                                 
c                                                                               
c----------------------------------------------------------------------         
c     excess energy is negative, bare ground cools to tf, then whole            
c     area cools together to lower temperature.                                 
c----------------------------------------------------------------------         
c                                                                               
200   tsd = 0.                                                                  
      if( (areas+dareas) .le. 0.999 )                                           
     &        tsd = ex/(csoil*( 1.-areas-dareas)) + tg                          
      if( tsd .gt. tf ) go to 300                                               
      tsd = tf + ( ex - (tf-tg)*csoil*(1.-areas-dareas) )                       
     &            /(snowhc+pinf*cw+csoil)                                       
300   tg = tsd                                                                  
      snoww(2) = snoww(2) + pinf                                                
      go to 600                                                                 
c                                                                               
c----------------------------------------------------------------------         
c     rain falling onto area                                                    
c----------------------------------------------------------------------         
c                                                                               
400   continue                                                                  
c                                                                               
c----------------------------------------------------------------------         
c     rain falls onto snow-free sector first.                                   
c----------------------------------------------------------------------         
c                                                                               
      tsd = tf - 0.01                                                           
      if ( areas .lt. 0.999 ) tsd = ( tm*pinf*cw + tg*csoil )                   
     &                            /  ( pinf*cw + csoil )                        
      tg = tsd                                                                  
      www(1)= www(1)+pinf*(1.-areas)/(poros(1)*zdepth(1))                          
c                                                                               
c----------------------------------------------------------------------         
c     rain falls onto snow-covered sector next.                                 
c----------------------------------------------------------------------         
c                                                                               
      ex = ( tm - tf )*pinf*cw*areas                                            
      dcap = -ex / ( snomel + ( tg-tf )*csoil*asnow )                           
      if( (snoww(2) + dcap) .lt. 0. ) go to 500                                 
      www(1) = www(1)+(pinf*areas-dcap)/(poros(1)*zdepth(1))                       
      snoww(2) = snoww(2) + dcap                                                
      go to 600                                                                 
500   tg = ( ex - snomel*snoww(2) - ( tg-tf )*csoil*areas ) / csoil             
     &      + tg                                                                
      www(1)=www(1)+(snoww(2)+pinf*areas)/(poros(1)*zdepth(1))                     
      capac(2) = 0.                                                             
      snoww(2) = 0.                                                             
c                                                                               
600   continue                                                                  
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine snow1                                                          
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of effects of snow cover on surface morphology and            
c     maximum water storage values.                                             
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       z0             roughness length (m)                                     
c       xdx              zero plane displacement (m)                              
c       rbc            rb coefficient (c1) (s m-1)**1/2                         
c       rdc            rd coefficient (c2)                                      
c       satcap(2)      interception capacities (m)                              
c       canex          fraction of exposed canopy (snow-free)                   
c       areas          ground snow cover fraction                               
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
c    alteration of aerodynamic transfer properties in case of snow              
c    accumulation. calculation of maximum water storage values.                 
c                                                                               
c      canex       (fraction of canopy not covered by snow)                     
c      xdx           (snow-modified value of dd, used in all calculations)        
c      z0          (snow-modified value of z0d, used in all calculations)       
c      rbc         (snow-modified value of cc1, used in all calculations)       
c      rdc         (snow-modified value of cc2, used in all calculations)       
c      areas       (fraction of ground covered by snow)                         
c      satcap(1)   (s-c)   : equation (56) , SE-86, page 741 se-89              
c      satcap(2)   (s-g)   : 0.002, surface interception store                  
c----------------------------------------------------------------------         
c                                                                               
       include 'comsibc.h'                                                       
c                                                                               
      canex  = 1.-( snoww(2)*5.-z1)/(z2-z1)                                     
      canex  = amax1( 0.1, canex )                                              
      canex  = amin1( 1.0, canex )                                              
      xdx    = z2 - ( z2-dd ) * canex                                           
      z0     = z0d/( z2-dd ) * ( z2-xdx )                                         
      rbc    = cc1/canex                                                        
      rdc    = cc2*canex                                                        
      areas    = amin1(1., asnow*snoww(2))                                      
      satcap(1) = zlt*0.0001 * canex                                            
      satcap(2) = 0.002                                                         
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine rada2                                                          
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of albedos via two stream approximation( direct               
c     and diffuse ) and partition of radiant energy                             
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     subroutines  called  : snow1                                         
c     --------------------   longrn                                        
c                                                                               
c                                                                               
c++++++++++++++++++++++++++++++output++++++++++++++++++++++++++++++++           
c                                                                               
c       salb(2,2)      surface albedos                                          
c       tgeff          effective surface radiative temperature (k)              
c       radfac(2,2,2)  radiation absorption factors                             
c       thermk         canopy gap fraction for tir radiation                    
c                                                                               
c++++++++++++++++++++++++++diagnostics+++++++++++++++++++++++++++++++           
c                                                                               
c       albedo(2,2,2)  component reflectances                                   
c       closs          tir emission from the canopy (w m-2)                     
c       gloss          tir emission from the ground (w m-2)                     
c                                                                               
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           
c                                                                               
       include 'comsibc.h'                                                       
c                                                                               
      dimension tranc1(2), tranc2(2), tranc3(2)                                 
c                                                                               
      xfx = sunang                                                                
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c                                                                               
c     modification for effect of snow on upper story albedo                     
c         snow reflectance   = 0.80, 0.40 . multiply by 0.6 if melting          
c         snow transmittance = 0.20, 0.54                                       
c                                                                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      call snow1                                                                
c                                                                               
      facs  = ( tg-tf ) * 0.04                                                  
      facs  = amax1( 0. , facs)                                                 
      facs  = amin1( 0.4, facs)                                                 
      fmelt = 1. - facs                                                         
c                                                                               
      do 1000 iwave = 1, 2                                                      
c                                                                               
      scov =  amin1( 0.5, snoww(1)/satcap(1) )                                  
      reff1 = ( 1. - scov ) * ref(iwave,1) + scov * ( 1.2 -                     
     &        iwave * 0.4 ) * fmelt                                             
      reff2 = ( 1. - scov ) * ref(iwave,2) + scov * ( 1.2 -                     
     &        iwave * 0.4 ) * fmelt                                             
      tran1 = tran(iwave,1) * ( 1. - scov )                                     
     &        + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt )                    
     &        * tran(iwave,1)                                                   
      tran2 = tran(iwave,2) * ( 1. - scov )                                     
     &        + scov * ( 1.- ( 1.2 - iwave * 0.4 ) * fmelt ) * 0.9              
     &        * tran(iwave,2)                                                   
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     calculate average scattering coefficient, leaf projection and             
c     other coefficients for two-stream model.                                  
c                                                                               
c      scat  (omega)        : equation (1,2) , SE-85                            
c      proj  (g(mu))        : equation (13)  , SE-85                            
c      extkb (k, g(mu)/mu)  : equation (1,2) , SE-85                            
c      zmew  (int(mu/g(mu)) : equation (1,2) , SE-85                            
c      acss  (a-s(mu))      : equation (5)   , SE-85                            
c      extk  (k, various)   : equation (13)  , SE-85                            
c      upscat(omega-beta)   : equation (3)   , SE-85                            
c      betao (beta-0)       : equation (4)   , SE-85                            
c      psi   (h)            : appendix       , SE-85                            
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      scat = green*( tran1 + reff1 ) +( 1. - green ) *                          
     &     ( tran2 + reff2)                                                   
      chiv = chil                                                               
c                                                                               
      if ( abs(chiv) .le. 0.01 ) chiv = 0.01                                    
      aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv                              
      bb = 0.877 * ( 1. - 2. * aa )                                             
c                                                                               
      proj = aa + bb * xfx                                                        
      extkb = ( aa + bb * xfx ) / xfx                                               
      zmew = 1. / bb * ( 1. - aa / bb * alog ( ( aa + bb ) / aa ) )             
      acss = scat / 2. * proj / ( proj + xfx * bb )                               
      acss = acss * ( 1. - xfx * aa / ( proj + xfx * bb ) *           
     &     alog ( ( proj + xfx * bb + xfx * aa ) / ( xfx * aa ) ) )                             
c                                                                               
      upscat = green * tran1 + ( 1. - green ) * tran2                           
      upscat = 0.5 * ( scat + ( scat - 2. * upscat ) *                          
     &     (( 1. - chiv ) / 2. ) ** 2 )                                     
      betao = ( 1. + zmew * extkb ) / ( scat * zmew * extkb ) * acss            
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     intermediate variables identified in appendix of SE-85.                   
c                                                                               
c      be          (b)     : appendix      , SE-85                              
c      ce          (c)     : appendix      , SE-85                              
c      bot         (sigma) : appendix      , SE-85                              
c      hh1         (h1)    : appendix      , SE-85                              
c      hh2         (h2)    : appendix      , SE-85                              
c      hh3         (h3)    : appendix      , SE-85                              
c      hh4         (h4)    : appendix      , SE-85                              
c      hh5         (h5)    : appendix      , SE-85                              
c      hh6         (h6)    : appendix      , SE-85                              
c      hh7         (h7)    : appendix      , SE-85                              
c      hh8         (h8)    : appendix      , SE-85                              
c      hh9         (h9)    : appendix      , SE-85                              
c      hh10        (h10)   : appendix      , SE-85                              
c      psi         (h)     : appendix      , SE-85                              
c      zat         (l-t)   : appendix      , SE-85                              
c      epsi        (s1)    : appendix      , SE-85                              
c      ek          (s2)    : appendix      , SE-85                              
c--------------------------------------------------------------------           
c                                                                               
      be = 1. - scat + upscat                                                   
      ce = upscat                                                               
      bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 )                           
      if ( abs(bot) .gt. 1.e-10) go to 100                                      
      scat = scat* 0.98                                                         
      be = 1. - scat + upscat                                                   
      bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 )                           
c
100   continue                                                                  
c
      de = scat * zmew * extkb * betao                                          
      fe = scat * zmew * extkb * ( 1. - betao )                                 
      hh1 = - de * be + zmew * de * extkb - ce * fe                             
      hh4 = - be * fe - zmew * fe * extkb - ce * de                             
c                                                                               
      psi = sqrt(be**2 - ce**2)/zmew                                            
c                                                                               
      zat = zlt/vcover*canex                                                    
c                                                                               
      power1 = amin1( psi*zat, 50. )                                            
      power2 = amin1( extkb*zat, 50. )                                          
      epsi = exp( - power1 )                                                    
      ek = exp ( - power2 )                                                     
c                                                                               
      albedo(2,iwave,1) = soref(iwave)*(1.-areas)                               
     &                  + ( 1.2-iwave*0.4 )*fmelt * areas                       
      albedo(2,iwave,2) = soref(iwave)*(1.-areas)                               
     &                  + ( 1.2-iwave*0.4 )*fmelt * areas                       
      ge = albedo(2,iwave,1)/albedo(2,iwave,2)                                  
c                                                                               
c-----------------------------------------------------------------------        
c     calculation of diffuse albedos                                            
c                                                                               
c      albedo(1,ir,2) ( i-up ) : appendix , SE-85                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      f1 = be - ce / albedo(2,iwave,2)                                          
      zp = zmew * psi                                                           
c                                                                               
      den = ( be + zp ) * ( f1 - zp ) / epsi -                                  
     &      ( be - zp ) * ( f1 + zp ) * epsi                                    
      hh7   = ce * ( f1 - zp ) / epsi / den                                     
      hh8  = -ce * ( f1 + zp ) * epsi / den                                     
      f1 = be - ce * albedo(2,iwave,2)                                          
      den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi                             
c                                                                               
      hh9   = ( f1 + zp ) / epsi / den                                          
      hh10  = - ( f1 - zp ) * epsi / den                                        
      tranc2(iwave) = hh9 * epsi + hh10 / epsi                                  
c                                                                               
      albedo(1,iwave,2) =  hh7 + hh8                                            
c                                                                               
c-----------------------------------------------------------------------        
c     calculation of direct albedos and canopy transmittances.                  
c                                                                               
c      albedo(1,iw,1) ( i-up ) : equation(11)   , SE-85                         
c      tranc (iw)   ( i-down ) : equation(10)   , SE-85                         
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      f1 = be - ce / albedo(2,iwave,2)                                          
      zmk = zmew * extkb                                                        
c                                                                               
      den = ( be + zp ) * ( f1 - zp ) / epsi -                                  
     &      ( be - zp ) * ( f1 + zp ) * epsi                                    
      hh2 = ( de - hh1/bot * ( be + zmk ) ) * ( f1 - zp ) / epsi -              
     &        ( be - zp ) * ( de - ce*ge - hh1/bot * ( f1 + zmk ) ) * ek        
      hh2 = hh2 / den                                                           
      hh3 = ( be + zp ) * (de - ce*ge - hh1/bot * ( f1 + zmk ))* ek -           
     &       ( de - hh1/bot * ( be + zmk ) ) * ( f1 + zp ) * epsi               
      hh3 = hh3 / den                                                           
      f1 = be - ce * albedo(2,iwave,2)                                          
      den = ( f1 + zp ) / epsi - ( f1 - zp ) * epsi                             
      hh5 = - hh4/bot * ( f1 + zp ) / epsi -                                    
     &        ( fe + ce*ge*albedo(2,iwave,2) + hh4/bot*( zmk-f1 ) ) * ek        
      hh5 = hh5 / den                                                           
      hh6 = hh4/bot * ( f1 - zp ) * epsi +                                      
     &        ( fe + ce*ge*albedo(2,iwave,2) + hh4/bot*( zmk-f1 ) ) * ek        
      hh6 = hh6 / den                                                           
      tranc1(iwave) = ek                                                        
      tranc3(iwave) = hh4/bot * ek + hh5 * epsi + hh6 / epsi                    
c                                                                               
      albedo(1,iwave,1) = hh1/bot + hh2 + hh3                                   
c                                                                               
c----------------------------------------------------------------------         
c     calculation of terms which multiply incoming short wave fluxes            
c     to give absorption of radiation by canopy and ground                      
c                                                                               
c      radfac      (f(il,imu,iv)) : equation (19,20) , SE-86                    
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      radfac(2,iwave,1) = ( 1.-vcover ) * ( 1.-albedo(2,iwave,1) )              
     &       + vcover * ( tranc1(iwave) * ( 1.-albedo(2,iwave,1) )              
     &       + tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )                       
c                                                                               
      radfac(2,iwave,2) = ( 1.-vcover ) * ( 1.-albedo(2,iwave,2) )              
     &       + vcover *  tranc2(iwave) * ( 1.-albedo(2,iwave,2) )               
c                                                                               
      radfac(1,iwave,1) = vcover * ( ( 1.-albedo(1,iwave,1) )                   
     &       - tranc1(iwave) * ( 1.-albedo(2,iwave,1) )                         
     &       - tranc3(iwave) * ( 1.-albedo(2,iwave,2) ) )                       
c                                                                               
      radfac(1,iwave,2) = vcover * ( ( 1.-albedo(1,iwave,2) )                   
     &       - tranc2(iwave) * ( 1.-albedo(2,iwave,2) ) )                       
c                                                                               
c----------------------------------------------------------------------         
c     calculation of total surface albedos ( salb ) with weighting              
c     for cover fractions.                                                      
c----------------------------------------------------------------------         
c                                                                               
      do 3000 irad = 1, 2                                                       
c
      salb(iwave,irad) = ( 1.-vcover ) * albedo(2,iwave,irad) +                 
     &                   vcover * albedo(1,iwave,irad)                          
c
3000  continue                                                                  
c                                                                               
1000  continue                                                                  
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     calculation of long-wave flux terms from canopy and ground                
c                                                                               
c      closs ( fc - rnc )     : equation (21),  SE-86                           
c      gloss ( fg - rng )     : equation (22),  SE-86                           
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      tgs = amin1(tf,tg)*areas + tg*(1.-areas)                                  
      tc4 = tc  * tc  * tc  * tc                                                
      tg4 = tgs * tgs * tgs * tgs                                               
c                                                                               
      zkat = 1./zmew * zlt / vcover                                             
      zkat = amin1( 50. , zkat )                                                
      zkat = amax1( 1.e-5, zkat )                                               
      thermk = exp(-zkat)                                                       
c                                                                               
      fac1 =  vcover * ( 1.-thermk )                                            
      fac2 =  1.                                                                
      closs =  2. * fac1 * stefan * tc4                                         
      closs =  closs - fac2 * fac1 * stefan * tg4                               
      gloss =  fac2 * stefan * tg4                                              
      gloss =  gloss - fac1 * fac2 * stefan * tc4                               
      zlwup =  fac1 * stefan * tc4 + (1. - fac1 ) * fac2 * stefan * tg4         
c                                                                               
      call longrn( tranc1, tranc2, tranc3 )                                   
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     calculation of absorption of radiation by surface                         
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      radt(1) = 0.                                                              
      radt(2) = 0.                                                              
c                                                                               
      do 2000 iveg  = 1, 2                                                      
      do 2000 iwave = 1, 2                                                      
      do 2000 irad  = 1, 2                                                      
c                                                                               
      radt(iveg) = radt(iveg)+radfac(iveg,iwave,irad)*radn(iwave,irad)          
c                                                                               
2000  continue                                                                  
c                                                                               
      radt(1) = radt(1) + radn(3,2)*vcover*(1.- thermk) - closs                 
      radt(2) = radt(2) + radn(3,2)*( 1.-vcover*(1.-thermk) ) - gloss           
c         
      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine longrn( tranc1, tranc2, tranc3 )                               
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of downward longwave. this is not required in gcm if          
c     downward longwave is provided by gcm-radiation code as radn(3,2).         
c                                                                               
c-----------------------------------------------------------------------        
       include 'comsibc.h'                                                       
c                                                                               
      dimension tranc1(2), tranc2(2), tranc3(2)                                 
c                                                                               
c                                                                               
      if(ilw .eq. 1)go to 101                                                   
      if(ilw .eq. 2)go to 102  ! Brunts                                                  
      if(ilw .eq. 3)go to 103  ! residuo Bal radiacao                                                 
           
      if(ilw .eq. 4)go to 104  ! Brutsaert(1975)                                                 
      if(ilw .eq. 5)go to 105  ! Idso and Jackson                                                         
      if(ilw .eq. 6)go to 106  ! Swinbank                                                  
      if(ilw .eq. 7)go to 107  ! Duarte                                                   
      if(ilw .eq. 8)go to 108  ! Kruk                                                   
c
c----------------------------------------------------------------------         
c     downward long-wave assumed to be provided as radn(3,2)                    
c----------------------------------------------------------------------         
101   continue     
      go to 200                                                                 
c                                                                               
c----------------------------------------------------------------------         
c     downward long-wave from brunt's equation, Monteith(1973), p37.            
c----------------------------------------------------------------------         
c
 102  esky = 0.53 + 0.06*sqrt(em) !brunts(1932) com correcao de Jacobs(1978)(SiB&_original)                                              
      radn(3,2)  =  esky*(1.+0.2*(cloud*cloud))*stefan*tm**4
      go to 200       
      
 104  esky = 1.24*((em/tm)**0.1428)!Brutsaert(1975)com correcao de Jacobs(1978)				
      radn(3,2)  =  esky*stefan*tm**4*(1.+0.2*(cloud*cloud))
      go to 200       
      
 105  esky =(0.26*exp(-0.00077*((273-tm)**2)))!Idso&Jackson(1969)c/correcaoJacobs (1978)
      radn(3,2)  = stefan*tm**4*(1-esky)*(1.+0.2*(cloud*cloud))
      go to 200       
          
 106  esky = 0.000009	 !Swinbank (1963) com correcao de Jacobs (1978)     
      radn(3,2)  =  esky*stefan*tm**6*(1.+0.2*(cloud*cloud))
      go to 200       
      
 107  esky = 0.625*(((em*100)/tm)**0.131) !Duarte (2006)com correcao de Duarte (2006)	       
      radn(3,2)  =  esky*(1.+0.242*((cloud)**0.583))*stefan*tm**4
      go to 200       
      
 108  esky = 0.576*(((em*100)/tm)**0.202) !Kruk(2008) com correcao de Kruk(2008) 
      radn(3,2)  =  esky*(1.+0.1007*((cloud)**0.9061))*stefan*tm**4  		 
      go to 200                                                                 
c                                                                               
103   continue                                                                                          
c
c----------------------------------------------------------------------         
c     downward long-wave flux calculated as residual from measured              
c     net radiation and outgoing longwave radiation.                            
c                                                                               
c     calculation of absorbed fractions of radiation ( expendable )             
c----------------------------------------------------------------------         
c
      do 2000 iwave = 1, 2                                                      
c                                                                               
      rab(2,iwave,1) =  ( 1. - vcover ) *                                       
     &  ( radn(iwave,1) * ( 1. - albedo(2,iwave,1) ) )                          
      rab(2,iwave,2) =  ( 1. - vcover ) *                                       
     &    radn(iwave,2) * ( 1. - albedo(2,iwave,2) )                            
c                                                                               
      rab(2,iwave,1) = rab(2,iwave,1) + vcover *                                
     &  ( radn(iwave,1) * ( tranc1(iwave) * ( 1. - albedo(2,iwave,1) ) +        
     &    tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) ) )                        
      rab(2,iwave,2) = rab(2,iwave,2) + vcover *                                
     &    radn(iwave,2) * tranc2(iwave) * ( 1. - albedo(2,iwave,2) )            
c                                                                               
      rab(1,iwave,1) =  vcover *                                                
     &    radn(iwave,1) * ( ( 1. - albedo(1,iwave,1) ) -                        
     &    tranc1(iwave) * ( 1. - albedo(2,iwave,1) ) -                          
     &    tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) )                          
      rab(1,iwave,2) =  vcover *                                                
     &    radn(iwave,2) * ( ( 1. - albedo(1,iwave,2) ) -                        
     &    tranc2(iwave) * ( 1. - albedo(2,iwave,2) ) )                          
2000  continue                                                                  
c                                                                               
      swab = rab(1,1,1) + rab(1,1,2) + rab(1,2,1) + rab(1,2,2) +                
     &       rab(2,1,1) + rab(2,1,2) + rab(2,2,1) + rab(2,2,2)                  
      swup = swdown - swab                                                      
      radn(3,2) = rnetm - swab + zlwup                                          
c                                                                               
200   continue                                                                  
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
c======================================================================         
c                                                                               
      subroutine endtem (ipbl)                                                  
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c      calculation of ea, ta, ra, rb, rd and soil moisture stress               
c      for the beginning of the time step                                       
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c                        modifications                                          
c                                                                               
c     (1)     : change in cog1,cog2,cogs1 to allow soil evaporation             
c               from beneath ground cover canopy.                               
c                                                                               
c     (2)     : change in temperature tgs to account for patchy snow.           
c               a bulk (area-weighted) temperature is substituted for           
c               all energy budget calculations. energy used to warm             
c               exposed patches is subtracted from snow evaporation.            
c                                                                               
c     (3)     : inclusion of randall-sellers backward implicit scheme           
c               for calculating dtc, dtg, dth, dqm. option remains to           
c               use original dtc, dtg scheme only using parameter ipbl.         
c                                                                               
c     (4)     : inclusion of integrated canopy  photosynthesis -                
c               conductance model. note that soil moisture stress is            
c               modelled as a chronic condition, while the relative             
c               humidity term is solved within a feedback loop.                 
c               reference : SE-92                                
c                                                                               
c=======================================================================        
c                                                                               
c     subroutines  called : rasite --> unstab,stab,rafcal                 
c     -------------------   rbrd                                          
c                           phosib --> cycalc-sortin                      
c                           delrn                                         
c                           delhf                                         
c                           delef                                         
c                           sibslv --> gauss                                        
c                           dtcdtg                                        
c                           newton                                                           
c-----------------------------------------------------------------------        
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c       ect            canopy transpiration (j m-2)                             
c       eci            canopy interception loss (j m-2)                         
c       egs            ground evaporation (j m-2)                               
c       egi            ground interception loss (j m-2)                         
c       ec             ect + eci                                                
c       eg             egs + egi                                                
c       hc             canopy sensible heat flux (j m-2)                        
c       hg             ground sensible heat flux (j m-2)                        
c       chf            canopy heat storage flux (j m-2)                         
c       shf            soil heat storage flux (j m-2)                           
c       fc             canopy dew indicator                                     
c       fg             ground dew indicator                                     
c       heaten         heat loss to snow melt process (j m-2)                   
c                                                                               
c++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++        
c                                                                               
c       tsnow          snow temperature (k)                                     
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
       include 'comsibc.h'                                                        
       include 'pardif.h'                                                         
c    
chrr  introduzido p/ evitar warning compilacao (variable used and not defined)
      rsnow = 0.
      tsnow = tg
chrr
                                                                           
      ifirst = 1                                                                
      icount = 0                                                                
c                                                                               
      fc = 1.                                                                   
      fg = 1.                                                                   
      ta = (tgs+tc)/2.                                                          
      ea = em                                                                   
      ht = 0.                                                                   
c                                                                               
1000  continue                                                                  
      icount = icount + 1                                                       
c                                                                               
      call rasite                                                               
c                                                                               
      call rbrd                                                                 
c                                                                               
      call phosib                                                               
c                                                                               
      if ( icount .le. 4 ) go to 1000                                           
c                                                                               
      ifirst = 0                                                                
      call delrn                                                                
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     dew calculation : dew condition is set at beginning of time step.         
c     if surface changes state during time step, latent heat flux is            
c     set to zero.                                                              
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      if ( ea .gt. etc ) fc = 0.                                                
      if ( ea .gt. etgs) fg = 0.                                                
c                                                                               
c----------------------------------------------------------------------         
c     start of non-neutral resistance calculation loop                          
c----------------------------------------------------------------------         
c                                                                               
      i = 0                                                                     
c                                                                               
c----------------------------------------------------------------------         
c         initialize newton-raphson iterative routine                    
c                    for ra calculation                                         
c----------------------------------------------------------------------         
c

                    nox = 0                                                     
                 nonpos = 0                                                     
                  iwalk = 0                                                     
                     lx = 2                                                     
                   finc = 50.                                                   
2000  continue                                                                  
c                                                                               
      call rasite                                                               
c                                                                               
      call delhf                                                                
c                                                                               
      call delef                                                                
c                                                                               
      if (ipbl .eq. 0 ) call dtcdtg                                             
c                                                                               
      if (ipbl .eq. 1 ) call sibslv                                             
c                                                                               
c-----------------------------------------------------------------------        
c     calculation of evaporative potentials (ecpot, egpot) and                  
c        interception losses; fluxes in j m**-2.  ecidif and egidif             
c        hold the excess energy if all intercepted water is evaporated          
c        during the timestep.  these energies are later added to the            
c        sensible heat fluxes.                                                  
c                                                                               
c      eci         (e-wc)  : equation (59) , SE-86                              
c      egi         (e-wg)  : equation (60) , SE-86                              
c-----------------------------------------------------------------------        
c                                                                               
c     check if interception loss term has exceeded canopy storage               
c----------------------------------------------------------------------         
c                                                                               
      ecpot = ( (etc-ea) + (  getc-deadtc)*dtc-deadtg*dtg-deadqm*dqm )          
      eci = ecpot * wc /(2.*rb) * rcp/psy * dtt                                 
      ecidif=amax1(0.0,(eci-(snoww(1)+capac(1))*1.e3*hlat))                     
      eci   =amin1(eci,(    (snoww(1)+capac(1))*1.e3*hlat))                     
c                                                                               
      egpot = ( (etgs-ea) + (getgs-deadtg)*dtg-deadtc*dtc-deadqm*dqm )          
      egi = egpot/rd*rcp/psy*dtt * ( wg*(1.-areas) + areas )                    
      egidif=                                                                   
     &  amax1(0.0, egi-(snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)               
      egit  =                                                                   
     &  amin1(egi,     (snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)               
c                                                                               
c----------------------------------------------------------------------         
c     calculation of interception loss from ground-snow. if snow patch          
c     shrinks, energy is taken from egi to warm exposed soil to tgs.            
c----------------------------------------------------------------------         
c                                                                               
      t1 = snoww(2) - 1./asnow                                                  
      t2 = amax1( 0., t1 )                                                      
      aven = egi - t2*hlat*1.e3/snofac                                          
      if ( (t1-t2)*egi .gt. 0. ) aven = egi                                     
      darea = aven/( (tsnow-tg)*csoil - 1./asnow*hlat*1.e3/snofac)
      arean = areas + darea                                                     
      egidif = egidif - amin1( 0., arean )*asnow*hlat*1.e3/snofac*rsnow         
      darea = amax1( darea, -areas )                                            
      darea = amin1( 1.-areas, darea )                                          
      heaten = (tsnow-tg)*csoil*darea*rsnow                                     
      egit = egit + ( egi - heaten - egidif )*rsnow                             
      egi = egit                                                                
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      d1 = 1./ra + 1./rb + 1./rd                                                
      taen = ( (tgs+dtg)/rd + (tc+dtc)/rb + tm/ra ) / d1                        
      hend = ( taen - tm ) * rcp / ra + (ecidif + egidif)/dtt                   
      y = ht - hend                                                             
      i = i + 1                                                                 
      if ( i .gt. itrunk ) go to 200                                            
c                                                                               
      call newton(ht,y,finc,nox,nonpos,iwalk,lx)                                
      if(nox.eq.0)go to 2000                                                    
c                                                                               
200    continue                                                                 
c                                                                               
c----------------------------------------------------------------------         
c     exit from non-neutral calculation                                         
c     evapotranspiration fluxes calculated first ( j m-2 )                      
c                                                                               
c----------------------------------------------------------------------         
c     calculation of transpiration and soil evaporation fluxes for the          
c        end of the timestep. see figure (2) of se-86.                          
c                                                                               
c      ect         (e-c)   : equation (64) , SE-86                              
c      egs         (e-s)   : equation (66) , SE-86                              
c----------------------------------------------------------------------         
c                                                                               
      hrr = hr                                                                  
      if ( fg .lt. .5 ) hrr = 1.                                                
c                                                                               
      coct = (1.-wc)/ ( rst*fc + 2.*rb )                                        
      cogs1 = (1.-areas)/(rd+rsoil*fg)*(1.-wg)*hrr                              
      cogs2 = cogs1 / hrr                                                       
c                                                                               
      ect = ecpot * coct * rcp/psy * dtt                                        
      egs = (etgs + getgs*dtg ) * cogs1                                         
     &      - ( ea + deadtg*dtg + deadtc*dtc + deadqm*dqm ) * cogs2             
      egs = egs * rcp/psy * dtt                                                 
      egsmax = www(1) / 2. * zdepth(1) * poros(1) * hlat * 1000.                   
      egidif = egidif + amax1( 0., egs - egsmax )                               
      egs = amin1 ( egs, egsmax )                                               
c                                                                               
c----------------------------------------------------------------------         
c     sensible heat flux calculated with latent heat flux correction            
c----------------------------------------------------------------------         
c                                                                               
c     calculation of sensible heat fluxes for the end of the timestep.          
c        see figure (2) of se-86.  note that interception loss excess           
c        energies (ecidif, egidif) are added.                                   
c                                                                               
c      hc          (hc)    : equation (63) , SE-86                              
c      hg          (hgs)   : equation (65) , SE-86                              
c----------------------------------------------------------------------         
c                                                                               
      hc = hc + (hcdtc*dtc + hcdtg*dtg + hcdth*dth)*dtt + ecidif                
      hg = hg + (hgdtc*dtc + hgdtg*dtg + hgdth*dth)*dtt + egidif                
c                                                                               
c----------------------------------------------------------------------         
c     test of dew condition. latent heat fluxes set to zero if sign             
c     of flux changes over time step.excess of energy donated to sensible       
c     heat flux.                                                                
c     calculation of total latent heat fluxes,  see figure (2), se-86.          
c                                                                               
c      ec          (ec)    : equation (63) , SE-86                              
c      eg          (eg)    : equation (65) , SE-86                              
c----------------------------------------------------------------------         
c                                                                               
      ecf = sign( 1., ecpot )                                                   
      egf = sign( 1., egpot )                                                   
      dewc = fc * 2. - 1.                                                       
      dewg = fg * 2. - 1.                                                       
c                                                                               
      if(dewc*ecf.gt.0.0) go to 300                                             
      hc = hc + eci + ect                                                       
      eci = 0.                                                                  
      ect = 0.                                                                  
300   if(dewg*egf.gt.0.0) go to 400                                             
      hg = hg + egs + egi                                                       
      egs = 0.                                                                  
      egi = 0.                                                                  
400   continue                                                                  
c                                                                               
      ec = eci + ect                                                            
      eg = egi + egs                                                            
c                                                                               
c----------------------------------------------------------------------         
c     adjustment of : temperatures and vapor pressure                           
c                     net radiation terms                                       
c                     storage heat fluxes                                       
c                     longwave loss and effective surface temperature           
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      ta  = taen                                                                
      ea = ea + deadtc*dtc + deadtg*dtg                                         
c                                                                               
      radt(1) = radt(1) + rncdtc*dtc + rncdtg*dtg                               
      radt(2) = radt(2) + rngdtc*dtc + rngdtg*dtg                               
c                                                                               
c----------------------------------------------------------------------         
c     calculation of storage heat fluxes                                        
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      chf = ccx / dtt * dtc                                                     
      shf = cg / dtt * dtg + timcon*cg*2. * ( tgs+dtg - td )                    
CH
      gflux = timcon*cg*2. * ( tgs+dtg - td )                    
CH
c                                                                               
      zlwup = zlwup - rncdtc * dtc / 2.                                         
     &              - rngdtg * dtg * (1.-vcover*(1.-thermk) )                   
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
      return                                                                    
      end                                                                       
c======================================================================         
c                                                                               
c        *********    auxiliary subroutine     **********                
c                                                                               
c=======================================================================        
c                                                                               
       subroutine rbrd                                                          
c                                                                               
c=======================================================================        
c                                                                               
c      calculation of rb and rd as functions of u2 and temperatures             
c                                                                               
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       rb (grb)       canopy to cas aerodynamic resistance (s m-1)             
c       rd (grd)       ground to cas aerodynamic resistance (s m-1)             
c       ta (gta)       cas temperature (k)                                      
c                      cas : canopy air space                                   
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
c-----------------------------------------------------------------------        
c     rb : equation (a9), SE-86                                                
c-----------------------------------------------------------------------        
c                                                                               
      temdif = amax1( 0.1,  tc-tm )                                             
      fac = zlt/890.* sqrt( sqrt(temdif/0.05))                                  
      rb  = 1.0/(sqrt(u2)/rbc+fac)                                              
c                                                                               
c-----------------------------------------------------------------------        
c     rd : equation (a15), se-86                                               
c-----------------------------------------------------------------------        
c                                                                               
      temdif = amax1( 0.1, tgs-tm )                                             
      fih = sqrt( 1. + 9. * gx * temdif * z2 / tgs / ( u2*u2) )                  
      rd  = rdc / u2 / fih                                                      
c                                                                               
c-----------------------------------------------------------------------        
c     calculation of ta, ht and effective soil aero-surface conductance,       
c        see equation (66) of SE-86 with snow area term added                   
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      d1 = 1./ra + 1./rb + 1./rd                                                
      ta = ( tgs/rd + tc/rb + tm/ra ) / d1                                      
      ht = ( ta - tm ) * rcp / ra                                               
c                                                                               
      cogr = (1.-wg)/(rsoil*fg+rd)                                              
      cogs =  wg/rd                                                             
      cog1 = (cogs + cogr*hr) * (1.-areas) + areas/rd                           
      cog2 = (cogs + cogr   ) * (1.-areas) + areas/rd                           
c                                                                               
      return                                                                   
      end                                                                      
c                                                                               
c=======================================================================        
c                                                                               
      subroutine phosib                                                         
c                                                                               
c                                                                               
c=======================================================================        
c                                                                               
c     calculation of canopy photosynthetic rate using the integrated            
c     model relating assimilation and stomatal conductance.                     
c     method uses numerical solution based on extrapolation from error          
c     versus co2i line.                                                         
c     units are converted from mks to biological units in this routine.         
c     base reference :  SE-92                                                  
c                                                                               
c                          units                                                
c                         -------                                               
c                                                                               
c      pco2m, pco2a, pco2i, po2m                : pascals                       
c      co2a, co2s, co2i, h2oa, h2os, h2oa       : mol mol-1                     
c      vmax0, respn, assim, gs, gb, ga, pfd     : mol m-2 s-1                   
c      effcon                                   : mol co2 mol quanta-1          
c      gcan, 1/rb, 1/ra, 1/rst                  : m s-1                         
c      evapkg                                   : kg m-2 s-1                    
c      q                                        : kg kg-1                       
c                                                                               
c                       conversions                                             
c                      -------------                                            
c                                                                               
c      1 mol h2o           = 0.018 kg                                           
c      1 mol co2           = 0.044 kg                                           
c      h2o (mol mol-1)     = ea / psur ( mb mb-1 )                              
c      h2o (mol mol-1)     = q*mm/(q*mm + 1)                                    
c      gs  (co2)           = gs (h2o) * 1./1.6                                  
c      gs  (mol m-2 s-1 )  = gs (m s-1) * 44.6*tf/t*p/po                        
c      par (mol m-2 s-1 )  = par(w m-2) * 4.6*1.e-6                             
c      mm  (molair/molh2o) = 1.611                                              
c                                                                               
c                                                                               
c                         output                                                
c                      -------------                                            
c                                                                               
c      assimn              = canopy net assimilation rate                       
c      ea                  = canopy air space vapor pressure                    
c      1/rst               = canopy conductance                                 
c      pco2i               = internal co2 concentration                         
c      respc               = canopy respiration                                 
c      respg               = ground respiration                                 
c      rstfac(4)      canopy resistance stress factors                         
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c         rstfac(1) ( f(h-s) )               : equation (17,18), SE-92         
c         rstfac(2) ( f(soil) )              : equation (12 mod), SE-89         
c         rstfac(3) ( f(temp) )              : equation (5b)   , CO-92          
c         rstfac(4) ( f(h-s)*f(soil)*f(temp))                                   
c                                                                               
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
      dimension pco2y(6), eyy(6)                                                
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
CHR      respg = 0.                                                                

      if (irespg.eq.0) respg = 0.


c...  Tropical forest RJA - Patrick Meir (pers com 1994) (molCO2 m-2 s-1)
      if (irespg.eq.1) respg = 1.e-06 * exp (0.084*(td-273.15) - 0.23)

cHR..  Tropical forest k83 - ajuste sazonal: aumento est.chuvosa, reduz est.seca 
c	(molCO2 m-2 s-1)
      if (irespg.eq.3) then
      xwet = 1. 
      if (www(1).lt.0.95) xwet = www(1)
      if (www(2).lt.0.9) xwet = (amin1(www(1),0.8)**3.)     
      xdry = 1. - xwet
      
      respg = 1.e-06 * 
     &  (    xwet * exp(0.090*(td-273.15)-0.01)+
     &       xdry * exp(0.080*(td-273.15)-0.30)   - 2.0 ) ! (avg ~ 4a8 K83)              
      endif

c...  Cerrado ss PDG - Rocha 2002 (molCO2 m-2 s-1)
      if (irespg.eq.2) respg = 1.e-06 * exp (0.159*(td-273.15) - 1.886)
     
CH
c                                                                               
c----------------------------------------------------------------------         
c
      c3     = 0.                                                               
      if( effcon .gt. 0.07 ) c3 = 1.                                            
      c4     = 1. - c3                                                          
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c                                                                               
c     calculation of canopy par use parameter.                                  
c                                                                               
c     fparkk      (pi)     : equation (31) , SE-92                            
c-----------------------------------------------------------------------        
c                                                                               
      scatp    =     green   * ( tran(1,1) + ref(1,1) )                         
     &         +( 1.-green ) * ( tran(1,2) + ref(1,2) )                         
c                                                                               
      scatg    = tran(1,1) + ref(1,1)                                           
      park = sqrt(1.-scatp) * gmudmu                                            
      fparc = 1. - exp ( -park*zlt )                                         
      fparkk   = fparc / park * green 
c
c-----------------------------------------------------------------------        
c                                                                               
c     q-10 temperature effects :                                                
c      qt          (qt)    : table (2)     , SE-92                             
c      qt for vm changed to 2.1                                                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      qt = 0.1*( tc - trop )                                                    
      respn = respcp * vmax0 * rstfac(2)                                        
      respc = respn * 2.0**qt / ( 1. + exp( trda*(tc-trdm )) )                  
      vm = vmax0 * 2.1**qt                                                      
c                                                                               
      templ = 1. + exp(slti*(hlti-tc))                                          
      temph = 1. + exp(shti*(tc-hhti))                                          
      rstfac(3) = 1./( templ*temph)                                             
      vm    = vm/temph * rstfac(2) * c3                                         
     &      + vm * rstfac(2)*rstfac(3) * c4                                     
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     Michaelis-Menten constants for co2 and o2, co2/o2 specificity,            
c     compensation point                                                        
c                                                                               
c      zkc          (kc)     : table (2)     , SE-92                           
c      zko          (ko)     : table (2)     , SE-92                           
c      spfy         (s)      : table (2)     , SE-92                           
c      gammas       (gamma-*): table (2)     , SE-92                           
c      omss         (omega-s): equation (13) , SE-92                           
c      bintc        (b*zlt)  : equation (35) , SE-92                           
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      zkc = 30. * 2.1**qt                                                       
      zko = 30000. * 1.2**qt                                                    
      spfy = 2600. * 0.57**qt                                                   
      gammas = 0.5 * po2m/spfy * c3                                             
      pfd    = 4.6*1.e-6 * gmudmu * ( radn(1,1)+radn(1,2) )                     
c                                                                               
      h2oi   = etc/psur                                                         
      h2oa   =  ea/psur                                                         
      h2om   =  em/psur                                                         
      h2osl  =etgs/psur                                                         
c                                                                               
      tprcor = tf*psur*100./1.013e5                                             
c                                                                               
      gbh2o  = 0.5/rb * 44.6*tprcor/tc                                          
      gah2o  = 1.0/ra * 44.6*tprcor/tm                                          
      gog1   = cog1   * 44.6*tprcor/tgs                                         
      gog2   = cog2   * 44.6*tprcor/tgs                                         
c                                                                               
      rrkk   = zkc*( 1. + po2m/zko ) * c3 + vmax0/5.* ( 1.8**qt) * c4           
      par    = pfd*effcon*( 1.-scatg )                                          
      bintc  = binter*zlt*green*amax1(0.1,rstfac(2))                            
c                                                                               
      omss  = ( vmax0/2.0 ) * ( 1.8**qt )/templ * rstfac(2) * c3                
     &                                   + rrkk * rstfac(2) * c4                
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     first guess is midway between compensation point and maximum              
c     assimilation rate.                                                        
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      range    = pco2m * ( 1. - 1.6/gradm ) - gammas                            
c                                                                               
      do 1000 ic = 1, 6                                                         
      pco2y(ic) = 0.                                                            
      eyy(ic) = 0.                                                              
1000  continue                                                                  
c                                                                               
      do 2000 ic = 1, 6                                                         
c                                                                               
      ic2 = ic                                                                  
c                                                                               
      call       sortin( eyy, pco2y, range, gammas, ic )                        
c                                                                               
      call       cycalc( fparkk, vm, gradm, bintc, atheta, btheta,              
     &                   gah2o, gbh2o, gog1, gog2, wc,                          
     &                   h2oi, h2om, h2osl, par, pco2m, psur,                   
     &                   gammas, respc, respg, rrkk, omss, c3, c4,              
     &                   pco2y(ic), eyy(ic), gsh2o, assimn, h2os, h2oa )        
c                                                                               
      if( abs(eyy(ic)) .lt. 0.1 ) go to 100                                     
c                                                                               
2000  continue                                                                  
c                                                                               
100   pco2i = pco2y(ic2)                                                        
c                                                                               
      rstfac(1) = h2os/h2oi                                                     
      rstfac(4) = rstfac(1)*rstfac(2)* rstfac(3)                                
      rst   = amin1( 1.e6, 1./( gsh2o*tc/( 44.6*tprcor) ) )                     
      ea    = h2oa*psur                                                         
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine cycalc( fparkk, vm, gradm, bintc, atheta, btheta,              
     &                   gah2o, gbh2o, gog1, gog2, wc,                          
     &                   h2oi, h2om, h2osl, par, pco2m, psur,                   
     &                   gammas, respc, respg, rrkk, omss, c3, c4,              
     &                   pco2i, eyy, gsh2o, assimn, h2os, h2oa )                
c
c=======================================================================        
c                                                                               
c     calculation equivalent to steps in figure 4 of SE-92                     
c     c4 calculation based on CO-92.                                            
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       pco2i          canopy internal co2 concentration (mol mol-1)            
c       gsh2o          canopy conductance (mol m-2 s-1)                         
c       h2os           canopy surface h2o concentration (mol mol-1)             
c                                                                               
c++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++        
c                                                                               
c       omc            rubisco limited assimilation (mol m-2 s-1)               
c                        (omega-c): equation (11) , SE-92                      
c       ome            light limited assimilation (mol m-2 s-1)                 
c                        (omega-e): equation (12) , SE-92                      
c       oms            sink limited assimilation (mol m-2 s-1)                  
c       co2s           canopy surface co2 concentration (mol mol-1)             
c                        equation (18c) , SE-92                                
c       assimn         (a-n)    :  equation (14,15), SE-92                     
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
      omc = vm  * ( pco2i-gammas )/( pco2i + rrkk ) * c3 + vm * c4              
      ome = par * ( pco2i-gammas )/( pco2i+2.*gammas ) * c3 + par * c4          
      sqrtin= amax1( 0., ( (ome+omc)**2 - 4.*atheta*ome*omc ) )                 
      omp   = ( ( ome+omc ) - sqrt( sqrtin ) ) / ( 2.*atheta )                  
      oms   = omss * c3 + omss*pco2i * c4                                       
      sqrtin= amax1( 0., ( (omp+oms)**2 - 4.*btheta*omp*oms ) )                 
      assim = ( ( oms+omp ) - sqrt( sqrtin ) ) / ( 2.*btheta )                  
      assimn= ( assim - respc) * fparkk                                         
c                                                                               
c-----------------------------------------------------------------------        
c     gah2o bottom stopped to prevent negative values of pco2a                  
c-----------------------------------------------------------------------        
c                                                                               
        pco2a = pco2m - (1.4/amax1(0.446,gah2o) *                               
     >             (assimn - respg)* psur*100.)                                

      co2s  = pco2a/(psur*100.) - 1.4*assimn/gbh2o                              
c                                                                               
      assmt = amax1( 1.e-12, assimn )                                           
      co2st = amin1( co2s, pco2a/(psur*100.) )                                  
      co2st = amax1( co2st,1./(psur*100.) )                                     
c                                                                               
      div2  = gah2o + gbh2o + gog2                                              
      hcdma = h2oi*co2st / ( gradm*assmt )                                      
      alpha = hcdma*gbh2o*(1.-wc) / div2                                        
      beta  = ( -hcdma*bintc*gbh2o*(1.-wc) + h2oi*gbh2o*wc                      
     &        + h2osl*gog1 + h2om*gah2o ) / div2                                
      aquad = hcdma                                                             
      bquad = gbh2o*( hcdma-alpha ) - h2oi - bintc*hcdma                        
      cquad = -gbh2o*( hcdma*bintc + beta )                                     
c                                                                               
      sqrtin= amax1( 0., ( bquad**2 - 4.*aquad*cquad ) )                        
      gsh2o = ( -bquad + sqrt ( sqrtin ) ) / (2.*aquad)                         
      h2os  = ( gsh2o-bintc ) * hcdma                                           
      h2os  = amin1( h2os, h2oi )                                               
      h2os  = amax1( h2os, 1.e-7)                                               
      gsh2o = h2os/hcdma + bintc                                                
      div3  = gbh2o*gsh2o/(gbh2o+gsh2o)*(1.-wc) + gbh2o*wc + gog2               
     &        + gah2o                                                           
      h2oa  = ( ( h2oi - h2oi*gsh2o/(gbh2o+gsh2o) )*gsh2o*(1.-wc)               
     &        + h2oi*gbh2o*wc + h2osl*gog1 + h2om*gah2o ) / div3                
      h2os  = ( h2oa*gbh2o + h2oi*gsh2o )/( gbh2o + gsh2o )                     
c                                                                               
c-----------------------------------------------------------------------        
c     implied value of co2i derived from assimilation rate and stomatal         
c     conductance.                                                              
c-----------------------------------------------------------------------        
c                                                                               
      pco2in = ( co2s - 1.6 * assimn / gsh2o )*psur*100.                        
      eyy = pco2i - pco2in                                                      
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
      subroutine sortin( eyy, pco2y, range, gammas, ic )                        
c                                                                               
c=======================================================================        
c                                                                               
c-----------------------------------------------------------------------        
                                                                               
c     arranges successive pco2/error pairs in order of increasing pco2.         
c     estimates next guess for pco2 using combination of linear and             
c     quadratic fits.                                                           
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c                                                                               
      dimension eyy(6), pco2y(6)                                                
c                                                                               
      if( ic .ge. 4 ) go to 500                                                 
      pco2y(1) = gammas + 0.5*range                                             
      pco2y(2) = gammas + range*( 0.5 - 0.3*sign(1.0,eyy(1)) )                  
      pco2y(3) = pco2y(1)                                                       
     &          - (pco2y(1)-pco2y(2))/(eyy(1)-eyy(2)+1.e-10)*eyy(1)             
c                                                                               
      pmin = amin1( pco2y(1), pco2y(2) )                                        
      emin = amin1(   eyy(1),   eyy(2) )                                        
      if ( emin .gt. 0. .and. pco2y(3) .gt. pmin ) pco2y(3) = gammas            
      go to 200                                                                 
500   continue                                                                  
c                                                                               
      n = ic - 1                                                                
      do 1000 j = 2, n                                                          
      a = eyy(j)                                                                
      b = pco2y(j)                                                              
      do 2000 i = j-1,1,-1                                                      
      if(eyy(i) .le. a ) go to 100                                              
      eyy(i+1) = eyy(i)                                                         
      pco2y(i+1) = pco2y(i)                                                     
2000  continue                                                                  
      i = 0                                                                     
100   eyy(i+1) = a                                                              
      pco2y(i+1) = b                                                            
1000  continue                                                                  
c                                                                               
      pco2b = 0.                                                                
      is    = 1                                                                 
      do 3000 ix = 1, n                                                         
      if( eyy(ix) .lt. 0. ) pco2b = pco2y(ix)                                   
      if( eyy(ix) .lt. 0. ) is = ix                                             
3000  continue                                                                  
      i1 = is-1                                                                 
      i1 = max0(1, i1)                                                          
      i1 = min0(n-2, i1)                                                        
      i2 = i1 + 1                                                               
      i3 = i1 + 2                                                               
      isp   = is + 1                                                            
      isp = min0( isp, n )                                                      
      is = isp - 1                                                              
c                                                                               
      pco2yl=pco2y(is)                                                          
     &      - (pco2y(is)-pco2y(isp))/(eyy(is)-eyy(isp))*eyy(is)                 
c                                                                               
c----------------------------------------------------------------------         
c   method using a quadratic fit                                                
c----------------------------------------------------------------------         
c                                                                               
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
      pco2yq= amax1( pco2yq, pco2b )                                            
      pco2y(ic) = ( pco2yl+pco2yq)/2.                                           
c                                                                               
200   continue                                                                  
c                                                                               
      pco2y(ic) = amax1 ( pco2y(ic), 0.01 )                                     
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c======================================================================         
c                                                                               
      subroutine delrn                                                          
c                                                                               
c======================================================================         
c                                                                               
c     partial derivatives of radiative and sensible heat fluxes                 
c                                                                               
c----------------------------------------------------------------------         
c
       include 'comsibc.h'                                                      
       include 'pardif.h'                                                       
c                                                                               
      tc3 = tc * tc * tc                                                        
      tg3 = tgs * tgs * tgs                                                     
      fac1 = ( 1.-thermk ) * vcover                                             
      fac2 = 1.                                                                 
c                                                                               
      rncdtc = - 2. * 4. * fac1 * stefan * tc3                                  
      rncdtg = 4. * fac1 * fac2 * stefan * tg3                                  
c                                                                               
      rngdtg = - 4. * fac2 * stefan * tg3                                       
      rngdtc = 4. * fac1 * fac2 * stefan * tc3                                  
c                                                                               
      return                                                                    
      end                                                                       
c======================================================================         
c                                                                               
      subroutine delhf                                                          
c                                                                               
c======================================================================         
c                                                                               
c     calculation of partial derivatives of canopy and ground sensible          
c     heat fluxes with respect to tc, tgs, and theta-m.                         
c     calculation of initial sensible heat fluxes.                              
c                                                                               
c========================================================================       
c                                                                               
c                                                                               
c       hc             canopy sensible heat flux (j m-2)                        
c       hg             ground sensible heat flux (j m-2)                        
c       hcdtc          dhc/dtc                                                  
c       hcdtg          dhc/dtgs                                                 
c       hcdth          dhc/dth                                                  
c       hgdtc          dhg/dtc                                                  
c       hgdtg          dhg/dtgs                                                 
c       hgdth          dhg/dth                                                  
c       aac            dh/dtc                                                   
c       aag            dh/dtgs                                                  
c       aam            dh/dth                                                   
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
c     fluxes expressed in joules m-2                                            
c                                                                               
c      hc  ( h-c ) : equation(63), SE-86                                        
c      hg  ( h-g ) : equation(65), SE-86                                        
c                                                                               
c----------------------------------------------------------------------         
c
       include 'comsibc.h'                                                      
       include 'pardif.h'                                                       
c                                                                               
      d1 = 1./ra + 1./rb + 1./rd                                                
      ta = ( tgs/rd + tc/rb + tm/ra ) / d1                                      
c                                                                               
      hc = rcp * ( tc - ta ) / rb * dtt                                         
      hg = rcp * ( tgs - ta ) / rd * dtt                                        
c----------------------------------------------------------------------         
c                                                                               
c      n.b.      fluxes expressed in joules m-2                                 
c                                                                               
c      hcdtc     (dhc/dtc) : equation (14) , SA-89B                             
c      hcdtg     (dhc/dtgs): equation (14) , SA-89B                             
c      hcdth     (dhc/dth) : equation (14) , SA-89B                             
c      hgdtc     (dhg/dtc) : equation (15) , SA-89B                             
c      hgdtg     (dhg/dtgs): equation (15) , SA-89B                             
c      hgdth     (dhg/dth) : equation (15) , SA-89B                             
c      aac       (dh/dtc)  : equation (12) , SA-89B                             
c      aag       (dh/dtgs) : equation (12) , SA-89B                             
c      aam       (dh/dth)  : equation (12) , SA-89B                             
c----------------------------------------------------------------------         
c                                                                               
      hcdtc = rcp / rb * ( 1./ra + 1./rd ) / d1                                 
      hcdtg = - rcp / ( rb * rd ) / d1                                          
c                                                                               
      hgdtg = rcp / rd * ( 1./ra + 1./rb ) / d1                                 
      hgdtc = - rcp / ( rd * rb ) / d1                                          
c                                                                               
      hcdth=- rcp/( rb*ra )/d1*bps                                              
      hcdqm=0.0                                                                 
c                                                                               
      hgdth=-rcp/(rd*ra)/d1*bps                                                 
      hgdqm=0.0                                                                 
c                                                                               
      aag=1.0/(rd*d1)                                                           
      aac=1.0/(rb*d1)                                                           
      aam=1.0/(ra*d1)*bps                                                       
c                                                                               
      return                                                                    
      end                                                                       
c======================================================================         
c                                                                               
      subroutine delef                                                          
c                                                                               
c======================================================================         
c                                                                               
c     calculation of partial derivatives of canopy and ground latent            
c     heat fluxes with respect to tc, tgs, theta-m, and qm.                     
c     calculation of initial latent heat fluxes.                                
c                                                                               
c      ec  ( e-c ) : equation(64), SE-86                                        
c      eg  ( e-gs) : equation(66), SE-86                                        
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       ec             ect + eci                                                
c       eg             egs + egi                                                
c       ecdtc          dec/dtc                                                  
c       ecdtg          dec/dtgs                                                 
c       ecdqm          dec/dqm                                                  
c       egdtc          deg/dtc                                                  
c       egdtg          deg/dtgs                                                 
c       egdqm          deg/dqm                                                  
c       bbc            de/dtc                                                   
c       bbg            de/dtgs                                                  
c       bbm            de/dqm                                                   
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
       include 'comsibc.h'                                                      
       include 'pardif.h'                                                       
c                                                                               
c----------------------------------------------------------------------         
c     modification for soil dryness : hr = rel. humidity in top layer           
c----------------------------------------------------------------------         
c                                                                               
      hrr = hr                                                                  
      if ( fg .lt. .5 ) hrr = 1.                                                
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     calculation of surface resistance components, see equations (64,66)       
c       of SE-86                                                                
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      rcc = rst*fc + 2. * rb                                                    
      coc = (1.-wc)/rcc + wc/(2.*rb)                                            
c                                                                               
      cogr = (1.-wg)/(rsoil*fg+rd)                                              
      cogs =  wg/rd                                                             
      cog1 = (cogs + cogr*hrr) * (1.-areas) + areas/rd                          
      cog2 = (cogs + cogr    ) * (1.-areas) + areas/rd                          
c                                                                               
      d2 = 1./ra + coc + cog2                                                   
      top = coc * etc + cog1 * etgs + em/ra                                     
      ea = top / d2                                                             
c                                                                               
      ec = ( etc - ea ) * coc * rcp/psy * dtt                                   
      eg = ( etgs*cog1 - ea*cog2 ) * rcp/psy * dtt                              
c                                                                               
      deadtc = getc * coc / d2                                                  
      deadtg = getgs * cog1 / d2                                                
c                                                                               
c-----------------------------------------------------------------------        
c      ecdtc     (dec/dtc) : equation (14) , SA-89B                             
c      ecdtg     (dec/dtgs): equation (14) , SA-89B                             
c      ecdqm     (dec/dqm) : equation (14) , SA-89B                             
c      egdtc     (deg/dtc) : equation (15) , SA-89B                             
c      egdtg     (deg/dtgs): equation (15) , SA-89B                             
c      egdqm     (deg/dqm) : equation (15) , SA-89B                             
c      bbc       (de/dtc)  : equation (13) , SA-89B                             
c      bbg       (de/dtgs) : equation (13) , SA-89B                             
c      bbm       (de/dqm)  : equation (13) , SA-89B                             
c-----------------------------------------------------------------------        
c                                                                               
      ecdtc = ( getc - deadtc ) * coc * rcp / psy                               
      ecdtg = - deadtg * coc * rcp / psy                                        
c                                                                               
      egdtg = ( getgs*cog1 - deadtg*cog2 ) * rcp / psy                          
      egdtc = - deadtc * cog2 * rcp / psy                                       
c                                                                               
      sh    = epsfac * em / (psur -em )                                         
      deadem=1.0/( ra*d2 )                                                      
      demdqm=epsfac * psur/(epsfac+sh)**2                                       
      deadqm=deadem*demdqm                                                      
c                                                                               
      ecdqm=- deadqm*coc*rcp/psy                                                
      egdqm=-deadqm*cog2*rcp/psy                                                
c                                                                               
      bbg=(cog1/d2) * getgs*epsfac * psur/(psur-etgs)**2                        
      bbc=(coc /d2) * getc *epsfac * psur/(psur-etc )**2                        
      bbm=1.0/(ra*d2)                                                           
c                                                                               
      return                                                                    
      end                                                                       
c======================================================================         
c                                                                               
      subroutine sibslv                                                         
c                                                                               
c                                                                               
c======================================================================         
c                                                                               
c     solve for time changes of pbl and sib variables,                          
c     using a semi-implicit scheme.                                             
c                                                                               
c      dtc, dtg, dth, dqm  : equations(12-15) , SA-89B + radiation terms        
c                                                                               
c                                                                               
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                                                                               
c       dtc            canopy temperature increment (K)                         
c       dtg            ground surface temperature increment (K)                 
c       dth            mixed layer potential temperature increment (K)          
c       dqm            mixed layer mixing ratio increment (kg kg-1)             
c                                                                               
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                                                                               
       include 'comsibc.h'                                                      
       include 'pardif.h'                                                       
c                                                                               
c                                                                               
      dimension pblsib(4,4),cvec(4),solvec(4),chin(4,5),work(4,5)               
c                                                                               
      grav2 = 0.01*gx                                                            
      gv    = grav2*rhoair/ra                                                   
      psb   = 100.                                                              
c                                                                               
      etem=0.                                                                   
      dthb = 0.                                                                 
      dwb = 0.                                                                  
c                                                                               
c     cvec uses provisional values of the fluxes.                               
c                                                                               
      fths=(hc+hg) / (dtt*cpair*bps)                                            
      fws =(ec+eg) / (dtt*hlat     )                                            
c                                                                               
c     tg equation                                                               
c                                                                               
      pblsib(1,1)= cg/dtt + hgdtg+egdtg-rngdtg + timcon*cg*2.0                  
      pblsib(1,2)=        + hgdtc+egdtc-rngdtc                                  
      pblsib(1,3)=        + hgdth                                               
      pblsib(1,4)=                egdqm                                         
c                                                                               
c     tc equation                                                               
c                                                                               
      pblsib(2,1)=        + hcdtg+ecdtg-rncdtg                                  
      pblsib(2,2)= ccx/dtt+ hcdtc+ecdtc-rncdtc                                  
      pblsib(2,3)=        + hcdth                                               
      pblsib(2,4)=                ecdqm                                         
c                                                                               
c     theta equation                                                            
c                                                                               
      pblsib(3,1)=-gv* aag                                                      
      pblsib(3,2)=-gv* aac                                                      
      pblsib(3,3)=-gv*(aam-1.0) + etem + psb/dtt                                
      pblsib(3,4)= 0.0                                                          
c                                                                               
c     sh equation                                                               
c                                                                               
      pblsib(4,1)=-gv*bbg                                                       
      pblsib(4,2)=-gv*bbc                                                       
      pblsib(4,3)= 0.0                                                          
      pblsib(4,4)=-gv*(bbm-1.0) + etem + psb/dtt                                
c                                                                               
      cvec(1) = radt(2) - hg/dtt - eg/dtt - timcon*(tgs-td)*cg*2.               
      cvec(2) = radt(1) - hc/dtt - ec/dtt                                       
      cvec(3) = grav2*fths + etem*dthb                                          
      cvec(4) = grav2*fws  + etem*dwb
c     
c     solve 4 x 4 matrix equation                                               
c                                                                               
      do 4000 j=1,4                                                             
      do 4000 i=1,4                                                             
 4000 chin(i,j)=pblsib(i,j)                                                     
      do 4100 i=1,4                                                             
 4100 chin(i,5)=cvec(i)                                                         
c                                                                               
      call gauss(chin,4,5,solvec,work)                                          
c                                                                               
      dtg=solvec(1)                                                             
      dtc=solvec(2)                                                             
      dth=solvec(3)                                                             
      dqm=solvec(4)                                                             
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c======================================================================         
c                                                                               
      subroutine dtcdtg                                                         
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     calculation of temperature tendencies assuming no interaction             
c     with the pbl : equations(69,70), SE-86                                    
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
       include 'comsibc.h'                                                      
       include 'pardif.h'                                                       
c                                                                               
      ccodtc = ccx / dtt - rncdtc + hcdtc + ecdtc                               
      ccodtg = - rncdtg + hcdtg + ecdtg                                         
      ccorhs = radt(1) - ( hc + ec ) / dtt                                      
c                                                                               
      gcodtg = cg / dtt + timcon*cg*2. - rngdtg + hgdtg + egdtg                 
      gcodtc = - rngdtc + hgdtc + egdtc                                         
      gcorhs = radt(2) - timcon*cg*2. * ( tgs -td ) - ( hg + eg ) / dtt         
c                                                                               
      denom = ccodtc * gcodtg - ccodtg * gcodtc                                 
c                                                                               
      dtc = ( ccorhs * gcodtg - ccodtg * gcorhs ) / denom                       
      dtg = ( ccodtc * gcorhs - ccorhs * gcodtc ) / denom                       
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
c=======================================================================        
c                                                                               
      subroutine snow2                                                          
c                                                                               
c=======================================================================        
c                                                                               
c    snowmelt / refreeze calculation                                            
c----------------------------------------------------------------------         
c                                                                               
c     calculation of snowmelt and modification of temperatures                  
c                                                                               
c     modification deals with snow patches:                                     
c          ts < tf, tsnow = ts                                                  
c          ts > tf, tsnow = tf                                                  
c                                                                               
c-----------------------------------------------------------------------        
       include 'comsibc.h'                                                      
c                                                                               
      do 1000 iveg = 1, 2                                                       
c                                                                               
      realc = (2 - iveg)*1.                                                     
      realg = (iveg - 1)*1.                                                     
c                                                                               
      cctt = realc*ccx  +  realg*cg                                             
      cct  = realc*ccx  +  realg*csoil                                          
      ts   = realc*tc   +  realg*tg                                             
      dts  = realc*dtc  +  realg*dtg                                            
      flux = realc*chf  +  realg*dtg/dtt*cg                                     
c                                                                               
      tsnow = amin1 ( tf-0.01, ts )                                             
      snowhc = amin1( 0.05, snoww(iveg) ) * cw * realg                          
      zmelt = 0.                                                                
c                                                                               
      if ( snoww(iveg) .gt. 0. ) go to 100                                      
      if ( ( ts+dts) .gt. tf ) go to 500                                        
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     no snow  present, simple thermal balance with possible freezing.          
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      freeze = amin1 ( 0., (flux*dtt - ( tf-0.01 - ts )*cctt ) )                
      snoww(iveg) = amin1( capac(iveg), - freeze/snomel )                       
      zmelt = capac(iveg) - snoww(iveg)                                         
      capac(iveg) = 0.                                                          
      dts = dts + snoww(iveg)*snomel/cctt                                       
      go to 500                                                                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     snow present                                                              
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
100   continue                                                                  
c                                                                               
      if ( ts .lt. tf .and. (ts+dts) .lt. tf ) go to 500                        
      if ( ts .gt. tf ) go to 200                                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     snow present : ts < tf,  ts+dts > tf                                      
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      avex = flux - ( tf-0.01 - ts ) * cctt/dtt                                 
      avmelt = ( avex/snomel * (areas*realg + realc ) )*dtt                     
      zmelt = amin1( avmelt, snoww(iveg) )                                      
      snoww(iveg) = snoww(iveg) - zmelt                                         
      avheat = avex*( 1.-areas )*realg + ( avmelt-zmelt )*snomel/dtt            
c                                                                               
      safe = amax1( ( 1.-areas*realg ), 1.e-8 )                                 
      dts = tf-0.01 - ts + avheat / ( cctt*safe )*dtt                           
      go to 500                                                                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     snow present and ts > tf : ground only.                                   
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
200   continue                                                                  
c                                                                               
      tbulk = tsnow*areas + ts*( 1. - areas )                                   
      tn = tbulk + dts                                                          
      exheat = cct*( 1.001-amax1(0.1,areas)) * dts                              
      exmelt = flux*dtt - exheat                                                
      heat = exheat                                                             
      dtsg = exheat / ( cct*(1.001-areas ))                                     
      if ( (ts+dtsg) .gt. tf ) go to 300                                        
      heat = ( tf-0.01 - ts ) * ( cct*(1.-areas) )                              
      dtsg = tf-0.01 - ts                                                       
c                                                                               
300   exmelt = exmelt + exheat - heat                                           
c                                                                               
      if( exmelt .lt. 0. ) go to 400                                            
      zmelt = exmelt/snomel                                                     
      if( asnow*(snoww(iveg)-zmelt) .lt. 1. )                                   
     &           zmelt = amax1( 0., snoww(iveg) - 1./asnow )                    
      snoww(iveg) = snoww(iveg) - zmelt                                         
      exmelt = exmelt - zmelt*snomel                                            
      zmelt2 = exmelt/ ( cct*( ts-tf )*asnow + snomel )                         
      zmelt2 = amin1( zmelt2, snoww(iveg) )                                     
      zmelt = zmelt + zmelt2                                                    
      snoww(iveg) = snoww(iveg) - zmelt2                                        
      exmelt = exmelt - zmelt2*( cct*( ts-tf )*asnow + snomel )                 
      dts  = dtsg + exmelt/cct                                                  
      go to 500                                                                 
c                                                                               
400   cool = amin1( 0., tf-0.01 - (ts+dtsg) ) * cct*(1.-areas)                  
      dtsg2 = amax1 ( cool, exmelt ) / ( cct*( 1.001-areas ) )                  
      exmelt = exmelt - dtsg2*cct*(1.-areas)                                    
      dtsg3 =exmelt/cctt                                                        
      dts = dtsg + dtsg2 + dtsg3                                                
c                                                                               
500   continue                                                                  
c                                                                               
      www(1) = www(1) + zmelt / ( poros(1) * zdepth(1) )                           
c                                                                               
      dtc = dtc*realg + dts*realc                                               
      dtg = dtg*realc + dts*realg                                               
c                                                                               
1000  continue                                                                  
c                                                                               
      fluxef = shf - cg*dtg/dtt                                                 
      dtd = fluxef / ( cg * 2. * sqrt ( pie*365. ) ) * dtt                      
c                                                                               
      return                                                                    
      end                                                                       
c=======================================================================        
c                                                                               
      subroutine radc2                                                          
c                                                                               
c=======================================================================        
c                                                                               
c     solar zenith angle computation; downcoming radiation at bottom.           
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
      dayspy = 365.                                                             
      if ( amod( year, 4. ) .eq. 0. ) dayspy = 366.                             
c                                                                               
c-----------------------------------------------------------------------        
c    julian day and time update; skip on 1st time step (initialized)            
c-----------------------------------------------------------------------        
      if(iter .eq. 1)go to 10                                                   
      time = time + dtt / 3600.                                                 
      if ( time .ge. 23.99 ) time = 0.0                                         
      day = day +  dtt / 86400.                                                 
c                                                                               
   10 continue                                                                  
c                                                                               
      if ( day .gt. dayspy ) year = year + 1.                                   
      if ( day .gt. dayspy ) day = day - dayspy                                 
c                                                                               
c-----------------------------------------------------------------------        
c    solar declination calculation                                              
c-----------------------------------------------------------------------        
c                                                                               
      decmax = pie * ( 23.5 / 180.)                                             
      sols   = ( 4141./24. ) + amod( year+3., 4. ) * 0.25                       
c                                                                               
      season = ( day - sols ) / 365.2                                           
      dec    = decmax * cos ( 2. * pie * season )                               
c                                                                               
      rfd  = pie / 180.                                                         
      sind = sin( dec )                                                         
      cosd = cos( dec )                                                         
      hac  = -tan( zlat * rfd )*tan( dec )                                      
      hac  = amin1(hac,1.0)                                                     
      hac  = amax1(hac,-1.0)                                                    
c                                                                               
c-----------------------------------------------------------------------        
c     h is the half-day length (in radians)                                     
c-----------------------------------------------------------------------        
c                                                                               
      h   = acos(hac)                                                           
      dawn= -h                                                                  
      dusk= +h                                                                  
      sr  = 12.-(h/(15.*rfd))                                                   
      ss  = 12.+(h/(15.*rfd))                                                   
      coshr = cos( - pie + (time + 0.5*dtt/3600.) / 24. * 2. * pie )            
      sunang = sin( zlat*rfd ) * sind + cos ( zlat*rfd ) * cosd * coshr         
      sunang = amax1( 0.01, sunang )                                            
c                                                                               
      return                                                                    
      end                                                                       
c======================================================================         
c                                                                               
      subroutine rasite                                                         
c                                                                               
c======================================================================         
c                                                                               
c     calculation of ustar, u2, ra and drag using Paulson's method.             
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     (1) site parameters derived from momopt program suite.                    
c                                                                               
c     (2) routine is not suitable for gcm applications; designed for            
c         use with forcing variables measured at a field site.                  
c                                                                               
c     (3) paulson psi-coefficients are constrained under unsatble               
c         conditions to prevent unrealistically low ra values.                  
c                                                                               
c     (4) wind speed (um) must be greater than or equal to 0.1 m/s              
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c     variables that must enter through comsibc                                 
c                                                                               
c      tm     : air temperature at zmet                                         
c      um     : wind speed at zwind, um .ge. 0.1                                
c      ht     : sensible heat flux from surface                                 
c                                                                               
c     parameters that must enter through comsibc                                
c                                                                               
c      z2     : height of canopy top                                            
c      z0     : roughness length                                                
c      xdx      : zero plane displacement                                         
c      vkc    : von karmans constant = 0.41                                     
c      rhoair : air density                                                     
c      cpair  : air specific heat                                               
c                                                                               
c     other parameters                                                          
c     ----------------                                                          
c                                                                               
c      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet                          
c                                                                               
c      g1     : ratio of km(actual) to km(log-linear) at z = z2                 
c      g2     : ratio of ra(actual) to ra(log-linear) for momentum              
c               between: z = z2 and z = zx, where zx = min(zl,zwind)            
c      g3     : ratio of ra(actual) to ra(log-linear) for heat                  
c               between: z = z2 and z = zx, where zx = min(zl,zmet)             
c      ztz0   : parameter to determine depth of transition layer above          
c               canopy, zl. zl = z2 + ztz0 * z0                                 
c      corb1  : non-neutral correction for calculation of aerodynamic           
c               resistance between ha and z2. when multiplied by                
c               h*rbb/tm gives bulk estimate of local richardson number.        
c               rbb = ra for heat between ha and z2.                            
c               corb2 = 9*g/( rhoair*cpair* (du/dz)**2 )                        
c      corb2  : neutral value of rbb*u2 ( squared ), equivalent to              
c               rdc**2 for upper canopy                                         
c      ha     : canopy source height for heat                                   
c      zwind  : reference height for wind measurement                           
c      zmet   : reference height for temperature, humidity measurement          
c                                                                               
c        the above are generated from sibx + momopt output                      
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     variables returned from this routine via comsibc                          
c                                                                               
c      ustar  : friction velocity                                               
c      u2     : wind speed at canopy top                                        
c      ra     : aerodynamic resistance for heat flux between ha and zmet        
c      drag   : shear stress at canopy top                                      
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     references                                                                
c     ----------                                                                
c                                                                               
c         Paulson C.A. (1970) ' Mathematical representation of wind             
c         and temperature profiles in the unstable atmospheric surface          
c         layer', J. Appl. Met., 9, 129-861.                                    
c                                                                               
c         SE-89                                                                 
c-----------------------------------------------------------------------        
       include 'comsibc.h'                                                      
c                                                                               
      hress = ht                                                                
      zl    = z2 + ztz0 * z0                                                    
      uest  = vkc*um / alog((zwind-xdx)/z0)                                       
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     calculation of u2 assuming neutral conditions                             
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      if ( zwind .gt. zl ) go to 100                                            
      top = 0.                                                                  
      zx1 = zwind - xdx                                                           
      zx2 = z2 - xdx                                                              
      go to 200                                                                 
100   zx1 = zwind - xdx                                                           
      zx2 = zl - xdx                                                              
      top = alog( zx1 / zx2 )                                                   
      zx1 = zl - xdx                                                              
      zx2 = z2 - xdx                                                              
200   bot = alog( zx1 / zx2 )                                                   
      ram = 1. / ( vkc * uest ) * ( top + g2 * bot )                            
      u2 = um - ram * uest**2                                                   
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     calculation of ra for heat follows : non-neutrality assumed               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       zx1 = zwind - xdx                                                          
       zx2 = 0.                                                                 
       arg1 = alog ( zx1 / z0 )                                                 
c                                                                               
c-----------------------------------------------------------------------        
c         initialize newton-raphson iterative routine                    
c-----------------------------------------------------------------------        
c
                    nox = 0                                                     
                 nonpos = 1                                                     
                  iwalk = 0                                                     
                     lx = 1                                                     
                   finc = 0.2                                                   
c                                                                               
       if( ht .le. 0. ) go to 300                                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c     unstable case : calculation of ustar followed by ra                       
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
1000   continue                                                                 
c                                                                               
       call unstab ( uest, zx1, zx2, arg1, ht, ps1, ps2)                        
c                                                                               
       y = um - uest/vkc * ( arg1 - ps1 )                                       
c                                                                               
       call newton ( uest, y, finc, nox, nonpos, iwalk, lx )                    
       if( nox .eq. 0 ) go to 1000                                              
c                                                                               
       if( nox .eq. 2 ) write(6,900)                                            
900    format( /,' convergence failure in rasite - unstable case' )             
c                                                                               
       call rafcal ( zl, uest, ht, raf )                                        
c                                                                               
       go to 500                                                                
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c      stable case : calculation of ustar                                       
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
300    continue                                                                 
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c      interpolation zone is defined: this spans negative sensible              
c      heat fluxes from positive side ( hence factor 0.95 ) of the              
c      following conditions:                                                    
c              y = 0,    dy/du* = 0.                                            
c      see notes for details                                                    
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
       gfac = alog((zwind - xdx)/z0)                                              
       hm1  = -0.95*tm*rhoair*cpair/(2.0*4.7*gx*(zwind-xdx))*                      
     &          (2.0*um/3.0)**3*(vkc/gfac)**2                                   
       hm2  = 5.0*hm1                                                           
       us2  = vkc*um/(gfac+4.7)                                                 
       if( ht .lt. hm2 ) go to 310                                              
       ht = amax1 ( hm1 , ht )                                                  
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
c      ustar calculated for slightly stable conditions : ht .ge. hm1            
c                                                                               
c----------------------------------------------------------------------         
c                                                                               
2000   continue                                                                 
c                                                                               
       call stab ( uest, zx1, zx2, ht, ps1, ps2)                          
c                                                                               
       y = um - uest/vkc * ( arg1 - ps1 )                                       
c                                                                               
       call newton ( uest, y, finc, nox, nonpos, iwalk, lx )                    
       if( nox .eq. 0 ) go to 2000                                              
c                                                                               
       if( nox .eq. 2 ) write(6,910)                                            
910    format( /,' convergence failure in rasite - stable case' )               
c                                                                               
       ht = hress                                                               
c                                                                               
c-----------------------------------------------------------------------        
c      ustar calculation in interpolation zone                                  
c-----------------------------------------------------------------------        
c                                                                               
       if ( ht .gt. hm1 ) go to 400                                             
       us1 = uest                                                               
       uest = ( ht-hm2 ) / ( hm1-hm2 ) * ( us1-us2 ) + us2                      
       go to 400                                                                
c                                                                               
c-----------------------------------------------------------------------        
c      ustar calculation for collapsed profiles                                 
c-----------------------------------------------------------------------        
c                                                                               
310    continue                                                                 
       uest = us2                                                               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c      calculation of ra for heat transfer between z2 and zmet                  
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
400    raf = 1.e5                                                               
c                                                                               
       call rafcal ( zl, us2, hm2, rafmax )                                     
c                                                                               
       if ( ht .lt. hm2 ) go to 410                                             
       hss = amax1 ( hm1, ht )                                                  
       uss = amax1 ( us1, uest )                                                
c                                                                               
       call rafcal ( zl, uss, hss, raf )                                        
c                                                                               
       if ( ht .gt. hm1 ) go to 410                                             
       raf1 = raf                                                               
       raf  = ( ht-hm2 ) / ( hm1-hm2 ) * ( raf1 - rafmax ) + rafmax             
c                                                                               
410    raf = amin1 ( raf , rafmax )                                             
c                                                                               
c-----------------------------------------------------------------------        
c     above canopy variables calculated.                                        
c-----------------------------------------------------------------------        
c                                                                               
500   hrb = ( ht + sqrt(ht**2) ) / 2.0 + 0.1                                    
c                                                                               
c-----------------------------------------------------------------------        
c     corb1 and corb2 are calculated for between ha and z2 only.                
c-----------------------------------------------------------------------        
c                                                                               
                          rbbest = sqrt(corb2)/u2                               
c                                                                               
c-----------------------------------------------------------------------        
c           initialize newton-raphson iterative routine                    
c-----------------------------------------------------------------------        
c                                                                               
                    nox = 0                                                     
                 nonpos = 1                                                     
                  iwalk = 0                                                     
                     lx = 1                                                     
                   finc = 0.2                                                   
c                                                                               
3000   continue                                                                 
c                                                                               
       coef3 = corb1 * hrb / tm / ( z2-ha )                                     
c                                                                               
       y = coef3 * rbbest**3 + ( u2*rbbest )**2 - corb2                         
c                                                                               
       call newton( rbbest , y, finc , nox, nonpos, iwalk, lx)                  
       if( nox .ne. 1 ) go to 3000                                              
c                                                                               
       ra  = raf + rbbest                                                       
c                                                                               
       ustar = uest                                                             
       drag = rhoair * uest*uest                                                
c                                                                               
       return                                                                   
       end                                                                      
c                                                                               
c=======================================================================        
c                                                                               
       subroutine unstab ( uest, a, b, argz, heat, psione , psitwo )            
c                                                                               
c=======================================================================        
c                                                                               
c      calculation of Paulson psi-function for unstable condition               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
       dimension x(2)                                                           
c                                                                               
       zin = a                                                                  
c                                                                               
       do 1000 i=1,2                                                            
       zml = -uest**3 * rhoair * cpair * tm                                     
       zml = zml / ( vkc*gx*heat )                                               
       fac = 16.0 * zin/zml                                                     
       x(i) = ( 1. - fac )**0.25                                                
       zin = b                                                                  
1000   continue                                                                 
c                                                                               
       psione = 2.*alog((1.+x(1))/(1.+x(2)))+alog((1.+x(1)**2)/                 
     &         (1.+x(2)**2))-2.*atan(x(1))+2.*atan(x(2))                       
       psione = amin1 ( argz * 0.75, psione )                                   
c                                                                               
       psitwo = 2.*alog((1.+x(1)**2)/(1.+x(2)**2))                              
       psitwo = amin1 ( argz * 0.75, psitwo )                                   
c                                                                               
       return                                                                   
       end                                                                      
c                                                                               
c=======================================================================        
c                                                                               
       subroutine stab ( uest, a, b, heat, psione , psitwo )              
c                                                                               
c=======================================================================        
c                                                                               
c      calculation of Paulson psi-function for stable condition                 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
       psione = 0.                                                              
       psitwo = 0.                                                              
       if ( abs(heat) .le. 1.e-4 ) go to 100                                    
c                                                                               
       zml = -uest**3. * rhoair * cpair * tm                                     
       zml = zml / ( vkc*gx*heat )                                               
c                                                                               
       psione = -4.7 * ( a-b ) / zml                                            
       psione = amax1( -4.7, psione )                                           
c                                                                               
       psitwo = psione                                                          
c                                                                               
100    continue                                                                 
c                                                                               
       return                                                                   
       end                                                                      
c                                                                               
c=======================================================================        
c                                                                               
       subroutine rafcal ( zl, uest, heat, raf )                                
c                                                                               
c=======================================================================        
c                                                                               
c      calculation of ra for heat between z2 and zmet                           
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
      if ( zmet . gt. zl ) go to 100                                            
c                                                                               
      top = 0.                                                                  
      zx1 = zmet - xdx                                                            
      zx2 = z2 - xdx                                                              
      go to 200                                                                 
c                                                                               
100   zx1 = zmet - xdx                                                            
      zx2 = zl - xdx                                                              
      arg = alog( zx1 / zx2 )                                                   
      if ( heat .gt. 0. )                                                       
     &      call unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
      if ( heat .le. 0. )                                                       
     &      call   stab ( uest, zx1, zx2,      heat, ps1, ps2)                  
      top = arg - ps2                                                           
c                                                                               
      zx1 = zl - xdx                                                              
      zx2 = z2 - xdx                                                              
c                                                                               
200   arg = alog ( zx1 / zx2 )                                                  
      if ( heat .gt. 0. )                                                       
     &      call unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
      if ( heat .le. 0. )                                                       
     &      call   stab ( uest, zx1, zx2,      heat, ps1, ps2)                  
      bot = arg - ps2                                                           
c                                                                               
      raf = 1. / ( vkc*uest ) * ( top + g3 * bot )                              
c                                                                               
      return                                                                    
      end                                                                       
c                                                                               
c=======================================================================        
c                                                                               
       subroutine newton(a1,y,finc,nox,nonpos,iwolk,l)                          
c                                                                               
c=======================================================================        
c                                                                               
c      the newton raphson iterative routine will be used to generate new        
c      values of a1 if dabsolute value of y is greater than ertol;              
c      a1 is estimate, y is resultant error                                     
c      nex is exit condition  (0=no exit) or (1 when dabs(y) lt ertol)          
c      ertol is the dabsolute value of y necessary to obtain an exit            
c      finc is initial increment size for second estimate of a1                 
c      nonpos=0 if quantity to be minimized can be less than zero;              
c      nonpos=1 if quantity can only be positive                                
c      l identifies which quantity is being calculated.                         
c                                                                               
c      control values: finc,ertol,nox,nonpos,l:must be set by user              
c-----------------------------------------------------------------------        
c                                                                               
       dimension iter(3), iwalk(3), nex(3)                                      
       dimension zinc(3), a2(3), y1(3)                                          
       save iter, a2, y1
       data cons/1.0/
       data iter /0,0,0/, a2 /0.,0.,0./, y1 /0.,0.,0./
c                                                                               
       ertol = 0.05 * finc                                                      
       iwalk(l) = iwolk                                                         
       nex(l)=nox                                                               
c                                                                               
       if ( iter(l) .ge. 490 ) go to 160                                        
       if (ertol .lt. 0.000001) ertol=0.000001                                  
       if (abs(y) .le. ertol) go to 150                                         
       if((abs(y-y1(l))).le.0.01*ertol .and. iwalk(l).eq.0 ) go to 8            
c                                                                               
       if(abs(y1(l)).gt.ertol) go to 1                                          
       a2(l)=a1                                                                 
c**    a1=a1-y                                                                  
       step = amin1( abs(y), abs(10.*finc) ) * sign(cons,y)                     
       a1=a1-step                                                               
       nex(l)=0                                                                 
       y1(l)=y                                                                  
       iter(l)=1                                                                
       if (iwalk(l) .eq. 3) go to 101                                           
       iwalk(l)=0                                                               
       go to 101                                                                
   1   iter(l)=iter(l)+1                                                        
       if(iter(l) .eq. 20) iwalk(l)=1                                           
       if(iwalk(l) .ne. 0) go to 2                                              
       if(abs(y) .gt. ertol) go to 3                                            
       nex(l)=1                                                                 
       go to 150                                                                
   3   a=a1-y*(a1-a2(l))/(y-y1(l))                                              
       if(abs(a-a1).gt.(10.0*finc))                                             
     &            a=a1+10.0*finc*sign(cons,(a-a1))                              
       a2(l)=a1                                                                 
       a1=a                                                                     
       y1(l)=y                                                                  
       go to 101                                                                
   2   if(iwalk(l).eq.2)go to 4                                                 
       if(iwalk(l).eq.3) go to 6                                                
       if(sign(cons,y).eq.sign(cons,y1(l))) go to  3                            
       zinc(l)=(a1-a2(l))/4.0                                                   
       a1=a2(l)+zinc(l)                                                         
       iwalk(l)=2                                                               
       nex(l)=0                                                                 
       go to 101                                                                
   4   if(sign(cons,y) .eq.sign(cons,y1(l))) go to 5                            
       zinc(l)=-zinc(l)/4.0                                                     
       a2(l)=a1                                                                 
       a1=a1+zinc(l)                                                            
       nex(l)=0                                                                 
       y1(l)=y                                                                  
       go to 101                                                                
   5   a2(l)=a1                                                                 
       a1=a1+zinc(l)                                                            
       y1(l)=y                                                                  
       nex(l)=0                                                                 
       go to 101                                                                
   6   if(sign(cons,y).eq.sign(cons,y1(l))) go to 7                             
       iwalk(l)=1                                                               
       go to 2                                                                  
   7   a2(l) = a1                                                               
       a1 = a1+finc                                                             
       y1(l)=y                                                                  
       nex(l) = 0                                                               
       go to 101                                                                
   8   a1 = a1 + finc*2.0                                                       
       nex(l)=0                                                                 
       go to 101                                                                
160    continue                                                                 
       write(6,900) y, l                                                        
 900   format ( 3x,' failure to converge after 490 iterations',                 
     & /, 3x,' y = ',g12.5, ' lx =',i2 )                                        
c                                                                               
 150   nex(l) = 1                                                               
       if( iter(l) .ge. 490 ) nex(l) = 2                                        
       zinc(l)=0.0                                                              
       iter(l) = 0                                                              
       iwalk(l)=0                                                               
       y1(l)=0.0                                                                
       y=0.0                                                                    
       a2(l)=0.0                                                                
 101   continue                                                                 
       if(nonpos.eq.1.and.a1.lt.0.0) a1=a2(l)/2.0                               
       nox = nex(l)                                                             
       iwolk = iwalk(l)                                                         
c                                                                               
       return                                                                   
       end                                                                      
c                                                                               
c=======================================================================        
c                                                                               
      subroutine gauss ( a, n, np1, x, work )                                   
c                                                                               
c=======================================================================        
c                                                                               
c     solve a linear system by gaussian elimination.  developed by              
c     dr. chin-hoh moeng.  a is the matrix of coefficients, with the            
c     vector of constants appended as an extra column.  x is the vector         
c     containing the results.  the input matrix is not destroyed.               
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
       include 'comsibc.h'                                                      
c                                                                               
      dimension a(4,5),work(4,5),x(4)                                           
c                                                                               
      do 1000 i=1,n                                                             
      do 1000 j=1,np1                                                           
 1000 work(i,j)=a(i,j)                                                          
c                                                                               
      do 20 i=2,n                                                               
      do 20 j=i,n                                                               
c                                                                               
      r=work(j,i-1)/work(i-1,i-1)                                               
c                                                                               
      do 20 k=1,np1                                                             
   20 work(j,k)=work(j,k)-r*work(i-1,k)                                         
c                                                                               
      do 30 i=2,n                                                               
      k=n-i+2                                                                   
      r=work(k,np1)/work(k,k)                                                   
c                                                                               
      do 30 j=i,n                                                               
      l=n-j+1                                                                   
   30 work(l,np1)=work(l,np1)-r*work(l,k)                                       
c                                                                               
      do 40 i=1,n                                                               
   40 x(i)=work(i,np1)/work(i,i)                                                
c                                                                               
      return                                                                    
      end                                                                       
                                                                                
