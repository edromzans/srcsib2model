c=======================================================================
c
c                  SUBROUTINES
c                    
c=======================================================================        
c                   
      SUBROUTINE driver( iu, icho2, ichmet, isnow, nymd )                        
c                   
                     
c-----------------------------------------------------------------------        
c     subroutines called : radc2
c
c-----------------------------------------------------------------------        
      INCLUDE 'COMSIBC.H'
c-----------------------------------------------------------------------        
c                   
      e(x) = exp( 21.18123 - 5418. / x ) / .622   
c                   
c-----------------------------------------------------------------------        
c                   
c     isnow = 0 : conventional run with received met data.                      
c                   
c     isnow = 1 : shock test with initial snow dump and freezing                
c                 temperatures at beginning of run warming to                   
c                 normal over  5-day period.      
c                   
c-----------------------------------------------------------------------        
c                   
      IF( isnow .ne. 0) THEN                 
          IF ( iter .le. 1) THEN                 
               tc = 270.     
               tg = 270.     
               snoww(2) = 0.1
          END IF 
          cold = amax1 ( 0., (120. - (1.*iter)) / 120. )
          rhair = em/e(tm)   
          tm = tm * ( 1. - cold ) + (tm - 30.) * cold 
          em = e(tm)*rhair   
          IF(em .lt. 0.)em = 0.1                      
      END IF 
c                   
c
c      IF(nymd .le. 85082523)THEN            
c                   
         um = amax1(um,0.25)
         ustarm = mustar/100.                        
         swdown = amax1(swdown,0.1)                  
         ihour = mod(nymd,100)                       
         ptot = ptot + tprec
         ppl = tprec   
         ppc = tprec-ppl    
c                   
         CALL radc2_2   
c                   
         cloud = (1160.*sunang - swdown) / (963. * sunang)
         cloud = amax1(cloud,0.)                     
         cloud = amin1(cloud,1.)                     
         cloud = amax1(0.58,cloud)                   
c                   
         difrat = 0.0604 / ( sunang-0.0223 ) + 0.0683
         IF ( difrat .lt. 0. ) difrat = 0.           
         IF ( difrat .gt. 1. ) difrat = 1.           
c                   
         difrat = difrat + ( 1. - difrat ) * cloud   
         vnrat = ( 580. - cloud*464. ) / ( ( 580. - cloud*499. )                   
     &         + ( 580. - cloud*464. ) )           
c                   
         radn(1,1) = (1.-difrat)*vnrat*swdown        
         radn(1,2) = difrat*vnrat*swdown             
         radn(2,1) = (1.-difrat)*(1.-vnrat)*swdown   
         radn(2,2) = difrat*(1.-vnrat)*swdown        
         radn(3,2) = zlwd  
         RETURN
c      END IF                   
1000  CONTINUE
      WRITE(icho2, 900)iu, nymd, iout              
900   FORMAT(5x,'eof encountered for unit= ',i2,' eof date= ',i8,1x,i2)        
      STOP 'in driver'          
       
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE balan ( iplace, icho2, nymd )     
c                   
c=======================================================================        
c                   
c     energy and water balance check.             
c                   
c-----------------------------------------------------------------------        
      INCLUDE 'COMSIBC.H'
c     
      IF( iplace .eq. 1 ) THEN
c                   
        etmass = 0.   
        roff   = 0. 
	  roff1	 = 0. 
	  roff2	 = 0. 
	  roff3	 = 0. 
	  roff4	 = 0. 
	  gwsoil = 0.
c                   
        totwb = www(1) * poros * zdepth(1)          
     &        + www(2) * poros * zdepth(2)          
     &        + www(3) * poros * zdepth(3)          
     &        + capac(1) + capac(2) + snoww(1) + snoww(2)   

c                   
      ELSE
c                   
        endwb = www(1) * poros * zdepth(1)          
     &        + www(2) * poros * zdepth(2)          
     &        + www(3) * poros * zdepth(3)          
     &        + capac(1) + capac(2) + snoww(1) + snoww(2) 
c 2005/9/28 changed by tangqh@iis.u-tokyo.ac.jp    
c old    &        - (ppl+ppc)/1000. + etmass/1000. + roff
     &        - (ppl+ppc)/1000. + etmass/1000. + roff -gwsoil   
c                   
c        errorw= totwb - endwb                       
        errorw= totwb - endwb                       
        pmeter= (ppl+ppc)/1000.                     
        emeter= etmass/1000.                       
c                   
        IF(abs(errorw) .gt. 0.0001) then
	WRITE(icho2,*) iter,item01,item02
	WRITE(icho2,*) swdown,zlwd,em,tm,um
	WRITE(icho2,*) tprec,istype,ivtype
      WRITE(icho2,900) nymd, totwb, endwb, errorw, 
     &                  www(1), www(2), www(3),    
     &                  capac(1), capac(2), snoww(1), snoww(2),             
     &                  pmeter, emeter, roff, gwsoil 
900   FORMAT(/,10x,'** warning: water balance violation **  ',/,              
     &         /,1x,'date ', i8,  
     &         /,1x,'begin, end, diff ', 3(f10.7,1x),      
     &         /,1x,'www,1-3          ', 3(f10.8,1x),      
     &         /,1x,'capac,1-2        ', 2(f10.8,1x),      
     &         /,1x,'snoww,1-2        ', 2(f10.8,1x),      
     &         /,1x,'p, et, roff,gwsoil', 4(f10.8,1x) )  
        ENDIF   
c                   
c                   
        cbal = radt(1) - chf - (ect+hc+eci)/dtt     
        gbal = radt(2) - shf - (egs+hg+egi)/dtt - heaten/dtt                      
        zlhs = radt(1) + radt(2) - chf - shf        
        zrhs = hflux + (ect + eci + egi + egs)/dtt + heaten/dtt                   
c             
      
        errore= zlhs - zrhs         
c
        IF(abs(errore) .gt. 1.) then
	WRITE(icho2,*) 'iter,inr,inc:',iter,item01,item02
	WRITE(icho2,*) 'Swdown, Zlwd, Vapor pressure:',swdown,zlwd,em
	WRITE(icho2,*) 'Temperature, wind speed:',tm,um
	WRITE(icho2,*) 'Precipitation, SoilType, VType',tprec,istype,ivtype
      WRITE(icho2,910) nymd, zlhs, zrhs, radt(1), radt(2), chf, shf,            
     &        hflux, ect, eci, egi, egs, hc, hg, heaten, cbal, gbal                    
910   FORMAT(/,10x,'** warning: energy balance violation **',/,               
     &         /,1x,'date ', i8, 
     &         /,1x,'rhs, lhs              ', 2g12.5,     
     &         /,1x,'rn1, rn2, chf, shf, h ', 5g12.5,     
     &         /,1x,'ect, eci, egi, egs    ', 4g12.5,     
     &         /,1x,'hc        hg          ',  g12.5, 12x, g12.5,                       
     &         /,1x,'heaten, c-bal, g-bal  ', 3g12.5 )  
	  ENDIF 
       
	END IF

      RETURN        
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE outer ( iout, iout1, iout2, iout3, iout4, nymd )               
c                   
c=======================================================================        
c                   
c     output of results to files.                  
c                   
c-----------------------------------------------------------------------        
      INCLUDE 'COMSIBC.H'
c                   
      trant  = ect/hlat  
      canil  = eci/hlat  
      evapg  = (egs+egi)/hlat                     
      radswd = radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2)                    
      radswa = (1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2)              
     &       + (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2)              
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
c                   
c      WRITE(iout,900) nymd, www(1), www(2), www(3), ppl
c900   FORMAT( 1x, i8, 1x, 3f7.4, 1x, f8.2)        
c                   
c      WRITE(iout1,910) nymd, roff, trant, canil, evapg, etmass, ppl             
c910   FORMAT( 1x, i8, 1x, f12.7, 4f10.4, f8.2)    
c                   
c      WRITE(iout2,920) nymd, tc, tg, td, tm, tgeff, capac(1), capac(2)          
c     & ,snoww(1), snoww(2)                        
c920   FORMAT( 1x, i8, 1x, 5f7.2, 4f7.5)           
c                   
c      WRITE(iout3,930) nymd, radn(3,2), zlwup, radswd, radswa, radtot,          
c     & elat, hflux, gcstor                        
c930   FORMAT(1x,i8,1x,8(f5.0,1x) )                
c                   
c      WRITE(iout4,940) nymd, rst,(rstfac(i),i=1,4),gsh2o,assimn                 
c940   FORMAT(1x,i8,1x, f10.1, 1x, 4( f8.6,1x), 1x, f8.4,1x,e10.3 )
c                   
      RETURN        
      END           
                    
	SUBROUTINE vegpar
	INCLUDE 'COMSIBC.H'
	include 'SiB2par.inc'	

	if (ivtype.ge.0) then
		z2		=	z2_v(ivtype)
		z1		=	z1_v(ivtype)
		vcover	=	vcover_v(ivtype)
		chil	=	chil_v(ivtype)

		rootd	=	rootd_v(ivtype)
		phc		=	phc_v(ivtype)

		tran(1,1)	=	tranlv_v(ivtype)
		tran(2,1)	=	tranln_v(ivtype)
		tran(1,2)	=	trandv_v(ivtype)
		tran(2,2)	=	trandn_v(ivtype)

		ref(1,1)	=	reflv_v(ivtype)
		ref(2,1)	=	refln_v(ivtype)
		ref(1,2)	=	refdv_v(ivtype)
		ref(2,2)	=	refdn_v(ivtype)

		effcon	=	effcon_v(ivtype)
		gradm	=	gradm_v(ivtype)
		binter	=	binter_v(ivtype)
		respcp	=	respcp_v(ivtype)
		atheta	=	atheta_v(ivtype)
		btheta	=	btheta_cst		! Constant

		trda	=	trda_cst		! Constant
		trdm	=	trdm_cst		! Constant
		trop	=	trop_cst		! Constant
		slti	=	slti_cst		! Constant
		hlti	=	hlti_v(ivtype)
		shti	=	shti_cst		! Constant
		hhti	=	hhti_v(ivtype)

		vmax0   =   vmax0_v(ivtype)

	else !RUN the sample (ivtype<0)
		z2		=	35.0
		z1		=	1.0
		vcover	=	0.98
		chil	=	0.1

		rootd	=	0.5
		phc		=	-200.0

		tran(1,1)	=	0.0500
		tran(2,1)	=	0.2500
		tran(1,2)	=	0.0010
		tran(2,2)	=	0.0010

		ref(1,1)	=	0.1000
		ref(2,1)	=	0.4500
		ref(1,2)	=	0.1600
		ref(2,2)	=	0.3900

		effcon	=	0.08
		gradm	=	9.00
		binter	=	0.01
		respcp	=	0.015
		atheta	=	0.95
		btheta	=	0.95		! Constant

		trda	=	1.30		! Constant
		trdm	=	328.16		! Constant
		trop	=	298.16		! Constant
		slti	=	0.2			! Constant
		hlti	=	288.16
		shti	=	0.3			! Constant
		hhti	=	313.16

		vmax0   =   0.00006
	endif

	if (ivtype.ge.0) then
		sodep	=	sodep_v(ivtype)
		soref(1)=	sorefv_v(ivtype)
		soref(2)=	sorefn_v(ivtype)
	else
		sodep	=	2.0
		soref(1)=	0.10
		soref(2)=	0.20
	endif
	END    
	
c	SUBROUTINE soipar(soilpara)
c	INCLUDE 'COMSIBC.H'
c	include 'SiB2par.inc'
c	real soilpara(7000,6)
c
c	if (istype.gt.0) then
c		bee		=	soilpara(istype,4)
c		phsat	=	soilpara(istype,2)
c		satco	=	soilpara(istype,3)
c		poros	=	soilpara(istype,1)
c	else
c		bee		=	7.797
c		phsat	=	-0.2
c		satco	=	3.5E-06
c		poros	=	0.458
c	endif	
c
c	END	                   

	SUBROUTINE varcal(varcal_para)
	INCLUDE 'COMSIBC.H'
	include 'SiB2par.inc'
	real varcal_para(9,585)		
		
	if (ivtype.ge.0) then
c	CALL derive_trans
c     $	(ivtype,zlt,rhoair,
c     $	ha, z0d, dd,g2, g3, cc1, cc2, corb1, corb2)

c	Get parameters for SIB2 from saved array
		iii = ANInt(zlt*10.0)
		iii = min(65,iii)
		iii = max(1,iii)
		ha		=	varcal_para(ivtype,(iii-1)*9+1)
		z0d		=	varcal_para(ivtype,(iii-1)*9+2)
		dd		=	varcal_para(ivtype,(iii-1)*9+3)
		g2		=	varcal_para(ivtype,(iii-1)*9+4)
		g3		=	varcal_para(ivtype,(iii-1)*9+5)
		cc1		=	varcal_para(ivtype,(iii-1)*9+6)
		cc2		=	varcal_para(ivtype,(iii-1)*9+7)
		corb1	=	varcal_para(ivtype,(iii-1)*9+8)
		corb2	=	varcal_para(ivtype,(iii-1)*9+9)
		g1		=	g1_cst
		ztz0	=	g4_cst
		if ((z2+10.*dd+0.1).lt.2.0) then
			zwind =	2.0			! (data from Chinese Weather Bureau)
			zmet  = 2.0			! (usually it is 2.00 m)
		else
			zwind =	z2+10.*dd+0.1	! (data from Chinese Weather Bureau)
			zmet  = z2+10.*dd+0.1	! (usually it is 2.00 m)
		endif

	else
		zwind =	45.00	! (data from Chinese Weather Bureau)
		zmet  = 45.00	! (usually it is 2.00 m)
	   z0d=2.02        
	   dd=28.81        
	   cc1=5.59        
	   cc2=1177.14     
	   corb1=0.111     
	   corb2=19.112    
	   ha=24.81        
	   g1=1.449        
	   g2=0.801        
	   g3=0.801        
	   ztz0=11.785     
	endif

		rootd = amin1( rootd, sodep*0.75 )          
		zdepth(1) = 0.02   
		zdepth(2) = rootd - 0.02                    
		zdepth(3) = sodep - zdepth(1) - zdepth(2)   
c If green and fparc are known, to get zlt(Leaf Area Index) (Original SiB2)                  
		scatp    =     green   * ( tran(1,1) + ref(1,1) )
     &			+( 1.-green ) * ( tran(1,2) + ref(1,2) )
		park = sqrt(1.-scatp) * gmudmu              
cc     fparc = 1. - exp ( -park*zlt )              
c		if ((1.-fparc).le.0.0) then
c			print *,"SIB2sub(359):fprac larger than 1.0:",(1.-fparc)
c		endif
c		zlt = -1./park*alog( 1.-fparc )

c	If the zlt(Leaf Area Index) and fparc are known, to get green
c	To do so, Please remove code before: zlt = -1./park*alog(1.-fparc)
c		scatp	= 1. - (alog(1.-fparc)/zlt/gmudmu)**2
c		tranref1= tran(1,1) + ref(1,1)
c		tranref2= tran(1,2) + ref(1,2)
c		green	= (scatp - tranref2) / ( tranref1 - tranref2 )
c		green	= min(0.95, green)
c		green	= max(0.05, green)
      END              
c=======================================================================        
c                   
      SUBROUTINE const2  
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
      INCLUDE 'COMSIBC.H'
c                   
      asnow    = 13.2    
      bps      = 1. 
      clai     = 4.2 * 1000. * 0.2                
      cpair    = 1010.   
      cw       = 4.2 * 1000. * 1000.              
      epsfac   = 0.622   
      g        = 9.81    
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
      RETURN        
      END           
                    
c=======================================================================        
c                   
      SUBROUTINE inter2  
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
c   patchs          
c   snow1           
c
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                   
c       roff           runoff (mm)                
c       tc             canopy temperature (K)     
c       tg             ground surface temperature (K)  
c       www(1)         ground wetness of surface layer 
c       capac(2)       canopy/ground liquid interception store (m)              
c       snoww(2)       canopy/ground snow interception store (m)                
c                   
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                   
      INCLUDE 'COMSIBC.H'
c                   
      real  pcoefs(2,2)                      
c      data pcoefs(1,1)/ 20. /, pcoefs(1,2)/ .206e-8 /, 
c     &     pcoefs(2,1)/ 0.0001 /, pcoefs(2,2)/ 0.9999 /, bp /20. /              
      data pcoefs(1,1)/ 25. /, pcoefs(1,2)/ 1.38879E-11 /, 
     &     pcoefs(2,1)/ 25. /, pcoefs(2,2)/ 1.38879E-11 /, bp /25. /              

c                   
c-----------------------------------------------------------------------        
c                   
      CALL snow1   
c                   
c-----------------------------------------------------------------------        
c                   
c     prec ( pi-x )   : equation (c.3), SA-89B    
c                   
c-----------------------------------------------------------------------        
c                   
	pcoefs(1,1) = app	
	pcoefs(2,1) = app	
	pcoefs(1,2) = cpp	
	pcoefs(2,2) = cpp	
	bp					=	bpp

      ap = pcoefs(2,1)   
      cp = pcoefs(2,2)   
      totalp = ppc + ppl 
      IF( snoww(1) .gt. 0. .or. snoww(2) .gt. 0. .or. tm .lt. tf )              
     &     ppc = 0. 
      ppl = totalp - ppc 
      IF(totalp.ge.1.e-8) THEN               
         ap = ppc/totalp * pcoefs(1,1) + ppl/totalp * pcoefs(2,1)                  
         cp = ppc/totalp * pcoefs(1,2) + ppl/totalp * pcoefs(2,2)                  
      END IF 
c                   
      roff = 0.  
	roff1	 = 0. 
	roff2	 = 0. 
	roff3	 = 0. 
	roff4	 = 0. 
	gwsoil	 = 0.	    	   
      thru = 0.     
      fpi  = 0.     
	finfil = 0.
c                   
c----------------------------------------------------------------------         
c     heat capacity of the soil, as used in force-restore heat flux             
c     description. dependence of csoil on porosity and wetness is               
c     based on CS-81.                       
c----------------------------------------------------------------------         
c                   
      slamda = ( 1.5*(1.-poros) + 1.3*www(1)*poros ) / 
     &         ( 0.75 + 0.65*poros - 0.4*www(1)*poros ) * 0.4186                
      shcap  = ( 0.5*(1.-poros) + www(1)*poros ) * 4.186 * 1.e6     
      csoil  = sqrt( slamda * shcap * 86400./pie ) / 2.
c                   
c----------------------------------------------------------------------         
c     input precipitation is given in mm, converted to m to give p0.            
c----------------------------------------------------------------------         
c                   
      p0 = totalp * 0.001
c                   
      DO iveg = 1, 2
c                   
      realc = 2. - iveg  
      realg = iveg - 1.
c                   
      xsc = amax1(0., capac(iveg) - satcap(iveg) )
      capac(iveg) = capac(iveg) - xsc
      xss = amax1(0., snoww(iveg) - satcap(iveg) ) * realc
      snoww(iveg) = snoww(iveg) - xss 
      if (iveg.eq.1) then  !Because freezing, snoww(1)>satcap, put to snoww(2)
         snoww(2) =snoww(2) + xss
         xss = 0.0
      endif
	p0 = p0 + xsc + xss			!tang@2005/12/21
c                   
      capacp = capac(iveg)                        
      snowwp = snoww(iveg)                        
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
      IF ( abs(chiv) .le. 0.01 ) chiv = 0.01      
      aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv
      bb = 0.877 * ( 1. - 2. * aa )               
      exrain = aa + bb   
c                   
      zload = capac(iveg) + snoww(iveg)       
      fpi = ( 1.-exp( - exrain*zlt/vcover ) )*vcover*realc + realg              
      tti = p0 * ( 1.-fpi ) 
      xs = 1.       
      IF ( p0 .ge. 1.e-9 ) THEN              
         arg =  ( satcap(iveg)-zload )/( p0*fpi*ap ) -cp/ap                        
         IF ( arg .ge. 1.e-9 ) THEN     
            xs = -1./bp * alog( arg )                   
            xs = amin1( xs, 1. )                        
            xs = amax1( xs, 0. )                        
         END IF
      END IF
	Pxi = amax1(0.0,satcap(iveg) - zload)
      tex = p0*fpi * ( ap/bp*( 1.- exp( -bp*xs )) + cp*xs ) -                   
     &      ( Pxi ) * xs                                       
      tex = amax1( tex, 0. )  
c                   
c----------------------------------------------------------------------         
c     total throughfall (thru) and store augmentation   
c----------------------------------------------------------------------         
c                   
      IF ( iveg .eq. 1 ) THEN                
c                   
         thru = tti + tex  
         pinf = p0 - thru   
         IF( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf
         IF( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf
c                   
         CALL adjust ( tc, spechc, capacp, snowwp, iveg ) 
c                   
         p0 = thru  
c                   
      ELSE IF ( tg .gt. tf .and. snoww(2) .gt. 0. ) THEN

          CALL patchs ( p0 )                       

      ELSE

          thru = tti + tex   
          IF ( tg .le. tf .or. tm .le. tf ) thru = 0. 
          pinf = p0 - thru   
          IF( tm .gt. tf ) capac(iveg) = capac(iveg) + pinf
          IF( tm .le. tf ) snoww(iveg) = snoww(iveg) + pinf
          IF( tm .gt. tf ) THEN                  
c            www(1) = www(1) + thru/ ( poros*zdepth(1) )                    
				finfil = finfil + thru
          END IF
c                   
          CALL adjust ( tg, spechc, capacp, snowwp, iveg ) 
c                   
      END IF
      END DO

c                   
c----------------------------------------------------------------------         
c                   
c     instantaneous overland flow contribution ( roff ) 
c     roff( r-i )     : equation (c.13), SA-89B   
c     finfil : through fall and snow melt etc which reach the soil top surface
c-----------------------------------------------------------------------        

	xw1 = amax1(0., www(1) - 1.0 )		
	www(1) = www(1) - xw1							
	finfil = finfil + xw1 * ( poros*zdepth(1) )  
c----------------------------------------------------------------------         
c	if froze, reduce infiltration                   
c-----------------------------------------------------------------------        
      tsnow = amin1 ( tf-0.01, tg )               
      tgs = tsnow*areas + tg*(1.-areas)           
      props = ( tgs-(tf-1.) ) / 1.               
      props = amax1( 0.0, amin1( 1.0, props ) )  

      if (idirr.ne.1) then
      ap = app
      bp = bpp
      cp = cpp
	equdep = satco * dtt * props
	xs = 1.       
	IF( finfil .ge. 1.e-9 ) THEN            
		arg = equdep / ( finfil * ap ) -cp/ap         
		IF( arg .ge. 1.e-9 ) THEN 
		xs = -1./bp * alog( arg )                   
		xs = amin1( xs, 1. )                        
		xs = amax1( xs, 0. )                        
		END IF
	END IF
	roffo = finfil * ( ap/bp * ( 1.-exp( -bp*xs )) + cp*xs )                    
     &	- equdep*xs  
	roffo = amax1 ( roffo, 0. )       
	roff = roff + roffo  
	roff1= roff1+ roffo
	finfil = finfil - roffo
        endif
c	if (finfil.gt.0.) then
c		xw1 = amin1(finfil, (1.0-www(1) )* ( poros*zdepth(1) ) )
c		www(1) = www(1) + xw1	/ ( poros*zdepth(1) )
c		finfil = finfil -xw1
c	endif
	CALL Green_AMPT(finfil)
c	thru_re= finfil
	roff = roff + finfil  
	roff4= roff4+ finfil
	finfil=0.

      RETURN        
      END           

c=======================================================================        
c                   
	SUBROUTINE Green_AMPT(dep)
      INCLUDE 'COMSIBC.H'
	real dep,se,a1,b1
c
c=======================================================================        
c                   
c  Calculate infiltration to soil and instantaneous overland flow (roff2)                       
c                   
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c 
	if (dep.gt.0.0) then
		seli= 1.0
		se	= (www(1)*zdepth(1)+www(2)*zdepth(2)+www(3)*zdepth(3) ) /
     $				(zdepth(1) + zdepth(2) +zdepth(3))
c		se	= www(1)
		se	= amax1(0.03,se)
		se	= amin1(se,1.0)
		ph_a= -phsat*(se**(-bee)-1.)/(zdepth(1)+zdepth(2)+zdepth(3))*2.
		f_i = satco*(1.+ ph_a )
		if (dep.gt.f_i) then	!case 1
			a1	= -phsat * amax1(0.,seli-se) * poros  !*se**(-bee)
			b1	=	dtt*satco
			y0	=	a1 + b1
			call newton_i(a1,b1,y0,y)
		else
			y		=	dep		
			a1	= -phsat * amax1(0.,seli-se) * poros  !*se**(-bee)
			f_i2=	satco*( 1 + a1/y ) 
			if (dep.le.(f_i2*dtt) ) then	!case 3
				y = dep
			else
				dt=	satco*a1/(f_i2-satco)/f_i2
				a1	= -phsat * amax1(0.,seli-se) * poros  !*se**(-bee)
				b1	=	(dtt-dt)*satco
				call newton_i(a1,b1,y0,y)
			endif
		endif

c		a1	= -phsat*amax1(0.,seli-se)*poros !*se**(-bee)
c		b1	=	dtt*satco
c		y0	=	a1 + b1
c		call newton_i(a1,b1,y0,y)
c		write(*,'(7f)') phsat,poros,se,satco,a1,b1,y
		y		=	amax1(0. , y)
		put =	amin1(dep, y)
		totwww = ( www(1)*zdepth(1) + www(2)*zdepth(2) 
     $				+ www(3)*zdepth(3) ) *poros
		www(1) = www(1) + put / ( poros*zdepth(1) )
	  xw		 = amax1(0., www(1) - 1.0 )		
		www(1) = www(1) - xw	
		www(2) = www(2) + xw*zdepth(1) / zdepth(2)
	  xw		 = amax1(0., www(2) - 1.0 )		
		www(2) = www(2) - xw	
		www(3) = www(3) + xw*zdepth(2) / zdepth(3)
	  xw		 = amax1(0., www(3) - 1.0 )		
		www(3) = www(3) - xw	
		dwww = ( www(1)*zdepth(1) + www(2)*zdepth(2) 
     $				+ www(3)*zdepth(3) ) *poros - totwww
		roff	 = roff  + put - dwww
		roff3  = roff3 + put - dwww
		dep		 = dep - put
	endif	

	END

	subroutine newton_i(a1,b1,y0,y)
	real a1,b1,y0,y,cret,err
	integer nn
	cret = 1.0E-10
	nn=0
	err=cret+1.
	y		= y0
	do while(nn.le.100.and.abs(err).gt.cret)
		fyn = a1*log(1.+ y/a1 ) - y + b1
		err = fyn
		fyn1= 1./(1.+ y/a1) -1.
		y		= y - fyn/fyn1
		nn=nn+1
c		print *,nn,fyn,y
	enddo

	end

c=======================================================================        
c                   
      SUBROUTINE adjust ( ts, spechc, capacp, snowwp, iveg )                    
c                   
c=======================================================================        
c                   
c     temperature change due to addition of precipitation                       
c                   
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                   
c     roff           runoff (mm)                
c     tc             canopy temperature (K)     
c     tg             ground surface temperature (K)  
c     www(1)         ground wetness of surface layer 
c     capac(2)       canopy/ground liquid interception store (m)              
c     snoww(2)       canopy/ground snow interception store (m)                
c                   
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        
c                   
      INCLUDE 'COMSIBC.H'                        
c                   
      freeze = 0.   
      diff = ( capac(iveg)+snoww(iveg) - capacp-snowwp )*cw                     
      ccp = spechc  
      cct = spechc + diff
c                   
      tsd = ( ts * ccp + tm * diff ) / cct        
c                   
      IF((ts-tf)*(tm-tf).lt.0 )THEN
c                   
        tta = ts      
        ttb = tm      
        cca = ccp     
        ccb = diff    
        IF( tsd .le. tf ) THEN                
c                   
c----------------------------------------------------------------------         
c    freezing of water on canopy or ground        
c----------------------------------------------------------------------         
c                   
           
          IF( ts .ge. tm ) THEN
              ccc = capacp * snomel                       
          ELSE
              ccc = diff * snomel / cw  
          END IF 

          tsd = ( tta * cca + ttb * ccb + ccc ) / cct 
c                   
          freeze = ( tf * cct - ( tta * cca + ttb * ccb ) )
          freeze = (amin1 ( ccc, freeze )) / snomel   
          IF(tsd .gt. tf)tsd = tf - 0.01              
c                   
        ELSE
c                   
c----------------------------------------------------------------------         
c    melting of snow on canopy or ground, water infiltrates.                    
c----------------------------------------------------------------------         
c                   
          IF ( ts .le. tm )THEN
               ccc = - snoww(iveg) * snomel                
          ELSE 
               ccc = - diff * snomel / cw
          END IF
c                   
          tsd = ( tta * cca + ttb * ccb + ccc ) / cct 
c                   
          freeze = ( tf * cct - ( tta * cca + ttb * ccb ) )
          freeze = (amax1( ccc, freeze )) / snomel    
          IF(tsd .le. tf)tsd = tf - 0.01              

        END IF 
      END IF 
c                   
      snoww(iveg) = snoww(iveg) + freeze          
      capac(iveg) = capac(iveg) - freeze          
c                   
      xs = amax1( 0., ( capac(iveg) - satcap(iveg) ) ) 
      IF( snoww(iveg) .ge. 0.0000001 ) xs = capac(iveg)
c      www(1) = www(1) + xs / ( poros * zdepth(1) )
	finfil = finfil + xs
      capac(iveg) = capac(iveg) - xs              
      ts = tsd      
c                   
      RETURN                   
	                   
      END           
c                   
c                   
c=======================================================================        
c                   
      SUBROUTINE patchs ( p0 )                    
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
      INCLUDE 'COMSIBC.H'
c                   
      pinf = p0     
      thru = 0.     
      snowhc = amin1( 0.05, snoww(2) ) * cw       
      areas = amin1( 1.,(asnow*snoww(2)) )        

      IF( tm .le. tf ) THEN                  
c                   
c----------------------------------------------------------------------         
c     snow falling onto area                      
c----------------------------------------------------------------------         
c                   
        rhs = tm*pinf*cw + tf*(snowhc + csoil*areas)
     &      + tg*csoil*(1.-areas)                   
        dareas = amin1( asnow*pinf, ( 1.-areas ) )  
        ex = rhs - tf*pinf*cw - tf*(snowhc + csoil*(areas + dareas))              
     &     - tg*csoil*(1.-areas-dareas)             
        IF( (areas+dareas) .ge. 0.999 ) tg = tf - 0.01   

        IF( ex .ge. 0. ) THEN                  
c                   
c----------------------------------------------------------------------         
c     excess energy is positive, some snow melts and infiltrates.               
c----------------------------------------------------------------------         
c                   
          IF( asnow*(snoww(2) + pinf - zmelt) .gt. 1. ) THEN                    
            zmelt = ex/snomel  
          ELSE
            IF( asnow*(snoww(2) + pinf) .lt. 1. )THEN       
                zmelt = 0.    
            ELSE
             zmelt = ( asnow*(snoww(2) + pinf) - 1. ) / asnow                      
            END IF
            zmelt = ( ex - zmelt*snomel )/
     &		      ( snomel + asnow*csoil*(tg-tf) ) + zmelt 
          END IF
          snoww(2) =  snoww(2) + pinf - zmelt         
c          www(1) = www(1) + zmelt/(poros*zdepth(1))   
			finfil = finfil + zmelt
        ELSE 
c                   
c----------------------------------------------------------------------         
c     excess energy is negative, bare ground cools to tf, then whole            
c     area cools together to lower temperature.   
c----------------------------------------------------------------------         
c                   
          IF((areas+dareas) .gt. 0.999 )THEN             
            tsd = 0.      
          ELSE 
           tsd = ex/(csoil*( 1.-areas-dareas)) + tg 
          END IF
          IF( tsd .le. tf ) THEN                 
            tsd = tf + ( ex - (tf-tg)*csoil*(1.-areas-dareas) )                       
     &          /(snowhc+pinf*cw+csoil)         
          END IF
          tg = tsd      
          snoww(2) = snoww(2) + pinf                  

        END IF

      ELSE 
c                   
c----------------------------------------------------------------------         
c     rain falling onto area                      
c----------------------------------------------------------------------         
c                   
c                   
c----------------------------------------------------------------------         
c     rain falls onto snow-free sector first.     
c----------------------------------------------------------------------         
c                   
        IF( areas .ge. 0.999 )THEN
           tsd = tf - 0.01    
        ELSE
           tsd = ( tm*pinf*cw + tg*csoil )/( pinf*cw + csoil )                        
        END IF
        tg = tsd      
c        www(1)= www(1)+pinf*(1.-areas)/(poros*zdepth(1)) 
			finfil = finfil + pinf*(1.-areas)
c                   
c----------------------------------------------------------------------         
c     rain falls onto snow-covered sector next.   
c----------------------------------------------------------------------         
c                   
        ex = ( tm - tf )*pinf*cw*areas              
        dcap = -ex / ( snomel + ( tg-tf )*csoil*asnow )  
        IF( (snoww(2) + dcap) .ge. 0. ) THEN   
c          www(1) = www(1)+(pinf*areas-dcap)/(poros*zdepth(1))                       
			finfil = finfil + (pinf*areas-dcap)
          snoww(2) = snoww(2) + dcap                  
        ELSE 
          tg = ( ex - snomel*snoww(2) - ( tg-tf )*csoil*areas ) / csoil
     &      + tg    
c          www(1)=www(1)+(snoww(2)+pinf*areas)/(poros*zdepth(1))                     
			finfil = finfil + (snoww(2)+pinf*areas)
          capac(2) = 0. 
          snoww(2) = 0. 
        END IF
      END IF  
c                   
      RETURN        
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE snow1   
c                   
c=======================================================================        
c                   
c     calculation of effects of snow cover on surface morphology and            
c     maximum water storage values.               
c                   
c++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++        
c                   
c       z0             roughness length (m)       
c       d              zero plane displacement (m)
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
c      d           (snow-modified value of dd, used in all calculations)        
c      z0          (snow-modified value of z0d, used in all calculations)       
c      rbc         (snow-modified value of cc1, used in all calculations)       
c      rdc         (snow-modified value of cc2, used in all calculations)       
c      areas       (fraction of ground covered by snow)
c      satcap(1)   (s-c)   : equation (56) , SE-86, page 741 se-89              
c      satcap(2)   (s-g)   : 0.002, surface interception store                  
c----------------------------------------------------------------------         
c                   
      INCLUDE 'COMSIBC.H'
c                   
      canex  = 1.-( snoww(2)*5.-z1)/(z2-z1)       
      canex  = amax1( 0.1, canex )                
      canex  = amin1( 1.0, canex )                
      d      = z2 - ( z2-dd ) * canex             
      z0     = z0d/( z2-dd ) * ( z2-d )           
      rbc    = cc1/canex 
      rdc    = cc2*canex 
      areas    = amin1(1., asnow*snoww(2))        
      satcap(1) = zlt*0.0001 * canex              
c 3/96 changes
c old      satcap(2) = 0.002
      satcap(2) = 0.0002
      RETURN        
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE rada2   
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
      INCLUDE 'COMSIBC.H'
c                   
      real tranc1(2), tranc2(2), tranc3(2)   
c                   
      f = sunang    
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
      CALL snow1    
c                   
      facs  = ( tg-tf ) * 0.04                    
      facs  = amax1( 0. , facs)                   
      facs  = amin1( 0.4, facs)                   
      fmelt = 1. - facs  
c                   
      DO 1000 iwave = 1, 2                        
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
     &       ( tran2 + reff2)                     
      chiv = chil   
c                   
      IF ( abs(chiv) .le. 0.01 ) chiv = 0.01      
      aa = 0.5 - 0.633 * chiv - 0.33 * chiv * chiv
      bb = 0.877 * ( 1. - 2. * aa )               
c                   
      proj = aa + bb * f 
      extkb = ( aa + bb * f ) / f  
      zmew = 1. / bb * ( 1. - aa / bb * alog ( ( aa + bb ) / aa ) )             
      acss = scat / 2. * proj / ( proj + f * bb )      
      acss = acss * ( 1. - f * aa / ( proj + f * bb ) * alog ( ( proj           
     *       +   f * bb + f * aa ) / ( f * aa ) ) )    
c                   
      upscat = green * tran1 + ( 1. - green ) * tran2  
      upscat = 0.5 * ( scat + ( scat - 2. * upscat ) * 
     *         (( 1. - chiv ) / 2. ) ** 2 )       
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
      IF( abs(bot) .le. 1.e-10) THEN        
        scat = scat* 0.98  
        be = 1. - scat + upscat                     
        bot = ( zmew * extkb ) ** 2 + ( ce**2 - be**2 )  
      END IF

      de = scat * zmew * extkb * betao            
      fe = scat * zmew * extkb * ( 1. - betao )   
      hh1 = - de * be + zmew * de * extkb - ce * fe    
      hh4 = - be * fe - zmew * fe * extkb - ce * de    
c                   
	if ((be**2 - ce**2).lt.0.0) then
		print*, be,ce,scat,upscat,bot,zmew,green,tran1,reff1,reff2
		print*, tran(iwave,1),scov,iwave,fmelt, tran(iwave,2)
		print*, snoww(1),satcap(1) 
	endif
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
      DO  irad = 1, 2
c
      salb(iwave,irad) = ( 1.-vcover ) * albedo(2,iwave,irad) +                 
     &                   vcover * albedo(1,iwave,irad) 
      END DO
c                   
1000  CONTINUE      
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
      CALL longrn( tranc1, tranc2, tranc3 )     
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
      DO iveg  = 1, 2                        
      DO iwave = 1, 2                        
      DO irad  = 1, 2                        
                   
        radt(iveg) = radt(iveg)+radfac(iveg,iwave,irad)*radn(iwave,irad)
                   
      END DO
      END DO
      END DO
c                   
      radt(1) = radt(1) + radn(3,2)*vcover*(1.- thermk) - closs                 
      radt(2) = radt(2) + radn(3,2)*( 1.-vcover*(1.-thermk) ) - gloss           
c                   
      RETURN        
      END           
c=======================================================================        
c                   
      SUBROUTINE longrn( tranc1, tranc2, tranc3 ) 
c                   
c=======================================================================        
c                   
c     calculation of downward longwave. this is not required in gcm if          
c     downward longwave is provided by gcm-radiation code as radn(3,2).         
c                   
c-----------------------------------------------------------------------        
      INCLUDE 'COMSIBC.H'
c                   
      real tranc1(2), tranc2(2), tranc3(2)   
c                   
      IF(ilw .eq. 1)THEN
c
c----------------------------------------------------------------------         
c     downward long-wave assumed to be provided as radn(3,2) 
c     no calculation is needed
c----------------------------------------------------------------------         
c
      ELSE IF(ilw .eq. 2)THEN                     

c----------------------------------------------------------------------         
c     downward long-wave from brunt's equation, Monteith(1973), p37.            
c----------------------------------------------------------------------         
c
        esky = 0.53 + 0.06*sqrt(em)                 
        radn(3,2)  =  esky*(1.+0.2*(cloud*cloud))*stefan*tm**4                    

      ELSE IF(ilw .eq. 3)THEN                     
c
c----------------------------------------------------------------------         
c     downward long-wave flux calculated as residual from measured              
c     net radiation and outgoing longwave radiation.   
c                   
c     calculation of absorbed fractions of radiation ( expendable )             
c----------------------------------------------------------------------         
c
        DO iwave = 1, 2                        
c                   
          rab(2,iwave,1) =  ( 1. - vcover ) *         
     &      ( radn(iwave,1) * ( 1. - albedo(2,iwave,1) ) ) 
          rab(2,iwave,2) =  ( 1. - vcover ) *         
     &      radn(iwave,2) * ( 1. - albedo(2,iwave,2) )   
c                   
          rab(2,iwave,1) = rab(2,iwave,1) + vcover *  
     &      (radn(iwave,1) * ( tranc1(iwave) * ( 1.-albedo(2,iwave,1))+        
     &      tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) ) )                        
          rab(2,iwave,2) = rab(2,iwave,2) + vcover *  
     &      radn(iwave,2) * tranc2(iwave) * ( 1. - albedo(2,iwave,2) )            
c                   
          rab(1,iwave,1) =  vcover *                  
     &      radn(iwave,1) * ( ( 1. - albedo(1,iwave,1) ) -                        
     &      tranc1(iwave) * ( 1. - albedo(2,iwave,1) ) - 
     &      tranc3(iwave) * ( 1. - albedo(2,iwave,2) ) ) 
          rab(1,iwave,2) =  vcover *                  
     &      radn(iwave,2) * ( ( 1. - albedo(1,iwave,2) ) -                        
     &      tranc2(iwave) * ( 1. - albedo(2,iwave,2) ) ) 

        END DO 
c                   
        swab = rab(1,1,1) + rab(1,1,2) + rab(1,2,1) + rab(1,2,2) +                
     &         rab(2,1,1) + rab(2,1,2) + rab(2,2,1) + rab(2,2,2)                  
        swup = swdown - swab                        
        radn(3,2) = rnetm - swab + zlwup            
c                   
      END IF
c                   
      RETURN        
      END           
                    
c=======================================================================        
c                   
      SUBROUTINE begtem  
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
      INCLUDE 'COMSIBC.H'                        
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
      CALL snow1    
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
      wc = amax1( 0.,wc)
      wg = amin1( 1.,wg)  
c                   
c-----------------------------------------------------------------------        
c     calculation of soil moisture stress factor. 
c     average soil moisture potential in root zone (layer-2) used as            
c     source for transpiration.                   
c                   
c      phroot      (psi-r) : equation (47) , SE-86
c      rstfac(2)  f(psi-l) :    "     (12) , SE-89
c-----------------------------------------------------------------------        
c                   
      phroot = phsat * amax1( 0.02, www(2) ) ** ( - bee )                       
      phroot = amax1 ( phroot, -2.e3 )            
      rstfac(2) = 1./( 1 + exp( 0.02*( phc-phroot) ) ) 
      rstfac(2) = amax1( 0.0001, rstfac(2) )      
      rstfac(2) = amin1( 1.,     rstfac(2) )      
c                   
c----------------------------------------------------------------------         
c                   
c      rsoil function from fit to FIFE-87 data.  soil surface layer             
c      relative humidity.
c                   
c      rsoil      (rsoil) : SE-92B (personal communication)                
c      hr         (fh)    : equation (66) , SE-86 
c                   
c----------------------------------------------------------------------         
c                   
      fac = amin1( www(1), 1. )                   
      fac = amax1( fac, 0.02  )                   
c 3/96 changes
c old      rsoil =  amax1 (0.1, 694. - fac*1500.) + 23.6
      rsoil =  exp (8.206 - 4.255*fac ) 
c                   
      psit = phsat * fac ** (- bee )              
      argg = amax1(-10.,(psit*g/461.5/tgs))       
      hr = exp(argg)
c                   
      RETURN        
      END           
c                   
c======================================================================         
c                   
      SUBROUTINE endtem (ipbl)                    
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
c               reference : SE-92A  
c                   
c=======================================================================        
c                   
c     subroutines  called : rasite --> unstab,stab,rafcal                 
c     -------------------   rbrd            
c  phosib --> cycalc-sortin                      
c  delrn           
c  delhf           
c  delef           
c  sibslv --> gauss          
c  dtcdtg          
c  newton    
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
      INCLUDE'COMSIBC.H' 
      INCLUDE'PARDIF.H'  
c                   
      ifirst = 1    
      icount = 0    
c                   
      fc = 1.       
      fg = 1.       
      ta = (tgs+tc)/2.   
      ea = em       
      ht = 0.  		     
c                   
      DO WHILE (icount .le. 4)
         icount = icount + 1
c                    
         CALL rasite   
c                   
         CALL rbrd     
c                 
      END DO
c                   
      CALL phosib   
c                   
      ifirst = 0    
      CALL delrn    

c                   
c----------------------------------------------------------------------         
c                   
c     dew calculation : dew condition is set at beginning of time step.         
c     if surface changes state during time step, latent heat flux is            
c     set to zero.  
c                   
c----------------------------------------------------------------------         
c                   
      IF ( ea .gt. etc ) fc = 0.                  
      IF ( ea .gt. etgs) fg = 0.                  
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
      DO WHILE (nox.eq.0)
c                   
        CALL rasite   
c                   
        CALL delhf    
c                   
        CALL delef    
c                   
        IF (ipbl .eq. 0 ) THEN
            CALL dtcdtg   
        ELSE IF (ipbl .eq. 1 ) THEN
            CALL sibslv    
        END IF 
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
     &    amax1(0.0, egi-(snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)               
        egit  =       
     &    amin1(egi,     (snoww(2)+capac(2))*1.e3*hlat )*(1.-rsnow)               
c                   
c----------------------------------------------------------------------         
c     calculation of interception loss from ground-snow. if snow patch          
c     shrinks, energy is taken from egi to warm exposed soil to tgs.            
c----------------------------------------------------------------------         
c                   
        t1 = snoww(2) - 1./asnow                    
        t2 = amax1( 0., t1 )                        
        aven = egi - t2*hlat*1.e3/snofac            
        IF ( (t1-t2)*egi .gt. 0. ) aven = egi       
        darea = aven/( (tsnow-tg)*csoil - 1./asnow*hlat*1.e3/snofac)              
        arean = areas + darea                       
        egidif = egidif - amin1( 0., arean )
     &         / asnow*hlat*1.e3/snofac*rsnow         
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
        IF ( i .gt. itrunk ) goto 44771                      
        CALL newton(ht,y,finc,nox,nonpos,iwalk,lx)  

      END DO 
44771 continue 
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
      IF ( fg .lt. .5 ) THEN
         hrr = 1.                                                
      ELSE
          hrr = hr                                                                  
      END IF
      emin = -1000.
      ecod = eci
      eci  = amax1 ( emin, eci )
      ecidif = ecidif + ecod - eci
      ecod = egi
      egi  = amax1 ( emin, egi )
      egidif = egidif + ecod - egi 
c                                                                               
      coct = (1.-wc)/ ( rst*fc + 2.*rb )                                        
      cogs1 = (1.-areas)/(rd+rsoil*fg)*(1.-wg)*hrr                              
      cogs2 = cogs1 / hrr                                                       
c                                                                               
      ect = ecpot * coct * rcp/psy * dtt                                        
      ecidif = ecidif + amin1 ( 0., ect)
      ect = amax1 ( ect , 0.)
      egs = (etgs + getgs*dtg ) * cogs1                                         
     &      - ( ea + deadtg*dtg + deadtc*dtc + deadqm*dqm ) * cogs2             	 
      egs = egs * rcp/psy * dtt                                                 
      egsmax = www(1) / 2. * zdepth(1) * poros * hlat * 1000.                   
      egidif = egidif + amax1( 0., egs - egsmax )                               
      egs = amin1 ( egs, egsmax )                                              
      egidif = egidif + amin1( 0., egs)
      egs = amax1 ( egs, 0.)               
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
      IF(dewc*ecf.le.0.0) THEN               
         hc = hc + eci + ect
         eci = 0.      
         ect = 0.      
      END IF
      IF(dewg*egf.le.0.0) THEN
         hg = hg + egs + egi
         egs = 0.      
         egi = 0.      
      END IF 
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
c 3/96 changes
c old    shf = cg / dtt * dtg + timcon*cg*2. * ( tgs+dtg - td )                    
      shf = cg / dtt * dtg + timcon*csoil*2. * ( tgs+dtg - td )                     
c                   
      zlwup = zlwup - rncdtc * dtc / 2.           
     &              - rngdtg * dtg * (1.-vcover*(1.-thermk) )                   
c                   
c----------------------------------------------------------------------         
c                   
      RETURN        
      END           
c======================================================================         
c                   
c        *********    auxiliary subroutine     **********                
c                   
c=======================================================================        
c                   
       SUBROUTINE rbrd   
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
      INCLUDE 'COMSIBC.H'                        
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
      fih = sqrt( 1. + 9. * g * temdif * z2 / tgs / ( u2*u2) )                  
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
      RETURN       
      END          
c                   
c=======================================================================        
c                   
      SUBROUTINE phosib  
c                   
c                   
c=======================================================================        
c                   
c     calculation of canopy photosynthetic rate using the integrated            
c     model relating assimilation and stomatal conductance.                     
c     method uses numerical solution based on extrapolation from error          
c     versus co2i line.  
c     units are converted from mks to biological units in this routine.         
c     base reference :  SE-92A                    
c                   
c units                  
c-------                 
c                   
c      pco2m, pco2a, pco2i, po2m                : pascals                       
c      co2a, co2s, co2i, h2oa, h2os, h2oa       : mol mol-1                     
c      vmax0, respn, assim, gs, gb, ga, pfd     : mol m-2 s-1                   
c      effcon     : mol co2 mol quanta-1          
c      gcan, 1/rb, 1/ra, 1/rst                  : m s-1
c      evapkg     : kg m-2 s-1                    
c      q          : kg kg-1                       
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
coutput                  
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
c         rstfac(1) ( f(h-s) )               : equation (17,18), SE-92A         
c         rstfac(2) ( f(soil) )              : equation (12 mod), SE-89         
c         rstfac(3) ( f(temp) )              : equation (5b)   , CO-92          
c         rstfac(4) ( f(h-s)*f(soil)*f(temp))     
c                   
c                   
c----------------------------------------------------------------------         
c                   
       INCLUDE 'COMSIBC.H'                        
c                   
      real pco2y(6), eyy(6)                  
c                   
c----------------------------------------------------------------------         
c                   
      respg = 0.    
c                   
c----------------------------------------------------------------------         
c
      IF(effcon .le. 0.07)THEN
         c3     = 0.   
         c4     = 1. 
      ELSE
         c3     = 1.              
         c4     = 0. 
      END IF 
c                   
c----------------------------------------------------------------------         
c                   
c                   
c     calculation of canopy par use parameter.    
c                   
c     fparkk      (pi)     : equation (31) , SE-92A   
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
c      qt          (qt)    : table (2)     , SE-92A    
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
c      zkc          (kc)     : table (2)     , SE-92A  
c      zko          (ko)     : table (2)     , SE-92A  
c      spfy         (s)      : table (2)     , SE-92A  
c      gammas       (gamma-*): table (2)     , SE-92A  
c      omss         (omega-s): equation (13) , SE-92A  
c      bintc        (b*zlt)  : equation (35) , SE-92A  
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
     &     + rrkk * rstfac(2) * c4                
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
      DO ic = 1, 6  
         pco2y(ic) = 0.
         eyy(ic) = 0.  
      END DO
c                   
      DO ic = 1, 6  
c                   
      ic2 = ic      
c                   
      CALL       sortin( eyy, pco2y, range, gammas, ic )                        
c                   
      CALL       cycalc( fparkk, vm, gradm, bintc, atheta, btheta,              
     &                   gah2o, gbh2o, gog1, gog2, wc, 
     &                   h2oi, h2om, h2osl, par, pco2m, psur,                   
     &                   gammas, respc, respg, rrkk, omss, c3, c4,              
     &                   pco2y(ic), eyy(ic), gsh2o, assimn, h2os, h2oa )        
c                   
      IF( abs(eyy(ic)) .lt. 0.1 ) goto 44772        
c                   
      END DO
44772 continue 
c                   
      pco2i = pco2y(ic2) 
c                   
      rstfac(1) = h2os/h2oi                       
      rstfac(4) = rstfac(1)*rstfac(2)* rstfac(3)  
      rst   = amin1( 1.e6, 1./( gsh2o*tc/( 44.6*tprcor) ) )                     
      ea    = h2oa*psur  
c                   
      RETURN        
      END           
c=======================================================================        
c                   
      SUBROUTINE cycalc( fparkk, vm, gradm, bintc, atheta, btheta,              
     &                   gah2o, gbh2o, gog1, gog2, wc, 
     &                   h2oi, h2om, h2osl, par, pco2m, psur,                   
     &                   gammas, respc, respg, rrkk, omss, c3, c4,              
     &                   pco2i, eyy, gsh2o, assimn, h2os, h2oa )                
c                   
c=======================================================================        
c                   
c     calculation equivalent to steps in figure 4 of SE-92A                     
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
c                        (omega-c): equation (11) , SE-92A                      
c       ome            light limited assimilation (mol m-2 s-1)                 
c                        (omega-e): equation (12) , SE-92A                      
c       oms            sink limited assimilation (mol m-2 s-1)                  
c       co2s           canopy surface co2 concentration (mol mol-1)             
c                        equation (18c) , SE-92  
c       assimn         (a-n)    :  equation (14,15), SE-92A                     
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
     &             (assimn - respg)* psur*100.)  
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
      RETURN        
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE sortin( eyy, pco2y, range, gammas, ic )                        
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
      real eyy(6), pco2y(6)                  
c                   
      IF( ic .lt. 4 ) THEN

        pco2y(1) = gammas + 0.5*range               
        pco2y(2) = gammas + range*( 0.5 - 0.3*sign(1.0,eyy(1)) )                  
        pco2y(3) = pco2y(1)
     &           - (pco2y(1)-pco2y(2))/(eyy(1)-eyy(2)+1.e-10)*eyy(1)             
c                   
        pmin = amin1( pco2y(1), pco2y(2) )          
        emin = amin1(   eyy(1),   eyy(2) )          
        IF ( emin .gt. 0. .and. pco2y(3) .gt. pmin ) pco2y(3) = gammas

      ELSE
c                   
        n = ic - 1    
        DO j = 2, n   
          a = eyy(j)    
          b = pco2y(j)  
          DO i = j-1,1,-1                        
            IF(eyy(i) .le. a ) goto 44773                
            eyy(i+1) = eyy(i)  
            pco2y(i+1) = pco2y(i)                       
          END DO
44773 continue 
c          i = 0         
          eyy(i+1) = a  
          pco2y(i+1) = b
        END DO
c                   
        pco2b = 0.    
        is    = 1     
        DO ix = 1, n  
          IF( eyy(ix) .lt. 0. ) THEN
              pco2b = pco2y(ix)     
              is = ix               
          END IF   
        END DO
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
     &        - (pco2y(is)-pco2y(isp))/(eyy(is)-eyy(isp))*eyy(is)                 
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
      END IF
c                   
      pco2y(ic) = amax1 ( pco2y(ic), 0.01 )       
c                   
      RETURN        
      END           
c                   
c======================================================================         
c                   
      SUBROUTINE delrn   
c                   
c======================================================================         
c                   
c     partial derivatives of radiative and sensible heat fluxes                 
c                   
c----------------------------------------------------------------------         
c
      INCLUDE 'COMSIBC.H'                        
      INCLUDE 'PARDIF.H'
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
      RETURN        
      END           
c======================================================================         
c                   
      SUBROUTINE delhf   
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
      INCLUDE 'COMSIBC.H'                        
      INCLUDE 'PARDIF.H'
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
      RETURN        
      END           
c======================================================================         
c                   
      SUBROUTINE delef   
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
      INCLUDE 'COMSIBC.H'                        
      INCLUDE 'PARDIF.H'
c                   
c----------------------------------------------------------------------         
c     modification for soil dryness : hr = rel. humidity in top layer           
c----------------------------------------------------------------------         
c                   
      IF ( fg .lt. .5 ) THEN
          hrr = 1.                  
      ELSE
          hrr = hr      
      END IF 
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
      RETURN        
      END           
c======================================================================         
c                   
      SUBROUTINE sibslv  
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
      INCLUDE 'COMSIBC.H'                        
      INCLUDE 'PARDIF.H'
c                   
c                   
      real pblsib(4,4),cvec(4),solvec(4),chin(4,5),work(4,5)               
c                   
      grav2 = 0.01*g
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
c 3/5/96 changes
c  old   pblsib(1,1)= cg/dtt + hgdtg+egdtg-rngdtg + timcon*cg*2.0                  
      pblsib(1,1)= cg/dtt + hgdtg+egdtg-rngdtg + timcon*csoil*2.0                
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
c 3/96 changes
c  old  cvec(1) = radt(2) - hg/dtt - eg/dtt - timcon*(tgs-td)*cg*2.               
      cvec(1) = radt(2) - hg/dtt - eg/dtt - timcon*(tgs-td)*csoil*2.             
      cvec(2) = radt(1) - hc/dtt - ec/dtt         
      cvec(3) = grav2*fths + etem*dthb            
      cvec(4) = grav2*fws  + etem*dwb             
c                   
c     solve 4 x 4 matrix equation                 
c                   
      DO j=1,4 
      DO i=1,4 
         chin(i,j)=pblsib(i,j)                       
      END DO
      END DO

      DO i=1,4 
         chin(i,5)=cvec(i)  
      END DO
c                   
      CALL gauss(chin,4,5,solvec,work)            
c                   
      dtg=solvec(1) 
      dtc=solvec(2) 
      dth=solvec(3) 
      dqm=solvec(4)  
c                   
      RETURN        
      END           
c                   
c======================================================================         
c                   
      SUBROUTINE dtcdtg  
c                   
c----------------------------------------------------------------------         
c                   
c     calculation of temperature tendencies assuming no interaction             
c     with the pbl : equations(69,70), SE-86      
c                   
c----------------------------------------------------------------------         
c                   
      INCLUDE 'COMSIBC.H'                        
      INCLUDE 'PARDIF.H'
c                   
      ccodtc = ccx / dtt - rncdtc + hcdtc + ecdtc 
      ccodtg = - rncdtg + hcdtg + ecdtg           
      ccorhs = radt(1) - ( hc + ec ) / dtt        
c                   
c 3/96 changes
c  old  gcodtg = cg / dtt + timcon*cg*2. - rngdtg + hgdtg + egdtg                 
      gcodtg = cg / dtt + timcon*csoil*2. - rngdtg + hgdtg + egdtg               
      gcodtc = - rngdtc + hgdtc + egdtc           
c 3/96 changes
c old   gcorhs = radt(2) - timcon*cg*2. * ( tgs -td ) - ( hg + eg ) / dtt         
      gcorhs = radt(2) - timcon*csoil*2. * ( tgs -td ) 
     &         - ( hg + eg ) / dtt        
c                   
      denom = ccodtc * gcodtg - ccodtg * gcodtc   
c                   
      dtc = ( ccorhs * gcodtg - ccodtg * gcorhs ) / denom                       
      dtg = ( ccodtc * gcorhs - ccorhs * gcodtc ) / denom                       
c                   
      RETURN        
      END           
                    
c=======================================================================        
c                   
      SUBROUTINE updat2  
c                   
c=======================================================================        
c                   
c     updating of all prognostic variables.       
c                   
c-----------------------------------------------------------------------        
c                   
c     subroutines called   : updat2          
c     ------------------     snow2           
c   run2            
c                   
c++++++++++++++++++++++++++++++output from this block++++++++++++++++++++       
c                   
c       dtc            canopy temperature increment (K)
c       dtd            deep soil temperature increment (K)                      
c       dtg            ground surface temperature increment (K)                 
c       www(3)         ground wetness             
c       capac(2)       canopy/ground liquid interception store (m)              
c       snoww(2)       canopy/ground snow interception store (m)                
c       roff           runoff (mm)                
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
      INCLUDE 'COMSIBC.H'
      CALL snow1    
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
        
      rsnow = snoww(1)/(snoww(1)+capac(1)+1.e-10) 
      facks = 1. + rsnow * ( snofac-1. )          
      IF ( (ect+eci) .le. 0.) THEN           
         eci = ect+eci 
         ect = 0.      
         facks = 1. / facks
      END IF	 
      capac(1) = capac(1) - ( 1.-rsnow )*eci*facks/hlat/1.e3                    
      snoww(1) = snoww(1) -      rsnow  *eci*facks/hlat/1.e3                    
      ecmass = eci*facks / hlat                   
c                   
      rsnow = snoww(2)/(snoww(2)+capac(2)+1.e-10) 
      facks = 1. + rsnow * ( snofac-1. )          
      IF ( (egs+egi) .le. 0. ) THEN
         egi = egs+egi 
         egs = 0.      
         facks = 1. / facks 
      END IF
      capac(2) = capac(2) - ( 1.-rsnow )*egi*facks/hlat/1.e3                    
      snoww(2) = snoww(2) -      rsnow  *egi*facks/hlat/1.e3                    
      egmass = egi*facks / hlat                   
c                   
c----------------------------------------------------------------------         
c    dumping of small capac values onto soil surface store                      
c----------------------------------------------------------------------         
c                   
      DO iveg = 1, 2
         IF ( (snoww(iveg)+capac(iveg)) .le. 0.00001 ) THEN                   
            www(1) = www(1) + 
     &               (snoww(iveg)+capac(iveg)) / ( poros*zdepth(1) )         
            capac(iveg) = 0.   
            snoww(iveg) = 0.
         END IF	   
      END DO
c                   
c----------------------------------------------------------------------         
c    snowmelt / refreeze calculation              
c----------------------------------------------------------------------         
c  
      CALL snow2    
c                   
c----------------------------------------------------------------------         
c    evapotranspiration losses,                   
c    extraction of transpiration loss from root zone, soil evaporation.         
c                   
c      ect         (e-dc)  : equation (5,6), SE-86
c      egs         (e-s)   : equation (5)  , SE-86
c----------------------------------------------------------------------         
c                   
      facl   = 1./hlat/1.e3/(poros*zdepth(2))     
      extrak = ect*facl  
      extrak = amin1( extrak, www(2) )            
      ectdif = ect - extrak/facl                  
      ect    = extrak/facl                        
      hc     = hc + ectdif                        
      ecmass = ecmass + ect/hlat                  
      www(2) = www(2) - ect*facl                  
c                   
      facl   = 1./hlat/1.e3/(poros*zdepth(1))     
      extrak = egs*facl  
      extrak = amin1( extrak, www(1) )            
      egsdif = egs - extrak/facl                  
      egs    = extrak/facl                        
      hg     = hg + egsdif                        
      egmass = egmass + egs/hlat                  
      www(1) = www(1) - egs*facl    		              

c                   
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
c                   
      CALL run2     
c                   
c----------------------------------------------------------------------         
c                   
c    update of temperatures and pbl variables. note that tc and tg              
c    are modified in interc as a result of precipitation inputs.                
c    update of interception stores.               
c                   
c----------------------------------------------------------------------         
c  
	dtg=max(-50., dtg)
	dtg=min(50., dtg)
	dtc=max(-50., dtc)
	dtc=min(50., dtc)
	dtd=max(-50., dtd)
	dtdc=min(50., dtd)

c	dth=max(-50., dth)
c	dth=min(50., dth)
c	dqm=max(-50., dqm)
c	dqm=min(50., dqm)
                 
      tc  = tc + dtc
      tg  = tg + dtg
      td  = td + dtd
      th  = th + dth
      qm  = qm + dqm
c                   
      RETURN        
      END           
c=======================================================================        
c                   
      SUBROUTINE snow2   
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
      INCLUDE 'COMSIBC.H'                        
c                   
      DO iveg = 1, 2
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
        IF ( snoww(iveg) .le. 0. ) THEN        
          IF ( ( ts+dts) .le. tf ) THEN
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
          END IF
        ELSE
c                   
c-----------------------------------------------------------------------        
c                   
c     snow present  
c                   
c-----------------------------------------------------------------------        
c                   
c                   
          IF ( ts .ge. tf .or. (ts+dts) .ge. tf ) THEN
             IF ( ts .le. tf ) THEN
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
               avheat = avex*( 1.-areas )*realg 
     &                + ( avmelt-zmelt )*snomel/dtt            
c                   
               safe = amax1( ( 1.-areas*realg ), 1.e-8 )   
               dts = tf-0.01 - ts + avheat / ( cctt*safe )*dtt  
             
		   ELSE
c                   
c-----------------------------------------------------------------------        
c                   
c     snow present and ts > tf : ground only.     
c                   
c-----------------------------------------------------------------------        
c                   
c                   
               tbulk = tsnow*areas + ts*( 1. - areas )     
               tn = tbulk + dts   
               exheat = cct*( 1.001-amax1(0.1,areas)) * dts
               exmelt = flux*dtt - exheat                  
               heat = exheat 
               dtsg = exheat / ( cct*(1.001-areas ))       
               IF ( (ts+dtsg) .le. tf ) THEN
                 heat = ( tf-0.01 - ts ) * ( cct*(1.-areas) )
                 dtsg = tf-0.01 - ts
               END IF     
               exmelt = exmelt + exheat - heat             
c                   
               IF( exmelt .ge. 0. ) THEN
                  zmelt = exmelt/snomel                       
                  IF( asnow*(snoww(iveg)-zmelt) .lt. 1. )THEN
                    zmelt = amax1( 0., snoww(iveg) - 1./asnow )                    
                  END IF
                  snoww(iveg) = snoww(iveg) - zmelt           
                  exmelt = exmelt - zmelt*snomel              
                  zmelt2 = exmelt/ ( cct*( ts-tf )*asnow + snomel )
                  zmelt2 = amin1( zmelt2, snoww(iveg) )       
                  zmelt = zmelt + zmelt2                      
                  snoww(iveg) = snoww(iveg) - zmelt2          
                  exmelt = exmelt - zmelt2*( cct*( ts-tf )*asnow 
     &                   + snomel )                 
                  dts  = dtsg + exmelt/cct                    
               ELSE
c                   
                  cool = amin1( 0., tf-0.01 - (ts+dtsg) ) * cct
     :                   *(1.-areas)                  
                  dtsg2 = amax1 ( cool, exmelt ) 
     &                  / ( cct*( 1.001-areas ) )                  
                  exmelt = exmelt - dtsg2*cct*(1.-areas)      
                  dtsg3 =exmelt/cctt 
                  dts = dtsg + dtsg2 + dtsg3                  
               END IF
             END IF
          END IF
        END IF
c                   
        www(1) = www(1) + zmelt / ( poros * zdepth(1) )  
c                   
        dtc = dtc*realg + dts*realc                 
        dtg = dtg*realc + dts*realg                 
c                   
      END DO
c                   
      fluxef = shf - cg*dtg/dtt                   
c 3/96 changes
c  old  dtd = fluxef / ( cg * 2. * sqrt ( pie*365. ) ) * dtt                      
      dtd = fluxef / ( csoil * 2. * sqrt ( pie*365. ) ) * dtt                     
c                   
      RETURN        
      END           

c=======================================================================        
c  Consider Soil Groundwater interaction tangqh@iis.u-tokyo.ac.jp                 
      SUBROUTINE run2    
c                   
c=======================================================================        
c    calculation of interflow, infiltration excess and loss to                  
c    groundwater .  all losses are assigned to variable 'roff' .                
c----------------------------------------------------------------------         
      INCLUDE 'COMSIBC.H'                        
	INTEGER icho1,icho2,icho3,icho4,icho5,icho6,icho32
	common icho1,icho2,icho3,icho4,icho5,icho6,icho32
c                   
      real temw(4), temwp(4), temwpp(4),     
     &     aaa(3) , bbb(3) , ccc(3) , ddd(3) ,qqq(3)  ,avkii(3) 
c 

c	if (item01.eq.60.and.item02.eq.153 ) then
c	write(icho3,*) www(1),www(2),www(3)
c	endif
	                  
      DO i = 1, 3   
         temw(i)   = amax1( 0.03, www(i) )           
         temwp(i)  = temw(i) ** ( -bee )             
         temwpp(i) = amin1( 1., temw(i)) ** ( 2.*bee+ 3. )
      END DO          

c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    calculation of gravitationally driven drainage from w(1-3) : taken           
c    as an integral of time varying conductivity.addition of liston             
c    baseflow term to original q3g to insure flow in                            
c    dry season. modified liston baseflow constant scaled                       
c    by available water.                                                        
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
      pows = 2.*bee+2.    

c      q1g = temw(1)**(-pows) + satco/zdepth(1)/poros*slope*pows*dtt             
c      q1g = q1g ** ( 1. / pows )                                                
c      q1g = - ( 1. / q1g - www(1) ) * poros * zdepth(1) / dtt                   
c	q1g = slope*satco* temw(1)**(2.*bee+3.)
c      q1g = amax1( 0., q1g )                                                    
c      q1g = amin1( q1g, www(1)*poros*zdepth(1)/dtt )   
c      www(1) = www(1) - q1g*dtt/(poros*zdepth(1)) 

c      roff  = roff  + q1g * dtt   
c	roff2 = roff2 + q1g * dtt                  
	
c      q2g = temw(2)**(-pows) + satco/zdepth(2)/poros*slope*pows*dtt             
c      q2g = q2g ** ( 1. / pows )                                                
c      q2g = - ( 1. / q2g - www(2) ) * poros * zdepth(2) / dtt                   
c	q2g = slope*satco* temw(2)**(2.*bee+3.)
c      q2g = amax1( 0., q2g )                                                    
c      q2g = amin1( q2g, www(2)*poros*zdepth(2)/dtt )                            
c      www(2) = www(2) - q2g*dtt/(poros*zdepth(2)) 

c      roff  = roff  + q2g * dtt   
c	roff2 = roff2 + q2g * dtt                  

      if (idirr.ne.1) then
      q3g = temw(3)**(-pows) + satco/zdepth(3)/poros*slope*pows*dtt             
      q3g = q3g ** ( 1. / pows )                                                
      q3g = - ( 1. / q3g - www(3) ) * poros * zdepth(3) / dtt                   
c	q3g = slope*satco* temw(3)**(2.*bee+3.)
      q3g = amax1( 0., q3g )                                                    
      q3g = amin1( q3g, www(3)*poros*zdepth(3)/dtt )                            
      www(3) = www(3) - q3g*dtt/(poros*zdepth(3)) 

      roff  = roff  + q3g * dtt   
	roff2 = roff2 + q3g * dtt       
      endif
	           
c                   
c----------------------------------------------------------------------         
c                   
c    calculation of inter-layer exchanges of water due to gravitation           
c    and hydraulic gradient. the values of w(x) + dw(x) are used to             
c    calculate the potential gradients between layers. 
c    modified calculation of mean conductivities follows ME-82 ), 
c    reduces recharge flux to top layer.                      
c                   
c      dpdw           : estimated derivative of soil moisture potential         
c                       with respect to soil wetness. assumption of             
c                       gravitational drainage used to estimate likely          
c                       minimum wetness over the time step.                     
c                   
c      qqq  (q     )  : equation (61) , SE-86     
c             i,i+1 
c            -      
c      avk  (k     )  : equation (4.14) , ME-82   
c             i,i+1 
c                   
c----------------------------------------------------------------------         
c                   
	

      wmax = amax1( www(1), www(2), www(3), 0.05 )
      wmax = amin1( wmax, 1. )                    
      pmax = wmax**(-bee)
      wmin =(pmax- (zdepth(1)+zdepth(2)+zdepth(3))/phsat )  
     &	**(-1./bee)                       
c      wmin = (pmax-2./( phsat*(zdepth(1)+2.*zdepth(2)+zdepth(3))))              
c     &        **(-1./bee)
      wmin = amin1( www(1), www(2), www(3), wmin )
      wmin = amax1( wmin, 0.02 )                  
      pmin = wmin**(-bee)
      dpdw = phsat*( pmax-pmin )/( wmax-wmin )    
c	print *,dpdw,wmax,wmin

c	dsg  = GWdep-(zdepth(1)+zdepth(2)+zdepth(3))/2.
c	dsg  = amax1 (dsg, (zdepth(1)+zdepth(2)+zdepth(3))/2.)
c      wmax = 1.
c      pmax = 1. 
c      wmin = (pmax- dsg/phsat)**(-1./bee) 
c      wmin = amin1( www(1),www(2),www(3), wmin)
c      wmin = amax1( wmin, 0.02 ) 
c      pmin = wmin**(-bee) 
c      dpdw = phsat*( pmax-pmin )/( wmax-wmin )

c                   
      DO i = 1, 2   
c                   
        rsame = 0.    
        avk  = temwp(i)*temwpp(i) - temwp(i+1)*temwpp(i+1)                        
        div  = temwp(i+1) - temwp(i)                
        IF ( abs(div) .lt. 1.e-6 ) rsame = 1.       
        avk = satco*avk / ( ( 1. + 3./bee ) * div + rsame )                       
        avkmin = satco * amin1( temwpp(i), temwpp(i+1) ) 
        avkmax = satco * amax1( temwpp(i), temwpp(i+1) )*1.01                     
        avk = amax1( avk, avkmin )                  
        avk = amin1( avk, avkmax )                  
c 3/96 changes
        IF (www(i).lt.www(i+1))avk = 0.1*avk
c                   
c-----------------------------------------------------------------------        
c     conductivities and base flow reduced when temperature drops below         
c     freezing.     
c-----------------------------------------------------------------------        
c                   
        tsnow = amin1 ( tf-0.01, tg )               
        tgs = tsnow*areas + tg*(1.-areas)           
        ts    = tgs*(2-i) + td*(i-1)                
        props = ( ts-(tf-10.) ) / 10.               
        props = amax1( 0.05, amin1( 1.0, props ) )  
        avk  = avk * props 
c                   
c-----------------------------------------------------------------------        
c     backward implicit calculation of flows between soil layers.               
c-----------------------------------------------------------------------        
c                   
        dpdwdz = dpdw * 2./( zdepth(i) + zdepth(i+1) )   
        aaa(i) = 1. + avk*dpdwdz*( 1./zdepth(i)+1./zdepth(i+1) )                  
     &            *dtt/poros                      
        bbb(i) =-avk *   dpdwdz * 1./zdepth(2)*dtt/poros 
        ddd(i) = avk * ( dpdwdz * ( www(i)-www(i+1) ) + 1.  ) 
	  if (i.eq.2) then
		ccc(i) = - avk * dpdwdz*1./zdepth(3)*dtt/poros
	  endif
	  avkii(i) =avk
      END DO
	ccc(1)=0.0
	ccc(3)=0.0
	
	d3g = GWdep-(zdepth(1)+zdepth(2)+zdepth(3)/2.)
	d3g = amax1 (d3g, zdepth(3)/2., 0.5)

c	d4g		= amax1(0.5 , (GWdep -(zdepth(1)+zdepth(2)+zdepth(3)) )/2.)
c	temw(4)   = (1.- d4g/phsat)**(-1./bee)
c      temw(4)   = amax1( 0.03, temw(4) )   
c      temw(4)   = amin1( 1., temw(4) )   
c	temwp(4)  = temw(4) ** ( -bee )       
c	temwpp(4) = amin1( 1., temw(4)) ** ( 2.*bee+ 3. ) 
c      rsame = 0.    
c      avk  = temwp(3)*temwpp(3) - temwp(4)*temwpp(4)                        
c      div  = temwp(4) - temwp(3)                
c      IF ( abs(div) .lt. 1.e-6 ) rsame = 1.       
c      avk = satco*avk / ( ( 1. + 3./bee ) * div + rsame ) 
c      avkmin = satco * amin1( temwpp(3), temwpp(4) ) 
c      avkmax = satco * amax1( temwpp(3), temwpp(4) ) * 1.01                    
c      avk = amax1( avk, avkmin )                  
c      avk = amin1( avk, avkmax )             
	avk = satco * temw(3)**( 2.*bee+3.)
c      ts    = td              
c      props = ( ts-(tf-10.) ) / 10.               
c      props = amax1( 0.05, amin1( 1.0, props ) )  
c      avk  = avk * props 
c	avk  = avk * 0.1
	avkii(3) = avk

      wmax = 1.
      pmax = 1. 
      wmin = (pmax- d3g/phsat)**(-1./bee) 
      wmin = amin1( www(3), wmin)
      wmin = amax1( wmin, 0.02 ) 
      pmin = wmin**(-bee) 
      dpdw = phsat*( pmax-pmin )/( wmax-wmin )

	dpdwdz = dpdw / d3g
	aaa(3) = 1.+ avk*dpdwdz*( 1./zdepth(3) )*dtt/poros  
	bbb(3) = -avk * dpdwdz * 1./zdepth(3)*dtt/poros
	ddd(3) = avk * ( dpdwdz * www(3)  + 1. - dpdwdz) 
c                   
      denom  = ( aaa(1)*aaa(2)*aaa(3) - aaa(3)*bbb(1)*bbb(2) - 
     $	aaa(1)*bbb(3)*ccc(2) )  
      rdenom = 0.   
      IF ( abs(denom) .lt. 1.e-6 ) rdenom = 1.    
      rdenom = ( 1.-rdenom)/( denom + rdenom )    
      qqq(1)   = ( aaa(2)*ddd(1)*aaa(3) - bbb(3)*ccc(2)*ddd(1) 
     $	- aaa(3)*ddd(2)*bbb(1)+ ccc(2)*ddd(3)*bbb(1) ) * rdenom                     
      qqq(2)   = ( aaa(1)*aaa(3)*ddd(2) - aaa(3)*ddd(1)*bbb(2) 
     $	- aaa(1)*ccc(2)*ddd(3) ) * rdenom
      q3g   = ( aaa(1)*aaa(2)*ddd(3) - bbb(1)*bbb(2)*ddd(3) 
     $	- aaa(1)*bbb(3)*ddd(2)+ bbb(3)*ddd(1)*bbb(2) ) * rdenom   

      q3gmax   =  www(3)   * (poros*zdepth(3)  /dtt)
      q3gmin   = (www(3)-1.) * (poros*zdepth(3)/dtt)
	q3gmin  = 0.
	q3g = amin1( q3g , q3gmax)  !, avkii(3)
	q3g = amax1( q3g , q3gmin)	!, -avkii(3)
c                   
c-----------------------------------------------------------------------        
c     update wetness of each soil moisture layer due to layer interflow         
c        and base flow.  
c-----------------------------------------------------------------------        
c                   
      www(3) = www(3) - q3g*dtt/(poros*zdepth(3)) 
c	gwsoil	= gwsoil-q3gn*dtt
c                   
      DO i = 1, 2   
        qmax   =  www(i)   * (poros*zdepth(i)  /dtt)
        qmin   = -www(i+1) * (poros*zdepth(i+1)/dtt)
        qqq(i) = amin1( qqq(i),qmax)         !, avkii(i)        
        qqq(i) = amax1( qqq(i),qmin)         !, -avkii(i)       
        www(i)   =   www(i)   - qqq(i)/(poros*zdepth(i)  /dtt)                    
        www(i+1) =   www(i+1) + qqq(i)/(poros*zdepth(i+1)/dtt)                    
      END DO


c	d4g		= amax1(0.5 , (GWdep -(zdepth(1)+zdepth(2)+zdepth(3)) )/2.)
c	temw(4)   = (1.- d4g/phsat)**(-1./bee) + q3g*dtt/(poros*d4g*2.)
c      temw(4)   = amax1( 0.03, temw(4) )   
c      temw(4)   = amin1( 1., temw(4) )   
c	temwp(4)  = temw(4) ** ( -bee )       
c	temwpp(4) = amin1( 1., temw(4)) ** ( 2.*bee+ 3. ) 
c      rsame = 0.    
c      avk		= temwp(4)*temwpp(4) - 1.                        
c      div		= 1. - temwp(4)                
c      IF ( abs(div) .lt. 1.e-6 ) rsame = 1.       
c      avk		= satco*avk / ( ( 1. + 3./bee ) * div + rsame ) 
c      avkmin= satco * amin1( temwpp(4), 1. ) 
c      avkmax= satco * amax1( temwpp(4), 1. ) * 1.01                    
c      avk		= amax1( avk, avkmin )                  
c      avk		= amin1( avk, avkmax )             
c      ts    = td              
c      props = ( ts-(tf-10.) ) / 10.               
c      props = amax1( 0.05, amin1( 1.0, props ) )  
c      avk		= avk * props 
c	dw_dz	=	phsat* (temwp(4) - 1.) /d4g
c	q4g		= avk *  (1. + dw_dz)
c	q4g		= amin1(q3g , q4g)
c	q4g		= amax1(q3g*0.5 , q4g)
c	print *,q4g,q3g
c	pause
c	roff2 = roff2 + (q3g-q4g) * dtt       
      roff  = roff  + q3g*dtt   
c	roff2 = roff2 + q3g*slope* dtt
	roff3 = roff3 + q3g* dtt                  


c		otest1	= q4g* dtt
c		otest2	= q2g* dtt
c		otest3	= q3g * dtt 
	
c                   
      DO i = 1, 3   
        excess = amax1(0.,(www(i) - 1.))            
        www(i) = www(i) - excess                    
        roff   = roff   + excess * poros*zdepth(i)
	  roff4	 = roff4  + excess * poros*zdepth(i)
      END DO

c	if (item01.eq.60.and.item02.eq.153 ) then
c	write(icho3,*) www(1),www(2),www(3)
c	write(icho3,*) qqq(1),qqq(2),q3g,satco
c	write(icho3,*) avk1,avk2,avk3
c	write(icho3,*)
c	endif
c	if (qqq(1).lt.-avkii(1).or.qqq(2).lt.-avkii(2)
c     $	.or.qqq(3).lt.-avkii(3)) then
c	write(icho3,*) www(1),www(2),www(3)
c	write(icho3,*) qqq(1),qqq(2),q3g,satco
c	write(icho3,*) avk1,avk2,avk3
c	write(icho3,*)
c	write(icho3,*) satco,q3g,q3g*dtt*1000.
c	endif
c                   
c-----------------------------------------------------------------------        
c     prevent negative values of www(i)           
c-----------------------------------------------------------------------        
c                   
      DO i = 1,2    
        deficit   = amax1 (0.,(1.e-12 - www(i)))    
        www (i)   = www(i) + deficit                
        www (i+1) = www(i+1) - deficit * zdepth(i) / zdepth (i+1)                 
      END DO
      www(3)    = amax1 (www(3),1.e-12)           
c                   
      RETURN        
      END 
c                   
c                   
c=======================================================================        
c                   
       SUBROUTINE cntrol(icho2,ichmet,iopt)   
c                   
c=======================================================================        
c                   
c      initialisation and switches.               
c                   
c-----------------------------------------------------------------------        
c
      INCLUDE 'COMSIBC.H'          
	dtt		= 3600.
	itrunk	= 20
	ilw		= 1
	
	tc_ini	= 300.0
	tg_ini	= 300.0
	td_ini	= 298.0
	
	www_ini(1)	= 0.6259
	www_ini(2)	= 0.6259	
	www_ini(3)	= 0.6259            
c                   
      IF(iopt .ne. 1)THEN
c                   
      WRITE(icho2,800)                
  800 FORMAT(10x,32('*')/10x,'*Sib2 off-line simulation in YR*'/10x,            
     &32('*')/5x)                   
      IF(itrunk .eq. 1) WRITE(icho2,801)           
      IF(itrunk .ge. 2) WRITE(icho2,802) itrunk    
  801 FORMAT(5x,'resistances calculated from initial fluxes')                   
  802 FORMAT(5x,'resistances calculated by iteration, itrunk=',i4)              
c      IF(ilw .eq. 1) WRITE(icho2,816)              
      IF(ilw .eq. 2) WRITE(icho2,817)              
      IF(ilw .eq. 3) WRITE(icho2,818)              
  816 FORMAT(5x,'downward longwave radiation read in as data'// )               
  817 FORMAT(5x,'downward longwave radiation computed from brunts',             
     & ' equation', // ) 
  818 FORMAT(5x,'downward longwave radiation computed as residual in ene        
     &rgy balance', /, 5x,'net radiation read in as data ',// )                 
c                   
      END IF 
c                   
      RETURN        
      END           
c                   
c=======================================================================        
c                   
      SUBROUTINE radc2_1  
c                   
c=======================================================================        
c                   
c     solar zenith angle computation; downcoming radiation at bottom.           
c                   
c-----------------------------------------------------------------------        
c                   
      INCLUDE 'COMSIBC.H'                        
c                   
      IF ( amod( year, 4. ) .eq. 0. ) THEN
          dayspy = 366.    
      ELSE
          dayspy = 365. 
      END IF
c                   
c-----------------------------------------------------------------------        
c    julian day and time update; skip on 1st time step (initialized)            
c-----------------------------------------------------------------------        
      IF(iter .gt. 1)THEN                     
         time = time + dtt / 3600.                   
         IF ( time .ge. 23.99 ) time = 0.0           
         day = day +  dtt / 86400.                   
      END IF
c                   
      IF ( day .gt. dayspy ) THEN
           year = year + 1.  
		 day = day - dayspy      
      END IF

	END

      SUBROUTINE radc2_2   
c                   
c=======================================================================        
c                   
c     solar zenith angle computation; downcoming radiation at bottom.           
c                   
c-----------------------------------------------------------------------        
c                   
      INCLUDE 'COMSIBC.H'                        
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
      RETURN        
      END           
c======================================================================         
c                   
      SUBROUTINE rasite  
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
c      d      : zero plane displacement           
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
c               h*rbb/(tm*u2*u2*(z2-ha)) gives bulk estimate of local 
c               richardson number.        
c               rbb = ra for heat between ha and z2. 
c               Rib = corb1*h*rbb/(tm*u2*u2*(z2-ha))             
c               corb1 = 9*g/( rhoair*cpair* (du/dz)**2 ) *u2*u2                       
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
      INCLUDE 'COMSIBC.H'                        
c                   

	us1  = 0.

      hress = ht    
      zl    = z2 + ztz0 * z0                      
      uest  = vkc*um / alog((zwind-d)/z0)  
c                   
c-----------------------------------------------------------------------        
c                   
c     calculation of u2 assuming neutral conditions    
c                   
c-----------------------------------------------------------------------        
c                   

      IF ( zwind .le. zl ) THEN
         top = 0.      
         zx1 = zwind - d    
         zx2 = z2 - d  
      ELSE
         zx1 = zwind - d    
         zx2 = zl - d    
         top = alog( zx1 / zx2 )                     
         zx1 = zl - d  
         zx2 = z2 - d  
      END IF
      bot = alog( zx1 / zx2 )                     
      ram = 1. / ( vkc * uest ) * ( top + g2 * bot )   
      u2 = um - ram * uest**2                     
c                   
c-----------------------------------------------------------------------        
c                   
c     calculation of ra for heat follows : non-neutrality assumed               
c                   
c-----------------------------------------------------------------------        
c                   
      zx1 = zwind - d   
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
      IF( ht .gt. 0. ) THEN                 
c                   
c-----------------------------------------------------------------------        
c                   
c     unstable case : calculation of ustar followed by ra                       
c                   
c-----------------------------------------------------------------------        
c  
		i=0
        DO WHILE (nox .eq. 0)
c       
          CALL unstab ( uest, zx1, zx2, arg1, ht, ps1, ps2)                        
c                   
          y = um - uest/vkc * ( arg1 - ps1 )         
c                   
		if (abs(y-uest).lt.0.0000001) then
			y=uest-0.0000001
		endif            
			i=i+1
			IF ( i .gt. itrunk ) goto 44441 
          CALL newton ( uest, y, finc, nox, nonpos, iwalk, lx ) 
		                   
        END DO
44441		continue
c                   
        IF( nox .eq. 2 ) WRITE(6,900)              
900     FORMAT( /,' convergence failure in rasite - unstable case' )             
c                   
        CALL rafcal ( zl, uest, ht, raf )          
c                   
      ELSE
c-----------------------------------------------------------------------        
c                   
c      stable case : calculation of ustar         
c                   
c-----------------------------------------------------------------------        
c                   
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
        gfac = alog((zwind - d)/z0)                
        hm1  = -0.95*tm*rhoair*cpair/(2.0*4.7*g*(zwind-d))*                      
     &          (2.0*um/3.0)**3*(vkc/gfac)**2     
        hm2  = 5.0*hm1    
        us2  = vkc*um/(gfac+4.7)                   
        IF( ht .ge. hm2 ) THEN                
           ht = amax1 ( hm1 , ht )                    
c                   
c----------------------------------------------------------------------         
c                   
c      ustar calculated for slightly stable conditions : ht .ge. hm1            
c                   
c----------------------------------------------------------------------         
c                   
			i=0
           DO WHILE (nox .eq. 0)
c                   
             CALL stab ( uest, zx1, zx2, arg1, ht, ps1, ps2) 
c                   
             y = um - uest/vkc * ( arg1 - ps1 )         
c                   
					i=i+1
					IF ( i .gt. itrunk ) goto 44442 
             CALL newton ( uest, y, finc, nox, nonpos, iwalk, lx )                    
           END DO
44442			continue
c                   
           IF( nox .eq. 2 ) WRITE(6,910)              
910        FORMAT( /,' convergence failure in rasite - stable case' )               
c                   
           ht = hress   
c                   
c-----------------------------------------------------------------------        
c      ustar calculation in interpolation zone    
c-----------------------------------------------------------------------        
c                   
           IF ( ht .le. hm1 ) THEN
              us1 = uest   
              uest = ( ht-hm2 ) / ( hm1-hm2 ) * ( us1-us2 ) + us2                      
           END IF
c                   
c-----------------------------------------------------------------------        
c      ustar calculation for collapsed profiles   
c-----------------------------------------------------------------------        
c                   
        ELSE
           uest = us2   
        END IF
c                   
c-----------------------------------------------------------------------        
c                   
c      calculation of ra for heat transfer between z2 and zmet                  
c                   
c-----------------------------------------------------------------------        
c                   
        raf = 1.e5   
c                   
        CALL rafcal ( zl, us2, hm2, rafmax )        
c                   
        IF ( ht .ge. hm2 ) THEN               
           hss = amax1 ( hm1, ht )                    
           uss = amax1 ( us1, uest )                  
c                   
           CALL rafcal ( zl, uss, hss, raf )          
c                   
           IF ( ht .le. hm1 ) THEN               
              raf1 = raf   
              raf  = ( ht-hm2 ) / ( hm1-hm2 ) * ( raf1 - rafmax ) 
     &               + rafmax             
            END IF
        END IF
c                  
        raf = amin1 ( raf , rafmax )   	              
c                   
c-----------------------------------------------------------------------        
c     above canopy variables calculated.          
c-----------------------------------------------------------------------        
c                   
      END IF 

      hrb = ( ht + sqrt(ht**2) ) / 2.0 + 0.1      
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
      nox    = 0                       
      nonpos = 1                       
      iwalk  = 0                       
      lx     = 1                       
      finc   = 0.2                     
c                   
	i=0
      DO WHILE (nox .ne. 1 )        
c                   
        coef3 = corb1 * hrb / tm / ( z2-ha )       
c                   
        y = coef3 * rbbest**3 + ( u2*rbbest )**2 - corb2
c                   
		i=i+1
		IF ( i .gt. itrunk ) goto 44443 
        CALL newton( rbbest , y, finc , nox, nonpos, iwalk, lx)                  

      END DO
44443		continue
c                   
      ra  = raf + rbbest
c                   
      ustar = uest 
      drag = rhoair * uest*uest                  
c                  
      RETURN       
      END          
c                   
c                   
c=======================================================================        
c                   
       SUBROUTINE unstab ( uest, a, b, argz, heat, psione , psitwo )            
c                   
c=======================================================================        
c                   
c      calculation of Paulson psi-function for unstable condition               
c                   
c-----------------------------------------------------------------------        
c                   
      INCLUDE 'COMSIBC.H'                        
c                  
      real x(2)    
c                   
      zin = a      
c                   
      DO i=1,2
         zml = -uest**3 * rhoair * cpair * tm       
         zml = zml / ( vkc*g*heat )                 
         fac = 16.0 * zin/zml                       
         x(i) = ( 1. - fac )**0.25   
         zin = b      
      END DO
c                   
      psione = 2.*alog((1.+x(1))/(1.+x(2)))+alog((1.+x(1)**2)/                 
     &          (1.+x(2)**2))-2.*atan(x(1))+2.*atan(x(2))                       
      psione = amin1 ( argz * 0.75, psione )     
c                   
	psitwo = 2.*alog((1.+x(1)**2)/(1.+x(2)**2))
      psitwo = amin1 ( argz * 0.75, psitwo )     
c                   
      RETURN       
      END          
c                   
c=======================================================================        
c                   
      SUBROUTINE stab ( uest, a, b, argz, heat, psione , psitwo )              
c                   
c=======================================================================        
c                   
c      calculation of Paulson psi-function for stable condition                 
c                   
c-----------------------------------------------------------------------        
c                   
      INCLUDE 'COMSIBC.H'                        
c                   
      psione = 0.  
      psitwo = 0.  
      IF ( abs(heat) .gt. 1.e-4 ) THEN
c                   
        zml = -uest**3 * rhoair * cpair * tm       
        zml = zml / ( vkc*g*heat )                 
c                    
        psione = -4.7 * ( a-b ) / zml              
        psione = amax1( -4.7, psione )             
c                   
        psitwo = psione   
c                   
      END IF
c                   
      RETURN       
      END          
c                   
c=======================================================================        
c                   
      SUBROUTINE rafcal ( zl, uest, heat, raf )  
c                   
c=======================================================================        
c                   
c      calculation of ra for heat between z2 and zmet  
c                   
c-----------------------------------------------------------------------        
c                   
      INCLUDE 'COMSIBC.H'                        
c                   
      IF ( zmet . le. zl ) THEN              
                    
         top = 0.      
         zx1 = zmet - d
         zx2 = z2 - d  
      
	ELSE
                   
         zx1 = zmet - d
         zx2 = zl - d   
         arg = alog( zx1 / zx2 )  
         IF ( heat .gt. 0. )THEN
            CALL unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
         ELSE
            CALL   stab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
         END IF
         top = arg - ps2    
                    
         zx1 = zl - d  
         zx2 = z2 - d  
     
	END IF

      arg = alog ( zx1 / zx2 )                    
      IF ( heat .gt. 0. )THEN
         CALL unstab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
      ELSE  
         CALL   stab ( uest, zx1, zx2, arg, heat, ps1, ps2)                  
      END IF
      bot = arg - ps2    
c                   
      raf = 1. / ( vkc*uest ) * ( top + g3 * bot )
c                   
      RETURN        
      END           
c                   
c=======================================================================        
c                   
       SUBROUTINE newton(a1,y,finc,nox,nonpos,iwolk,l) 
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
       integer iter(3), iwalk(3), nex(3)        
       real zinc(3), a2(3), y1(3)            
                    
       data cons/1.0/    
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
      RETURN
      END 
c                   
c=======================================================================        
c                   
      SUBROUTINE gauss ( a, n, np1, x, work )     
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
       INCLUDE 'COMSIBC.H'                        
c                   
      real a(4,5),work(4,5),x(4)             
c                   
      DO i=1,n 
      DO j=1,np1    
         work(i,j)=a(i,j)   
      END DO
      END DO
c                   
      DO i=2,n   
      DO j=i,n   

         r=work(j,i-1)/work(i-1,i-1)                 
c                   
         DO k=1,np1 
            work(j,k)=work(j,k)-r*work(i-1,k)           
         END DO
      END DO
      END DO
c                   
      DO i=2,n   
         k=n-i+2       
         r=work(k,np1)/work(k,k)                     
c                   
         DO j=i,n   
            l=n-j+1       
            work(l,np1)=work(l,np1)-r*work(l,k)         
         END DO
      END DO

c                   
      DO i=1,n   
         x(i)=work(i,np1)/work(i,i)                  
      END DO

      RETURN        
      END        
