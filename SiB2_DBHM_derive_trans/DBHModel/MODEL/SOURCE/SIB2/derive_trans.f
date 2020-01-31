c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE DERIVE_TRANS                ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     River and Environmental Engineering Laboratory   ######
c     ######                University of Tokyo                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE derive_trans
     &	(vtype,lai,rhoair,
     &     ha, z0d, dd,g2, g3, cc1, cc2, corb1, corb2)
c
C#######################################################################
c
c     PURPOSE:
c
c     Derive aerodynamic transfer parameters used in SiB2. 
c
c     The aerodynamic transfer model is a little different from Sellers, 1996, 1989, 1996:
c
c     (1)The model use the concept of mixing length
c        Km = G5*k*ustr*(z-d)           
c        where d is a local zero-displacement, defined in seelers 1986
c        G5 = 1+(G1-1)*(zt-z)/(zt-z2),   z>z2
c        G5 = G1,                        zc < z < z2
c        G5 = 1+(G1-1)*(z-z1)/(zc-z1),   zc < z < z1
c        G5 = 1                          z <z1
c     (2) The mixing coef. is continuous at canopy base.
c     (3) The model needs verification  
c
C#######################################################################
c      
c
c     AUTHOR: Kun Yang
c     01/12/01.
c
c     MODIFICATION HISTORY:
c
c     01/12/01 (K. Yang)
c     Added full documentation.
c
c
c#######################################################################
c
c
c#######################################################################
c
c     Variable Declarations.
c
c#######################################################################
c
      implicit none
 
C#######################################################################
c
c     Input variables:
c
C#######################################################################
c
      integer vtype     ! ARPS vegetation type
      real    lai       ! Leaf area index averaged on area from ARPS
      real    rhoair    ! Air density

c     Parameters defined in SiB2par.inc  
c
c     z0s_cst           ! Ground roughness length  (m)
c     g4_cst            ! Transition height factor for mom. transfer
c     g1_cst            ! km(actual) : km(log-linear) at z2
c     z2_v     (nv)     ! Canopy-top height  (m)
c     z1_v     (nv)     ! Canopy-base height (m)
c     zc_v     (nv)     ! Inflection height for leaf-area density (m)
c     chil_v   (nv)     ! Leaf area distribution factor 
c     leafw_v  (nv)     ! Leaf width (m)
c     leafl_v  (nv)     ! Leaf length	(m)
c
c
C#######################################################################
c
c     Output variables:
c
C#######################################################################
c
      real  ha    ! canopy source height for heat                               
      real  z0d   ! roughness length                                            
      real  dd    ! zero plane displacement                                     
      real  g2    ! ratio of ra(actual) to ra(log-linear) for momentum          
                  ! between: z = z2 and z = zx, where zx = min(zl,zwind)        
      real  g3    ! ratio of ra(actual) to ra(log-linear) for heat              
                  ! between: z = z2 and z = zx, where zx = min(zl,zmet)         
      real  cc1   ! rb coefficient (c1)                                         
      real  cc2   ! rd coefficient (c2)                                         
      real  corb1 ! non-neutral correction for calculation of aerodynami        
                  ! resistance between ha and z2. when multiplied by            
                  ! heat*rbb/tm gives bulk estimate of local richardson            
                  ! number                                                      
                  ! rbb = ra for heat between ha and z2.                        
                  ! corb1 = 9*g/( rhoair*cpair* (du/dz)**2 )                    
      real  corb2 ! = neutral value of (rbb*u2)**2, equivalent to          
                  ! c2**2 for upper canopy                                     
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
      real    zs        ! Ground roughness length  (m)
      real    g4        ! Transition height factor for mom. transfer
      real    g1        ! km(actual) : km(log-linear) at z2
      real    z2        ! Canopy-top height  (m)
      real    z1        ! Canopy-base height (m)
      real    zc        ! Inflection height for leaf-area density (m)
      real    chil      ! Leaf area distribution factor 
      real    leafw     ! Leaf width (m)
      real    leafl     ! Leaf length	(m)
      real    vcover
      real    bz        ! constant in Eq. (6), Sellers 1996b
      real    hz        ! constant in Eq. (6), Sellers 1996b
      parameter (bz = 0.91, hz = 0.0075)
      real    ra, rb,rd ! heat trans. resistance defined in Sellers 1986
      real    ustr      ! friction velocity above canopy
      real    ustr1     ! friction velocity at canopy base
      real    ustr2     ! friction velocity at canopy top
      real    zt        ! transitional layer top
      real    ut        ! velocity at transitional layer top
      real    u2        ! velocity at canopy top
      real    u1        ! velocity at canopy base
      real    kmt       ! Mom. mixing coef. in transitional layer (z2-zt)
      real    ksc       ! Heat mixing coef. in canopy layer 
                        ! = sigma*u in SiB2
                        ! = ku*G1(z-dlocal) in new scheme
      real    rat       ! Mom. resistance in transitional layer (z2-zt)
      real    kar       ! Karman constant
      real    g
      real    cpair
      parameter (kar = 0.4, g=9.81, cpair = 1010 )
c#######################################################################
c
c     Local variables for canopy solution
c
c#######################################################################
c
      integer nlay           ! number of canopy sublayer
      parameter (nlay = 2000)
      real    dzlay          ! depth of each sublayer = (z2-z1)/nlay
      real    ps(nlay)       ! sheltor factor
      real    laid(nlay)     ! local lai in each layer (m2/m3)
      real    u(nlay+1)      ! local vel.in each layer (m/s)     
      real    tau(nlay+1)    ! ustr**2 in each layer (m2/s2)      
      real    Km(nlay+1) 	   ! mixing coef. (m2/s)
      real    raint(nlay+1)  ! integrating result of 1/rb 
      real    dzero(nlay+1)  ! local zero-plane displacement
	integer halay		   ! layer of source height
      real    dudz1, dudz2   ! du/dz at layer i and i+1
      real    dudz           ! du/dz at layer i+1/2
      real    sintheta	   ! A function of leaf distribution factor
      real    Re             ! Typical Reynolds number over leaf
      real    Cd		       ! leaf drag coef. 
      real    Cs             ! leaf heat trans. coef. =90(lw)**1/2
      real    a1 ,b1, a2, b2 ! constant used to determine lai prof. 
      integer i,k,n !,j,m
	real    dz, z,tema, temb, temc,tem !, zz,dzz
c      real    ui,taui,kmi,mixli,di
      real    pi
      parameter (pi = 3.1415926)
      integer nox, nonpos, iwalk, lx
      real    finc, err
c
c#######################################################################
c
c     Include files: 
c
c#######################################################################
c
      include 'SiB2par.inc'       ! SiB2 parameters 
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c
C#######################################################################
c
c     Import data related with vegetation type and soil types 
c
C#######################################################################
c
c      CALL SiB2data
c
C#######################################################################
c
c     Vegetation type locates in the range of SiB2 ?
c
C#######################################################################
c
      IF( vtype.lt.1. or . vtype. gt.9 ) THEN
            write(6,*)
     :      'Vtype is out of SiB classification, vtyp = ', vtype
            write(6,*)'Stop in subroutine SiB2DERIVE'
            Stop 'in subroutine SiB2DERIVE'
      END IF 
c
C#######################################################################
c
c     Initialize relative parameter from SiB2 database
c
C#######################################################################
c
      zs    = z0s_cst       !	Ground roughness length  (m)
      g4    = g4_cst        ! Transition height factor for mom. transfer
      g1    = g1_cst        ! km(actual) : km(log-linear) at z2
      z2    = z2_v (vtype)  ! Canopy-top height  (m)
      z1    = z1_v (vtype)  ! Canopy-base height  (m)
      zc    = zc_v (vtype)  ! Inflection height for leaf-area density(m)
      chil  = chil_v(vtype) ! Leaf area distribution factor 
      leafw = leafw_v(vtype)! Leaf width (m)
      leafl = leafl_v(vtype)! Leaf width (m)
      vcover= vcover_v(vtype)  
c
C#######################################################################
c
c     Initialize other constants used 
c
C#######################################################################
c
      sintheta = (1-chil)/pi           ! mean leaf incident
      Re       = (leafw+leafl)/2 * 1 / 0.15e-4
                                       ! Reynolds number over leaf
      Cd       = 1.328 * (2/sqrt(Re))+0.45*sintheta**1.6
                                       ! Leaf drag coefficient
      Cs       = 90 * sqrt (leafw) 	 ! Leaf heat trans. coef.
      b1       = 2*lai/(z2-z1)/(zc-z1) ! z < zc lai(z) = a1 + b1*z
      a1       = -b1 * z1				 ! 
      b2       = 2*lai/(z2-z1)/(zc-z2) ! z > zc lai(z) = a2 + b2*z
      a2       = -b2 * z2				 ! 
      
      dzlay = (z2-z1)/nlay			 ! Thickness in canopy sublayer 
      DO i = 1, nlay
         z = z1 + (i-0.5) * dzlay
         IF (z.gt.zc) THEN
            laid (i)  = a2 + b2 * z
         ELSE
            laid (i) = a1 + b1 * z
         END IF             	   		           	     
         ps   (i)  = 1 + (laid (i)/vcover) ** 0.6 ! Sheltor factor
      END DO
c
C#######################################################################
c
c     Give u* at canopy base, and set relevant variables at canopy base
c
C#######################################################################
c
      ustr1   = 0.1					   ! u* at canopy base
	if ((z1/zs).lt.0.0) then
	print *, "derive_trans 001:1",z1/zs,z1,zs
	endif
	u1   	=ustr1*log(z1/zs)/kar	   ! vel. at canopy base
	u(1)	= u1					   !
      tau(1)  =ustr1 * ustr1  		   ! shear stress at canopy base
	dzero(1)=0						   ! Local zero-displacement
      Km(1)   = kar * (z1-dzero(1)) * sqrt(tau(1)) ! Mixing coef.
      raint(1)=0       				   ! integration to ra
c
C#######################################################################
c
c     Initialize newton-raphson iterative routine                    
c
C#######################################################################
c
      nox    = 0                                                     
      nonpos = 1                                                     
      iwalk  = 0                                                     
      lx     = 1                                                     
      finc   = 0.1                                                   
c                                                                               
      DO WHILE (nox .ne. 1 )        
c
C#######################################################################
c
c     Calculate velocity in canopy layer through three equation
c     d(tau)/dz= Cd*Laid*u*u/ps 
c	du/dz    = tau/km
c     km  = k*G1*(z-dlocal), 
c     dlocal is a local displacement defined as
c     d = int{d(tau)/dz * z dz}| [z1,z] /	tau (z), z1<z<z2
c
C#######################################################################
c
        DO i = 1, nlay
         z = z1 + i*dzlay  
         dudz1 = tau (i) / Km(i)    	 
         dudz2 = dudz1
         tem =  Cd*Laid(i)/ps(i)
         temc =  G1
         IF(z.lt.zc)temc =  1+(G1-1)*(z-z1)/(zc-z1)
         dzero (i+1) = dzero (i)
         tau (i+1) =tau(i)
         k = 1
         DO WHILE (k.lt.5 )
            dudz = 0.5 * (dudz1 + dudz2)    	   	     
            u(i+1) = u(i) + dudz * dzlay
            tema   = (u(i+1)+u(i))/2             ! average vel.
            temb   = z-0.5*dzlay                 ! average level
            tau(i+1)= tau(i)+tem * tema**2*dzlay
            dzero (i+1) = (dzero(i)*tau (i) +
     &                     tem*tema**2*temb*dzlay)/tau(i+1)  
            Km(i+1) = Kar * temc * (z-dzero(i+1))*sqrt(tau(i+1))
            dudz2 = tau(i+1)/Km(i+1)
            k = k+1
         END DO
         raint(i+1) = raint (i) 
     :              + laid(i)*sqrt(tema)/ps(i)/Cs*dzlay  
        END DO
c
C#######################################################################
c
c     Calculate z0 from Kondo (1994) p227 
c
C#######################################################################
c
        ustr2 = sqrt(tau(nlay+1)) 
        u2 = u(nlay+1)
        dd    = dzero(nlay+1)

        tem = lai/(z2-z1)
	  tem = 1+tem**0.6
        tem = exp(-Cd/tem*lai/2/kar**2)
	if ((z2/zs).le.0.0) then
	print *, "derive_trans 002:2",z2/zs,z2,zs
	endif
        tem = (1-tem+log(z2/zs)**(-1/0.45)*tem**2)**0.45
        tem =1/tem
        z0d = (z2-dd)/exp(tem)
c
C#######################################################################
c
c     Calculate velocity at transitional layer top through integration 
c     from z2 to zt
c          rat  = int{1/km*dz}| [z2, zt]
c     =>   ut   = u2 + ustr2**2 * rat
c     =>   ustr = k*ut/log((zt-dd)/z0)
c     If(ustr.ne.ustr2) Adjust z0
c     
C#######################################################################
c
c      Integrate mom. trans. resistnce in transitional layer
c	 
C#######################################################################
c
        zt  = z2 + z0d * g4    ! zt is transitional layer top
        n   = (zt-z2) / (0.01 * z0d) 
        dz  = (zt-z2) / n
        rat = 0
        DO i = 1, n
         z = zt - dz * (i-0.5)
         kmt = kar * ustr2 * (z-dd) * (1 + (g1-1) * (zt-z) / (zt-z2) )         
         rat = rat + 1/kmt * dz             
        END DO

        ut = u2 + ustr2 * ustr2 * rat     
	    
	if (((zt-dd) / z0d).le.0.0) then
	print *, "derive_trans 003:3",(zt-dd) / z0d,zt,dd,z0d
	endif   
        ustr = kar*ut/ LOG ((zt-dd) / z0d)
	
C#######################################################################
c
c     Adjust roughness length z0d according to velocity at zt
c
C#######################################################################
c
c                                                                               
        err = (ustr2-ustr) 
        CALL newton( G1 , err, finc , nox, nonpos, iwalk, lx)                  
c        write(*,*)ustr2,ustr,ustr2-ustr,g1
        z0d = (zt-dd)*exp(-(kar*ut)/ustr2)

      END DO
 
c      write(*,*)'Finish the calculation of velocity, shear stress,' 
c	write(*,*)'zero-displacement in canopy	and roughness length.'

c
C#######################################################################
c
c     Calculate canopy source height of heat transfer
c
C#######################################################################
c
      DO i = 1, nlay
         IF(raint(i).ge.0.5*raint(nlay+1))THEN
            halay = i
            ha    = z1+(halay-1)*dzlay
		  goto 44774
         END IF  
      END DO
44774 continue
c      write(*,*)'Finish source height calculation'
c
C#######################################################################
c
c     Calculate various resistance and coefficient
c
C#######################################################################
c
c
C#######################################################################
c
c     Calculate boundary bulk heat resistance in canopy layer and C1
c
C#######################################################################
c
      rb = 0
      DO i = 1, nlay
         z = z1 + dzlay * (i-0.5)
         tem  =  Laid(i)/(ps(i)*Cs)
         rb   = rb + tem * sqrt ((u(i)+u(i+1))/2) * dzlay             
      END DO
      rb  = 1/rb
      cc1 = rb * sqrt (u2)	 
c
C#######################################################################
c
c     Calculate heat resistance from ground to source height and C2
c
C#######################################################################
c

      rd =  u(1)/ (ustr1 * ustr1)
      DO i = 1, halay
         z = z1 + dzlay * (i-0.5)

         temc =  G1
         IF(z.lt.zc)temc =  1+(G1-1)*(z-z1)/(zc-z1)
         
         ksc = kar * temc * sqrt(tau(i))* (z-dzero(i))         
         ksc = (ksc + kar * temc * sqrt(tau(i+1))* (z-dzero(i+1)) )/2
         rd = rd + 1/ksc * dzlay             
      END DO
      cc2 = rd * u2
c
C#######################################################################
c
c     Calculate heat resistance from source height to canopy top
c
C#######################################################################
c
      dudz  = (1-u(halay)/u2)/(z2-ha)
      corb1 = 9*g/( rhoair*cpair* dudz**2 )
      ra    =  0
      DO i = halay, nlay
         z = z1 + dzlay * (i+0.5)

         temc =  G1
         IF(z.lt.zc)temc =  1+(G1-1)*(z-z1)/(zc-z1)

         ksc = kar * temc * sqrt(tau(i))* (z-dzero(i))         
         ksc = (ksc + kar * temc * sqrt(tau(i+1))* (z-dzero(i+1)))/2
         ra = ra + 1/ksc * dzlay             
      END DO
      corb2 = (ra * u2)**2
c
C#######################################################################
c
c     Calculate heat resistance from canopy top to zt, and g2, g3
c
C#######################################################################
c
c      real  g2    ! ratio of ra(actual) to ra(log-linear) for momentum          
c                  ! between: z = z2 and z = zx, where zx = min(zl,zwind)        
c      real  g3    ! ratio of ra(actual) to ra(log-linear) for heat              
c                  ! between: z = z2 and z = zx, where zx = min(zl,zmet)         
      n   = (zt-z2) / (0.01 * z0d) 
      dz  = (zt-z2) / n
	rat = 0
      DO i = 1, n
         z = zt - dz * (i-0.5)
         kmt = kar * ustr2 * (z-dd) * (1 + (G1-1) * (zt-z) / (zt-z2) )         
         rat = rat + 1/kmt * dz             
      END DO

	if (((z2-dd) / z0d).le.0.0) then
	print *, "derive_trans 004:4",(z2-dd) / z0d,z2,dd,z0d
	endif   

      tem= ustr2*log((z2-dd)/z0d)/kar
      g2 = rat / ((ut-tem)/ustr2**2)  
      g3 = g2

      RETURN
      END !SUBROUTINE derive_trans      


