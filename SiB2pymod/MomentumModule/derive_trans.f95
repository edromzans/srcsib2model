!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE DERIVE_TRANS               ######
!     ######                                                      ######
!     ######                     Developed by                     ######
!     ######     River and Environmental Engineering Laboratory   ######
!     ######                University of Tokyo                   ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################

    ! SUBROUTINE derive_trans &
    ! (vtype,lai,rhoair, &
    ! ha, z0d, dd,g2, g3, cc1, cc2, corb1, corb2)

    SUBROUTINE derive_trans(g1, z2, z1, chil, vcover, zlt, ztz0, &
         vkc, gx, cpair, rhoair, &
         ha, z0d, dd, g2, g3, cc1, cc2, corb1, corb2)

!#######################################################################

!     PURPOSE:

!     Derive aerodynamic transfer parameters used in SiB2.

!     The aerodynamic transfer model is a little different from Sellers, 1996, 1989, 1996:

!     (1)The model use the concept of mixing length
!        Km = G5*k*ustr*(z-d)
!        where d is a local zero-displacement, defined in seelers 1986
!        G5 = 1+(G1-1)*(zt-z)/(zt-z2),   z>z2
!        G5 = G1,                        zc < z < z2
!        G5 = 1+(G1-1)*(z-z1)/(zc-z1),   zc < z < z1
!        G5 = 1                          z <z1
!     (2) The mixing coef. is continuous at canopy base.
!     (3) The model needs verification

!#######################################################################

!     AUTHOR: Kun Yang
!     01/12/01.

!     MODIFICATION HISTORY:

!     01/12/01 (K. Yang)
!     Added full documentation.

!     2020/01 (Evandro M Anselmo)
!     Acoplamento da derive_trans com o SiB2, Fortran 95.

!#######################################################################


!#######################################################################

!     Variable Declarations.

!#######################################################################

!     Input variables:

!#######################################################################
    !use comsibc ! Variaveis sao recebidas pelo modulo comsibc
                ! Estas variaveis sao de entrada e locais para a derive_
                ! trans
    implicit none
      
    !integer :: vtype     ! ARPS vegetation type
    real (kind=8) :: lai  ! Leaf area index averaged - zlt do SiB2
    !real ::    rhoair    ! Air density

!     Parameters defined in SiB2par.inc

!     z0s_cst           ! Ground roughness length  (m)
!     g4_cst            ! Transition height factor for mom. transfer
!     g1_cst            ! km(actual) : km(log-linear) at z2
!     z2_v     (nv)     ! Canopy-top height  (m)
!     z1_v     (nv)     ! Canopy-base height (m)
!     zc_v     (nv)     ! Inflection height for leaf-area density (m)
!     chil_v   (nv)     ! Leaf area distribution factor
!     leafw_v  (nv)     ! Leaf width (m)
!     leafl_v  (nv)     ! Leaf length	(m)


!#######################################################################

!     Output variables:

!#######################################################################

    ! real (kind=8) ::  ha    ! canopy source height for heat
    ! real (kind=8) ::  z0d   ! roughness length
    ! real (kind=8) ::  dd    ! zero plane displacement
    ! real (kind=8) ::  g2    ! ratio of ra(actual) to ra(log-linear) for momentum
    !                ! between: z = z2 and z = zx, where zx = min(zl,zwind)
    ! real (kind=8) ::  g3    ! ratio of ra(actual) to ra(log-linear) for heat
    !                ! between: z = z2 and z = zx, where zx = min(zl,zmet)
    ! real (kind=8) ::  cc1   ! rb coefficient (c1)
    ! real (kind=8) ::  cc2   ! rd coefficient (c2)
    ! real (kind=8) ::  corb1 ! non-neutral correction for calculation of aerodynami
    !                ! resistance between ha and z2. when multiplied by
    !                ! heat*rbb/tm gives bulk estimate of local richardson
    !                ! number
    !                ! rbb = ra for heat between ha and z2.
    !                ! corb1 = 9*g/( rhoair*cpair* (du/dz)**2 )
    ! real (kind=8) ::  corb2 ! = neutral value of (rbb*u2)**2, equivalent to
    !                ! c2**2 for upper canopy

!#######################################################################

!     Misc. local variables:

!#######################################################################

    real (kind=8) ::    zs        ! Ground roughness length  (m)
    ! real (kind=8) ::    g4        ! Transition height factor for mom. transfer
    ! real ::    g1        ! km(actual) : km(log-linear) at z2
    ! real ::    z2        ! Canopy-top height  (m)
    ! real ::    z1        ! Canopy-base height (m)
    real (kind=8) ::    zc        ! Inflection height for leaf-area density (m)
    ! real ::    chil      ! Leaf area distribution factor
    real (kind=8) ::    leafw     ! Leaf width (m)
    real (kind=8) ::    leafl     ! Leaf length	(m)
    ! real ::    vcover
    real (kind=8), parameter ::    bz=0.91    ! constant in Eq. (6), Sellers 1996b
    real (kind=8), parameter ::    hz=0.0075  ! constant in Eq. (6), Sellers 1996b
    ! parameter (bz = 0.91, hz = 0.0075)
    ! real ::    ra, rb,rd ! heat trans. resistance defined in Sellers 1986
    !
    real (kind=8) :: raderivetrans
    real (kind=8) :: rbderivetrans
    real (kind=8) :: rdderivetrans
    !
    real (kind=8) ::    ustr      ! friction velocity above canopy
    real (kind=8) ::    ustr1     ! friction velocity at canopy base
    real (kind=8) ::    ustr2     ! friction velocity at canopy top
    real (kind=8) ::    zt        ! transitional layer top
    real (kind=8) ::    ut        ! velocity at transitional layer top
    ! real ::    u2        ! velocity at canopy top
    !
    real (kind=8) :: u2derivetrans 
    !
    real (kind=8) ::    u1        ! velocity at canopy base
    real (kind=8) ::    kmt       ! Mom. mixing coef. in transitional layer (z2-zt)
    real (kind=8) ::    ksc       ! Heat mixing coef. in canopy layer
    !                      ! = sigma*u in SiB2
    !                      ! = ku*G1(z-dlocal) in new scheme
    real (kind=8) ::    rat       ! Mom. resistance in transitional layer (z2-zt)
    !real (kind=8), parameter :: kar = 0.4      ! Karman constant
    !real (kind=8), parameter :: g = 9.81
    !real (kind=8), parameter :: cpair = 1010
    !parameter (kar = 0.4, g=9.81, cpair = 1010 )
!#######################################################################

!     Local variables for canopy solution

!#######################################################################

    integer, parameter :: nlay = 2000           ! number of canopy sublayer
    ! parameter (nlay = 2000)
    real (kind=8) ::    dzlay          ! depth of each sublayer = (z2-z1)/nlay
    real (kind=8) ::    ps(nlay)       ! sheltor factor
    real (kind=8) ::    laid(nlay)     ! local lai in each layer (m2/m3)
    real (kind=8) ::    u(nlay+1)      ! local vel.in each layer (m/s)
    real (kind=8) ::    tau(nlay+1)    ! ustr**2 in each layer (m2/s2)
    real (kind=8) ::    Km(nlay+1) 	   ! mixing coef. (m2/s)
    real (kind=8) ::    raint(nlay+1)  ! integrating result of 1/rb
    real (kind=8) ::    dzero(nlay+1)  ! local zero-plane displacement
    integer :: halay		   ! layer of source height
    real (kind=8) ::    dudz1, dudz2   ! du/dz at layer i and i+1
    real (kind=8) ::    dudz           ! du/dz at layer i+1/2
    real (kind=8) ::    sintheta	   ! A function of leaf distribution factor
    real (kind=8) ::    Re             ! Typical Reynolds number over leaf
    real (kind=8) ::    Cd		       ! leaf drag coef.
    real (kind=8) ::    Cs             ! leaf heat trans. coef. =90(lw)**1/2
    !real (kind=8) ::    a1 ,b1, a2, b2 ! constant used to determine lai prof.
    !
    real (kind=8) :: a1
    real (kind=8) :: b1
    real (kind=8) :: a2
    real (kind=8) :: b2 
    !
    !integer :: i,k,n !,j,m
    integer :: i
    integer :: k
    integer :: n
    !
    !real (kind=8) ::    dz, z, tema, temb, temc, tem !, zz,dzz
    real (kind=8) :: dz
    real (kind=8) :: z
    real (kind=8) :: tema
    real (kind=8) :: temb
    real (kind=8) :: temc
    real (kind=8) :: tem
    !
    ! real    ui,taui,kmi,mixli,di
    real (kind=8), parameter ::  pi = 3.1415926 
    !parameter (pi = 3.1415926)
    ! integer :: nox, nonpos, iwalk, lx
    !
    integer :: nox
    integer :: nonpos
    integer :: iwalk
    integer :: lx
    !
    ! real (kind=8) :: finc, err
    real (kind=8) :: finc
    real (kind=8) :: err

    ! Evandro M Anselmo
    ! Entrada e saida, variaveis do SiB2 (locais, comsibc e data1),
    ! trabalhando com a derive_trans
    !
    !entradas
    real (kind=8) :: g1
    real (kind=8) :: z2
    real (kind=8) :: z1
    real (kind=8) :: chil
    real (kind=8) :: vcover
    real (kind=8) :: kar ! (no SiB2 = vkc)
    real (kind=8) :: vkc
    real (kind=8) :: g   ! (no SiB2 = gx )
    real (kind=8) :: gx  
    real (kind=8) :: cpair
    real (kind=8) :: g4  ! (no SiB2 = ztz0)
    real (kind=8) :: ztz0
    real (kind=8) :: zlt
    real (kind=8) :: rhoair
    !saidas
    real (kind=8) :: ha
    real (kind=8) :: z0d
    real (kind=8) :: dd
    real (kind=8) :: g2
    real (kind=8) :: g3
    real (kind=8) :: cc1
    real (kind=8) :: cc2
    real (kind=8) :: corb1
    real (kind=8) :: corb2

!#######################################################################

!     Include files:

!#######################################################################
    !le apenas as variaveis do SiB2

    
    !include 'SiB2par.inc'       ! SiB2 parameters

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

!     Beginning of executable code...

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


!#######################################################################

!     Import data related with vegetation type and soil types

!#######################################################################

!      CALL SiB2data

!#######################################################################

!     Vegetation type locates in the range of SiB2 ?

!#######################################################################

    ! IF( vtype < 1 .OR. vtype > 9 ) THEN
    !     write(6,*) &
    !     'Vtype is out of SiB classification, vtyp = ', vtype
    !     write(6,*)'Stop in subroutine SiB2DERIVE'
    !     Stop 'in subroutine SiB2DERIVE'
    ! END IF

!#######################################################################

!     Initialize relative parameter from SiB2 database

!#######################################################################

    ! zs    = z0s_cst       !	Ground roughness length  (m)
    ! g4    = g4_cst        ! Transition height factor for mom. transfer
    ! g1    = g1_cst        ! km(actual) : km(log-linear) at z2
    ! z2    = z2_v (vtype)  ! Canopy-top height  (m)
    ! z1    = z1_v (vtype)  ! Canopy-base height  (m)
    ! zc    = zc_v (vtype)  ! Inflection height for leaf-area density(m)
    ! chil  = chil_v(vtype) ! Leaf area distribution factor
    ! leafw = leafw_v(vtype)! Leaf width (m)
    ! leafl = leafl_v(vtype)! Leaf width (m)
    ! vcover= vcover_v(vtype)
    
    !------------------------------------------------------------------
    ! Evandro M Anselmo
    ! Ajustes para inicio dos calculos:
    ! Observe que aqui define-se os valores de zs, zc, leafw (zlw) e
    ! leafl (zlen), variaveis adicionais as variaveis que participam
    ! do SiB2. Também aqui algumas variaveis do SiB2 podem assumir
    ! nomes ou valores diverentes para os calculos da derive_trans
    ! como: lai = zlt, g4 = ztz0, kar = vkc, g = gx. 
    zs = 0.05         ! Ground roughness length  (m)
    zc = 28.0         ! Inflection height for leaf-area density(m)
    leafw = 0.05      ! Leaf width (m) - zlw
    leafl = 0.1       ! Leaf width (m) - zlen
    g4 = ztz0         ! Transition height factor for mom. transfer
    lai = zlt         ! leaf area index
    g = gx
    kar = vkc ! 0.4   ! von karmans constant: SiB2 = 0.41 / derive
                      ! _trans = 0.4
    halay = 0
    !------------------------------------------------------------------
    
!#######################################################################

!     Initialize other constants used

!#######################################################################

    sintheta = (1d0-chil)/pi           ! mean leaf incident
    Re       = (leafw+leafl)/2d0 * 1d0 / 0.15d-04
! Reynolds number over leaf
    Cd       = 1.328d0 * (2d0/sqrt(Re))+0.45d0*sintheta**1.6d0
! Leaf drag coefficient
    Cs       = 90d0 * sqrt (leafw) 	 ! Leaf heat trans. coef.
    b1       = 2d0*lai/(z2-z1)/(zc-z1) ! z < zc lai(z) = a1 + b1*z
    a1       = -b1 * z1				 !
    b2       = 2d0*lai/(z2-z1)/(zc-z2) ! z > zc lai(z) = a2 + b2*z
    a2       = -b2 * z2				 !
          
    dzlay = (z2-z1)/nlay			 ! Thickness in canopy sublayer
    DO i = 1, nlay
        z = z1 + (i-0.5d0) * dzlay
        IF (z > zc) THEN
            laid (i)  = a2 + b2 * z
        ELSE
            laid (i) = a1 + b1 * z
        END IF
        ps   (i)  = 1d0 + (laid (i)/vcover) ** 0.6d0 ! Sheltor factor
    END DO

!#######################################################################

!     Give u* at canopy base, and set relevant variables at canopy base

!#######################################################################

    ustr1   = 0.1d0					   ! u* at canopy base
    if ((z1/zs) < 0.0d0) then
        print *, "derive_trans 001:1",z1/zs,z1,zs
    endif
    u1   	=ustr1*log(z1/zs)/kar	   ! vel. at canopy base
    u(1)	= u1					   !
    tau(1)  =ustr1 * ustr1  		   ! shear stress at canopy base
    dzero(1)=0						   ! Local zero-displacement
    Km(1)   = kar * (z1-dzero(1)) * sqrt(tau(1)) ! Mixing coef.
    raint(1)=0d0       				   ! integration to ra

!#######################################################################

!     Initialize newton-raphson iterative routine

!#######################################################################

    nox    = 0
    nonpos = 1
    iwalk  = 0
    lx     = 1
    finc   = 0.1d0

    DO WHILE (nox /= 1 )
    
    !#######################################################################
    
    !     Calculate velocity in canopy layer through three equation
    !     d(tau)/dz= Cd*Laid*u*u/ps
    !	du/dz    = tau/km
    !     km  = k*G1*(z-dlocal),
    !     dlocal is a local displacement defined as
    !     d = int{d(tau)/dz * z dz}| [z1,z] /	tau (z), z1<z<z2
    
    !#######################################################################
    
        DO i = 1, nlay
            z = z1 + i*dzlay
            dudz1 = tau (i) / Km(i)
            dudz2 = dudz1
            tem =  Cd*Laid(i)/ps(i)
            temc =  G1
            IF(z < zc)temc =  1+(G1-1)*(z-z1)/(zc-z1)
            dzero (i+1) = dzero (i)
            tau (i+1) =tau(i)
            k = 1
            DO WHILE (k < 5 )
                dudz = 0.5d0 * (dudz1 + dudz2)
                u(i+1) = u(i) + dudz * dzlay
                tema   = (u(i+1)+u(i))/2             ! average vel.
                temb   = z-0.5d0*dzlay                 ! average level
                tau(i+1)= tau(i)+tem * tema**2*dzlay
                dzero (i+1) = (dzero(i)*tau (i) + &
                tem*tema**2d0*temb*dzlay)/tau(i+1)
                Km(i+1) = Kar * temc * (z-dzero(i+1))*sqrt(tau(i+1))
                dudz2 = tau(i+1)/Km(i+1)
                k = k+1
            END DO
            raint(i+1) = raint (i) &
            + laid(i)*sqrt(tema)/ps(i)/Cs*dzlay
        END DO
    
    !#######################################################################
    
    !     Calculate z0 from Kondo (1994) p227
    
    !#######################################################################
    
        ustr2 = sqrt(tau(nlay+1))
        u2derivetrans = u(nlay+1)
        dd    = dzero(nlay+1)

        tem = lai/(z2-z1)
        tem = 1+tem**0.6d0
        tem = exp(-Cd/tem*lai/2/kar**2)
        if ((z2/zs) <= 0.0d0) then
            print *, "derive_trans 002:2",z2/zs,z2,zs
        endif
        tem = (1d0-tem+log(z2/zs)**(-1d0/0.45d0)*tem**2d0)**0.45d0
        tem = 1d0/tem
        z0d = (z2-dd)/exp(tem)
    
    !#######################################################################
    
    !     Calculate velocity at transitional layer top through integration
    !     from z2 to zt
    !          rat  = int{1/km*dz}| [z2, zt]
    !     =>   ut   = u2 + ustr2**2 * rat
    !     =>   ustr = k*ut/log((zt-dd)/z0)
    !     If(ustr.ne.ustr2) Adjust z0
    
    !#######################################################################
    
    !      Integrate mom. trans. resistnce in transitional layer
    
    !#######################################################################
    
        zt  = z2 + z0d * g4    ! zt is transitional layer top
        n   = int((zt-z2) / (0.01d0 * z0d)) ! Evandro uso da int()
        dz  = (zt-z2) / n
        rat = 0d0
        DO i = 1, n
            z = zt - dz * (i-0.5d0)
            kmt = kar * ustr2 * (z-dd) * (1d0 + (g1-1) * (zt-z) / (zt-z2) )
            rat = rat + 1d0/kmt * dz
        END DO

        ut = u2derivetrans + ustr2 * ustr2 * rat
        	    
        if (((zt-dd) / z0d) <= 0.0d0) then
            print *, "derive_trans 003:3",(zt-dd) / z0d,zt,dd,z0d
        endif
        ustr = kar*ut/ LOG ((zt-dd) / z0d)
        	
    !#######################################################################
    
    !     Adjust roughness length z0d according to velocity at zt
    
    !#######################################################################
    
    
        err = (ustr2-ustr)
        CALL newton( G1 , err, finc , nox, nonpos, iwalk, lx)
            !write(*,*)ustr2,ustr,ustr2-ustr,g1,nox
        z0d = (zt-dd)*exp(-(kar*ut)/ustr2)

    END DO
     
!      write(*,*)'Finish the calculation of velocity, shear stress,'
!	write(*,*)'zero-displacement in canopy	and roughness length.'


!#######################################################################

!     Calculate canopy source height of heat transfer

!#######################################################################

    DO i = 1, nlay
       IF(raint(i) >= 0.5d0*raint(nlay+1))THEN
          halay = i
          ha    = z1+(halay-1d0)*dzlay
          goto 44774
       END IF
!       print *, halay
    END DO
    44774 continue

!      write(*,*)'Finish source height calculation'

!#######################################################################

!     Calculate various resistance and coefficient

!#######################################################################


!#######################################################################

!     Calculate boundary bulk heat resistance in canopy layer and C1

!#######################################################################

    rbderivetrans = 0d0
    DO i = 1, nlay
        z = z1 + dzlay * (i-0.5d0)
        tem  =  Laid(i)/(ps(i)*Cs)
        rbderivetrans   = rbderivetrans + tem * sqrt ((u(i)+u(i+1))/2d0) * dzlay
    END DO
    rbderivetrans  = 1d0/rbderivetrans
    cc1 = rbderivetrans * sqrt (u2derivetrans)

!#######################################################################

!     Calculate heat resistance from ground to source height and C2

!#######################################################################


    rdderivetrans =  u(1)/ (ustr1 * ustr1)
    DO i = 1, halay
        z = z1 + dzlay * (i-0.5d0)

        temc =  G1
        IF(z < zc)temc =  1d0+(G1-1d0)*(z-z1)/(zc-z1)
                 
        ksc = kar * temc * sqrt(tau(i))* (z-dzero(i))
        ksc = (ksc + kar * temc * sqrt(tau(i+1))* (z-dzero(i+1)) )/2d0
        rdderivetrans = rdderivetrans + 1d0/ksc * dzlay
    END DO
    cc2 = rdderivetrans * u2derivetrans

!#######################################################################

!     Calculate heat resistance from source height to canopy top

!#######################################################################

    dudz  = (1d0-u(halay)/u2derivetrans)/(z2-ha)
    corb1 = 9d0*g/( rhoair*cpair* dudz**2d0 )
    raderivetrans    =  0
    DO i = halay, nlay
        z = z1 + dzlay * (i+0.5d0)

        temc =  G1
        IF(z < zc)temc =  1d0+(G1-1d0)*(z-z1)/(zc-z1)

        ksc = kar * temc * sqrt(tau(i))* (z-dzero(i))
        ksc = (ksc + kar * temc * sqrt(tau(i+1))* (z-dzero(i+1)))/2d0
        raderivetrans = raderivetrans + 1d0/ksc * dzlay
    END DO
    corb2 = (raderivetrans * u2derivetrans)**2d0

!#######################################################################

!     Calculate heat resistance from canopy top to zt, and g2, g3

!#######################################################################

!      real  g2    ! ratio of ra(actual) to ra(log-linear) for momentum
!                  ! between: z = z2 and z = zx, where zx = min(zl,zwind)
!      real  g3    ! ratio of ra(actual) to ra(log-linear) for heat
!                  ! between: z = z2 and z = zx, where zx = min(zl,zmet)
    n   = int((zt-z2) / (0.01d0 * z0d)) !Evandro, uso da int()
    dz  = (zt-z2) / n
    rat = 0d0
    DO i = 1, n
        z = zt - dz * (i-0.5d0)
        kmt = kar * ustr2 * (z-dd) * (1d0 + (G1-1d0) * (zt-z) / (zt-z2) )
        rat = rat + 1d0/kmt * dz
    END DO

    if (((z2-dd) / z0d) <= 0.0d0) then
        print *, "derive_trans 004:4",(z2-dd) / z0d,z2,dd,z0d
    endif

    tem= ustr2*log((z2-dd)/z0d)/kar
    g2 = rat / ((ut-tem)/ustr2**2d0)
    g3 = g2

    RETURN
    END !SUBROUTINE derive_trans


