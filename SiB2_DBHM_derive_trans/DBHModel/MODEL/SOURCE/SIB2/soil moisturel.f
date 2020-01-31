c   this is an example
c
C#######################################################################
c      
c    input      
c
C#######################################################################
      ! grid information
      integer nlvl         ! number of boundary levels of soil layers 
      integer nlay         ! number of soil layers (nlay = nlvl-1)
      parameter (nlvl = 21, nlay = 20)

      real    zlvl  (nlvl) ! Boundary levels of each computational layer (m)
      real    zlay  (nlay) ! Mid-level of soil layer (m)

      ! time step
      real    dtime

      ! soil properties, soil-type dependent, input by user
      real    wsat     ! saturation point of water content (m3/m3)
      real    psat     ! saturated matric potential (m)
      real    khsat    ! saturated hydraulic conductivity (m s-1)
      real    bslope   ! slope of potential curve (dimensionles)

      ! slope of surface
      real    slope    ! dimensionless

      ! initial condition and top B. C.
      real    Wsoil0(nlay) ! input soil moisture (m3/m3)
      real    flux0        ! infiltration rate, or minus evaporative flux 
                           ! from the surface (m/(m2.s))
                           ! note: the unit is equivalent to 1000 kg/(m2 s)
                           ! evaporation from bulk method is kg/(m2 s)
                           ! Dr. Gao Zhiqiu may have made a mistake here.           
c
C#######################################################################
c      
c      output
c
C#######################################################################
      real    Wsoil (nlay) ! output soil moisture of each layer (m3/m3)
c
C#######################################################################
c      
c     temporary
c
C#######################################################################
      real    dpdwi (nlvl) ! dpsi/dw 
      real    flux  (nlvl) ! soil moisture flux (m/s)
      real    khav         ! average moisture diffusivity (m3/m3)

      real  TA(nlvl),TB(nlvl),TC(nlvl),TD(nlvl),TP(nlvl),TQ(nlvl)  

      DO i = 1, nlvl
         zlvl (i) = (i-1) * 0.02
      END DO
      DO i = 1, nlay
         zlay (i) = 0.5*(zlvl(i+1)+zlvl(i))
      END DO

      dtime = 600

      wsat  = 0.4
      psat  = -0.1
      khsat = 1.0e-5
      bslope= 4.0   
      slope = 0.01

      DO i = 1, nlay
         wsoil0 (i) = 0.2
      END DO

	print *,wsoil0
	print *,'Begin'
      flux0 = -0.003 / 86400   ! 3mm evaporation per day

      DO i = 1, 86400/dtime
         
	CALL moisture(nlvl,nlay,zlvl,zlay,dtime,
     :                    wsat,psat,khsat,bslope,slope,
     :                    Wsoil,Wsoil0,flux0,
     :                    dpdwi,flux,TA,TB,TC,TD,TP,TQ)
      wsoil0 = wsoil 
	print *,wsoil0
	print *,'tq'
	print *,tq
	pause
      END DO


      END 
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE MOISTURE                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######      River and Environmental Engineering Lab.        ######
c     ######              The University of Tokyo                 ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
	SUBROUTINE moisture(nlvl,nlay,zlvl,zlay,dtime,
     :                    wsat,psat,khsat,bslope,slope,
     :                    Wsoil,Wsoil0,flux0,
     :                    dpdwi,flux,TA,TB,TC,TD,TP,TQ)
c
C#######################################################################
c
c     PURPOSE:
c
c     solve soil moisture with input infiltration rate as top 
c     boundary condition and gravity lateral flow as lower 
c     boundary condition
c
C#######################################################################
c      
c     AUTHOR: K. Yang
c     09/04/2002  
c
C#######################################################################

      implicit none

c
C#######################################################################
c      
c    input      
c
C#######################################################################
      ! grid information
      integer nlvl         ! number of boundary levels of soil layers 
      integer nlay         ! number of soil layers (nlay = nlvl-1)
      real    zlvl  (nlvl) ! Boundary levels of each computational layer (m)
      real    zlay  (nlay) ! Mid-level of soil layer (m)

      ! time step
      real    dtime

      ! soil properties, soil-type dependent, input by user
      real    wsat     ! saturation point of water content (m3/m3)
      real    psat     ! saturated matric potential (m)
      real    khsat    ! saturated hydraulic conductivity (m s-1)
      real    bslope   ! slope of potential curve (dimensionles)

      ! slope of surface
      real    slope    ! dimensionless

      ! initial condition and top B. C.
      real    Wsoil0(nlay) ! input soil moisture (m3/m3)
      real    flux0        ! infiltration rate, or minus evaporative flux 
                           ! from the surface (m/(m2.s))
                           ! note: the unit is equivalent to 1000 kg/(m2 s)
                           ! evaporation from bulk method is kg/(m2 s)
                           ! Dr. Gao Zhiqiu may have made a mistake here.           
c
C#######################################################################
c      
c      output
c
C#######################################################################
      real    Wsoil (nlay) ! output soil moisture of each layer (m3/m3)
c
C#######################################################################
c      
c     temporary
c
C#######################################################################
      real    dpdwi (nlvl) ! dpsi/dw 
      real    flux  (nlvl) ! soil moisture flux (m/s)
      real    khav         ! average moisture diffusivity (m3/m3)

      real    psi          ! matric potential(Pa)
      real    kh           ! soil hydraulic conductivity
      real    dpdw         ! dpsi / dw

      real deficit, excess 
      real qmin, qmax       
      real sum
      real tema
      real  TA(nlvl),TB(nlvl),TC(nlvl),TD(nlvl),TP(nlvl),TQ(nlvl)  
      integer k
      real eps
      data eps/1.0e-2/
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
c    ! calculate d(psi)/dw
      DO k=1,nlay
            dpdwi(k) = dpdw(wsoil0(k),wsat,psat,bslope)
            dpdwi(k) = min(1.0E+5,max(eps,dpdwi(k)))
      END DO
c    ! calculate matric potential at last step
      DO k=1,nlay
         TQ(k) = psi(wsoil0(k),wsat,psat,bslope)
      END DO 

c    ! calculate averaged hydraulic conductivity 
      DO k=1,nlvl

         IF(k.eq.1)THEN
            TP(k)   =kh(wsoil0(k),wsat,khsat,bslope)
         ELSE IF(k.eq.nlvl)THEN
            TP(k)   =kh(wsoil0(nlay),wsat,khsat,bslope)
         ELSE
            tema = (wsoil0(k-1)*(zlvl(k)-zlay(k-1))
     :           + wsoil0(k)*(zlay(k)-zlvl(k)))/(zlay(k)-zlay(k-1))
            TP(k)   =kh(tema,wsat,khsat,bslope)
         END IF
      END DO

c	AT(j)=B*T(j+1)+C*T(j-1)+D

c     set top boundary: input surface infilatration rate (unit: m/(m2.s))
      flux(1)    = flux0
      k = 1
      TA(k)=1.0
      TB(k)=0.0
      TC(k)=0.0
      TD(k)=flux(1)

c     set lower boundary: gravity lateral flow 
      k = nlvl
      TA(k)=1.0
      TB(k)=0.0
      TC(k)=0.0
      TD(k)=slope*khsat*(wsoil0(nlay)/wsat)**(2*bslope+3)

c     solve water diffusion equation

      DO k=2,nlvl-1

         khav   =TP(k)

         TA(k)=1+dtime*khav/(zlay(k)-zlay(k-1))*
     :         (dpdwi(k)  /(zlvl(k+1)-zlvl(k)) + 
     :          dpdwi(k-1)/(zlvl(k)-zlvl(k-1)))
         TB(k)=dtime*khav/(zlay(k)-zlay(k-1))*
     :         dpdwi(k)  /(zlvl(k+1)-zlvl(k))
         TC(k)=dtime*khav/(zlay(k)-zlay(k-1))*
     :         dpdwi(k-1)  /(zlvl(k)-zlvl(k-1))
         TD(k)=khav*( 1+(TQ(k-1) -TQ(k))
     :          /(zlay(k)-zlay(k-1)) )
      END DO

      TP = 0
      TQ = 0 
      CALL TDMA(flux,TA,TB,TC,TD,TP,TQ,nlvl)	

c     adjust water flux to prevent from over-drying

      DO k = 2, nlvl-1                                                          
        qmax   =  wsoil0(k-1)*(zlvl(k)-zlvl(k-1)) /dtime                              
        qmin   = -wsoil0(k)*(zlvl(k+1)-zlvl(k)) /dtime
        flux(k)= amin1( flux(k),qmax)                                              
        flux(k)= amax1( flux(k),qmin)                                              

      END DO

c     calculate water content in each soil layer

      DO k = 1, nlay                                                          
        wsoil(k) =   wsoil0(k)+(flux(k)- flux(k+1))*dtime
     :               /(zlvl(k+1)-zlvl(k))                    
      END DO
c
C#######################################################################
c
c     prevent over-saturation
c
C#######################################################################
c
      excess = 0.0                                                                               
      DO k = nlay,1,-1                                                          
        wsoil(k) = wsoil(k) + excess / (zlvl(k+1)-zlvl(k))                                                  
        excess   = amax1(0.,(wsoil(k) - wsat))* (zlvl(k+1)-zlvl(k))                                         
        wsoil(k) = min(wsat,wsoil(k))
      END DO
c
C#######################################################################
c
c     prevent negative values of wsoil(k)                                         
c
C#######################################################################
c
      DO k = 1,nlay-2                                                           
        deficit   = amax1 (0.,(0.01-wsoil(k)))*(zlvl(k+1)-zlvl(k))                                  
        wsoil(k)  = wsoil(k)  + deficit/(zlvl(k+1)-zlvl(k))                                              
        wsoil(k+1)= wsoil(k+1)- deficit/(zlvl(k+2)-zlvl(k+1))                 
      END DO
c
C#######################################################################
c
c     Check water balance
c
C#######################################################################
c
      sum = (flux(1)-flux(nlvl))*dtime - excess
      DO k=1, nlay
         sum = sum-(zlvl(k+1)-zlvl(k))*(wsoil(k)-wsoil0(k))
      END DO
      IF(sum.gt.1.0e-6)THEN  

         write(*,*)'water imbalance occured in subroutine moisture',
     :             'imbalance = ', sum
         write(*,*)wsoil0 
         write(*,*)wsoil 
         write(*,*)flux 
         write(*,*)TP 
         write(*,*)dpdwi 
         write(*,*)wsat 

         write(*,*)khsat 
         write(*,*)psat 
         write(*,*)bslope 
c     
c         stop 'stop in moisture!'
      END IF                                                           
      DO k=1, nlay
         wsoil(k) = amin1(wsoil(k),wsat)
      END DO
c
C#######################################################################
c
c     Because the above adjustment, the actual water flux was changed
c
C#######################################################################
c
      flux(nlvl)   = flux(nlvl)
      flux(nlvl-1) = flux(nlvl-1)
      DO k = nlvl-2,1,-1
         flux(k) = flux(k+1) +
     :               (zlvl(k+1)-zlvl(k))*(wsoil(k)-wsoil0(k))/dtime 
      END DO 

	END	SUBROUTINE moisture


      REAL FUNCTION psi(w,wsat,psat,b)
      implicit none 
      real w,wsat,psat,b
      real se,sei,psii,m,n
      
       
      se   = min(1.0,max(0.01,w/wsat))
      sei = (b+0.1)/(b+1)
      IF(se.gt.sei)THEN
        psii = psat*sei**(-b) 
        m    = psii/(1-sei)**2-psii*b/(sei*(1-sei))
        n    = 2*sei-(psii*b/(m*sei))-1
        psi = -m*(se-n)*(se-1)
      ELSE 
        psi = psat*se**(-b)
      END IF
      END         


      REAL FUNCTION dpdw(w,wsat,psat,b)
      implicit none 
      real w,wsat,psat,b
      real se
      
      se   = min(1.0,max(0.01,w/wsat))
      dpdw = -b*psat/(se*wsat)*se**(-b)

      END         

      REAL FUNCTION kh(w,wsat,khsat,b)
      implicit none
      real w,wsat,khsat,b
      real se

      se   = min(1.0,max(0.01,w/wsat))
      kh = khsat * se**(2*b+3)
      kh = max(kh,1.0E-30)
      END

c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE TDMA                       ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######      River and Environmental Engineering Lab.        ######
c     ######              The University of Tokyo                 ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE TDMA(T,A,B,C,D,P,Q,M)
      implicit none 
c
C#######################################################################
c
c     PURPOSE:
c
c     solve a triangle matrix equation system
c	AT(j)=B*T(j+1)+C*T(j-1)+D
c
C#######################################################################
c      
c     AUTHOR: K. Yang
c     09/04/2002  
c
C#######################################################################
      INTEGER M 

     	REAL A(M),B(M),C(M),D(M),P(M),Q(M),T(M)
      REAL tema  
      integer i,j
c
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C     Beginning of executable code...
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
     	P(1)=B(1)/A(1)
     	Q(1)=D(1)/A(1)
     	DO I=2,M
         tema = A(I)-C(I)*P(I-1) 
     	   IF(ABS(tema).LT.1.0E-20)THEN
     		  WRITE(*,*)'SUBROUTINE TDMA ERROR, i=',i
            DO j = 1, M
               WRITE(*,'(i3,4(E11.4,1x))')j,A(j),B(j),C(j),D(j)
            END DO
     	      STOP
     	   END IF

     	   P(I)=B(I)/tema
     	   Q(I)=(D(I)+C(I)*Q(I-1))/tema
     	END DO
     	T(M)=Q(M)       
     	DO I=M-1,1,-1
     	    T(I)=P(I)*T(I+1)+Q(I)
     	END DO
      END SUBROUTINE TDMA    
