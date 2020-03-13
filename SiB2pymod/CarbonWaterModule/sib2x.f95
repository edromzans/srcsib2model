!=======================================================================
!     SiB2 como um modulo de python - SiB2pymod - via f2py
!
      subroutine sib2(gradm_param, gmudmu_param, greeness_param, vmax_param, &
           nlinha, h_out, le_out)

!                                                                       
!=======================================================================
!                                                                       
!...  SiB driver offline version, forced by meteorological data         
!                                                                       
!	core of routines for sib2 model                                       
!                                                                       
!	**** to use SiB2 in stand-by mode ****                                
!                                                                       
!=======================================================================
!                                                                       
!                                                                       
!     last revision:          dec. 15, 1993                             
!                                                                       
!     P. Sellers, J. Collatz, L. Bounoua                                
!                                                                       
!                                                                       
!     last update:      04 fev 97   Humberto Rocha                      
!	soil multi-layer model                                               
!	                                                                      
!=======================================================================
!                                                                       
!     subroutines called from this segment  :    veginc                 
!                                                cntrol                 
!                                                const2                 
!                                                driver                 
!                                                balan                  
!                                                inter2                 
!                                                rada2                  
!                                                begtem                 
!                                                endtem                 
!                                                updat2                 
!                                                balan                  
!                                                                       
!-----------------------------------------------------------------------
!     ipbl = 0 : dtc, dtg only     ; as per SE-86                       
!     ipbl = 1 : dtc, dtg, dth, dqm; as per Randall-Sellers, SA-89b     
!                                                                       
!     isnow= 0 : conventional run using amazon forcings.                
!     isnow= 1 : snow shock test, warming to normal over 5 days.        
!-----------------------------------------------------------------------
!...                                                                    
      use pardif 
      use comsibc
      implicit none
!...  variables only important to the optimization module                
      integer :: itero=0
      integer :: ipbl=0
      integer :: isnow=0
      integer :: maxit
      ! data ichi, icho, iu /1,6,8/ 
      integer :: ichi=1
      integer :: icho=6
      integer :: iu=8
      !  data itmp1/88/,itmp2/78/,itmp3/79/,itmp4/80/,itmp5/81/,           &
      ! &     itero/0/, ipbl/0/, isnow/0/
      integer :: nyfirst
      integer :: nylast
      real (kind=8) :: totwb
      !Variaveis para SiB2pymod:---------------------------------------------
      ! entrada via python      
      integer, intent(in) :: nlinha
      real (kind=8), intent(in) :: gradm_param
      real (kind=8), intent(in) :: gmudmu_param
      real (kind=8), intent(in) :: greeness_param
      real (kind=8), intent(in) :: vmax_param
      ! saida
      real (kind=8), intent(out) :: h_out(nlinha)    ! vout(nlinha)
      real (kind=8), intent(out) :: le_out(nlinha)    ! vout(nlinha)
      real (kind=8) :: elat

      !----------------------------------------------------------------------
      itmp1=88
      itmp2=78
      itmp3=79
      itmp4=80
      itmp5=81
      !
      h_out = -99999.
      le_out = -99999.
      itero = 0    ! Deve zerar a cada execucao do modulo SiB2pymod
      !
!...  output files opening                                               
      !open(98,file='sib2diag.dat',status='unknown') 
      !open(itmp1,file='sib2dt.dat',status='unknown')	 
      !open(itmp2,file='sib2dt_SM.dat',status='unknown') !umidade do solo
!...
!     parameters file  
      open(ichi, file='data1', status='old') 
!
!      write(98,'(20a11)')                                              
!     & 'xs','P','D','Pinf','Dc','Dd','Ri','R','Pi','q0'                
!
      call veginc(ichi)
      !
      !recebe os parametros do processo de otimizacao---------------------
      gradm = gradm_param
      gmudmu = gmudmu_param
      !-------------------------------------------------------------------
      !      
      call cntrol(ichi,icho,maxit,nylast,nyfirst) 
!... simulation start: at nyfirst (data1)                               
!... simulation end  : at maxit or nylast (most limiting out of two)    
      do 1000 iter = 1, maxit 
         !write( *,'(a20,2i10)')'iter nymd:',iter,nymd                     
         call const2 
         !write(98,'(A20)')'  to call driver'                             
         call driver (iu, icho, isnow, ichi, itero, nyfirst, &
              greeness_param, vmax_param)
         ! if (mod(iter,int(86400./dtt)).eq.0) then ! display screen cada dia	 
         !    ! write( *,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd 
         !    ! write(98,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd 
         ! endif
         
         !if (mod(iter,365*int(86400./dtt)).eq.0) then  !display screen cada ano
         !write( *,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd     
         !write(98,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd     
         !endif                                                            
         !!display screen cada passo de tempo                                
         !write(98,'(a20,i5,i12)')'  iter  nymd:',iter,nymd                
         !write(*,'(a20,i5,i12)')'  iter nymd:',iter,nymd                  
         !write(98,'(A20)')'   passed driver'                             
         
         if (nymd.gt.nylast) goto 1001 

         !write(98,'(A20)')'  to call optima'                                   
         !if (lopt) call optima (xx, mo, no, ff, iqc, itero)                    
         !write(98,'(A20)')'  passed optima'                                    
         !write(98,'(A20)')'  to call balan'                              
         call balan( 1, totwb ) 
         !write(98,'(A20)')'   passed balan'                              
         !print*,maxit                                                     
         !write(98,'(A20)')'  to call inter2'                             
         call inter2 
         !write(98,'(A20)')'   passed inter2'                             
         !write(98,'(A20)')'  to call rada2'                              
         call rada2 
         !write(98,'(A20)')'   passed rada2'                              
         !write(98,'(A20)')'  to call begtem'                             
         call begtem 
         !write(98,'(A20)')'   passed begtem'                             
         !write(98,'(A20)')'  to call endtem'                             
         call endtem (ipbl) 
         !write(98,'(A20)')'   passed endtem'                             
         !write(98,'(A20)')'  to call updat2'                             
         call updat2 
         !write(98,'(A20)')'   passed updat2'                             
         !write(98,'(A20)')'  to call balan'                              
         call balan( 2, totwb ) 
         !write(98,'(A20)')'   passed balan'                              
         !write(98,'(A20)')'  to call outer'                              
         call outer(elat) ! modificada para modulos de carbono e agua
         
         !--------------------------------------
         !Saida do SiB2pymod
         !use comsibc
         h_out(itero) = hflux
         le_out(itero) = elat
         !--------------------------------------
         
         !write(98,'(A20)')'   passed outer'                              
         if (nymd.eq.nylast) goto 1001 
 1000 continue 
!---------------------------------------------------------------------- 
 1001 continue 
                                                                        
      close(ichi) 
      close(iu) 
      !close(itmp1) 
      !close(itmp2) 
!H     close(itmp3)                                                    
!H     close(itmp4)                                                    
!H     close(itmp5)                                                    
      !close(98) 
!                                                                       
      !stop ' SiB2 HAS DONE. ' ! SiB2pymod nao pode parar
      END                                           
!                                                                       
!=======================================================================
!                                                                       
!   SUBROUTINES                                                         
!                                                                       
!=======================================================================
      subroutine outer(elat)  
!---------------------------------------------------------------------- 
! writes out selected variables (unformatted) select in data1 namelist  
!---------------------------------------------------------------------- 
      use comsibc
      implicit none
      !dimension  rvar(170)
      real (kind=8) :: rvar(170)
      !
      !integer :: itero
      real (kind=8) :: calbe
      real (kind=8) :: canil
      real (kind=8) :: cgsto
      real (kind=8) :: elat
      real (kind=8) :: evapg
      real (kind=8) :: fco2
      real (kind=8) :: gcstor
      !integer :: i
      integer :: in
      real (kind=8) :: radswa
      real (kind=8) :: radswd
      real (kind=8) :: radtot
      real (kind=8) :: tc4
      real (kind=8) :: tg4
      real (kind=8) :: trant
      real (kind=8) :: zlwf    
!...                                                                    
      trant  = ect/hlat 
      canil  = eci/hlat 
      evapg  = (egs+egi)/hlat 
      radswd = radn(1,1) + radn(1,2) + radn(2,1) + radn(2,2) 
      radswa = (1.-salb(1,1))*radn(1,1) + (1.-salb(1,2))*radn(1,2)      &
     &       + (1.-salb(2,1))*radn(2,1) + (1.-salb(2,2))*radn(2,2)      
      calbe = 1. - (radswa/radswd) 
      cgsto = shf - gflux 
                                                                        
!..   fco2 = co2 flux in umol m-2 s-1                                   
      fco2    = (respg - assimn) * 1.e06                   !Rs calculado
      
      if (xmrsco2.ne.-9999.) fco2 = xmrsco2 - assimn*1.e06 !Rs observado
                                                                        
      radtot = radt(1) + radt(2) 
      elat   = etmass / dtt * hlat 
      gcstor = chf + shf 
!                                                                       
      tgs  = amin1(tf,tg)*areas + tg*(1.-areas) 
      tc4  = tc  * tc  * tc  * tc 
      tg4  = tgs * tgs * tgs * tgs 
      zlwf =  tc4 * stefan * vcover * ( 1. - thermk )                   &
     &     + (1. - vcover * ( 1. - thermk ) ) * stefan * tg4            
!                                                                       
      tgeff= sqrt ( sqrt ( zlwf/stefan ) ) 
!                                                                        
!=======================================================================
!... list of all variables to write out                                 
!=======================================================================
!... water balance                                                      
!1   elat            latent heat flux (Wm-2)                            
!2   ect/dtt         canopy transpiration (Wm-2)                        
!3   eci/dtt         canopy interception loss (Wm-2)                    
!4   egs/dtt         soil evaporation (Wm-2)                            
!5   egi/dtt         ground cover interception loss (Wm-2)              
!6   q0              top soil infiltration flux (ms-1)                  
!7   qng             deep drainage flux (ms-1)                          
!8   croff           surface runoff (m)                                 
!9   roff            total runoff (m)                                   
!...	sensible heat balance                                              
!10  hflux           sensible heat flux (Wm-2)                          
!11  hc/dtt          canopy sensible heat flux (Wm-2)                   
!12  hg/dtt          ground cover sensible heat flux (Wm-2)             
!13  chf             canopy storage energy (Wm-2)                       
!14  cgsto           ground storage energy (Wm-2)                       
!15  gflux           soil heat flux (Wm-2)                              
!16  heaten/dtt      snowmelt heat loss (Wm-2)                          
!...    radiation balance ..............................................
!17  radtot          calculated net radiation (Wm-2)                    
!18  zlwf            long wave upward radiation (Wm-2)                  
!19  calbe           surface albedo (dimensionless)                     
!20  tgeff           radiative surface temperature (K)                  
!21  td              deep soil temperature (K)                          
!22  radt(1)         canopy net radiation (Wm-2)                        
!23  radt(2)         ground cover net radiation (Wm-2)                  
!24  radn(1,1)       direct par incoming radiation (Wm-2)               
!25  radn(1,2)       difuse par incoming radiation (Wm-2)               
!26  radn(2,1)       direct nir incoming radiation (Wm-2)               
!27  radn(2,2)       direct nir incoming radiation (Wm-2)               
!28  radn(3,2)       tir incoming radiation (Wm-2)                      
!29  salb(1,1)       direct par albedo                                  
!30  salb(1,2)       difuse par albedo                                  
!31  salb(2,1)       direct nir albedo                                  
!32  salb(2,2)       direct nir albedo                                  
!...  carbon balance                                                    
!33  assimn          net assimilation (mimol m-2 s-1)                   
!34  fco2            net ecosystem exchange (mimol m-2 s-1)             
!35  respg           soil respiration (mimol m-2 s-1)                   
!36  respc           canopy (plant) respiration (mimol m-2 s-1)         
!37  pco2a           co2 atmospheric partial pressure (Pa)              
!... conductance calculation variables                                  
!38  rst             canopy (leaf) conductance (m s-1)                  
!39  gsh2o           canopy (leaf) conductance (molm-2s-1)              
!40  rstfac(1)       conductance stress factor vpd                      
!41  rstfac(2)       conductance stress factor soil moisture            
!42  rstfac(3)       conductance stress factor temperature              
!43  rstfac(4)       total conductance stress factor                    
!...  other calculated variables (FILL IT IF NECESSARY)                 
!44                  blank                                              
!45                  blank                                              
!46                  blank                                              
!47                  blank                                              
!48                  blank                                              
!49                  blank                                              
!50                  blank                                              
!...    observed (forcing or not) variables                             
!51  swdown          downward shortwave radiation (Wm-2)                
!52  rnetm           net radiation (Wm-2)                               
!53  em              water vapour pressure (HPa)                        
!54  tm              surface air temperature (K)                        
!55  um              horizontal wind speed (ms-1)                       
!56  udm             surface wind direction (degree)                    
!57  ppl+ppc         total precipitation (mm)                           
!58  pco2m           atmospheric co2 concentration (Pa)                 
!59  xmevap          observed latent heat flux (Wm-2)                   
!60 xmsensh          observed sensible heat flux (Wm-2)                 
!61 xmgheat           observed soil heat flux (Wm-2)                    
!62 xmco2           observed co2 flux (Wm-2)                            
!63 xalbedo          observed surface albedo                            
!...  other observed variables (FILL IT IF NECESSARY)                   
!64                  blank                                              
!65                  blank                                              
!66 ustar            calculated friction velocity (ms-1)                
!67                  blank                                              
!68                  blank                                              
!69                  blank                                              
!70                  blank                                              
!.. soil variables (up to nlayer = 50 MAXIMUM)                          
!71..120     qqq(i) interlayer i to i+1 water flux (m s-1) (+ downward) 
!121..170    www(i) soil wetness at layer i                             
!------------------------------------------------------------------     
!	variables atribution                                                  
!------------------------------------------------------------------     
!  700 continue 
!                                                                        
      rvar(1) = elat 
      rvar(2) = ect / hlat            !mm
      rvar(3) = eci / hlat            !mm
      rvar(4) = egs / hlat            !mm
      rvar(5) = egi / hlat            !mm
      rvar(6) = q0  * 1000. * dtt     !mm
      rvar(7) = qng * 1000. * dtt     !mm
      rvar(8) = croff * 1000.         !mm
      rvar(9) = roff * 1000.          !mm
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
      rvar(39) = gsh2o * 18. ! stomat conductance mm /s               
      rvar(40) = rstfac(1) 
      rvar(41) = rstfac(2) 
      rvar(42) = rstfac(3) 
      rvar(43) = rstfac(4) 
!                                                                       
      rvar(44) = rvar(3) + rvar(5) ! total interception loss
      rvar(45) = rvar(13) + rvar(14) ! column energy storage
!      rvar(46) = rvar(1)/hlat*dtt ! Evptrans (mm)                      
      rvar(46) = rvar(2)+rvar(3)+rvar(4)+rvar(5) ! Evptrans (mm) 
!      rvar(47) = blank                                                  
!      rvar(48) = blank                                                  
!      rvar(49) = blank                                                  
!      rvar(50) = blank                                                  
!                                                                       
      rvar(51) = swdown 
      rvar(52) = rnetm ! Rn observada (for Ld calculatio 
      rvar(53) = em 
      rvar(54) = tm -273.15 ! Celsius		 
      rvar(55) = um 
      rvar(56) = udm 
      rvar(57) = ppl+ppc 
      rvar(58) = pco2m 
      rvar(59) = xmevap 
      rvar(60) = xmsensh 
      rvar(61) = xmgheat 
      rvar(62) = xmfco2 
      rvar(63) = xalbedo 
!                                                                       
      rvar(64) = xmrsco2 
      rvar(65) = rvar(35) + rvar(36) ! Rsc calculado 
      rvar(66) = ustar 
      rvar(67) = xmustar 
!      rvar(69) = blank                                                      
!      rvar(70) = blank                                                      
      do 40 in=1,nlayer 
         rvar(70+in) = qqq(in) 
   40    rvar(120+in) = www(in) 
!                                                                        
!--------------------------------------------------------------------   
!	write out variables                                                   
!--------------------------------------------------------------------   
!                                                                        
      ! if (itero.eq.1) then                             ! write labels 
      !    write(itmp1,'(a8,100(1x,a9))')'NYMD',                            &
      !    'Tm', 'em','um',                                                 &
      !    'Ki','Rn_m','alb','Ldwn','Lupw', 'Rn_C','H_C',                   &
      !    'LE_C','G_C','J_C','Fc_C','Rsc_C','An_C','u*_C',                 &
      !    'Td','W1_C','W2_C','W3_C','gcond',                               &
      !    'Evpt','Trans','Esoil','Einterc','Prec','Rss','Rs','Runoff',     &
      !    'PARidir','PARidif','albPARdir','albPARdif'                      
      !    write(itmp2,'(a8,9(7x,a2,i1),90(7x,a1,i2))')                     &
      !    'NYMD',('W_',i,i=1,9),('W',i,i=10,nlayer)                        
      ! endif 
                                                                        
                                                                        
     !  write(itmp1,'(i8.8,50(1x,f9.3))') nymd,&
     ! & rvar(54), rvar(53), rvar(55),&                   ! tm,em,um       
     ! & rvar(51), rvar(52), rvar(19),rvar(28),rvar(18),& ! Ki,Rn,alb,Ldwn,Lupw
     ! & rvar(17), rvar(10), rvar(01),rvar(15),rvar(45),& ! Rn,H,LE,G,J     
     ! & rvar(34), rvar(65), rvar(33),rvar(66),&          ! Fc,Rsc,An ,u*   
     ! & rvar(21), rvar(121),rvar(122),rvar(123),&        ! Td,W1,W2,W3    
     ! & rvar(39), rvar(46),rvar(2),rvar(4),rvar(44),&    ! gc,Evptran,Transp,Esoil,Einterc
     ! & rvar(57), rvar(7), rvar(8), rvar(9),&            ! precip,Qng,Croff,Runoff
     ! & rvar(24), rvar(25), rvar(29), rvar(30)           ! PARidir, PARidif, albPARdir, albPARdif                 
      
     ! write(itmp2,'(i8.8,50(1x,f9.3))') nymd,(www(i),i=1,nlayer) 
!      write(*,'(i8.8,11(1x,f6.3))') nymd,(www(i),i=1,nlayer)           
      return 
      END                                           
!                                                                        
!=======================================================================
!                                                                       
      subroutine driver (iu, icho, isnow, ichi, itero, nyfirst, &
           greeness_param, vmax_param) 
!                                                                       
!=======================================================================
!                                                                       
!     forcing meteo data:                                               
!     data required to run sib :                                        
!                                                                       
!     nymd     : date and time (yymmddhh)                               
!     swdown   : shortwave downward radiation.                          
!     zlwd     : longwave  downward radiation.                          
!     em,tm,um : vapor pressure,temperature and wind speed              
!                at measurement height ( or lowest model level.)        
!     tprec    : precipitation.                                         
!                                                                       
!     subroutines called : radc2                                        
!                                                                       
      use comsibc
      implicit none
      character (len=30) :: cfinp 
      real (kind=8) :: vchec(6)
      real (kind=8) :: feno(12) 
      integer :: diames(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      real (kind=8) :: x
      !
      integer :: iu
      integer :: icho
      integer :: isnow
      integer :: ichi
      integer :: itero
      integer :: nyfirst
      real (kind=8) :: co2amp
      real (kind=8) :: cold
      real (kind=8) :: difrat
      real (kind=8) :: fenot
      real (kind=8) :: gfrac
      real (kind=8) :: gpost
      real (kind=8) :: gpres
      real (kind=8) :: gprev
      integer :: idddd
      integer :: ihour
      integer :: imont
      integer :: iqc
      integer :: jj
      integer :: k
      integer :: kk
      integer :: mm
      real (kind=8) :: ptot = 0d0
      real (kind=8) :: rhair
      real (kind=8) :: tprec
      real (kind=8) :: vnrat
      real (kind=8) :: xco2m
      real (kind=8) :: zlwd
      real (kind=8) :: e
      !para modulo de calibracao: carbono e agua (Evandro)
      real (kind=8) :: greeness_param
      real (kind=8) :: vmax_param
      !para ler conjunto de parametros aerodinamicos calibrados
      integer, parameter :: nlinha_zlt=21 !numero de linhas de zlt calibrado
      integer, parameter :: digsig=10 ! inteiro para selecao dos digitos
                                      ! significativos para comparacao dos
                                      ! dos valores de zlt do sib2 com zlt do
                                      ! arquivo de parametros calibrados
      e(x) = exp( 21.18123d0 - 5418.0d0 / x ) / 0.622d0

!-----------------------------------------------------------------------
!...    open monthly input forcing met data / read green phenology      
!-----------------------------------------------------------------------
      if (itero.eq.0) then 
         read(ichi,*) 
         read(ichi,*) (greex(mm),mm=1,12) 
         read(ichi,*) (zltex(mm),mm=1,12) 
         read(ichi,*) ( vmex(mm),mm=1,12)
         ! print *, 'ANTES-----------------------------------'
         ! print '(12F5.2)', greex
         ! print '(12F6.1)', vmex
         !write(98,*) 'Greex' ,(greex(mm),mm=1,12) 
         !write(98,*) 'Zltes' ,(zltex(mm),mm=1,12) 
         !write(*,*) 'passei..' ,(zltex(mm),mm=1,12)                       
         read(ichi,*) 
         read(ichi,*) cfinp 
         write( *,'(a25,a40)')' # opening file counting ',cfinp 
         !write(98,'(a25,a40)')' # opening file counting ',cfinp 
         open(iu, file= cfinp, status='old') 
         read(iu,*)
         !Evandro M Anselmo---------
         ptot = 0d0
         !para uso do modulo de calibracao carbono agua
         greex(1:12) = greeness_param
         vmex(1:12) = vmax_param
         ! print *, 'DEPOIS-----------------------------------'
         ! print '(12F5.2)', greex
         ! print '(12F6.1)', vmex
         !--------------------------
      endif 
!  851 CONTINUE 
!---------------------------------------------------------------------  
!       read in meteorological forcing data                             
!---------------------------------------------------------------------  
      xmevap  = -9999. 
      xmsensh = -9999. 
      xmgheat = -9999. 
      xmfco2  = -9999. 
      xmrsco2 = -9999. 
      xco2m   = -9999. 
      xmustar = -9999. 
      xalbedo = -9999. 
      zlwd    = -9999. 
!  100 continue 
!---------------------------------------------------------------------##
!     Defining a default format to read data2                           
!     and check iwl flag to read                                        
!     Evandro M Anselmo 16/08/2019                                      
  201 continue 
      if (ilw.ne.3) then 
         read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec 
      else 
         read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec, rnetm 
      end if 
!---------------------------------------------------------------------##
                                                                        
!-----Com-rnetm---------------------------------------------------------
! 201  read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec, rnetm       
! 201  read(iu,*,end=1000) nymd, swdown, rnetm, em, tm, um, tprec       
!-----------------------------------------------------------------------
      if (nymd.ge.nyfirst) then 
         itero = itero + 1           ! incremento iteracoes apos nyfirst 
      else 
         goto 201 
      endif 
!...  checa qualidade forcantes                                         
      iqc  =  1 
      vchec(1) = swdown 
      vchec(2) = rnetm 
      vchec(3) = em 
      vchec(4) = tm 
      vchec(5) = um 
      vchec(6) = tprec 
!      if (tprec.eq.-9999.0) tprec = 0.                                 
      do 55 k=1,6 
   55    if (vchec(k).eq.-9999.) iqc = 0 
      if (iqc.eq.0) then 
         write(* ,*) ' Forcante com erro -9999: iqc = 0 at', nymd 
         !write(98,*) ' Forcante com erro -9999: iqc = 0 at', nymd 
         stop 
      endif 
!... vegetation phenology	and co2 concentration diurnal cycle           
      ihour = mod(nymd,100) 
      imont = int(mod(nymd,1000000)/10000) 
      idddd = int(mod(nymd,10000)/100) 
!... ico2m=  0:  pco2m constant = 34 (set in const2 every time step)    
!            1:  pco2m read in as xco2m (have to be a forcing data)     
!            2:  pco2m com ciclo diurno forcado                         
      if (ico2m.eq.0) co2amp = 0.                                                        
      if (ico2m.eq.2) co2amp = 100. ! amplitude (ppm)
      pco2m = facco2*(340 + co2amp/2.*cos(2.*pie * real(ihour - 8)/24.)) 
      if(ico2m.eq.1.and.xco2m.ne.-9999.) pco2m = xco2m !xco2m in ppm 
      pco2m = pco2m * 0.1 !0.1 ppm 
!      write(*,'(2(a10,f12.7))') 'pco2m=',pco2m,'real(ihour)',real(ihour)    
!..   interpola mes a mes (N - green fraction, L - LAI)                 
!      write(*,*) 'antes N ' ,(greex(mm),mm=1,12)                        
!      write(*,*) 'antes L' ,KK,(zltex(mm),mm=1,12)                      
      do 360 kk = 1,3 
         do 350 jj = 1,12 
            if (kk.eq.1) feno(jj) = greex (jj) 
            if (kk.eq.2) feno(jj) = zltex (jj) 
  350       if (kk.eq.3) feno(jj) =  vmex (jj) * 1.e-06 
!                                                                        
         gprev = feno(imont-1) 
         gpres = feno(imont) 
         gpost = feno(imont+1) 
         if (imont.eq.01) gprev = feno(12) 
         if (imont.eq.12) gpost = feno(01) 
!                                                                        
         if (idddd.gt.15) then
            gfrac = 0.5* (real(idddd-15)/real(diames(imont) - idddd + 15)) 
            fenot = (1.-gfrac) * gpres + gfrac * gpost 
         else 
            gfrac = 0.5* (real(15 - idddd)/ real(idddd + 15)) 
            fenot = (1.-gfrac) * gpres + gfrac * gprev 
         endif
!                                                                        
         if (kk.eq.1) green = fenot 
         if (kk.eq.2) zlt   = fenot 
  360    if (kk.eq.3) vmax0 = fenot 
!  361 continue 
! --------------------------------------------------------------------- 
!                                                                       
!     isnow = 0 : conventional run with received met data.              
!                                                                       
!     isnow = 1 : shock test with initial snow dump and freezing        
!                 temperatures at beginning of run warming to           
!                 normal over  5-day period.                            
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      if( isnow .eq. 0) go to 200 
      if ( iter .gt. 1) go to 210 
      tc = 270. 
      tg = 270. 
      snoww(2) = 0.1 
  210 cold = amax1 ( 0., (120. - (1.*iter)) / 120. ) 
      rhair = em/e(tm) 
      tm = tm * ( 1. - cold ) + (tm - 30.) * cold 
      em = e(tm)*rhair 
      if(em .lt. 0.)em = 0.1 
  200 continue 
!                                                                       
      um = amax1(um,0.25) 
!h    ustarm = mustar/100.                                              
      swdown = amax1(swdown,0.1) 
      ihour = mod(nymd,100) 
      ptot = ptot + tprec 
      ppl = tprec 
      ppc = tprec-ppl 
!                                                                        
      call radc2 
!                                                                        
      cloud = (1160.*sunang - swdown) / (963. * sunang) 
      cloud = amax1(cloud,0.) 
      cloud = amin1(cloud,1.) 
      cloud = amax1(0.58,cloud) 
!                                                                       
      difrat = 0.0604 / ( sunang-0.0223 ) + 0.0683 
      if ( difrat .lt. 0. ) difrat = 0. 
      if ( difrat .gt. 1. ) difrat = 1. 
!                                                                       
      difrat = difrat + ( 1. - difrat ) * cloud 
      vnrat = ( 580. - cloud*464. ) / ( ( 580. - cloud*499. )           &
     &        + ( 580. - cloud*464. ) )                                 
!                                                                       
      radn(1,1) = (1.-difrat)*vnrat*swdown 
      radn(1,2) = difrat*vnrat*swdown 
      radn(2,1) = (1.-difrat)*(1.-vnrat)*swdown 
      radn(2,2) = difrat*(1.-vnrat)*swdown 
      if (ilw.eq.3) radn(3,2) = 0.0 
      if (ilw.eq.1) radn(3,2) = zlwd 
!                                                                        
      if (ilw.eq.1.and.zlwd.le.100.)                                    &
      stop 'warning: checar ilw: incompativel'                         
      !
      !
      !Carrega os parametros aerodinamicos calibrados
      print *,'-------ANTES-------'
      print '(9F11.3)', ha, z0d, dd, g2, g3, cc1, cc2, corb1, corb2
      !
      ! Aqui e preciso informar:
      ! nlinha_zlt : numero de linhas do arquivo de parametros
      ! digsig : inteiro para selecionar o numero de digitos significativos
      !          para comparacao de zlt do sib2 com o zlt referente ao arquivo
      !          do conjunto de parametros aerodinamicos calibrados
      ! zlt : zlt que vem do comsibc.f95
      !
      call load_aeropars(nlinha_zlt, digsig, zlt, &
           ha, z0d, dd, g2, g3, cc1, cc2, corb1, corb2) ! saidas - reescreve os
                                       ! parametros aerodinamicos
                                       ! no passo de tempo considerendo a
                                       ! a calibracao
      !
      print *,'-------DEPOIS-------'
      print '(9F11.3)', ha, z0d, dd, g2, g3, cc1, cc2, corb1, corb2
      !
      return 
      !                                                                        
 1000 write(icho, 90)iu, nymd 
   90 format(5x,'eof encountered for unit= ',i2,' eof date = ',i8) 
      stop 
      END                                           
!=======================================================================
!                                                                       
      subroutine balan ( iplace, totwb ) 
!                                                                       
!=======================================================================
!                                                                       
!     energy and water balance check.                                   
!                                                                       
!-----------------------------------------------------------------------
      use comsibc 
      implicit none
      !
      integer :: iplace
      real (kind=8) :: totwb
      real (kind=8) :: cbal
      real (kind=8) :: emeter
      real (kind=8) :: endwb
      real (kind=8) :: errore
      real (kind=8) :: errorw
      real (kind=8) :: gbal
      integer :: i
      real (kind=8) :: pmeter
      real (kind=8) :: zlhs
      real (kind=8) :: zrhs
      !
      if( iplace .eq. 2 ) go to 100 
!                                                                       
      etmass = 0. 
      roff   = 0. 
!                                                                       
!	write(98,*)'====================================================='    
      totwb = 0. 
      do 20 i=1,nlayer 
!      write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i)                    
   20 totwb = totwb + ( www(i) * poros(i) * zdepth(i) ) 
      totwb = totwb + (capac(1) + capac(2) + snoww(1) + snoww(2)) 
                                                                        
!      totwb1 = dble(totwb)                                             
!      write(98,'(1x,a10,2a13,/,1x,f12.5,1x,f12.10,1x,f12.5)')          
!     .    ' S ',' M ',' To1',                                          
!     .   totwb1*1000.,                                                 
!     .   (capac(1)+capac(2)+snoww(1)+snoww(2))*1000., totwb*1000.      
!                                                                       
      goto 200 
!                                                                       
  100 continue 
!                                                                       
      endwb = 0. 
      do 40 i=1,nlayer 
   40 endwb = endwb +  (www(i) * poros(i) * zdepth(i)) 
!                                                                        
      endwb = endwb +  (capac(1) + capac(2) + snoww(1) + snoww(2))      &
     &    -  (ppl+ppc)/1000. +  (etmass)/1000. + roff                   
!                                                                        
!      errorw= totwb1 - dble(endwb)                                     
      errorw= totwb - endwb 
!                                                                        
      pmeter= (ppl+ppc)/1000. 
      emeter= etmass/1000. 
!                                                                       
      if (abs(errorw) .gt. 0.0001) then 
         write( *,'(a40)') ' ---> warning: water balance violation' 
         write(*,*) 'nymd , abs(errorw)= ',nymd ,abs(errorw) 
         !write(98,'(a40)') ' ---> warning: water balance violation' 
         !write(98,*) 'nymd , abs(errorw)= ',nymd ,abs(errorw) 
         !	do 35 i=1,nlayer                                                      
         ! 35	write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i)                 
         !      write(98,'(1x,a10,5a13,/,1x,f12.5,2(1x,f12.10),3(1x,f12.5))')    
         !     .  ' S ',' M ',' E',' R',' -Pr',                                  
         !     .' To2', endwb1*1000.,(capac(1)+capac(2)+snoww(1)+snoww(2))*1000.,
         !     . dble(etmass), dble(roff)*1000.,-dble(ppl+ppc),endwb*1000.       
         !      write(98,'(1x,a20,1x,f20.10)')'Error To1-To2(mm):',errorw*1000.d0
         !      write(98,*)                                                      
      endif 
!                                                                       
      cbal = radt(1) - chf - (ect+hc+eci)/dtt 
      gbal = radt(2) - shf - (egs+hg+egi)/dtt - heaten/dtt 
      zlhs = radt(1) + radt(2) - chf - shf 
      zrhs = hflux + (ect + eci + egi + egs)/dtt + heaten/dtt 
!                                                                       
      errore= zlhs - zrhs 
!                                                                       
      if(abs(errore) .gt. 1.) then 
         write(*,'(a40)') ' ---> warning: energy balance violation' 
         ! write(98,910) nymd, zlhs, zrhs, radt(1), radt(2), chf, shf,    &
         ! hflux, ect/dtt, eci/dtt, egi/dtt, egs/dtt, hc/dtt, hg/dtt,     &
         ! heaten/dtt, cbal, gbal         
      endif 
  ! 910 format(//,10x,'---> warning: energy balance violation **',//,     &
  !    & /,1x,'date ', i8,                                                &
  !    & /,1x,'rhs, lhs              ', 2g12.5,                           &
  !    & /,1x,'rn1, rn2, chf, shf, h ', 5g12.5,                           &
  !    & /,1x,'ect, eci, egi, egs    ', 4g12.5,                           &
  !    & /,1x,'hc        hg          ',  g12.5, 12x, g12.5,               &
  !    & /,1x,'heaten, c-bal, g-bal  ', 3g12.5 )                          
!                                                                       
  200 continue 
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
!                                                                       
      subroutine veginc(ichi) 
!                                                                       
!=======================================================================
!                                                                       
!    read vegetation and soil parameters for SIB2                       
!-----------------------------------------------------------------------
!    subroutines called :  vegpar                                       
!                          soipar                                       
!                          dynveg                                       
!                          varcal                                       
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!                                                                       
!        subscripts (iv, iw, il) :                                      
!                                                                       
!              iv  : surface layer ;                                    
!                1 = canopy                                             
!                2 = ground                                             
!              iw  : radiation wavelength;                              
!                1 = visible                                            
!                2 = near infrared                                      
!              il  : vegetation state;                                  
!                1 = live (green) and                                   
!                2 = dead (stems and trunk)                             
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!                 input parameter set                                   
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!   ivtype        : vegetation type                                     
!                                                                       
!        static parameters associated with vegetation type              
!        -------------------------------------------------              
!                                                                       
!   z2            : canopy top height                                   
!   z1            : canopy base height                                  
!   vcover        : vegetation cover fraction                           
!   chil          : leaf angle distribution factor                      
!   rootd         : rooting depth                                       
!   phc           : 1/2 critical leaf water potential limit             
!   tran(iw,il)   : leaf transmittance                                  
!   ref (iw,il)   : leaf reflectance                                    
!   effcon        : quantum efficiency                                  
!   gradm         : conductance-photosynthesis slope parameter          
!   binter        : conductance-photosynthesis intercept                
!   respcp        : respiration fraction of vmax                        
!   atheta        : wc, we coupling parameter                           
!   btheta        : wc & we, ws coupling parameter                      
!   trda          : temperature coefficient in gs-a model               
!   trdm          : temperature coefficient in gs-a model               
!   trop          : temperature coefficient in gs-a model               
!   slti          : slope of low temperature inhibition function        
!   hlti          : 1/2 point of low temperature inhibition function    
!   shti          : slope of high temperature inhibition function       
!   hhti          : 1/2 point of high temperature inhibition function   
!                                                                       
!   istype        : soil type                                           
!                                                                       
!        static parameters associated with soil type                    
!        -------------------------------------------                    
!                                                                       
!   sodep         : total depth of 3 soil moisture layers               
!   soref(iw)     : soil reflectance                                    
!   bee           : soil wetness exponent                               
!   phsat         : soil tension at saturation                          
!   satco         : hydraulic conductivity at saturation                
!   poros         : soil porosity                                       
!   slope         : cosine of mean slope                                
!                                                                       
!        time-space varying vegetation parameters, from                 
!        spectral vegetation indice (svi).                              
!        --------------------------------------------------             
!                                                                       
!   zlt           : leaf area index                                     
!   green         : green leaf fraction                                 
!   fparc         : canopy absorbed fraction of photosynthetically      
!                 : active radiation (par)  from svi                    
!                                                                       
!        parameters derived from the above                              
!        ---------------------------------                              
!                                                                       
!   vmax0         : rubisco velocity of sun-leaf                        
!   gmudmu        : time-mean leaf projection ( g(mu)/ mu )             
!   z0d           : roughness length                                    
!   dd            : zero plane displacement                             
!   cc1           : rb coefficient (c1)                                 
!   cc2           : rd coefficient (c2)                                 
!   zdepth        : individual depths of 3 soil moisture layers         
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!        other variables                                                
!        ---------------                                                
!                                                                       
!      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet                  
!                                                                       
!   g1            : ratio of km(actual) to km(log-linear) at z = z2     
!   g2            : ratio of ra(actual) to ra(log-linear) for momentum  
!                   between: z = z2 and z = zx, where zx = min(zl,zwind)
!   g3            : ratio of ra(actual) to ra(log-linear) for heat      
!                   between: z = z2 and z = zx, where zx = min(zl,zmet) 
!   ztz0          : parameter to determine depth of transition layer    
!                   above canopy, zl. zl = z2 + ztz0 * z0               
!   corb1         : non-neutral correction for calculation of aerodynami
!                   resistance between ha and z2. when multiplied by    
!                   h*rbb/tm gives bulk estimate of local richardson    
!                   number                                              
!                   rbb = ra for heat between ha and z2.                
!                   corb2 = 9*g/( rhoair*cpair* (du/dz)**2 )            
!   corb2         : neutral value of rbb*u2 ( squared ), equivalent to  
!                   rdc**2 for upper canopy                             
!   ha            : canopy source height for heat                       
!   zwind         : reference height for wind measurement               
!   zmet          : reference height for temperature, humidity          
!                   measurement                                         
!                                                                       
!        the above are generated from sibx + momopt output              
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none  
      logical :: pfirst = .true.  
      !data pfirst/.true./
      integer :: ichi
      integer :: iwave      
!                                                                       
      read(ichi, *) 
      read(ichi, *) 
      read(ichi, *) 
!                                                                       
      read(ichi, *) ivtype 
      call vegpar (ichi) 
!                                                                       
      read(ichi, *) 
      read(ichi, *) 
      read(ichi, *) istype, sodep,  (soref(iwave),iwave=1,2) 
                                                                        
      if (pfirst) then 
         pfirst=.false. 
!   50    format(/,6(1x,e12.6),/) 
         ! write(98,'(a72)')' ivtype' 
         ! write(98,*) ivtype 
         ! write(98,'(a72)') ' istype, sodep,  (soref(iwave),iwave=1,2)' 
         ! write(98,*) istype, sodep,  (soref(iwave),iwave=1,2)         
      endif 
                                                                        
      call soipar (ichi) 
!                                                                       
      call dynveg (ichi) 
!                                                                       
      call varcal (ichi) 
!                                                                       
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine vegpar (ichi) 
!                                                                       
!=======================================================================
!                                                                       
!     reading/setting of vegetation-type dependent static parameters.   
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none
      logical :: pfirst = .true. 
      !data pfirst/.true./
      integer :: ichi
      integer :: iw
!                                                                       
      read(ichi,*) 
      read(ichi,*) 
      read(ichi, *) z2, z1, vcover, chil 
      read(ichi,*) 
      read(ichi, *) rootd, phc 
!                                                                       
      read(ichi,*) 
      read(ichi,*) 
      read(ichi,*)(tran(iw,1), iw=1,2), (tran(iw,2), iw=1,2) !(iw,ilive)
      read(ichi,*)(ref (iw,1), iw=1,2), (ref (iw,2), iw=1,2) 
!                                                                       
      read(ichi,*) 
      read(ichi, *) effcon, gradm, binter, respcp, atheta, btheta 
      read(ichi,*) 
      read(ichi, *) trda, trdm, trop, slti, hlti, shti, hhti 
      read(ichi,*) 
      read(ichi,*)  acoef, bcoef, ccoef 
!                                                                       
      if (pfirst) then 
         pfirst=.false. 
!   50    format(/,6(1x,e12.6,/)) 
         ! write(98,'(a72)') ' z2, z1, vcover, chil' 
         ! write(98,*) z2, z1, vcover, chil 
         ! write(98,'(a72)') ' rootd, phc ' 
         ! write(98,*) rootd, phc 
         ! write(98,'(a72)') ' (tran(iw,2), iw=1,2)' 
         ! write(98,*) (tran(iw,1), iw=1,2) 
         ! write(98,'(a72)') ' (ref (iw,2), iw=1,2)' 
         ! write(98,*) (ref (iw,2), iw=1,2) 
         ! write(98,'(a72)') 'effcon, gradm, binter, respcp, atheta, btheta' 
         ! write(98,*) effcon, gradm, binter, respcp, atheta, btheta 
         ! write(98,'(a72)') ' trda, trdm, trop, slti, hlti, shti, hhti' 
         ! write(98,*) trda, trdm, trop, slti, hlti, shti, hhti 
         ! write(98,'(a72)') ' acoef, bcoef, ccoef' 
         ! write(98,*) acoef, bcoef, ccoef 
      endif 
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine soipar (ichi) 
!                                                                       
!=======================================================================
!                                                                       
!     reading/setting of soil-type dependent static parameters.         
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none
      logical :: pfirst = .true. 
      !data pfirst/.true./
      !
      integer :: ichi
      real (kind=8) :: extmax
      integer :: i
      integer :: il
      !                                                                       
      read(ichi, *) 
      read(ichi, *) 
      read(ichi, *) iinf, slope, xcs, jqng, xkb,                        &
     &               jesq, jdpsi, jkcon, jsys, jjgs, jqini              
      read(ichi, *) 
      do 10 i=1,nlayer 
   10 read(ichi,*)                                                      &
     & zdepth(i),extfrac(i),bee(i),phsat(i),satco(i),poros(i), www(i)   
                                                                        
      extmax = 0. 
      do 20 il=2,nlayer 
   20   extmax = extmax + extfrac(il) 
                                                                        
      if (pfirst) then 
         pfirst=.false. 
         ! write(98,'(a72)') ' iinf  slope ' 
         ! write(98,*)          iinf, slope 
         ! write(98,'(a60)')                                              &
         ! 'zdepth(i) extfrac(i) bee(i) phsat(i) satco(i) poros(i) www(i)'   
   !       do 19 i=1,nlayer 
   ! 19    write(98, '(3(1x,f6.4),2(1x,e10.3),3(1x,f6.4),/)')             &
   !       zdepth(i),extfrac(i),bee(i),phsat(i),satco(i),poros(i),www(i)     
   !       write(98,*) ' extmax(%): ',extmax*100. 
                                                                        
         if (extmax.gt.1.00001.or.extfrac(1).lt.1.) then 
            write(*,29) '!!! warning: extfrac violation ', extmax*100.,' %' 
            !write(98,29)'!!! warning: extfrac violation ', extmax*100.,' %' 
   29       format(a32,f20.10,a3) 
            stop 
         endif
      endif !if pfirst			 
!...                                                                    
      return 
      END                                           
!                                                                       
!=======================================================================
!                                                                       
      subroutine dynveg (ichi) 
!                                                                       
!=======================================================================
!                                                                       
!     reading of phenological vegetation parameters.                    
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none  
      logical :: pfirst = .true. 
      !data pfirst/.true./ 
      !
      integer :: ichi
      !
      read(ichi, *) 
      read(ichi, *) 
      read(ichi, *) fparc 
      if (pfirst) then 
         pfirst=.false. 
         ! write(98,'(a72)') ' fparc' 
         ! write(98,*) fparc         
!   50    format(6(1x,e12.6)) 
      endif 
!                                                                       
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine varcal (ichi) 
!                                                                       
!=======================================================================
!                                                                       
!     calculation of secondary parameters from input data               
!                                                                       
!-----------------------------------------------------------------------
      use comsibc
      implicit none  
      logical :: pfirst = .true. 
      !data pfirst /.true./ 
      !
      integer :: ichi
      integer :: i
      real (kind=8) :: park
      real (kind=8) :: scatp
      real (kind=8) :: sodep1      
      !                                                                       
      read(ichi,*) 
      read(ichi,*) 
      read(ichi,*) vmax0, gmudmu, green, zlt 
      read(ichi,*) 
      read(ichi,*) z0d, dd, cc1, cc2 
      read(ichi,*) 
      read(ichi,*) 
      read(ichi,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet 
!      write(*,'(a72)') ' vmax0, gmudmu, green, zlt'                    
!      write(*,*) vmax0, gmudmu, green, zlt                             
!      write(*,'(a72)') ' z0d, dd, cc1, cc2'                            
!      write(*,*) z0d, dd, cc1, cc2                                     
!      write(*,'(a72)')'corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet'
!      write(*,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet       
!new...                                                                 
      sodep1=0. 
      do 10 i=1,nlayer 
   10 sodep1 = sodep1 + zdepth(i) 
!new...                                                                 
                                                                        
      rootd = amin1( rootd, sodep1*0.75 ) 
                                                                        
      scatp    =     green   * ( tran(1,1) + ref(1,1) )                 &
     &         +( 1.-green ) * ( tran(1,2) + ref(1,2) )                 
      park = sqrt(1.-scatp) * gmudmu 
!      fparc = 1. - exp ( -park*zlt )                                    
      fparc = 1. - exp ( -park*zlt ) 
                                                                        
!h007      zlt = -1./park*alog( 1.-fparc )                              
                                                                        
!      write(*,'(a72)') ' rootd '                                       
!      write(*,*) rootd                                                 
!      write(*,'(a72)') ' scatp '                                       
!      write(*,*) scatp                                                 
!      write(*,'(a72)') ' park '                                        
!      write(*,*) park                                                  
!      write(*,'(a72)') ' zlt'                                          
!      write(*,*) zlt                                                   
      if (pfirst) then 
         pfirst=.false. 
!   50    format(/,6(1x,e12.6),/) 
         ! write(98,'(a72)') ' vmax0, gmudmu, green, zlt' 
         ! write(98,*) vmax0, gmudmu, green, zlt 
         ! write(98,'(a72)') ' z0d, dd, cc1, cc2' 
         ! write(98,*) z0d, dd, cc1, cc2 
         ! write(98,'(a72)')'corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet' 
         ! write(98,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet 
         ! write(98,'(a72)') ' rootd ' 
         ! write(98,*) rootd 
         ! write(98,'(a72)') ' scatp ' 
         ! write(98,*) scatp 
         ! write(98,'(a72)') ' park ' 
         ! write(98,*) park 
         ! write(98,'(a72)') ' zlt' 
         ! write(98,*) zlt         
      endif 
                                                                        
      return 
      END                                           
!=======================================================================
!                                                                       
      subroutine cntrol(ichi,icho,maxit,nylast,nyfirst) 
!                                                                       
!=======================================================================
!                                                                       
!      initialisation and switches.                                     
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      use comsibc 
      implicit none
      real (kind=8) :: ydep(nlayer)
      real (kind=8) :: eacum(nlayer) 
      logical :: pfirst = .true. 
      !data pfirst/.true./ 
      !
      integer :: ichi
      integer :: icho
      integer :: maxit
      integer :: nylast
      integer :: nyfirst
      integer :: i
      real (kind=8) :: qa
      !
!       write(98,*)'  entrando cntrol'                                  
!       write(*,*)'  entrando cntrol'                                   
!  900 format(18a4) 
      read(ichi,*) 
      read(ichi,*) 
      read(ichi, * )dtt, itrunk, ilw, ico2m,facco2, irespg 
                                                                        
      read(ichi,*) 
      read(ichi,*) zlat, zlong, time, month, day, year,                 &
     &             maxit, nyfirst, nylast                               
      read(ichi,*) 
      read(ichi, * )tc, tg, td, ta, tm, ht, qa 
                                                                        
!   48 format(a24,1x,i2,1x,a5) 
!   49 format(/,a24,1x,i2,1x,a5,/) 
                                                                        
      if (pfirst) then 
         pfirst=.false. 
         ! write(98,*)                                                    &
         ! zlat, zlong,time,month,day,year,maxit,nyfirst,nylast             
!   50    format(                                                        &
!         'zlat, zlong,time,month,day,year,maxit,nyfirst,nylast',/,      &
!     &   3f8.2,i3,1x,2f5.0,i3,4(1x,i8) )                                  
         write(icho,800) zlat, zlong,nyfirst,nylast,maxit 
  800    format(10x,32('*')/10x,'*       SiB2 off-line run      *'/10x, &
     &   32('*')/5x,'latitude : ',f6.2,5x,' longitude : ',f7.2/,        &
     &   4x,' run length: nyfirst - nylast',4x,i8,4x,i8,/,              &
     &   4x,'             maxit           ',4x,i8,/)                                                                        
         if(itrunk .eq. 1) write(icho,801) 
         if(itrunk .ge. 2) write(icho,802) itrunk 
  801    format(5x,'resistances calculated from initial fluxes') 
  802    format(5x,'resistances calculated by iteration, itrunk=',i4) 
         if(ilw .eq. 1) write(icho,816) 
         if(ilw .eq. 2) write(icho,817) 
         if(ilw .eq. 3) write(icho,818) 
  816    format(5x,'downward longwave radiation read in as data') 
  817    format(5x,'downward longwave radiation computed from brunts',  &
     &   ' equation' )                                                    
  818    format(5x,'downward longwave radiation computed as residual in &
     &   energy balance', /, 5x,'net radiation read in as data ')             
                                                                        
         !      if (iinf.eq.1) write(*,*)'Infiltration=1 top layer diffusion'    
         !      if (iinf.eq.2) write(*,*)                                        
         !     &           'Infiltration=2 wave front, w/  overland flow'        
         !      if (iinf.eq.3) write(*,*)                                        
         !     &           'Infiltration=3 wave front, w/o overland flow'        
         !      if (iinf.eq.4) write(*,*)                                        
         !     &  'Infiltration=4 wave front w/ sat impeding, w/o overland flow' 
         
         write(*,'(a28,/,3(5x,a25,2x,f9.4,/),10(5x,a25,2x,i2,/))')      &
     &   ' Soil hydrology parameters: ',                                &
     &   ' slope qng     - slope  ', slope,                             &
     &   ' qstar Horton  - xcs    ', xcs  ,                             &
     &   ' Liston qng    - xkb    ', xkb  ,                             &
     &   ' ativar qng    - jqng   ', jqng ,                             &
     &   ' infiltration  - iinf   ', iinf ,                             &
     &   ' numer scheme  - jesq   ', jesq ,                             &
     &   ' C(psi) calc   - jdpsi  ', jdpsi,                             &
     &   ' K(psi) calc   - jkcon  ', jkcon,                             &
     &   ' linear system - jsys   ', jsys ,                             &
     &   ' Jacobi/Gauss  - jjgs   ', jjgs ,                             &
     &   ' Q initial     - jqini  ', jqini                               
                                                                                 
         ydep(1) = zdepth(1)/2. 
         eacum(2) = extfrac(2) 
         eacum(1) = extfrac(1) 
                                                                        
         do i=2,nlayer 
            ydep(i) = zdepth(i-1) + zdepth(i)/2. 
            if (i.ge.3) eacum(i) = eacum(i-1) + extfrac(i) 
         enddo
                                                                        
         write (*,'(a44,/,50(i3,3x,f6.0,3x,f7.2,3x,f7.2,3x,f7.3,/))')   &
     &   'Layer Depth_cm  Root % Root_acum %  Winicial',                &
     &   (i, 100.*ydep(i), extfrac(i)*100.,eacum(i),www(i),i=1,nlayer)    
      endif
!      write(98,*)'  saindo cntrol' 
!     write(*,*)'  saindo cntrol'                                     
      return 
      END
