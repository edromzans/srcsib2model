c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE SIB2DATA                   ######
c     ######                                                      ######
c     ######                     Developed by                     ######
c     ######     River and Environmental Engineering Laboratory   ######
c     ######                University of Tokyo                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
      SUBROUTINE SiB2DATA
c
C#######################################################################
c
c     PURPOSE:
c
c     Set the static, dynamic parameters used in SiB2. The data iclude 
c     that for vegetation and soil and will be used for the calculation 
c     Set the static, dynamic parameters used in SiB2. The data include 
c     those for vegetation and soil and will be used for the calculation 
c     of derived aerodynamic parameters of SiB2. 
c
c     Included in subroutine SiB2derive_trans, SiB2derive_lai
c
C#######################################################################
c      
c
c     AUTHOR: Kun Yang
c     01/11/01.
c
c     MODIFICATION HISTORY:
c
c     01/11/01 (K. Yang)
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
 
c
c#######################################################################
c
c     Misc. local variables:
c
c#######################################################################
c
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
c     Definition of vegetation types:
c     SiB2 type           Name						ARPS type
c
c     1           Broadleaf-evergreen trees           7, 8  
c     2           Broadleaf-deciduous trees			6
c     3           Broadleaf and needleleaf trees
c     4           Needleleaf-evergreen trees
c     5           Needleleaf-deciduous trees
c     6           Short vegetation/C4 grass			1, 2, 3, 4, 5,11
c     7           Broadleaf shrubs with bare soil
c     8           Dwarf trees and shrubs				5, 12
c     9           Agriculture/C3 grassland			10
c
C#######################################################################
c
C#######################################################################
c
c     Set vegetation static parameters
c
C#######################################################################
c      
c
C#######################################################################
c
c     Morphological Properties
c
C#######################################################################
c      
C#######################################################################
c
c     Type 1: time and biomes invariant parameters
c
C#######################################################################
c      
      z0s_cst   =   0.05     ! Ground roughness length  (m)
      g4_cst  =   11.785     ! Transition height factor for mom. transfer
      dsfc_cst  =   0.02     ! Depth of surface layer (m)
      g1_cst    =   1.449    ! km(actual) : km(log-linear) at z2
c      
C#######################################################################
c
c     Type 2: time-invariant biomes dependent parameters
c
C#######################################################################
c
c     Canopy-top height  (m)
      z2_v  (1)    =    35.0       
      z2_v  (2)    =    20.0   
      z2_v  (3)    =    20.0   
      z2_v  (4)    =    17.0   
      z2_v  (5)    =    17.0   
      z2_v  (6)    =    1.0   
      z2_v  (7)    =    0.5   
      z2_v  (8)    =    0.6   
      z2_v  (9)    =    1.0   

c     Canopy-base height (m)
      z1_v  (1)    =    1.0      
      z1_v  (2)    =    11.5
      z1_v  (3)    =    10.0   
      z1_v  (4)    =    8.5    
      z1_v  (5)    =    8.5    
      z1_v  (6)    =    0.1   
      z1_v  (7)    =    0.1   
      z1_v  (8)    =    0.1   
      z1_v  (9)    =    0.1   

c     Inflection height for leaf-area density (m)
      zc_v  (1)    =    28.0     
      zc_v  (2)    =    17.0
      zc_v  (3)    =    15.0   
      zc_v  (4)    =    10.0   
      zc_v  (5)    =    10.0   
      zc_v  (6)    =    0.55  
      zc_v  (7)    =    0.3   
      zc_v  (8)    =    0.35  
      zc_v  (9)    =    0.55  

c     Inflection height for leaf-area density
      vcover_v   (1)    =    1.0     
      vcover_v   (2)    =    1.0     
      vcover_v   (3)    =    1.0     
      vcover_v   (4)    =    1.0     
      vcover_v   (5)    =    1.0     
      vcover_v   (6)    =    1.0     
      vcover_v   (7)    =    0.1     
      vcover_v   (8)    =    1.0     
      vcover_v   (9)    =    1.0     

c     Leaf area distribution factor 
      chil_v   (1)    =    0.1     
      chil_v   (2)    =    0.25     
      chil_v   (3)    =    0.125    
      chil_v   (4)    =    0.01    
      chil_v   (5)    =    0.01    
      chil_v   (6)    =    -0.3    
      chil_v   (7)    =    0.01    
      chil_v   (8)    =    0.2     
      chil_v   (9)    =    -0.3    

c     Leaf width (m)
      leafw_v   (1)    =    0.05    
      leafw_v   (2)    =    0.08    
      leafw_v   (3)    =    0.04    
      leafw_v   (4)    =    0.001    
      leafw_v   (5)    =    0.001    
      leafw_v   (6)    =    0.01    
      leafw_v   (7)    =    0.003    
      leafw_v   (8)    =    0.01     
      leafw_v   (9)    =    0.01

c     Leaf length	(m)
      leafl_v   (1)    =    0.1    
      leafl_v   (2)    =    0.15    
      leafl_v   (3)    =    0.1    
      leafl_v   (4)    =    0.055   
      leafl_v   (5)    =    0.04   
      leafl_v   (6)    =    0.3    
      leafl_v   (7)    =    0.03    
      leafl_v   (8)    =    0.3     
      leafl_v   (9)    =    0.3

c     Total depth	of three moisture layers (m)
      sodep_v   (1)    =    3.5    
      sodep_v   (2)    =    2.0
      sodep_v   (3)    =    2.0
      sodep_v   (4)    =    2.0
      sodep_v   (5)    =    2.0
      sodep_v   (6)    =    1.5
      sodep_v   (7)    =    1.5
      sodep_v   (8)    =    1.5
      sodep_v   (9)    =    1.5

c     Rooting depth (m)
      rootd_v   (1)    =    1.5    
      rootd_v   (2)    =    1.5
      rootd_v   (3)    =    1.5
      rootd_v   (4)    =    1.5
      rootd_v   (5)    =    1.5
      rootd_v   (6)    =    1.0
      rootd_v   (7)    =    1.0
      rootd_v   (8)    =    1.0
      rootd_v   (9)    =    1.0
c
C#######################################################################
c
c     Optical properties
c
C#######################################################################
c      
c     Live leaf reflectance to visible 
      reflv_v   (1)    =    0.1
      reflv_v   (2)    =    0.1
      reflv_v   (3)    =    0.07
      reflv_v   (4)    =    0.07
      reflv_v   (5)    =    0.07
      reflv_v   (6)    =    0.105
      reflv_v   (7)    =    0.1
      reflv_v   (8)    =    0.105
      reflv_v   (9)    =    0.105

c     Dead leaf reflectance to visible 
      refdv_v   (1)    =    0.16
      refdv_v   (2)    =    0.16
      refdv_v   (3)    =    0.16
      refdv_v   (4)    =    0.16
      refdv_v   (5)    =    0.16
      refdv_v   (6)    =    0.36 
      refdv_v   (7)    =    0.16
      refdv_v   (8)    =    0.36 
      refdv_v   (9)    =    0.16 

c     Live leaf reflectance to near IR 
      refln_v   (1)    =    0.45
      refln_v   (2)    =    0.45
      refln_v   (3)    =    0.40
      refln_v   (4)    =    0.35
      refln_v   (5)    =    0.35
      refln_v   (6)    =    0.58 
      refln_v   (7)    =    0.45
      refln_v   (8)    =    0.58 
      refln_v   (9)    =    0.58 

c     Dead leaf reflectance to near IR 
      refdn_v   (1)    =    0.39
      refdn_v   (2)    =    0.39
      refdn_v   (3)    =    0.39
      refdn_v   (4)    =    0.39
      refdn_v   (5)    =    0.39
      refdn_v   (6)    =    0.58 
      refdn_v   (7)    =    0.39
      refdn_v   (8)    =    0.58 
      refdn_v   (9)    =    0.58 

c     Live leaf transmittance to visible 
      tranlv_v   (1)    =    0.05
      tranlv_v   (2)    =    0.05
      tranlv_v   (3)    =    0.05
      tranlv_v   (4)    =    0.05
      tranlv_v   (5)    =    0.05
      tranlv_v   (6)    =    0.07
      tranlv_v   (7)    =    0.05
      tranlv_v   (8)    =    0.07 
      tranlv_v   (9)    =    0.07 

c     Dead leaf transmittance to visible 
      trandv_v   (1)    =    0.001
      trandv_v   (2)    =    0.001
      trandv_v   (3)    =    0.001
      trandv_v   (4)    =    0.001
      trandv_v   (5)    =    0.001
      trandv_v   (6)    =    0.22 
      trandv_v   (7)    =    0.001
      trandv_v   (8)    =    0.22 
      trandv_v   (9)    =    0.22 

c     Live leaf transmittance to near IR
      tranln_v   (1)    =    0.25
      tranln_v   (2)    =    0.25
      tranln_v   (3)    =    0.15
      tranln_v   (4)    =    0.10
      tranln_v   (5)    =    0.10
      tranln_v   (6)    =    0.25
      tranln_v   (7)    =    0.25
      tranln_v   (8)    =    0.25 
      tranln_v   (9)    =    0.25 

c     Dead leaf transmittance to near IR
      trandn_v   (1)    =    0.001
      trandn_v   (2)    =    0.001
      trandn_v   (3)    =    0.001
      trandn_v   (4)    =    0.001
      trandn_v   (5)    =    0.001
      trandn_v   (6)    =    0.38 
      trandn_v   (7)    =    0.001
      trandn_v   (8)    =    0.38 
      trandn_v   (9)    =    0.38 

c     Soil reflectance to visible
      sorefv_v   (1)    =    0.11
      sorefv_v   (2)    =    0.11
      sorefv_v   (3)    =    0.11
      sorefv_v   (4)    =    0.11
      sorefv_v   (5)    =    0.11

c Soil reflectance for grid cells designed as bare soil within biomes 6 and 7 
c should be specified according to ERBE. The following value is assumed

      sorefv_v   (6)    =    0.11   
      sorefv_v   (7)    =    0.11
      sorefv_v   (8)    =    0.11 
      sorefv_v   (9)    =    0.10 

c     Soil reflectance to near IR
      sorefn_v   (1)    =    0.225
      sorefn_v   (2)    =    0.225
      sorefn_v   (3)    =    0.225
      sorefn_v   (4)    =    0.225
      sorefn_v   (5)    =    0.225

c Soil reflectance for grid cells designed as bare soil within biomes 6 and 7 
c should be specified according to ERBE. The following value is assumed

      sorefn_v   (6)    =    0.225  
      sorefn_v   (7)    =    0.225
      sorefn_v   (8)    =    0.225
      sorefn_v   (9)    =    0.150

c
C#######################################################################
c
c     Type 3: time and biomes dependent variables 
c
c     The variables include
c     real    ha_var, z0d_var, dd_var,  
c	real    g2_var, g3_var, cc1_var, cc2_var, corb1_var, corb2_var       
c     real    zlt_var, green_var, fparc_var, gmudmu_var
c    
c     They should be derived from mophological and NDVI before calling
c     SiB2 and only need to calculate once for a not long-term 
c     simulation since their time-scale is quite long. 
c
c     These variables should be defined as permanent 2D arrary (nx,ny) 
c     in ARPS.  
c
C#######################################################################
c      
c
C#######################################################################
c
c     Type 4: location dependent variables
c     slope_cst, which should be calculated in ARPS
c     The following value is given by SiB2 paper.
C#######################################################################
c      
c      slope_cst =   0.176    ! Mean topographic slope (radians)
c
c
C#######################################################################
c
c     Physiological properties
c
C#######################################################################
c      
C#######################################################################
c
c     Type 1: time and biomes invariant parameters
c
C#######################################################################
c      
      shti_cst   =  0.3      ! Slope of high temp. inhibition func.(K-1)
      slti_cst   =  0.2      ! Slope of low temp. inhibition func. (K-1)
      trda_cst   =  1.3      ! Slope of high temp. inhibition function 
                             ! for respriation (K-1) 
      trdm_cst   =  328      ! One-halp point of high temp. inhibition 
                             ! function for respriation (K) 
      trop_cst   =  298      ! Temperature ceof. in GS-A model (K)  
                             ! using calculate qt=(Tc-Trop)/10      
      btheta_cst = 0.95      ! Coupling parameter for wp and ws
c      
C#######################################################################
c
c     Type 2: time-invariant biomes dependent parameters
c
C#######################################################################
c      
c     Maximum rubsico capacity of top leaf (mol / m2 s)
      vmax0_v   (1)    =    1.0e-4
      vmax0_v   (2)    =    1.0e-4
      vmax0_v   (3)    =    8.0e-5
      vmax0_v   (4)    =    6.0e-5
      vmax0_v   (5)    =    1.0e-4
      vmax0_v   (6)    =    3.0e-5
      vmax0_v   (7)    =    6.0e-5
      vmax0_v   (8)    =    6.0e-5
      vmax0_v   (9)    =    1.0e-4

c     Intrinsic quantum efficiency (mol / mol)
      effcon_v   (1)   =    0.08 
      effcon_v   (2)   =    0.08 
      effcon_v   (3)   =    0.08 
      effcon_v   (4)   =    0.08 
      effcon_v   (5)   =    0.08 
      effcon_v   (6)   =    0.05
      effcon_v   (7)   =    0.08 
      effcon_v   (8)   =    0.08 
      effcon_v   (9)   =    0.08 

c     Conductance_photosynthesis slope parameter (mol / m2 s)
      gradm_v   (1)    =    9.0
      gradm_v   (2)    =    9.0
      gradm_v   (3)    =    9.0
      gradm_v   (4)    =    9.0
      gradm_v   (5)    =    9.0
      gradm_v   (6)    =    4.0
      gradm_v   (7)    =    9.0
      gradm_v   (8)    =    9.0
      gradm_v   (9)    =    9.0

c     Minimum stamotal conductance (i.e., Conductance_photosynthesis intercept)
c     (mol / m2 s)
      binter_v   (1)   =    0.01
      binter_v   (2)   =    0.01
      binter_v   (3)   =    0.01
      binter_v   (4)   =    0.01
      binter_v   (5)   =    0.01
      binter_v   (6)   =    0.04
      binter_v   (7)   =    0.01
      binter_v   (8)   =    0.01
      binter_v   (9)   =    0.01
   
c     Photosynthesis coupling coeffiicent for wc and we
c     Note: Photosynthesis coupling coeffiicent for wp,wc and we is independent
c           of vegetation and thus is a constant, 
      atheta_v   (1)   =    0.98
      atheta_v   (2)   =    0.98
      atheta_v   (3)   =    0.98
      atheta_v   (4)   =    0.98
      atheta_v   (5)   =    0.98
      atheta_v   (6)   =    0.80
      atheta_v   (7)   =    0.98
      atheta_v   (8)   =    0.98
      atheta_v   (9)   =    0.98

c     One half point of high temperature inhibition function (K)
      hhti_v   (1)   =    313
      hhti_v   (2)   =    311
      hhti_v   (3)   =    307
      hhti_v   (4)   =    303
      hhti_v   (5)   =    303
      hhti_v   (6)   =    313
      hhti_v   (7)   =    313
      hhti_v   (8)   =    303
      hhti_v   (9)   =    308

c     One half point of low temperature inhibition function (K)
      hlti_v   (1)   =    288
      hlti_v   (2)   =    283
      hlti_v   (3)   =    281
      hlti_v   (4)   =    278
      hlti_v   (5)   =    278
      hlti_v   (6)   =    288
      hlti_v   (7)   =    283
      hlti_v   (8)   =    278
      hlti_v   (9)   =    281

c     One half critical leaf-water potential limit (m)
      phc_v   (1)   =    -200
      phc_v   (2)   =    -200
      phc_v   (3)   =    -200
      phc_v   (4)   =    -200
      phc_v   (5)   =    -200
      phc_v   (6)   =    -200
      phc_v   (7)   =    -300
      phc_v   (8)   =    -200
      phc_v   (9)   =    -200

c     Leaf respiration fraction of vmax0          
      respcp_v   (1)   =    0.015
      respcp_v   (2)   =    0.015
      respcp_v   (3)   =    0.015
      respcp_v   (4)   =    0.015
      respcp_v   (5)   =    0.015
      respcp_v   (6)   =    0.025
      respcp_v   (7)   =    0.015
      respcp_v   (8)   =    0.015
      respcp_v   (9)   =    0.015
c
C#######################################################################
c
c     Definition of soil types
c     SiB2 type         Name                    ARPS type
c
c     1           Sand -> Loamy sand	          1, 2 
c     2           Sandy loam                    3
c     3           Loam, silt loam               4, 5 
c     4           Clay loam -> sandy clay loam  6,7 
c     5           Clay, Clay loam               8,9,10,11
c     6           Ice                           1
c     7           Organic						  5
c
C#######################################################################
c
C#######################################################################
c
c     Set soil parameters
c
C#######################################################################
c      
c     Soil wetness exponent 
	bee_s    (1)     =	 4.05
	bee_s    (2)     =	 4.90
	bee_s    (3)     =	 5.39
	bee_s    (4)     =	 7.12
	bee_s    (5)     =	 8.52
	bee_s    (6)     =	 4.05
	bee_s    (7)     =	 5.39

c     Soil tension at saturation	(m)
	phsat_s  (1)     =	 -0.04
	phsat_s  (2)     =	 -0.07
	phsat_s  (3)     =	 -0.15
	phsat_s  (4)     =	 -0.12
	phsat_s  (5)     =	 -0.36
	phsat_s  (6)     =	 -0.04
	phsat_s  (7)     =	 -0.15

c     Hydraulic conductivity at saturation (m/s)
	satco_s  (1)     =	 176.0e-6
	satco_s  (2)     =	  35.0e-6
	satco_s  (3)     =	   7.0e-6
	satco_s  (4)     =	   6.3e-6
	satco_s  (5)     =     2.5e-6
	satco_s  (6)     =	 176.0e-6
	satco_s  (7)     =	   7.0e-6

c     Soil porosity  (m3/m3)
	poros_s  (1)     =	 0.40
	poros_s  (2)     =	 0.44
	poros_s  (3)     =	 0.45
	poros_s  (4)     =	 0.42
	poros_s  (5)     =   0.48
	poros_s  (6)     =	 0.40
	poros_s  (7)     =	 0.45

      RETURN
      END !SUBROUTINE SiB2DATA	       
