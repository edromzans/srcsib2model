c=======================================================================
c                       comsibc.h
c=======================================================================
c              
c     sib common block : 1-d version ( carbon )
c                                                              may 1991
c=======================================================================
C
      parameter (nlayer=10)
c-----------------------------------------------------------------------
c        prognostic variables
c-----------------------------------------------------------------------
      common /stepv/ tc, tg, td, capac(2), snoww(2), www(nlayer)
c-----------------------------------------------------------------------
c        physical constants
c-----------------------------------------------------------------------
      common /const/ pie, timcon, cpair, rhoair, psy, hlat, g, vkc,
     &                 snomel, stefan, tf, clai, cw, snofac, asnow, rcp,
     &                 kappa, epsfac
      common /atchem/ po2m, pco2m, facco2
c-----------------------------------------------------------------------
c        vegetation : static, dynamic, derived parameters
c-----------------------------------------------------------------------
      common /gridij/ ivtype, istype
      common /vstate/ z2, z1, vcover, chil, tran(2,2), ref(2,2),
     &                 rootd, phc,
     &                 effcon, gradm, binter, respcp, atheta, btheta,
     &                 trda, trdm, trop, slti, hlti, shti, hhti
cxx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc
cxx  &                 rootd, ph1, ph2
      common /vdyijt/ zlt, green, fparc, greex(12), zltex(12), vmex(12)
      common /vderiv/ z0d, dd, cc1, cc2, vmax0, gmudmu
cnew
      common /respva/ pco2a, irespg, acoef, bcoef, ccoef
c----------------------------------------------------------------------- 
c        soils : space-varying, type-dependent parameters
c-----------------------------------------------------------------------
      common /soilij/ sodep, soref(2)
      common /soils/ bee(nlayer), phsat(nlayer), poros(nlayer),
     &      satco(nlayer), zdepth(nlayer), extfrac(nlayer), slope
c-----------------------------------------------------------------------
c        input atmospheric and site data
c-----------------------------------------------------------------------        
      common /atmos/ em, tm, um, zm, psur, ppc, ppl, radn(3,2),
     &                 sunang, swdown, rnetm, cloud, bps
c-----------------------------------------------------------------------
c        site parameters specific to 1-d model operation
c----------------------------------------------------------------------- 
      common /caerod/ corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet,
     &                 ht
      common /site/ zlong, zlat, salb(2,2), rab(2,3,2)
      common /steps/ dtt, itrunk, ilw, niter, iter, ispare
      common /govern/ time, year, month, day, hour, nymd
c-----------------------------------------------------------------------
c        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m),
c                                       lw(w m-2), drag(kg m-1 s-2) )
c-----------------------------------------------------------------------
      common /donor/ etmass, hflux, roff, zlwup, drag
c----------------------------------------------------------------------- 
c        variables calculated from above and ambient conditions
c-----------------------------------------------------------------------
      common /rause/ z0, d, rbc, rdc
      common /aerorx/ ra, rb, rd
      common /grads/ tgs, ta, ea, etc, etgs, getc, getgs ,u2, ustar
      common /radabs/ albedo(2,2,2), radfac(2,2,2), radt(2),
     &                 thermk, exrain, tgeff
      common /surfrs/ rst, rstfac(4), rsoil, cog1, cog2, hr, fc, fg
      common /hydrol/ satcap(2), wc, wg, canex, areas
      common /stores/ ccx, cg, csoil
      common /delts/ dtc, dtg, dtd, dth, dqm
      common /carbio/ assimn, respc, respg, pco2i, gsh2o
c-----------------------------------------------------------------------
c        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
c-----------------------------------------------------------------------
      common /flux/ ec, eg, hc, hg, chf, shf,
     &                 ect, eci, egi, egs,
     &                 ecmass, egmass, heaten
c-----------------------------------------------------------------------
cHR  Observed & calculated fluxes
      common /flob/  xmevap, xmsensh, xmgheat, xmfco2, xmrsco2, xmustar,
     &               xalbedo, ico2m, gflux, udm

cHR  soil mlayer model
      common /soimul/ qqq(nlayer), q0, qng, croff, cthru, xcs, xkb,
     & iinf, jesq, jdpsi, jkcon, jsys, jjgs, jqini, jqng
     
     
cHR  output files
      common /outfile/ itmp1, itmp2, itmp3, itmp4, itmp5   
c-----------------------------------------------------------------------
