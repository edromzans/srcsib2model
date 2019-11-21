!=======================================================================
!                       comsibc.h
!=======================================================================

!     sib common block : 1-d version ( carbon )
!                                                              may 1991
!=======================================================================

    parameter (nlayer=10)
!-----------------------------------------------------------------------
!        prognostic variables
!-----------------------------------------------------------------------
    common /stepv/ tc, tg, td, capac(2), snoww(2), www(nlayer)
!-----------------------------------------------------------------------
!        physical constants
!-----------------------------------------------------------------------
    common /const/ pie, timcon, cpair, rhoair, psy, hlat, gx, vkc, &
    snomel, stefan, tf, clai, cw, snofac, asnow, rcp, &
    kappa, epsfac
    common /atchem/ po2m, pco2m, facco2
!-----------------------------------------------------------------------
!        vegetation : static, dynamic, derived parameters
!-----------------------------------------------------------------------
    common /gridij/ ivtype, istype
    common /vstate/ z2, z1, vcover, chil, tran(2,2), ref(2,2), &
    rootd, phc, &
    effcon, gradm, binter, respcp, atheta, btheta, &
    trda, trdm, trop, slti, hlti, shti, hhti
! x  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc
! x  &                 rootd, ph1, ph2
    common /vdyijt/ zlt, green, fparc, greex(12), zltex(12), vmex(12)
    common /vderiv/ z0d, dd, cc1, cc2, vmax0, gmudmu
! ew
    common /respva/ pco2a, irespg, acoef, bcoef, ccoef
!-----------------------------------------------------------------------
!        soils : space-varying, type-dependent parameters
!-----------------------------------------------------------------------
    common /soilij/ sodep, soref(2)
    common /soils/ bee(nlayer), phsat(nlayer), poros(nlayer), &
    satco(nlayer), zdepth(nlayer), extfrac(nlayer), slope
!-----------------------------------------------------------------------
!        input atmospheric and site data
!-----------------------------------------------------------------------
    common /atmos/ em, tm, um, zm, psur, ppc, ppl, radn(3,2), &
    sunang, swdown, rnetm, cloud, bps
!-----------------------------------------------------------------------
!        site parameters specific to 1-d model operation
!-----------------------------------------------------------------------
    common /caerod/ corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet, &
    ht
    common /site/ zlong, zlat, salb(2,2), rab(2,3,2)
    common /steps/ dtt, itrunk, ilw, niter, iter, ispare
    common /govern/ time, year, month, day, hour, nymd
!-----------------------------------------------------------------------
!        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m),
!                                       lw(w m-2), drag(kg m-1 s-2) )
!-----------------------------------------------------------------------
    common /donor/ etmass, hflux, roff, zlwup, drag
!-----------------------------------------------------------------------
!        variables calculated from above and ambient conditions
!-----------------------------------------------------------------------
    common /rause/ z0, xdx, rbc, rdc
    common /aerorx/ ra, rb, rd
    common /grads/ tgs, ta, ea, etc, etgs, getc, getgs ,u2, ustar
    common /radabs/ albedo(2,2,2), radfac(2,2,2), radt(2), &
    thermk, exrain, tgeff
    common /surfrs/ rst, rstfac(4), rsoil, cog1, cog2, hr, fc, fg
    common /hydrol/ satcap(2), wc, wg, canex, areas
    common /stores/ ccx, cg, csoil
    common /delts/ dtc, dtg, dtd, dth, dqm
    common /carbio/ assimn, respc, respg, pco2i, gsh2o
!-----------------------------------------------------------------------
!        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
!-----------------------------------------------------------------------
    common /flux/ ec, eg, hc, hg, chf, shf, &
    ect, eci, egi, egs, &
    ecmass, egmass, heaten
!-----------------------------------------------------------------------
! R  Observed & calculated fluxes
    common /flob/  xmevap, xmsensh, xmgheat, xmfco2, xmrsco2, xmustar, &
    xalbedo, ico2m, gflux, udm

! R  soil mlayer model
    common /soimul/ qqq(nlayer), q0, qng, croff, cthru, xcs, xkb, &
    iinf, jesq, jdpsi, jkcon, jsys, jjgs, jqini, jqng
         
         
! R  output files
    common /outfile/ itmp1, itmp2, itmp3, itmp4, itmp5
!-----------------------------------------------------------------------
