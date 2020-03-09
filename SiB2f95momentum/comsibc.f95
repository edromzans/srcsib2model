module comsibc
  implicit none
!-----------------------------------------------------------------------
!        comsibc as module variables (Evandro M Anselmo)
!-----------------------------------------------------------------------
  integer, parameter :: float64=8
  integer, parameter :: nlayer=11
!-----------------------------------------------------------------------
!        prognostic variables
!-----------------------------------------------------------------------
    ! common /stepv/ tc, tg, td, capac(2), snoww(2), www(nlayer)
  real (kind=float64) :: tc
  real (kind=float64) :: tg
  real (kind=float64) :: td
  real (kind=float64) :: capac(2)
  real (kind=float64) :: snoww(2)
  real (kind=float64) :: www(nlayer)
!-----------------------------------------------------------------------
!        physical constants
!-----------------------------------------------------------------------
    ! common /const/ pie, timcon, cpair, rhoair, psy, hlat, gx, vkc, &
    ! snomel, stefan, tf, clai, cw, snofac, asnow, rcp, &
    ! kappa, epsfac
  real (kind=float64) :: pie
  real (kind=float64) :: timcon
  real (kind=float64) :: cpair
  real (kind=float64) :: rhoair
  real (kind=float64) :: psy
  real (kind=float64) :: hlat
  real (kind=float64) :: gx
  real (kind=float64) :: vkc
  real (kind=float64) :: snomel
  real (kind=float64) :: stefan
  real (kind=float64) :: tf
  real (kind=float64) :: clai
  real (kind=float64) :: cw
  real (kind=float64) :: snofac
  real (kind=float64) :: asnow
  real (kind=float64) :: rcp
  real (kind=float64) :: kappa ! integer
  real (kind=float64) :: epsfac
    ! common /atchem/ po2m, pco2m, facco2
  real (kind=float64) :: po2m
  real (kind=float64) :: pco2m
  real (kind=float64) :: facco2
!-----------------------------------------------------------------------
!        vegetation : static, dynamic, derived parameters
!-----------------------------------------------------------------------
    ! common /gridij/ ivtype, istype
  integer :: ivtype
  integer :: istype
    ! common /vstate/ z2, z1, vcover, chil, tran(2,2), ref(2,2), &
    ! rootd, phc, &
    ! effcon, gradm, binter, respcp, atheta, btheta, &
    ! trda, trdm, trop, slti, hlti, shti, hhti
  real (kind=float64) :: z2
  real (kind=float64) :: z1
  real (kind=float64) :: vcover
  real (kind=float64) :: chil
  real (kind=float64) :: tran(2,2)
  real (kind=float64) :: ref(2,2)
  real (kind=float64) :: rootd
  real (kind=float64) :: phc
  real (kind=float64) :: effcon
  real (kind=float64) :: gradm
  real (kind=float64) :: binter
  real (kind=float64) :: respcp
  real (kind=float64) :: atheta
  real (kind=float64) :: btheta
  real (kind=float64) :: trda
  real (kind=float64) :: trdm
  real (kind=float64) :: trop
  real (kind=float64) :: slti
  real (kind=float64) :: hlti
  real (kind=float64) :: shti
  real (kind=float64) :: hhti
    ! common /vdyijt/ zlt, green, fparc, greex(12), zltex(12), vmex(12)
  real (kind=float64) :: zlt
  real (kind=float64) :: green
  real (kind=float64) :: fparc
  real (kind=float64) :: greex(12)
  real (kind=float64) :: zltex(12)
  real (kind=float64) :: vmex(12)
    ! common /vderiv/ z0d, dd, cc1, cc2, vmax0, gmudmu
  real (kind=float64) :: z0d
  real (kind=float64) :: dd
  real (kind=float64) :: cc1
  real (kind=float64) :: cc2
  real (kind=float64) :: vmax0
  real (kind=float64) :: gmudmu
    ! common /respva/ pco2a, irespg, acoef, bcoef, ccoef
  real (kind=float64) :: pco2a
  integer :: irespg
  real (kind=float64) :: acoef
  real (kind=float64) :: bcoef
  real (kind=float64) :: ccoef
!-----------------------------------------------------------------------
!        soils : space-varying, type-dependent parameters
!-----------------------------------------------------------------------
    ! common /soilij/ sodep, soref(2)
  real (kind=float64) :: sodep
  real (kind=float64) :: soref(2)
    ! common /soils/ bee(nlayer), phsat(nlayer), poros(nlayer), &
    ! satco(nlayer), zdepth(nlayer), extfrac(nlayer), slope
  real (kind=float64) :: bee(nlayer)
  real (kind=float64) :: phsat(nlayer)
  real (kind=float64) :: poros(nlayer)
  real (kind=float64) :: satco(nlayer)
  real (kind=float64) :: zdepth(nlayer)
  real (kind=float64) :: extfrac(nlayer)
  real (kind=float64) :: slope
!-----------------------------------------------------------------------
!        input atmospheric and site data
!-----------------------------------------------------------------------
    ! common /atmos/ em, tm, um, zm, psur, ppc, ppl, radn(3,2), &
    ! sunang, swdown, rnetm, cloud, bps
  real (kind=float64) :: em
  real (kind=float64) :: tm
  real (kind=float64) :: um
  real (kind=float64) :: zm
  real (kind=float64) :: psur
  real (kind=float64) :: ppc
  real (kind=float64) :: ppl
  real (kind=float64) :: radn(3,2)
  real (kind=float64) :: sunang
  real (kind=float64) :: swdown
  real (kind=float64) :: rnetm
  real (kind=float64) :: cloud
  real (kind=float64) :: bps
!-----------------------------------------------------------------------
!        site parameters specific to 1-d model operation
!-----------------------------------------------------------------------
    ! common /caerod/ corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet, &
    ! ht
  real (kind=float64) :: corb1
  real (kind=float64) :: corb2
  real (kind=float64) :: ha
  real (kind=float64) :: g1
  real (kind=float64) :: g2
  real (kind=float64) :: g3
  real (kind=float64) :: ztz0
  real (kind=float64) :: zwind
  real (kind=float64) :: zmet
  real (kind=float64) :: ht
    ! common /site/ zlong, zlat, salb(2,2), rab(2,3,2)
  real (kind=float64) :: zlong
  real (kind=float64) :: zlat
  real (kind=float64) :: salb(2,2)
  real (kind=float64) :: rab(2,3,2)
    ! common /steps/ dtt, itrunk, ilw, niter, iter, ispare
  real (kind=float64) :: dtt
  integer :: itrunk
  integer :: ilw
  integer :: niter
  integer :: iter
  integer :: ispare
    ! common /govern/ time, year, month, day, hour, nymd
  real (kind=float64) :: time
  real (kind=float64) :: year
  real (kind=float64) :: month
  real (kind=float64) :: day
  real (kind=float64) :: hour
  integer :: nymd
!-----------------------------------------------------------------------
!        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m),
!                                       lw(w m-2), drag(kg m-1 s-2) )
!-----------------------------------------------------------------------
    ! common /donor/ etmass, hflux, roff, zlwup, drag
  real (kind=float64) :: etmass
  real (kind=float64) :: hflux
  real (kind=float64) :: roff
  real (kind=float64) :: zlwup
  real (kind=float64) :: drag
!-----------------------------------------------------------------------
!        variables calculated from above and ambient conditions
!-----------------------------------------------------------------------
    ! common /rause/ z0, xdx, rbc, rdc
  real (kind=float64) :: z0
  real (kind=float64) :: xdx
  real (kind=float64) :: rbc
  real (kind=float64) :: rdc
    ! common /aerorx/ ra, rb, rd
  real (kind=float64) :: ra
  real (kind=float64) :: rb
  real (kind=float64) :: rd
    ! common /grads/ tgs, ta, ea, etc, etgs, getc, getgs ,u2, ustar
  real (kind=float64) :: tgs
  real (kind=float64) :: ta
  real (kind=float64) :: ea
  real (kind=float64) :: etc
  real (kind=float64) :: etgs
  real (kind=float64) :: getc
  real (kind=float64) :: getgs
  real (kind=float64) :: u2
  real (kind=float64) :: ustar
    ! common /radabs/ albedo(2,2,2), radfac(2,2,2), radt(2), &
    ! thermk, exrain, tgeff
  real (kind=float64) :: albedo(2,2,2)
  real (kind=float64) :: radfac(2,2,2)
  real (kind=float64) :: radt(2)
  real (kind=float64) :: thermk
  real (kind=float64) :: exrain
  real (kind=float64) :: tgeff
    ! common /surfrs/ rst, rstfac(4), rsoil, cog1, cog2, hr, fc, fg
  real (kind=float64) :: rst
  real (kind=float64) :: rstfac(4)
  real (kind=float64) :: rsoil
  real (kind=float64) :: cog1
  real (kind=float64) :: cog2
  real (kind=float64) :: hr
  real (kind=float64) :: fc
  real (kind=float64) :: fg
    ! common /hydrol/ satcap(2), wc, wg, canex, areas
  real (kind=float64) :: satcap(2)
  real (kind=float64) :: wc
  real (kind=float64) :: wg
  real (kind=float64) :: canex
  real (kind=float64) :: areas
    ! common /stores/ ccx, cg, csoil
  real (kind=float64) :: ccx
  real (kind=float64) :: cg
  real (kind=float64) :: csoil
    ! common /delts/ dtc, dtg, dtd, dth, dqm
  real (kind=float64) :: dtc
  real (kind=float64) :: dtg
  real (kind=float64) :: dtd
  real (kind=float64) :: dth
  real (kind=float64) :: dqm
    ! common /carbio/ assimn, respc, respg, pco2i, gsh2o
  real (kind=float64) :: assimn
  real (kind=float64) :: respc
  real (kind=float64) :: respg
  real (kind=float64) :: pco2i
  real (kind=float64) :: gsh2o
!-----------------------------------------------------------------------
!        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2
!-----------------------------------------------------------------------
    ! common /flux/ ec, eg, hc, hg, chf, shf, &
    ! ect, eci, egi, egs, &
    ! ecmass, egmass, heaten
  real (kind=float64) :: ec
  real (kind=float64) :: eg
  real (kind=float64) :: hc
  real (kind=float64) :: hg
  real (kind=float64) :: chf
  real (kind=float64) :: shf
  real (kind=float64) :: ect
  real (kind=float64) :: eci
  real (kind=float64) :: egi
  real (kind=float64) :: egs
  real (kind=float64) :: ecmass
  real (kind=float64) :: egmass
  real (kind=float64) :: heaten
!-----------------------------------------------------------------------
! R  Observed & calculated fluxes
    ! common /flob/  xmevap, xmsensh, xmgheat, xmfco2, xmrsco2, xmustar, &
    ! xalbedo, ico2m, gflux, udm
  real (kind=float64) :: xmevap
  real (kind=float64) :: xmsensh
  real (kind=float64) :: xmgheat
  real (kind=float64) :: xmfco2
  real (kind=float64) :: xmrsco2
  real (kind=float64) :: xmustar
  real (kind=float64) :: xalbedo
  integer :: ico2m
  real (kind=float64) :: gflux
  real (kind=float64) :: udm
!-----------------------------------------------------------------------    
! R  soil mlayer model
    ! common /soimul/ qqq(nlayer), q0, qng, croff, cthru, xcs, xkb, &
    ! iinf, jesq, jdpsi, jkcon, jsys, jjgs, jqini, jqng
  real (kind=float64) :: qqq(nlayer)
  real (kind=float64) :: q0
  real (kind=float64) :: qng
  real (kind=float64) :: croff
  real (kind=float64) :: cthru
  real (kind=float64) :: xcs
  real (kind=float64) :: xkb
  integer :: iinf
  integer :: jesq
  integer :: jdpsi
  integer :: jkcon
  integer :: jsys
  integer :: jjgs
  integer :: jqini
  integer :: jqng
!-----------------------------------------------------------------------  
! R  output files
    ! common /outfile/ itmp1, itmp2, itmp3, itmp4, itmp5
  integer :: itmp1
  integer :: itmp2
  integer :: itmp3
  integer :: itmp4
  integer :: itmp5
  !-----------------------------------------------------------------------

end module comsibc
