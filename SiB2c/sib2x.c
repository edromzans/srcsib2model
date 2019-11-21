/* sib2x.F -- translated by f2c (version 20160102).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real rngdtg, rngdtc, rncdtg, rncdtc, hgdtg, hgdtc, hgdth, hcdtg, hcdtc, 
	    hcdth, egdtg, egdtc, egdqm, ecdtg, ecdtc, ecdqm, deadtc, deadtg, 
	    demdqm, deadqm, aag, aac, aam, bbg, bbc, bbm;
} flxdif_;

#define flxdif_1 flxdif_

struct {
    real tc, tg, td, capac[2], snoww[2], www[10];
} stepv_;

#define stepv_1 stepv_

struct {
    real pie, timcon, cpair, rhoair, psy, hlat, gx, vkc, snomel, stefan, tf, 
	    clai, cw, snofac, asnow, rcp, epsfac;
} const_;

#define const_1 const_

struct {
    integer kappa;
} const22_;

#define const22_1 const22_

struct {
    real po2m, pco2m, facco2;
} atchem_;

#define atchem_1 atchem_

struct {
    integer ivtype, istype;
} gridij_;

#define gridij_1 gridij_

struct {
    real z2, z1, vcover, chil, tran[4]	/* was [2][2] */, ref[4]	/* 
	    was [2][2] */, rootd, phc, effcon, gradm, binter, respcp, atheta, 
	    btheta, trda, trdm, trop, slti, hlti, shti, hhti;
} vstate_;

#define vstate_1 vstate_

struct {
    real zlt, green, fparc, greex[12], zltex[12], vmex[12];
} vdyijt_;

#define vdyijt_1 vdyijt_

struct {
    real z0d, dd, cc1, cc2, vmax0, gmudmu;
} vderiv_;

#define vderiv_1 vderiv_

struct {
    real pco2a, acoef, bcoef, ccoef;
} respva_;

#define respva_1 respva_

struct {
    integer irespg;
} respva2_;

#define respva2_1 respva2_

struct {
    real sodep, soref[2];
} soilij_;

#define soilij_1 soilij_

struct {
    real bee[10], phsat[10], poros[10], satco[10], zdepth[10], extfrac[10], 
	    slope;
} soils_;

#define soils_1 soils_

struct {
    real em, tm, um, zm, psur, ppc, ppl, radn[6]	/* was [3][2] */, 
	    sunang, swdown, rnetm, cloud, bps;
} atmos_;

#define atmos_1 atmos_

struct {
    real corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet, ht;
} caerod_;

#define caerod_1 caerod_

struct {
    real zlong, zlat, salb[4]	/* was [2][2] */, rab[12]	/* was [2][3][
	    2] */;
} site_;

#define site_1 site_

struct {
    real dtt;
    integer itrunk, ilw, niter, iter, ispare;
} steps_;

#define steps_1 steps_

struct {
    real time, year, day, hour;
} govern_;

#define govern_1 govern_

struct {
    integer month, nymd;
} govern2_;

#define govern2_1 govern2_

struct {
    real etmass, hflux, roff, zlwup, drag;
} donor_;

#define donor_1 donor_

struct {
    real z0, xdx, rbc, rdc;
} rause_;

#define rause_1 rause_

struct {
    real ra, rb, rd;
} aerorx_;

#define aerorx_1 aerorx_

struct {
    real tgs, ta, ea, etc, etgs, getc, getgs, u2, ustar;
} grads_;

#define grads_1 grads_

struct {
    real albedo[8]	/* was [2][2][2] */, radfac[8]	/* was [2][2][2] */, 
	    radt[2], thermk, exrain, tgeff;
} radabs_;

#define radabs_1 radabs_

struct {
    real rst, rstfac[4], rsoil, cog1, cog2, hr, fc, fg;
} surfrs_;

#define surfrs_1 surfrs_

struct {
    real satcap[2], wc, wg, canex, areas;
} hydrol_;

#define hydrol_1 hydrol_

struct {
    real ccx, cg, csoil;
} stores_;

#define stores_1 stores_

struct {
    real dtc, dtg, dtd, dth, dqm;
} delts_;

#define delts_1 delts_

struct {
    real assimn, respc, respg, pco2i, gsh2o;
} carbio_;

#define carbio_1 carbio_

struct {
    real ec, eg, hc, hg, chf, shf, ect, eci, egi, egs, ecmass, egmass, heaten;
} flux_;

#define flux_1 flux_

struct {
    real xmevap, xmsensh, xmgheat, xmfco2, xmrsco2, xmustar, xalbedo, gflux, 
	    udm;
} flob_;

#define flob_1 flob_

struct {
    integer ico2m;
} flob2_;

#define flob2_1 flob2_

struct {
    real qqq[10], q0, qng, croff, cthru, xcs, xkb;
    integer iinf, jesq, jdpsi, jkcon, jsys, jjgs, jqini, jqng;
} soimul_;

#define soimul_1 soimul_

struct outfile_1_ {
    integer itmp1, itmp2, itmp3, itmp4, itmp5;
};

#define outfile_1 (*(struct outfile_1_ *) &outfile_)

/* Initialized data */

struct {
    integer e_1[5];
    } outfile_ = { 88, 78, 79, 80, 81 };


/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__9 = 9;
static integer c__3 = 3;

/* ======================================================================= */


/* Main program */ int MAIN__(void)
{
    /* Initialized data */

    static integer ichi = 1;
    static integer icho = 6;
    static integer iu = 8;
    static integer itero = 0;
    static integer ipbl = 0;
    static integer isnow = 0;

    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), s_wsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_wsfe(void), f_clos(cllist *);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int rada2_(void), balan_(integer *, real *);
    static integer maxit;
    static real totwb;
    extern /* Subroutine */ int outer_(integer *), updat2_(void), inter2_(
	    void), const2_(void), begtem_(void), veginc_(integer *), cntrol_(
	    integer *, integer *, integer *, integer *, integer *), driver_(
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     endtem_(integer *);
    static integer nylast, nyfirst;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 6, 0, "(a17,1x,i8,2x,i8.8)", 0 };
    static cilist io___11 = { 0, 98, 0, "(a17,1x,i8,2x,i8.8)", 0 };



/* ======================================================================= */

/* ...  SiB driver offline version, forced by meteorological data */

/* 	core of routines for sib2 model */

/* 	**** to use SiB2 in stand-by mode **** */

/* ======================================================================= */


/*     last revision:          dec. 15, 1993 */

/*     P. Sellers, J. Collatz, L. Bounoua */


/*     last update:      04 fev 97   Humberto Rocha */
/* 	soil multi-layer model */

/* ======================================================================= */

/*     subroutines called from this segment  :    veginc */
/*                                                cntrol */
/*                                                const2 */
/*                                                driver */
/*                                                balan */
/*                                                inter2 */
/*                                                rada2 */
/*                                                begtem */
/*                                                endtem */
/*                                                updat2 */
/*                                                balan */

/* ----------------------------------------------------------------------- */
/*     ipbl = 0 : dtc, dtg only     ; as per SE-86 */
/*     ipbl = 1 : dtc, dtg, dth, dqm; as per Randall-Sellers, SA-89b */

/*     isnow= 0 : conventional run using amazon forcings. */
/*     isnow= 1 : snow shock test, warming to normal over 5 days. */
/* ----------------------------------------------------------------------- */
/* ...	variables only important to the optimization module */
/* ... */
/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */
/* ... output files opening */
    o__1.oerr = 0;
    o__1.ounit = 98;
    o__1.ofnmlen = 12;
    o__1.ofnm = "sib2diag.dat";
    o__1.orl = 0;
    o__1.osta = "unknown";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = outfile_1.itmp1;
    o__1.ofnmlen = 10;
    o__1.ofnm = "sib2dt.dat";
    o__1.orl = 0;
    o__1.osta = "unknown";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = outfile_1.itmp2;
    o__1.ofnmlen = 13;
    o__1.ofnm = "sib2dt_SM.dat";
    o__1.orl = 0;
    o__1.osta = "unknown";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/* H      open(itmp3,file='sib2dt_Qu.dat',status='unknown')		! vazão Q upward */
/* H      open(itmp4,file='sib2dt_Qd.dat',status='unknown')		! vazão Q downward */
/* H      open(itmp5,file='sib2dt_Qh.dat',status='unknown')		! vazão Q Hortoniana */
/* umidade do so */
    o__1.oerr = 0;
    o__1.ounit = ichi;
    o__1.ofnmlen = 5;
    o__1.ofnm = "data1";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*      write(98,'(20a11)') */
/*     & 'xs','P','D','Pinf','Dc','Dd','Ri','R','Pi','q0' */
/* parameters file */
    veginc_(&ichi);
    cntrol_(&ichi, &icho, &maxit, &nylast, &nyfirst);
/* ... simulation start: at nyfirst (data1) */
/* ... simulation end  : at maxit or nylast (most limiting out of two) */
    i__1 = maxit;
    for (steps_1.iter = 1; steps_1.iter <= i__1; ++steps_1.iter) {
/*      write( *,'(a20,2i10)')'iter nymd:',iter,nymd */
	const2_();
/*       write(98,'(A20)')'  to call driver' */
	driver_(&iu, &icho, &isnow, &ichi, &itero, &nyfirst);
	if (steps_1.iter % (integer) (86400.f / steps_1.dtt) == 0) {
/* display screen cada di */
	    s_wsfe(&io___10);
	    do_fio(&c__1, "main: iter nymd=", (ftnlen)16);
	    do_fio(&c__1, (char *)&steps_1.iter, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
	    e_wsfe();
	    s_wsfe(&io___11);
	    do_fio(&c__1, "main: iter nymd=", (ftnlen)16);
	    do_fio(&c__1, (char *)&steps_1.iter, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
/*      if (mod(iter,365*int(86400./dtt)).eq.0) then  !display screen cada ano */
/*      write( *,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd */
/*      write(98,'(a17,1x,i8,2x,i8.8)') 'main: iter nymd=',iter,nymd */
/*      endif */
/* 				!display screen cada passo de tempo */
/*      write(98,'(a20,i5,i12)')'  iter  nymd:',iter,nymd */
/*      write(*,'(a20,i5,i12)')'  iter nymd:',iter,nymd */
/*       write(98,'(A20)')'   passed driver' */
	if (govern2_1.nymd > nylast) {
	    goto L1001;
	}
/* 	write(98,'(A20)')'  to call optima' */
/* 	if (lopt) call optima (xx, mo, no, ff, iqc, itero) */
/* 	write(98,'(A20)')'  passed optima' */
/*       write(98,'(A20)')'  to call balan' */
	balan_(&c__1, &totwb);
/*       write(98,'(A20)')'   passed balan' */
/*      print*,maxit */
/*       write(98,'(A20)')'  to call inter2' */
	inter2_();
/*       write(98,'(A20)')'   passed inter2' */
/*       write(98,'(A20)')'  to call rada2' */
	rada2_();
/*       write(98,'(A20)')'   passed rada2' */
/*       write(98,'(A20)')'  to call begtem' */
	begtem_();
/*       write(98,'(A20)')'   passed begtem' */
/*       write(98,'(A20)')'  to call endtem' */
	endtem_(&ipbl);
/*       write(98,'(A20)')'   passed endtem' */
/*       write(98,'(A20)')'  to call updat2' */
	updat2_();
/*       write(98,'(A20)')'   passed updat2' */
/*       write(98,'(A20)')'  to call balan' */
	balan_(&c__2, &totwb);
/*       write(98,'(A20)')'   passed balan' */
/*       write(98,'(A20)')'  to call outer' */
	outer_(&itero);
/*       write(98,'(A20)')'   passed outer' */
	if (govern2_1.nymd == nylast) {
	    goto L1001;
	}
/* L1000: */
    }
/* ---------------------------------------------------------------------- */
L1001:
    cl__1.cerr = 0;
    cl__1.cunit = ichi;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = iu;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = outfile_1.itmp1;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = outfile_1.itmp2;
    cl__1.csta = 0;
    f_clos(&cl__1);
/* H      close(itmp3) */
/* H      close(itmp4) */
/* H      close(itmp5) */
    cl__1.cerr = 0;
    cl__1.cunit = 98;
    cl__1.csta = 0;
    f_clos(&cl__1);

    s_stop(" SiB2 HAS DONE. ", (ftnlen)16);
    return 0;
} /* MAIN__ */


/* ======================================================================= */

/*   SUBROUTINES */

/* ======================================================================= */
/* Subroutine */ int outer_(integer *itero)
{
    /* Builtin functions */
    double sqrt(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__, in;
    static real tc4, tg4, fco2, elat, rvar[170], zlwf, calbe, canil, evapg, 
	    trant, cgsto, radswa, radswd, radtot, gcstor;

    /* Fortran I/O blocks */
    static cilist io___29 = { 0, 0, 0, "(a8,100(1x,a9))", 0 };
    static cilist io___30 = { 0, 0, 0, "(a8,9(7x,a2,i1),90(7x,a1,i2))", 0 };
    static cilist io___32 = { 0, 0, 0, "(i8.8,50(1x,f9.3))", 0 };
    static cilist io___33 = { 0, 0, 0, "(i8.8,50(1x,f9.3))", 0 };


/* ---------------------------------------------------------------------- */
/* writes out selected variables (unformatted) select in data1 namelist */
/* ---------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */
/* ... */
    trant = flux_1.ect / const_1.hlat;
    canil = flux_1.eci / const_1.hlat;
    evapg = (flux_1.egs + flux_1.egi) / const_1.hlat;
    radswd = atmos_1.radn[0] + atmos_1.radn[3] + atmos_1.radn[1] + 
	    atmos_1.radn[4];
    radswa = (1.f - site_1.salb[0]) * atmos_1.radn[0] + (1.f - site_1.salb[2])
	     * atmos_1.radn[3] + (1.f - site_1.salb[1]) * atmos_1.radn[1] + (
	    1.f - site_1.salb[3]) * atmos_1.radn[4];
    calbe = 1.f - radswa / radswd;
    cgsto = flux_1.shf - flob_1.gflux;
/* ..   fco2 = co2 flux in umol m-2 s-1 */
    fco2 = (carbio_1.respg - carbio_1.assimn) * 1e6f;
/* Rs ca */
    if (flob_1.xmrsco2 != -9999.f) {
	fco2 = flob_1.xmrsco2 - carbio_1.assimn * 1e6f;
    }
/* Rs ob */
    radtot = radabs_1.radt[0] + radabs_1.radt[1];
    elat = donor_1.etmass / steps_1.dtt * const_1.hlat;
    gcstor = flux_1.chf + flux_1.shf;

    grads_1.tgs = dmin(const_1.tf,stepv_1.tg) * hydrol_1.areas + stepv_1.tg * 
	    (1.f - hydrol_1.areas);
    tc4 = stepv_1.tc * stepv_1.tc * stepv_1.tc * stepv_1.tc;
    tg4 = grads_1.tgs * grads_1.tgs * grads_1.tgs * grads_1.tgs;
    zlwf = tc4 * const_1.stefan * vstate_1.vcover * (1.f - radabs_1.thermk) + 
	    (1.f - vstate_1.vcover * (1.f - radabs_1.thermk)) * 
	    const_1.stefan * tg4;
    radabs_1.tgeff = sqrt(sqrt(zlwf / const_1.stefan));
/* ...................................................................... */
/* ======================================================================= */
/* ... list of all variables to write out */
/* ======================================================================= */
/* ... water balance */
/* 1   elat            latent heat flux (Wm-2) */
/* 2   ect/dtt         canopy transpiration (Wm-2) */
/* 3   eci/dtt         canopy interception loss (Wm-2) */
/* 4   egs/dtt         soil evaporation (Wm-2) */
/* 5   egi/dtt         ground cover interception loss (Wm-2) */
/* 6   q0              top soil infiltration flux (ms-1) */
/* 7   qng             deep drainage flux (ms-1) */
/* 8   croff           surface runoff (m) */
/* 9   roff            total runoff (m) */
/* ...	sensible heat balance */
/* 10  hflux           sensible heat flux (Wm-2) */
/* 11  hc/dtt          canopy sensible heat flux (Wm-2) */
/* 12  hg/dtt          ground cover sensible heat flux (Wm-2) */
/* 13  chf             canopy storage energy (Wm-2) */
/* 14  cgsto           ground storage energy (Wm-2) */
/* 15  gflux           soil heat flux (Wm-2) */
/* 16  heaten/dtt      snowmelt heat loss (Wm-2) */
/* ...    radiation balance ................................................ */
/* 17  radtot          calculated net radiation (Wm-2) */
/* 18  zlwf            long wave upward radiation (Wm-2) */
/* 19  calbe           surface albedo (dimensionless) */
/* 20  tgeff           radiative surface temperature (K) */
/* 21  td              deep soil temperature (K) */
/* 22  radt(1)         canopy net radiation (Wm-2) */
/* 23  radt(2)         ground cover net radiation (Wm-2) */
/* 24  radn(1,1)       direct par incoming radiation (Wm-2) */
/* 25  radn(1,2)       difuse par incoming radiation (Wm-2) */
/* 26  radn(2,1)       direct nir incoming radiation (Wm-2) */
/* 27  radn(2,2)       direct nir incoming radiation (Wm-2) */
/* 28  radn(3,2)       tir incoming radiation (Wm-2) */
/* 29  salb(1,1)       direct par albedo */
/* 30  salb(1,2)       difuse par albedo */
/* 31  salb(2,1)       direct nir albedo */
/* 32  salb(2,2)       direct nir albedo */
/* ...  carbon balance */
/* 33  assimn          net assimilation (mimol m-2 s-1) */
/* 34  fco2            net ecosystem exchange (mimol m-2 s-1) */
/* 35  respg           soil respiration (mimol m-2 s-1) */
/* 36  respc           canopy (plant) respiration (mimol m-2 s-1) */
/* 37  pco2a           co2 atmospheric partial pressure (Pa) */
/* ... conductance calculation variables */
/* 38  rst             canopy (leaf) conductance (m s-1) */
/* 39  gsh2o           canopy (leaf) conductance (molm-2s-1) */
/* 40  rstfac(1)       conductance stress factor vpd */
/* 41  rstfac(2)       conductance stress factor soil moisture */
/* 42  rstfac(3)       conductance stress factor temperature */
/* 43  rstfac(4)       total conductance stress factor */
/* ...  other calculated variables (FILL IT IF NECESSARY) */
/* 44                  blank */
/* 45                  blank */
/* 46                  blank */
/* 47                  blank */
/* 48                  blank */
/* 49                  blank */
/* 50                  blank */
/* ...    observed (forcing or not) variables */
/* 51  swdown          downward shortwave radiation (Wm-2) */
/* 52  rnetm           net radiation (Wm-2) */
/* 53  em              water vapour pressure (HPa) */
/* 54  tm              surface air temperature (K) */
/* 55  um              horizontal wind speed (ms-1) */
/* 56  udm             surface wind direction (degree) */
/* 57  ppl+ppc         total precipitation (mm) */
/* 58  pco2m           atmospheric co2 concentration (Pa) */
/* 59  xmevap          observed latent heat flux (Wm-2) */
/* 60 xmsensh          observed sensible heat flux (Wm-2) */
/* 61 xmgheat           observed soil heat flux (Wm-2) */
/* 62 xmco2           observed co2 flux (Wm-2) */
/* 63 xalbedo          observed surface albedo */
/* ...  other observed variables (FILL IT IF NECESSARY) */
/* 64                  blank */
/* 65                  blank */
/* 66 ustar            calculated friction velocity (ms-1) */
/* 67                  blank */
/* 68                  blank */
/* 69                  blank */
/* 70                  blank */
/* .. soil variables (up to nlayer = 50 MAXIMUM) */
/* 71..120     qqq(i) interlayer i to i+1 water flux (m s-1) (+ downward) */
/* 121..170    www(i) soil wetness at layer i */
/* ------------------------------------------------------------------ */
/* 	variables atribution */
/* ------------------------------------------------------------------ */
/* L700: */
    rvar[0] = elat;
    rvar[1] = flux_1.ect / const_1.hlat;
/* mm */
    rvar[2] = flux_1.eci / const_1.hlat;
/* mm */
    rvar[3] = flux_1.egs / const_1.hlat;
/* mm */
    rvar[4] = flux_1.egi / const_1.hlat;
/* mm */
    rvar[5] = soimul_1.q0 * 1e3f * steps_1.dtt;
/* mm */
    rvar[6] = soimul_1.qng * 1e3f * steps_1.dtt;
/* mm */
    rvar[7] = soimul_1.croff * 1e3f;
/* mm */
    rvar[8] = donor_1.roff * 1e3f;
/* mm */
    rvar[9] = donor_1.hflux;
    rvar[10] = flux_1.hc;
    rvar[11] = flux_1.hg;
    rvar[12] = flux_1.chf;
    rvar[13] = cgsto;
    rvar[14] = flob_1.gflux;
    rvar[15] = flux_1.heaten;
    rvar[16] = radtot;
    rvar[17] = zlwf;
    rvar[18] = calbe;
    rvar[19] = radabs_1.tgeff;
    rvar[20] = stepv_1.td - 273.15f;
    rvar[21] = radabs_1.radt[0];
    rvar[22] = radabs_1.radt[1];
    rvar[23] = atmos_1.radn[0];
    rvar[24] = atmos_1.radn[3];
    rvar[25] = atmos_1.radn[1];
    rvar[26] = atmos_1.radn[4];
    rvar[27] = atmos_1.radn[5];
    rvar[28] = site_1.salb[0];
    rvar[29] = site_1.salb[2];
    rvar[30] = site_1.salb[1];
    rvar[31] = site_1.salb[3];
    rvar[32] = carbio_1.assimn * 1e6f;
    rvar[33] = fco2;
    rvar[34] = carbio_1.respg * 1e6f;
    rvar[35] = carbio_1.respc * 1e6f;
    rvar[36] = respva_1.pco2a;
    rvar[37] = surfrs_1.rst;
    rvar[38] = carbio_1.gsh2o * 18.f;
/* stomat conductance mm /s */
    rvar[39] = surfrs_1.rstfac[0];
    rvar[40] = surfrs_1.rstfac[1];
    rvar[41] = surfrs_1.rstfac[2];
    rvar[42] = surfrs_1.rstfac[3];

    rvar[43] = rvar[2] + rvar[4];
/* total interception loss */
    rvar[44] = rvar[12] + rvar[13];
/* 	rvar(46) = rvar(1)/hlat*dtt		! Evptrans (mm) */
/* column energy storage */
    rvar[45] = rvar[1] + rvar[2] + rvar[3] + rvar[4];
/* 	rvar(47) = blank */
/* 	rvar(48) = blank */
/* 	rvar(49) = blank */
/* 	rvar(50) = blank */

/* Evptrans (mm) */
    rvar[50] = atmos_1.swdown;
    rvar[51] = atmos_1.rnetm;
/* Rn observada (for Ld calculatio */
    rvar[52] = atmos_1.em;
    rvar[53] = atmos_1.tm - 273.15f;
/* Celsius */
    rvar[54] = atmos_1.um;
    rvar[55] = flob_1.udm;
    rvar[56] = atmos_1.ppl + atmos_1.ppc;
    rvar[57] = atchem_1.pco2m;
    rvar[58] = flob_1.xmevap;
    rvar[59] = flob_1.xmsensh;
    rvar[60] = flob_1.xmgheat;
    rvar[61] = flob_1.xmfco2;
    rvar[62] = flob_1.xalbedo;

    rvar[63] = flob_1.xmrsco2;
    rvar[64] = rvar[34] + rvar[35];
/* Rsc calculado */
    rvar[65] = grads_1.ustar;
    rvar[66] = flob_1.xmustar;
/* 	rvar(69) = blank */
/* 	rvar(70) = blank */
    for (in = 1; in <= 10; ++in) {
	rvar[in + 69] = soimul_1.qqq[in - 1];
/* L40: */
	rvar[in + 119] = stepv_1.www[in - 1];
    }
/* -------------------------------------------------------------------- */
/* 	write out variables */
/* -------------------------------------------------------------------- */
    if (*itero == 1) {
/* write labels */
	io___29.ciunit = outfile_1.itmp1;
	s_wsfe(&io___29);
	do_fio(&c__1, "NYMD", (ftnlen)4);
	do_fio(&c__1, "Tm", (ftnlen)2);
	do_fio(&c__1, "em", (ftnlen)2);
	do_fio(&c__1, "um", (ftnlen)2);
	do_fio(&c__1, "Ki", (ftnlen)2);
	do_fio(&c__1, "Rn_m", (ftnlen)4);
	do_fio(&c__1, "alb", (ftnlen)3);
	do_fio(&c__1, "Ldwn", (ftnlen)4);
	do_fio(&c__1, "Lupw", (ftnlen)4);
	do_fio(&c__1, "Rn_C", (ftnlen)4);
	do_fio(&c__1, "H_C", (ftnlen)3);
	do_fio(&c__1, "LE_C", (ftnlen)4);
	do_fio(&c__1, "G_C", (ftnlen)3);
	do_fio(&c__1, "J_C", (ftnlen)3);
	do_fio(&c__1, "Fc_C", (ftnlen)4);
	do_fio(&c__1, "Rsc_C", (ftnlen)5);
	do_fio(&c__1, "An_C", (ftnlen)4);
	do_fio(&c__1, "u*_C", (ftnlen)4);
	do_fio(&c__1, "Td", (ftnlen)2);
	do_fio(&c__1, "W1_C", (ftnlen)4);
	do_fio(&c__1, "W2_C", (ftnlen)4);
	do_fio(&c__1, "W3_C", (ftnlen)4);
	do_fio(&c__1, "gcond", (ftnlen)5);
	do_fio(&c__1, "Evpt", (ftnlen)4);
	do_fio(&c__1, "Trans", (ftnlen)5);
	do_fio(&c__1, "Esoil", (ftnlen)5);
	do_fio(&c__1, "Einterc", (ftnlen)7);
	do_fio(&c__1, "Prec", (ftnlen)4);
	do_fio(&c__1, "Rss", (ftnlen)3);
	do_fio(&c__1, "Rs", (ftnlen)2);
	do_fio(&c__1, "Runoff", (ftnlen)6);
	do_fio(&c__1, "PARidir", (ftnlen)7);
	do_fio(&c__1, "PARidif", (ftnlen)7);
	do_fio(&c__1, "albPARdir", (ftnlen)9);
	do_fio(&c__1, "albPARdif", (ftnlen)9);
	e_wsfe();
	io___30.ciunit = outfile_1.itmp2;
	s_wsfe(&io___30);
	do_fio(&c__1, "NYMD", (ftnlen)4);
	for (i__ = 1; i__ <= 9; ++i__) {
	    do_fio(&c__1, "W_", (ftnlen)2);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	}
	for (i__ = 10; i__ <= 10; ++i__) {
	    do_fio(&c__1, "W", (ftnlen)1);
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }
    io___32.ciunit = outfile_1.itmp1;
    s_wsfe(&io___32);
    do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&rvar[53], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[52], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[54], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[50], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[51], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[18], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[27], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[17], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[16], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[9], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[0], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[14], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[44], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[33], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[64], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[32], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[65], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[20], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[120], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[121], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[122], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[38], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[45], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[1], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[3], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[43], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[56], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[6], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[7], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[8], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[23], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[24], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[28], (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&rvar[29], (ftnlen)sizeof(real));
    e_wsfe();
/* tm, em, um */
/* Ki,Rn, alb,Ldw */
/* Rn,H,LE,G,J */
/* Fc,Rsc,An ,u* */
/* Td, W1,W2,W3 */
/* gc, Evptran, T */
/* precip, Qng  , */
/* PARidir, PARid */
    io___33.ciunit = outfile_1.itmp2;
    s_wsfe(&io___33);
    do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
    for (i__ = 1; i__ <= 10; ++i__) {
	do_fio(&c__1, (char *)&stepv_1.www[i__ - 1], (ftnlen)sizeof(real));
    }
    e_wsfe();
/*      write(*,'(i8.8,11(1x,f6.3))') nymd,(www(i),i=1,nlayer) */
    return 0;
} /* outer_ */

/* ======================================================================= */

/* Subroutine */ int driver_(integer *iu, integer *icho, integer *isnow, 
	integer *ichi, integer *itero, integer *nyfirst)
{
    /* Initialized data */

    static integer diames[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

    /* Format strings */
    static char fmt_90[] = "(5x,\002eof encountered for unit= \002,i2,\002 e"
	    "of date = \002,i8)";

    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    olist o__1;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsle(cilist *), e_wsle(void), s_wsfe(cilist *), 
	    do_fio(integer *, char *, ftnlen), e_wsfe(void), f_open(olist *);
    /* Subroutine */ int s_stop(char *, ftnlen);
    double cos(doublereal), exp(doublereal);

    /* Local variables */
    static integer k, kk, mm, jj, iqc;
    static real feno[12], cold, zlwd, ptot;
    extern /* Subroutine */ int radc2_(void);
    static real xco2m;
    static integer idddd;
    static real gfrac, vchec[6];
    static char cfinp[30];
    static real rhair, tprec;
    static integer ihour, imont;
    static real gprev, gpres, gpost, fenot, vnrat, co2amp, difrat;

    /* Fortran I/O blocks */
    static cilist io___35 = { 0, 0, 0, 0, 0 };
    static cilist io___36 = { 0, 0, 0, 0, 0 };
    static cilist io___38 = { 0, 0, 0, 0, 0 };
    static cilist io___39 = { 0, 0, 0, 0, 0 };
    static cilist io___40 = { 0, 98, 0, 0, 0 };
    static cilist io___41 = { 0, 98, 0, 0, 0 };
    static cilist io___42 = { 0, 0, 0, 0, 0 };
    static cilist io___43 = { 0, 0, 0, 0, 0 };
    static cilist io___45 = { 0, 6, 0, "(a25,a40)", 0 };
    static cilist io___46 = { 0, 98, 0, "(a25,a40)", 0 };
    static cilist io___47 = { 0, 0, 0, 0, 0 };
    static cilist io___50 = { 0, 0, 1, 0, 0 };
    static cilist io___52 = { 0, 0, 1, 0, 0 };
    static cilist io___56 = { 0, 6, 0, 0, 0 };
    static cilist io___57 = { 0, 98, 0, 0, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_90, 0 };



/* ======================================================================= */

/*     forcing meteo data: */
/*     data required to run sib : */

/*     nymd     : date and time (yymmddhh) */
/*     swdown   : shortwave downward radiation. */
/*     zlwd     : longwave  downward radiation. */
/*     em,tm,um : vapor pressure,temperature and wind speed */
/*                at measurement height ( or lowest model level.) */
/*     tprec    : precipitation. */

/*     subroutines called : radc2 */

/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* ...    open monthly input forcing met data / read green phenology */
/* ----------------------------------------------------------------------- */
    if (*itero == 0) {
	io___35.ciunit = *ichi;
	s_rsle(&io___35);
	e_rsle();
	io___36.ciunit = *ichi;
	s_rsle(&io___36);
	for (mm = 1; mm <= 12; ++mm) {
	    do_lio(&c__4, &c__1, (char *)&vdyijt_1.greex[mm - 1], (ftnlen)
		    sizeof(real));
	}
	e_rsle();
	io___38.ciunit = *ichi;
	s_rsle(&io___38);
	for (mm = 1; mm <= 12; ++mm) {
	    do_lio(&c__4, &c__1, (char *)&vdyijt_1.zltex[mm - 1], (ftnlen)
		    sizeof(real));
	}
	e_rsle();
	io___39.ciunit = *ichi;
	s_rsle(&io___39);
	for (mm = 1; mm <= 12; ++mm) {
	    do_lio(&c__4, &c__1, (char *)&vdyijt_1.vmex[mm - 1], (ftnlen)
		    sizeof(real));
	}
	e_rsle();
	s_wsle(&io___40);
	do_lio(&c__9, &c__1, "Greex", (ftnlen)5);
	for (mm = 1; mm <= 12; ++mm) {
	    do_lio(&c__4, &c__1, (char *)&vdyijt_1.greex[mm - 1], (ftnlen)
		    sizeof(real));
	}
	e_wsle();
	s_wsle(&io___41);
	do_lio(&c__9, &c__1, "Zltes", (ftnlen)5);
	for (mm = 1; mm <= 12; ++mm) {
	    do_lio(&c__4, &c__1, (char *)&vdyijt_1.zltex[mm - 1], (ftnlen)
		    sizeof(real));
	}
	e_wsle();
/*      write(*,*) 'passei..' ,(zltex(mm),mm=1,12) */
	io___42.ciunit = *ichi;
	s_rsle(&io___42);
	e_rsle();
	io___43.ciunit = *ichi;
	s_rsle(&io___43);
	do_lio(&c__9, &c__1, cfinp, (ftnlen)30);
	e_rsle();
	s_wsfe(&io___45);
	do_fio(&c__1, " # opening file counting ", (ftnlen)25);
	do_fio(&c__1, cfinp, (ftnlen)30);
	e_wsfe();
	s_wsfe(&io___46);
	do_fio(&c__1, " # opening file counting ", (ftnlen)25);
	do_fio(&c__1, cfinp, (ftnlen)30);
	e_wsfe();
	o__1.oerr = 0;
	o__1.ounit = *iu;
	o__1.ofnmlen = 30;
	o__1.ofnm = cfinp;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
	io___47.ciunit = *iu;
	s_rsle(&io___47);
	e_rsle();
    }
/* L851: */
/* ... */
/* --------------------------------------------------------------------- */
/*       read in meteorological forcing data */
/* --------------------------------------------------------------------- */
    flob_1.xmevap = -9999.f;
    flob_1.xmsensh = -9999.f;
    flob_1.xmgheat = -9999.f;
    flob_1.xmfco2 = -9999.f;
    flob_1.xmrsco2 = -9999.f;
    xco2m = -9999.f;
    flob_1.xmustar = -9999.f;
    flob_1.xalbedo = -9999.f;
    zlwd = -9999.f;
/* L100: */
/* ----------------------------------------------------------------------- */
/*     Defining a default format to read data2 */
/*     and check iwl flag to read */
/*     Evandro M Anselmo 16/08/2019 */
L201:
    if (steps_1.ilw != 3) {
	io___50.ciunit = *iu;
	i__1 = s_rsle(&io___50);
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(
		integer));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.swdown, (ftnlen)sizeof(
		real));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.em, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.tm, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.um, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&tprec, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = e_rsle();
	if (i__1 != 0) {
	    goto L1000;
	}
    } else {
	io___52.ciunit = *iu;
	i__1 = s_rsle(&io___52);
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(
		integer));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.swdown, (ftnlen)sizeof(
		real));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.em, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.tm, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.um, (ftnlen)sizeof(real))
		;
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&tprec, (ftnlen)sizeof(real));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = do_lio(&c__4, &c__1, (char *)&atmos_1.rnetm, (ftnlen)sizeof(
		real));
	if (i__1 != 0) {
	    goto L1000;
	}
	i__1 = e_rsle();
	if (i__1 != 0) {
	    goto L1000;
	}
    }
/* ----------------------------------------------------------------------- */
/* -----Com-rnetm------------------------------------------------------------- */
/* 201  read(iu,*,end=1000) nymd, swdown, em, tm, um, tprec, rnetm */
/* 201  read(iu,*,end=1000) nymd, swdown, rnetm, em, tm, um, tprec */
/* --------------------------------------------------------------------------- */
    if (govern2_1.nymd >= *nyfirst) {
	++(*itero);
/* incremento iteracoes apos nyfirst */
    } else {
	goto L201;
    }
/* ...  checa qualidade forcantes */
    iqc = 1;
    vchec[0] = atmos_1.swdown;
    vchec[1] = atmos_1.rnetm;
    vchec[2] = atmos_1.em;
    vchec[3] = atmos_1.tm;
    vchec[4] = atmos_1.um;
    vchec[5] = tprec;
/*      if (tprec.eq.-9999.0) tprec = 0. */
    for (k = 1; k <= 6; ++k) {
/* L55: */
	if (vchec[k - 1] == -9999.f) {
	    iqc = 0;
	}
    }
    if (iqc == 0) {
	s_wsle(&io___56);
	do_lio(&c__9, &c__1, " Forcante com erro -9999: iqc = 0 at", (ftnlen)
		36);
	do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer))
		;
	e_wsle();
	s_wsle(&io___57);
	do_lio(&c__9, &c__1, " Forcante com erro -9999: iqc = 0 at", (ftnlen)
		36);
	do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer))
		;
	e_wsle();
	s_stop("", (ftnlen)0);
    }
/* ... vegetation phenology	and co2 concentration diurnal cycle */
    ihour = govern2_1.nymd % 100;
    imont = govern2_1.nymd % 1000000 / 10000;
    idddd = govern2_1.nymd % 10000 / 100;
/* ... ico2m=  0:  pco2m constant = 34 (set in const2 every time step) */
/*            1:  pco2m read in as xco2m (have to be a forcing data) */
/*            2:  pco2m com ciclo diurno forcado */
    if (flob2_1.ico2m == 0) {
	co2amp = 0.f;
    }
    if (flob2_1.ico2m == 2) {
	co2amp = 100.f;
    }
/* amplitude (ppm) */
    atchem_1.pco2m = atchem_1.facco2 * (co2amp / 2.f * cos(const_1.pie * 2.f *
	     (real) (ihour - 8) / 24.f) + 340);
    if (flob2_1.ico2m == 1 && xco2m != -9999.f) {
	atchem_1.pco2m = xco2m;
    }
/* xco2m in ppm */
    atchem_1.pco2m *= .1f;
/* 	write(*,'(2(a10,f12.7))') 'pco2m=',pco2m,'real(ihour)',real(ihour) */
/* ..   interpola mes a mes (N - green fraction, L - LAI) */
/*     write(*,*) 'antes N ' ,(greex(mm),mm=1,12) */
/*     write(*,*) 'antes L' ,KK,(zltex(mm),mm=1,12) */
/* 0.1 ppm */
    for (kk = 1; kk <= 3; ++kk) {
	for (jj = 1; jj <= 12; ++jj) {
	    if (kk == 1) {
		feno[jj - 1] = vdyijt_1.greex[jj - 1];
	    }
	    if (kk == 2) {
		feno[jj - 1] = vdyijt_1.zltex[jj - 1];
	    }
/* L350: */
	    if (kk == 3) {
		feno[jj - 1] = vdyijt_1.vmex[jj - 1] * 1e-6f;
	    }
	}
	gprev = feno[imont - 2];
	gpres = feno[imont - 1];
	gpost = feno[imont];
	if (imont == 1) {
	    gprev = feno[11];
	}
	if (imont == 12) {
	    gpost = feno[0];
	}
	if (idddd > 15) {
	    gfrac = (real) (idddd - 15) / (real) (diames[imont - 1] - idddd + 
		    15) * .5f;
	    fenot = (1.f - gfrac) * gpres + gfrac * gpost;
	} else {
	    gfrac = (real) (15 - idddd) / (real) (idddd + 15) * .5f;
	    fenot = (1.f - gfrac) * gpres + gfrac * gprev;
	}
	if (kk == 1) {
	    vdyijt_1.green = fenot;
	}
	if (kk == 2) {
	    vdyijt_1.zlt = fenot;
	}
/* L360: */
	if (kk == 3) {
	    vderiv_1.vmax0 = fenot;
	}
    }
/* L361: */
/* --------------------------------------------------------------------- */

/*     isnow = 0 : conventional run with received met data. */

/*     isnow = 1 : shock test with initial snow dump and freezing */
/*                 temperatures at beginning of run warming to */
/*                 normal over  5-day period. */

/* ----------------------------------------------------------------------- */

    if (*isnow == 0) {
	goto L200;
    }
    if (steps_1.iter > 1) {
	goto L210;
    }
    stepv_1.tc = 270.f;
    stepv_1.tg = 270.f;
    stepv_1.snoww[1] = .1f;
L210:
/* Computing MAX */
    r__1 = 0.f, r__2 = (120.f - steps_1.iter * 1.f) / 120.f;
    cold = dmax(r__1,r__2);
    rhair = atmos_1.em / (exp(21.18123f - 5418.f / atmos_1.tm) / .622f);
    atmos_1.tm = atmos_1.tm * (1.f - cold) + (atmos_1.tm - 30.f) * cold;
    atmos_1.em = exp(21.18123f - 5418.f / atmos_1.tm) / .622f * rhair;
    if (atmos_1.em < 0.f) {
	atmos_1.em = .1f;
    }
L200:

    atmos_1.um = dmax(atmos_1.um,.25f);
/* h    ustarm = mustar/100. */
    atmos_1.swdown = dmax(atmos_1.swdown,.1f);
    ihour = govern2_1.nymd % 100;
    ptot += tprec;
    atmos_1.ppl = tprec;
    atmos_1.ppc = tprec - atmos_1.ppl;
    radc2_();
    atmos_1.cloud = (atmos_1.sunang * 1160.f - atmos_1.swdown) / (
	    atmos_1.sunang * 963.f);
    atmos_1.cloud = dmax(atmos_1.cloud,0.f);
    atmos_1.cloud = dmin(atmos_1.cloud,1.f);
    atmos_1.cloud = dmax(.58f,atmos_1.cloud);

    difrat = .0604f / (atmos_1.sunang - .0223f) + .0683f;
    if (difrat < 0.f) {
	difrat = 0.f;
    }
    if (difrat > 1.f) {
	difrat = 1.f;
    }

    difrat += (1.f - difrat) * atmos_1.cloud;
    vnrat = (580.f - atmos_1.cloud * 464.f) / (580.f - atmos_1.cloud * 499.f 
	    + (580.f - atmos_1.cloud * 464.f));

    atmos_1.radn[0] = (1.f - difrat) * vnrat * atmos_1.swdown;
    atmos_1.radn[3] = difrat * vnrat * atmos_1.swdown;
    atmos_1.radn[1] = (1.f - difrat) * (1.f - vnrat) * atmos_1.swdown;
    atmos_1.radn[4] = difrat * (1.f - vnrat) * atmos_1.swdown;
    if (steps_1.ilw == 3) {
	atmos_1.radn[5] = 0.f;
    }
    if (steps_1.ilw == 1) {
	atmos_1.radn[5] = zlwd;
    }
    if (steps_1.ilw == 1 && zlwd <= 100.f) {
	s_stop("warning: checar ilw: incompativel", (ftnlen)33);
    }
    return 0;
L1000:
    io___75.ciunit = *icho;
    s_wsfe(&io___75);
    do_fio(&c__1, (char *)&(*iu), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
    e_wsfe();
    s_stop("", (ftnlen)0);
    return 0;
} /* driver_ */

/* ======================================================================= */

/* Subroutine */ int balan_(integer *iplace, real *totwb)
{
    /* Format strings */
    static char fmt_910[] = "(//,10x,\002---> warning: energy balance violat"
	    "ion **\002,//,/,1x,\002date \002,i8,/,1x,\002rhs, lhs           "
	    "   \002,2g12.5,/,1x,\002rn1, rn2, chf, shf, h \002,5g12.5,/,1x"
	    ",\002ect, eci, egi, egs    \002,4g12.5,/,1x,\002hc        hg    "
	    "      \002,g12.5,12x,g12.5,/,1x,\002heaten, c-bal, g-bal  \002,3"
	    "g12.5)";

    /* System generated locals */
    real r__1, r__2, r__3, r__4, r__5, r__6, r__7;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer i__;
    static real cbal, gbal, zlhs, zrhs, endwb, emeter, pmeter, errore, errorw;

    /* Fortran I/O blocks */
    static cilist io___81 = { 0, 6, 0, "(a40)", 0 };
    static cilist io___82 = { 0, 6, 0, 0, 0 };
    static cilist io___83 = { 0, 98, 0, "(a40)", 0 };
    static cilist io___84 = { 0, 98, 0, 0, 0 };
    static cilist io___90 = { 0, 6, 0, "(a40)", 0 };
    static cilist io___91 = { 0, 98, 0, fmt_910, 0 };



/* ======================================================================= */

/*     energy and water balance check. */

/* ----------------------------------------------------------------------- */

/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */
    if (*iplace == 2) {
	goto L100;
    }

    donor_1.etmass = 0.f;
    donor_1.roff = 0.f;

/* 	write(98,*)'=====================================================' */
    *totwb = 0.f;
    for (i__ = 1; i__ <= 10; ++i__) {
/* 	write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i) */
/* L20: */
	*totwb += stepv_1.www[i__ - 1] * soils_1.poros[i__ - 1] * 
		soils_1.zdepth[i__ - 1];
    }
    *totwb += stepv_1.capac[0] + stepv_1.capac[1] + stepv_1.snoww[0] + 
	    stepv_1.snoww[1];
/*      totwb1 = dble(totwb) */
/*      write(98,'(1x,a10,2a13,/,1x,f12.5,1x,f12.10,1x,f12.5)') */
/*     .    ' S ',' M ',' To1', */
/*     .   totwb1*1000., */
/*     .   (capac(1)+capac(2)+snoww(1)+snoww(2))*1000., totwb*1000. */

    goto L200;

L100:

    endwb = 0.f;
    for (i__ = 1; i__ <= 10; ++i__) {
/* L40: */
	endwb += stepv_1.www[i__ - 1] * soils_1.poros[i__ - 1] * 
		soils_1.zdepth[i__ - 1];
    }
    endwb = endwb + (stepv_1.capac[0] + stepv_1.capac[1] + stepv_1.snoww[0] + 
	    stepv_1.snoww[1]) - (atmos_1.ppl + atmos_1.ppc) / 1e3f + 
	    donor_1.etmass / 1e3f + donor_1.roff;
/*      errorw= totwb1 - dble(endwb) */
    errorw = *totwb - endwb;
    pmeter = (atmos_1.ppl + atmos_1.ppc) / 1e3f;
    emeter = donor_1.etmass / 1e3f;

    if (dabs(errorw) > 1e-4f) {
	s_wsfe(&io___81);
	do_fio(&c__1, " ---> warning: water balance violation", (ftnlen)38);
	e_wsfe();
	s_wsle(&io___82);
	do_lio(&c__9, &c__1, "nymd , abs(errorw)= ", (ftnlen)20);
	do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer))
		;
	r__1 = dabs(errorw);
	do_lio(&c__4, &c__1, (char *)&r__1, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___83);
	do_fio(&c__1, " ---> warning: water balance violation", (ftnlen)38);
	e_wsfe();
	s_wsle(&io___84);
	do_lio(&c__9, &c__1, "nymd , abs(errorw)= ", (ftnlen)20);
	do_lio(&c__3, &c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer))
		;
	r__1 = dabs(errorw);
	do_lio(&c__4, &c__1, (char *)&r__1, (ftnlen)sizeof(real));
	e_wsle();
/* 	do 35 i=1,nlayer */
/* 35	write(98,'(1x,a3,1x,i3,1x,f20.10)') '  w',i,www(i) */
/*      write(98,'(1x,a10,5a13,/,1x,f12.5,2(1x,f12.10),3(1x,f12.5))') */
/*     .  ' S ',' M ',' E',' R',' -Pr', */
/*     .' To2', endwb1*1000.,(capac(1)+capac(2)+snoww(1)+snoww(2))*1000., */
/*     . dble(etmass), dble(roff)*1000.,-dble(ppl+ppc),endwb*1000. */
/*      write(98,'(1x,a20,1x,f20.10)')'Error To1-To2(mm):',errorw*1000.d0 */
/*      write(98,*) */
    }

    cbal = radabs_1.radt[0] - flux_1.chf - (flux_1.ect + flux_1.hc + 
	    flux_1.eci) / steps_1.dtt;
    gbal = radabs_1.radt[1] - flux_1.shf - (flux_1.egs + flux_1.hg + 
	    flux_1.egi) / steps_1.dtt - flux_1.heaten / steps_1.dtt;
    zlhs = radabs_1.radt[0] + radabs_1.radt[1] - flux_1.chf - flux_1.shf;
    zrhs = donor_1.hflux + (flux_1.ect + flux_1.eci + flux_1.egi + flux_1.egs)
	     / steps_1.dtt + flux_1.heaten / steps_1.dtt;

    errore = zlhs - zrhs;

    if (dabs(errore) > 1.f) {
	s_wsfe(&io___90);
	do_fio(&c__1, " ---> warning: energy balance violation", (ftnlen)39);
	e_wsfe();
	s_wsfe(&io___91);
	do_fio(&c__1, (char *)&govern2_1.nymd, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&zlhs, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&zrhs, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&radabs_1.radt[0], (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&radabs_1.radt[1], (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&flux_1.chf, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&flux_1.shf, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&donor_1.hflux, (ftnlen)sizeof(real));
	r__1 = flux_1.ect / steps_1.dtt;
	do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	r__2 = flux_1.eci / steps_1.dtt;
	do_fio(&c__1, (char *)&r__2, (ftnlen)sizeof(real));
	r__3 = flux_1.egi / steps_1.dtt;
	do_fio(&c__1, (char *)&r__3, (ftnlen)sizeof(real));
	r__4 = flux_1.egs / steps_1.dtt;
	do_fio(&c__1, (char *)&r__4, (ftnlen)sizeof(real));
	r__5 = flux_1.hc / steps_1.dtt;
	do_fio(&c__1, (char *)&r__5, (ftnlen)sizeof(real));
	r__6 = flux_1.hg / steps_1.dtt;
	do_fio(&c__1, (char *)&r__6, (ftnlen)sizeof(real));
	r__7 = flux_1.heaten / steps_1.dtt;
	do_fio(&c__1, (char *)&r__7, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&cbal, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&gbal, (ftnlen)sizeof(real));
	e_wsfe();
    }

L200:
    return 0;
} /* balan_ */


/* ======================================================================= */


/* Subroutine */ int veginc_(integer *ichi)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), 
	    e_wsfe(void), s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer iwave;
    extern /* Subroutine */ int varcal_(integer *), vegpar_(integer *), 
	    soipar_(integer *), dynveg_(integer *);

    /* Fortran I/O blocks */
    static cilist io___93 = { 0, 0, 0, 0, 0 };
    static cilist io___94 = { 0, 0, 0, 0, 0 };
    static cilist io___95 = { 0, 0, 0, 0, 0 };
    static cilist io___96 = { 0, 0, 0, 0, 0 };
    static cilist io___97 = { 0, 0, 0, 0, 0 };
    static cilist io___98 = { 0, 0, 0, 0, 0 };
    static cilist io___99 = { 0, 0, 0, 0, 0 };
    static cilist io___101 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___102 = { 0, 98, 0, 0, 0 };
    static cilist io___103 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___104 = { 0, 98, 0, 0, 0 };



/* ======================================================================= */

/*    read vegetation and soil parameters for SIB2 */
/* ----------------------------------------------------------------------- */
/*    subroutines called :  vegpar */
/*                          soipar */
/*                          dynveg */
/*                          varcal */

/* ----------------------------------------------------------------------- */


/*        subscripts (iv, iw, il) : */

/*              iv  : surface layer ; */
/*                1 = canopy */
/*                2 = ground */
/*              iw  : radiation wavelength; */
/*                1 = visible */
/*                2 = near infrared */
/*              il  : vegetation state; */
/*                1 = live (green) and */
/*                2 = dead (stems and trunk) */

/* ----------------------------------------------------------------------- */

/*                 input parameter set */

/* ----------------------------------------------------------------------- */

/*   ivtype        : vegetation type */

/*        static parameters associated with vegetation type */
/*        ------------------------------------------------- */

/*   z2            : canopy top height */
/*   z1            : canopy base height */
/*   vcover        : vegetation cover fraction */
/*   chil          : leaf angle distribution factor */
/*   rootd         : rooting depth */
/*   phc           : 1/2 critical leaf water potential limit */
/*   tran(iw,il)   : leaf transmittance */
/*   ref (iw,il)   : leaf reflectance */
/*   effcon        : quantum efficiency */
/*   gradm         : conductance-photosynthesis slope parameter */
/*   binter        : conductance-photosynthesis intercept */
/*   respcp        : respiration fraction of vmax */
/*   atheta        : wc, we coupling parameter */
/*   btheta        : wc & we, ws coupling parameter */
/*   trda          : temperature coefficient in gs-a model */
/*   trdm          : temperature coefficient in gs-a model */
/*   trop          : temperature coefficient in gs-a model */
/*   slti          : slope of low temperature inhibition function */
/*   hlti          : 1/2 point of low temperature inhibition function */
/*   shti          : slope of high temperature inhibition function */
/*   hhti          : 1/2 point of high temperature inhibition function */

/*   istype        : soil type */

/*        static parameters associated with soil type */
/*        ------------------------------------------- */

/*   sodep         : total depth of 3 soil moisture layers */
/*   soref(iw)     : soil reflectance */
/*   bee           : soil wetness exponent */
/*   phsat         : soil tension at saturation */
/*   satco         : hydraulic conductivity at saturation */
/*   poros         : soil porosity */
/*   slope         : cosine of mean slope */

/*        time-space varying vegetation parameters, from */
/*        spectral vegetation indice (svi). */
/*        -------------------------------------------------- */

/*   zlt           : leaf area index */
/*   green         : green leaf fraction */
/*   fparc         : canopy absorbed fraction of photosynthetically */
/*                 : active radiation (par)  from svi */

/*        parameters derived from the above */
/*        --------------------------------- */

/*   vmax0         : rubisco velocity of sun-leaf */
/*   gmudmu        : time-mean leaf projection ( g(mu)/ mu ) */
/*   z0d           : roughness length */
/*   dd            : zero plane displacement */
/*   cc1           : rb coefficient (c1) */
/*   cc2           : rd coefficient (c2) */
/*   zdepth        : individual depths of 3 soil moisture layers */

/* ----------------------------------------------------------------------- */

/*        other variables */
/*        --------------- */

/*      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet */

/*   g1            : ratio of km(actual) to km(log-linear) at z = z2 */
/*   g2            : ratio of ra(actual) to ra(log-linear) for momentum */
/*                   between: z = z2 and z = zx, where zx = min(zl,zwind) */
/*   g3            : ratio of ra(actual) to ra(log-linear) for heat */
/*                   between: z = z2 and z = zx, where zx = min(zl,zmet) */
/*   ztz0          : parameter to determine depth of transition layer */
/*                   above canopy, zl. zl = z2 + ztz0 * z0 */
/*   corb1         : non-neutral correction for calculation of aerodynami */
/*                   resistance between ha and z2. when multiplied by */
/*                   h*rbb/tm gives bulk estimate of local richardson */
/*                   number */
/*                   rbb = ra for heat between ha and z2. */
/*                   corb2 = 9*g/( rhoair*cpair* (du/dz)**2 ) */
/*   corb2         : neutral value of rbb*u2 ( squared ), equivalent to */
/*                   rdc**2 for upper canopy */
/*   ha            : canopy source height for heat */
/*   zwind         : reference height for wind measurement */
/*   zmet          : reference height for temperature, humidity */
/*                   measurement */

/*        the above are generated from sibx + momopt output */

/* ----------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

    io___93.ciunit = *ichi;
    s_rsle(&io___93);
    e_rsle();
    io___94.ciunit = *ichi;
    s_rsle(&io___94);
    e_rsle();
    io___95.ciunit = *ichi;
    s_rsle(&io___95);
    e_rsle();

    io___96.ciunit = *ichi;
    s_rsle(&io___96);
    do_lio(&c__3, &c__1, (char *)&gridij_1.ivtype, (ftnlen)sizeof(integer));
    e_rsle();
    vegpar_(ichi);

    io___97.ciunit = *ichi;
    s_rsle(&io___97);
    e_rsle();
    io___98.ciunit = *ichi;
    s_rsle(&io___98);
    e_rsle();
    io___99.ciunit = *ichi;
    s_rsle(&io___99);
    do_lio(&c__3, &c__1, (char *)&gridij_1.istype, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&soilij_1.sodep, (ftnlen)sizeof(real));
    for (iwave = 1; iwave <= 2; ++iwave) {
	do_lio(&c__4, &c__1, (char *)&soilij_1.soref[iwave - 1], (ftnlen)
		sizeof(real));
    }
    e_rsle();
    if (pfirst) {
	pfirst = FALSE_;
/* L50: */
	s_wsfe(&io___101);
	do_fio(&c__1, " ivtype", (ftnlen)7);
	e_wsfe();
	s_wsle(&io___102);
	do_lio(&c__3, &c__1, (char *)&gridij_1.ivtype, (ftnlen)sizeof(integer)
		);
	e_wsle();
	s_wsfe(&io___103);
	do_fio(&c__1, " istype, sodep,  (soref(iwave),iwave=1,2)", (ftnlen)41)
		;
	e_wsfe();
	s_wsle(&io___104);
	do_lio(&c__3, &c__1, (char *)&gridij_1.istype, (ftnlen)sizeof(integer)
		);
	do_lio(&c__4, &c__1, (char *)&soilij_1.sodep, (ftnlen)sizeof(real));
	for (iwave = 1; iwave <= 2; ++iwave) {
	    do_lio(&c__4, &c__1, (char *)&soilij_1.soref[iwave - 1], (ftnlen)
		    sizeof(real));
	}
	e_wsle();
    }
    soipar_(ichi);

    dynveg_(ichi);

    varcal_(ichi);

    return 0;
} /* veginc_ */


/* ======================================================================= */

/* Subroutine */ int vegpar_(integer *ichi)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), 
	    e_wsfe(void), s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer iw;

    /* Fortran I/O blocks */
    static cilist io___106 = { 0, 0, 0, 0, 0 };
    static cilist io___107 = { 0, 0, 0, 0, 0 };
    static cilist io___108 = { 0, 0, 0, 0, 0 };
    static cilist io___109 = { 0, 0, 0, 0, 0 };
    static cilist io___110 = { 0, 0, 0, 0, 0 };
    static cilist io___111 = { 0, 0, 0, 0, 0 };
    static cilist io___112 = { 0, 0, 0, 0, 0 };
    static cilist io___113 = { 0, 0, 0, 0, 0 };
    static cilist io___115 = { 0, 0, 0, 0, 0 };
    static cilist io___116 = { 0, 0, 0, 0, 0 };
    static cilist io___117 = { 0, 0, 0, 0, 0 };
    static cilist io___118 = { 0, 0, 0, 0, 0 };
    static cilist io___119 = { 0, 0, 0, 0, 0 };
    static cilist io___120 = { 0, 0, 0, 0, 0 };
    static cilist io___121 = { 0, 0, 0, 0, 0 };
    static cilist io___122 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___123 = { 0, 98, 0, 0, 0 };
    static cilist io___124 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___125 = { 0, 98, 0, 0, 0 };
    static cilist io___126 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___127 = { 0, 98, 0, 0, 0 };
    static cilist io___128 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___129 = { 0, 98, 0, 0, 0 };
    static cilist io___130 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___131 = { 0, 98, 0, 0, 0 };
    static cilist io___132 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___133 = { 0, 98, 0, 0, 0 };
    static cilist io___134 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___135 = { 0, 98, 0, 0, 0 };



/* ======================================================================= */

/*     reading/setting of vegetation-type dependent static parameters. */

/* ----------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

    io___106.ciunit = *ichi;
    s_rsle(&io___106);
    e_rsle();
    io___107.ciunit = *ichi;
    s_rsle(&io___107);
    e_rsle();
    io___108.ciunit = *ichi;
    s_rsle(&io___108);
    do_lio(&c__4, &c__1, (char *)&vstate_1.z2, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.z1, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.vcover, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.chil, (ftnlen)sizeof(real));
    e_rsle();
    io___109.ciunit = *ichi;
    s_rsle(&io___109);
    e_rsle();
    io___110.ciunit = *ichi;
    s_rsle(&io___110);
    do_lio(&c__4, &c__1, (char *)&vstate_1.rootd, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.phc, (ftnlen)sizeof(real));
    e_rsle();

    io___111.ciunit = *ichi;
    s_rsle(&io___111);
    e_rsle();
    io___112.ciunit = *ichi;
    s_rsle(&io___112);
    e_rsle();
    io___113.ciunit = *ichi;
    s_rsle(&io___113);
    for (iw = 1; iw <= 2; ++iw) {
	do_lio(&c__4, &c__1, (char *)&vstate_1.tran[iw - 1], (ftnlen)sizeof(
		real));
    }
    for (iw = 1; iw <= 2; ++iw) {
	do_lio(&c__4, &c__1, (char *)&vstate_1.tran[iw + 1], (ftnlen)sizeof(
		real));
    }
    e_rsle();
/* (iw,iliv */
    io___115.ciunit = *ichi;
    s_rsle(&io___115);
    for (iw = 1; iw <= 2; ++iw) {
	do_lio(&c__4, &c__1, (char *)&vstate_1.ref[iw - 1], (ftnlen)sizeof(
		real));
    }
    for (iw = 1; iw <= 2; ++iw) {
	do_lio(&c__4, &c__1, (char *)&vstate_1.ref[iw + 1], (ftnlen)sizeof(
		real));
    }
    e_rsle();

    io___116.ciunit = *ichi;
    s_rsle(&io___116);
    e_rsle();
    io___117.ciunit = *ichi;
    s_rsle(&io___117);
    do_lio(&c__4, &c__1, (char *)&vstate_1.effcon, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.gradm, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.binter, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.respcp, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.atheta, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.btheta, (ftnlen)sizeof(real));
    e_rsle();
    io___118.ciunit = *ichi;
    s_rsle(&io___118);
    e_rsle();
    io___119.ciunit = *ichi;
    s_rsle(&io___119);
    do_lio(&c__4, &c__1, (char *)&vstate_1.trda, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.trdm, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.trop, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.slti, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.hlti, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.shti, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstate_1.hhti, (ftnlen)sizeof(real));
    e_rsle();
    io___120.ciunit = *ichi;
    s_rsle(&io___120);
    e_rsle();
    io___121.ciunit = *ichi;
    s_rsle(&io___121);
    do_lio(&c__4, &c__1, (char *)&respva_1.acoef, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&respva_1.bcoef, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&respva_1.ccoef, (ftnlen)sizeof(real));
    e_rsle();

    if (pfirst) {
	pfirst = FALSE_;
/* L50: */
	s_wsfe(&io___122);
	do_fio(&c__1, " z2, z1, vcover, chil", (ftnlen)21);
	e_wsfe();
	s_wsle(&io___123);
	do_lio(&c__4, &c__1, (char *)&vstate_1.z2, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.z1, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.vcover, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.chil, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___124);
	do_fio(&c__1, " rootd, phc ", (ftnlen)12);
	e_wsfe();
	s_wsle(&io___125);
	do_lio(&c__4, &c__1, (char *)&vstate_1.rootd, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.phc, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___126);
	do_fio(&c__1, " (tran(iw,2), iw=1,2)", (ftnlen)21);
	e_wsfe();
	s_wsle(&io___127);
	for (iw = 1; iw <= 2; ++iw) {
	    do_lio(&c__4, &c__1, (char *)&vstate_1.tran[iw - 1], (ftnlen)
		    sizeof(real));
	}
	e_wsle();
	s_wsfe(&io___128);
	do_fio(&c__1, " (ref (iw,2), iw=1,2)", (ftnlen)21);
	e_wsfe();
	s_wsle(&io___129);
	for (iw = 1; iw <= 2; ++iw) {
	    do_lio(&c__4, &c__1, (char *)&vstate_1.ref[iw + 1], (ftnlen)
		    sizeof(real));
	}
	e_wsle();
	s_wsfe(&io___130);
	do_fio(&c__1, "effcon, gradm, binter, respcp, atheta, btheta", (
		ftnlen)45);
	e_wsfe();
	s_wsle(&io___131);
	do_lio(&c__4, &c__1, (char *)&vstate_1.effcon, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.gradm, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.binter, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.respcp, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.atheta, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.btheta, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___132);
	do_fio(&c__1, " trda, trdm, trop, slti, hlti, shti, hhti", (ftnlen)41)
		;
	e_wsfe();
	s_wsle(&io___133);
	do_lio(&c__4, &c__1, (char *)&vstate_1.trda, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.trdm, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.trop, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.slti, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.hlti, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.shti, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vstate_1.hhti, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___134);
	do_fio(&c__1, " acoef, bcoef, ccoef", (ftnlen)20);
	e_wsfe();
	s_wsle(&io___135);
	do_lio(&c__4, &c__1, (char *)&respva_1.acoef, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&respva_1.bcoef, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&respva_1.ccoef, (ftnlen)sizeof(real));
	e_wsle();
    }
    return 0;
} /* vegpar_ */


/* ======================================================================= */

/* Subroutine */ int soipar_(integer *ichi)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* Format strings */
    static char fmt_29[] = "(a32,f20.10,a3)";

    /* System generated locals */
    real r__1;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), 
	    e_wsfe(void), s_wsle(cilist *), e_wsle(void);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer i__, il;
    static real extmax;

    /* Fortran I/O blocks */
    static cilist io___137 = { 0, 0, 0, 0, 0 };
    static cilist io___138 = { 0, 0, 0, 0, 0 };
    static cilist io___139 = { 0, 0, 0, 0, 0 };
    static cilist io___140 = { 0, 0, 0, 0, 0 };
    static cilist io___142 = { 0, 0, 0, 0, 0 };
    static cilist io___145 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___146 = { 0, 98, 0, 0, 0 };
    static cilist io___147 = { 0, 98, 0, "(a60)", 0 };
    static cilist io___148 = { 0, 98, 0, "(3(1x,f6.4),2(1x,e10.3),3(1x,f6.4)"
	    ",/)", 0 };
    static cilist io___149 = { 0, 98, 0, 0, 0 };
    static cilist io___150 = { 0, 6, 0, fmt_29, 0 };
    static cilist io___151 = { 0, 98, 0, fmt_29, 0 };



/* ======================================================================= */

/*     reading/setting of soil-type dependent static parameters. */

/* ----------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

    io___137.ciunit = *ichi;
    s_rsle(&io___137);
    e_rsle();
    io___138.ciunit = *ichi;
    s_rsle(&io___138);
    e_rsle();
    io___139.ciunit = *ichi;
    s_rsle(&io___139);
    do_lio(&c__3, &c__1, (char *)&soimul_1.iinf, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&soils_1.slope, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&soimul_1.xcs, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jqng, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&soimul_1.xkb, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jesq, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jdpsi, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jkcon, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jsys, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jjgs, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&soimul_1.jqini, (ftnlen)sizeof(integer));
    e_rsle();
    io___140.ciunit = *ichi;
    s_rsle(&io___140);
    e_rsle();
    for (i__ = 1; i__ <= 10; ++i__) {
/* L10: */
	io___142.ciunit = *ichi;
	s_rsle(&io___142);
	do_lio(&c__4, &c__1, (char *)&soils_1.zdepth[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&soils_1.extfrac[i__ - 1], (ftnlen)
		sizeof(real));
	do_lio(&c__4, &c__1, (char *)&soils_1.bee[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&soils_1.phsat[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&soils_1.satco[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&soils_1.poros[i__ - 1], (ftnlen)sizeof(
		real));
	do_lio(&c__4, &c__1, (char *)&stepv_1.www[i__ - 1], (ftnlen)sizeof(
		real));
	e_rsle();
    }
    extmax = 0.f;
    for (il = 2; il <= 10; ++il) {
/* L20: */
	extmax += soils_1.extfrac[il - 1];
    }
    if (pfirst) {
	pfirst = FALSE_;
	s_wsfe(&io___145);
	do_fio(&c__1, " iinf  slope ", (ftnlen)13);
	e_wsfe();
	s_wsle(&io___146);
	do_lio(&c__3, &c__1, (char *)&soimul_1.iinf, (ftnlen)sizeof(integer));
	do_lio(&c__4, &c__1, (char *)&soils_1.slope, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___147);
	do_fio(&c__1, "zdepth(i) extfrac(i) bee(i) phsat(i) satco(i) poros(i"
		") www(i)", (ftnlen)61);
	e_wsfe();
	for (i__ = 1; i__ <= 10; ++i__) {
/* L19: */
	    s_wsfe(&io___148);
	    do_fio(&c__1, (char *)&soils_1.zdepth[i__ - 1], (ftnlen)sizeof(
		    real));
	    do_fio(&c__1, (char *)&soils_1.extfrac[i__ - 1], (ftnlen)sizeof(
		    real));
	    do_fio(&c__1, (char *)&soils_1.bee[i__ - 1], (ftnlen)sizeof(real))
		    ;
	    do_fio(&c__1, (char *)&soils_1.phsat[i__ - 1], (ftnlen)sizeof(
		    real));
	    do_fio(&c__1, (char *)&soils_1.satco[i__ - 1], (ftnlen)sizeof(
		    real));
	    do_fio(&c__1, (char *)&soils_1.poros[i__ - 1], (ftnlen)sizeof(
		    real));
	    do_fio(&c__1, (char *)&stepv_1.www[i__ - 1], (ftnlen)sizeof(real))
		    ;
	    e_wsfe();
	}
	s_wsle(&io___149);
	do_lio(&c__9, &c__1, " extmax(%): ", (ftnlen)12);
	r__1 = extmax * 100.f;
	do_lio(&c__4, &c__1, (char *)&r__1, (ftnlen)sizeof(real));
	e_wsle();
	if (extmax > 1.00001f || soils_1.extfrac[0] < 1.f) {
	    s_wsfe(&io___150);
	    do_fio(&c__1, "!!! warning: extfrac violation ", (ftnlen)31);
	    r__1 = extmax * 100.f;
	    do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	    do_fio(&c__1, " %", (ftnlen)2);
	    e_wsfe();
	    s_wsfe(&io___151);
	    do_fio(&c__1, "!!! warning: extfrac violation ", (ftnlen)31);
	    r__1 = extmax * 100.f;
	    do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	    do_fio(&c__1, " %", (ftnlen)2);
	    e_wsfe();
	    s_stop("", (ftnlen)0);
	}
    }
/* ... */
/* if pfirst */
    return 0;
} /* soipar_ */


/* ======================================================================= */

/* Subroutine */ int dynveg_(integer *ichi)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), 
	    e_wsfe(void), s_wsle(cilist *), e_wsle(void);

    /* Fortran I/O blocks */
    static cilist io___153 = { 0, 0, 0, 0, 0 };
    static cilist io___154 = { 0, 0, 0, 0, 0 };
    static cilist io___155 = { 0, 0, 0, 0, 0 };
    static cilist io___156 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___157 = { 0, 98, 0, 0, 0 };



/* ======================================================================= */

/*     reading of phenological vegetation parameters. */

/* ----------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

    io___153.ciunit = *ichi;
    s_rsle(&io___153);
    e_rsle();
    io___154.ciunit = *ichi;
    s_rsle(&io___154);
    e_rsle();
    io___155.ciunit = *ichi;
    s_rsle(&io___155);
    do_lio(&c__4, &c__1, (char *)&vdyijt_1.fparc, (ftnlen)sizeof(real));
    e_rsle();
    if (pfirst) {
	pfirst = FALSE_;
	s_wsfe(&io___156);
	do_fio(&c__1, " fparc", (ftnlen)6);
	e_wsfe();
	s_wsle(&io___157);
	do_lio(&c__4, &c__1, (char *)&vdyijt_1.fparc, (ftnlen)sizeof(real));
	e_wsle();
/* L50: */
    }

    return 0;
} /* dynveg_ */

/* ======================================================================= */

/* Subroutine */ int varcal_(integer *ichi)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen);
    double sqrt(doublereal), exp(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), e_wsle(void);

    /* Local variables */
    static integer i__;
    static real park, scatp, sodep1;

    /* Fortran I/O blocks */
    static cilist io___159 = { 0, 0, 0, 0, 0 };
    static cilist io___160 = { 0, 0, 0, 0, 0 };
    static cilist io___161 = { 0, 0, 0, 0, 0 };
    static cilist io___162 = { 0, 0, 0, 0, 0 };
    static cilist io___163 = { 0, 0, 0, 0, 0 };
    static cilist io___164 = { 0, 0, 0, 0, 0 };
    static cilist io___165 = { 0, 0, 0, 0, 0 };
    static cilist io___166 = { 0, 0, 0, 0, 0 };
    static cilist io___171 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___172 = { 0, 98, 0, 0, 0 };
    static cilist io___173 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___174 = { 0, 98, 0, 0, 0 };
    static cilist io___175 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___176 = { 0, 98, 0, 0, 0 };
    static cilist io___177 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___178 = { 0, 98, 0, 0, 0 };
    static cilist io___179 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___180 = { 0, 98, 0, 0, 0 };
    static cilist io___181 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___182 = { 0, 98, 0, 0, 0 };
    static cilist io___183 = { 0, 98, 0, "(a72)", 0 };
    static cilist io___184 = { 0, 98, 0, 0, 0 };



/* ======================================================================= */

/*     calculation of secondary parameters from input data */

/* ----------------------------------------------------------------------- */
/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

    io___159.ciunit = *ichi;
    s_rsle(&io___159);
    e_rsle();
    io___160.ciunit = *ichi;
    s_rsle(&io___160);
    e_rsle();
    io___161.ciunit = *ichi;
    s_rsle(&io___161);
    do_lio(&c__4, &c__1, (char *)&vderiv_1.vmax0, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vderiv_1.gmudmu, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vdyijt_1.green, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vdyijt_1.zlt, (ftnlen)sizeof(real));
    e_rsle();
    io___162.ciunit = *ichi;
    s_rsle(&io___162);
    e_rsle();
    io___163.ciunit = *ichi;
    s_rsle(&io___163);
    do_lio(&c__4, &c__1, (char *)&vderiv_1.z0d, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vderiv_1.dd, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vderiv_1.cc1, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vderiv_1.cc2, (ftnlen)sizeof(real));
    e_rsle();
    io___164.ciunit = *ichi;
    s_rsle(&io___164);
    e_rsle();
    io___165.ciunit = *ichi;
    s_rsle(&io___165);
    e_rsle();
    io___166.ciunit = *ichi;
    s_rsle(&io___166);
    do_lio(&c__4, &c__1, (char *)&caerod_1.corb1, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.corb2, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.ha, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.g1, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.g2, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.g3, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.ztz0, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.zwind, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.zmet, (ftnlen)sizeof(real));
    e_rsle();
/*      write(*,'(a72)') ' vmax0, gmudmu, green, zlt' */
/*      write(*,*) vmax0, gmudmu, green, zlt */
/*      write(*,'(a72)') ' z0d, dd, cc1, cc2' */
/*      write(*,*) z0d, dd, cc1, cc2 */
/*      write(*,'(a72)')'corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet' */
/*      write(*,*) corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet */
/* new... */
    sodep1 = 0.f;
    for (i__ = 1; i__ <= 10; ++i__) {
/* L10: */
	sodep1 += soils_1.zdepth[i__ - 1];
    }
/* new... */
/* Computing MIN */
    r__1 = vstate_1.rootd, r__2 = sodep1 * .75f;
    vstate_1.rootd = dmin(r__1,r__2);
    scatp = vdyijt_1.green * (vstate_1.tran[0] + vstate_1.ref[0]) + (1.f - 
	    vdyijt_1.green) * (vstate_1.tran[2] + vstate_1.ref[2]);
    park = sqrt(1.f - scatp) * vderiv_1.gmudmu;
/*     fparc = 1. - exp ( -park*zlt ) */
    vdyijt_1.fparc = 1.f - exp(-park * vdyijt_1.zlt);
/* h007      zlt = -1./park*alog( 1.-fparc ) */
/*      write(*,'(a72)') ' rootd ' */
/*      write(*,*) rootd */
/*      write(*,'(a72)') ' scatp ' */
/*      write(*,*) scatp */
/*      write(*,'(a72)') ' park ' */
/*      write(*,*) park */
/*      write(*,'(a72)') ' zlt' */
/*      write(*,*) zlt */
    if (pfirst) {
	pfirst = FALSE_;
/* L50: */
	s_wsfe(&io___171);
	do_fio(&c__1, " vmax0, gmudmu, green, zlt", (ftnlen)26);
	e_wsfe();
	s_wsle(&io___172);
	do_lio(&c__4, &c__1, (char *)&vderiv_1.vmax0, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vderiv_1.gmudmu, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vdyijt_1.green, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vdyijt_1.zlt, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___173);
	do_fio(&c__1, " z0d, dd, cc1, cc2", (ftnlen)18);
	e_wsfe();
	s_wsle(&io___174);
	do_lio(&c__4, &c__1, (char *)&vderiv_1.z0d, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vderiv_1.dd, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vderiv_1.cc1, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&vderiv_1.cc2, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___175);
	do_fio(&c__1, "corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet", (
		ftnlen)47);
	e_wsfe();
	s_wsle(&io___176);
	do_lio(&c__4, &c__1, (char *)&caerod_1.corb1, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.corb2, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.ha, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.g1, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.g2, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.g3, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.ztz0, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.zwind, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&caerod_1.zmet, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___177);
	do_fio(&c__1, " rootd ", (ftnlen)7);
	e_wsfe();
	s_wsle(&io___178);
	do_lio(&c__4, &c__1, (char *)&vstate_1.rootd, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___179);
	do_fio(&c__1, " scatp ", (ftnlen)7);
	e_wsfe();
	s_wsle(&io___180);
	do_lio(&c__4, &c__1, (char *)&scatp, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___181);
	do_fio(&c__1, " park ", (ftnlen)6);
	e_wsfe();
	s_wsle(&io___182);
	do_lio(&c__4, &c__1, (char *)&park, (ftnlen)sizeof(real));
	e_wsle();
	s_wsfe(&io___183);
	do_fio(&c__1, " zlt", (ftnlen)4);
	e_wsfe();
	s_wsle(&io___184);
	do_lio(&c__4, &c__1, (char *)&vdyijt_1.zlt, (ftnlen)sizeof(real));
	e_wsle();
    }
    return 0;
} /* varcal_ */

/* ======================================================================= */

/* Subroutine */ int cntrol_(integer *ichi, integer *icho, integer *maxit, 
	integer *nylast, integer *nyfirst)
{
    /* Initialized data */

    static logical pfirst = TRUE_;

    /* Format strings */
    static char fmt_800[] = "(10x,32(\002*\002)/10x,\002*       SiB2 off-lin"
	    "e run      *\002/10x,32(\002*\002)/5x,\002latitude : \002,f6.2,5"
	    "x,\002 longitude : \002,f7.2/,4x,\002 run length: nyfirst - nyla"
	    "st\002,4x,i8,4x,i8,/,4x,\002             maxit           \002,4x"
	    ",i8,/)";
    static char fmt_801[] = "(5x,\002resistances calculated from initial flu"
	    "xes\002)";
    static char fmt_802[] = "(5x,\002resistances calculated by iteration, it"
	    "runk=\002,i4)";
    static char fmt_816[] = "(5x,\002downward longwave radiation read in as "
	    "data\002)";
    static char fmt_817[] = "(5x,\002downward longwave radiation computed fr"
	    "om brunts\002,\002 equation\002)";
    static char fmt_818[] = "(5x,\002downward longwave radiation computed as"
	    " residual in energy balance\002,/,5x,\002net radiation read in a"
	    "s data \002)";

    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    integer s_rsle(cilist *), e_rsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen), s_wsle(cilist *), e_wsle(void), s_wsfe(cilist *), 
	    do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer i__;
    static real qa, ydep[10], eacum[10];

    /* Fortran I/O blocks */
    static cilist io___186 = { 0, 0, 0, 0, 0 };
    static cilist io___187 = { 0, 0, 0, 0, 0 };
    static cilist io___188 = { 0, 0, 0, 0, 0 };
    static cilist io___189 = { 0, 0, 0, 0, 0 };
    static cilist io___190 = { 0, 0, 0, 0, 0 };
    static cilist io___191 = { 0, 0, 0, 0, 0 };
    static cilist io___192 = { 0, 0, 0, 0, 0 };
    static cilist io___194 = { 0, 98, 0, 0, 0 };
    static cilist io___195 = { 0, 0, 0, fmt_800, 0 };
    static cilist io___196 = { 0, 0, 0, fmt_801, 0 };
    static cilist io___197 = { 0, 0, 0, fmt_802, 0 };
    static cilist io___198 = { 0, 0, 0, fmt_816, 0 };
    static cilist io___199 = { 0, 0, 0, fmt_817, 0 };
    static cilist io___200 = { 0, 0, 0, fmt_818, 0 };
    static cilist io___201 = { 0, 6, 0, "(a28,/,3(5x,a25,2x,f9.4,/),10(5x,a2"
	    "5,2x,i2,/))", 0 };
    static cilist io___205 = { 0, 6, 0, "(a44,/,50(i3,3x,f6.0,3x,f7.2,3x,f7."
	    "2,3x,f7.3,/))", 0 };
    static cilist io___206 = { 0, 98, 0, 0, 0 };



/* ======================================================================= */

/*      initialisation and switches. */

/* ----------------------------------------------------------------------- */

/* ======================================================================= */
/*                       comsibc.h */
/* ======================================================================= */

/*     sib common block : 1-d version ( carbon ) */
/*                                                              may 1991 */
/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*        prognostic variables */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        physical constants */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        vegetation : static, dynamic, derived parameters */
/* ----------------------------------------------------------------------- */
/* xx  &                 ,trda, trdm, trop, tpsa, tpsb, tpsc */
/* xx  &                 rootd, ph1, ph2 */
/* new */
/* ----------------------------------------------------------------------- */
/*        soils : space-varying, type-dependent parameters */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        input atmospheric and site data */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        site parameters specific to 1-d model operation */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m), */
/*                                       lw(w m-2), drag(kg m-1 s-2) ) */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        variables calculated from above and ambient conditions */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/*        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2 */
/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* HR  Observed & calculated fluxes */
/* HR  soil mlayer model */
/* HR  output files */
/* ----------------------------------------------------------------------- */

/*       write(98,*)'  entrando cntrol' */
/*       write(*,*)'  entrando cntrol' */
/* L900: */
    io___186.ciunit = *ichi;
    s_rsle(&io___186);
    e_rsle();
    io___187.ciunit = *ichi;
    s_rsle(&io___187);
    e_rsle();
    io___188.ciunit = *ichi;
    s_rsle(&io___188);
    do_lio(&c__4, &c__1, (char *)&steps_1.dtt, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&steps_1.itrunk, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&steps_1.ilw, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&flob2_1.ico2m, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&atchem_1.facco2, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&respva2_1.irespg, (ftnlen)sizeof(integer));
    e_rsle();
    io___189.ciunit = *ichi;
    s_rsle(&io___189);
    e_rsle();
    io___190.ciunit = *ichi;
    s_rsle(&io___190);
    do_lio(&c__4, &c__1, (char *)&site_1.zlat, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&site_1.zlong, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&govern_1.time, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&govern2_1.month, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&govern_1.day, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&govern_1.year, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&(*maxit), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*nyfirst), (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&(*nylast), (ftnlen)sizeof(integer));
    e_rsle();
    io___191.ciunit = *ichi;
    s_rsle(&io___191);
    e_rsle();
    io___192.ciunit = *ichi;
    s_rsle(&io___192);
    do_lio(&c__4, &c__1, (char *)&stepv_1.tc, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&stepv_1.tg, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&stepv_1.td, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&grads_1.ta, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&atmos_1.tm, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&caerod_1.ht, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&qa, (ftnlen)sizeof(real));
    e_rsle();
/* L48: */
/* L49: */
    if (pfirst) {
	pfirst = FALSE_;
	s_wsle(&io___194);
	do_lio(&c__4, &c__1, (char *)&site_1.zlat, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&site_1.zlong, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&govern_1.time, (ftnlen)sizeof(real));
	do_lio(&c__3, &c__1, (char *)&govern2_1.month, (ftnlen)sizeof(integer)
		);
	do_lio(&c__4, &c__1, (char *)&govern_1.day, (ftnlen)sizeof(real));
	do_lio(&c__4, &c__1, (char *)&govern_1.year, (ftnlen)sizeof(real));
	do_lio(&c__3, &c__1, (char *)&(*maxit), (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&(*nyfirst), (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&(*nylast), (ftnlen)sizeof(integer));
	e_wsle();
/* L50: */
	io___195.ciunit = *icho;
	s_wsfe(&io___195);
	do_fio(&c__1, (char *)&site_1.zlat, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&site_1.zlong, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&(*nyfirst), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*nylast), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*maxit), (ftnlen)sizeof(integer));
	e_wsfe();
	if (steps_1.itrunk == 1) {
	    io___196.ciunit = *icho;
	    s_wsfe(&io___196);
	    e_wsfe();
	}
	if (steps_1.itrunk >= 2) {
	    io___197.ciunit = *icho;
	    s_wsfe(&io___197);
	    do_fio(&c__1, (char *)&steps_1.itrunk, (ftnlen)sizeof(integer));
	    e_wsfe();
	}
	if (steps_1.ilw == 1) {
	    io___198.ciunit = *icho;
	    s_wsfe(&io___198);
	    e_wsfe();
	}
	if (steps_1.ilw == 2) {
	    io___199.ciunit = *icho;
	    s_wsfe(&io___199);
	    e_wsfe();
	}
	if (steps_1.ilw == 3) {
	    io___200.ciunit = *icho;
	    s_wsfe(&io___200);
	    e_wsfe();
	}
/*      if (iinf.eq.1) write(*,*)'Infiltration=1 top layer diffusion' */
/*      if (iinf.eq.2) write(*,*) */
/*     &           'Infiltration=2 wave front, w/  overland flow' */
/*      if (iinf.eq.3) write(*,*) */
/*     &           'Infiltration=3 wave front, w/o overland flow' */
/*      if (iinf.eq.4) write(*,*) */
/*     &  'Infiltration=4 wave front w/ sat impeding, w/o overland flow' */
	s_wsfe(&io___201);
	do_fio(&c__1, " Soil hydrology parameters: ", (ftnlen)28);
	do_fio(&c__1, " slope qng     - slope  ", (ftnlen)24);
	do_fio(&c__1, (char *)&soils_1.slope, (ftnlen)sizeof(real));
	do_fio(&c__1, " qstar Horton  - xcs    ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.xcs, (ftnlen)sizeof(real));
	do_fio(&c__1, " Liston qng    - xkb    ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.xkb, (ftnlen)sizeof(real));
	do_fio(&c__1, " ativar qng    - jqng   ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jqng, (ftnlen)sizeof(integer));
	do_fio(&c__1, " infiltration  - iinf   ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.iinf, (ftnlen)sizeof(integer));
	do_fio(&c__1, " numer scheme  - jesq   ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jesq, (ftnlen)sizeof(integer));
	do_fio(&c__1, " C(psi) calc   - jdpsi  ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jdpsi, (ftnlen)sizeof(integer));
	do_fio(&c__1, " K(psi) calc   - jkcon  ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jkcon, (ftnlen)sizeof(integer));
	do_fio(&c__1, " linear system - jsys   ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jsys, (ftnlen)sizeof(integer));
	do_fio(&c__1, " Jacobi/Gauss  - jjgs   ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jjgs, (ftnlen)sizeof(integer));
	do_fio(&c__1, " Q initial     - jqini  ", (ftnlen)24);
	do_fio(&c__1, (char *)&soimul_1.jqini, (ftnlen)sizeof(integer));
	e_wsfe();
	ydep[0] = soils_1.zdepth[0] / 2.f;
	eacum[1] = soils_1.extfrac[1];
	eacum[0] = soils_1.extfrac[0];
	for (i__ = 2; i__ <= 10; ++i__) {
	    ydep[i__ - 1] = soils_1.zdepth[i__ - 2] + soils_1.zdepth[i__ - 1] 
		    / 2.f;
	    if (i__ >= 3) {
		eacum[i__ - 1] = eacum[i__ - 2] + soils_1.extfrac[i__ - 1];
	    }
	}
	s_wsfe(&io___205);
	do_fio(&c__1, "Layer Depth_cm  Root % Root_acum %  Winicial", (ftnlen)
		44);
	for (i__ = 1; i__ <= 10; ++i__) {
	    do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    r__1 = ydep[i__ - 1] * 100.f;
	    do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	    r__2 = soils_1.extfrac[i__ - 1] * 100.f;
	    do_fio(&c__1, (char *)&r__2, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&eacum[i__ - 1], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&stepv_1.www[i__ - 1], (ftnlen)sizeof(real))
		    ;
	}
	e_wsfe();
    }
    s_wsle(&io___206);
    do_lio(&c__9, &c__1, "  saindo cntrol", (ftnlen)15);
    e_wsle();
/*       write(*,*)'  saindo cntrol' */
    return 0;
} /* cntrol_ */

/* Main program alias */ int sib2_ () { MAIN__ (); return 0; }
