/* Sib2xa.F -- translated by f2c (version 20160102).
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

struct {
    integer itmp1, itmp2, itmp3, itmp4, itmp5;
} outfile_;

#define outfile_1 outfile_

struct {
    real rngdtg, rngdtc, rncdtg, rncdtc, hgdtg, hgdtc, hgdth, hcdtg, hcdtc, 
	    hcdth, egdtg, egdtc, egdqm, ecdtg, ecdtc, ecdqm, deadtc, deadtg, 
	    demdqm, deadqm, aag, aac, aam, bbg, bbc, bbm;
} flxdif_;

#define flxdif_1 flxdif_

/* Table of constant values */

static doublereal c_b28 = .1428;
static doublereal c_b29 = .131;
static doublereal c_b30 = .583;
static doublereal c_b31 = .202;
static doublereal c_b32 = .9061;
static real c_b38 = 1.f;
static doublereal c_b44 = 3.;
static doublereal c_b45 = 2.;
static doublereal c_b46 = 2.1;
static doublereal c_b48 = 1.2;
static doublereal c_b49 = .57;
static doublereal c_b50 = 1.8;
static integer c__4 = 4;
static integer c__5 = 5;
static real c_b81 = 4.f;
static doublereal c_b101 = .25;
static integer c__1 = 1;


/* Subroutine */ int const2_(void)
{

/* ======================================================================= */

/*     initialization of physical constants */

/*     subroutine const2 is called at every time step */
/*     because in some applications constants may depend on */
/*     environmental conditions. */

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
    const_1.asnow = 13.2f;
    atmos_1.bps = 1.f;
    const_1.clai = 840.f;
    const_1.cpair = 1010.f;
    const_1.cw = 4.2e6f;
    const_1.epsfac = .622f;
    const_1.gx = 9.81f;
    const22_1.kappa = .286f;
    const_1.pie = 3.14159265f;
    atchem_1.po2m = 20900.f;
    atchem_1.pco2m = 34.f;
    atmos_1.psur = 1e3f;
    const_1.rhoair = 1.225f;
    const_1.snomel = 370518500.f;
    const_1.stefan = 5.669e-8f;
    const_1.tf = 273.16f;
    const_1.vkc = .41f;
    const_1.rcp = const_1.rhoair * const_1.cpair;
    const_1.timcon = const_1.pie / 86400.f;

/* ----------------------------------------------------------------------- */
/*     n.b. :  snomel is expressed in j m-1 */
/* ----------------------------------------------------------------------- */

    return 0;
} /* const2_ */

/* ======================================================================= */

/* Subroutine */ int adjust_(real *ts, real *spechc, real *capacp, real *
	snowwp, integer *iveg)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static real xs, cca, ccb, ccc, ccp, cct, tsd, tta, ttb, diff, freeze;


/* ======================================================================= */

/*     temperature change due to addition of precipitation */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*     roff           runoff (m) */
/*     tc             canopy temperature (K) */
/*     tg             ground surface temperature (K) */
/*     www(1)         ground wetness of surface layer */
/*     capac(2)       canopy/ground liquid interception store (m) */
/*     snoww(2)       canopy/ground snow interception store (m) */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


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
    freeze = 0.f;
    diff = (stepv_1.capac[*iveg - 1] + stepv_1.snoww[*iveg - 1] - *capacp - *
	    snowwp) * const_1.cw;
    ccp = *spechc;
    cct = *spechc + diff;

    tsd = (*ts * ccp + atmos_1.tm * diff) / cct;

    if (*ts > const_1.tf && atmos_1.tm > const_1.tf) {
	goto L200;
    }
    if (*ts <= const_1.tf && atmos_1.tm <= const_1.tf) {
	goto L200;
    }

    tta = *ts;
    ttb = atmos_1.tm;
    cca = ccp;
    ccb = diff;
    if (tsd > const_1.tf) {
	goto L100;
    }

/* ---------------------------------------------------------------------- */
/*    freezing of water on canopy or ground */
/* ---------------------------------------------------------------------- */

    ccc = *capacp * const_1.snomel;
    if (*ts < atmos_1.tm) {
	ccc = diff * const_1.snomel / const_1.cw;
    }
    tsd = (tta * cca + ttb * ccb + ccc) / cct;

    freeze = const_1.tf * cct - (tta * cca + ttb * ccb);
    freeze = dmin(ccc,freeze) / const_1.snomel;
    if (tsd > const_1.tf) {
	tsd = const_1.tf - .01f;
    }

    goto L200;

L100:

/* ---------------------------------------------------------------------- */
/*    melting of snow on canopy or ground, water infiltrates. */
/* ---------------------------------------------------------------------- */

    ccc = -stepv_1.snoww[*iveg - 1] * const_1.snomel;
    if (*ts > atmos_1.tm) {
	ccc = -diff * const_1.snomel / const_1.cw;
    }

    tsd = (tta * cca + ttb * ccb + ccc) / cct;

    freeze = const_1.tf * cct - (tta * cca + ttb * ccb);
    freeze = dmax(ccc,freeze) / const_1.snomel;
    if (tsd <= const_1.tf) {
	tsd = const_1.tf - .01f;
    }

L200:
    stepv_1.snoww[*iveg - 1] += freeze;
    stepv_1.capac[*iveg - 1] -= freeze;

/* Computing MAX */
    r__1 = 0.f, r__2 = stepv_1.capac[*iveg - 1] - hydrol_1.satcap[*iveg - 1];
    xs = dmax(r__1,r__2);
    if (stepv_1.snoww[*iveg - 1] >= 1e-7f) {
	xs = stepv_1.capac[*iveg - 1];
    }
    stepv_1.www[0] += xs / (soils_1.poros[0] * soils_1.zdepth[0]);
    stepv_1.capac[*iveg - 1] -= xs;
    *ts = tsd;

    return 0;
} /* adjust_ */


/* ======================================================================= */

/* Subroutine */ int patchs_(real *p0)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static real ex, rhs, tsd, dcap, pinf, thru, zmelt, dareas, snowhc;


/* ======================================================================= */

/*     marginal situation: snow exists in patches at temperature tf */
/*     with remaining area at temperature tg > tf. */

/* ---------------------------------------------------------------------- */

/*     calculation of effect of intercepted snow and rainfall on ground. */
/*     patchy snowcover situation involves complex treatment to keep */
/*     energy conserved. */

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
    pinf = *p0;
    thru = 0.f;
    snowhc = dmin(.05f,stepv_1.snoww[1]) * const_1.cw;
/* Computing MIN */
    r__1 = 1.f, r__2 = const_1.asnow * stepv_1.snoww[1];
    hydrol_1.areas = dmin(r__1,r__2);
    if (atmos_1.tm > const_1.tf) {
	goto L400;
    }

/* ---------------------------------------------------------------------- */
/*     snow falling onto area */
/* ---------------------------------------------------------------------- */

    rhs = atmos_1.tm * pinf * const_1.cw + const_1.tf * (snowhc + 
	    stores_1.csoil * hydrol_1.areas) + stepv_1.tg * stores_1.csoil * (
	    1.f - hydrol_1.areas);
/* Computing MIN */
    r__1 = const_1.asnow * pinf, r__2 = 1.f - hydrol_1.areas;
    dareas = dmin(r__1,r__2);
    ex = rhs - const_1.tf * pinf * const_1.cw - const_1.tf * (snowhc + 
	    stores_1.csoil * (hydrol_1.areas + dareas)) - stepv_1.tg * 
	    stores_1.csoil * (1.f - hydrol_1.areas - dareas);
    if (hydrol_1.areas + dareas >= .999f) {
	stepv_1.tg = const_1.tf - .01f;
    }
    if (ex < 0.f) {
	goto L200;
    }

/* ---------------------------------------------------------------------- */
/*     excess energy is positive, some snow melts and infiltrates. */
/* ---------------------------------------------------------------------- */

    zmelt = ex / const_1.snomel;
    if (const_1.asnow * (stepv_1.snoww[1] + pinf - zmelt) > 1.f) {
	goto L100;
    }
    zmelt = 0.f;
    if (const_1.asnow * (stepv_1.snoww[1] + pinf) >= 1.f) {
	zmelt = (const_1.asnow * (stepv_1.snoww[1] + pinf) - 1.f) / 
		const_1.asnow;
    }
    zmelt = (ex - zmelt * const_1.snomel) / (const_1.snomel + const_1.asnow * 
	    stores_1.csoil * (stepv_1.tg - const_1.tf)) + zmelt;
L100:
    stepv_1.snoww[1] = stepv_1.snoww[1] + pinf - zmelt;
    stepv_1.www[0] += zmelt / (soils_1.poros[0] * soils_1.zdepth[0]);
    goto L600;

/* ---------------------------------------------------------------------- */
/*     excess energy is negative, bare ground cools to tf, then whole */
/*     area cools together to lower temperature. */
/* ---------------------------------------------------------------------- */

L200:
    tsd = 0.f;
    if (hydrol_1.areas + dareas <= .999f) {
	tsd = ex / (stores_1.csoil * (1.f - hydrol_1.areas - dareas)) + 
		stepv_1.tg;
    }
    if (tsd > const_1.tf) {
	goto L300;
    }
    tsd = const_1.tf + (ex - (const_1.tf - stepv_1.tg) * stores_1.csoil * (
	    1.f - hydrol_1.areas - dareas)) / (snowhc + pinf * const_1.cw + 
	    stores_1.csoil);
L300:
    stepv_1.tg = tsd;
    stepv_1.snoww[1] += pinf;
    goto L600;

/* ---------------------------------------------------------------------- */
/*     rain falling onto area */
/* ---------------------------------------------------------------------- */

L400:

/* ---------------------------------------------------------------------- */
/*     rain falls onto snow-free sector first. */
/* ---------------------------------------------------------------------- */

    tsd = const_1.tf - .01f;
    if (hydrol_1.areas < .999f) {
	tsd = (atmos_1.tm * pinf * const_1.cw + stepv_1.tg * stores_1.csoil) /
		 (pinf * const_1.cw + stores_1.csoil);
    }
    stepv_1.tg = tsd;
    stepv_1.www[0] += pinf * (1.f - hydrol_1.areas) / (soils_1.poros[0] * 
	    soils_1.zdepth[0]);

/* ---------------------------------------------------------------------- */
/*     rain falls onto snow-covered sector next. */
/* ---------------------------------------------------------------------- */

    ex = (atmos_1.tm - const_1.tf) * pinf * const_1.cw * hydrol_1.areas;
    dcap = -ex / (const_1.snomel + (stepv_1.tg - const_1.tf) * stores_1.csoil 
	    * const_1.asnow);
    if (stepv_1.snoww[1] + dcap < 0.f) {
	goto L500;
    }
    stepv_1.www[0] += (pinf * hydrol_1.areas - dcap) / (soils_1.poros[0] * 
	    soils_1.zdepth[0]);
    stepv_1.snoww[1] += dcap;
    goto L600;
L500:
    stepv_1.tg = (ex - const_1.snomel * stepv_1.snoww[1] - (stepv_1.tg - 
	    const_1.tf) * stores_1.csoil * hydrol_1.areas) / stores_1.csoil + 
	    stepv_1.tg;
    stepv_1.www[0] += (stepv_1.snoww[1] + pinf * hydrol_1.areas) / (
	    soils_1.poros[0] * soils_1.zdepth[0]);
    stepv_1.capac[1] = 0.f;
    stepv_1.snoww[1] = 0.f;

L600:

    return 0;
} /* patchs_ */


/* ======================================================================= */

/* Subroutine */ int snow1_(void)
{
    /* System generated locals */
    real r__1, r__2;


/* ======================================================================= */

/*     calculation of effects of snow cover on surface morphology and */
/*     maximum water storage values. */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       z0             roughness length (m) */
/*       xdx              zero plane displacement (m) */
/*       rbc            rb coefficient (c1) (s m-1)**1/2 */
/*       rdc            rd coefficient (c2) */
/*       satcap(2)      interception capacities (m) */
/*       canex          fraction of exposed canopy (snow-free) */
/*       areas          ground snow cover fraction */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*    alteration of aerodynamic transfer properties in case of snow */
/*    accumulation. calculation of maximum water storage values. */

/*      canex       (fraction of canopy not covered by snow) */
/*      xdx           (snow-modified value of dd, used in all calculations) */
/*      z0          (snow-modified value of z0d, used in all calculations) */
/*      rbc         (snow-modified value of cc1, used in all calculations) */
/*      rdc         (snow-modified value of cc2, used in all calculations) */
/*      areas       (fraction of ground covered by snow) */
/*      satcap(1)   (s-c)   : equation (56) , SE-86, page 741 se-89 */
/*      satcap(2)   (s-g)   : 0.002, surface interception store */
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
    hydrol_1.canex = 1.f - (stepv_1.snoww[1] * 5.f - vstate_1.z1) / (
	    vstate_1.z2 - vstate_1.z1);
    hydrol_1.canex = dmax(.1f,hydrol_1.canex);
    hydrol_1.canex = dmin(1.f,hydrol_1.canex);
    rause_1.xdx = vstate_1.z2 - (vstate_1.z2 - vderiv_1.dd) * hydrol_1.canex;
    rause_1.z0 = vderiv_1.z0d / (vstate_1.z2 - vderiv_1.dd) * (vstate_1.z2 - 
	    rause_1.xdx);
    rause_1.rbc = vderiv_1.cc1 / hydrol_1.canex;
    rause_1.rdc = vderiv_1.cc2 * hydrol_1.canex;
/* Computing MIN */
    r__1 = 1.f, r__2 = const_1.asnow * stepv_1.snoww[1];
    hydrol_1.areas = dmin(r__1,r__2);
    hydrol_1.satcap[0] = vdyijt_1.zlt * 1e-4f * hydrol_1.canex;
    hydrol_1.satcap[1] = .002f;
    return 0;
} /* snow1_ */


/* ======================================================================= */

/* Subroutine */ int rada2_(void)
{
    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal), exp(doublereal);

    /* Local variables */
    static real f1, aa, bb, be, ce, de, fe, ek, ge, zp, hh1, hh2, hh3, hh4, 
	    hh5, hh6, hh7, hh8, hh9, tc4, tg4, hh10, den, xfx, bot, psi, zat, 
	    zmk, fac1, fac2, facs;
    static integer irad;
    static real chiv, scov, scat, proj, zmew, acss, epsi, zkat;
    static integer iveg;
    static real reff1, reff2, tran1, tran2;
    extern /* Subroutine */ int snow1_(void);
    static real betao, fmelt;
    static integer iwave;
    static real extkb, closs, gloss, tranc1[2], tranc2[2], tranc3[2], power1, 
	    power2, upscat;
    extern /* Subroutine */ int longrn_(real *, real *, real *);


/* ======================================================================= */

/*     calculation of albedos via two stream approximation( direct */
/*     and diffuse ) and partition of radiant energy */

/* ----------------------------------------------------------------------- */

/*     subroutines  called  : snow1 */
/*     --------------------   longrn */


/* ++++++++++++++++++++++++++++++output++++++++++++++++++++++++++++++++ */

/*       salb(2,2)      surface albedos */
/*       tgeff          effective surface radiative temperature (k) */
/*       radfac(2,2,2)  radiation absorption factors */
/*       thermk         canopy gap fraction for tir radiation */

/* ++++++++++++++++++++++++++diagnostics+++++++++++++++++++++++++++++++ */

/*       albedo(2,2,2)  component reflectances */
/*       closs          tir emission from the canopy (w m-2) */
/*       gloss          tir emission from the ground (w m-2) */

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


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

    xfx = atmos_1.sunang;

/* ---------------------------------------------------------------------- */


/*     modification for effect of snow on upper story albedo */
/*         snow reflectance   = 0.80, 0.40 . multiply by 0.6 if melting */
/*         snow transmittance = 0.20, 0.54 */


/* ----------------------------------------------------------------------- */

    snow1_();

    facs = (stepv_1.tg - const_1.tf) * .04f;
    facs = dmax(0.f,facs);
    facs = dmin(.4f,facs);
    fmelt = 1.f - facs;

    for (iwave = 1; iwave <= 2; ++iwave) {

/* Computing MIN */
	r__1 = .5f, r__2 = stepv_1.snoww[0] / hydrol_1.satcap[0];
	scov = dmin(r__1,r__2);
	reff1 = (1.f - scov) * vstate_1.ref[iwave - 1] + scov * (1.2f - iwave 
		* .4f) * fmelt;
	reff2 = (1.f - scov) * vstate_1.ref[iwave + 1] + scov * (1.2f - iwave 
		* .4f) * fmelt;
	tran1 = vstate_1.tran[iwave - 1] * (1.f - scov) + scov * (1.f - (1.2f 
		- iwave * .4f) * fmelt) * vstate_1.tran[iwave - 1];
	tran2 = vstate_1.tran[iwave + 1] * (1.f - scov) + scov * (1.f - (1.2f 
		- iwave * .4f) * fmelt) * .9f * vstate_1.tran[iwave + 1];

/* ----------------------------------------------------------------------- */

/*     calculate average scattering coefficient, leaf projection and */
/*     other coefficients for two-stream model. */

/*      scat  (omega)        : equation (1,2) , SE-85 */
/*      proj  (g(mu))        : equation (13)  , SE-85 */
/*      extkb (k, g(mu)/mu)  : equation (1,2) , SE-85 */
/*      zmew  (int(mu/g(mu)) : equation (1,2) , SE-85 */
/*      acss  (a-s(mu))      : equation (5)   , SE-85 */
/*      extk  (k, various)   : equation (13)  , SE-85 */
/*      upscat(omega-beta)   : equation (3)   , SE-85 */
/*      betao (beta-0)       : equation (4)   , SE-85 */
/*      psi   (h)            : appendix       , SE-85 */

/* ----------------------------------------------------------------------- */

	scat = vdyijt_1.green * (tran1 + reff1) + (1.f - vdyijt_1.green) * (
		tran2 + reff2);
	chiv = vstate_1.chil;

	if (dabs(chiv) <= .01f) {
	    chiv = .01f;
	}
	aa = .5f - chiv * .633f - chiv * .33f * chiv;
	bb = (1.f - aa * 2.f) * .877f;

	proj = aa + bb * xfx;
	extkb = (aa + bb * xfx) / xfx;
	zmew = 1.f / bb * (1.f - aa / bb * log((aa + bb) / aa));
	acss = scat / 2.f * proj / (proj + xfx * bb);
	acss *= 1.f - xfx * aa / (proj + xfx * bb) * log((proj + xfx * bb + 
		xfx * aa) / (xfx * aa));

	upscat = vdyijt_1.green * tran1 + (1.f - vdyijt_1.green) * tran2;
/* Computing 2nd power */
	r__1 = (1.f - chiv) / 2.f;
	upscat = (scat + (scat - upscat * 2.f) * (r__1 * r__1)) * .5f;
	betao = (zmew * extkb + 1.f) / (scat * zmew * extkb) * acss;

/* ---------------------------------------------------------------------- */

/*     intermediate variables identified in appendix of SE-85. */

/*      be          (b)     : appendix      , SE-85 */
/*      ce          (c)     : appendix      , SE-85 */
/*      bot         (sigma) : appendix      , SE-85 */
/*      hh1         (h1)    : appendix      , SE-85 */
/*      hh2         (h2)    : appendix      , SE-85 */
/*      hh3         (h3)    : appendix      , SE-85 */
/*      hh4         (h4)    : appendix      , SE-85 */
/*      hh5         (h5)    : appendix      , SE-85 */
/*      hh6         (h6)    : appendix      , SE-85 */
/*      hh7         (h7)    : appendix      , SE-85 */
/*      hh8         (h8)    : appendix      , SE-85 */
/*      hh9         (h9)    : appendix      , SE-85 */
/*      hh10        (h10)   : appendix      , SE-85 */
/*      psi         (h)     : appendix      , SE-85 */
/*      zat         (l-t)   : appendix      , SE-85 */
/*      epsi        (s1)    : appendix      , SE-85 */
/*      ek          (s2)    : appendix      , SE-85 */
/* -------------------------------------------------------------------- */

	be = 1.f - scat + upscat;
	ce = upscat;
/* Computing 2nd power */
	r__1 = zmew * extkb;
/* Computing 2nd power */
	r__2 = ce;
/* Computing 2nd power */
	r__3 = be;
	bot = r__1 * r__1 + (r__2 * r__2 - r__3 * r__3);
	if (dabs(bot) > 1e-10f) {
	    goto L100;
	}
	scat *= .98f;
	be = 1.f - scat + upscat;
/* Computing 2nd power */
	r__1 = zmew * extkb;
/* Computing 2nd power */
	r__2 = ce;
/* Computing 2nd power */
	r__3 = be;
	bot = r__1 * r__1 + (r__2 * r__2 - r__3 * r__3);

L100:

	de = scat * zmew * extkb * betao;
	fe = scat * zmew * extkb * (1.f - betao);
	hh1 = -de * be + zmew * de * extkb - ce * fe;
	hh4 = -be * fe - zmew * fe * extkb - ce * de;

/* Computing 2nd power */
	r__1 = be;
/* Computing 2nd power */
	r__2 = ce;
	psi = sqrt(r__1 * r__1 - r__2 * r__2) / zmew;

	zat = vdyijt_1.zlt / vstate_1.vcover * hydrol_1.canex;

/* Computing MIN */
	r__1 = psi * zat;
	power1 = dmin(r__1,50.f);
/* Computing MIN */
	r__1 = extkb * zat;
	power2 = dmin(r__1,50.f);
	epsi = exp(-power1);
	ek = exp(-power2);

	radabs_1.albedo[(iwave + 2 << 1) - 5] = soilij_1.soref[iwave - 1] * (
		1.f - hydrol_1.areas) + (1.2f - iwave * .4f) * fmelt * 
		hydrol_1.areas;
	radabs_1.albedo[(iwave + 4 << 1) - 5] = soilij_1.soref[iwave - 1] * (
		1.f - hydrol_1.areas) + (1.2f - iwave * .4f) * fmelt * 
		hydrol_1.areas;
	ge = radabs_1.albedo[(iwave + 2 << 1) - 5] / radabs_1.albedo[(iwave + 
		4 << 1) - 5];

/* ----------------------------------------------------------------------- */
/*     calculation of diffuse albedos */

/*      albedo(1,ir,2) ( i-up ) : appendix , SE-85 */

/* ----------------------------------------------------------------------- */

	f1 = be - ce / radabs_1.albedo[(iwave + 4 << 1) - 5];
	zp = zmew * psi;

	den = (be + zp) * (f1 - zp) / epsi - (be - zp) * (f1 + zp) * epsi;
	hh7 = ce * (f1 - zp) / epsi / den;
	hh8 = -ce * (f1 + zp) * epsi / den;
	f1 = be - ce * radabs_1.albedo[(iwave + 4 << 1) - 5];
	den = (f1 + zp) / epsi - (f1 - zp) * epsi;

	hh9 = (f1 + zp) / epsi / den;
	hh10 = -(f1 - zp) * epsi / den;
	tranc2[iwave - 1] = hh9 * epsi + hh10 / epsi;

	radabs_1.albedo[(iwave + 4 << 1) - 6] = hh7 + hh8;

/* ----------------------------------------------------------------------- */
/*     calculation of direct albedos and canopy transmittances. */

/*      albedo(1,iw,1) ( i-up ) : equation(11)   , SE-85 */
/*      tranc (iw)   ( i-down ) : equation(10)   , SE-85 */

/* ----------------------------------------------------------------------- */

	f1 = be - ce / radabs_1.albedo[(iwave + 4 << 1) - 5];
	zmk = zmew * extkb;

	den = (be + zp) * (f1 - zp) / epsi - (be - zp) * (f1 + zp) * epsi;
	hh2 = (de - hh1 / bot * (be + zmk)) * (f1 - zp) / epsi - (be - zp) * (
		de - ce * ge - hh1 / bot * (f1 + zmk)) * ek;
	hh2 /= den;
	hh3 = (be + zp) * (de - ce * ge - hh1 / bot * (f1 + zmk)) * ek - (de 
		- hh1 / bot * (be + zmk)) * (f1 + zp) * epsi;
	hh3 /= den;
	f1 = be - ce * radabs_1.albedo[(iwave + 4 << 1) - 5];
	den = (f1 + zp) / epsi - (f1 - zp) * epsi;
	hh5 = -hh4 / bot * (f1 + zp) / epsi - (fe + ce * ge * radabs_1.albedo[
		(iwave + 4 << 1) - 5] + hh4 / bot * (zmk - f1)) * ek;
	hh5 /= den;
	hh6 = hh4 / bot * (f1 - zp) * epsi + (fe + ce * ge * radabs_1.albedo[(
		iwave + 4 << 1) - 5] + hh4 / bot * (zmk - f1)) * ek;
	hh6 /= den;
	tranc1[iwave - 1] = ek;
	tranc3[iwave - 1] = hh4 / bot * ek + hh5 * epsi + hh6 / epsi;

	radabs_1.albedo[(iwave + 2 << 1) - 6] = hh1 / bot + hh2 + hh3;

/* ---------------------------------------------------------------------- */
/*     calculation of terms which multiply incoming short wave fluxes */
/*     to give absorption of radiation by canopy and ground */

/*      radfac      (f(il,imu,iv)) : equation (19,20) , SE-86 */

/* ---------------------------------------------------------------------- */

	radabs_1.radfac[(iwave + 2 << 1) - 5] = (1.f - vstate_1.vcover) * (
		1.f - radabs_1.albedo[(iwave + 2 << 1) - 5]) + 
		vstate_1.vcover * (tranc1[iwave - 1] * (1.f - radabs_1.albedo[
		(iwave + 2 << 1) - 5]) + tranc3[iwave - 1] * (1.f - 
		radabs_1.albedo[(iwave + 4 << 1) - 5]));

	radabs_1.radfac[(iwave + 4 << 1) - 5] = (1.f - vstate_1.vcover) * (
		1.f - radabs_1.albedo[(iwave + 4 << 1) - 5]) + 
		vstate_1.vcover * tranc2[iwave - 1] * (1.f - radabs_1.albedo[(
		iwave + 4 << 1) - 5]);

	radabs_1.radfac[(iwave + 2 << 1) - 6] = vstate_1.vcover * (1.f - 
		radabs_1.albedo[(iwave + 2 << 1) - 6] - tranc1[iwave - 1] * (
		1.f - radabs_1.albedo[(iwave + 2 << 1) - 5]) - tranc3[iwave - 
		1] * (1.f - radabs_1.albedo[(iwave + 4 << 1) - 5]));

	radabs_1.radfac[(iwave + 4 << 1) - 6] = vstate_1.vcover * (1.f - 
		radabs_1.albedo[(iwave + 4 << 1) - 6] - tranc2[iwave - 1] * (
		1.f - radabs_1.albedo[(iwave + 4 << 1) - 5]));

/* ---------------------------------------------------------------------- */
/*     calculation of total surface albedos ( salb ) with weighting */
/*     for cover fractions. */
/* ---------------------------------------------------------------------- */

	for (irad = 1; irad <= 2; ++irad) {

	    site_1.salb[iwave + (irad << 1) - 3] = (1.f - vstate_1.vcover) * 
		    radabs_1.albedo[(iwave + (irad << 1) << 1) - 5] + 
		    vstate_1.vcover * radabs_1.albedo[(iwave + (irad << 1) << 
		    1) - 6];

/* L3000: */
	}

/* L1000: */
    }

/* ---------------------------------------------------------------------- */

/*     calculation of long-wave flux terms from canopy and ground */

/*      closs ( fc - rnc )     : equation (21),  SE-86 */
/*      gloss ( fg - rng )     : equation (22),  SE-86 */

/* ---------------------------------------------------------------------- */

    grads_1.tgs = dmin(const_1.tf,stepv_1.tg) * hydrol_1.areas + stepv_1.tg * 
	    (1.f - hydrol_1.areas);
    tc4 = stepv_1.tc * stepv_1.tc * stepv_1.tc * stepv_1.tc;
    tg4 = grads_1.tgs * grads_1.tgs * grads_1.tgs * grads_1.tgs;

    zkat = 1.f / zmew * vdyijt_1.zlt / vstate_1.vcover;
    zkat = dmin(50.f,zkat);
    zkat = dmax(1e-5f,zkat);
    radabs_1.thermk = exp(-zkat);

    fac1 = vstate_1.vcover * (1.f - radabs_1.thermk);
    fac2 = 1.f;
    closs = fac1 * 2.f * const_1.stefan * tc4;
    closs -= fac2 * fac1 * const_1.stefan * tg4;
    gloss = fac2 * const_1.stefan * tg4;
    gloss -= fac1 * fac2 * const_1.stefan * tc4;
    donor_1.zlwup = fac1 * const_1.stefan * tc4 + (1.f - fac1) * fac2 * 
	    const_1.stefan * tg4;

    longrn_(tranc1, tranc2, tranc3);

/* ----------------------------------------------------------------------- */

/*     calculation of absorption of radiation by surface */

/* ----------------------------------------------------------------------- */

    radabs_1.radt[0] = 0.f;
    radabs_1.radt[1] = 0.f;

    for (iveg = 1; iveg <= 2; ++iveg) {
	for (iwave = 1; iwave <= 2; ++iwave) {
	    for (irad = 1; irad <= 2; ++irad) {

		radabs_1.radt[iveg - 1] += radabs_1.radfac[iveg + (iwave + (
			irad << 1) << 1) - 7] * atmos_1.radn[iwave + irad * 3 
			- 4];

/* L2000: */
	    }
	}
    }

    radabs_1.radt[0] = radabs_1.radt[0] + atmos_1.radn[5] * vstate_1.vcover * 
	    (1.f - radabs_1.thermk) - closs;
    radabs_1.radt[1] = radabs_1.radt[1] + atmos_1.radn[5] * (1.f - 
	    vstate_1.vcover * (1.f - radabs_1.thermk)) - gloss;

    return 0;
} /* rada2_ */

/* ======================================================================= */

/* Subroutine */ int longrn_(real *tranc1, real *tranc2, real *tranc3)
{
    /* System generated locals */
    real r__1;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), pow_dd(doublereal *, doublereal *), exp(
	    doublereal);

    /* Local variables */
    static real esky, swab, swup;
    static integer iwave;


/* ======================================================================= */

/*     calculation of downward longwave. this is not required in gcm if */
/*     downward longwave is provided by gcm-radiation code as radn(3,2). */

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


    /* Parameter adjustments */
    --tranc3;
    --tranc2;
    --tranc1;

    /* Function Body */
    if (steps_1.ilw == 1) {
	goto L101;
    }
    if (steps_1.ilw == 2) {
	goto L102;
    }
/* Brunts */
    if (steps_1.ilw == 3) {
	goto L103;
    }
/* residuo Bal radiacao */
    if (steps_1.ilw == 4) {
	goto L104;
    }
/* Brutsaert(1975) */
    if (steps_1.ilw == 5) {
	goto L105;
    }
/* Idso and Jackson */
    if (steps_1.ilw == 6) {
	goto L106;
    }
/* Swinbank */
    if (steps_1.ilw == 7) {
	goto L107;
    }
/* Duarte */
    if (steps_1.ilw == 8) {
	goto L108;
    }

/* ---------------------------------------------------------------------- */
/*     downward long-wave assumed to be provided as radn(3,2) */
/* ---------------------------------------------------------------------- */
/* Kruk */
L101:
    goto L200;

/* ---------------------------------------------------------------------- */
/*     downward long-wave from brunt's equation, Monteith(1973), p37. */
/* ---------------------------------------------------------------------- */

L102:
    esky = sqrt(atmos_1.em) * .06f + .53f;
/* brunts(1932) com correcao de Jacobs(1 */
/* Computing 4th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = esky * (atmos_1.cloud * atmos_1.cloud * .2f + 1.f) * 
	    const_1.stefan * (r__1 * r__1);
    goto L200;
L104:
    d__1 = (doublereal) (atmos_1.em / atmos_1.tm);
    esky = pow_dd(&d__1, &c_b28) * 1.24f;
/* Brutsaert(1975)com correcao de Jacob */
/* Computing 4th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = esky * const_1.stefan * (r__1 * r__1) * (atmos_1.cloud *
	     atmos_1.cloud * .2f + 1.f);
    goto L200;
L105:
/* Computing 2nd power */
    r__1 = 273 - atmos_1.tm;
    esky = exp(r__1 * r__1 * -7.7e-4f) * .26f;
/* Idso&Jackson(1969)c/corre */
/* Computing 4th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = const_1.stefan * (r__1 * r__1) * (1 - esky) * (
	    atmos_1.cloud * atmos_1.cloud * .2f + 1.f);
    goto L200;
L106:
    esky = 9e-6f;
/* Swinbank (1963) com correcao de Jacobs (1978) */
/* Computing 6th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = esky * const_1.stefan * (r__1 * (r__1 * r__1)) * (
	    atmos_1.cloud * atmos_1.cloud * .2f + 1.f);
    goto L200;
L107:
    d__1 = (doublereal) (atmos_1.em * 100 / atmos_1.tm);
    esky = pow_dd(&d__1, &c_b29) * .625f;
/* Duarte (2006)com correcao de */
    d__1 = (doublereal) atmos_1.cloud;
/* Computing 4th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = esky * (pow_dd(&d__1, &c_b30) * .242f + 1.f) * 
	    const_1.stefan * (r__1 * r__1);
    goto L200;
L108:
    d__1 = (doublereal) (atmos_1.em * 100 / atmos_1.tm);
    esky = pow_dd(&d__1, &c_b31) * .576f;
/* Kruk(2008) com correcao de Kr */
    d__1 = (doublereal) atmos_1.cloud;
/* Computing 4th power */
    r__1 = atmos_1.tm, r__1 *= r__1;
    atmos_1.radn[5] = esky * (pow_dd(&d__1, &c_b32) * .1007f + 1.f) * 
	    const_1.stefan * (r__1 * r__1);
    goto L200;

L103:

/* ---------------------------------------------------------------------- */
/*     downward long-wave flux calculated as residual from measured */
/*     net radiation and outgoing longwave radiation. */

/*     calculation of absorbed fractions of radiation ( expendable ) */
/* ---------------------------------------------------------------------- */

    for (iwave = 1; iwave <= 2; ++iwave) {

	site_1.rab[(iwave + 3 << 1) - 7] = (1.f - vstate_1.vcover) * (
		atmos_1.radn[iwave - 1] * (1.f - radabs_1.albedo[(iwave + 2 <<
		 1) - 5]));
	site_1.rab[(iwave + 6 << 1) - 7] = (1.f - vstate_1.vcover) * 
		atmos_1.radn[iwave + 2] * (1.f - radabs_1.albedo[(iwave + 4 <<
		 1) - 5]);

	site_1.rab[(iwave + 3 << 1) - 7] += vstate_1.vcover * (atmos_1.radn[
		iwave - 1] * (tranc1[iwave] * (1.f - radabs_1.albedo[(iwave + 
		2 << 1) - 5]) + tranc3[iwave] * (1.f - radabs_1.albedo[(iwave 
		+ 4 << 1) - 5])));
	site_1.rab[(iwave + 6 << 1) - 7] += vstate_1.vcover * atmos_1.radn[
		iwave + 2] * tranc2[iwave] * (1.f - radabs_1.albedo[(iwave + 
		4 << 1) - 5]);

	site_1.rab[(iwave + 3 << 1) - 8] = vstate_1.vcover * atmos_1.radn[
		iwave - 1] * (1.f - radabs_1.albedo[(iwave + 2 << 1) - 6] - 
		tranc1[iwave] * (1.f - radabs_1.albedo[(iwave + 2 << 1) - 5]) 
		- tranc3[iwave] * (1.f - radabs_1.albedo[(iwave + 4 << 1) - 5]
		));
	site_1.rab[(iwave + 6 << 1) - 8] = vstate_1.vcover * atmos_1.radn[
		iwave + 2] * (1.f - radabs_1.albedo[(iwave + 4 << 1) - 6] - 
		tranc2[iwave] * (1.f - radabs_1.albedo[(iwave + 4 << 1) - 5]))
		;
/* L2000: */
    }

    swab = site_1.rab[0] + site_1.rab[6] + site_1.rab[2] + site_1.rab[8] + 
	    site_1.rab[1] + site_1.rab[7] + site_1.rab[3] + site_1.rab[9];
    swup = atmos_1.swdown - swab;
    atmos_1.radn[5] = atmos_1.rnetm - swab + donor_1.zlwup;

L200:

    return 0;
} /* longrn_ */

/* ====================================================================== */

/* Subroutine */ int endtem_(integer *ipbl)
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static integer i__;
    static real y, d1, t1, t2;
    static integer lx;
    static real ecf, egf;
    static integer nox;
    static real hrr, hend, finc, dewc, taen;
    extern /* Subroutine */ int rbrd_(void);
    static real egit, aven, coct, dewg, cogs1, cogs2, darea;
    extern /* Subroutine */ int delef_(void), delhf_(void);
    static real arean;
    extern /* Subroutine */ int delrn_(void);
    static real rsnow;
    static integer iwalk;
    static real tsnow, ecpot, egpot, ecidif, egidif;
    extern /* Subroutine */ int dtcdtg_(void);
    static integer ifirst, icount;
    extern /* Subroutine */ int rasite_(void), phosib_(void), sibslv_(void), 
	    newton_(real *, real *, real *, integer *, integer *, integer *, 
	    integer *);
    static integer nonpos;
    static real egsmax;


/* ---------------------------------------------------------------------- */

/*      calculation of ea, ta, ra, rb, rd and soil moisture stress */
/*      for the beginning of the time step */

/* ---------------------------------------------------------------------- */

/*                        modifications */

/*     (1)     : change in cog1,cog2,cogs1 to allow soil evaporation */
/*               from beneath ground cover canopy. */

/*     (2)     : change in temperature tgs to account for patchy snow. */
/*               a bulk (area-weighted) temperature is substituted for */
/*               all energy budget calculations. energy used to warm */
/*               exposed patches is subtracted from snow evaporation. */

/*     (3)     : inclusion of randall-sellers backward implicit scheme */
/*               for calculating dtc, dtg, dth, dqm. option remains to */
/*               use original dtc, dtg scheme only using parameter ipbl. */

/*     (4)     : inclusion of integrated canopy  photosynthesis - */
/*               conductance model. note that soil moisture stress is */
/*               modelled as a chronic condition, while the relative */
/*               humidity term is solved within a feedback loop. */
/*               reference : SE-92 */

/* ======================================================================= */

/*     subroutines  called : rasite --> unstab,stab,rafcal */
/*     -------------------   rbrd */
/*                           phosib --> cycalc-sortin */
/*                           delrn */
/*                           delhf */
/*                           delef */
/*                           sibslv --> gauss */
/*                           dtcdtg */
/*                           newton */
/* ----------------------------------------------------------------------- */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */
/*       ect            canopy transpiration (j m-2) */
/*       eci            canopy interception loss (j m-2) */
/*       egs            ground evaporation (j m-2) */
/*       egi            ground interception loss (j m-2) */
/*       ec             ect + eci */
/*       eg             egs + egi */
/*       hc             canopy sensible heat flux (j m-2) */
/*       hg             ground sensible heat flux (j m-2) */
/*       chf            canopy heat storage flux (j m-2) */
/*       shf            soil heat storage flux (j m-2) */
/*       fc             canopy dew indicator */
/*       fg             ground dew indicator */
/*       heaten         heat loss to snow melt process (j m-2) */

/* ++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++ */

/*       tsnow          snow temperature (k) */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
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

/* hrr  introduzido p/ evitar warning compilacao (variable used and not defined) */
/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

    rsnow = 0.f;
    tsnow = stepv_1.tg;
/* hrr */
    ifirst = 1;
    icount = 0;

    surfrs_1.fc = 1.f;
    surfrs_1.fg = 1.f;
    grads_1.ta = (grads_1.tgs + stepv_1.tc) / 2.f;
    grads_1.ea = atmos_1.em;
    caerod_1.ht = 0.f;

L1000:
    ++icount;

    rasite_();

    rbrd_();

    phosib_();

    if (icount <= 4) {
	goto L1000;
    }

    ifirst = 0;
    delrn_();

/* ---------------------------------------------------------------------- */

/*     dew calculation : dew condition is set at beginning of time step. */
/*     if surface changes state during time step, latent heat flux is */
/*     set to zero. */

/* ---------------------------------------------------------------------- */

    if (grads_1.ea > grads_1.etc) {
	surfrs_1.fc = 0.f;
    }
    if (grads_1.ea > grads_1.etgs) {
	surfrs_1.fg = 0.f;
    }

/* ---------------------------------------------------------------------- */
/*     start of non-neutral resistance calculation loop */
/* ---------------------------------------------------------------------- */

    i__ = 0;

/* ---------------------------------------------------------------------- */
/*         initialize newton-raphson iterative routine */
/*                    for ra calculation */
/* ---------------------------------------------------------------------- */

    nox = 0;
    nonpos = 0;
    iwalk = 0;
    lx = 2;
    finc = 50.f;
L2000:

    rasite_();

    delhf_();

    delef_();

    if (*ipbl == 0) {
	dtcdtg_();
    }

    if (*ipbl == 1) {
	sibslv_();
    }

/* ----------------------------------------------------------------------- */
/*     calculation of evaporative potentials (ecpot, egpot) and */
/*        interception losses; fluxes in j m**-2.  ecidif and egidif */
/*        hold the excess energy if all intercepted water is evaporated */
/*        during the timestep.  these energies are later added to the */
/*        sensible heat fluxes. */

/*      eci         (e-wc)  : equation (59) , SE-86 */
/*      egi         (e-wg)  : equation (60) , SE-86 */
/* ----------------------------------------------------------------------- */

/*     check if interception loss term has exceeded canopy storage */
/* ---------------------------------------------------------------------- */

    ecpot = grads_1.etc - grads_1.ea + (grads_1.getc - flxdif_1.deadtc) * 
	    delts_1.dtc - flxdif_1.deadtg * delts_1.dtg - flxdif_1.deadqm * 
	    delts_1.dqm;
    flux_1.eci = ecpot * hydrol_1.wc / (aerorx_1.rb * 2.f) * const_1.rcp / 
	    const_1.psy * steps_1.dtt;
/* Computing MAX */
    r__1 = 0.f, r__2 = flux_1.eci - (stepv_1.snoww[0] + stepv_1.capac[0]) * 
	    1e3f * const_1.hlat;
    ecidif = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = flux_1.eci, r__2 = (stepv_1.snoww[0] + stepv_1.capac[0]) * 1e3f * 
	    const_1.hlat;
    flux_1.eci = dmin(r__1,r__2);

    egpot = grads_1.etgs - grads_1.ea + (grads_1.getgs - flxdif_1.deadtg) * 
	    delts_1.dtg - flxdif_1.deadtc * delts_1.dtc - flxdif_1.deadqm * 
	    delts_1.dqm;
    flux_1.egi = egpot / aerorx_1.rd * const_1.rcp / const_1.psy * 
	    steps_1.dtt * (hydrol_1.wg * (1.f - hydrol_1.areas) + 
	    hydrol_1.areas);
/* Computing MAX */
    r__1 = 0.f, r__2 = flux_1.egi - (stepv_1.snoww[1] + stepv_1.capac[1]) * 
	    1e3f * const_1.hlat;
    egidif = dmax(r__1,r__2) * (1.f - rsnow);
/* Computing MIN */
    r__1 = flux_1.egi, r__2 = (stepv_1.snoww[1] + stepv_1.capac[1]) * 1e3f * 
	    const_1.hlat;
    egit = dmin(r__1,r__2) * (1.f - rsnow);

/* ---------------------------------------------------------------------- */
/*     calculation of interception loss from ground-snow. if snow patch */
/*     shrinks, energy is taken from egi to warm exposed soil to tgs. */
/* ---------------------------------------------------------------------- */

    t1 = stepv_1.snoww[1] - 1.f / const_1.asnow;
    t2 = dmax(0.f,t1);
    aven = flux_1.egi - t2 * const_1.hlat * 1e3f / const_1.snofac;
    if ((t1 - t2) * flux_1.egi > 0.f) {
	aven = flux_1.egi;
    }
    darea = aven / ((tsnow - stepv_1.tg) * stores_1.csoil - 1.f / 
	    const_1.asnow * const_1.hlat * 1e3f / const_1.snofac);
    arean = hydrol_1.areas + darea;
    egidif -= dmin(0.f,arean) * const_1.asnow * const_1.hlat * 1e3f / 
	    const_1.snofac * rsnow;
/* Computing MAX */
    r__1 = darea, r__2 = -hydrol_1.areas;
    darea = dmax(r__1,r__2);
/* Computing MIN */
    r__1 = 1.f - hydrol_1.areas;
    darea = dmin(r__1,darea);
    flux_1.heaten = (tsnow - stepv_1.tg) * stores_1.csoil * darea * rsnow;
    egit += (flux_1.egi - flux_1.heaten - egidif) * rsnow;
    flux_1.egi = egit;

/* ---------------------------------------------------------------------- */

    d1 = 1.f / aerorx_1.ra + 1.f / aerorx_1.rb + 1.f / aerorx_1.rd;
    taen = ((grads_1.tgs + delts_1.dtg) / aerorx_1.rd + (stepv_1.tc + 
	    delts_1.dtc) / aerorx_1.rb + atmos_1.tm / aerorx_1.ra) / d1;
    hend = (taen - atmos_1.tm) * const_1.rcp / aerorx_1.ra + (ecidif + egidif)
	     / steps_1.dtt;
    y = caerod_1.ht - hend;
    ++i__;
    if (i__ > steps_1.itrunk) {
	goto L200;
    }

    newton_(&caerod_1.ht, &y, &finc, &nox, &nonpos, &iwalk, &lx);
    if (nox == 0) {
	goto L2000;
    }

L200:

/* ---------------------------------------------------------------------- */
/*     exit from non-neutral calculation */
/*     evapotranspiration fluxes calculated first ( j m-2 ) */

/* ---------------------------------------------------------------------- */
/*     calculation of transpiration and soil evaporation fluxes for the */
/*        end of the timestep. see figure (2) of se-86. */

/*      ect         (e-c)   : equation (64) , SE-86 */
/*      egs         (e-s)   : equation (66) , SE-86 */
/* ---------------------------------------------------------------------- */

    hrr = surfrs_1.hr;
    if (surfrs_1.fg < .5f) {
	hrr = 1.f;
    }

    coct = (1.f - hydrol_1.wc) / (surfrs_1.rst * surfrs_1.fc + aerorx_1.rb * 
	    2.f);
    cogs1 = (1.f - hydrol_1.areas) / (aerorx_1.rd + surfrs_1.rsoil * 
	    surfrs_1.fg) * (1.f - hydrol_1.wg) * hrr;
    cogs2 = cogs1 / hrr;

    flux_1.ect = ecpot * coct * const_1.rcp / const_1.psy * steps_1.dtt;
    flux_1.egs = (grads_1.etgs + grads_1.getgs * delts_1.dtg) * cogs1 - (
	    grads_1.ea + flxdif_1.deadtg * delts_1.dtg + flxdif_1.deadtc * 
	    delts_1.dtc + flxdif_1.deadqm * delts_1.dqm) * cogs2;
    flux_1.egs = flux_1.egs * const_1.rcp / const_1.psy * steps_1.dtt;
    egsmax = stepv_1.www[0] / 2.f * soils_1.zdepth[0] * soils_1.poros[0] * 
	    const_1.hlat * 1e3f;
/* Computing MAX */
    r__1 = 0.f, r__2 = flux_1.egs - egsmax;
    egidif += dmax(r__1,r__2);
    flux_1.egs = dmin(flux_1.egs,egsmax);

/* ---------------------------------------------------------------------- */
/*     sensible heat flux calculated with latent heat flux correction */
/* ---------------------------------------------------------------------- */

/*     calculation of sensible heat fluxes for the end of the timestep. */
/*        see figure (2) of se-86.  note that interception loss excess */
/*        energies (ecidif, egidif) are added. */

/*      hc          (hc)    : equation (63) , SE-86 */
/*      hg          (hgs)   : equation (65) , SE-86 */
/* ---------------------------------------------------------------------- */

    flux_1.hc = flux_1.hc + (flxdif_1.hcdtc * delts_1.dtc + flxdif_1.hcdtg * 
	    delts_1.dtg + flxdif_1.hcdth * delts_1.dth) * steps_1.dtt + 
	    ecidif;
    flux_1.hg = flux_1.hg + (flxdif_1.hgdtc * delts_1.dtc + flxdif_1.hgdtg * 
	    delts_1.dtg + flxdif_1.hgdth * delts_1.dth) * steps_1.dtt + 
	    egidif;

/* ---------------------------------------------------------------------- */
/*     test of dew condition. latent heat fluxes set to zero if sign */
/*     of flux changes over time step.excess of energy donated to sensible */
/*     heat flux. */
/*     calculation of total latent heat fluxes,  see figure (2), se-86. */

/*      ec          (ec)    : equation (63) , SE-86 */
/*      eg          (eg)    : equation (65) , SE-86 */
/* ---------------------------------------------------------------------- */

    ecf = r_sign(&c_b38, &ecpot);
    egf = r_sign(&c_b38, &egpot);
    dewc = surfrs_1.fc * 2.f - 1.f;
    dewg = surfrs_1.fg * 2.f - 1.f;

    if (dewc * ecf > 0.f) {
	goto L300;
    }
    flux_1.hc = flux_1.hc + flux_1.eci + flux_1.ect;
    flux_1.eci = 0.f;
    flux_1.ect = 0.f;
L300:
    if (dewg * egf > 0.f) {
	goto L400;
    }
    flux_1.hg = flux_1.hg + flux_1.egs + flux_1.egi;
    flux_1.egs = 0.f;
    flux_1.egi = 0.f;
L400:

    flux_1.ec = flux_1.eci + flux_1.ect;
    flux_1.eg = flux_1.egi + flux_1.egs;

/* ---------------------------------------------------------------------- */
/*     adjustment of : temperatures and vapor pressure */
/*                     net radiation terms */
/*                     storage heat fluxes */
/*                     longwave loss and effective surface temperature */

/* ---------------------------------------------------------------------- */

    grads_1.ta = taen;
    grads_1.ea = grads_1.ea + flxdif_1.deadtc * delts_1.dtc + flxdif_1.deadtg 
	    * delts_1.dtg;

    radabs_1.radt[0] = radabs_1.radt[0] + flxdif_1.rncdtc * delts_1.dtc + 
	    flxdif_1.rncdtg * delts_1.dtg;
    radabs_1.radt[1] = radabs_1.radt[1] + flxdif_1.rngdtc * delts_1.dtc + 
	    flxdif_1.rngdtg * delts_1.dtg;

/* ---------------------------------------------------------------------- */
/*     calculation of storage heat fluxes */

/* ---------------------------------------------------------------------- */

    flux_1.chf = stores_1.ccx / steps_1.dtt * delts_1.dtc;
    flux_1.shf = stores_1.cg / steps_1.dtt * delts_1.dtg + const_1.timcon * 
	    stores_1.cg * 2.f * (grads_1.tgs + delts_1.dtg - stepv_1.td);
/* H */
    flob_1.gflux = const_1.timcon * stores_1.cg * 2.f * (grads_1.tgs + 
	    delts_1.dtg - stepv_1.td);
/* H */

    donor_1.zlwup = donor_1.zlwup - flxdif_1.rncdtc * delts_1.dtc / 2.f - 
	    flxdif_1.rngdtg * delts_1.dtg * (1.f - vstate_1.vcover * (1.f - 
	    radabs_1.thermk));

/* ---------------------------------------------------------------------- */

    return 0;
} /* endtem_ */

/* ====================================================================== */

/*        *********    auxiliary subroutine     ********** */

/* ======================================================================= */

/* Subroutine */ int rbrd_(void)
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real d1, fac, fih, cogr, cogs, temdif;


/* ======================================================================= */

/*      calculation of rb and rd as functions of u2 and temperatures */


/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       rb (grb)       canopy to cas aerodynamic resistance (s m-1) */
/*       rd (grd)       ground to cas aerodynamic resistance (s m-1) */
/*       ta (gta)       cas temperature (k) */
/*                      cas : canopy air space */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* ----------------------------------------------------------------------- */
/*     rb : equation (a9), SE-86 */
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
/* Computing MAX */
    r__1 = .1f, r__2 = stepv_1.tc - atmos_1.tm;
    temdif = dmax(r__1,r__2);
    fac = vdyijt_1.zlt / 890.f * sqrt(sqrt(temdif / .05f));
    aerorx_1.rb = 1.f / (sqrt(grads_1.u2) / rause_1.rbc + fac);

/* ----------------------------------------------------------------------- */
/*     rd : equation (a15), se-86 */
/* ----------------------------------------------------------------------- */

/* Computing MAX */
    r__1 = .1f, r__2 = grads_1.tgs - atmos_1.tm;
    temdif = dmax(r__1,r__2);
    fih = sqrt(const_1.gx * 9.f * temdif * vstate_1.z2 / grads_1.tgs / (
	    grads_1.u2 * grads_1.u2) + 1.f);
    aerorx_1.rd = rause_1.rdc / grads_1.u2 / fih;

/* ----------------------------------------------------------------------- */
/*     calculation of ta, ht and effective soil aero-surface conductance, */
/*        see equation (66) of SE-86 with snow area term added */

/* ----------------------------------------------------------------------- */

    d1 = 1.f / aerorx_1.ra + 1.f / aerorx_1.rb + 1.f / aerorx_1.rd;
    grads_1.ta = (grads_1.tgs / aerorx_1.rd + stepv_1.tc / aerorx_1.rb + 
	    atmos_1.tm / aerorx_1.ra) / d1;
    caerod_1.ht = (grads_1.ta - atmos_1.tm) * const_1.rcp / aerorx_1.ra;

    cogr = (1.f - hydrol_1.wg) / (surfrs_1.rsoil * surfrs_1.fg + aerorx_1.rd);
    cogs = hydrol_1.wg / aerorx_1.rd;
    surfrs_1.cog1 = (cogs + cogr * surfrs_1.hr) * (1.f - hydrol_1.areas) + 
	    hydrol_1.areas / aerorx_1.rd;
    surfrs_1.cog2 = (cogs + cogr) * (1.f - hydrol_1.areas) + hydrol_1.areas / 
	    aerorx_1.rd;

    return 0;
} /* rbrd_ */


/* ======================================================================= */

/* Subroutine */ int phosib_(void)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1;

    /* Builtin functions */
    double exp(doublereal), pow_dd(doublereal *, doublereal *), sqrt(
	    doublereal);

    /* Local variables */
    static real c3, c4;
    static integer ic;
    static real vm, qt;
    static integer ic2;
    static real pfd, eyy[6], zkc, zko, par, h2oa, gog1, gog2, h2oi, h2om, 
	    h2os, park, rrkk, spfy, omss, xdry, xwet, gah2o, gbh2o, h2osl, 
	    pco2y[6], scatg, bintc, range, scatp, respn, templ, temph;
    extern /* Subroutine */ int cycalc_(real *, real *, real *, real *, real *
	    , real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *);
    static real gammas, fparkk, tprcor;
    extern /* Subroutine */ int sortin_(real *, real *, real *, real *, 
	    integer *);



/* ======================================================================= */

/*     calculation of canopy photosynthetic rate using the integrated */
/*     model relating assimilation and stomatal conductance. */
/*     method uses numerical solution based on extrapolation from error */
/*     versus co2i line. */
/*     units are converted from mks to biological units in this routine. */
/*     base reference :  SE-92 */

/*                          units */
/*                         ------- */

/*      pco2m, pco2a, pco2i, po2m                : pascals */
/*      co2a, co2s, co2i, h2oa, h2os, h2oa       : mol mol-1 */
/*      vmax0, respn, assim, gs, gb, ga, pfd     : mol m-2 s-1 */
/*      effcon                                   : mol co2 mol quanta-1 */
/*      gcan, 1/rb, 1/ra, 1/rst                  : m s-1 */
/*      evapkg                                   : kg m-2 s-1 */
/*      q                                        : kg kg-1 */

/*                       conversions */
/*                      ------------- */

/*      1 mol h2o           = 0.018 kg */
/*      1 mol co2           = 0.044 kg */
/*      h2o (mol mol-1)     = ea / psur ( mb mb-1 ) */
/*      h2o (mol mol-1)     = q*mm/(q*mm + 1) */
/*      gs  (co2)           = gs (h2o) * 1./1.6 */
/*      gs  (mol m-2 s-1 )  = gs (m s-1) * 44.6*tf/t*p/po */
/*      par (mol m-2 s-1 )  = par(w m-2) * 4.6*1.e-6 */
/*      mm  (molair/molh2o) = 1.611 */


/*                         output */
/*                      ------------- */

/*      assimn              = canopy net assimilation rate */
/*      ea                  = canopy air space vapor pressure */
/*      1/rst               = canopy conductance */
/*      pco2i               = internal co2 concentration */
/*      respc               = canopy respiration */
/*      respg               = ground respiration */
/*      rstfac(4)      canopy resistance stress factors */

/* ---------------------------------------------------------------------- */

/*         rstfac(1) ( f(h-s) )               : equation (17,18), SE-92 */
/*         rstfac(2) ( f(soil) )              : equation (12 mod), SE-89 */
/*         rstfac(3) ( f(temp) )              : equation (5b)   , CO-92 */
/*         rstfac(4) ( f(h-s)*f(soil)*f(temp)) */


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

/* ---------------------------------------------------------------------- */

/* HR      respg = 0. */
    if (respva2_1.irespg == 0) {
	carbio_1.respg = 0.f;
    }
/* ...  Tropical forest RJA - Patrick Meir (pers com 1994) (molCO2 m-2 s-1) */
    if (respva2_1.irespg == 1) {
	carbio_1.respg = exp((stepv_1.td - 273.15f) * .084f - .23f) * 1e-6f;
    }
/* HR..  Tropical forest k83 - ajuste sazonal: aumento est.chuvosa, reduz est.seca */
/* 	(molCO2 m-2 s-1) */
    if (respva2_1.irespg == 3) {
	xwet = 1.f;
	if (stepv_1.www[0] < .95f) {
	    xwet = stepv_1.www[0];
	}
	if (stepv_1.www[1] < .9f) {
	    d__1 = (doublereal) dmin(stepv_1.www[0],.8f);
	    xwet = pow_dd(&d__1, &c_b44);
	}
	xdry = 1.f - xwet;
	carbio_1.respg = (xwet * exp((stepv_1.td - 273.15f) * .09f - .01f) + 
		xdry * exp((stepv_1.td - 273.15f) * .08f - .3f) - 2.f) * 
		1e-6f;
/* (avg ~ 4a8 K */
    }
/* ...  Cerrado ss PDG - Rocha 2002 (molCO2 m-2 s-1) */
    if (respva2_1.irespg == 2) {
	carbio_1.respg = exp((stepv_1.td - 273.15f) * .159f - 1.886f) * 1e-6f;
    }
/* H */

/* ---------------------------------------------------------------------- */

    c3 = 0.f;
    if (vstate_1.effcon > .07f) {
	c3 = 1.f;
    }
    c4 = 1.f - c3;

/* ---------------------------------------------------------------------- */


/*     calculation of canopy par use parameter. */

/*     fparkk      (pi)     : equation (31) , SE-92 */
/* ----------------------------------------------------------------------- */

    scatp = vdyijt_1.green * (vstate_1.tran[0] + vstate_1.ref[0]) + (1.f - 
	    vdyijt_1.green) * (vstate_1.tran[2] + vstate_1.ref[2]);

    scatg = vstate_1.tran[0] + vstate_1.ref[0];
    park = sqrt(1.f - scatp) * vderiv_1.gmudmu;
    vdyijt_1.fparc = 1.f - exp(-park * vdyijt_1.zlt);
    fparkk = vdyijt_1.fparc / park * vdyijt_1.green;

/* ----------------------------------------------------------------------- */

/*     q-10 temperature effects : */
/*      qt          (qt)    : table (2)     , SE-92 */
/*      qt for vm changed to 2.1 */

/* ----------------------------------------------------------------------- */

    qt = (stepv_1.tc - vstate_1.trop) * .1f;
    respn = vstate_1.respcp * vderiv_1.vmax0 * surfrs_1.rstfac[1];
    d__1 = (doublereal) qt;
    carbio_1.respc = respn * pow_dd(&c_b45, &d__1) / (exp(vstate_1.trda * (
	    stepv_1.tc - vstate_1.trdm)) + 1.f);
    d__1 = (doublereal) qt;
    vm = vderiv_1.vmax0 * pow_dd(&c_b46, &d__1);

    templ = exp(vstate_1.slti * (vstate_1.hlti - stepv_1.tc)) + 1.f;
    temph = exp(vstate_1.shti * (stepv_1.tc - vstate_1.hhti)) + 1.f;
    surfrs_1.rstfac[2] = 1.f / (templ * temph);
    vm = vm / temph * surfrs_1.rstfac[1] * c3 + vm * surfrs_1.rstfac[1] * 
	    surfrs_1.rstfac[2] * c4;

/* ----------------------------------------------------------------------- */

/*     Michaelis-Menten constants for co2 and o2, co2/o2 specificity, */
/*     compensation point */

/*      zkc          (kc)     : table (2)     , SE-92 */
/*      zko          (ko)     : table (2)     , SE-92 */
/*      spfy         (s)      : table (2)     , SE-92 */
/*      gammas       (gamma-*): table (2)     , SE-92 */
/*      omss         (omega-s): equation (13) , SE-92 */
/*      bintc        (b*zlt)  : equation (35) , SE-92 */

/* ----------------------------------------------------------------------- */

    d__1 = (doublereal) qt;
    zkc = pow_dd(&c_b46, &d__1) * 30.f;
    d__1 = (doublereal) qt;
    zko = pow_dd(&c_b48, &d__1) * 3e4f;
    d__1 = (doublereal) qt;
    spfy = pow_dd(&c_b49, &d__1) * 2600.f;
    gammas = atchem_1.po2m * .5f / spfy * c3;
    pfd = vderiv_1.gmudmu * 4.5999999999999992e-6f * (atmos_1.radn[0] + 
	    atmos_1.radn[3]);

    h2oi = grads_1.etc / atmos_1.psur;
    h2oa = grads_1.ea / atmos_1.psur;
    h2om = atmos_1.em / atmos_1.psur;
    h2osl = grads_1.etgs / atmos_1.psur;

    tprcor = const_1.tf * atmos_1.psur * 100.f / 101300.f;

    gbh2o = .5f / aerorx_1.rb * 44.6f * tprcor / stepv_1.tc;
    gah2o = 1.f / aerorx_1.ra * 44.6f * tprcor / atmos_1.tm;
    gog1 = surfrs_1.cog1 * 44.6f * tprcor / grads_1.tgs;
    gog2 = surfrs_1.cog2 * 44.6f * tprcor / grads_1.tgs;

    d__1 = (doublereal) qt;
    rrkk = zkc * (atchem_1.po2m / zko + 1.f) * c3 + vderiv_1.vmax0 / 5.f * 
	    pow_dd(&c_b50, &d__1) * c4;
    par = pfd * vstate_1.effcon * (1.f - scatg);
    bintc = vstate_1.binter * vdyijt_1.zlt * vdyijt_1.green * dmax(.1f,
	    surfrs_1.rstfac[1]);

    d__1 = (doublereal) qt;
    omss = vderiv_1.vmax0 / 2.f * pow_dd(&c_b50, &d__1) / templ * 
	    surfrs_1.rstfac[1] * c3 + rrkk * surfrs_1.rstfac[1] * c4;

/* ----------------------------------------------------------------------- */

/*     first guess is midway between compensation point and maximum */
/*     assimilation rate. */

/* ----------------------------------------------------------------------- */

    range = atchem_1.pco2m * (1.f - 1.6f / vstate_1.gradm) - gammas;

    for (ic = 1; ic <= 6; ++ic) {
	pco2y[ic - 1] = 0.f;
	eyy[ic - 1] = 0.f;
/* L1000: */
    }

    for (ic = 1; ic <= 6; ++ic) {

	ic2 = ic;

	sortin_(eyy, pco2y, &range, &gammas, &ic);

	cycalc_(&fparkk, &vm, &vstate_1.gradm, &bintc, &vstate_1.atheta, &
		vstate_1.btheta, &gah2o, &gbh2o, &gog1, &gog2, &hydrol_1.wc, &
		h2oi, &h2om, &h2osl, &par, &atchem_1.pco2m, &atmos_1.psur, &
		gammas, &carbio_1.respc, &carbio_1.respg, &rrkk, &omss, &c3, &
		c4, &pco2y[ic - 1], &eyy[ic - 1], &carbio_1.gsh2o, &
		carbio_1.assimn, &h2os, &h2oa);

	if ((r__1 = eyy[ic - 1], dabs(r__1)) < .1f) {
	    goto L100;
	}

/* L2000: */
    }

L100:
    carbio_1.pco2i = pco2y[ic2 - 1];

    surfrs_1.rstfac[0] = h2os / h2oi;
    surfrs_1.rstfac[3] = surfrs_1.rstfac[0] * surfrs_1.rstfac[1] * 
	    surfrs_1.rstfac[2];
/* Computing MIN */
    r__1 = 1e6f, r__2 = 1.f / (carbio_1.gsh2o * stepv_1.tc / (tprcor * 44.6f))
	    ;
    surfrs_1.rst = dmin(r__1,r__2);
    grads_1.ea = h2oa * atmos_1.psur;

    return 0;
} /* phosib_ */


/* ======================================================================= */

/* Subroutine */ int cycalc_(real *fparkk, real *vm, real *gradm, real *bintc,
	 real *atheta, real *btheta, real *gah2o, real *gbh2o, real *gog1, 
	real *gog2, real *wc, real *h2oi, real *h2om, real *h2osl, real *par, 
	real *pco2m, real *psur, real *gammas, real *respc, real *respg, real 
	*rrkk, real *omss, real *c3, real *c4, real *pco2i, real *eyy, real *
	gsh2o, real *assimn, real *h2os, real *h2oa)
{
    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real omc, ome, omp, oms, div2, co2s, div3, beta, pco2a, co2st, 
	    hcdma, alpha, aquad, bquad, cquad, assim, assmt, pco2in, sqrtin;


/* ======================================================================= */

/*     calculation equivalent to steps in figure 4 of SE-92 */
/*     c4 calculation based on CO-92. */

/* ----------------------------------------------------------------------- */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       pco2i          canopy internal co2 concentration (mol mol-1) */
/*       gsh2o          canopy conductance (mol m-2 s-1) */
/*       h2os           canopy surface h2o concentration (mol mol-1) */

/* ++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++ */

/*       omc            rubisco limited assimilation (mol m-2 s-1) */
/*                        (omega-c): equation (11) , SE-92 */
/*       ome            light limited assimilation (mol m-2 s-1) */
/*                        (omega-e): equation (12) , SE-92 */
/*       oms            sink limited assimilation (mol m-2 s-1) */
/*       co2s           canopy surface co2 concentration (mol mol-1) */
/*                        equation (18c) , SE-92 */
/*       assimn         (a-n)    :  equation (14,15), SE-92 */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

    omc = *vm * (*pco2i - *gammas) / (*pco2i + *rrkk) * *c3 + *vm * *c4;
    ome = *par * (*pco2i - *gammas) / (*pco2i + *gammas * 2.f) * *c3 + *par * 
	    *c4;
/* Computing MAX */
/* Computing 2nd power */
    r__3 = ome + omc;
    r__1 = 0.f, r__2 = r__3 * r__3 - *atheta * 4.f * ome * omc;
    sqrtin = dmax(r__1,r__2);
    omp = (ome + omc - sqrt(sqrtin)) / (*atheta * 2.f);
    oms = *omss * *c3 + *omss * *pco2i * *c4;
/* Computing MAX */
/* Computing 2nd power */
    r__3 = omp + oms;
    r__1 = 0.f, r__2 = r__3 * r__3 - *btheta * 4.f * omp * oms;
    sqrtin = dmax(r__1,r__2);
    assim = (oms + omp - sqrt(sqrtin)) / (*btheta * 2.f);
    *assimn = (assim - *respc) * *fparkk;

/* ----------------------------------------------------------------------- */
/*     gah2o bottom stopped to prevent negative values of pco2a */
/* ----------------------------------------------------------------------- */

    pco2a = *pco2m - 1.4f / dmax(.446f,*gah2o) * (*assimn - *respg) * *psur * 
	    100.f;
    co2s = pco2a / (*psur * 100.f) - *assimn * 1.4f / *gbh2o;

    assmt = dmax(1e-12f,*assimn);
/* Computing MIN */
    r__1 = co2s, r__2 = pco2a / (*psur * 100.f);
    co2st = dmin(r__1,r__2);
/* Computing MAX */
    r__1 = co2st, r__2 = 1.f / (*psur * 100.f);
    co2st = dmax(r__1,r__2);

    div2 = *gah2o + *gbh2o + *gog2;
    hcdma = *h2oi * co2st / (*gradm * assmt);
    alpha = hcdma * *gbh2o * (1.f - *wc) / div2;
    beta = (-hcdma * *bintc * *gbh2o * (1.f - *wc) + *h2oi * *gbh2o * *wc + *
	    h2osl * *gog1 + *h2om * *gah2o) / div2;
    aquad = hcdma;
    bquad = *gbh2o * (hcdma - alpha) - *h2oi - *bintc * hcdma;
    cquad = -(*gbh2o) * (hcdma * *bintc + beta);

/* Computing MAX */
/* Computing 2nd power */
    r__3 = bquad;
    r__1 = 0.f, r__2 = r__3 * r__3 - aquad * 4.f * cquad;
    sqrtin = dmax(r__1,r__2);
    *gsh2o = (-bquad + sqrt(sqrtin)) / (aquad * 2.f);
    *h2os = (*gsh2o - *bintc) * hcdma;
    *h2os = dmin(*h2os,*h2oi);
    *h2os = dmax(*h2os,1e-7f);
    *gsh2o = *h2os / hcdma + *bintc;
    div3 = *gbh2o * *gsh2o / (*gbh2o + *gsh2o) * (1.f - *wc) + *gbh2o * *wc + 
	    *gog2 + *gah2o;
    *h2oa = ((*h2oi - *h2oi * *gsh2o / (*gbh2o + *gsh2o)) * *gsh2o * (1.f - *
	    wc) + *h2oi * *gbh2o * *wc + *h2osl * *gog1 + *h2om * *gah2o) / 
	    div3;
    *h2os = (*h2oa * *gbh2o + *h2oi * *gsh2o) / (*gbh2o + *gsh2o);

/* ----------------------------------------------------------------------- */
/*     implied value of co2i derived from assimilation rate and stomatal */
/*     conductance. */
/* ----------------------------------------------------------------------- */

    pco2in = (co2s - *assimn * 1.6f / *gsh2o) * *psur * 100.f;
    *eyy = *pco2i - pco2in;

    return 0;
} /* cycalc_ */


/* ======================================================================= */

/* Subroutine */ int sortin_(real *eyy, real *pco2y, real *range, real *
	gammas, integer *ic)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    static real a, b;
    static integer i__, j, n, i1, i2, i3, is, ix;
    static real ac1, ac2, bc1, bc2, cc1, cc2;
    static integer isp;
    static real emin, pmin, pco2b, aterm, bterm, cterm, pco2yl, pco2yq;


/* ======================================================================= */

/* ----------------------------------------------------------------------- */
/*     arranges successive pco2/error pairs in order of increasing pco2. */
/*     estimates next guess for pco2 using combination of linear and */
/*     quadratic fits. */

/* ----------------------------------------------------------------------- */



    /* Parameter adjustments */
    --pco2y;
    --eyy;

    /* Function Body */
    if (*ic >= 4) {
	goto L500;
    }
    pco2y[1] = *gammas + *range * .5f;
    pco2y[2] = *gammas + *range * (.5f - r_sign(&c_b38, &eyy[1]) * .3f);
    pco2y[3] = pco2y[1] - (pco2y[1] - pco2y[2]) / (eyy[1] - eyy[2] + 1e-10f) *
	     eyy[1];

    pmin = dmin(pco2y[1],pco2y[2]);
    emin = dmin(eyy[1],eyy[2]);
    if (emin > 0.f && pco2y[3] > pmin) {
	pco2y[3] = *gammas;
    }
    goto L200;
L500:

    n = *ic - 1;
    i__1 = n;
    for (j = 2; j <= i__1; ++j) {
	a = eyy[j];
	b = pco2y[j];
	for (i__ = j - 1; i__ >= 1; --i__) {
	    if (eyy[i__] <= a) {
		goto L100;
	    }
	    eyy[i__ + 1] = eyy[i__];
	    pco2y[i__ + 1] = pco2y[i__];
/* L2000: */
	}
	i__ = 0;
L100:
	eyy[i__ + 1] = a;
	pco2y[i__ + 1] = b;
/* L1000: */
    }

    pco2b = 0.f;
    is = 1;
    i__1 = n;
    for (ix = 1; ix <= i__1; ++ix) {
	if (eyy[ix] < 0.f) {
	    pco2b = pco2y[ix];
	}
	if (eyy[ix] < 0.f) {
	    is = ix;
	}
/* L3000: */
    }
    i1 = is - 1;
    i1 = max(1,i1);
/* Computing MIN */
    i__1 = n - 2;
    i1 = min(i__1,i1);
    i2 = i1 + 1;
    i3 = i1 + 2;
    isp = is + 1;
    isp = min(isp,n);
    is = isp - 1;

    pco2yl = pco2y[is] - (pco2y[is] - pco2y[isp]) / (eyy[is] - eyy[isp]) * 
	    eyy[is];

/* ---------------------------------------------------------------------- */
/*   method using a quadratic fit */
/* ---------------------------------------------------------------------- */

    ac1 = eyy[i1] * eyy[i1] - eyy[i2] * eyy[i2];
    ac2 = eyy[i2] * eyy[i2] - eyy[i3] * eyy[i3];
    bc1 = eyy[i1] - eyy[i2];
    bc2 = eyy[i2] - eyy[i3];
    cc1 = pco2y[i1] - pco2y[i2];
    cc2 = pco2y[i2] - pco2y[i3];
    bterm = (cc1 * ac2 - cc2 * ac1) / (bc1 * ac2 - ac1 * bc2);
    aterm = (cc1 - bc1 * bterm) / ac1;
    cterm = pco2y[i2] - aterm * eyy[i2] * eyy[i2] - bterm * eyy[i2];
    pco2yq = cterm;
    pco2yq = dmax(pco2yq,pco2b);
    pco2y[*ic] = (pco2yl + pco2yq) / 2.f;

L200:

/* Computing MAX */
    r__1 = pco2y[*ic];
    pco2y[*ic] = dmax(r__1,.01f);

    return 0;
} /* sortin_ */


/* ====================================================================== */

/* Subroutine */ int delrn_(void)
{
    static real tc3, tg3, fac1, fac2;


/* ====================================================================== */

/*     partial derivatives of radiative and sensible heat fluxes */

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

/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

    tc3 = stepv_1.tc * stepv_1.tc * stepv_1.tc;
    tg3 = grads_1.tgs * grads_1.tgs * grads_1.tgs;
    fac1 = (1.f - radabs_1.thermk) * vstate_1.vcover;
    fac2 = 1.f;

    flxdif_1.rncdtc = fac1 * -8.f * const_1.stefan * tc3;
    flxdif_1.rncdtg = fac1 * 4.f * fac2 * const_1.stefan * tg3;

    flxdif_1.rngdtg = fac2 * -4.f * const_1.stefan * tg3;
    flxdif_1.rngdtc = fac1 * 4.f * fac2 * const_1.stefan * tc3;

    return 0;
} /* delrn_ */

/* ====================================================================== */

/* Subroutine */ int delhf_(void)
{
    static real d1, hcdqm, hgdqm;


/* ====================================================================== */

/*     calculation of partial derivatives of canopy and ground sensible */
/*     heat fluxes with respect to tc, tgs, and theta-m. */
/*     calculation of initial sensible heat fluxes. */

/* ======================================================================== */


/*       hc             canopy sensible heat flux (j m-2) */
/*       hg             ground sensible heat flux (j m-2) */
/*       hcdtc          dhc/dtc */
/*       hcdtg          dhc/dtgs */
/*       hcdth          dhc/dth */
/*       hgdtc          dhg/dtc */
/*       hgdtg          dhg/dtgs */
/*       hgdth          dhg/dth */
/*       aac            dh/dtc */
/*       aag            dh/dtgs */
/*       aam            dh/dth */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     fluxes expressed in joules m-2 */

/*      hc  ( h-c ) : equation(63), SE-86 */
/*      hg  ( h-g ) : equation(65), SE-86 */

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

/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

    d1 = 1.f / aerorx_1.ra + 1.f / aerorx_1.rb + 1.f / aerorx_1.rd;
    grads_1.ta = (grads_1.tgs / aerorx_1.rd + stepv_1.tc / aerorx_1.rb + 
	    atmos_1.tm / aerorx_1.ra) / d1;

    flux_1.hc = const_1.rcp * (stepv_1.tc - grads_1.ta) / aerorx_1.rb * 
	    steps_1.dtt;
    flux_1.hg = const_1.rcp * (grads_1.tgs - grads_1.ta) / aerorx_1.rd * 
	    steps_1.dtt;
/* ---------------------------------------------------------------------- */

/*      n.b.      fluxes expressed in joules m-2 */

/*      hcdtc     (dhc/dtc) : equation (14) , SA-89B */
/*      hcdtg     (dhc/dtgs): equation (14) , SA-89B */
/*      hcdth     (dhc/dth) : equation (14) , SA-89B */
/*      hgdtc     (dhg/dtc) : equation (15) , SA-89B */
/*      hgdtg     (dhg/dtgs): equation (15) , SA-89B */
/*      hgdth     (dhg/dth) : equation (15) , SA-89B */
/*      aac       (dh/dtc)  : equation (12) , SA-89B */
/*      aag       (dh/dtgs) : equation (12) , SA-89B */
/*      aam       (dh/dth)  : equation (12) , SA-89B */
/* ---------------------------------------------------------------------- */

    flxdif_1.hcdtc = const_1.rcp / aerorx_1.rb * (1.f / aerorx_1.ra + 1.f / 
	    aerorx_1.rd) / d1;
    flxdif_1.hcdtg = -const_1.rcp / (aerorx_1.rb * aerorx_1.rd) / d1;

    flxdif_1.hgdtg = const_1.rcp / aerorx_1.rd * (1.f / aerorx_1.ra + 1.f / 
	    aerorx_1.rb) / d1;
    flxdif_1.hgdtc = -const_1.rcp / (aerorx_1.rd * aerorx_1.rb) / d1;

    flxdif_1.hcdth = -const_1.rcp / (aerorx_1.rb * aerorx_1.ra) / d1 * 
	    atmos_1.bps;
    hcdqm = 0.f;

    flxdif_1.hgdth = -const_1.rcp / (aerorx_1.rd * aerorx_1.ra) / d1 * 
	    atmos_1.bps;
    hgdqm = 0.f;

    flxdif_1.aag = 1.f / (aerorx_1.rd * d1);
    flxdif_1.aac = 1.f / (aerorx_1.rb * d1);
    flxdif_1.aam = 1.f / (aerorx_1.ra * d1) * atmos_1.bps;

    return 0;
} /* delhf_ */

/* ====================================================================== */

/* Subroutine */ int delef_(void)
{
    /* System generated locals */
    real r__1;

    /* Local variables */
    static real d2, sh, rcc, coc, hrr, top, cogr, cogs, deadem;


/* ====================================================================== */

/*     calculation of partial derivatives of canopy and ground latent */
/*     heat fluxes with respect to tc, tgs, theta-m, and qm. */
/*     calculation of initial latent heat fluxes. */

/*      ec  ( e-c ) : equation(64), SE-86 */
/*      eg  ( e-gs) : equation(66), SE-86 */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       ec             ect + eci */
/*       eg             egs + egi */
/*       ecdtc          dec/dtc */
/*       ecdtg          dec/dtgs */
/*       ecdqm          dec/dqm */
/*       egdtc          deg/dtc */
/*       egdtg          deg/dtgs */
/*       egdqm          deg/dqm */
/*       bbc            de/dtc */
/*       bbg            de/dtgs */
/*       bbm            de/dqm */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

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

/* ---------------------------------------------------------------------- */
/*     modification for soil dryness : hr = rel. humidity in top layer */
/* ---------------------------------------------------------------------- */

/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

    hrr = surfrs_1.hr;
    if (surfrs_1.fg < .5f) {
	hrr = 1.f;
    }

/* ----------------------------------------------------------------------- */

/*     calculation of surface resistance components, see equations (64,66) */
/*       of SE-86 */

/* ----------------------------------------------------------------------- */

    rcc = surfrs_1.rst * surfrs_1.fc + aerorx_1.rb * 2.f;
    coc = (1.f - hydrol_1.wc) / rcc + hydrol_1.wc / (aerorx_1.rb * 2.f);

    cogr = (1.f - hydrol_1.wg) / (surfrs_1.rsoil * surfrs_1.fg + aerorx_1.rd);
    cogs = hydrol_1.wg / aerorx_1.rd;
    surfrs_1.cog1 = (cogs + cogr * hrr) * (1.f - hydrol_1.areas) + 
	    hydrol_1.areas / aerorx_1.rd;
    surfrs_1.cog2 = (cogs + cogr) * (1.f - hydrol_1.areas) + hydrol_1.areas / 
	    aerorx_1.rd;

    d2 = 1.f / aerorx_1.ra + coc + surfrs_1.cog2;
    top = coc * grads_1.etc + surfrs_1.cog1 * grads_1.etgs + atmos_1.em / 
	    aerorx_1.ra;
    grads_1.ea = top / d2;

    flux_1.ec = (grads_1.etc - grads_1.ea) * coc * const_1.rcp / const_1.psy *
	     steps_1.dtt;
    flux_1.eg = (grads_1.etgs * surfrs_1.cog1 - grads_1.ea * surfrs_1.cog2) * 
	    const_1.rcp / const_1.psy * steps_1.dtt;

    flxdif_1.deadtc = grads_1.getc * coc / d2;
    flxdif_1.deadtg = grads_1.getgs * surfrs_1.cog1 / d2;

/* ----------------------------------------------------------------------- */
/*      ecdtc     (dec/dtc) : equation (14) , SA-89B */
/*      ecdtg     (dec/dtgs): equation (14) , SA-89B */
/*      ecdqm     (dec/dqm) : equation (14) , SA-89B */
/*      egdtc     (deg/dtc) : equation (15) , SA-89B */
/*      egdtg     (deg/dtgs): equation (15) , SA-89B */
/*      egdqm     (deg/dqm) : equation (15) , SA-89B */
/*      bbc       (de/dtc)  : equation (13) , SA-89B */
/*      bbg       (de/dtgs) : equation (13) , SA-89B */
/*      bbm       (de/dqm)  : equation (13) , SA-89B */
/* ----------------------------------------------------------------------- */

    flxdif_1.ecdtc = (grads_1.getc - flxdif_1.deadtc) * coc * const_1.rcp / 
	    const_1.psy;
    flxdif_1.ecdtg = -flxdif_1.deadtg * coc * const_1.rcp / const_1.psy;

    flxdif_1.egdtg = (grads_1.getgs * surfrs_1.cog1 - flxdif_1.deadtg * 
	    surfrs_1.cog2) * const_1.rcp / const_1.psy;
    flxdif_1.egdtc = -flxdif_1.deadtc * surfrs_1.cog2 * const_1.rcp / 
	    const_1.psy;

    sh = const_1.epsfac * atmos_1.em / (atmos_1.psur - atmos_1.em);
    deadem = 1.f / (aerorx_1.ra * d2);
/* Computing 2nd power */
    r__1 = const_1.epsfac + sh;
    flxdif_1.demdqm = const_1.epsfac * atmos_1.psur / (r__1 * r__1);
    flxdif_1.deadqm = deadem * flxdif_1.demdqm;

    flxdif_1.ecdqm = -flxdif_1.deadqm * coc * const_1.rcp / const_1.psy;
    flxdif_1.egdqm = -flxdif_1.deadqm * surfrs_1.cog2 * const_1.rcp / 
	    const_1.psy;

/* Computing 2nd power */
    r__1 = atmos_1.psur - grads_1.etgs;
    flxdif_1.bbg = surfrs_1.cog1 / d2 * grads_1.getgs * const_1.epsfac * 
	    atmos_1.psur / (r__1 * r__1);
/* Computing 2nd power */
    r__1 = atmos_1.psur - grads_1.etc;
    flxdif_1.bbc = coc / d2 * grads_1.getc * const_1.epsfac * atmos_1.psur / (
	    r__1 * r__1);
    flxdif_1.bbm = 1.f / (aerorx_1.ra * d2);

    return 0;
} /* delef_ */

/* ====================================================================== */

/* Subroutine */ int sibslv_(void)
{
    static integer i__, j;
    static real gv, psb, dwb, fws, cvec[4], chin[20]	/* was [4][5] */, 
	    dthb, etem, fths, work[20]	/* was [4][5] */, grav2;
    extern /* Subroutine */ int gauss_(real *, integer *, integer *, real *, 
	    real *);
    static real pblsib[16]	/* was [4][4] */, solvec[4];



/* ====================================================================== */

/*     solve for time changes of pbl and sib variables, */
/*     using a semi-implicit scheme. */

/*      dtc, dtg, dth, dqm  : equations(12-15) , SA-89B + radiation terms */


/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       dtc            canopy temperature increment (K) */
/*       dtg            ground surface temperature increment (K) */
/*       dth            mixed layer potential temperature increment (K) */
/*       dqm            mixed layer mixing ratio increment (kg kg-1) */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

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


/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */


    grav2 = const_1.gx * .01f;
    gv = grav2 * const_1.rhoair / aerorx_1.ra;
    psb = 100.f;

    etem = 0.f;
    dthb = 0.f;
    dwb = 0.f;

/*     cvec uses provisional values of the fluxes. */

    fths = (flux_1.hc + flux_1.hg) / (steps_1.dtt * const_1.cpair * 
	    atmos_1.bps);
    fws = (flux_1.ec + flux_1.eg) / (steps_1.dtt * const_1.hlat);

/*     tg equation */

    pblsib[0] = stores_1.cg / steps_1.dtt + flxdif_1.hgdtg + flxdif_1.egdtg - 
	    flxdif_1.rngdtg + const_1.timcon * stores_1.cg * 2.f;
    pblsib[4] = flxdif_1.hgdtc + flxdif_1.egdtc - flxdif_1.rngdtc;
    pblsib[8] = flxdif_1.hgdth;
    pblsib[12] = flxdif_1.egdqm;

/*     tc equation */

    pblsib[1] = flxdif_1.hcdtg + flxdif_1.ecdtg - flxdif_1.rncdtg;
    pblsib[5] = stores_1.ccx / steps_1.dtt + flxdif_1.hcdtc + flxdif_1.ecdtc 
	    - flxdif_1.rncdtc;
    pblsib[9] = flxdif_1.hcdth;
    pblsib[13] = flxdif_1.ecdqm;

/*     theta equation */

    pblsib[2] = -gv * flxdif_1.aag;
    pblsib[6] = -gv * flxdif_1.aac;
    pblsib[10] = -gv * (flxdif_1.aam - 1.f) + etem + psb / steps_1.dtt;
    pblsib[14] = 0.f;

/*     sh equation */

    pblsib[3] = -gv * flxdif_1.bbg;
    pblsib[7] = -gv * flxdif_1.bbc;
    pblsib[11] = 0.f;
    pblsib[15] = -gv * (flxdif_1.bbm - 1.f) + etem + psb / steps_1.dtt;

    cvec[0] = radabs_1.radt[1] - flux_1.hg / steps_1.dtt - flux_1.eg / 
	    steps_1.dtt - const_1.timcon * (grads_1.tgs - stepv_1.td) * 
	    stores_1.cg * 2.f;
    cvec[1] = radabs_1.radt[0] - flux_1.hc / steps_1.dtt - flux_1.ec / 
	    steps_1.dtt;
    cvec[2] = grav2 * fths + etem * dthb;
    cvec[3] = grav2 * fws + etem * dwb;

/*     solve 4 x 4 matrix equation */

    for (j = 1; j <= 4; ++j) {
	for (i__ = 1; i__ <= 4; ++i__) {
/* L4000: */
	    chin[i__ + (j << 2) - 5] = pblsib[i__ + (j << 2) - 5];
	}
    }
    for (i__ = 1; i__ <= 4; ++i__) {
/* L4100: */
	chin[i__ + 15] = cvec[i__ - 1];
    }

    gauss_(chin, &c__4, &c__5, solvec, work);

    delts_1.dtg = solvec[0];
    delts_1.dtc = solvec[1];
    delts_1.dth = solvec[2];
    delts_1.dqm = solvec[3];

    return 0;
} /* sibslv_ */


/* ====================================================================== */

/* Subroutine */ int dtcdtg_(void)
{
    static real denom, ccodtc, ccodtg, gcodtc, gcodtg, ccorhs, gcorhs;


/* ---------------------------------------------------------------------- */

/*     calculation of temperature tendencies assuming no interaction */
/*     with the pbl : equations(69,70), SE-86 */

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

/* ======================================================================= */
/*                              pardif.h */
/* ======================================================================= */

    ccodtc = stores_1.ccx / steps_1.dtt - flxdif_1.rncdtc + flxdif_1.hcdtc + 
	    flxdif_1.ecdtc;
    ccodtg = -flxdif_1.rncdtg + flxdif_1.hcdtg + flxdif_1.ecdtg;
    ccorhs = radabs_1.radt[0] - (flux_1.hc + flux_1.ec) / steps_1.dtt;

    gcodtg = stores_1.cg / steps_1.dtt + const_1.timcon * stores_1.cg * 2.f - 
	    flxdif_1.rngdtg + flxdif_1.hgdtg + flxdif_1.egdtg;
    gcodtc = -flxdif_1.rngdtc + flxdif_1.hgdtc + flxdif_1.egdtc;
    gcorhs = radabs_1.radt[1] - const_1.timcon * stores_1.cg * 2.f * (
	    grads_1.tgs - stepv_1.td) - (flux_1.hg + flux_1.eg) / steps_1.dtt;

    denom = ccodtc * gcodtg - ccodtg * gcodtc;

    delts_1.dtc = (ccorhs * gcodtg - ccodtg * gcorhs) / denom;
    delts_1.dtg = (ccodtc * gcorhs - ccorhs * gcodtc) / denom;

    return 0;
} /* dtcdtg_ */

/* ======================================================================= */

/* Subroutine */ int snow2_(void)
{
    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static real tn, ts, cct, dts, safe, heat;
    static integer iveg;
    static real cctt, flux, avex, dtsg, cool, dtsg2, dtsg3, realc, realg, 
	    zmelt, tbulk, tsnow, zmelt2, avheat, exheat, freeze, avmelt, 
	    snowhc, exmelt, fluxef;


/* ======================================================================= */

/*    snowmelt / refreeze calculation */
/* ---------------------------------------------------------------------- */

/*     calculation of snowmelt and modification of temperatures */

/*     modification deals with snow patches: */
/*          ts < tf, tsnow = ts */
/*          ts > tf, tsnow = tf */

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
    for (iveg = 1; iveg <= 2; ++iveg) {

	realc = (2 - iveg) * 1.f;
	realg = (iveg - 1) * 1.f;

	cctt = realc * stores_1.ccx + realg * stores_1.cg;
	cct = realc * stores_1.ccx + realg * stores_1.csoil;
	ts = realc * stepv_1.tc + realg * stepv_1.tg;
	dts = realc * delts_1.dtc + realg * delts_1.dtg;
	flux = realc * flux_1.chf + realg * delts_1.dtg / steps_1.dtt * 
		stores_1.cg;

/* Computing MIN */
	r__1 = const_1.tf - .01f;
	tsnow = dmin(r__1,ts);
/* Computing MIN */
	r__1 = .05f, r__2 = stepv_1.snoww[iveg - 1];
	snowhc = dmin(r__1,r__2) * const_1.cw * realg;
	zmelt = 0.f;

	if (stepv_1.snoww[iveg - 1] > 0.f) {
	    goto L100;
	}
	if (ts + dts > const_1.tf) {
	    goto L500;
	}

/* ----------------------------------------------------------------------- */

/*     no snow  present, simple thermal balance with possible freezing. */

/* ----------------------------------------------------------------------- */

/* Computing MIN */
	r__1 = 0.f, r__2 = flux * steps_1.dtt - (const_1.tf - .01f - ts) * 
		cctt;
	freeze = dmin(r__1,r__2);
/* Computing MIN */
	r__1 = stepv_1.capac[iveg - 1], r__2 = -freeze / const_1.snomel;
	stepv_1.snoww[iveg - 1] = dmin(r__1,r__2);
	zmelt = stepv_1.capac[iveg - 1] - stepv_1.snoww[iveg - 1];
	stepv_1.capac[iveg - 1] = 0.f;
	dts += stepv_1.snoww[iveg - 1] * const_1.snomel / cctt;
	goto L500;

/* ----------------------------------------------------------------------- */

/*     snow present */

/* ----------------------------------------------------------------------- */

L100:

	if (ts < const_1.tf && ts + dts < const_1.tf) {
	    goto L500;
	}
	if (ts > const_1.tf) {
	    goto L200;
	}

/* ----------------------------------------------------------------------- */

/*     snow present : ts < tf,  ts+dts > tf */

/* ----------------------------------------------------------------------- */

	avex = flux - (const_1.tf - .01f - ts) * cctt / steps_1.dtt;
	avmelt = avex / const_1.snomel * (hydrol_1.areas * realg + realc) * 
		steps_1.dtt;
/* Computing MIN */
	r__1 = avmelt, r__2 = stepv_1.snoww[iveg - 1];
	zmelt = dmin(r__1,r__2);
	stepv_1.snoww[iveg - 1] -= zmelt;
	avheat = avex * (1.f - hydrol_1.areas) * realg + (avmelt - zmelt) * 
		const_1.snomel / steps_1.dtt;

/* Computing MAX */
	r__1 = 1.f - hydrol_1.areas * realg;
	safe = dmax(r__1,1e-8f);
	dts = const_1.tf - .01f - ts + avheat / (cctt * safe) * steps_1.dtt;
	goto L500;

/* ----------------------------------------------------------------------- */

/*     snow present and ts > tf : ground only. */

/* ----------------------------------------------------------------------- */

L200:

	tbulk = tsnow * hydrol_1.areas + ts * (1.f - hydrol_1.areas);
	tn = tbulk + dts;
	exheat = cct * (1.001f - dmax(.1f,hydrol_1.areas)) * dts;
	exmelt = flux * steps_1.dtt - exheat;
	heat = exheat;
	dtsg = exheat / (cct * (1.001f - hydrol_1.areas));
	if (ts + dtsg > const_1.tf) {
	    goto L300;
	}
	heat = (const_1.tf - .01f - ts) * (cct * (1.f - hydrol_1.areas));
	dtsg = const_1.tf - .01f - ts;

L300:
	exmelt = exmelt + exheat - heat;

	if (exmelt < 0.f) {
	    goto L400;
	}
	zmelt = exmelt / const_1.snomel;
	if (const_1.asnow * (stepv_1.snoww[iveg - 1] - zmelt) < 1.f) {
/* Computing MAX */
	    r__1 = 0.f, r__2 = stepv_1.snoww[iveg - 1] - 1.f / const_1.asnow;
	    zmelt = dmax(r__1,r__2);
	}
	stepv_1.snoww[iveg - 1] -= zmelt;
	exmelt -= zmelt * const_1.snomel;
	zmelt2 = exmelt / (cct * (ts - const_1.tf) * const_1.asnow + 
		const_1.snomel);
/* Computing MIN */
	r__1 = zmelt2, r__2 = stepv_1.snoww[iveg - 1];
	zmelt2 = dmin(r__1,r__2);
	zmelt += zmelt2;
	stepv_1.snoww[iveg - 1] -= zmelt2;
	exmelt -= zmelt2 * (cct * (ts - const_1.tf) * const_1.asnow + 
		const_1.snomel);
	dts = dtsg + exmelt / cct;
	goto L500;

L400:
/* Computing MIN */
	r__1 = 0.f, r__2 = const_1.tf - .01f - (ts + dtsg);
	cool = dmin(r__1,r__2) * cct * (1.f - hydrol_1.areas);
	dtsg2 = dmax(cool,exmelt) / (cct * (1.001f - hydrol_1.areas));
	exmelt -= dtsg2 * cct * (1.f - hydrol_1.areas);
	dtsg3 = exmelt / cctt;
	dts = dtsg + dtsg2 + dtsg3;

L500:

	stepv_1.www[0] += zmelt / (soils_1.poros[0] * soils_1.zdepth[0]);

	delts_1.dtc = delts_1.dtc * realg + dts * realc;
	delts_1.dtg = delts_1.dtg * realc + dts * realg;

/* L1000: */
    }

    fluxef = flux_1.shf - stores_1.cg * delts_1.dtg / steps_1.dtt;
    delts_1.dtd = fluxef / (stores_1.cg * 2.f * sqrt(const_1.pie * 365.f)) * 
	    steps_1.dtt;

    return 0;
} /* snow2_ */

/* ======================================================================= */

/* Subroutine */ int radc2_(void)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double r_mod(real *, real *), cos(doublereal), sin(doublereal), tan(
	    doublereal), acos(doublereal);

    /* Local variables */
    static real h__, sr, ss, dec, hac, rfd, cosd, sind, dawn, sols, dusk, 
	    coshr, decmax, season, dayspy;


/* ======================================================================= */

/*     solar zenith angle computation; downcoming radiation at bottom. */

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
    dayspy = 365.f;
    if (r_mod(&govern_1.year, &c_b81) == 0.f) {
	dayspy = 366.f;
    }

/* ----------------------------------------------------------------------- */
/*    julian day and time update; skip on 1st time step (initialized) */
/* ----------------------------------------------------------------------- */
    if (steps_1.iter == 1) {
	goto L10;
    }
    govern_1.time += steps_1.dtt / 3600.f;
    if (govern_1.time >= 23.99f) {
	govern_1.time = 0.f;
    }
    govern_1.day += steps_1.dtt / 86400.f;

L10:

    if (govern_1.day > dayspy) {
	govern_1.year += 1.f;
    }
    if (govern_1.day > dayspy) {
	govern_1.day -= dayspy;
    }

/* ----------------------------------------------------------------------- */
/*    solar declination calculation */
/* ----------------------------------------------------------------------- */

    decmax = const_1.pie * .13055555555555556f;
    r__1 = govern_1.year + 3.f;
    sols = r_mod(&r__1, &c_b81) * .25f + 172.54166666666666f;

    season = (govern_1.day - sols) / 365.2f;
    dec = decmax * cos(const_1.pie * 2.f * season);

    rfd = const_1.pie / 180.f;
    sind = sin(dec);
    cosd = cos(dec);
    hac = -tan(site_1.zlat * rfd) * tan(dec);
    hac = dmin(hac,1.f);
    hac = dmax(hac,-1.f);

/* ----------------------------------------------------------------------- */
/*     h is the half-day length (in radians) */
/* ----------------------------------------------------------------------- */

    h__ = acos(hac);
    dawn = -h__;
    dusk = h__;
    sr = 12.f - h__ / (rfd * 15.f);
    ss = h__ / (rfd * 15.f) + 12.f;
    coshr = cos(-const_1.pie + (govern_1.time + steps_1.dtt * .5f / 3600.f) / 
	    24.f * 2.f * const_1.pie);
    atmos_1.sunang = sin(site_1.zlat * rfd) * sind + cos(site_1.zlat * rfd) * 
	    cosd * coshr;
    atmos_1.sunang = dmax(.01f,atmos_1.sunang);

    return 0;
} /* radc2_ */

/* ====================================================================== */

/* Subroutine */ int rasite_(void)
{
    /* Format strings */
    static char fmt_900[] = "(/,\002 convergence failure in rasite - unstabl"
	    "e case\002)";
    static char fmt_910[] = "(/,\002 convergence failure in rasite - stable "
	    "case\002)";

    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double log(doublereal);
    integer s_wsfe(cilist *), e_wsfe(void);
    double sqrt(doublereal);

    /* Local variables */
    static real y;
    static integer lx;
    static real zl, hm1, hm2, ps1, ps2, us1, us2, zx1, zx2, raf, top, bot, 
	    ram;
    static integer nox;
    static real hss, uss, hrb, arg1, raf1, gfac, finc;
    extern /* Subroutine */ int stab_(real *, real *, real *, real *, real *, 
	    real *);
    static real uest, coef3;
    static integer iwalk;
    static real hress;
    extern /* Subroutine */ int rafcal_(real *, real *, real *, real *);
    static real rafmax, rbbest;
    extern /* Subroutine */ int unstab_(real *, real *, real *, real *, real *
	    , real *, real *), newton_(real *, real *, real *, integer *, 
	    integer *, integer *, integer *);
    static integer nonpos;

    /* Fortran I/O blocks */
    static cilist io___299 = { 0, 6, 0, fmt_900, 0 };
    static cilist io___305 = { 0, 6, 0, fmt_910, 0 };



/* ====================================================================== */

/*     calculation of ustar, u2, ra and drag using Paulson's method. */

/* ---------------------------------------------------------------------- */

/*     (1) site parameters derived from momopt program suite. */

/*     (2) routine is not suitable for gcm applications; designed for */
/*         use with forcing variables measured at a field site. */

/*     (3) paulson psi-coefficients are constrained under unsatble */
/*         conditions to prevent unrealistically low ra values. */

/*     (4) wind speed (um) must be greater than or equal to 0.1 m/s */

/* ---------------------------------------------------------------------- */

/*     variables that must enter through comsibc */

/*      tm     : air temperature at zmet */
/*      um     : wind speed at zwind, um .ge. 0.1 */
/*      ht     : sensible heat flux from surface */

/*     parameters that must enter through comsibc */

/*      z2     : height of canopy top */
/*      z0     : roughness length */
/*      xdx      : zero plane displacement */
/*      vkc    : von karmans constant = 0.41 */
/*      rhoair : air density */
/*      cpair  : air specific heat */

/*     other parameters */
/*     ---------------- */

/*      g1, g2, g3, ztz0, corb1, corb2, ha, zwind, zmet */

/*      g1     : ratio of km(actual) to km(log-linear) at z = z2 */
/*      g2     : ratio of ra(actual) to ra(log-linear) for momentum */
/*               between: z = z2 and z = zx, where zx = min(zl,zwind) */
/*      g3     : ratio of ra(actual) to ra(log-linear) for heat */
/*               between: z = z2 and z = zx, where zx = min(zl,zmet) */
/*      ztz0   : parameter to determine depth of transition layer above */
/*               canopy, zl. zl = z2 + ztz0 * z0 */
/*      corb1  : non-neutral correction for calculation of aerodynamic */
/*               resistance between ha and z2. when multiplied by */
/*               h*rbb/tm gives bulk estimate of local richardson number. */
/*               rbb = ra for heat between ha and z2. */
/*               corb2 = 9*g/( rhoair*cpair* (du/dz)**2 ) */
/*      corb2  : neutral value of rbb*u2 ( squared ), equivalent to */
/*               rdc**2 for upper canopy */
/*      ha     : canopy source height for heat */
/*      zwind  : reference height for wind measurement */
/*      zmet   : reference height for temperature, humidity measurement */

/*        the above are generated from sibx + momopt output */

/* ----------------------------------------------------------------------- */

/*     variables returned from this routine via comsibc */

/*      ustar  : friction velocity */
/*      u2     : wind speed at canopy top */
/*      ra     : aerodynamic resistance for heat flux between ha and zmet */
/*      drag   : shear stress at canopy top */

/* ----------------------------------------------------------------------- */

/*     references */
/*     ---------- */

/*         Paulson C.A. (1970) ' Mathematical representation of wind */
/*         and temperature profiles in the unstable atmospheric surface */
/*         layer', J. Appl. Met., 9, 129-861. */

/*         SE-89 */
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
    hress = caerod_1.ht;
    zl = vstate_1.z2 + caerod_1.ztz0 * rause_1.z0;
    uest = const_1.vkc * atmos_1.um / log((caerod_1.zwind - rause_1.xdx) / 
	    rause_1.z0);

/* ----------------------------------------------------------------------- */

/*     calculation of u2 assuming neutral conditions */

/* ----------------------------------------------------------------------- */

    if (caerod_1.zwind > zl) {
	goto L100;
    }
    top = 0.f;
    zx1 = caerod_1.zwind - rause_1.xdx;
    zx2 = vstate_1.z2 - rause_1.xdx;
    goto L200;
L100:
    zx1 = caerod_1.zwind - rause_1.xdx;
    zx2 = zl - rause_1.xdx;
    top = log(zx1 / zx2);
    zx1 = zl - rause_1.xdx;
    zx2 = vstate_1.z2 - rause_1.xdx;
L200:
    bot = log(zx1 / zx2);
    ram = 1.f / (const_1.vkc * uest) * (top + caerod_1.g2 * bot);
/* Computing 2nd power */
    r__1 = uest;
    grads_1.u2 = atmos_1.um - ram * (r__1 * r__1);

/* ----------------------------------------------------------------------- */

/*     calculation of ra for heat follows : non-neutrality assumed */

/* ----------------------------------------------------------------------- */

    zx1 = caerod_1.zwind - rause_1.xdx;
    zx2 = 0.f;
    arg1 = log(zx1 / rause_1.z0);

/* ----------------------------------------------------------------------- */
/*         initialize newton-raphson iterative routine */
/* ----------------------------------------------------------------------- */

    nox = 0;
    nonpos = 1;
    iwalk = 0;
    lx = 1;
    finc = .2f;

    if (caerod_1.ht <= 0.f) {
	goto L300;
    }

/* ----------------------------------------------------------------------- */

/*     unstable case : calculation of ustar followed by ra */

/* ----------------------------------------------------------------------- */

L1000:

    unstab_(&uest, &zx1, &zx2, &arg1, &caerod_1.ht, &ps1, &ps2);

    y = atmos_1.um - uest / const_1.vkc * (arg1 - ps1);

    newton_(&uest, &y, &finc, &nox, &nonpos, &iwalk, &lx);
    if (nox == 0) {
	goto L1000;
    }

    if (nox == 2) {
	s_wsfe(&io___299);
	e_wsfe();
    }

    rafcal_(&zl, &uest, &caerod_1.ht, &raf);

    goto L500;

/* ----------------------------------------------------------------------- */

/*      stable case : calculation of ustar */

/* ----------------------------------------------------------------------- */

L300:

/* ---------------------------------------------------------------------- */

/*      interpolation zone is defined: this spans negative sensible */
/*      heat fluxes from positive side ( hence factor 0.95 ) of the */
/*      following conditions: */
/*              y = 0,    dy/du* = 0. */
/*      see notes for details */

/* ---------------------------------------------------------------------- */

    gfac = log((caerod_1.zwind - rause_1.xdx) / rause_1.z0);
/* Computing 3rd power */
    r__1 = atmos_1.um * 2.f / 3.f;
/* Computing 2nd power */
    r__2 = const_1.vkc / gfac;
    hm1 = atmos_1.tm * -.95f * const_1.rhoair * const_1.cpair / (const_1.gx * 
	    9.4000000000000004f * (caerod_1.zwind - rause_1.xdx)) * (r__1 * (
	    r__1 * r__1)) * (r__2 * r__2);
    hm2 = hm1 * 5.f;
    us2 = const_1.vkc * atmos_1.um / (gfac + 4.7f);
    if (caerod_1.ht < hm2) {
	goto L310;
    }
    caerod_1.ht = dmax(hm1,caerod_1.ht);

/* ---------------------------------------------------------------------- */

/*      ustar calculated for slightly stable conditions : ht .ge. hm1 */

/* ---------------------------------------------------------------------- */

L2000:

    stab_(&uest, &zx1, &zx2, &caerod_1.ht, &ps1, &ps2);

    y = atmos_1.um - uest / const_1.vkc * (arg1 - ps1);

    newton_(&uest, &y, &finc, &nox, &nonpos, &iwalk, &lx);
    if (nox == 0) {
	goto L2000;
    }

    if (nox == 2) {
	s_wsfe(&io___305);
	e_wsfe();
    }

    caerod_1.ht = hress;

/* ----------------------------------------------------------------------- */
/*      ustar calculation in interpolation zone */
/* ----------------------------------------------------------------------- */

    if (caerod_1.ht > hm1) {
	goto L400;
    }
    us1 = uest;
    uest = (caerod_1.ht - hm2) / (hm1 - hm2) * (us1 - us2) + us2;
    goto L400;

/* ----------------------------------------------------------------------- */
/*      ustar calculation for collapsed profiles */
/* ----------------------------------------------------------------------- */

L310:
    uest = us2;

/* ----------------------------------------------------------------------- */

/*      calculation of ra for heat transfer between z2 and zmet */

/* ----------------------------------------------------------------------- */

L400:
    raf = 1e5f;

    rafcal_(&zl, &us2, &hm2, &rafmax);

    if (caerod_1.ht < hm2) {
	goto L410;
    }
    hss = dmax(hm1,caerod_1.ht);
    uss = dmax(us1,uest);

    rafcal_(&zl, &uss, &hss, &raf);

    if (caerod_1.ht > hm1) {
	goto L410;
    }
    raf1 = raf;
    raf = (caerod_1.ht - hm2) / (hm1 - hm2) * (raf1 - rafmax) + rafmax;

L410:
    raf = dmin(raf,rafmax);

/* ----------------------------------------------------------------------- */
/*     above canopy variables calculated. */
/* ----------------------------------------------------------------------- */

L500:
/* Computing 2nd power */
    r__1 = caerod_1.ht;
    hrb = (caerod_1.ht + sqrt(r__1 * r__1)) / 2.f + .1f;

/* ----------------------------------------------------------------------- */
/*     corb1 and corb2 are calculated for between ha and z2 only. */
/* ----------------------------------------------------------------------- */

    rbbest = sqrt(caerod_1.corb2) / grads_1.u2;

/* ----------------------------------------------------------------------- */
/*           initialize newton-raphson iterative routine */
/* ----------------------------------------------------------------------- */

    nox = 0;
    nonpos = 1;
    iwalk = 0;
    lx = 1;
    finc = .2f;

L3000:

    coef3 = caerod_1.corb1 * hrb / atmos_1.tm / (vstate_1.z2 - caerod_1.ha);

/* Computing 3rd power */
    r__1 = rbbest;
/* Computing 2nd power */
    r__2 = grads_1.u2 * rbbest;
    y = coef3 * (r__1 * (r__1 * r__1)) + r__2 * r__2 - caerod_1.corb2;

    newton_(&rbbest, &y, &finc, &nox, &nonpos, &iwalk, &lx);
    if (nox != 1) {
	goto L3000;
    }

    aerorx_1.ra = raf + rbbest;

    grads_1.ustar = uest;
    donor_1.drag = const_1.rhoair * uest * uest;

    return 0;
} /* rasite_ */


/* ======================================================================= */

/* Subroutine */ int unstab_(real *uest, real *a, real *b, real *argz, real *
	heat, real *psione, real *psitwo)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), log(doublereal), atan(
	    doublereal);

    /* Local variables */
    static integer i__;
    static real x[2], fac, zin, zml;


/* ======================================================================= */

/*      calculation of Paulson psi-function for unstable condition */

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

    zin = *a;

    for (i__ = 1; i__ <= 2; ++i__) {
/* Computing 3rd power */
	r__1 = *uest;
	zml = -(r__1 * (r__1 * r__1)) * const_1.rhoair * const_1.cpair * 
		atmos_1.tm;
	zml /= const_1.vkc * const_1.gx * *heat;
	fac = zin * 16.f / zml;
	d__1 = (doublereal) (1.f - fac);
	x[i__ - 1] = pow_dd(&d__1, &c_b101);
	zin = *b;
/* L1000: */
    }

/* Computing 2nd power */
    r__1 = x[0];
/* Computing 2nd power */
    r__2 = x[1];
    *psione = log((x[0] + 1.f) / (x[1] + 1.f)) * 2.f + log((r__1 * r__1 + 1.f)
	     / (r__2 * r__2 + 1.f)) - atan(x[0]) * 2.f + atan(x[1]) * 2.f;
/* Computing MIN */
    r__1 = *argz * .75f;
    *psione = dmin(r__1,*psione);

/* Computing 2nd power */
    r__1 = x[0];
/* Computing 2nd power */
    r__2 = x[1];
    *psitwo = log((r__1 * r__1 + 1.f) / (r__2 * r__2 + 1.f)) * 2.f;
/* Computing MIN */
    r__1 = *argz * .75f;
    *psitwo = dmin(r__1,*psitwo);

    return 0;
} /* unstab_ */


/* ======================================================================= */

/* Subroutine */ int stab_(real *uest, real *a, real *b, real *heat, real *
	psione, real *psitwo)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real zml;


/* ======================================================================= */

/*      calculation of Paulson psi-function for stable condition */

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
    *psione = 0.f;
    *psitwo = 0.f;
    if (dabs(*heat) <= 1e-4f) {
	goto L100;
    }

    d__1 = (doublereal) (*uest);
    zml = -pow_dd(&d__1, &c_b44) * const_1.rhoair * const_1.cpair * 
	    atmos_1.tm;
    zml /= const_1.vkc * const_1.gx * *heat;

    *psione = (*a - *b) * -4.7f / zml;
    *psione = dmax(-4.7f,*psione);

    *psitwo = *psione;

L100:

    return 0;
} /* stab_ */


/* ======================================================================= */

/* Subroutine */ int rafcal_(real *zl, real *uest, real *heat, real *raf)
{
    /* Builtin functions */
    double log(doublereal);

    /* Local variables */
    static real ps1, ps2, zx1, zx2, top, arg, bot;
    extern /* Subroutine */ int stab_(real *, real *, real *, real *, real *, 
	    real *), unstab_(real *, real *, real *, real *, real *, real *, 
	    real *);


/* ======================================================================= */

/*      calculation of ra for heat between z2 and zmet */

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
    if (caerod_1.zmet > *zl) {
	goto L100;
    }

    top = 0.f;
    zx1 = caerod_1.zmet - rause_1.xdx;
    zx2 = vstate_1.z2 - rause_1.xdx;
    goto L200;

L100:
    zx1 = caerod_1.zmet - rause_1.xdx;
    zx2 = *zl - rause_1.xdx;
    arg = log(zx1 / zx2);
    if (*heat > 0.f) {
	unstab_(uest, &zx1, &zx2, &arg, heat, &ps1, &ps2);
    }
    if (*heat <= 0.f) {
	stab_(uest, &zx1, &zx2, heat, &ps1, &ps2);
    }
    top = arg - ps2;

    zx1 = *zl - rause_1.xdx;
    zx2 = vstate_1.z2 - rause_1.xdx;

L200:
    arg = log(zx1 / zx2);
    if (*heat > 0.f) {
	unstab_(uest, &zx1, &zx2, &arg, heat, &ps1, &ps2);
    }
    if (*heat <= 0.f) {
	stab_(uest, &zx1, &zx2, heat, &ps1, &ps2);
    }
    bot = arg - ps2;

    *raf = 1.f / (const_1.vkc * *uest) * (top + caerod_1.g3 * bot);

    return 0;
} /* rafcal_ */


/* ======================================================================= */

/* Subroutine */ int newton_(real *a1, real *y, real *finc, integer *nox, 
	integer *nonpos, integer *iwolk, integer *l)
{
    /* Initialized data */

    static real cons = 1.f;

    /* Format strings */
    static char fmt_900[] = "(3x,\002 failure to converge after 490 iteratio"
	    "ns\002,/,3x,\002 y = \002,g12.5,\002 lx =\002,i2)";

    /* System generated locals */
    real r__1, r__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double r_sign(real *, real *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static real a, a2[3], y1[3];
    static integer nex[3], iter[3];
    static real zinc[3], step;
    static integer iwalk[3];
    static real ertol;

    /* Fortran I/O blocks */
    static cilist io___337 = { 0, 6, 0, fmt_900, 0 };



/* ======================================================================= */

/*      the newton raphson iterative routine will be used to generate new */
/*      values of a1 if dabsolute value of y is greater than ertol; */
/*      a1 is estimate, y is resultant error */
/*      nex is exit condition  (0=no exit) or (1 when dabs(y) lt ertol) */
/*      ertol is the dabsolute value of y necessary to obtain an exit */
/*      finc is initial increment size for second estimate of a1 */
/*      nonpos=0 if quantity to be minimized can be less than zero; */
/*      nonpos=1 if quantity can only be positive */
/*      l identifies which quantity is being calculated. */

/*      control values: finc,ertol,nox,nonpos,l:must be set by user */
/* ----------------------------------------------------------------------- */


    ertol = .05 * *finc;
    iwalk[*l - 1] = *iwolk;
    nex[*l - 1] = *nox;

    if (iter[*l - 1] >= 490) {
	goto L160;
    }
    if (ertol < 1e-6) {
	ertol = 1e-6f;
    }
    if (dabs(*y) <= ertol) {
	goto L150;
    }
    if ((r__1 = *y - y1[*l - 1], dabs(r__1)) <= ertol * .01f && iwalk[*l - 1] 
	    == 0) {
	goto L8;
    }

    if ((r__1 = y1[*l - 1], dabs(r__1)g) > ertol) {
	goto L1;
    }
    a2[*l - 1] = *a1;
/* **    a1=a1-y */
/* Computing MIN */
    d__2 = dabs(*y), d__3 = (d__1 = *finc * 10., abs(d__1));
    step = (real) min(d__2,d__3) * r_sign(&cons, y);
    *a1 -= step;
    nex[*l - 1] = 0;
    y1[*l - 1] = *y;
    iter[*l - 1] = 1;
    if (iwalk[*l - 1] == 3) {
	goto L101;
    }
    iwalk[*l - 1] = 0;
    goto L101;
L1:
    ++iter[*l - 1];
    if (iter[*l - 1] == 20) {
	iwalk[*l - 1] = 1;
    }
    if (iwalk[*l - 1] != 0) {
	goto L2;
    }
    if (dabs(*y) > ertol) {
	goto L3;
    }
    nex[*l - 1] = 1;
    goto L150;
L3:
    a = *a1 - *y * (*a1 - a2[*l - 1]) / (*y - y1[*l - 1]);
    if ((r__1 = a - *a1, dabs(r__1)) > *finc * 10.) {
	r__2 = a - *a1;
	a = *a1 + *finc * 10. * r_sign(&cons, &r__2);
    }
    a2[*l - 1] = *a1;
    *a1 = a;
    y1[*l - 1] = *y;
    goto L101;
L2:
    if (iwalk[*l - 1] == 2) {
	goto L4;
    }
    if (iwalk[*l - 1] == 3) {
	goto L6;
    }
    if (r_sign(&cons, y) == r_sign(&cons, &y1[*l - 1])) {
	goto L3;
    }
    zinc[*l - 1] = (*a1 - a2[*l - 1]) / 4.;
    *a1 = a2[*l - 1] + zinc[*l - 1];
    iwalk[*l - 1] = 2;
    nex[*l - 1] = 0;
    goto L101;
L4:
    if (r_sign(&cons, y) == r_sign(&cons, &y1[*l - 1])) {
	goto L5;
    }
    zinc[*l - 1] = -zinc[*l - 1] / 4.;
    a2[*l - 1] = *a1;
    *a1 += zinc[*l - 1];
    nex[*l - 1] = 0;
    y1[*l - 1] = *y;
    goto L101;
L5:
    a2[*l - 1] = *a1;
    *a1 += zinc[*l - 1];
    y1[*l - 1] = *y;
    nex[*l - 1] = 0;
    goto L101;
L6:
    if (r_sign(&cons, y) == r_sign(&cons, &y1[*l - 1])) {
	goto L7;
    }
    iwalk[*l - 1] = 1;
    goto L2;
L7:
    a2[*l - 1] = *a1;
    *a1 += *finc;
    y1[*l - 1] = *y;
    nex[*l - 1] = 0;
    goto L101;
L8:
    *a1 += *finc * 2.;
    nex[*l - 1] = 0;
    goto L101;
L160:
    s_wsfe(&io___337);
    do_fio(&c__1, (char *)&(*y), (ftnlen)sizeof(real));
    do_fio(&c__1, (char *)&(*l), (ftnlen)sizeof(integer));
    e_wsfe();

L150:
    nex[*l - 1] = 1;
    if (iter[*l - 1] >= 490) {
	nex[*l - 1] = 2;
    }
    zinc[*l - 1] = 0.f;
    iter[*l - 1] = 0;
    iwalk[*l - 1] = 0;
    y1[*l - 1] = 0.f;
    *y = 0.f;
    a2[*l - 1] = 0.f;
L101:
    if (*nonpos == 1 && *a1 < 0.0) {
	*a1 = a2[*l - 1] / 2.0;
    }
    *nox = nex[*l - 1];
    *iwolk = iwalk[*l - 1];

    return 0;
} /* newton_ */


/* ======================================================================= */

/* Subroutine */ int gauss_(real *a, integer *n, integer *np1, real *x, real *
	work)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, k, l;
    static real r__;


/* ======================================================================= */

/*     solve a linear system by gaussian elimination.  developed by */
/*     dr. chin-hoh moeng.  a is the matrix of coefficients, with the */
/*     vector of constants appended as an extra column.  x is the vector */
/*     containing the results.  the input matrix is not destroyed. */

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

    /* Parameter adjustments */
    work -= 5;
    --x;
    a -= 5;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *np1;
	for (j = 1; j <= i__2; ++j) {
/* L1000: */
	    work[i__ + (j << 2)] = a[i__ + (j << 2)];
	}
    }

    i__2 = *n;
    for (i__ = 2; i__ <= i__2; ++i__) {
	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {

	    r__ = work[j + (i__ - 1 << 2)] / work[i__ - 1 + (i__ - 1 << 2)];

	    i__3 = *np1;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
		work[j + (k << 2)] -= r__ * work[i__ - 1 + (k << 2)];
	    }
	}
    }

    i__3 = *n;
    for (i__ = 2; i__ <= i__3; ++i__) {
	k = *n - i__ + 2;
	r__ = work[k + (*np1 << 2)] / work[k + (k << 2)];

	i__1 = *n;
	for (j = i__; j <= i__1; ++j) {
	    l = *n - j + 1;
/* L30: */
	    work[l + (*np1 << 2)] -= r__ * work[l + (k << 2)];
	}
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L40: */
	x[i__] = work[i__ + (*np1 << 2)] / work[i__ + (i__ << 2)];
    }

    return 0;
} /* gauss_ */

