/* Sib2xb.F -- translated by f2c (version 20160102).
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

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__9 = 9;

/* ======================================================================= */

/* Subroutine */ int updat2_(void)
{
    /* System generated locals */
    real r__1, r__2;

    /* Local variables */
    static integer il;
    static real th, qm, facl;
    static integer iveg;
    extern /* Subroutine */ int run2n_(void), snow1_(void), snow2_(void);
    static real facks, ectil, rsnow, ectdif, egsdif, ectnew, extrak, extra2p[
	    10];


/* ======================================================================= */

/*     updating of all prognostic variables. */

/* ----------------------------------------------------------------------- */

/*     subroutines called   : updat2 */
/*     ------------------     snow2 */
/*                            run2n */

/* ++++++++++++++++++++++++++++++output from this block++++++++++++++++++++ */

/*       dtc            canopy temperature increment (K) */
/*       dtd            deep soil temperature increment (K) */
/*       dtg            ground surface temperature increment (K) */
/*       www(3)         ground wetness */
/*       capac(2)       canopy/ground liquid interception store (m) */
/*       snoww(2)       canopy/ground snow interception store (m) */
/*       roff           runoff (m) */
/*       etmass (fws)   evapotranspiration (mm) */
/*       hflux (fss)    sensible heat flux (w m-2) */

/* ++++++++++++++++++++++++++diagnostics++++++++++++++++++++++++++++++++++ */

/*       ecmass         canopy evapotranspiration (mm) */
/*       egmass         ground evapotranspiration (mm) */

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
/*       write(98,*)' updat2: begin' */
/*       write(98,*)' updat2: to call snow1' */
    snow1_();
/*       write(98,*)' updat2: snow1 passed' */


/* ---------------------------------------------------------------------- */
/*    interception losses. */
/*    evaporation losses are expressed in j m-2 : when divided by */
/*    ( hlat*1000.) loss is in m m-2. mass terms are in kg m-2 dt-1 */
/*    interception and drainage treated in inter2. */

/* ---------------------------------------------------------------------- */

/*       write(98,*)' updat2: interception losses begin' */
    rsnow = stepv_1.snoww[0] / (stepv_1.snoww[0] + stepv_1.capac[0] + 1e-10f);
    facks = rsnow * (const_1.snofac - 1.f) + 1.f;
    if (flux_1.ect + flux_1.eci > 0.f) {
	goto L100;
    }
    flux_1.eci = flux_1.ect + flux_1.eci;
    flux_1.ect = 0.f;
    facks = 1.f / facks;
L100:
    stepv_1.capac[0] -= (1.f - rsnow) * flux_1.eci * facks / const_1.hlat / 
	    1e3f;
    stepv_1.snoww[0] -= rsnow * flux_1.eci * facks / const_1.hlat / 1e3f;
    flux_1.ecmass = flux_1.eci * facks / const_1.hlat;

    rsnow = stepv_1.snoww[1] / (stepv_1.snoww[1] + stepv_1.capac[1] + 1e-10f);
    facks = rsnow * (const_1.snofac - 1.f) + 1.f;
    if (flux_1.egs + flux_1.egi > 0.f) {
	goto L200;
    }
    flux_1.egi = flux_1.egs + flux_1.egi;
    flux_1.egs = 0.f;
    facks = 1.f / facks;
L200:
    stepv_1.capac[1] -= (1.f - rsnow) * flux_1.egi * facks / const_1.hlat / 
	    1e3f;
    stepv_1.snoww[1] -= rsnow * flux_1.egi * facks / const_1.hlat / 1e3f;
    flux_1.egmass = flux_1.egi * facks / const_1.hlat;
/*       write(98,*)' updat2: interception losses passed' */

/* ---------------------------------------------------------------------- */
/*    dumping of small capac values onto soil surface store */
/* ---------------------------------------------------------------------- */

/*       write(98,*)' updat2: dump snow to w1 begin' */
    for (iveg = 1; iveg <= 2; ++iveg) {
	if (stepv_1.snoww[iveg - 1] + stepv_1.capac[iveg - 1] > 1e-5f) {
	    goto L300;
	}
/* new  www(1) = www(1) + (snoww(iveg)+capac(iveg)) / ( poros*zdepth(1) ) */
	stepv_1.www[0] += (stepv_1.snoww[iveg - 1] + stepv_1.capac[iveg - 1]) 
		/ (soils_1.poros[0] * soils_1.zdepth[0]);
	stepv_1.capac[iveg - 1] = 0.f;
	stepv_1.snoww[iveg - 1] = 0.f;
L300:
/* L1000: */
	;
    }
/*       write(98,*)' updat2: dump snow to w1 passed' */

/* ---------------------------------------------------------------------- */
/*    snowmelt / refreeze calculation */
/* ---------------------------------------------------------------------- */

/*       write(98,*)' updat2: call snow2  begin' */
    snow2_();
/*       write(98,*)' updat2: call snow2  passed' */

/* ---------------------------------------------------------------------- */
/*    evapotranspiration losses, */
/*    extraction of transpiration loss from root zone, soil evaporation. */

/*      ect         (e-dc)  : equation (5,6), SE-86 */
/*      egs         (e-s)   : equation (5)  , SE-86 */
/* ---------------------------------------------------------------------- */
/* h.. preferential multilayer extraction */
    ectnew = 0.f;
/*      write(98,'22f8.7') (www(i),i= 1, nlayer) */
    for (il = 1; il <= 10; ++il) {
	if (soils_1.extfrac[il - 1] > 0.f) {
	    facl = 1.f / const_1.hlat / 1e3f / (soils_1.poros[il - 1] * 
		    soils_1.zdepth[il - 1]);
	    if (il == 1) {
/*     write(98,*)' updat2: prefer extraction il=1' */
/* Computing MIN */
		r__1 = stepv_1.www[0], r__2 = flux_1.egs * soils_1.extfrac[0] 
			* facl;
		extrak = dmin(r__1,r__2);
		egsdif = flux_1.egs - extrak / facl;
		flux_1.egs = extrak / facl;
		flux_1.hg += egsdif;
		flux_1.egmass += flux_1.egs / const_1.hlat;
		stepv_1.www[0] -= flux_1.egs * facl;
	    } else {
/*     write(98,*)' updat2: prefer extraction il=',il */
		ectil = flux_1.ect * facl * soils_1.extfrac[il - 1];
/* Computing MIN */
		r__1 = stepv_1.www[il - 1];
		extrak = dmin(r__1,ectil);
		ectdif = (ectil - extrak) / facl;
		ectnew += extrak / facl;
		flux_1.hc += ectdif;
		flux_1.ecmass += extrak / facl / const_1.hlat;
		stepv_1.www[il - 1] -= extrak;
	    }
	}
/* HR..211108 */
	extra2p[il - 1] = extrak;
/* L40: */
    }
    flux_1.ect = ectnew;
/* HR..211108 */
/*      write(98,'2(11(1x,f8.7))')(www(il),il=1,nlayer), */
/*     &      (1.e+02*extra2p(il),il= 1, nlayer) */
/* h ... */
/* ----------------------------------------------------------------------- */
/*    calculation of total moisture and sensible heat fluxes from surface. */
/* ----------------------------------------------------------------------- */

    donor_1.etmass = flux_1.ecmass + flux_1.egmass;
    donor_1.hflux = (flux_1.hc + flux_1.hg) / steps_1.dtt;

/* ---------------------------------------------------------------------- */
/*    calculation of interflow, infiltration excess and loss to */
/*    groundwater .  all losses are assigned to variable 'roff' . */
/* ---------------------------------------------------------------------- */
    run2n_();

/* ---------------------------------------------------------------------- */

/*    update of temperatures and pbl variables. note that tc and tg */
/*    are modified in interc as a result of precipitation inputs. */
/*    update of interception stores. */

/* ---------------------------------------------------------------------- */

    stepv_1.tc += delts_1.dtc;
    stepv_1.tg += delts_1.dtg;
    stepv_1.td += delts_1.dtd;
    th += delts_1.dth;
    qm += delts_1.dqm;

    return 0;
} /* updat2_ */

/* ======================================================================= */

/* Subroutine */ int inter2_(void)
{
    /* Initialized data */

    static real pcoefs[4]	/* was [2][2] */ = { 20.f,1e-4f,2.06e-9f,
	    .9999f };
    static real bp = 20.f;

    /* System generated locals */
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), exp(doublereal), log(doublereal);

    /* Local variables */
    static real p0, aa, bb, ap, cp, ts, xs, arg, fpi, xsc, tti, xss, tex, 
	    chiv;
    static integer iveg;
    static real pinf, thru;
    extern /* Subroutine */ int snow1_(void);
    static real realc, shcap, realg, zload, roffo, capacp, slamda, spechc;
    extern /* Subroutine */ int patchs_(real *);
    static real equdep, totalp;
    extern /* Subroutine */ int adjust_(real *, real *, real *, real *, 
	    integer *);
    static real snowwp;


/* ======================================================================= */

/*     calculation of  interception and drainage of rainfall and snow */
/*     incorporating effects of patchy snow cover and temperature */
/*     adjustments. */

/* ---------------------------------------------------------------------- */

/*     (1) non-uniform precipitation */
/*         convective ppn. is described by area-intensity */
/*         relationship :- */

/*                   f(x) = a*exp(-b*x)+c */

/*         throughfall, interception and infiltration */
/*         excess are functional on this relationship */
/*         and proportion of large-scale ppn. */
/*         reference: sato et al.(1989b), appendix. */

/*     (2) reorganisation of snowmelt and rain-freeze procedures. */
/*               subroutine adjust */

/*     (3) additional calculation for partial snow-cover case. */
/*               subroutine patchs */

/*     (4) reorganisation of overland flow. */
/*         reference: SA-89B, appendix. */

/*     (5) modified calaculation of soil heat capacity and */
/*         conductivity. */

/* ======================================================================= */

/*     subroutines in this block : snow1 */
/*     -------------------------   adjust */
/*                                 patchs */
/*                                 snow1 */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       roff           runoff (m) */
/*       tc             canopy temperature (K) */
/*       tg             ground surface temperature (K) */
/*       www(1)         ground wetness of surface layer */
/*       capac(2)       canopy/ground liquid interception store (m) */
/*       snoww(2)       canopy/ground snow interception store (m) */

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

/* ----------------------------------------------------------------------- */

    snow1_();

/* ----------------------------------------------------------------------- */

/*     prec ( pi-x )   : equation (c.3), SA-89B */

/* ----------------------------------------------------------------------- */

    ap = pcoefs[1];
    cp = pcoefs[3];
    totalp = atmos_1.ppc + atmos_1.ppl;
    if (stepv_1.snoww[0] > 0.f || stepv_1.snoww[1] > 0.f || atmos_1.tm < 
	    const_1.tf) {
	atmos_1.ppc = 0.f;
    }
    atmos_1.ppl = totalp - atmos_1.ppc;
    if (totalp < 1e-8f) {
	goto L100;
    }
    ap = atmos_1.ppc / totalp * pcoefs[0] + atmos_1.ppl / totalp * pcoefs[1];
    cp = atmos_1.ppc / totalp * pcoefs[2] + atmos_1.ppl / totalp * pcoefs[3];
L100:

/* sml... */
    soimul_1.croff = 0.f;
/* surface runoff */
    soimul_1.cthru = 0.f;
/* sml... */
/* incoming thrufall to soil surface */
    donor_1.roff = 0.f;
    thru = 0.f;
    fpi = 0.f;

/* ---------------------------------------------------------------------- */
/*     heat capacity of the soil, as used in force-restore heat flux */
/*     description. dependence of csoil on porosity and wetness is */
/*     based on CS-81. */
/* ---------------------------------------------------------------------- */

/* sml...	porosity is taken of the layer 2 */
    slamda = ((1.f - soils_1.poros[1]) * 1.5f + stepv_1.www[0] * 1.3f * 
	    soils_1.poros[1]) / (soils_1.poros[1] * .65f + .75f - stepv_1.www[
	    0] * .4f * soils_1.poros[1]) * .4186f;
    shcap = ((1.f - soils_1.poros[1]) * .5f + stepv_1.www[0] * soils_1.poros[
	    1]) * 4.186f * 1e6f;
    stores_1.csoil = sqrt(slamda * shcap * 86400.f / const_1.pie) / 2.f;

/* ---------------------------------------------------------------------- */
/*     input precipitation is given in mm, converted to m to give p0. */
/* ---------------------------------------------------------------------- */

    p0 = totalp * .001f;

    for (iveg = 1; iveg <= 2; ++iveg) {

	realc = 2.f - iveg;
	realg = iveg - 1.f;

	capacp = stepv_1.capac[iveg - 1];
	snowwp = stepv_1.snoww[iveg - 1];

/* Computing MAX */
	r__1 = 0.f, r__2 = stepv_1.capac[iveg - 1] - hydrol_1.satcap[iveg - 1]
		;
	xsc = dmax(r__1,r__2);
	stepv_1.capac[iveg - 1] -= xsc;
/* Computing MAX */
	r__1 = 0.f, r__2 = stepv_1.snoww[iveg - 1] - hydrol_1.satcap[iveg - 1]
		;
	xss = dmax(r__1,r__2) * realc;
	stepv_1.snoww[iveg - 1] -= xss;
	donor_1.roff = donor_1.roff + xsc + xss;

/* Computing MIN */
	r__1 = .05f, r__2 = stepv_1.capac[iveg - 1] + stepv_1.snoww[iveg - 1];
	spechc = dmin(r__1,r__2) * const_1.cw + realc * vdyijt_1.zlt * 
		const_1.clai + realg * stores_1.csoil;
	ts = stepv_1.tc * realc + stepv_1.tg * realg;

/* ---------------------------------------------------------------------- */
/*     proportional saturated area (xs) and leaf drainage(tex) */

/*     tex ( d-c )     : equation (c.8), SA-89B */
/*     xs  ( x-s )     : equation (c.7), SA-89B */
/*     tex ( d-c )     : equation (c.8), SA-89B */

/* ----------------------------------------------------------------------- */

	chiv = vstate_1.chil;
	if (dabs(chiv) <= .01f) {
	    chiv = .01f;
	}
	aa = .5f - chiv * .633f - chiv * .33f * chiv;
	bb = (1.f - aa * 2.f) * .877f;
	radabs_1.exrain = aa + bb;

	zload = stepv_1.capac[iveg - 1] + stepv_1.snoww[iveg - 1];
	fpi = (1.f - exp(-radabs_1.exrain * vdyijt_1.zlt / vstate_1.vcover)) *
		 vstate_1.vcover * realc + realg;
	tti = p0 * (1.f - fpi);
	xs = 1.f;
	if (p0 < 1e-9f) {
	    goto L200;
	}
	arg = (hydrol_1.satcap[iveg - 1] - zload) / (p0 * fpi * ap) - cp / ap;
	if (arg < 1e-9f) {
	    goto L200;
	}
	xs = -1.f / bp * log(arg);
	xs = dmin(xs,1.f);
	xs = dmax(xs,0.f);
L200:
	tex = p0 * fpi * (ap / bp * (1.f - exp(-bp * xs)) + cp * xs) - (
		hydrol_1.satcap[iveg - 1] - zload) * xs;
	tex = dmax(tex,0.f);

/* ---------------------------------------------------------------------- */
/*     total throughfall (thru) and store augmentation */
/* ---------------------------------------------------------------------- */

	if (iveg == 2) {
	    goto L300;
	}

	thru = tti + tex;
	pinf = p0 - thru;
	if (atmos_1.tm > const_1.tf) {
	    stepv_1.capac[iveg - 1] += pinf;
	}
	if (atmos_1.tm <= const_1.tf) {
	    stepv_1.snoww[iveg - 1] += pinf;
	}

	adjust_(&stepv_1.tc, &spechc, &capacp, &snowwp, &iveg);

	p0 = thru;
	goto L700;

L300:

	if (stepv_1.tg > const_1.tf && stepv_1.snoww[1] > 0.f) {

/* ---------------------------------------------------------------------- */

	    patchs_(&p0);
	    goto L700;

/* ----------------------------------------------------------------------- */

	}

	thru = tti + tex;
	if (stepv_1.tg <= const_1.tf || atmos_1.tm <= const_1.tf) {
	    thru = 0.f;
	}
	pinf = p0 - thru;
	if (atmos_1.tm > const_1.tf) {
	    stepv_1.capac[iveg - 1] += pinf;
	}
	if (atmos_1.tm <= const_1.tf) {
	    stepv_1.snoww[iveg - 1] += pinf;
	}
	if (atmos_1.tm <= const_1.tf) {
	    goto L500;
	}

/* ---------------------------------------------------------------------- */

/*     instantaneous overland flow contribution ( roff ) */

/*     roff( r-i )     : equation (c.13), SA-89B */

/* ----------------------------------------------------------------------- */

	equdep = soils_1.satco[0] * steps_1.dtt;

	xs = 1.f;
	if (thru < 1e-9f) {
	    goto L400;
	}
	arg = equdep / (thru * ap) - cp / ap;
	if (arg < 1e-9f) {
	    goto L400;
	}
	xs = -1.f / bp * log(arg);
	xs = dmin(xs,1.f);
	xs = dmax(xs,0.f);
L400:
	roffo = thru * (ap / bp * (1.f - exp(-bp * xs)) + cp * xs) - equdep * 
		xs;
	roffo = dmax(roffo,0.f);
/* HR.. */
/*      write(98,'(20a10)') */
/*    & 'xs','P','D','P-D','Dc','Dd','Ri','R','Pi','q0' (no Main) */
/*      write(98,'(30(1x,f10.5))') */
/*     & xs,p0*1000.,thru*1000.,pinf*1000.,tex*1000., */
/*     & tti*1000.,roffo*1000.,roff*1000., */
/*     & amax1 (0., (1.0 - www(1))*zdepth(1)*poros(1) )*1000., */
/*     & q0*1000.*dtt, */
/*     & (www(i),i=1,nlayer) */
/* .................................................................... */
/* HR..Jan2008	infiltration/surface runoff options: iinf = */
/* .................................................................... */

/*  =1  capac. infiltracao = disponibilidade top layer */

/*  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente onda */
/*  =6  id. 2, frente de onda cessa encontro saturacao */

/*  =3  all thrufall infiltrates; infiltracao frente onda; */
/*  =5  id. 3, frente de onda cessa encontro saturacao */

/*  =4  all thrufall infiltrates; frente onda limitada R-Horton */
/* 	qstar (f* de EN-89) */

	if (soimul_1.iinf == 1) {
/* Computing MAX */
	    r__1 = 0.f, r__2 = (1.f - stepv_1.www[0]) * soils_1.zdepth[0] * 
		    soils_1.poros[0];
	    soimul_1.q0 = dmax(r__1,r__2);
/* Computing MIN */
	    r__1 = soimul_1.q0, r__2 = thru - roffo;
	    soimul_1.q0 = dmin(r__1,r__2) / steps_1.dtt;
/* m/s */
/* Computing MAX */
	    r__1 = 0.f, r__2 = thru - soimul_1.q0 * steps_1.dtt;
	    donor_1.roff += dmax(r__1,r__2);
	}
	if (soimul_1.iinf == 2 || soimul_1.iinf == 6) {
/* Computing MAX */
	    r__1 = 0.f, r__2 = thru - roffo;
	    soimul_1.q0 = dmax(r__1,r__2) / steps_1.dtt;
	    donor_1.roff += roffo;
	}
	if (soimul_1.iinf == 3 || soimul_1.iinf == 4 || soimul_1.iinf == 5) {
	    soimul_1.q0 = dmax(0.f,thru) / steps_1.dtt;
	}
	soimul_1.croff += donor_1.roff;
	soimul_1.cthru = thru;
/* ==================================================================== */
L500:

	adjust_(&stepv_1.tg, &spechc, &capacp, &snowwp, &iveg);

L700:

/* L1000: */
	;
    }

    return 0;
} /* inter2_ */

/* ======================================================================= */

/* Subroutine */ int begtem_(void)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double exp(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real rstfac2p;
    static integer i__;
    static real fac, argg, psit;
    extern /* Subroutine */ int snow1_(void);
    static real rsnow, tsnow, phroot;


/* ----------------------------------------------------------------------- */
/*     core routine: calculation of canopy and ground temperature */
/*     increments over time step, fluxes derived. */

/* ----------------------------------------------------------------------- */


/*     subroutinescalled : snow1 */
/*     ----------------- */

/* ++++++++++++++++++++++++++++++output+++++++++++++++++++++++++++++++++++ */

/*       rstfac(2)      soil moisture stress factor */
/*       rsoil          soil surface resistance (s m-1) */
/*       hr             soil surface relative humidity */
/*       wc             canopy wetness fraction */
/*       wg             ground wetness fraction */
/*       ccx            canopy heat capacity (j m-2 k-1) */
/*       cg             ground heat capacity (j m-2 k-1) */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* ---------------------------------------------------------------------- */

/* ---------------------------------------------------------------------- */
/*     e(x) is vapour pressure in mbars as a function of temperature */
/*     ge(x) is d e(x) / d ( temp ) */
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

    snow1_();

/* ---------------------------------------------------------------------- */

    const_1.hlat = (3150.19f - atmos_1.tm * 2.378f) * 1e3f;
    const_1.psy = const_1.cpair / const_1.hlat * atmos_1.psur / .622f;
    const_1.snofac = const_1.hlat / (const_1.hlat + const_1.snomel / 1e3f);

/* ---------------------------------------------------------------------- */
/*    calculation of canopy and ground heat capacities. */
/*    n.b. this specification does not necessarily conserve energy when */
/*    dealing with very large snowpacks. */
/* ---------------------------------------------------------------------- */

    stores_1.ccx = vdyijt_1.zlt * const_1.clai + (stepv_1.snoww[0] + 
	    stepv_1.capac[0]) * const_1.cw;
/* Computing MIN */
    r__1 = .05f, r__2 = stepv_1.snoww[1] + stepv_1.capac[1];
    stores_1.cg = stores_1.csoil + dmin(r__1,r__2) * const_1.cw;

/* ---------------------------------------------------------------------- */

/* ---------------------------------------------------------------------- */
/*      calculation of ground surface temperature and wetness fractions */

/* ---------------------------------------------------------------------- */

/* Computing MIN */
    r__1 = const_1.tf - .01f;
    tsnow = dmin(r__1,stepv_1.tg);
    rsnow = stepv_1.snoww[1] / (stepv_1.snoww[1] + stepv_1.capac[1] + 1e-10f);

    grads_1.tgs = tsnow * hydrol_1.areas + stepv_1.tg * (1.f - hydrol_1.areas)
	    ;

    grads_1.etc = exp(21.18123f - 5418.f / stepv_1.tc) / .622f;
    grads_1.etgs = exp(21.18123f - 5418.f / grads_1.tgs) / .622f;
    grads_1.getc = exp(21.18123f - 5418.f / stepv_1.tc) * 5418.f / (
	    stepv_1.tc * stepv_1.tc) / .622f;
    grads_1.getgs = exp(21.18123f - 5418.f / grads_1.tgs) * 5418.f / (
	    grads_1.tgs * grads_1.tgs) / .622f;

/* Computing MIN */
    r__1 = 1.f, r__2 = (stepv_1.capac[0] + stepv_1.snoww[0]) / 
	    hydrol_1.satcap[0];
    hydrol_1.wc = dmin(r__1,r__2);
/* Computing MAX */
    r__1 = 0.f, r__2 = stepv_1.capac[1] / hydrol_1.satcap[1];
    hydrol_1.wg = dmax(r__1,r__2) * .25f;

/* ----------------------------------------------------------------------- */
/*     calculation of soil moisture stress factor. */
/*     average soil moisture potential in root zone (layer-2) used as */
/*     source for transpiration. */

/*      phroot      (psi-r) : equation (47) , SE-86, (22), SE-89 */
/*      rstfac(2)  f(psi-l) :    "     (C17), SE-96 */
/* ----------------------------------------------------------------------- */

/* h      phroot = phsath * amax1( 0.02, www(2) ) ** ( - beeh ) */
/* h      phroot = amax1 ( phroot, -2.e3 ) */
/* h... */
/* new..equation 22 (SE-89) used as potential in root zone */
/* new    rdepth= 0. */
/* new    phroot= 0. */
/* new    do 40 i= 1, nlayer */
/* new    if (rdepth.gt.rootd) goto 40 */
/* new    phroot= phroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee)) */
/* new    rdepth= rdepth + zdepth(i) */
/* new 40 continue */
/* new    phroot = amax1 ( phroot*phsat, -2.e3 ) */
/* new .. */
/* tes..equation 22 (SE-89) used as potential in root zone */
/* 	wdepth= 0. */
/* 	whroot= 0. */
/* write(98,*)nymd */
/* 	do 40 i= 2, nlayer */
/* 	if (wdepth.gt.rootd) goto 40 */
/* new    whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee)) */
/*      whroot= whroot + zdepth(i)/rootd*((amax1(0.02,www(i)))**(-bee(i))) */
/* ml05_nov      whroot= whroot + extfrac(i)*((amax1(0.02,www(i)))**(-bee(i))) */
/*  	wdepth= wdepth + zdepth(i) */
/* 	if (nymd.eq.40101512.or.nymd.eq.40041512) then */
/* 	write (98,*) */
/*     &  ' i,zdepth(i),www(i),psi(i),whroot,wdepth' */
/*        write(*,*)   ' aqui nymd 40101512 ou 40041512' */
/*        write (98,*)i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)),whroot, */
/*     &	wdepth */
/* 	endif */
/* 40     continue */
/* 	whroot = amax1 ( whroot*phsat(3), -2.e3 ) */
/* 	phroot = whroot */
/* h... */
/*       write(98,'(a15,2(1x,e10.4))')' phroot, whroot:', phroot, whroot */
/* tes .. */
/*      rstfac(2) = 1./( 1 + exp( 0.02*( phc-phroot) ) ) */
/*      rstfac(2) = amax1( 0.0001, rstfac(2) ) */
/*      rstfac(2) = amin1( 1.,     rstfac(2) ) */
/* HR alteracao feita dia 06_nov */
    phroot = 0.f;
    rstfac2p = 0.f;
/* 	 write(98,'22f8.7') (www(i),i= 1, nlayer) */
    for (i__ = 2; i__ <= 10; ++i__) {
/* Computing MAX */
	r__1 = .02f, r__2 = stepv_1.www[i__ - 1];
	d__1 = (doublereal) dmax(r__1,r__2);
	d__2 = (doublereal) (-soils_1.bee[i__ - 1]);
	phroot = soils_1.phsat[i__ - 1] * pow_dd(&d__1, &d__2);
	phroot = dmax(phroot,-2e4f);
	rstfac2p += soils_1.extfrac[i__ - 1] * (1.f / (exp((vstate_1.phc - 
		phroot) * .02f) + 1));
/* 	if (nymd.eq.40101512.or.nymd.eq.40041512) then */
/* 	write (98,'a60') */
/*     &  ' i,zdepth(i),www(i),psi(i),phroot,rstfac2p,extfrac,fpsi' */
/*        write(*,*)   ' aqui nymd 40101512 ou 40041512' */
/*        write (98,'i3,7f7.4') */
/*     &  i,zdepth(i),www(i),phsat(i)/(www(i)**bee(i)), */
/*     &	phroot,rstfac2p, extfrac(i), (1./(1+exp(0.02*(phc-phroot)))) */
/* 	endif */
/* L40: */
    }
    surfrs_1.rstfac[1] = rstfac2p;
    surfrs_1.rstfac[1] = dmax(1e-4f,surfrs_1.rstfac[1]);
    surfrs_1.rstfac[1] = dmin(1.f,surfrs_1.rstfac[1]);
/*        write(98,'22f8.7') (www(i),i= 1, nlayer) */
/* 	if (nymd.eq.40101512.or.nymd.eq.40041512) */
/*     &	write (98,*)rstfac(2),phroot */
/* ---------------------------------------------------------------------- */

/*      rsoil function from fit to FIFE-87 data.  soil surface layer */
/*      relative humidity. */

/*      rsoil      (rsoil) : Heiser 1992 (personal communication) */
/*      hr         (fh)    : equation (66) , SE-86 */

/* ---------------------------------------------------------------------- */

    fac = dmin(stepv_1.www[0],1.f);
    fac = dmax(fac,.02f);
/*      rsoil =  amax1 (0.1, 694. - fac*1500.) + 23.6 */
/* Computing MAX */
    r__1 = .1f, r__2 = 1001.f - exp(fac * 6.686f);
    surfrs_1.rsoil = dmax(r__1,r__2);

/* Cinthia_cerrado16Out */
    d__1 = (doublereal) fac;
    d__2 = (doublereal) (-soils_1.bee[0]);
    psit = soils_1.phsat[0] * pow_dd(&d__1, &d__2);
/* Computing MAX */
    r__1 = -10.f, r__2 = psit * const_1.gx / 461.5f / grads_1.tgs;
    argg = dmax(r__1,r__2);
    surfrs_1.hr = exp(argg);

    return 0;
} /* begtem_ */


/* ==================================================================== */

/* Subroutine */ int run2n_(void)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real a[10], b[10], c__[10], d__[10];
    static integer i__;
    static real da[10], db[10], dc[10], cs, d10, xni, avk, temw[10], dpdw, 
	    qmin, qmax, pows, q0now, avdif;
    extern /* Subroutine */ int retec_(real *, integer *, integer *), eqlin_(
	    integer *, integer *, integer *, real *, real *, real *, real *);
    static real temwp[10], zpond, qstar, wover;
    static logical wavend;
    extern /* Subroutine */ int hydcon_(real *, integer *);
    static real qdowrd[10], dpsidz, excess, temwpp[10], qupwrd[10];
    extern /* Subroutine */ int tridia2_(integer *, real *, real *, real *, 
	    real *);
    static real deficit, qhorton[10];


/*     modified multi-layer scheme:  Humberto da Rocha 03/04/1996 */

/* ==================================================================== */

/* HR..Dez2008	infiltration/surface runoff options: iinf = */
/* .................................................................... */

/*  =1  capac. infiltracao = disponibilidade top layer */

/*  =2  thrufall reduced by overland flow Ri(SA-89); infiltracao frente onda */
/*  =6  id. 2, frente de onda cessa encontro saturacao */

/*  =3  all thrufall infiltrates; frente onda; */
/*  =5  id. 3, frente de onda cessa encontro saturacao */

/*  =4  all thrufall infiltrates; frente onda limitada R-Horton */
/* 	qstar (f* de EN-89) */

/* ALL (see routine inter2 for q0 calculation) */
/* .................................................................... */

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
/* HR */
    for (i__ = 1; i__ <= 10; ++i__) {
/* Computing MAX */
	r__1 = .03f, r__2 = stepv_1.www[i__ - 1];
	temw[i__ - 1] = dmax(r__1,r__2);
	d__1 = (doublereal) temw[i__ - 1];
	d__2 = (doublereal) (-soils_1.bee[i__ - 1]);
	temwp[i__ - 1] = pow_dd(&d__1, &d__2);
/* Computing MIN */
	r__1 = 1.f, r__2 = temw[i__ - 1];
	d__1 = (doublereal) dmin(r__1,r__2);
	d__2 = (doublereal) (soils_1.bee[i__ - 1] * 2.f + 3.f);
	temwpp[i__ - 1] = pow_dd(&d__1, &d__2);
/* H */
	qdowrd[i__ - 1] = 0.f;
/* fluxos downward (drenagem rapida + difusiva), i= */
	qupwrd[i__ - 1] = 0.f;
/* fluxos upward (capilar) */
	qhorton[i__ - 1] = 0.f;
/* infiltração maxima Hortoniana EN-89 */
/* L1000: */
    }
/* ----------------------------------------------------------------------- */
/* 	iinf  = 1 */
/* ----------------------------------------------------------------------- */
    if (soimul_1.iinf == 1) {
	stepv_1.www[0] += soimul_1.q0 * steps_1.dtt / (soils_1.poros[0] * 
		soils_1.zdepth[0]);
	soimul_1.q0 = 0.f;
	qdowrd[0] += soimul_1.q0;
/* infiltração superficie (m/s) */
    }
/* ----------------------------------------------------------------------- */
/* 	iinf  > 1 */
/* ----------------------------------------------------------------------- */
    if (soimul_1.iinf > 1) {
	q0now = soimul_1.q0;
	wavend = FALSE_;
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
	if (q0now > 0.f) {
/* ............................................................................. */
/* 	capacidade infiltração da camada i */
/* ............................................................................. */
/* throughfall event */
	    while(! wavend) {
		for (i__ = 1; i__ <= 10; ++i__) {
/* 	write(*,*) ' do while (.not.wavend), E wavend = ', wavend */
/* 	pause */
/* 	jdpsi = 2 */
/* 	call retec ( jdpsi, dpdw, i) */
/* 	call hydcon (avk, i,temwp(i),temwp(i+1),temwpp(i),temwpp(i+1)) */
/* 	avk = sqrt( satco(i)*satco(i+1) ) */
/* 	qstar = - 2.*avk*(dpdw*(1.-www(i+1))/(zdepth(i)+zdepth(i+1))-1.) */
/* Computing MAX */
		    r__1 = 0.f, r__2 = (1.f - stepv_1.www[i__ - 1]) * 
			    soils_1.zdepth[i__ - 1] * soils_1.poros[i__ - 1];
		    zpond = dmax(r__1,r__2);
		    retec_(&dpdw, &c__1, &i__);
/* dpsi/dw at saturation threshold p/ capac infiltracao */
		    xni = dpdw / soils_1.zdepth[i__ - 1];
		    qstar = soils_1.satco[i__ - 1] * (xni * stepv_1.www[i__ - 
			    1] + 1.f - xni);
/* infiltração Horton */
		    qstar = dmin(qstar,0.f);
/* only negative flux is downward eq(13) EN-89 */
		    qstar = dabs(qstar);
/* physical drainage */
		    if (soimul_1.iinf != 4) {
/* Computing MAX */
			r__1 = cs * qstar;
			qstar = dmax(r__1,1e20f);
		    }
/* infiltração ilimitada,  sem Runoff-Horton (iinf=2,6,3,5) */
		    soimul_1.q0 = q0now;
/* oferta sempre>0 */
/* Computing MIN */
		    r__1 = q0now, r__2 = zpond / steps_1.dtt, r__1 = min(r__1,
			    r__2);
		    q0now = dmin(r__1,qstar);
/* infiltrado >=0 */
		    qdowrd[i__ - 1] += q0now;
/* diagnostico m/s */
		    qhorton[i__ - 1] = qstar;
/* diagnostico m/s */
		    if (q0now == 0.f) {
/* há oferta (mas não infiltra em alguns casos iinf) */
			if (soimul_1.iinf == 4 || soimul_1.iinf == 5 || 
				soimul_1.iinf == 6) {
			    wavend = TRUE_;
			}
		    }
		    stepv_1.www[i__ - 1] += q0now * steps_1.dtt / (
			    soils_1.zdepth[i__ - 1] * soils_1.poros[i__ - 1]);
/* absorvido (ou não, se infiltrado=0) */
/* Computing MAX */
		    r__1 = 0.f, r__2 = stepv_1.www[i__ - 1] - 1.f;
		    wover = dmax(r__1,r__2);
/* check arredon/to */
		    stepv_1.www[i__ - 1] -= wover;
/* .. */
		    donor_1.roff += wover * soils_1.poros[i__ - 1] * 
			    soils_1.zdepth[i__ - 1];
/* .. */
/* Computing MAX */
		    r__1 = 0.f, r__2 = soimul_1.q0 - q0now;
		    q0now = dmax(r__1,r__2);
/* atualiza oferta de infiltração */
		    if (q0now == 0.f || q0now > 0.f && i__ == 10) {
/* oferta cessou */
/* onda atingiu camada pro */
			wavend = TRUE_;
		    }
/* 	write(*,*) ' i layer,  q0now =' , i, q0now */
/* 	qexcess =  amax1 (0., ( q0now -  amin1 ( q0now, qstar ) ) ) */
/* 	roff = roff + qexcess*dtt */
/* 	croff = croff + qexcess*dtt */
/* L75: */
		}
	    }
/*  wavend = F */
	}
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* throughfall event */
	soimul_1.croff += q0now * steps_1.dtt;
/* ...excess to surface runoff */
	donor_1.roff += q0now * steps_1.dtt;
/* ----------------------------------------------------------------------- */
    }
/* ---------------------------------------------------------------------- */

/*    calculation of inter-layer exchanges of water due to gravitation */
/*    and matric gradient. the values of w(x) + dw(x) are used to calculate */
/*    the potential gradients between layers. */
/*    modified calculation of mean conductivies follows me-82, which */
/*    reduces recharge flow to top layer */

/*    dpdw : estimated derivative of soil moisture potential with respect */
/*           to soil wetness. assumption of gravitational drainage used */
/*           to estimate likely minimum wetness over the time step. */

/*     qqq (q    )  equation (61) , se 86     i = 1 , nlayer   (m/s) */
/*             I , I + 1 */

/*      avk  (k)     equation (4.14), me 82 */
/*             I , I + 1 */

/* ----------------------------------------------------------------------- */
/*  option iinf > 1 */
    for (i__ = 1; i__ <= 10; ++i__) {
/* Computing MAX */
	r__1 = .03f, r__2 = stepv_1.www[i__ - 1];
	temw[i__ - 1] = dmax(r__1,r__2);
/* W */
	d__1 = (doublereal) temw[i__ - 1];
	d__2 = (doublereal) (-soils_1.bee[i__ - 1]);
	temwp[i__ - 1] = pow_dd(&d__1, &d__2);
/* Psi */
/* Computing MIN */
	r__1 = 1.f, r__2 = temw[i__ - 1];
	d__1 = (doublereal) dmin(r__1,r__2);
	d__2 = (doublereal) (soils_1.bee[i__ - 1] * 2.f + 3.f);
	temwpp[i__ - 1] = pow_dd(&d__1, &d__2);
/* L49: */
    }
/* ------------------------------------------------------------------- */
/* 	 Flow between soil layers, diffusion equation in multi-layer scheme */
/*         backward implicit calculation, or Crank-Nicholson  method */
/*         linear system of (nlayer-1 x nlayer -1 ) dimension */
/*         tridiagonal matrix solution - Thomas algorithm */

/* 	b = diagonal principal */
/* 	c = superior diagonal principal */
/* 	a = sub (inferior) diagonal principal */
/* 	d = coeficientes independentes (direita igualdade) */
/* 				HRocha 15.12.96, atualizado 07.01.09 */
/* ------------------------------------------------------------------- */
/* ..	esquema discretização numérica ................................ */
/* 	jesq = 1		! backward */
/* 	jesq = 2		! Crank-Nicolson */
/* 	jesq = 3		! desativa difusão de agua */
/* ..	método calculo dpsi/dteta ..................................... */
/* 	jdpsi = 1		!  SiB2 original */
/* 	jdpsi = 2		!  derivada CH-78 media ponderada espessura */
/*       jdpsi = 3		!  derivada CH-78 media geometrica */
/*       jdpsi = 4		!  derivada CH-78 media aritmetica */
/* ..	método calculo condutividade hidraulica ....................... */
/* 	jkcon = 1		! SiB2 original ME-82 */
/* 	jkcon = 2		! media ponderada espessura */
/*       jkcon = 3		! media geometrica */
/*       jkcon = 4		! media aritmetica */
/* ...	método solução sistema eqs lineares */
/* 	jsys=	1    matriz tridiagonal (HR-1996, revisado-2009) */
/* 		3    método iterativo */
/* 			3-definir jjgs = 1   método Jacobi */
/* 			3-definir jjgs = 2   método Gauss-Seidel */
/* ...................................................................... */
    for (i__ = 1; i__ <= 9; ++i__) {
/* ...	calculo dpsi/dw  ...................................... */
	retec_(&dpdw, &c__0, &i__);
/* ...	calculo condutividade e difusividade hidraulica ....................... */
	hydcon_(&avk, &i__);
	avdif = avk * dpdw / soils_1.poros[i__ - 1];
/* ...	valor inicial de qqq(i) */
	if (soimul_1.jqini == 0) {
	    soimul_1.qqq[i__ - 1] = 0.f;
	}
	if (soimul_1.jqini == 1) {
	    dpsidz = soils_1.phsat[i__ - 1] * temwp[i__ - 1];
	    dpsidz -= soils_1.phsat[i__] * temwp[i__];
	    dpsidz = dpsidz * 2.f / (soils_1.zdepth[i__ - 1] + soils_1.zdepth[
		    i__]);
	    soimul_1.qqq[i__ - 1] = -avk * (dpsidz + 1.f);
	}
	soimul_1.jqini = -999.f;
/* ... 	matriz multi-camada: esquema backward */
	if (soimul_1.jesq == 1) {
	    a[i__ - 1] = steps_1.dtt * -2.f * avdif / (soils_1.zdepth[i__ - 1]
		     * (soils_1.zdepth[i__ - 1] + soils_1.zdepth[i__]));
	    b[i__ - 1] = steps_1.dtt * 2.f * avdif / (soils_1.zdepth[i__ - 1] 
		    * soils_1.zdepth[i__]) + 1.f;
	    c__[i__ - 1] = steps_1.dtt * -2.f * avdif / (soils_1.zdepth[i__] *
		     (soils_1.zdepth[i__ - 1] + soils_1.zdepth[i__]));
	    d__[i__ - 1] = soimul_1.qqq[i__ - 1];
	    if (i__ == 1) {
		a[i__ - 1] = 0.f;
	    }
	    if (i__ == 9) {
		c__[i__ - 1] = 0.f;
	    }
/*        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))') */
/*     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif', */
/*     &   'log(avdif1)','log(avdif2)','log(avdif' */
/*        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))') */
/*     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,  ! x 3.6e5 m/s p/ cm2/h */
/*     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,), */
/*     &    log(avdif* 3.6e5) */
	}
/* ... 	matriz multi-camada: esquema Crank-Nicolson */
	if (soimul_1.jesq == 2) {
	    a[i__ - 1] = -steps_1.dtt * avdif / (soils_1.zdepth[i__ - 1] * (
		    soils_1.zdepth[i__ - 1] + soils_1.zdepth[i__]));
	    b[i__ - 1] = steps_1.dtt * avdif / (soils_1.zdepth[i__ - 1] * 
		    soils_1.zdepth[i__]) + 1.f;
	    c__[i__ - 1] = -steps_1.dtt * avdif / (soils_1.zdepth[i__] * (
		    soils_1.zdepth[i__ - 1] + soils_1.zdepth[i__]));
	    if (i__ == 1) {
		a[i__ - 1] = 0.f;
	    }
	    if (i__ == 9) {
		c__[i__ - 1] = 0.f;
	    }
	    da[i__ - 1] = -a[i__ - 1];
	    db[i__ - 1] = 1.f - steps_1.dtt * avdif / (soils_1.zdepth[i__ - 1]
		     * soils_1.zdepth[i__]);
	    dc[i__ - 1] = -c__[i__ - 1];
	    if (i__ >= 2 && i__ <= 8) {
		d__[i__ - 1] = da[i__ - 1] * soimul_1.qqq[i__ - 2] + db[i__ - 
			1] * soimul_1.qqq[i__ - 1] + dc[i__ - 1] * 
			soimul_1.qqq[i__];
	    }
	    if (i__ == 1) {
		d__[i__ - 1] = db[i__ - 1] * soimul_1.qqq[i__ - 1] + dc[i__ - 
			1] * soimul_1.qqq[i__];
	    }
	    if (i__ == 9) {
		d__[i__ - 1] = da[i__ - 1] * soimul_1.qqq[i__ - 2] + db[i__ - 
			1] * soimul_1.qqq[i__ - 1];
	    }
/*        if (i.eq.1) write(itmp5,'(a8,a3,1x, a5, 5(4x,a9),5(4x,a8))') */
/*     &   'nymd','i','www(i)','avk','dpdw','avdif1','avdif2','avdif', */
/*     &   'log(avdif1)','log(avdif2)','log(avdif' */
/*        write(itmp5,'(i8.8,i3,1x, f5.3, 5(4x,e9.3),5(4x,f8.3))') */
/*     &   nymd, i, www(i), avk, dpdw, avdif1*3.6e5, avdif2*3.6e5,	! x 3.6e5 m/s p/ cm2/h */
/*     &    avdif*3.6e5,log(avdif1*3.6e5,), log(avdif2*3.6e5,), */
/*     &    log(avdif* 3.6e5) */
	}
/* .. */
/* HHH 	qng incrementado em W no final apenas */
/* HH	if (i.eq.nlayer-1) d(i) = d(i) + qng */
/* HHH */
/* H       if(i.eq.1) then */
/* H       write(itmp5,*) ' Run2n: sistema eqs montado ' */
/* H       write(itmp5,'(a8,50(2x,a9,2x))') */
/* H     &    'i','a(i)','b(i)','c(i)','d(i)','d(i)_mm/d' */
/* H       endif */
/* H       write(itmp5,'(i8.8,50(4x,e9.3))') i, a(i),b(i),c(i),d(i), */
/* H     &                                   d(i)*1000.*dtt*24. */
/* L50: */
    }
/* ..	solucao sistema eqs lineares */
    if (soimul_1.jsys == 1) {
	tridia2_(&c__9, d__, c__, a, b);
    }
    if (soimul_1.jsys == 3) {
	eqlin_(&outfile_1.itmp5, &soimul_1.jjgs, &c__9, d__, c__, a, b);
    }
/*       write(itmp5,'(/,i8.8,50(1x,e9.3),/)') */
/*     &   nymd, (d(i),i=1,nlayer-1) */
/* H	write(itmp5,*) */
/* --------------------------------------------------------------------- */
/*   update wetness of soil layers due to layer interflow and base flow */
/* --------------------------------------------------------------------- */
    for (i__ = 1; i__ <= 9; ++i__) {
	soimul_1.qqq[i__ - 1] = d__[i__ - 1];
	if (soimul_1.jesq == 3) {
	    soimul_1.qqq[i__ - 1] = 0.f;
	}
	if (soimul_1.qqq[i__ - 1] < 0.f) {
	    qdowrd[i__] += (r__1 = soimul_1.qqq[i__ - 1], dabs(r__1));
	}
	if (soimul_1.qqq[i__ - 1] > 0.f) {
	    qupwrd[i__] += soimul_1.qqq[i__ - 1];
	}
	qmin = -stepv_1.www[i__ - 1] * (soils_1.poros[i__ - 1] * 
		soils_1.zdepth[i__ - 1] / steps_1.dtt);
	qmax = stepv_1.www[i__] * (soils_1.poros[i__] * soils_1.zdepth[i__] / 
		steps_1.dtt);
/* Computing MIN */
	r__1 = soimul_1.qqq[i__ - 1];
	soimul_1.qqq[i__ - 1] = dmin(r__1,qmax);
/* Computing MAX */
	r__1 = soimul_1.qqq[i__ - 1];
	soimul_1.qqq[i__ - 1] = dmax(r__1,qmin);
	stepv_1.www[i__ - 1] += soimul_1.qqq[i__ - 1] / (soils_1.poros[i__ - 
		1] * soils_1.zdepth[i__ - 1] / steps_1.dtt);
	stepv_1.www[i__] -= soimul_1.qqq[i__ - 1] / (soils_1.poros[i__] * 
		soils_1.zdepth[i__] / steps_1.dtt);
/* ...	qqq(i): fluxo base camada i (+downward) = */
/* 	qdowrd,qupwrd(i+1): fluxo topo camada i+1 */
/* 	if (qqq(i).gt.0.) qdowrd(i+1) = qdowrd(i+1) + qqq(i) */
/* 	if (qqq(i).lt.0.) qupwrd(i+1) = qupwrd(i+1) + qqq(i) */
/* L3000: */
    }
/* ---------------------------------------------------------------------- */
/*      calculation of gravitationally driven drainage (qng) from */
/*      w(n): taken as an integral of time varying conductivity; */
/*      addition of liston baseflow term to original qng to insure */
/*      flow in dry season (modified liston baseflow constant */
/*      scaled by available water) */

/*     QNG: equation of q3g (SE 86) */

/* ----------------------------------------------------------------------- */

    pows = soils_1.bee[9] * 2.f + 2.f;
    d__1 = (doublereal) temw[9];
    d__2 = (doublereal) (-pows);
    soimul_1.qng = pow_dd(&d__1, &d__2) + soils_1.satco[9] / soils_1.zdepth[9]
	     / soils_1.poros[9] * soils_1.slope * pows * steps_1.dtt;
    d__1 = (doublereal) soimul_1.qng;
    d__2 = (doublereal) (1.f / pows);
    soimul_1.qng = pow_dd(&d__1, &d__2);
    soimul_1.qng = -(1.f / soimul_1.qng - stepv_1.www[9]) * soils_1.poros[9] *
	     soils_1.zdepth[9] / steps_1.dtt;
/* HRDez2008... ajuste coeficiente de Liston */
/* _original_1994      qng = qng + 0.002 * poros(nlayer)*zdepth(nlayer) * 0.5 / 86400. */
/*     &      * www (nlayer) */
/* HR...  kb = escoamento de base se W3=1 ( em m/d) */
    soimul_1.qng += soimul_1.xkb / 1e3f * soils_1.poros[9] * soils_1.zdepth[9]
	     / 86400.f * stepv_1.www[9];
    soimul_1.qng = dmax(0.f,soimul_1.qng);
/* Computing MIN */
    r__1 = soimul_1.qng, r__2 = stepv_1.www[9] * soils_1.poros[9] * 
	    soils_1.zdepth[9] / steps_1.dtt;
    soimul_1.qng = dmin(r__1,r__2);
/* H...	teste de difusao interna */
/* 	if (jqng.eq.0) qng = 0. */
    stepv_1.www[9] -= soimul_1.qng * steps_1.dtt / (soils_1.poros[9] * 
	    soils_1.zdepth[9]);
    donor_1.roff += soimul_1.qng * steps_1.dtt;
    for (i__ = 1; i__ <= 10; ++i__) {
/* arredondamento ... */
/* Computing MAX */
	r__1 = 0.f, r__2 = stepv_1.www[i__ - 1] - 1.f;
	excess = dmax(r__1,r__2);
	stepv_1.www[i__ - 1] -= excess;
	donor_1.roff += excess * soils_1.poros[i__ - 1] * soils_1.zdepth[i__ 
		- 1];
/* L401: */
    }
/* ----------------------------------------------------------------- */
/*       prevent negative values of soil wetness */
/* ----------------------------------------------------------------- */
    for (i__ = 1; i__ <= 9; ++i__) {
/* Computing MAX */
	r__1 = 0.f, r__2 = 1e-12f - stepv_1.www[i__ - 1];
	deficit = dmax(r__1,r__2);
	stepv_1.www[i__ - 1] += deficit;
	stepv_1.www[i__] -= deficit * soils_1.zdepth[i__ - 1] / 
		soils_1.zdepth[i__];
/* L402: */
    }
    stepv_1.www[9] = dmax(stepv_1.www[9],1e-12f);
/* ... diagnostico escoamento de agua do solo */
    d10 = steps_1.dtt * 1e3f;
/* H      write(itmp3,'(i8.8,50(1x,f10.3))')nymd,(qupwrd (i)*d10,i=1,nlayer) */
/* H      write(itmp4,'(i8.8,50(1x,f10.5))')nymd,(qdowrd (i)*d10,i=1,nlayer) */
/*      write(itmp5,'(i8.8,50(1x,e9.3))')nymd,(qhorton(i)*d10,i=1,nlayer) */
    return 0;
} /* run2n_ */

/* ======================================================================= */
/* Subroutine */ int retec_(real *dpdw, integer *jhort, integer *i__)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static integer j;
    static real w0, w1, w2, psi1, psi2, wmax, pmax, wmin, pmin, dpdw1, dpdw2;

/* ======================================================================= */

/* ..	output: cdpdw (derivada curva retencao) dpsi / dw */

/* ---------------------------------------------------------------------- */
/* ... */
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
    j = *i__ + 1;
    if (*i__ > 1) {
/* Computing MIN */
	r__1 = stepv_1.www[*i__ - 2];
	w0 = dmin(r__1,1.f);
/* Computing MAX */
	r__1 = stepv_1.www[*i__ - 2];
	w0 = dmax(r__1,.05f);
    }
/* Computing MIN */
    r__1 = stepv_1.www[*i__ - 1];
    w1 = dmin(r__1,1.f);
/* Computing MAX */
    r__1 = stepv_1.www[*i__ - 1];
    w1 = dmax(r__1,.05f);
/* Computing MIN */
    r__1 = stepv_1.www[j - 1];
    w2 = dmin(r__1,1.f);
/* Computing MAX */
    r__1 = stepv_1.www[j - 1];
    w2 = dmax(r__1,.05f);
/* ...	jhorton = 1  (calcula proximo saturacao; Runoff Horton) */
    if (*jhort == 1) {
	w1 = 1.f;
	w2 = .95f;
    }
    d__1 = (doublereal) w1;
    d__2 = (doublereal) (-soils_1.bee[*i__ - 1]);
    psi1 = soils_1.phsat[*i__ - 1] * pow_dd(&d__1, &d__2);
    d__1 = (doublereal) w2;
    d__2 = (doublereal) (-soils_1.bee[j - 1]);
    psi2 = soils_1.phsat[j - 1] * pow_dd(&d__1, &d__2);
/* ... calculo método original SiB2 SE-96 */
    if (soimul_1.jdpsi == 1) {
/* Computing MAX */
	r__1 = stepv_1.www[*i__ - 1], r__2 = stepv_1.www[*i__];
	wmax = dmax(r__1,r__2);
/* Computing MAX */
	r__1 = dmin(wmax,1.f);
	wmax = dmax(r__1,.05f);
	d__1 = (doublereal) wmax;
	d__2 = (doublereal) (-soils_1.bee[*i__ - 1]);
	pmax = pow_dd(&d__1, &d__2);
	if (*i__ == 1) {
	    wmin = pmax - 2.f / (soils_1.phsat[0] * (soils_1.zdepth[0] + 
		    soils_1.zdepth[1] * 2.f + soils_1.zdepth[2]));
/* h    wmin= (pmax-2./(phsat(1)*(2.*zdepth(1)+zdepth(2)))) */
/* Computing MIN */
	    r__1 = min(w1,w2);
	    wmin = dmin(r__1,wmin);
	    wmin = dmax(wmin,.01f);
	}
	if (*i__ > 1) {
	    wmin = pmax - 2.f / (soils_1.phsat[*i__ - 1] * (soils_1.zdepth[*
		    i__ - 2] + soils_1.zdepth[*i__ - 1] * 2.f + 
		    soils_1.zdepth[*i__]));
/* Computing MIN */
	    r__1 = min(w0,w1), r__1 = min(r__1,w2);
	    wmin = dmin(r__1,wmin);
	    wmin = dmax(wmin,.01f);
	}
	if (wmin == wmax) {
	    wmin = pmax - 1.f / (soils_1.phsat[*i__ - 1] * (soils_1.zdepth[*
		    i__ - 2] + soils_1.zdepth[*i__ - 1] * 2.f + 
		    soils_1.zdepth[*i__]));
	    wmin = dmax(wmin,.001f);
	}
	d__1 = (doublereal) wmin;
	d__2 = (doublereal) (-soils_1.bee[*i__ - 1]);
	pmin = pow_dd(&d__1, &d__2);
	*dpdw = soils_1.phsat[*i__ - 1] * (pmax - pmin) / (wmax - wmin);
	return 0;
    }
/* ...  calculo derivada psi(W) nas camada i,i+1 - CH-78 */
    d__1 = (doublereal) w1;
    d__2 = (doublereal) (soils_1.bee[*i__ - 1] + 1.f);
    dpdw1 = -soils_1.bee[*i__ - 1] * soils_1.phsat[*i__ - 1] / pow_dd(&d__1, &
	    d__2);
    d__1 = (doublereal) w2;
    d__2 = (doublereal) (soils_1.bee[j - 1] + 1.f);
    dpdw2 = -soils_1.bee[j - 1] * soils_1.phsat[j - 1] / pow_dd(&d__1, &d__2);
    if (soimul_1.jdpsi == 2) {
/* media ponderada espessura */
	*dpdw = (dpdw1 * soils_1.zdepth[*i__ - 1] + dpdw2 * soils_1.zdepth[*
		i__]) / (soils_1.zdepth[*i__] + soils_1.zdepth[*i__ - 1]);
    }
/* ... */
    if (soimul_1.jdpsi == 3) {
/* media geometrica */
	*dpdw = sqrt(dpdw1 * dpdw2);
    }

    if (soimul_1.jdpsi == 4) {
/* media aritmetica */
	*dpdw = (dpdw1 + dpdw2) / 2.f;
    }
    return 0;
} /* retec_ */

/* ==================================================================== */
/* Subroutine */ int hydcon_(real *avk, integer *i__)
{
    /* System generated locals */
    real r__1, r__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static integer j;
    static real w1, w2, ts, avb, div, avk1, avk2, psi1, psi2, rsame, props, 
	    tsnow, avkmin, avkmax;

/* ==================================================================== */

/*     avk (condutividade hidraulica segmento camadas i,i+1) */

/* -------------------------------------------------------------------- */
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
    j = *i__ + 1;
/* Computing MIN */
    r__1 = stepv_1.www[*i__ - 1];
    w1 = dmin(r__1,1.f);
/* Computing MAX */
    r__1 = stepv_1.www[*i__ - 1];
    w1 = dmax(r__1,.05f);
/* Computing MIN */
    r__1 = stepv_1.www[j - 1];
    w2 = dmin(r__1,1.f);
/* Computing MAX */
    r__1 = stepv_1.www[j - 1];
    w2 = dmax(r__1,.05f);
    d__1 = (doublereal) w1;
    d__2 = (doublereal) (-soils_1.bee[*i__ - 1]);
    psi1 = soils_1.phsat[*i__ - 1] * pow_dd(&d__1, &d__2);
    d__1 = (doublereal) w2;
    d__2 = (doublereal) (-soils_1.bee[j - 1]);
    psi2 = soils_1.phsat[j - 1] * pow_dd(&d__1, &d__2);
    d__1 = (doublereal) w1;
    d__2 = (doublereal) (soils_1.bee[*i__ - 1] * 2.f + 3.f);
    avk1 = soils_1.satco[*i__ - 1] * pow_dd(&d__1, &d__2);
    d__1 = (doublereal) w2;
    d__2 = (doublereal) (soils_1.bee[j - 1] * 2.f + 3.f);
    avk2 = soils_1.satco[j - 1] * pow_dd(&d__1, &d__2);
/* ... */
    if (soimul_1.jkcon == 1) {
/* metodo ME-82 */
	rsame = 0.f;
	avb = (soils_1.bee[*i__ - 1] + soils_1.bee[j - 1]) / 2.f;
	div = psi2 - psi1;
	if (dabs(div) < 1e-6f) {
	    rsame = 1.f;
	}
	*avk = (psi1 * avk1 - psi2 * avk2) / ((3.f / avb + 1.f) * div + rsame)
		;
	avkmin = dmin(avk1,avk2);
	*avk = dmax(*avk,avkmin);
	avkmax = dmax(avk1,avk2);
	*avk = dmin(*avk,avkmax);
    }
/* ... */
    if (soimul_1.jkcon == 2) {
/* media ponderada espessura */
	*avk = (avk1 * soils_1.zdepth[*i__ - 1] + avk2 * soils_1.zdepth[*i__])
		 / (soils_1.zdepth[*i__] + soils_1.zdepth[*i__ - 1]);
    }
/* ... */
    if (soimul_1.jkcon == 3) {
/* media geometrica */
	*avk = sqrt(avk1 * avk2);
    }

    if (soimul_1.jkcon == 4) {
/* media aritmetica */
	*avk = (avk1 + avk2) / 2.f;
    }
/* ---------------------------------------------------------------- */
/*      conductivites and baseflow reduced when temperature drops */
/*      below freezing */
/* ---------------------------------------------------------------- */

/* Computing MIN */
    r__1 = const_1.tf - .01f;
    tsnow = dmin(r__1,stepv_1.tg);
    grads_1.tgs = tsnow * hydrol_1.areas + stepv_1.tg * (1.f - hydrol_1.areas)
	    ;
    ts = grads_1.tgs * (2 - j) + stepv_1.td * (j - 1);
    props = (ts - (const_1.tf - 10.f)) / 10.f;
/* Computing MAX */
    r__1 = .05f, r__2 = dmin(1.f,props);
    props = dmax(r__1,r__2);
    *avk *= props;
    soimul_1.qng *= props;
    return 0;
} /* hydcon_ */


/* ======================================================================= */
/* Subroutine */ int tridia2_(integer *n, real *d__, real *c__, real *a, real 
	*b)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, k, ii;

/* ======================================================================= */

/*  	HR  algoritmo original 1996, corrigido 2009 */

/*             solution of linear system of n x n dimension */
/*             for a tridiagonal matrix */
/*             d    : independent coefficients (input) */
/*                         vector solution      (output) */
/*             b  : main diagonal coefs */
/*             c   :  suprior diagonal coefs */
/*             a   : inferior diagonal coefs */
/* -------------------------------------------------------------- */
    /* Parameter adjustments */
    --b;
    --a;
    --c__;
    --d__;

    /* Function Body */
    c__[1] /= b[1];
    d__[1] /= b[1];
    i__1 = *n - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	ii = i__ - 1;
	b[i__] -= c__[ii] * a[i__];
	if (i__ == *n) {
	    goto L10;
	}
	c__[i__] /= b[i__];
/* calculado */
L10:
	d__[i__] = (d__[i__] - d__[ii] * a[i__]) / b[i__];
    }
    i__1 = *n - 1;
    for (k = 1; k <= i__1; ++k) {
	i__ = *n - k;
/* back substitution */
/* L20: */
	d__[i__] -= c__[i__] * d__[i__ + 1];
    }
    return 0;
} /* tridia2_ */

/* ======================================================================= */
/* Subroutine */ int eqlin_(integer *ifi, integer *inum, integer *n, real *
	d__, real *sup, real *sub, real *diag)
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen),
	     e_wsfe(void);

    /* Local variables */
    static real a[900]	/* was [30][30] */, b[30];
    static integer i__, j, k, m;
    static real x[60]	/* was [30][2] */;
    static integer it;
    static real dif[30];
    static logical conv;
    static integer isum;
    static char aconv[20*3];

    /* Fortran I/O blocks */
    static cilist io___122 = { 0, 0, 0, 0, 0 };
    static cilist io___123 = { 0, 0, 0, "(a3,a10,30(1x,a2,i2,5x))", 0 };
    static cilist io___124 = { 0, 0, 0, "(i3,30(1x,e9.3))", 0 };
    static cilist io___128 = { 0, 0, 0, 0, 0 };
    static cilist io___129 = { 0, 0, 0, "(4a17)", 0 };
    static cilist io___130 = { 0, 0, 0, "(a3,5a13,i3,a20)", 0 };
    static cilist io___131 = { 0, 0, 0, "(i3,4(1x,e12.3))", 0 };
    static cilist io___132 = { 0, 0, 0, 0, 0 };


/* ======================================================================= */
/* 	solução sistema eqs lineares m x n por metodo iterativo */
/* 	HRocha Jan 2009 */
/* 	inum =  1 (método de Jacobi) */
/* 		2 (método de Gauss-Seidel) */

/* 	elements of soil multi-layer scheme in SiB: */
/*             d     : independent coefficients (inputs) */
/* 			 vector solution (output) */
/*             diag  : main diagonal coefs */
/*             sup   :  suprior diagonal coefs */
/*             sub   : inferior diagonal coefs */

/* -------------------------------------------------------------- */
/* critérios convergencia */
    /* Parameter adjustments */
    --diag;
    --sub;
    --sup;
    --d__;

    /* Function Body */
    for (k = 1; k <= 3; ++k) {
/* L1: */
	s_copy(aconv + (k - 1) * 20, " ", (ftnlen)20, (ftnlen)1);
    }
    m = *n;
/* este caso matriz n x n */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	x[i__ + 29] = d__[i__];
/* cond.inicial column vector X(n) */
	dif[i__ - 1] = 0.f;
	i__2 = m;
	for (j = 1; j <= i__2; ++j) {
/* L20: */
	    a[j + i__ * 30 - 31] = 0.f;
	}
/* matrix A(m,n) */
/* L10: */
    }
/* ...	atribuicao matriz eqs Jacobi c/ input esquema soil multi-layer */
    a[0] = diag[1];
    a[30] = sup[1];
    b[0] = d__[1];
    a[m + (*n - 1) * 30 - 31] = sub[*n];
    a[m + *n * 30 - 31] = diag[*n];
    b[m - 1] = d__[*n];
/* 	inserir aqui goto caso matriz 2x2 */
    i__1 = m - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	a[i__ + (i__ - 1) * 30 - 31] = sub[i__];
	a[i__ + i__ * 30 - 31] = diag[i__];
	a[i__ + (i__ + 1) * 30 - 31] = sup[i__];
/* L30: */
	b[i__ - 1] = d__[i__];
    }
    io___122.ciunit = *ifi;
    s_wsle(&io___122);
    do_lio(&c__9, &c__1, " Eqlin: sistema montado", (ftnlen)23);
    e_wsle();
    io___123.ciunit = *ifi;
    s_wsfe(&io___123);
    do_fio(&c__1, "i", (ftnlen)1);
    do_fio(&c__1, " b ", (ftnlen)3);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	do_fio(&c__1, "a_", (ftnlen)2);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
    }
    e_wsfe();
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___124.ciunit = *ifi;
	s_wsfe(&io___124);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&b[i__ - 1], (ftnlen)sizeof(real));
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    do_fio(&c__1, (char *)&a[i__ + j * 30 - 31], (ftnlen)sizeof(real))
		    ;
	}
	e_wsfe();
    }
/* ... 	solução do sistema */
    it = 0;
    conv = FALSE_;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (a[i__ + i__ * 30 - 31] == 0.f) {
	    conv = TRUE_;
	    s_copy(aconv, " a(i,i)=0", (ftnlen)20, (ftnlen)9);
	}
/* L35: */
    }
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    while(! conv) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* recupera passo anterior */
	    x[i__ - 1] = x[i__ + 29];
/* L50: */
	    x[i__ + 29] = 0.f;
	}
/* ---------------------------------------------------------------------- */
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = i__ - 1;
	    for (j = 1; j <= i__2; ++j) {
		if (*inum == 2) {
		    x[i__ + 29] += a[i__ + j * 30 - 31] * x[j + 29];
		}
/* G-Seidel */
/* L60: */
		if (*inum == 1) {
		    x[i__ + 29] += a[i__ + j * 30 - 31] * x[j - 1];
		}
	    }
/* Jacobi */
	    i__2 = *n;
	    for (j = i__ + 1; j <= i__2; ++j) {
/* L65: */
		x[i__ + 29] += a[i__ + j * 30 - 31] * x[j - 1];
	    }
	    x[i__ + 29] = (b[i__ - 1] - x[i__ + 29]) / a[i__ + i__ * 30 - 31];
/* L56: */
	}
/* ---------------------------------------------------------------------- */
/*       if (it.eq.0) write(ifi,'(a3,50(1x,a4,i3,1x,a4,i3))') */
/*     &   'IT',  ( 'xt0_',i,  'xt1_',i   , i= 1,n ) */
/*       write(ifi,'(i3,50(1x,e7.1))') it, (x(i,1),x(i,2),i=1,n) */
	isum = 0;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (x[i__ - 1] != 0.f) {
		dif[i__ - 1] = (r__1 = x[i__ + 29] - x[i__ - 1], dabs(r__1)) /
			 x[i__ - 1];
		if (dif[i__ - 1] < 1e-6f) {
		    ++isum;
		}
	    }
/* L58: */
	    d__[i__] = x[i__ + 29];
	}
	if (isum == *n) {
	    conv = TRUE_;
	    s_copy(aconv + 20, " eps<0 ", (ftnlen)20, (ftnlen)7);
	}
	++it;
	if (it == 50) {
	    conv = TRUE_;
	    s_copy(aconv + 40, " it=itmax ", (ftnlen)20, (ftnlen)10);
	}
    }
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
    io___128.ciunit = *ifi;
    s_wsle(&io___128);
    e_wsle();
    io___129.ciunit = *ifi;
    s_wsfe(&io___129);
    do_fio(&c__1, "Convergencia:", (ftnlen)13);
    for (i__ = 1; i__ <= 3; ++i__) {
	do_fio(&c__1, aconv + (i__ - 1) * 20, (ftnlen)20);
    }
    e_wsfe();
    io___130.ciunit = *ifi;
    s_wsfe(&io___130);
    do_fio(&c__1, "i", (ftnlen)1);
    do_fio(&c__1, "x(i,1)", (ftnlen)6);
    do_fio(&c__1, "x(i,2)", (ftnlen)6);
    do_fio(&c__1, "d(i)", (ftnlen)4);
    do_fio(&c__1, "d(i)_mm/d", (ftnlen)9);
    do_fio(&c__1, "it final=", (ftnlen)9);
    do_fio(&c__1, (char *)&it, (ftnlen)sizeof(integer));
    do_fio(&c__1, " sistema resolvido", (ftnlen)18);
    e_wsfe();
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	io___131.ciunit = *ifi;
	s_wsfe(&io___131);
	do_fio(&c__1, (char *)&i__, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&x[i__ - 1], (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&x[i__ + 29], (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&d__[i__], (ftnlen)sizeof(real));
	r__1 = d__[i__] * 1e3f * 3600 * 24.f;
	do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	e_wsfe();
    }
    io___132.ciunit = *ifi;
    s_wsle(&io___132);
    e_wsle();
    return 0;
} /* eqlin_ */

