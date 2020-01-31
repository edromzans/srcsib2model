c=======================================================================        
c                       comsibc.h
c=======================================================================
c                                                                               
c     sib common block : 1-d version ( carbon )                                 
c                                                              may 1991         
c=======================================================================        
c-----------------------------------------------------------------------        
c        prognostic variables                                                   
c-----------------------------------------------------------------------        
      common/ stepv  / tc, tg, td, capac(2),snoww(2), www(3)                             
c-----------------------------------------------------------------------        
c        physical constants                                                     
c-----------------------------------------------------------------------        
      common/ const  / pie, timcon, cpair, rhoair, psy, hlat, g, vkc,           
     &                 snomel, stefan, tf, clai, cw, snofac, asnow, rcp,        
     &                 kappa, epsfac                                            
      common/ atchem / po2m, pco2m                                              
c-----------------------------------------------------------------------        
c        vegetation : static, dynamic, derived parameters                       
c-----------------------------------------------------------------------        
      common/ gridij / ivtype, istype                                           
      common/ vstate / z2, z1, vcover, chil, tran(2,2), ref(2,2),               
cxx  &                 rootd, ph1, ph2,                                         
     &                 rootd, phc,                                              
     &                 effcon, gradm, binter, respcp, atheta, btheta,           
cxx  &                 trda, trdm, trop, tpsa, tpsb, tpsc                       
     &                 trda, trdm, trop, slti, hlti, shti, hhti                 
      common/ vdyijt / zlt, green, fparc                                        
      common/ vderiv / z0d, dd, cc1, cc2, vmax0, gmudmu                         
c-----------------------------------------------------------------------        
c        soils : space-varying, type-dependent parameters                       
c-----------------------------------------------------------------------        
      common/ soilij / sodep, soref(2)                                          
      common/ soils  / bee, phsat, poros, satco, slope, zdepth(3)               
c-----------------------------------------------------------------------        
c        input atmospheric and site data                                        
c-----------------------------------------------------------------------        
      common/ atmos  / em, tm, um, zm, psur, ppc, ppl, radn(3,2),               
     &                 sunang, swdown, rnetm, cloud, bps                        
c-----------------------------------------------------------------------        
c        site parameters specific to 1-d model operation                        
c-----------------------------------------------------------------------        
      common/ caerod / corb1, corb2, ha, g1, g2, g3, ztz0, zwind, zmet,         
     &                 ht                                                       
      common/ site   / zlong, zlat, salb(2,2), rab(2,3,2)                       
      common/ steps  / dtt, itrunk, ilw, niter, iter, ispare                    
      common/ govern / time, year, month, day, hour                             
c-----------------------------------------------------------------------        
c        variables returned to g.c.m. ( et(kg), h(w m-2), runoff(m),            
c                                       lw(w m-2), drag(kg m-1 s-2) )           
c-----------------------------------------------------------------------        
      common/ donor  / etmass, hflux, roff, zlwup, drag                         
c-----------------------------------------------------------------------        
c        variables calculated from above and ambient conditions                 
c-----------------------------------------------------------------------        
      common/ rause  / z0, d, rbc, rdc                                          
      common/ aerorx / ra, rb, rd                                               
      common/ grads  / tgs, ta, ea, etc, etgs, getc, getgs ,u2, ustar           
      common/ radabs / albedo(2,2,2), radfac(2,2,2), radt(2),                   
     &                 thermk, exrain, tgeff                                    
      common/ surfrs / rst, rstfac(4), rsoil, cog1, cog2, hr, fc, fg            
      common/ hydrol / satcap(2), wc, wg, canex, areas                
      common/ stores / ccx, cg, csoil                                           
      common/ delts  / dtc, dtg, dtd, dth, dqm                                  
      common/ carbio / assimn, respc, respg, pco2i,gsh2o                              
c-----------------------------------------------------------------------        
c        heat fluxes : c-canopy, g-ground, t-trans, e-evap  in j m-2            
c-----------------------------------------------------------------------        
      common/ flux   / ec, eg, hc, hg, chf, shf,                                
     &                 ect, eci, egi, egs,                                      
     &                 ecmass, egmass, heaten                                   
c-----------------------------------------------------------------------        
c       newly added variables
      common/ snow / tsnow,rsnow
c-----------------------------------------------------------------------        
c       newly added variables by TANG Qiuhong, Univ. of Tokyo
      common/ initialv/ tc_ini, tg_ini, td_ini, www_ini(3)                                                                                   

c-----------------------------------------------------------------------        
c       newly added variables by TANG Qiuhong, Univ. of Tokyo
      common/ readin/  iqcaws,   zlwd,             
     & tprec, iqchyd, mevap, msensh, mustar    

      common/ checkbal/  totwb

c	Specify roff: runoff
c	roff1 is roff1= xsc + xss	: adjust canopy/ground water and canopy snow
c	roff2 is roff2= roffo		: overland flow
c	roff3 is roff3= q3g * dtt	: base flow
c	roff4 is roff4= excess * poros*zdepth(i)	: calculate excess
c	gwsoil : groundwater charge to soilwater (or soil abstract GW)
      common/ output/  roff1, roff2, roff3, roff4, gwsoil, 
     & otest1,otest2,otest3
      common/ temp/  item01,item02,GWdep

	  common/ suroff/ surdep, finfil

	  common/ preccoff/ app, bpp, cpp
          common/ irrgrid/ idirr
