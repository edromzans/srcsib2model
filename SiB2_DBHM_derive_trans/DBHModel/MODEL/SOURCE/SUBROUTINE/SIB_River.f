ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	River Routing Model (Kinematic Wave method, With SIB2 Model) 
c	Combine old river routing model and SiB2 model
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine SIB_river(dt,levels,n_sub,sub,
     $	i_y,i_m,i_d,i_h,imd,iyd)
c      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
c      INCLUDE './SIB2/PARDIF.H'      
c	INCLUDE './SIB2/SiB2par.inc'  	                                                
c      INCLUDE './SIB2/COMSIBC.H' 	

	real	dt
	integer isub,iflow,insub1,insub2,levels,igrid
	real	dx,B,S0,rough,Qi_j,WDj !,Lj,Qi1_j
	real	Qi_j1,Lj1,Qi1_j1,WDj1
	integer inr,inc
	real	Lj1_min,LjiLA
	real	roff_t,river_t,totarea,dqt !,criterion
	character*20	stnname
c	real	flow_t,flow_t1
	

c	flow_t=0.0
c	flow_t1=0.0
	do 444 isub=1,n_sub
		call inflow_subs(levels,sub,isub,insub1,insub2)
		do 333 iflow=sub(isub).nflow,1,-1
			if (iflow.eq.sub(isub).nflow) then
				if (insub1.ne.0) then
				Qi_j1=sub(insub1).flow(1).Qi1_j1+sub(insub2).flow(1).Qi1_j1	
				else
				Qi_j=0.0
				Qi_j1=0.0
				endif
			else
				Qi_j1=sub(isub).flow(iflow+1).Qi1_j1
			endif

			B=sub(isub).flow(iflow).width
			rough=sub(isub).flow(iflow).rough

			dx=sub(isub).flow(iflow).dis
			S0=sub(isub).flow(iflow).slope
			WDj=sub(isub).flow(iflow).WDj1

			S0=Max(S0,1.0E-7)
			Lj1_min=-(dt*Qi_j1/dx +WDj*B*0.001)/dt+1.0e-8 !(m2/s)	
			LjiLA=Lj1_min*dx*dt	   !(m3/h)

			river_t=0.0
			totarea=0.0
			do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)	
				river_t=river_t+(riv_rof(inr,inc)+QreA(inr,inc))*
     $			gridarea(inr,inc)*fracnd(inr,inc)/100.*1000.0		!(m3/h)
				totarea=totarea+gridarea(inr,inc)*fracnd(inr,inc)/100. !km2
			enddo
c	According to river water to decide real GW_SW exchange			
			if (river_t.lt.LjiLA) then
			 dqt=(LjiLA-river_t)/totarea/1000.		!(mm /h)
			  do igrid = 1, sub(isub).flow(iflow).ngrid
			   inr = sub(isub).flow(iflow).gridxy(igrid,1)
			   inc = sub(isub).flow(iflow).gridxy(igrid,2)
			   QreA(inr,inc)=QreA(inr,inc)+dqt	!(mm)
			   QreA_h(inr,inc) = QreA_h(inr,inc) + dqt
			   dh=QreA(inr,inc)/Speyield(inr,inc)
			   GWdepA(inr,inc) = GWdepA(inr,inc) + dh /1000.	!(mm -> m)
			  enddo
			  roff_t =LjiLA
			else
			  do igrid = 1, sub(isub).flow(iflow).ngrid
			   inr = sub(isub).flow(iflow).gridxy(igrid,1)
			   inc = sub(isub).flow(iflow).gridxy(igrid,2)
			   dh=QreA(inr,inc)/Speyield(inr,inc)
			   GWdepA(inr,inc) = GWdepA(inr,inc) + dh /1000.	!(mm -> m)
			  enddo
			  roff_t = river_t
			endif
			Lj1= roff_t/dx/dt		!m2/s

			CALL kinematic_wave(dt,dx,B,S0,rough,WDj,
     $			Qi_j1,Lj1,Qi1_j1,WDj1)
			Qi1_j1tmp=Qi_j1
			CALL cal_irrigation(dt,sub,isub,iflow,Qi1_j1,wthdr_t)	
c Adjust WDj1 to insure water balance, it is not required			
c			WDj1=(FINQ+FBE+FLQ-FOUTQ_-wthdr_t*dt/1000.)*1000.
c     $				*1000./B/dx
			WDj1=(Qi_j1*dt+WDj*B*dx*0.001+Lj1*dt*dx-Qi1_j1*dt
     $				-wthdr_t*dt) *1000./B/dx
			WDj1=max (WDj1 , 1.0E-30)

			do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)
				rirr = irriratio(inr,inc)/100.0
				IF (rirr.gt.0.0.and.rirr.le.1.0) THEN		!Return flow
					retnfw_h(inr,inc) = retnfw_h(inr,inc) + retnfw_i(inr,inc)
					Qi1_j1 = Qi1_j1 + retnfw_i(inr,inc)*
     $					gridarea(inr,inc)*fracdd(inr,inc)/100.*1000./dt !(m3/s)
					retnfw_i(inr,inc) = 0.
					tgwir_h (inr,inc) = tgwir_h(inr,inc) + tgw_irr(inr,inc)
					GWdepA(inr,inc) = GWdepA(inr,inc) - 
     $					tgw_irr(inr,inc)/Speyield(inr,inc)/1000.
					tgw_irr(inr,inc) = 0.
				ENDIF
			   GWdep_h(inr,inc)=GWdep_h(inr,inc)+GWdepA(inr,inc)/imd/24.
			   riv_roff(inr,inc)=riv_rof(inr,inc)+QreA(inr,inc)
			enddo

c			flow_t=flow_t+Lj*dx	
c			flow_t1=flow_t1+Lj1*dx	
			sub(isub).flow(iflow).Qi1_j1=Qi1_j1
			sub(isub).flow(iflow).WDj1=WDj1
			sub(isub).flow(iflow).Lj1=Lj1
333		continue
444	continue
c	print*,flow_t,flow_t1,sub(n_sub).flow(1).Qi1_j1
c	pause
	
	end !subroutine SIB_river


