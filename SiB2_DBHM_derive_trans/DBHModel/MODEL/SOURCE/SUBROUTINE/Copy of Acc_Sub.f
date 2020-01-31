c The files for subroutines to account items in sub- river basins


c The station control area
c input:
c	codesub: sub basin code
c	flowsub: flow interval id
c	levels:	 Levels for the sub basins
c	n_sub:	 total sub basin
c	sub:	 basin structure
c output:
c	acca:	 the station controlled area
c	KID:	 =0, the station cannot be found in the basin struture
c			 =1, OK
	Subroutine Stn_ctl_area(codesub,flowsub,levels,n_sub,sub,acca,KID)
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H'
	integer codesub,flowsub,KID,flowsub2,levels
	real	acca,flowa

	KID = 0
	do 4441 isub=1,n_sub
	call inflow_subs(levels,sub,isub,insub1,insub2)
	do 3331 iflow=sub(isub).nflow,1,-1
		if (iflow.eq.sub(isub).nflow) then
			if (insub1.ne.0) then
			flowa= 0.0
			do igrid = 1, sub(isub).flow(iflow).ngrid
			inr = sub(isub).flow(iflow).gridxy(igrid,1)
			inc = sub(isub).flow(iflow).gridxy(igrid,2)	
			flowa =flowa +gridarea(inr,inc)*fracnd(inr,inc)/100. !km2
			enddo
			sub(isub).flow(iflow).accarea = 
     ^			flowa + sub(insub1).flow(1).accarea +
     ^			sub(insub2).flow(1).accarea
			else
				flowa= 0.0
				do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)	
				flowa =flowa +gridarea(inr,inc)*fracnd(inr,inc)/100. !km2
				enddo
				sub(isub).flow(iflow).accarea = flowa
			endif
		else
			flowa= 0.0
			do igrid = 1, sub(isub).flow(iflow).ngrid
			inr = sub(isub).flow(iflow).gridxy(igrid,1)
			inc = sub(isub).flow(iflow).gridxy(igrid,2)	
			flowa =flowa +gridarea(inr,inc)*fracnd(inr,inc)/100. !km2
			enddo
			sub(isub).flow(iflow).accarea = 
     ^			 flowa + sub(isub).flow(iflow+1).accarea
		endif
		if (sub(isub).code.eq.codesub) then
			if (flowsub.eq.0) then 
				flowsub2=sub(isub).nflow
			else
				flowsub2=flowsub
			endif
			if (flowsub2.eq.iflow) then
				KID =1
				acca = sub(isub).flow(iflow).accarea
				goto 5551
			endif
		endif
3331	continue
4441	continue
5551	return
	END !Subroutine Stn_ctl_area

c Returen the total area of the basin structure
c	area: total area
c	ngirds: grids number
	subroutine totarea(n_sub,sub,area,ngrids)
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	
	real area
	integer ngrids !,ngrid1(max_nr,max_nc)

	area =0.0
	ngrids=0.0
	do 444 isub=1,n_sub
		do 333 iflow=sub(isub).nflow,1,-1
			do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)	
				area=area+
     $			gridarea(inr,inc)*fracnd(inr,inc)/100.		!(km2)
				!ngrid1(inr,inc)=1
				ngrids=ngrids+1
			enddo
333		continue
444	continue
	end !subroutine SIB_River