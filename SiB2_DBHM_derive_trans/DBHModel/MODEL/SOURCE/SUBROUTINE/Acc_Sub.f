c The files for subroutines to account items in sub- river basins


c The station control area
c input:
c	codesub: sub basin code
c	flowsub: flow interval id
c	levels:	 Levels for the sub basins
c	n_sub:	 total sub basin
c	sub:	 basin structure
c output:
c	codeid2:	 the station controlled area
c	KID:	 =0, the station cannot be found in the basin struture
c			 =1, OK
	Subroutine Stn_ctl_area(codesub,flowsub,levels,n_sub,sub,codeid2,KID)
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H'
	integer codesub,flowsub,KID,flowsub2,levels
	integer codeid(max_nr,max_nc),codeid2(max_nr,max_nc),dig(3)
c	character*80	tmpname

	KID = 0
	codeid =-9999
	codeid2=-9999

	do 4441 isub=1,n_sub
	call inflow_subs(levels,sub,isub,insub1,insub2)
	do 3331 iflow=sub(isub).nflow,1,-1
		if (iflow.eq.sub(isub).nflow) then
			if (insub1.ne.0) then
			do igrid = 1, sub(isub).flow(iflow).ngrid
			inr = sub(isub).flow(iflow).gridxy(igrid,1)
			inc = sub(isub).flow(iflow).gridxy(igrid,2)
			codeid(inr,inc)=sub(isub).code	
			enddo
			else
				do igrid = 1, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)	
				codeid(inr,inc)=sub(isub).code
				enddo
			endif
		else
			do igrid = 1, sub(isub).flow(iflow).ngrid
			inr = sub(isub).flow(iflow).gridxy(igrid,1)
			inc = sub(isub).flow(iflow).gridxy(igrid,2)	
			codeid(inr,inc)=sub(isub).code
			enddo
		endif
		if (sub(isub).code.eq.codesub) then
			if (flowsub.eq.0) then 
				flowsub2=sub(isub).nflow
			else
				flowsub2=flowsub
			endif
			if (flowsub2.eq.iflow) then
				KID =1

		codeid2=codeid
		id=sub(isub).code
		dig(1)=id/100
		dig(2)=id/10-id/100*10
		dig(3)=id-id/10*10
c		print *,"Summary stn_ctl_area, used for 3 layer river network"
		do i=1,max_nr
			do j=1,max_nc
			if (codeid2(i,j).ne.-9999) then
				idig1=codeid2(i,j)/100
				if (mod(dig(1),2).eq.0.and.dig(1).ne.0) then
					if (idig1.ne.dig(1)) codeid2(i,j)=-9999
				endif
			endif
			enddo
		enddo
		do i=1,max_nr
		do j=1,max_nc
		if (codeid2(i,j).ne.-9999) then
			idig1=codeid2(i,j)/100
			idig2=codeid2(i,j)/10-codeid2(i,j)/100*10
			if (mod(dig(2),2).eq.0.and.dig(2).ne.0) then
			if (idig1.ne.dig(1).or.idig2.ne.dig(2)) codeid2(i,j)=-9999
			endif
		endif
		enddo
		enddo
		do i=1,max_nr
		do j=1,max_nc
		if (codeid2(i,j).ne.-9999) then
			idig1=codeid2(i,j)/100
			idig2=codeid2(i,j)/10-codeid2(i,j)/100*10
			idig3=codeid2(i,j)-codeid2(i,j)/10*10
			if (mod(dig(3),2).eq.0.and.dig(3).ne.0) then
				if (idig1.ne.dig(1).or.idig2.ne.dig(2).or.
     $				idig3.ne.dig(3)) codeid2(i,j)=-9999
			endif
		endif
		enddo
		enddo
c		tmpname=""
c		write(tmpname,'(I3,I1)') codesub,flowsub
c		print *, "BE:",codesub,flowsub
c	call writefile_int (tmpname,max_nr,max_nc,-457500.,-1500500.,10000.,
c     $	-9999,codeid2,max_nr,max_nc)
c	write(*,'("kill ",I3,I1,"_")') codesub,flowsub
c	write(*,'("asciigrid ",I3,I1," ",I3,I1,"_ int")') 
c     $	codesub,flowsub,codesub,flowsub

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