c According to river water to decide  actual withdraw for irrigation
c Input:
c	sub: state of river 
c	isub: the sub basin id for irrigation
c	iflow: the iflow id for irrgation
c	Qi1_j1: the discharge of the sub basin flow interval
	subroutine cal_irrigation(dt,sub,isub,iflow,Qi1_j1,wthdr_t)
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	

	real	wthdr_t,wthdr,percent
	real	supcap,fmin,QQtmp
	integer irrmax,irrtime
	real	irrireq2(max_nr,max_nc)
c	real	criterion,B,rough,dx,S0

	supcap	= 500.0		!Water supply capacity (m3/s)
	fmin	= 10.0		!Min flow in river way(m3/s)
	irrmax	= 120*6		!Max irrigation time step (h)
	
	wthdr	=0.0
	wthdr_t =0.0
	QQtmp	=Qi1_j1

c	Set irrigation time period
	do igrid = 1, sub(isub).flow(iflow).ngrid
		inr = sub(isub).flow(iflow).gridxy(igrid,1)
		inc = sub(isub).flow(iflow).gridxy(igrid,2)	
		if (irrireq(inr,inc).lt.0.0) irrireq(inr,inc) =0.0
		irrtime = iter -irriter(inr,inc)+1
		if (irrtime.gt.irrmax) then
			irriter(inr,inc) = 0
			irrireq(inr,inc) = 0.0
		endif		
	enddo

	do igrid = 1, sub(isub).flow(iflow).nirrg
		inr = sub(isub).flow(iflow).irrxy(igrid,1)
		inc = sub(isub).flow(iflow).irrxy(igrid,2)
		if (irrireq(inr,inc).lt.0.0) irrireq(inr,inc) =0.0
		irrtime = iter -irriter(inr,inc)+1
		if (irrtime.gt.irrmax) then
			irriter(inr,inc) = 0
			irrireq(inr,inc) = 0.0
		endif		
	enddo

c	water required to specified grids (according to irrcode map, irrpro map)
	do igrid = 1, sub(isub).flow(iflow).nirrg
		inr = sub(isub).flow(iflow).irrxy(igrid,1)
		inc = sub(isub).flow(iflow).irrxy(igrid,2)
		percent =irrriper(inr,inc)/100.
		irrireq2(inr,inc) = irrireq(inr,inc)
		wthdr = wthdr + irrireq(inr,inc)*percent
     $		*gridarea(inr,inc)*fracdd(inr,inc)/100.*1000.0/dt		!(m3/s)
	enddo

c	water required to grids in the same flow intervals
	do igrid = 1, sub(isub).flow(iflow).ngrid
		inr = sub(isub).flow(iflow).gridxy(igrid,1)
		inc = sub(isub).flow(iflow).gridxy(igrid,2)	
		percent =1.0- (irrriper(inr,inc)+irresper(inr,inc))/100.
		percent = max( min (percent, 1.0) , 0.0)
		irrireq2(inr,inc) = irrireq(inr,inc)
		wthdr = wthdr + irrireq(inr,inc)*percent
     $		*gridarea(inr,inc)*fracdd(inr,inc)/100.*1000.0/dt		!(m3/s)
	enddo	

	if (wthdr.gt.0.0) then
		if (Qi1_j1.le.fmin) then
			wthdr_t = 0.0
		else				
			if (wthdr.lt.supcap) then
				if (wthdr.lt.(Qi1_j1-fmin)) then
					wthdr_t= wthdr
					Qi1_j1 = Qi1_j1 - wthdr_t
				else
					wthdr_t= Qi1_j1 - fmin
					Qi1_j1 = fmin
				endif
			else
				wthdr_t = supcap
				if (wthdr_t.lt.(Qi1_j1-fmin)) then
					Qi1_j1 = Qi1_j1 - wthdr_t
				else
					wthdr_t= Qi1_j1 - fmin
					Qi1_j1 = fmin
				endif			
			endif
		endif		
		if (wthdr_t.gt.0.0)	
     $	CALL upirrreq(dt,sub,isub,iflow,wthdr_t,wthdr,irrireq2)

	endif

	if (abs(QQtmp-Qi1_j1-wthdr_t).gt.1.E-1) then
		WRITE(icho3, *)'WITHDRAW INBALANCE'
		WRITE(icho3, *)'INBAL,Qorg,Qout,Wthdr:',
     $		QQtmp-Qi1_j1-wthdr_t,QQtmp,Qi1_j1,wthdr_t
	ENDIF

	end ! subroutine cal_irrigation


c Update 'irrireq','soil moisture' accord to actual withdraw
c Input:
c	sub: state of river 
c	isub: the sub basin id for irrigation
c	iflow: the iflow id for irrgation
c	wthdr_t: the actual water withdraw (use to update soil moisture)
	subroutine upirrreq(dt,sub,isub,iflow,wthdr_t,wthdr,irrireq2)
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 
	real	wthdr_t,wthdr,wthtt,sumwb
	real	wthrat
	real	w3t(3),port,zde3t(3)
	real	irrireq2(max_nr,max_nc)

	wthrat = wthdr_t / wthdr	
	wthrat = max( min (wthrat, 1.0) , 0.0)

	sumwb =0.0 !used for check water balance
c update the soil moisture in specified grids
	do igrid = 1, sub(isub).flow(iflow).nirrg
	  inr = sub(isub).flow(iflow).irrxy(igrid,1)
	  inc = sub(isub).flow(iflow).irrxy(igrid,2)	
	  percent =irrriper(inr,inc)/100.
	  irrireq(inr,inc)=irrireq(inr,inc)-irrireq2(inr,inc)*wthrat*percent
	  wthdr_h(inr,inc)=wthdr_h(inr,inc)+irrireq2(inr,inc)*wthrat*percent
	  sumwb =sumwb+ irrireq2(inr,inc)*wthrat*percent*
     $	  gridarea(inr,inc)*fracdd(inr,inc)/100.*1000.0/dt		!(m3/s)
c	Put irrigation water to soil water layer
		rirr = irriratio(inr,inc)/100.0
		IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
		  w3t(1)	= www1_i(inr,inc)
		  w3t(2)	= www2_i(inr,inc)
		  w3t(3)	= www3_i(inr,inc)
		  port		= sporos(inr,inc)
		  zde3t(1)	= zdepth1_i(inr,inc)
		  zde3t(2)	= zdepth2_i(inr,inc)
		  zde3t(3)	= zdepth3_i(inr,inc)
			wsltot		= irrireq2(inr,inc)*wthrat*percent
		  wthtt			= ( wsltot*coffirr+ wsltot*(1.-coffirr)*rirr ) /rirr
		  if (wthtt.gt.0.0) then
			wbl1req = (1.0 - w3t(1)) *port*zde3t(1)*1000.  !(mm)
			if (wthtt.lt.wbl1req) then
			  w3t(1) = w3t(1)+ wthtt/port/zde3t(1)/1000.0
			  wthtt  = 0.0
			else
			  w3t(1) = 1.0
			  wthtt = wthtt - wbl1req
			  if (wthtt.gt.0.0) then
				wbl2req = (1.0 - w3t(2)) *port*zde3t(2)*1000.	 !(mm)
				if (wthtt.lt.wbl2req) then
				  w3t(2) = w3t(2)+ wthtt/port/zde3t(2)/1000.0
				  wthtt  = 0.0
				else
				  w3t(2) = 1.0
				  wthtt = wthtt - wbl2req
				  if (wthtt.gt.0.0) then
c					Print *,"Surplus irrigation to Layer3",inr,inc,wthtt
					wbl3req = (1.0 - w3t(3)) *port*zde3t(3)*1000.	 !(mm)
					if (wthtt.lt.wbl3req) then
					w3t(3) = w3t(3)+ wthtt/port/zde3t(3)/1000.0
					wthtt  = 0.0
					else
					w3t(3) = 1.0
					wthtt = wthtt - wbl3req  !(mm)
					retnfw_i(inr,inc) = retnfw_i(inr,inc) + wthtt*rirr		!(mm)
					endif
				  endif
				endif
			  endif
			endif
		  endif
		  www1_i(inr,inc)=w3t(1)
		  www2_i(inr,inc)=w3t(2)
		  www3_i(inr,inc)=w3t(3)

		  w3t(1)	= www1_ni(inr,inc)
		  w3t(2)	= www2_ni(inr,inc)
		  w3t(3)	= www3_ni(inr,inc)
		  port		= sporos(inr,inc)
		  zde3t(1)	= zdepth1_ni(inr,inc)
		  zde3t(2)	= zdepth2_ni(inr,inc)
		  zde3t(3)	= zdepth3_ni(inr,inc)
		  wthtt			= wsltot*(1.-coffirr)
		  if (wthtt.gt.0.0) then
			wbl1req = (1.0 - w3t(1)) *port*zde3t(1)*1000.  !(mm)
			if (wthtt.lt.wbl1req) then
			  w3t(1) = w3t(1)+ wthtt/port/zde3t(1)/1000.0
			  wthtt  = 0.0
			else
			  w3t(1) = 1.0
			  wthtt = wthtt - wbl1req
			  if (wthtt.gt.0.0) then
				wbl2req = (1.0 - w3t(2)) *port*zde3t(2)*1000.	 !(mm)
				if (wthtt.lt.wbl2req) then
				  w3t(2) = w3t(2)+ wthtt/port/zde3t(2)/1000.0
				  wthtt  = 0.0
				else
				  w3t(2) = 1.0
				  wthtt = wthtt - wbl2req
				  if (wthtt.gt.0.0) then
c					Print *,"Surplus irrigation to Layer3",inr,inc,wthtt
					wbl3req = (1.0 - w3t(3)) *port*zde3t(3)*1000.	 !(mm)
					if (wthtt.lt.wbl3req) then
					w3t(3) = w3t(3)+ wthtt/port/zde3t(3)/1000.0
					wthtt  = 0.0
					else
					w3t(3) = 1.0
					wthtt = wthtt - wbl3req  !(mm)
					!(tgw_irr : mm over whole region)
					tgw_irr(inr,inc) = tgw_irr(inr,inc) + wthtt*(1.-rirr) 
					endif
				  endif
				endif
			  endif
			endif
		  endif
		  www1_ni(inr,inc)=w3t(1)
		  www2_ni(inr,inc)=w3t(2)
		  www3_ni(inr,inc)=w3t(3)
		ENDIF
	enddo

c update the soil moisture in the same flow intervals
	do igrid = 1, sub(isub).flow(iflow).ngrid
	  inr = sub(isub).flow(iflow).gridxy(igrid,1)
	  inc = sub(isub).flow(iflow).gridxy(igrid,2)	
	  percent =1.0- (irrriper(inr,inc)+irresper(inr,inc))/100.
	  percent = max( min (percent, 1.0) , 0.0)
	  irrireq(inr,inc)=irrireq(inr,inc)-irrireq2(inr,inc)*wthrat*percent
	  wthdr_h(inr,inc)=wthdr_h(inr,inc)+irrireq2(inr,inc)*wthrat*percent
	  sumwb =sumwb+ irrireq2(inr,inc)*wthrat*percent*
     $	  gridarea(inr,inc)*fracdd(inr,inc)/100.*1000.0/dt		!(m3/s)
c	Put irrigation water to soil water layer
		rirr = irriratio(inr,inc)/100.0
		IF (rirr.gt.0.0.and.rirr.le.1.0) THEN
		  w3t(1)	= www1_i(inr,inc)
		  w3t(2)	= www2_i(inr,inc)
		  w3t(3)	= www3_i(inr,inc)
		  port		= sporos(inr,inc)
		  zde3t(1)	= zdepth1_i(inr,inc)
		  zde3t(2)	= zdepth2_i(inr,inc)
		  zde3t(3)	= zdepth3_i(inr,inc)
			wsltot		= irrireq2(inr,inc)*wthrat*percent
		  wthtt			= ( wsltot*coffirr+ wsltot*(1.-coffirr)*rirr ) /rirr
		  if (wthtt.gt.0.0) then
			wbl1req = (1.0 - w3t(1)) *port*zde3t(1)*1000.  !(mm)
			if (wthtt.lt.wbl1req) then
			  w3t(1) = w3t(1)+ wthtt/port/zde3t(1)/1000.0
			  wthtt  = 0.0
			else
			  w3t(1) = 1.0
			  wthtt = wthtt - wbl1req
			  if (wthtt.gt.0.0) then
				wbl2req = (1.0 - w3t(2)) *port*zde3t(2)*1000.	 !(mm)
				if (wthtt.lt.wbl2req) then
				  w3t(2) = w3t(2)+ wthtt/port/zde3t(2)/1000.0
				  wthtt  = 0.0
				else
				  w3t(2) = 1.0
				  wthtt = wthtt - wbl2req
				  if (wthtt.gt.0.0) then
c					Print *,"Surplus irrigation to Layer3",inr,inc,wthtt
					wbl3req = (1.0 - w3t(3)) *port*zde3t(3)*1000.	 !(mm)
					if (wthtt.lt.wbl3req) then
					w3t(3) = w3t(3)+ wthtt/port/zde3t(3)/1000.0
					wthtt  = 0.0
					else
					w3t(3) = 1.0
					wthtt = wthtt - wbl3req	!(mm)
					retnfw_i(inr,inc) = retnfw_i(inr,inc) + wthtt*rirr  !(mm)
					endif
				  endif
				endif
			  endif
			endif
		  endif
		  www1_i(inr,inc)=w3t(1)
		  www2_i(inr,inc)=w3t(2)
		  www3_i(inr,inc)=w3t(3)

		  w3t(1)	= www1_ni(inr,inc)
		  w3t(2)	= www2_ni(inr,inc)
		  w3t(3)	= www3_ni(inr,inc)
		  port		= sporos(inr,inc)
		  zde3t(1)	= zdepth1_ni(inr,inc)
		  zde3t(2)	= zdepth2_ni(inr,inc)
		  zde3t(3)	= zdepth3_ni(inr,inc)
		  wthtt			= wsltot*(1.-coffirr)
		  if (wthtt.gt.0.0) then
			wbl1req = (1.0 - w3t(1)) *port*zde3t(1)*1000.  !(mm)
			if (wthtt.lt.wbl1req) then
			  w3t(1) = w3t(1)+ wthtt/port/zde3t(1)/1000.0
			  wthtt  = 0.0
			else
			  w3t(1) = 1.0
			  wthtt = wthtt - wbl1req
			  if (wthtt.gt.0.0) then
				wbl2req = (1.0 - w3t(2)) *port*zde3t(2)*1000.	 !(mm)
				if (wthtt.lt.wbl2req) then
				  w3t(2) = w3t(2)+ wthtt/port/zde3t(2)/1000.0
				  wthtt  = 0.0
				else
				  w3t(2) = 1.0
				  wthtt = wthtt - wbl2req
				  if (wthtt.gt.0.0) then
c					Print *,"Surplus irrigation to Layer3",inr,inc,wthtt
					wbl3req = (1.0 - w3t(3)) *port*zde3t(3)*1000.	 !(mm)
					if (wthtt.lt.wbl3req) then
					w3t(3) = w3t(3)+ wthtt/port/zde3t(3)/1000.0
					wthtt  = 0.0
					else
					w3t(3) = 1.0
					wthtt = wthtt - wbl3req  !(mm)
					!(tgw_irr : mm over whole region)
					tgw_irr(inr,inc) = tgw_irr(inr,inc) + wthtt*(1.-rirr) 
					endif
				  endif
				endif
			  endif
			endif
		  endif
		  www1_ni(inr,inc)=w3t(1)
		  www2_ni(inr,inc)=w3t(2)
		  www3_ni(inr,inc)=w3t(3)
		ENDIF
	enddo

	if (abs(sumwb-wthdr_t).gt.1.E-3) then
		WRITE(icho3, *) 'IRRIGATION INBALANCE'
		WRITE(icho3, *) 'INBAL, Wthdr, AccWthdr:',
     $		sumwb-wthdr_t,wthdr_t,sumwb
	endif

	end !subroutine upirrreq

