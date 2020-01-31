c Get the River water depth etc. time varying data from sub
c Input:
c	Sub
c Output
c	Drivwat	: River water depth (the distance from river water level to ground)
c
	subroutine Get_river_var(n_sub,sub,Drivwat)
      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	real	Driv	!river bed depth
	real	RivWat	!Water depth in river
	real	Drivwat(max_nr,max_nc)
	integer isub,iflow,igrid,inr,inc,n_sub

	do 444 isub=1,n_sub
c		print *,isub, n_sub
		do 333 iflow=sub(isub).nflow,1,-1
			Driv = sub(isub).flow(iflow).height
c			print *,iflow, sub(isub).nflow
			do igrid = 1, sub(isub).flow(iflow).ngrid
c				print *,igrid, sub(isub).flow(iflow).ngrid
				inr = sub(isub).flow(iflow).gridxy(igrid,1)
				inc = sub(isub).flow(iflow).gridxy(igrid,2)
c					print *, inr,inc
				RivWat = sub(isub).flow(iflow).WDj1/1000.
				Drivwat(inr,inc)= Driv - RivWat
c					print *,RivWat,Driv - RivWat
			enddo
c	pause
333		continue
444	continue
	end