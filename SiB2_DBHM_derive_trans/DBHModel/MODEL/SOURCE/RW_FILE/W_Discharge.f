c Output the result to files, for one year, one sub-basin
c Input:
c	startyear:  the start model year
c	year:		the year will be output
c	isub:		the discharge of the sub-basin will be output
c	Qy,Qm,Qd,Qh:array with calculated discharge data
c
	subroutine output_discharge (startyear,year,isub,Qy,Qm,Qd,Qh)
      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	integer			startyear,endyear,year
	real			Qh(max_sub,8800)		! hourly mean discharge (m3/s)
      real			Qd(max_sub,366)			! daily average discharge (m3/s)
	real			Qm(max_sub,12)			! monthly mean discharge (m3/s)
	real			Qy(max_sub,max_year)	! annual mean discharge (m3/s)

	integer			iyc,i,j,k,ihc,idc
	integer			isub,dayinmonth(12)
      data			dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	character*80	yearfile,monthfile,dayfile,hourfile,Subid,Yid,OUT_D
	character*75	YTitl,MTitl,DTitl,HTitl
	integer			l1,l2,ll1,ll2,o1,o2 !,Imkdir,makedirqq

	OUT_D='../HYDRO_OUTPUT/'
	write (Subid,*), isub
	write (Yid,*), year
	call strlen(Subid,l1,l2)
	call strlen(Yid,ll1,ll2)
	call strlen(OUT_D,o1,o2)

c	Imkdir=makedirqq(OUT_D(o1:o2))

	yearfile=OUT_D(o1:o2)//Yid(ll1:ll2)//'_STN_'//Subid(l1:l2)//'_Y.asc'
	monthfile=OUT_D(o1:o2)//Yid(ll1:ll2)//'_STN_'//Subid(l1:l2)//'_M.asc'
	dayfile=OUT_D(o1:o2)//Yid(ll1:ll2)//'_STN_'//Subid(l1:l2)//'_D.asc'
	hourfile=OUT_D(o1:o2)//Yid(ll1:ll2)//'_STN_'//Subid(l1:l2)//'_H.asc'
	YTitl='        Year   Discharge'
	MTitl='        Year       Month   Discharge'
	DTitl='        Year       Month         Day   Discharge'
	HTitl='        Year       Month         Day        Hour   Discharge'

	print*, 'Output Year: ', year

	iyc=year-startyear+1
	if (iyc.eq.1) then
		open(1,file=yearfile,	status='unknown')
		open(2,file=monthfile,	status='unknown')
		open(3,file=dayfile,	status='unknown')
		open(4,file=hourfile,	status='unknown')
		write (1,*),YTitl
		write (2,*),MTitl
		write (3,*),DTitl
		write (4,*),HTitl
	else
		open(1,file=yearfile,	access='append', status='old')
		open(2,file=monthfile,	access='append', status='old')
		open(3,file=dayfile,	access='append', status='old')
		open(4,file=hourfile,	access='append', status='old')
	endif

	write (1,*),year,Qy(isub,iyc)

	idc=0
	ihc=0
	do i=1,12
		write (2,*),year,i,Qm(isub,i)
	    if(mod(year,4).eq.0 .and. i.eq.2) dayinmonth(i)=29	    
	    if(mod(year,4).ne.0 .and. i.eq.2) dayinmonth(i)=28
		do j=1,dayinmonth(i)
			idc=idc+1
			write (3,*),year,i,j,Qd(isub,idc)
			do k=1,24
				ihc=ihc+1
				write (4,*),year,i,j,k,Qh(isub,ihc)
			enddo
		enddo
	enddo

	close(1)
	close(2)
	close(3)
	close(4)

	end
