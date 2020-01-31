c This function return the days from the start date
c Input: 
c	Star_Y,Star_M,Star_D:	Start Date
c	year,month,day:			Current Date
c Output:
c	return days from the start date to current date
c Note:
c	If the current date is same as start date, return is 1 (not zero !)
	integer function days_fromstart(Star_Y,Star_M,Star_D,year,month,day)
	implicit none
	integer Star_Y,Star_M,Star_D,year,month,day
	integer date_lt_start,days,dayinmonth(12)
	data	dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	integer SY,SM,SD

	date_lt_start=0
	if (year.lt.Star_Y) then
		date_lt_start=1
	else if (year.eq.Star_Y) then
		if (month.lt.Star_M) then
			date_lt_start=1
		else if(month.eq.Star_M) then
			if (day.lt.Star_D) then
				date_lt_start=1
			endif
		endif
	endif

	if (date_lt_start.eq.1) then
		days_fromstart=-1
	else
		days=1
		SY=Star_Y
		SM=Star_M
		SD=Star_D
		do while (.not.(SY.eq.year.and.SM.eq.month.and.SD.eq.day))     	
			days=days+1
	    	if(mod(SY,4).eq.0.and.SM.eq.2) dayinmonth(SM)=29	    
	    	if(mod(SY,4).ne.0.and.SM.eq.2) dayinmonth(SM)=28
			SD=SD+1
			if (SD.gt.dayinmonth(SM)) then
				SD=1
				SM=SM+1
				if (SM.gt.12) then
					SM=1
					SY=SY+1
				endif
			endif
		enddo
		days_fromstart=days
	endif
	return
	end