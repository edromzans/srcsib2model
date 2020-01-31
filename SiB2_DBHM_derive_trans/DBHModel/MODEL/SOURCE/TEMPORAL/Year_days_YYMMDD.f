c This function return Iyear,Imonth,Iday with given year and julian day
c Input: 
c	year, days:	given year and julian day
c Output:
c	return Iyear,Imonth,Iday
	SUBROUTINE Year_days_YYMMDD(year,days,SY,SM,SD,SH)
	implicit none
	real	year,days
	integer Iday,Idays !,Iyear,Imonth
	integer dayinmonth(12)
	data	dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	integer SY,SM,SD,SH

		Iday=0
		Idays=Int(days)
		SY=Int(year)
		SM=1
		SD=0
		SH=int((days-int(days))*24)
		do while (Iday.lt.Idays)     	
			Iday=Iday+1
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
	return
	end