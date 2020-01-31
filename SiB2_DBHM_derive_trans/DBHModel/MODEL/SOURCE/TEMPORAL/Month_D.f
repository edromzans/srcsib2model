c This function return the month of given days to known StarYear and StarMonth, StraDay
c Input: 
c	Star_Y,Star_M,Star_D:	Start Date
c	k:			given days, k > 0
c Output:
c	return month of the date of StarYear/StarMonth/StraDay+k 
	integer function month_days(Star_Y,Star_M,Star_D,k)
	implicit none
	integer Star_Y,Star_M,Star_D,k
	integer date_lt_start,days,dayinmonth(12)
	data	dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	integer SY,SM,SD

		days=0
		SY=Star_Y
		SM=Star_M
		SD=Star_D
		do while (days.lt.k)     	
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
		month_days=SM
	return
	end


	subroutine YYMMDD_days(Star_Y,Star_M,Star_D,k,YY,MM,DD)
	implicit none
	integer Star_Y,Star_M,Star_D,k,YY,MM,DD
	integer date_lt_start,days,dayinmonth(12)
	data	dayinmonth /31,28,31,30,31,30,31,31,30,31,30,31/
	integer SY,SM,SD

		days=0
		SY=Star_Y
		SM=Star_M
		SD=Star_D
		do while (days.lt.k)     	
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
		YY=SY
		MM=SM
		DD=SD
	return
	end