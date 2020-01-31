c	This subroutine convert Month/Year to YYMM format CHARACTER
	subroutine ConvYYMM(MM,YYYY,YYMM)
	implicit none
	integer MM,YYYY
	CHARACTER*4	YYMM

	if (MM.lt.1.or.MM.gt.12) then
		PRINT *, 'ERROR INPUT MONTH, MONTH SHOULD BETWEEN 1-12'
		STOP
	else if (MM.lt.10) then
		YYMM(3:3)='0'
		Write (YYMM(4:4),'(i1)') MM
	else 
		Write (YYMM(3:4),'(i2)') MM
	endif

	if (YYYY.lt.1000.or.YYYY.gt.9999) then
		PRINT *, 'ERROR INPUT YEAR, YEAR SHOULD BETWEEN 1000-9999'
		STOP
	else 
		if ((mod(YYYY,100)).lt.10) then
			YYMM(1:1)='0'
			Write (YYMM(2:2),'(i1)') mod(YYYY,100)
		else
			Write (YYMM(1:2),'(i2)') mod(YYYY,100)
		endif
	endif

	end