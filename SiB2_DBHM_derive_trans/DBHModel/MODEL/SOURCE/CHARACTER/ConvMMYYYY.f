c	This subroutine convert Month/Year to MMYYYY format CHARACTER
	subroutine ConvMMYYYY(MM,YYYY,MMYYYY)
	implicit none
	integer MM,YYYY
	CHARACTER*6	MMYYYY

	if (MM.lt.1.or.MM.gt.12) then
		PRINT *, 'ERROR INPUT MONTH, MONTH SHOULD BETWEEN 1-12'
		STOP
	else if (MM.lt.10) then
		MMYYYY(1:1)='0'
		Write (MMYYYY(2:2),'(i1)') MM
	else 
		Write (MMYYYY(1:2),'(i2)') MM
	endif

	if (YYYY.lt.1000.or.YYYY.gt.9999) then
		PRINT *, 'ERROR INPUT YEAR, YEAR SHOULD BETWEEN 1000-9999'
		STOP
	else 
		Write (MMYYYY(3:6),'(i4)') YYYY
	endif

	end