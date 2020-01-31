c	This subroutine convert integer to character (0 < integer < 100) 	
	subroutine ConvI2C(i,chari)
	implicit none
	integer i
	CHARACTER*2	chari

	if (i.lt.0) then
		chari='-1'
	else if (i.lt.10) then
		chari(1:1)='0'
		Write (chari(2:2),'(i1)') i
	else if (i.lt.100) then
		Write (chari,'(i2)') i
	else
		chari='-1'
	endif

	end