c	Use Thiessen Polygon method to interpolate input data
c	Nearest distance method

	SUBROUTINE TS(NPI,XI,YI,FI,NXO,XO,NYO,YO,FO,KER)
C     DESCRIPTION OF ARGUMENTS- Please see PROLOGUE of SUBROUTINE LOTPS
      DIMENSION XI(NPI), YI(NPI), FI(NPI), 
     1 XO(NXO), YO(NYO), FO(NXO,NYO)

	IF (NPI.LT.1) GO TO 330
      KER = 0

C     Search for the nearest stations for each grids point
C	Set the value of station to the grid
	do i=1,NXO
		do j=1,NYO

			do k=1,NPI
				disk2=(XI(k)-XO(i))**2.0+(YI(k)-YO(j))**2.0
				if (disk2.gt.0.0) then
					dis=sqrt(disk2)	
				else
					dis=0.0
				endif
				if (k.eq.1) then
					id=1
					dis_min=dis
				else
					if (dis.lt.dis_min) then
						id=k
						dis_min=dis
					endif
				endif
			enddo

			FO(i,j)=FI(id)
	
		enddo
	enddo
	
	RETURN
  330 KER = 1
      PRINT*,'NPI SHOULD BE GREATER THAN ZERO.'
      RETURN
		
	END
