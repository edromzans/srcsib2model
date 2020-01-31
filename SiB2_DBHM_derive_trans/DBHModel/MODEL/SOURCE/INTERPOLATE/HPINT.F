c	The least-squares line uses a straight line Y = a + bX 
c	to approximate the given set of data (x1,y1), (x2, y2), ...
c	b: slope, a: intercept	
c	AVy: averaged value of Y
c	Sign: significance 
c	The statistical significance of the trends is evaluated using the 
c	Student¡¯s t-test with the following significance parameter:
c	 t = r \sqrt{(n-2)/(1-r^2)}
c	where r is the correlation coefficient between $t$ and $y$.
	Subroutine HPINT(N,X,Y,a,b,r2,AVy)
	DIMENSION X(*), Y(*)
	REAL	X, Y,slope,y_intercept,AVy,Sign
	INTEGER N
	REAL	a,b,r2,bbru,bbrd,bbr

	a=0.0
	b=0.0
	r2=0.0
	AVy=0.0
	Sign=0.0
	SUMx=0.0
	SUMy=0.0
	SUMxy=0.0
	SUMxx=0.0
	Sumyy=0.0

	do i = 1, n
		SUMx = SUMx + x(i)
		SUMy = SUMy + y(i)
		SUMxy = SUMxy + x(i)*y(i)
		SUMxx = SUMxx + x(i)*x(i)
		Sumyy = SUMyy + y(i)*y(i)
	end do
	
	AVy = SUMy / N

	slope = ( SUMx*SUMy - n*SUMxy ) / ( SUMx*SUMx - n*SUMxx )
	y_intercept = ( SUMy - slope*SUMx ) / n

c	SSE = 0.0
c	SST = Sumyy - SUMy**2 / n
c	do i = 1, n
c		SSE = SSE + (y_intercept+slope*X(i)-Y(i))**2
c	enddo

c	bbru = n * SUMxy - SUMx * SUMy
c	bbrd = sqrt ( (n*SUMxx-SUMx*SUMx) * (n*Sumyy - SUMy*SUMy)  )
	bbr  = (n * SUMxy - SUMx * SUMy)/
     $ sqrt ( (n*SUMxx-SUMx*SUMx) * (n*Sumyy - SUMy*SUMy)  )

	r2= bbr*bbr
	Sign=bbr*sqrt( (n-2)/(1-bbr*bbr))*100.
cccccccccccccccccc
c	AVy=Sign
cccccccccccccccccc
	b = slope
	a = y_intercept
	END

