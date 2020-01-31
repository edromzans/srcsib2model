c     Same as HPINT, but with the output of err (r.m.s error), and mbe (mean bias err)
c	The least-squares line uses a straight line Y = a + bX 
c	to approximate the given set of data (x1,y1), (x2, y2), ...
c	b: slope, a: intercept	
c     err: Root Mean-Square Error
c     mbe: Mean Bias Error (It should be zero. just for check)
	Subroutine HPINT_2 (N,X,Y,a,b,r2,err,mbe)
	DIMENSION X(*), Y(*)
	REAL	X, Y,slope,y_intercept
	INTEGER N
	REAL	a,b,r2,err,mbe
	REAL sigmaei2,sigmaei

	a=0.0
	b=0.0
	r2=0.0
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
	
	slope = ( SUMx*SUMy - n*SUMxy ) / ( SUMx*SUMx - n*SUMxx )
	y_intercept = ( SUMy - slope*SUMx ) / n

	SSE = 0.0
	SST = Sumyy - SUMy**2 / n
	do i = 1, n
		SSE = SSE + (y_intercept+slope*X(i)-Y(i))**2
	enddo

	r2= 1 - SSE / SST
	
	b = slope
	a = y_intercept
	
c     calculate r.m.s. error
      sigmaei2 = 0.0
      do i = 1, n
        sigmaei2 = sigmaei2 + (a+b*x(i)-y(i))**2
      enddo	
      err = sqrt(sigmaei2/n)
      
c     calculate m.b.e.
      sigmaei = 0.0
      do i = 1, n
        sigmaei = sigmaei + ( a + b * x(i) - y(i) )
      enddo	
      mbe =  sigmaei/n     
      
	END

