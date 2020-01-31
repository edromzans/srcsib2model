***CUBIC SPLINE*************************************************************
*                  INTERPOLATION USING CUBIC SPLINES                       *
*ORIGNAL SOURCE COMES FORM (SMALL REVISION HAD DONE FOR OUR PURPOSE)       *
*URL:                                                                      *
*http://www.sali.freeservers.com/engineering/fortran_codes/cubicspline.html*
*NOTE (VERY IMPORTANT):                                                    *
*THE DATA TYPE IS doubleprecision/real, PLESE CHANGE TO corresponding type *
****************************************************************************
      Subroutine CUBSPL(nd,XM,YM,nf,XF,FX)
      integer nd,n,i,m,j,k,count,nnf,over
      real XM(nd),YM(nd),LM(nd),UM(nd),DM(nd),CM(nd),EM(nd),
     $XF(nf),FX(nf),fact,e,ff,g,h

	n = nd
	m = n - 1
      j = m - 1
      k = j - 1
**
*  GENERATION OF TRIDIAGONAL SYSTEM FOR SECOND DERIVATIVE
      do 10 i = 1,j
         DM(i) = 2.* (XM(i+2) - XM(i))
         CM(i) = 6.* (YM(i+2) - YM(i+1)) / (XM(i+2) - XM(i+1)) + 6.* 
     $                         (YM(i) - YM(i+1)) / (XM(i+1) - XM(i))
10    continue
      do 20 i = 2,j
         LM(i) = XM(i+1) - XM(i)
20    continue
      do 30 i = 1,k
         UM(i) = XM(i+2) - XM(i+1)
30    continue 
**
*  SOLUTION OF TRIDIAGONAL SYSTEM
      CALL TRIDI(LM,DM,UM,CM,j,nd)

**
*  EVALUATION AND PRINTING OF CUBIC SPLINES
c      CM(0) = 0.
      CM(n) = 0.
	do nnf=1,nf
		xv = XF(nnf)
         do 50 i = 1,m
            fact = XM(i+1) - XM(i)
		  IF (i.EQ.1) THEN
			e = 0.0
		  ELSE
			e = CM(i-1) / (6.*fact)
		  ENDIF
            ff = CM(i) / (6.*fact)
            IF (i.EQ.1) THEN
			g = (YM(i)/fact)
            ELSE
			g = (YM(i)/fact) - (CM(i-1)*fact/6.)
	      ENDIF
		  h = (YM(i+1)/fact) - (CM(i)*fact/6.)
            EM(i) = e* (XM(i+1) - xv) **3 + ff* (xv - XM(i)) **3 + 
     $              g* (XM(i+1)- xv) + h* (xv - XM(i))
50       continue 

*  SELECTION OF APPROPRIATE SEGMENT (BASED ON THE VALUE) WHERE
*  INTERPOLATION REQUIRED
         count = 1
	   over = 0
60       if (xv.le.XM(count+1)) go to 70
         count = count + 1
		if (count.eq.nd) then
			over =1
			goto 70
		endif
         go to 60
70       continue 
** 
		if (over.eq.0) then
			if (xv.lt.XM(1)) then
				FX(nnf)=-9999.0
			else
				FX(nnf)=EM(count)
			endif
		else
			FX(nnf)=-9999.0
		endif

	enddo
	RETURN
	end
*
***TRIDIAGONAL MATRIX******************************************************
      subroutine TRIDI(L,D,U,COE,n,nd)
      integer nd,n,m,i
      real L(nd),D(nd),U(nd),COE(nd)
**
      m = n - 1 
      do 10 i = 1,m
         L(i+1) = L(i+1) / D(i)
         D(i+1) = D(i+1) - L(i+1) *U(i)
         COE(i+1) = COE(i+1) - L(i+1) *COE(i)
10    continue
**
*  THE COEFFICIENT VECTOR WILL TRANSFORM TO SOLUTION VECTOR*
      COE(n) = COE(n)/D(n)
      do 20 i = m,1,-1
         COE(i) = (COE(i) - U(i) *COE(i+1)) / D(i)
20    continue
      return
      end

