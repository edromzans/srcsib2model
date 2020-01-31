c	Use Inverse Distance Weighting method to interpolate input data

	SUBROUTINE IDW(NPI,XI,YI,FI,NXO,XO,NYO,YO,CM,NP,R0,FO,KER)
C     DESCRIPTION OF ARGUMENTS- Please see PROLOGUE of SUBROUTINE LOTPS
C		CM: adjustive coefficient usually, it is 4.0
C		NP: The number of nearest points (stations), usually it is 8-12
C		R0: Infulence radius
      DIMENSION XI(NPI), YI(NPI), FI(NPI), 
     1 XO(NXO), YO(NYO), FO(NXO,NYO)
C	Local Arguments
	DIMENSION dis(NPI),id(NPI),wk(NP),ak(NP)

	IF (NPI.LT.NP) GO TO 330
	IF (R0.LE.0.0) GO TO 440

      KER = 0

C     Search for NP(8) nearest stations for each grids point
C     AND calculate WT (weighting) for each grids point
C	THEN calculate interpolate results
	do i=1,NXO
		do j=1,NYO
			do k=1,NPI
				id(k)=k
				disk2=(XI(k)-XO(i))**2.0+(YI(k)-YO(j))**2.0
				if (disk2.gt.0.0) then
					dis(k)=sqrt(disk2)	
				else
					dis(k)=0.0
				endif
			enddo

			call SORTPOINT(dis,id,NPI)

			kzero=0
			do k=1,NP
				if (dis(k).gt.0.0) then
					wk(k)=exp(-CM*dis(k)/R0)
				else
					wk(k)=1.0
					kzero=k
				endif
			enddo
			
			if (kzero.ne.0) then
				do k=1,NP
					if (k.ne.kzero) then
						wk(k)=0.0
					endif	
				enddo
			endif

			sum_wk=0.0
			do k=1,NP
				ak(k)=0.0
				sum_wl=0.0
				b2=dis(k)**2.0
				if (b2.eq.0.0) then
					ak(k)=0.0
				else
					mzero=0
					do m=1,NP
						if (m.ne.k) then
							c2=dis(m)**2.0
							if (c2.eq.0.0) then
								mzero=1
							else
								a2=(XI(id(k))-XI(id(m)))**2+
     $								(YI(id(k))-YI(id(m)))**2							
								cos=(b2+c2-a2)/2.0/sqrt(b2*c2) !law of cosines
								ak(k)=ak(k)+wk(m)*(1-cos)								
							endif	
							sum_wl=sum_wl+wk(m)						
						endif
					enddo
					if (mzero.eq.1) then
						ak(k)=0.0
					else
						ak(k)=ak(k)/sum_wl
					endif
				endif
				wk(k)=wk(k)*(1.0+ak(k))
				sum_wk=sum_wk+wk(k)
			enddo

			FO(i,j)=0.0
			do k=1,NP
				FO(i,j)=FO(i,j)+FI(id(k))*wk(k)/sum_wk
			enddo		
		enddo
	enddo
	
	RETURN
		
  440 KER = 3
      PRINT*,'R0 MUST BE POSITIVE VALUE.'
      RETURN
  330 KER = 4
      PRINT*,'NP SHOULD BE LESS THAN NPI.'
      RETURN
  220 KER = 5
      PRINT*,'IDW-MODE IS OUT OF RANGE.  MUST BE 1 OR 2'
      RETURN
		
	END


      SUBROUTINE SORTPOINT (X, IY, N)
      IMPLICIT NONE
c    Use Insertion Sort (increasing order)
C    Description of Parameters
C      X   - array of values to be sorted   (usually abscissas)
C      IY  - array to be carried with X 
C      N   - number of values in array X to be sorted
      INTEGER N
      REAL X(*),IY(*)
 
      REAL TEMP,ITEMP
      INTEGER I, J, K

      DO 101 I=2,N
         IF ( X(I).LT.X(I-1) ) THEN
            DO 51 J=I-2,1,-1
              IF(X(I).GT.X(J)) go to 71
  51          CONTINUE
            J=0
  71        TEMP=X(I)
            ITEMP=IY(I)
            DO 91 K=I,J+2,-1
              IY(K)=IY(K-1)
  91          X(K)=X(K-1)
            X(J+1)=TEMP
            IY(J+1)=ITEMP
         ENDIF
  101 CONTINUE
      RETURN
      END