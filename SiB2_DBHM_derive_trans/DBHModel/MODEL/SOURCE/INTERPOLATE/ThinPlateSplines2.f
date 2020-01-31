      SUBROUTINE LOTPS (MODE,NPPR,NPI,XI,YI,FI,NXO,XO,NYO,YO,IWK,NIWK,
     1                  NIWKU,WK,NWK,NWKU,FO,KER)
C***START PROLOGUE LOTPS
C
C     THIS VERSION IS DATED 03/04/82.
C
C                RICHARD FRANKE
C                DEPARTMENT OF MATHEMATICS
C                NAVAL POSTGRADUATE SCHOOL
C                MONTEREY, CALIFORNIA  93940
C                     (408)646-2758 / 2206
C
C
C
C     REFERENCE
C        SMOOTH INTERPOLATION OF SCATTERED DATA BY LOCAL THIN
C        PLATE SPLINES, COMPUTERS AND MATHEMATICS WITH
C        APPLICATIONS 8(1982)???-???+8
C                    OR
C        NAVAL POSTGRADUATE SCHOOL TR#NPS-53-81-002, 1981
C        (AVAILABLE FROM NTIS, AD-A098 232/2)
C
C     ABSTRACT
C        SUBROUTINE LOTPS SERVES AS THE USER INTERFACE FOR A SET OF
C        SUBROUTINES WHICH SOLVE THE SCATTERED DATA INTERPOLATION
C        PROBLEM.  A SMOOTH FUNCTION PASSING THROUGH THE GIVEN POINTS
C        (XI(K),YI(K),FI(K)),K=1,...,NPI IS CONSTRUCTED.
C        THE RESULT RETURNED IS AN ARRAY OF VALUES, FO(I,J), OF THE INT-
C        ERPOLATION FUNCTION AT GRID POINTS, (XO(I),YO(J)),I=1,...,NXO,
C        J=1,...,NYO.
C        THE METHOD USED INVOLVES CONSTRUCTION OF LOCALLY DEFINED 'THIN
C        PLATE SPLINES', WHICH ARE THEN BLENDED TOGETHER SMOOTHLY
C        THROUGH THE USE OF A PARTITION OF UNITY DEFINED ON A
C        RECTANGULAR GRID ON THE PLANE.  THE FUNCTIONS IN THE PARTITION
C        OF UNITY ARE UNIVARIATE PIECEWISE HERMITE CUBIC POLYNOMIALS.
C
C     CAUTIONS
C        THE USER SHOULD BE AWARE THAT FOR SOME DATA THE INTERPOLATION
C        FUNCTION MAY BE ILL-BEHAVED.  SOME INVESTIGATION OF ITS
C        BEHAVIOR FOR THE TYPE OF DATA TO BE INPUT SHOULD BE UNDERTAKEN
C        BEFORE IMBEDDING ANY SCHEME FOR SCATTERED DATA INTERPOLATION
C        INTO ANOTHER PROGRAM.
C
C     DESCRIPTION OF ARGUMENTS
C
C        MODE - INPUT.  INDICATES THE STATUS OF THE CALCULATION.
C                 = 1,  SET UP THE PROBLEM.  COMPUTE THE COEFFICIENTS
C                       FOR THE LOCAL APPROXIMATIONS BY THIN PLATE
C                       SPLINES, AND RETURN THE GRID OF INTERPOLATED
C                       FUNCTION VALUES INDICATED BY NXO, XO, NYO, YO
C                       IN THE ARRAY FO.
C                 = 2,  THIS MODE VALUE IS A CONVENIENCE FOR USERS WHO
C                       WISH TO CALL THE ROUTINE TO EVALUATE THE
C                       SURFACE REPEATEDLY ON DIFFERENT GRIDS OF
C                       POINTS.  A CALL TO LOTPS WITH MODE = 1 HAS
C                       BEEN MADE PREVIOUSLY, NOW CALCULATE
C                       THE GRID OF INTERPOLATED POINTS INDICATED
C                       BY NXO, XO, NYO, YO IN IN THE ARRAY FO.  THE
C                       PROGRAM ASSUMES THAT THE ARRAYS XI, YI, IWK,
C                       AND WK ARE UNCHANGED FROM THE PREVIOUS CALL.
C        NPPR - INPUT.  DESIRED AVERAGE NUMBER OF POINTS PER REGION.
C                       THE SUGGESTED VALUE FOR THE NOVICE USER IS TEN,
C                       WHICH USUALLY GIVES GOOD RESULTS.  THIS PAR-
C                       AMETER HAS TO DO WITH THE LOCAL PROPERTY OF THE
C                       SURFACE.  THE INFLUENCE REGION OF A POINT HAS
C                       AREA WHICH IS ROUGHLY PROPORTIONAL TO NPPR.
C                       UNDER CERTAIN CONDITIONS, SUCH AS TO PRESERVE
C                       ROTATIONAL INVARIANCE, OR TO FORCE CERTAIN
C                       SETS OF POINTS TO BELONG TO THE SAME REGION,
C                       THE USER MAY SPECIFY HIS OWN GRID LINES.
C                       IF THE USER WISHES TO SPECIFY HIS OWN GRID LINES
C                       X TILDA AND Y TILDA, HE MAY DO SO BY SETTING
C                       NPPR = 0 AND SETTING NECESSARY VALUES IN THE
C                       ARRAYS IWK AND WK, AS NOTED BELOW.  DATA WHICH
C                       HAS A POOR DISTRIBUTION OVER THE REGION OF INT-
C                       EREST SHOULD PROBABLY HAVE THE GRID SPECIFIED.
C                       THIS IS ALSO ADVISABLE IF THE X-Y POINTS OCCUR
C                       ALONG LINES.  SEE THE REFERENCE FOR ADDITIONAL
C                       DETAILS.
C        NPI  - INPUT.  NUMBER OF INPUT DATA POINTS.
C        XI   - \
C        YI   - INPUT ARRAYS.  THE DATA POINTS (XI,YI,FI), I=1,...,NPI.
C        FI   - /
C        NXO  - INPUT.  THE NUMBER OF XO VALUES AT WHICH THE INTERP-
C                       OLATION FUNCTION IS TO BE CALCULATED.
C        XO   - INPUT ARRAY.  THE VALUES OF X AT WHICH THE INTERPOLATION
C                       FUNCTION IS TO BE CALCULATED.  THESE SHOULD
C                       BE IN INCREASING ORDER FOR MOST EFFICIENT
C                       EVALUATION, HOWEVER, THEY ONLY NEED TO BE
C                       MONOTONIC.
C        NYO  - INPUT.  THE NUMBER OF YO VALUES AT WHICH THE INTERP-
C                       OLATION FUNCTION IS TO BE CALCULATED.
C        YO   - INPUT ARRAY.  THE VALUES OF Y AT WHICH THE INTERPOLATION
C                       FUNCTION IS TO BE CALCULATED.  THESE SHOULD
C                       BE IN INCREASING ORDER FOR MOST EFFICIENT
C                       EVALUATION, HOWEVER, THEY ONLY NEED TO BE
C                       MONOTONIC.
C        IWK  - INPUT/OUTPUT ARRAY.  THIS ARRAY IS OUTPUT WHEN MODE = 1
C                       AND IS INPUT WHEN MODE = 2.  THIS MUST BE
C                       AN ARRAY DIMENSIONED APPROXIMATELY 7*NPI.  THE
C                       EXACT DIMENSION IS NOT KNOWN A PRIORI, BUT
C                       WILL BE RETURNED AS THE VALUE OF NIWKU.
C                       WHEN NPPR IS INPUT AS ZERO THE USER MUST
C                       SPECIFY THE NUMBER OF VERTICAL GRID LINES (THE
C                       NUMBER OF X TILDA VALUES) IN IWK(1) AND THE
C                       NUMBER OF HORIZONTAL GRID LINES (THE NUMBER OF
C                       Y TILDA VALUES) IN IWK(2).
C        NIWK - INPUT.  ON ENTRY WITH MODE = 1 THIS MUST BE SET TO THE
C                       DIMENSION OF THE ARRAY IWK IN THE CALLING
C                       PROGRAM.
C        NIWKU- OUTPUT.  THE ACTUAL NUMBER OF LOCATIONS NEEDED IN THE
C                       ARRAY IWK.
C        WK   - INPUT/OUTPUT ARRAY.  THIS ARRAY IS OUTPUT WHEN MODE = 1
C                       AND IS INPUT WHEN MODE = 2.  THIS MUST BE AN
C                       ARRAY DIMENSIONED APPROXIMATELY 7*NPI PLUS
C                       THE NUMBER NEEDED TO SET UP AND SOLVE THE SYSTEM
C                       OF EQUATIONS FOR THE LOCAL APPROXIMATIONS.  FOR
C                       NPPR NONZERO THIS WILL BE ABOUT 2.5*NPPR*NPPR
C                       PLUS 11*NPPR.  THE EXACT DIMENSION IS NOT KNOWN
C                       A PRIORI, BUT WILL BE RETURNED AS THE VALUE OF
C                       NWKU.
C                       WHEN NPPR IS INPUT AS ZERO THE USER MUST SPECIFY
C                       THE VALUES OF X TILDA AND Y TILDA AS FOLLOWS.
C                       WK(2), ... , WK(NXG+1) ARE THE NXG (= IWK(1))
C                       X GRID VALUES, X(I) TILDA, IN INCREASING ORDER.
C                       TYPICALLY WK(1) = MIN X(I), ALTHOUGH IT NEED
C                       NOT BE.  WK(1) MUST BE LESS THAN OR EQUAL TO
C                       WK(2), AND SHOULD BE LESS THAN OR EQUAL TO
C                       MIN X(I).  WK(NXG+2) IS USUALLY MAX X(I), AL-
C                       THOUGH IT NEED NOT BE.  WK(NXG+2) MUST BE
C                       GREATER THAN WK(NXG+1), AND SHOULD BE GREATER
C                       THAN OR EQUAL TO MAX X(I).
C                       THE VALUES OF WK(NXG+3), ... , WK(NXG+NYG+4)
C                       ARE THE Y GRID VALUES, Y(I) TILDA, AND MUST
C                       SATISFY DUAL CONDITIONS.
C        NWK  - INPUT.  ON ENTRY WITH MODE = 1 THIS MUST BE SET TO THE
C                       DIMENSION OF THE ARRAY WK IN THE CALLING
C                       PROGRAM.
C        NWKU - OUTPUT.  THE ACTUAL NUMBER OF LOCATIONS NEEDED IN THE
C                       ARRAY WK.
C        FO   - OUTPUT ARRAY.  VALUES OF THE INTERPOLATION FUNCTION AT
C                       THE GRID OF POINTS INDICATED BY NXO, XO, NYO, YO
C                       FO IS ASSUMED TO BE DIMENSIONED (NXO,NYO) IN THE
C                       CALLING PROGRAM.
C        KER  - OUTPUT.  RETURN INDICATOR.
C                 = 0,  NORMAL RETURN.
C                 = NONZERO, ERROR CONDITION ENCOUNTERED.
C
C     ERROR MESSAGES
C        NO. 1   FATAL         SINGULAR MATRIX IN THE CALCULATION OF
C                              LOCAL THIN PLATE SPLINES.  TRY LARGER
C                              VALUE FOR NPPR AND/OR MINPTS.  (MINPTS
C                              IS IN SUBROUTINE LOLIP.)
C        NO. 2   RECOVERABLE   FIRST CALL TO LOTPS MUST BE WITH MODE=1
C        NO. 3   FATAL         PREVIOUS ERROR RETURN FROM SUBROUTINE
C                              LOCAL NOT CORRECTED.
C        NO. 4   FATAL         ARRAY IWK AND/OR WK NOT DIMENSIONED LARGE
C                              ENOUGH.  REDIMENSION AS GIVEN BY NIWKU
C                              AND NWKU.
C        NO. 5   RECOVERABLE   MODE IS OUT OF RANGE.
C
C     SUBROUTINES USED
C
C        THIS PACKAGE:  LOGRD, LOLIP, LOCAL, LOEVL.
C        LINPACK: SGECO, SGESL
C        SLATEC:  SSORT, XERROR
C
C***END PROLOGUE
      DIMENSION XI(NPI), YI(NPI), FI(NPI), IWK(NIWK), WK(NWK),
     1 XO(NXO), YO(NYO), FO(NXO,NYO)
      DATA KERO/-1/
      IF (MODE.LT.1.OR.MODE.GT.2) GO TO 220
      KER = 0
C
C     ON INITIAL ENTRY MODE = 1, THE GRID LINES ARE SET UP,
C     LOCAL INTERPOLATION POINTS ARE DETERMINED AND LOCAL APPROXIMATIONS
C     ARE COMPUTED.
C
      IF (MODE.EQ.2) GO TO 140
      NXGWK = 1
      NPWK = 3
      IF (NPPR.LE.0) GO TO 100
      NXG = SQRT(4.*FLOAT(NPI)/FLOAT(NPPR))-.5
      NXG = MAX0(NXG,1)
      NYG = NXG
      IWK(1) = NXG
      IWK(2) = NYG
      GO TO 120
  100 NXG = IWK(1)
      NYG = IWK(2)
  120 IALWK = NXG+NYG+5
      IABWK = IALWK + 3*NXG*NYG
      NYGWK = NXG+3
      MPWK = NXG*NYG+4
C
      IF(NPPR.LE.0)GO TO 130
      CALL LOGRD(XI,NPI,NXG,WK(NXGWK),WK(IALWK))
      CALL LOGRD(YI,NPI,NYG,WK(NYGWK),WK(IALWK))
  130 CONTINUE
C
C     DETERMINE THE LOCAL INTERPOLATION POINTS FOR THE REGIONS.
      MWK = NWK - MPWK + 1
      CALL LOLIP (NXG,WK(NXGWK),NYG,WK(NYGWK),NPI,XI,YI,IWK(NPWK),
     1IWK(MPWK),MWK,NMAX,WK(IALWK))
      NCFM = IABWK +IWK(MPWK - 1)-1
      NWKU = NCFM + (NMAX+3)*(NMAX+5) - 1
      NIPVT = NXG*NYG+3+IWK(MPWK-1)
      NIWKU = NIPVT + NMAX + 2
      IF (NIWKU.GT.NIWK) GO TO 200
      IF (NWKU.GT.NWK) GO TO 200
C
C     COMPUTE THE LOCAL APPROXIMATIONS.
      CALL LOCAL (XI,YI,FI,NXG,WK(NXGWK),NYG,WK(NYGWK),IWK(NPWK),
     1 IWK(MPWK),WK(IALWK),WK(IABWK),WK(NCFM),IWK(NIPVT),IER)
      KERO = IER
      IF (IER.NE.0) GO TO 160
  140 IF (KERO.NE.0) GO TO 180
C
C     COMPUTE THE FUNCTION VALUES ON THE DESIRED GRID OF POINTS.
C
      CALL LOEVL (XI,YI,IWK(1),WK(NXGWK),IWK(2),WK(NYGWK),IWK(NPWK),
     1 IWK(MPWK),WK(IALWK),WK(IABWK),NXO,XO,NYO,YO,FO)
      RETURN
C
C     ERROR RETURNS
C
  160 KER = IER
      IF(IER.NE.0)CALL XERROR('LOTPS-SINGULAR MATRIX IN LOCAL; INCREAS
     1E NPPR OR SPECIFY OWN GRID LINES',71,1,2)
      RETURN
  180 KER = 3
      IF (KERO.LT.0) GO TO 190
       CALL XERROR('LOTPS-PREVIOUS ERROR FROM SUBROUTINE LOCAL HAS NOT
     1BEEN CORRECTED.',65,3,2)
      RETURN
  190 KER = 2
      CALL XERROR('LOTPS-FIRST CALL TO LOTPS MUST BE WITH MODE = 1',
     1 47,2,1)
      RETURN
  200 KER = 4
      CALL XERROR('LOTPS-WORK ARRAYS IWK AND/OR WK NOT DIMENSIONED LAR
     1GE ENOUGH',60,4,2)
      RETURN
  220 KER = 5
      CALL XERROR('LOTPS-MODE IS OUT OF RANGE.  MUST BE 1 OR 2',43,5,
     1 1)
      RETURN
      END


      SUBROUTINE LOCAL (XI,YI,FI,NXG,XG,NYG,YG,NP,MP,AL,AB,C,IP,IER)
C
C     THIS SUBROUTINE CONSTRUCTS THE LOCAL APPROXIMANTS FOR THE GRID
C     VERSION OF FRANKE'S METHOD.  THE LOCAL APPROXIMATIONS ARE TAKEN
C     TO BE THE THIN PLATE SPLINES DESCRIBED BY DUCHON AND OTHERS.
C
C     THE ARGUMENTS ARE AS FOLLOWS.
C
C        XI   - \
C        YI   - INPUT.  THE DATA POINTS (XI,YI,FI),I=1,NPI.
C        FI   - /
C        NXG  - INPUT.  THE NUMBER OF VERTICAL GRID LINES.
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN
C                       INCREASING ORDER.
C        NYG  - INPUT.  THE NUMBER OF HORIZONTAL GRID LINES.
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES, IN
C                       INCREASING ORDER.
C        NP   - INPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS ARE STORED.
C        MP   - INPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS.
C        AL   - OUTPUT.  THE COEFFICIENTS FOR THE LINEAR PART OF THE
C                       LOCAL THIN PLATE SPLINE FIT.
C        AB   - OUTPUT.  THE COEFFICIENTS FOR THE THIN PLATE SPLINES
C        C    - OUTPUT.  NOT MEANINGFUL.  THIS IS A SCRATCH ARRAY USED
C                       DURING CALCULATION OF THE LOCAL APPROXIMATIONS.
C        IP   - OUTPUT.  NOT MEANINGFUL.  THIS IS A SCRATCH ARRAY USED
C                       TO STORE PIVOT ORDER IN EQUATION SOLUTION.
C        IER  - OUTPUT.  RETURN INDICATOR.
C                 = 0,  NORMAL RETURN.
C                 = 1,  SINGULAR MATRIX HAS BEEN DETECTED IN THE
C                       THIN PLATE SPLINE FIT.
C
C     SUBROUTINES USED
C        LINPACK:  SGECO,SGESL
C        SLATEC:  XERROR
C
      DIMENSION XI(1), YI(1), FI(1), NP(1), MP(1), AL(1), AB(1), XG(1),
     1 YG(1),C(1),IP(1)
C
C     ARITHMETIC STATEMENT FUNCTION FOR THE THIN PLATE SPLINE BASIS
C     FUNCTIONS.
C
      PHI(X,Y,XP,YP) = ((X-XP)**2+(Y-YP)**2)*ALOG(((X-XP)**2+(Y-YP)**2)
     1 + 1.E-20)
      IER = 0
      IJ = 0
      DO 160 J=1,NYG
      DO 140 I=1,NXG
      IJ = IJ + 1
  140 CONTINUE
  160 CONTINUE
      IJ = 0
C
      DO 260 J=1,NYG
      DY = YG(J+2)-YG(J)
C
      DO 240 I=1,NXG
      DX = XG(I+2)-XG(I)
      IJ = IJ+1
      LEND = NP(IJ+1)-NP(IJ)
      LEND3 = LEND + 3
      IALS = (IJ-1)*3
C
      DO 200 LI=1,LEND
      MPI = NP(IJ)+LI-1
      KI = MP(MPI)
      XKI = (XI(KI)-XG(I))/DX
      YKI = (YI(KI)-YG(J))/DY
      LIJ = LEND*LEND3 + LI
      C(LIJ) = 1.
      C(LIJ+LEND3) = XKI
      C(LIJ+2*LEND3) = YKI
      LIJ = LEND3*(LI - 1) + LEND + 1
      C(LIJ) = 1.
      C(LIJ+1) = XKI
      C(LIJ+2) = YKI
      LIJL = LI
      LIJU = LEND3*(LI-1)+1
C
      DO 180 LJ=1,LI
      MPJ = NP(IJ)+LJ-1
      KJ = MP(MPJ)
      XKJ = (XI(KJ)-XG(I))/DX
      YKJ = (YI(KJ)-YG(J))/DY
      C(LIJL) = PHI(XKI,YKI,XKJ,YKJ)
      C(LIJU) = C(LIJL)
      LIJL = LIJL + LEND3
  180 LIJU = LIJU + 1
C
      LIJ = LEND3*LEND3 + LI
      C(LIJ) = FI(KI)
  200 CONTINUE
      DO 215 LLI=1,3
      LIJU = (LEND + LLI - 1)*LEND3 + LEND + 1
      LIJL = LEND*LEND3 + LEND + LLI
      DO 210 LLJ = 1,LLI
      C(LIJL) = 0.
      C(LIJU) = 0.
      LIJL = LIJL + LEND3
  210 LIJU = LIJU + 1
      LIJ = LEND3*LEND3 + LEND + LLI
      C(LIJ) = 0.
  215 CONTINUE
C
      LR = LEND3*LEND3 + 1
      LRR = LR + LEND3
  216    CONTINUE
      CALL SGECO(C,LEND3,LEND3,IP,RCOND,C(LRR))
      IF((.1*RCOND+1.).EQ.1.)GO TO 300
      CALL SGESL(C,LEND3,LEND3,IP,C(LR),0)
C
      DO 220 LI=1,LEND
      IAB = NP(IJ)+LI-1
      AB(IAB) = C(LR)
  220 LR = LR + 1
C
      AL(IALS+1) = C(LR)
      AL(IALS+2) = C(LR+1)
      AL(IALS+3) = C(LR+2)
  240 CONTINUE
C
  260 CONTINUE
C
      RETURN
C
C     ERROR RETURN
C
  300 IER = 1
      RETURN
      END

      SUBROUTINE LOEVL (XI,YI,NXG,XG,NYG,YG,NP,MP,AL,AB,NXO,XO,NYO
     1,YO,FO)
C
C     THIS SUBROUTINE EVALUATES THE INTERPOLANT FOR THE GRID VERSION OF
C     FRANKE'S METHOD.  THE FUNCTION IS EVALUATED AT THE GRID OF POINTS
C     INDICATED BY NXO, XO, NYO, YO, AND THESE VALUES ARE RETURNED
C     IN THE ARRAY FO, WHICH IS ASSUMED TO BE DIMENSIONED (NXO,NYO).
C
C     THE ARGUMENTS ARE AS FOLLOWS.
C
C        XI   - \
C        YI   - INPUT.  THE DATA POINTS (XI,YI,FI),I=1,...,NPI.
C        FI   - /
C        NXG  - INPUT.  THE NUMBER OF VERTICAL GRID LINES.
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN
C                       INCREASING ORDER.
C        NYG  - INPUT.  THE NUMBER OF HORIZONTAL GRID LINES.
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES,
C                       IN INCREASING ORDER.
C        NP   - INPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS ARE STORED.
C        MP   - INPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS.
C        AL   - INPUT.  THE COEFFICIENTS FOR THE LINEAR PART OF THE
C                       THIN PLATE SPLINE APPROXIMATIONS.
C        AB   - INPUT.  THE COEFFICIENTS FOR THE LOCAL THIN PLATE
C                       SPLINE APPROXIMATIONS.
C        NXO  - INPUT.  THE NUMBER OF XO VALUES AT WHICH THE INTERPO-
C                       LATION FUNCTION IS TO BE CALCULATED.
C        XO   - INPUT.  THE VALUES OF X AT WHICH THE INTERPOLATION
C                       FUNCTION IS TO BE CALCULATED.
C        NYO  - INPUT.  THE NUMBER OF YO VALUES AT WHICH THE INTERPO-
C                       LATION FUNCTION IS TO BE CALCULATED.
C        YO   - INPUT.  THE VALUES OF Y AT WHICH THE INTERPOLATION
C                       FUNCTION IS TO BE CALCULATED.
C        FO   - OUTPUT.  VALUES OF THE INTERPOLATION FUNCTION AT THE
C                       GRID POINTS INDICATED BY NXO, XO, NYO, YO.
C                       FO IS ASSUMED TO BE DIMENSIONED (NXO,NYO) IN THE
C                       CALLING PROGRAM.
C
      DIMENSION XG(1), YG(1), XI(1), YI(1), NP(1), MP(1), FC(4), AL(1),
     1AB(1), XO(1), YO(1), FO(NXO,1)
C
C     ARITHMETIC STATEMENT FUNCTION FOR THE HERMITE CUBIC.
C
      H3(S) = 1. - S**2*(3. - 2.*S)
C
C     ARITHMETIC STATEMENT FUNCTION FOR THE THIN PLATE SPLINE BASIS
C     FUNCTIONS.
C
      PHI(X,Y,XP,YP) = ((X-XP)**2+(Y-YP)**2)*ALOG(((X-XP)**2+(Y-YP)**2)
     1 + 1.E-20)
C
      J = 1
C
      DO 640 JO=1,NYO
C
C     DETERMINE THE LOCATION OF THE POINT YO IN TERMS OF THE SMALLEST
C     VALUE OF J SUCH THAT YO(JO) IS IN SOME RECTANGLE (I,J).
C
      YV = YO(JO)
      JJS = J+1
      IF (YV.LT.YG(JJS)) JJS=1
C
      DO 100 JJ=JJS,NYG
      IF (YV.LT.YG(JJ+1)) GO TO 120
  100 CONTINUE
C
      J = NYG
      GO TO 140
  120 J = JJ-1
  140 JD = 3
      IF (J.GE.1) GO TO 160
      JD = 0
      J = 1
      GO TO 180
  160 IF (J.LT.NYG) GO TO 180
      JD = 6
  180 DY = YG(J+2)-YG(J+1)
      I = 1
C
      DO 620 IO=1,NXO
C
C     DETERMINE THE LOCATION OF THE POINT XO IN TERMS OF THE SMALLEST
C     VALUE OF I SUCH THAT XO(IO) IS IN THE RECTANGLE (I,J).
C
      IIS = I+1
      XV = XO(IO)
      IF (XV.LT.XG(IIS)) IIS=1
C
      DO 200 II=IIS,NXG
      IF (XV.LT.XG(II+1)) GO TO 220
  200 CONTINUE
C
      I = NXG
      GO TO 240
  220 I = II-1
  240 ID = 2
      IF (I.GE.1) GO TO 260
      ID = 1
      I = 1
      GO TO 280
  260 IF (I.LT.NXG) GO TO 280
      ID = 3
  280 DX = XG(I+2)-XG(I+1)
      KD = ID+JD
      GO TO (300,360,300,440,520,440,300,360,300), KD
C
C     THIS IS FOR (XO(IO),YO(JO)) POINTS IN A SINGLE RECTANGLE (I,J)
C
  300 FV = 0.
      IJ = (J-1)*NXG+I
      IAL = 3*IJ-2
      LMAX = NP(IJ+1)-NP(IJ)
      DXA = XG(I+2)-XG(I)
      DYA = YG(J+2)-YG(J)
      XVD = (XV-XG(I))/DXA
      YVD = (YV-YG(J))/DYA
C
      DO 320 L=1,LMAX
      MPS = NP(IJ)+L-1
      KI = MP(MPS)
      XKI = (XI(KI)-XG(I))/DXA
      YKI = (YI(KI)-YG(J))/DYA
  320 FV = FV+AB(MPS)*PHI(XKI,YKI,XVD,YVD)
C
  340 FV = FV + AL(IAL) + AL(IAL+1)*XVD + AL(IAL+2)*YVD
      GO TO 620
C
C     THIS IS FOR XO(IO),YO(JO)) POINTS WHICH ARE IN TWO RECTANGLES,
C     (I,J) AND (I+1,J).
C
  360 DYA = YG(J+2)-YG(J)
      YVD = (YV-YG(J))/DYA
C
      DO 420 IP=1,2
      FC(IP) = 0.
      IS = I+IP-1
      IJ = (J-1)*NXG+IS
      IAL = 3*IJ-2
      DXA = XG(IS+2)-XG(IS)
      XVD = (XV-XG(IS))/DXA
      LMAX = NP(IJ+1)-NP(IJ)
C
      DO 380 L=1,LMAX
      MPS = NP(IJ)+L-1
      KI = MP(MPS)
      XKI = (XI(KI)-XG(IS))/DXA
      YKI = (YI(KI)-YG(J))/DYA
  380 FC(IP) = FC(IP)+AB(MPS)*PHI(XKI,YKI,XVD,YVD)
C
  400 FC(IP)=FC(IP)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD
  420 CONTINUE
C
      WI = H3((XV-XG(I+1))/DX)
      FV = FC(1)*WI+(1.-WI)*FC(2)
      GO TO 620
C
C     THIS IS FOR (XO(IO),YO(JO)) POINTS WHICH ARE IN TWO RECTANGLES,
C     (I,J) AND (I,J+1).
C
  440 DXA = XG(I+2)-XG(I)
      XVD = (XV-XG(I))/DXA
C
      DO 500 JP=1,2
      FC(JP) = 0.
      JS = J+JP-1
      IJ = (JS-1)*NXG+I
      IAL = 3*IJ-2
      DYA = YG(JS+2)-YG(JS)
      YVD = (YV-YG(JS))/DYA
      LMAX = NP(IJ+1)-NP(IJ)
C
      DO 460 L=1,LMAX
      MPS = NP(IJ)+L-1
      KJ = MP(MPS)
      XKJ = (XI(KJ)-XG(I))/DXA
      YKJ = (YI(KJ)-YG(JS))/DYA
  460 FC(JP) = FC(JP)+AB(MPS)*PHI(XKJ,YKJ,XVD,YVD)
C
  480 FC(JP)=FC(JP)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD
  500 CONTINUE
C
      UJ = H3((YV-YG(J+1))/DY)
      FV = FC(1)*UJ+(1.-UJ)*FC(2)
      GO TO 620
C
C     THIS IS FOR (XO(IO),YO(JO)) POINTS WHICH ARE IN FOUR RECTANGLES,
C     (I,J), (I+1,J), (I,J+1), AND (I+1,J+1).
C
  520 KFC = 0
C
      DO 600 JP=1,2
      JS = J+JP-1
      DYA = YG(JS+2)-YG(JS)
      YVD = (YV-YG(JS))/DYA
C
      DO 580 IP=1,2
      IS = I+IP-1
      IJ = (JS-1)*NXG+IS
      IAL = 3*IJ-2
      KFC = KFC+1
      FC(KFC) = 0.
      DXA = XG(IS+2)-XG(IS)
      XVD = (XV-XG(IS))/DXA
      LMAX = NP(IJ+1)-NP(IJ)
C
      DO 540 L=1,LMAX
      MPS = NP(IJ)+L-1
      KI = MP(MPS)
      XKI = (XI(KI)-XG(IS))/DXA
      YKI = (YI(KI)-YG(JS))/DYA
  540 FC(KFC) = FC(KFC)+AB(MPS)*PHI(XKI,YKI,XVD,YVD)
C
  560 FC(KFC)=FC(KFC)+AL(IAL)+AL(IAL+1)*XVD+AL(IAL+2)*YVD
  580 CONTINUE
C
  600 CONTINUE
C
      WI = H3((XV-XG(I+1))/DX)
      UJ = H3((YV-YG(J+1))/DY)
      FV = WI*(UJ*FC(1)+(1.-UJ)*FC(3))+(1.-WI)*(UJ*FC(2)+(1.-UJ)*FC(4))
  620 FO(IO,JO) = FV
C
  640 CONTINUE
C
      RETURN
      END

      SUBROUTINE LOLIP (NXG,XG,NYG,YG,NPI,XI,YI,NP,MP,MPM,NMAX,D)
C
C     THIS SUBROUTINE DETERMINES THE LOCAL INTERPOLATION POINTS FOR THE
C     GRID VERSION OF FRANKE'S METHOD OF SURFACE INTERPOLATION.
C     MINPTS POINTS ARE REQUIRED FOR EACH REGION.
C     IF FEWER THAN MINPTS POINTS ARE FOUND IN THE REGION, THE NEXT
C     CLOSEST POINTS (IN THE SUP NORM AFTER THE CURRENT RECTANGLE IS
C     TRANSFORMED ONTO (0,1)) ARE USED.  MINPTS IS SET TO 3, WHICH IS
C     THE RECOMMENDED VALUE, ALTHOUGH IT MAY BE ALTERED.
C
C     THE ARGUMENTS ARE AS FOLLOWS.
C
C        NXG  - INPUT.  NUMBER OF VERTICAL GRID LINES.
C        XG   - INPUT.  THE COORDINATES OF THE VERTICAL GRID LINES, IN
C                       INCREASING ORDER
C        NYG  - INPUT.  NUMBER OF HORIZONTAL GRID LINES.
C        YG   - INPUT.  THE COORDINATES OF THE HORIZONTAL GRID LINES,
C                       IN INCREASING ORDER.
C        NPI  - INPUT.  THE NUMBER OF DATA POINTS.
C        XI   - \
C        YI   - INPUT.  THE DATA POINTS (XI,YI), I=1,...,NPI.
C        FI   - /
C        NP   - OUTPUT.  AN ARRAY WHICH GIVES THE INITIAL SUBSCRIPT IN
C                       THE ARRAY MP AT WHICH THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS ARE STORED.
C        MP   - OUTPUT.  AN ARRAY WHICH GIVES THE SUBSCRIPTS FOR THE
C                       LOCAL INTERPOLATION POINTS.
C        MPM  - INPUT.  DIMENSION OF THE ARRAY MP IN THE CALLING PROGRAM
C        NMAX - OUTPUT.  THE MAXIMUM NUMBER OF INTERPOLATION POINTS
C                        OVER ALL THE REGIONS.
C        D    - A WORK ARRAY OF DIMENSION AT LEAST NPI.
C
      DIMENSION XG(1), YG(1), XI(1), YI(1), NP(1), MP(1), D(1)
      DATA MINPTS/3/
      IJ = 1
      NP(1) = 1
      NMAX = 0
      L = 0
C
      DO 200 J=1,NYG
      YGA = (YG(J+2)+YG(J))/2.
      DYG = YG(J+2)-YG(J)
C
      DO 180 I=1,NXG
      XGA = (XG(I+2)+XG(I))/2.
      DXG = XG(I+2)-XG(I)
      IJ = IJ+1
C
C     DETERMINE THE POINTS IN THE (I,J)TH RECTANGLE.
C
      DO 120 NK=1,NPI
      D(NK) = AMAX1(ABS(XI(NK) - XGA)/DXG,ABS(YI(NK) - YGA)/DYG)
      IF(D(NK).GT..6125)GO TO 120
      D(NK) = 1.E10
      L = L + 1
      LL = MIN0(L,MPM)
      MP(LL) = NK
  120 CONTINUE
C
      NP(IJ) = L+1
      IF (NP(IJ)-NP(IJ-1).GE.MINPTS) GO TO 180
C
C     ADD THE CLOSEST POINTS IF THERE ARE LESS THAN MINPTS IN THE
C     RECTANGLE.
C
      LM = MINPTS-(NP(IJ)-NP(IJ-1))
C
      DO 160 II=1,LM
      L = L+1
      LL = MIN0(L,MPM)
      MP(LL) = 1
      DM = D(1)
C
      DO 140 NK=2,NPI
      IF (D(NK).GE.DM) GO TO 140
      DM = D(NK)
      MP(LL) = NK
  140 CONTINUE
C
      NK = MP(LL)
  160 D(NK) = 1.E10
C
      NP(IJ) = L+1
  180 NMAX = MAX0(NMAX,NP(IJ)-NP(IJ-1))
C
  200 CONTINUE
C
      RETURN
      END


      SUBROUTINE LOGRD(X,N,NX,XG,T)
C     THIS SUBROUTINE PLACES A SET OF INTERVALS OVER THE SET OF POINTS
C     (X(I), I=1,...,N).  THIS IS DONE BY PLACING APPROXIMATELY EQUAL
C     NUMBERS OF THEM WITHIN EACH INTERVAL.
C
C     THE ARGUMENTS ARE AS FOLLOWS.
C
C        N   - INPUT.  THE NUMBER OF POINTS IN THE ARRAY X.
C        X   - INPUT.  THE ARRAY OF X POINTS.
C        NX  - INPUT.  THE DESIRED NUMBER OF INTERVALS.
C        XG  - OUTPUT.  THE COORDINATES OF THE INTERVAL ENDPOINTS.
C        T   - WORK ARRAY OF DIMENSION AT LEAST N.
C
C     SUBROUTINES USED
C        SLATEC:  SSORT
C
      DIMENSION X(1),XG(1),T(1)
C
      DO 100 I=1,N
  100 T(I) = X(I)
C
      CALL SSORT(T,T,N,1)
C
      FINC = FLOAT(N-1)/FLOAT(NX+1)
  120 DO 140 J=1,NX
      FK = J*FINC + 1.
      K = FK
      WK1 = FK - K
  140 XG(J+1) = (1. - WK1)*T(K) + WK1*T(K+1)
C
      XG(1) = T(1)
      XG(NX+2) = T(N)
C
      RETURN
      END

      SUBROUTINE XERROR(MESSG,NMESSG,NERR,LEVEL)
      CHARACTER*(*) MESSG
	print*,MESSG
      RETURN
      END

      SUBROUTINE SGECO(A,LDA,N,IPVT,RCOND,Z)
C***BEGIN PROLOGUE  SGECO
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  CONDITION,FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Factors a real matrix by Gaussian elimination and estimates
C            the condition number of the matrix.
C***DESCRIPTION
C
C     SGECO factors a real matrix by Gaussian elimination
C     and estimates the condition of the matrix.
C
C     If  RCOND  is not needed, SGEFA is slightly faster.
C     To solve  A*X = B , follow SGECO by SGESL.
C     To compute  INVERSE(A)*C , follow SGECO by SGESL.
C     To compute  DETERMINANT(A) , follow SGECO by SGEDI.
C     To compute  INVERSE(A) , follow SGECO by SGEDI.
C
C     On Entry
C
C        A       REAL(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U , where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        RCOND   REAL
C                an estimate of the reciprocal condition of  A .
C                For the system  A*X = B , relative perturbations
C                in  A  and  B  of size  EPSILON  may cause
C                relative perturbations in  X  of size  EPSILON/RCOND .
C                If  RCOND  is so small that the logical expression
C                           1.0 + RCOND .EQ. 1.0
C                is true, then  A  may be singular to working
C                precision.  In particular,  RCOND  is zero  if
C                exact singularity is detected or the estimate
C                underflows.
C
C        Z       REAL(N)
C                a work vector whose contents are usually unimportant.
C                If  A  is close to a singular matrix, then  Z  is
C                an approximate null vector in the sense that
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     LINPACK SGEFA
C     BLAS SAXPY,SDOT,SSCAL,SASUM
C     Fortran ABS,AMAX1,SIGN
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  SASUM,SAXPY,SDOT,SGEFA,SSCAL
C***END PROLOGUE  SGECO
      INTEGER LDA,N,IPVT(1)
      REAL A(LDA,1),Z(1)
      REAL RCOND
C
      REAL SDOT,EK,T,WK,WKM
      REAL ANORM,S,SASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
C
C     COMPUTE 1-NORM OF A
C
C***FIRST EXECUTABLE STATEMENT  SGECO
      ANORM = 0.0E0
      DO 10 J = 1, N
         ANORM = AMAX1(ANORM,SASUM(N,A(1,J),1))
   10 CONTINUE
C
C     FACTOR
C
      CALL SGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = 1.0E0
      DO 20 J = 1, N
         Z(J) = 0.0E0
   20 CONTINUE
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0E0) EK = SIGN(EK,-Z(K))
         IF (ABS(EK-Z(K)) .LE. ABS(A(K,K))) GO TO 30
            S = ABS(A(K,K))/ABS(EK-Z(K))
            CALL SSCAL(N,S,Z,1)
            EK = S*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = ABS(WK)
         SM = ABS(WKM)
         IF (A(K,K) .EQ. 0.0E0) GO TO 40
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         GO TO 50
   40    CONTINUE
            WK = 1.0E0
            WKM = 1.0E0
   50    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GO TO 90
            DO 60 J = KP1, N
               SM = SM + ABS(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + ABS(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*A(K,J)
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
C
C     SOLVE TRANS(L)*Y = W
C
      DO 120 KB = 1, N
         K = N + 1 - KB
         IF (K .LT. N) Z(K) = Z(K) + SDOT(N-K,A(K+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0E0) GO TO 110
            S = 1.0E0/ABS(Z(K))
            CALL SSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
C
      YNORM = 1.0E0
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL SAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0E0) GO TO 130
            S = 1.0E0/ABS(Z(K))
            CALL SSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (ABS(Z(K)) .LE. ABS(A(K,K))) GO TO 150
            S = ABS(A(K,K))/ABS(Z(K))
            CALL SSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (A(K,K) .NE. 0.0E0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0E0) Z(K) = 1.0E0
         T = -Z(K)
         CALL SAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
      RETURN
      END


      SUBROUTINE SGESL(A,LDA,N,IPVT,B,JOB)
C***BEGIN PROLOGUE  SGESL
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  LINEAR ALGEBRA,LINPACK,MATRIX,SOLVE
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Solves the real system A*X=B or TRANS(A)*X=B
C            using the factors of SGECO or SGEFA
C***DESCRIPTION
C
C     SGESL solves the real system
C     A * X = B  or  TRANS(A) * X = B
C     using the factors computed by SGECO or SGEFA.
C
C     On Entry
C
C        A       REAL(LDA, N)
C                the output from SGECO or SGEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from SGECO or SGEFA.
C
C        B       REAL(N)
C                the right hand side vector.
C
C        JOB     INTEGER
C                = 0         to solve  A*X = B ,
C                = nonzero   to solve  TRANS(A)*X = B  where
C                            TRANS(A)  is the transpose.
C
C     On Return
C
C        B       the solution vector  X .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains a
C        zero on the diagonal.  Technically, this indicates singularity,
C        but it is often caused by improper arguments or improper
C        setting of LDA .  It will not occur if the subroutines are
C        called correctly and if SGECO has set RCOND .GT. 0.0
C        or SGEFA has set INFO .EQ. 0 .
C
C     To compute  INVERSE(A) * C  where  C  is a matrix
C     with  P  columns
C           CALL SGECO(A,LDA,N,IPVT,RCOND,Z)
C           IF (RCOND is too small) GO TO ...
C           DO 10 J = 1, P
C              CALL SGESL(A,LDA,N,IPVT,C(1,J),0)
C        10 CONTINUE
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS SAXPY,SDOT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  SAXPY,SDOT
C***END PROLOGUE  SGESL
      INTEGER LDA,N,IPVT(1),JOB
      REAL A(LDA,1),B(1)
C
      REAL SDOT,T
      INTEGER K,KB,L,NM1
C***FIRST EXECUTABLE STATEMENT  SGESL
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
C
C        JOB = 0 , SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B
C
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL SAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL SAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
C
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
C        FIRST SOLVE  TRANS(U)*Y = B
C
         DO 60 K = 1, N
            T = SDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
C
C        NOW SOLVE TRANS(L)*X = Y
C
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + SDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END


      SUBROUTINE SSORT(X,Y,N,KFLAG)
C***BEGIN PROLOGUE  SSORT
C***DATE WRITTEN   761101   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  N6A2B1
C***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  SSORT sorts array X and optionally makes the same
C            interchanges in array Y.  The array X may be sorted in
C            increasing order or decreasing order.  A slightly modified
C            QUICKSORT algorithm is used.
C***DESCRIPTION
C
C     Written by Rondall E. Jones
C     Modified by John A. Wisniewski to use the Singleton quicksort
C     algorithm.  Date 18 November 1976.
C
C     Abstract
C         SSORT sorts array X and optionally makes the same
C         interchanges in array Y.  The array X may be sorted in
C         increasing order or decreasing order.  A slightly modified
C         quicksort algorithm is used.
C
C     Reference
C         Singleton, R. C., Algorithm 347, An Efficient Algorithm for
C         Sorting with Minimal Storage, CACM,12(3),1969,185-7.
C
C     Description of Parameters
C         X - array of values to be sorted   (usually abscissas)
C         Y - array to be (optionally) carried along
C         N - number of values in array X to be sorted
C         KFLAG - control parameter
C             =2  means sort X in increasing order and carry Y along.
C             =1  means sort X in increasing order (ignoring Y)
C             =-1 means sort X in decreasing order (ignoring Y)
C             =-2 means sort X in decreasing order and carry Y along.
C***REFERENCES  SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM
C                 FOR SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,
C                 185-7.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  SSORT
      DIMENSION X(N),Y(N),IL(21),IU(21)
C***FIRST EXECUTABLE STATEMENT  SSORT
      NN = N
      IF (NN.GE.1) GO TO 10
      CALL XERROR ( 'SSORT- THE NUMBER OF VALUES TO BE SORTED WAS NOT PO
     1SITIVE.',58,1,1)
      RETURN
   10 KK = IABS(KFLAG)
      IF ((KK.EQ.1).OR.(KK.EQ.2)) GO TO 15
      CALL XERROR ( 'SSORT- THE SORT CONTROL PARAMETER, K, WAS NOT 2, 1,
     1 -1, OR -2.',62,2,1)
      RETURN
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 IF (KFLAG.GE.1) GO TO 30
      DO 20 I=1,NN
   20 X(I) = -X(I)
   30 GO TO (100,200),KK
C
C SORT X ONLY
C
  100 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
  115 IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
C                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 135
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
C
C SORT X AND CARRY Y ALONG
C
  200 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  210 IF (I .EQ. J) GO TO 255
  215 IF (R .GT. .5898437) GO TO 220
      R=R+3.90625E-2
      GO TO 225
  220 R=R-.21875
  225 K=I
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      IJ = I + IFIX (FLOAT (J-I) *R)
      T=X(IJ)
      TY= Y(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 230
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
  230 L=J
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 240
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
       Y(IJ)= Y(J)
       Y(J)=TY
      TY= Y(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 240
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
      GO TO 240
  235 TT=X(L)
      X(L)=X(K)
      X(K)=TT
      TTY= Y(L)
       Y(L)= Y(K)
       Y(K)=TTY
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  240 L=L-1
      IF (X(L) .GT. T) GO TO 240
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
C                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 235
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 250
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 260
  250 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  260 IF (J-I .GE. 1) GO TO 225
      IF (I .EQ. 1) GO TO 210
      I=I-1
  265 I=I+1
      IF (I .EQ. J) GO TO 255
      T=X(I+1)
      TY= Y(I+1)
      IF (X(I) .LE. T) GO TO 265
      K=I
  270 X(K+1)=X(K)
       Y(K+1)= Y(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 270
      X(K+1)=T
       Y(K+1)=TY
      GO TO 265
C
C CLEAN UP
C
  300 IF (KFLAG.GE.1) RETURN
      DO 310 I=1,NN
  310 X(I) = -X(I)
      RETURN
      END

      REAL FUNCTION SASUM(N,SX,INCX)
C***BEGIN PROLOGUE  SASUM
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A3A
C***KEYWORDS  ADD,BLAS,LINEAR ALGEBRA,MAGNITUDE,SUM,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Sum of magnitudes of s.p vector components
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(S)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C    SASUM  single precision result (zero if N .LE. 0)
C
C     Returns sum of magnitudes of single precision SX.
C     SASUM = sum from 0 to N-1 of  ABS(SX(1+I*INCX))
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  SASUM
C
      REAL SX(1)
C***FIRST EXECUTABLE STATEMENT  SASUM
      SASUM = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I=1,NS,INCX
          SASUM = SASUM + ABS(SX(I))
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SASUM = SASUM + ABS(SX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        SASUM = SASUM + ABS(SX(I)) + ABS(SX(I + 1)) + ABS(SX(I + 2))
     1  + ABS(SX(I + 3)) + ABS(SX(I + 4)) + ABS(SX(I + 5))
   50 CONTINUE
      RETURN
      END


      SUBROUTINE SGEFA(A,LDA,N,IPVT,INFO)
C***BEGIN PROLOGUE  SGEFA
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Factors a real matrix by Gaussian elimination.
C***DESCRIPTION
C
C     SGEFA factors a real matrix by Gaussian elimination.
C
C     SGEFA is usually called by SGECO, but it can be called
C     directly with a saving in time if  RCOND  is not needed.
C     (Time for SGECO) = (1 + 9/N)*(Time for SGEFA) .
C
C     On Entry
C
C        A       REAL(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U , where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that SGESL or SGEDI will divide by zero
C                     if called.  Use  RCOND  in SGECO for a reliable
C                     indication of singularity.
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS SAXPY,SSCAL,ISAMAX
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  ISAMAX,SAXPY,SSCAL
C***END PROLOGUE  SGEFA
      INTEGER LDA,N,IPVT(1),INFO
      REAL A(LDA,1)
C
      REAL T
      INTEGER ISAMAX,J,K,KP1,L,NM1
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
C***FIRST EXECUTABLE STATEMENT  SGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = ISAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0E0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0E0/A(K,K)
            CALL SSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0E0) INFO = N
      RETURN
      END

      SUBROUTINE SSCAL(N,SA,SX,INCX)
C***BEGIN PROLOGUE  SSCAL
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A6
C***KEYWORDS  BLAS,LINEAR ALGEBRA,SCALE,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  S.P. vector scale x = a*x
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SA  single precision scale factor
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C       SX  single precision result (unchanged if N .LE. 0)
C
C     Replace single precision SX by single precision SA*SX.
C     For I = 0 to N-1, replace SX(1+I*INCX) with  SA * SX(1+I*INCX)
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  SSCAL
C
      REAL SA,SX(1)
C***FIRST EXECUTABLE STATEMENT  SSCAL
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I = 1,NS,INCX
          SX(I) = SA*SX(I)
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END

      REAL FUNCTION SDOT(N,SX,INCX,SY,INCY)
C***BEGIN PROLOGUE  SDOT
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A4
C***KEYWORDS  BLAS,INNER PRODUCT,LINEAR ALGEBRA,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  S.P. inner product of s.p. vectors
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C     SDOT  single precision dot product (zero if N .LE. 0)
C
C     Returns the dot product of single precision SX and SY.
C     SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  SDOT
C
      REAL SX(1),SY(1)
C***FIRST EXECUTABLE STATEMENT  SDOT
      SDOT = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1)5,20,60
    5 CONTINUE
C
C        CODE FOR UNEQUAL INCREMENTS OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SDOT = SDOT + SX(IX)*SY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SDOT = SDOT + SX(I)*SY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SDOT = SDOT + SX(I)*SY(I) + SX(I + 1)*SY(I + 1) +
     1   SX(I + 2)*SY(I + 2) + SX(I + 3)*SY(I + 3) + SX(I + 4)*SY(I + 4)
   50 CONTINUE
      RETURN
C
C        CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C
   60 CONTINUE
      NS=N*INCX
      DO 70 I=1,NS,INCX
        SDOT = SDOT + SX(I)*SY(I)
   70   CONTINUE
      RETURN
      END

      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
C***BEGIN PROLOGUE  SAXPY
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A7
C***KEYWORDS  BLAS,LINEAR ALGEBRA,TRIAD,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  S.P. computation y = a*x + y
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SA  single precision scalar multiplier
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C       SY  single precision result (unchanged if N .LE. 0)
C
C     Overwrite single precision SY with single precision SA*SX +SY.
C     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) +
C       SY(LY+I*INCY), where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N
C       and LY is defined in a similar way using INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  SAXPY
C
      REAL SX(1),SY(1),SA
C***FIRST EXECUTABLE STATEMENT  SAXPY
      IF(N.LE.0.OR.SA.EQ.0.E0) RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SY(I) + SA*SX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        SY(I) = SY(I) + SA*SX(I)
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
   50 CONTINUE
      RETURN
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          SY(I) = SA*SX(I) + SY(I)
   70     CONTINUE
      RETURN
      END

      INTEGER FUNCTION ISAMAX(N,SX,INCX)
C***BEGIN PROLOGUE  ISAMAX
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A2
C***KEYWORDS  BLAS,LINEAR ALGEBRA,MAXIMUM COMPONENT,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Find largest component of s.p. vector
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C   ISAMAX  smallest index (zero if N .LE. 0)
C
C     Find smallest index of maximum magnitude of single precision SX.
C     ISAMAX =  first I, I = 1 to N, to minimize  ABS(SX(1-INCX+I*INCX)
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ISAMAX
C
      REAL SX(1),SMAX,XMAG
C***FIRST EXECUTABLE STATEMENT  ISAMAX
      ISAMAX = 0
      IF(N.LE.0) RETURN
      ISAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      SMAX = ABS(SX(1))
      NS = N*INCX
      II = 1
          DO 10 I=1,NS,INCX
          XMAG = ABS(SX(I))
          IF(XMAG.LE.SMAX) GO TO 5
          ISAMAX = II
          SMAX = XMAG
    5     II = II + 1
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         XMAG = ABS(SX(I))
         IF(XMAG.LE.SMAX) GO TO 30
         ISAMAX = I
         SMAX = XMAG
   30 CONTINUE
      RETURN
      END


