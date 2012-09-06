      PROGRAM SPEED

*+
*
*  Demonstration of different plotting style/speed
*  tradeoffs in NCAR/SGS/GKS
*
*  P T Wallace   Starlink   10 June 1987
*
*+

      IMPLICIT NONE

      INTEGER NPMAX
      PARAMETER (NPMAX=10000)
      INTEGER N,NP,IPREC,NCOUNT
      REAL RNULL1,TICK
      CHARACTER FNAME*80,GLAB*80,XLAB*80,YLAB*80,K*1
      REAL X(NPMAX),Y(NPMAX)
      LOGICAL PLOTED,NTBAD



*  Open input file
      PRINT *,'Filename?'
      READ (*,'(A)') FNAME
      N = LEN( FNAME )
      OPEN (UNIT=1,STATUS='OLD',
#if HAVE_FC_OPEN_ACTIONREAD
     :     ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :     READONLY,
#endif
     :     FILE=FNAME(:N))

*  Read label text
      READ (1,'(A)',END=9000) GLAB
      READ (1,'(A)',END=9000) XLAB
      READ (1,'(A)',END=9000) YLAB

*  Read x,y data
      DO NP=1,NPMAX
         READ (1,*,END=100) X(NP),Y(NP)
      END DO
      NP = NPMAX+1

*  Adjust number of points
 100  CONTINUE
      NP = NP-1

*  Check enough data
      IF (NP.LT.2) GO TO 9000

*  Prepare to plot the graph
      CALL snx_AGOP
      CALL AGGETF('NULL/1.',RNULL1)
      PLOTED = .FALSE.

*  Select character precision and font
 200  CONTINUE
      PRINT *,'Precision/font?  H=h/w, S=GKS, N=NCAR, E=exit'
      READ (*,'(A)') K
      NTBAD = .FALSE.
      IF (K.EQ.'H'.OR.K.EQ.'h') THEN

*     Hardware characters - fast but tacky
         CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
         IPREC = 0
         NCOUNT = 1
         TICK = 1.0

      ELSE IF (K.EQ.'S'.OR.K.EQ.'s') THEN

*     GKS software characters - reasonably fast and attractive
         CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
         IPREC = 2
         NCOUNT = 6
         TICK = RNULL1

      ELSE IF (K.EQ.'N'.OR.K.EQ.'n') THEN

*     PWRITX characters - fancy but slow
         CALL AGPWRT(0.0,0.0,' ',0,0,0,100)
         IPREC = 2
         NCOUNT = 6
         TICK = RNULL1

      ELSE IF (K.EQ.'E'.OR.K.EQ.'e') THEN

*     Exit requested - wrap up
         CALL sgs_CLOSE
         GO TO 9999

      ELSE

*     Unrecognised command
         NTBAD = .TRUE.
         PRINT *,'?'

      END IF

*  Repeat prompt if problems
      IF (NTBAD) GO TO 200

*  Clear the zone if necessary
      IF (PLOTED) CALL sgs_CLRZ

*
*  Setup for plotting:
*
*  SGS/GKS text precision
*      CALL sgs_SPREC(IPREC)
*  Density of tick marks and numeric labels
      CALL AGSETI('LEFT/MAJOR/COUNT.',NCOUNT)
      CALL AGSETI('RIGHT/MAJOR/COUNT.',NCOUNT)
      CALL AGSETI('BOTTOM/MAJOR/COUNT.',NCOUNT)
      CALL AGSETI('TOP/MAJOR/COUNT.',NCOUNT)
      CALL AGSETF('LEFT/MINOR/SPACING.',TICK)
      CALL AGSETF('RIGHT/MINOR/SPACING.',TICK)
      CALL AGSETF('BOTTOM/MINOR/SPACING.',TICK)
      CALL AGSETF('TOP/MINOR/SPACING.',TICK)

*  Plot the graph
      CALL snx_EZRXY(X,Y,NP,XLAB,YLAB,GLAB)
      CALL PLOTIT(0,0,2)
      CALL sgs_FLUSH

      PLOTED = .TRUE.

*  Next plot
      GO TO 200

*  Exits
 9000 CONTINUE
      PRINT *,'Insufficient data!'

 9999 CONTINUE

      END
