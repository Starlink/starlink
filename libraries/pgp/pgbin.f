C*PGBIN -- histogram of binned data
C%void cpgbin(int nbin, const float *x, const float *data, \
C% Logical center);
C+
      SUBROUTINE PGBIN (NBIN, X, DATA, CENTER)
      INTEGER NBIN
      REAL X(*), DATA(*)
      LOGICAL CENTER
C
C Plot a histogram of NBIN values with X(1..NBIN) values along
C the ordinate, and DATA(1...NBIN) along the abscissa. Bin width is
C spacing between X values.
C
C Arguments:
C  NBIN   (input)  : number of values.
C  X      (input)  : abscissae of bins.
C  DATA   (input)  : data values of bins.
C  CENTER (input)  : if .TRUE., the X values denote the center of the
C                    bin; if .FALSE., the X values denote the lower
C                    edge (in X) of the bin.
C--
C 19-Aug-92: change argument check (TJP).
C-----------------------------------------------------------------------
      LOGICAL  PGNOTO
      INTEGER  IBIN
      REAL     TX(4), TY(4)
C
C Check arguments.
C
      IF (NBIN.LT.2) RETURN
      IF (PGNOTO('PGBIN')) RETURN
      CALL PGBBUF
C
C Draw Histogram. Centered an uncentered bins are treated separately.
C
      IF (CENTER) THEN
C         !set up initial point.
          TX(2) = (3.*X(1) - X(2))/2.
          TY(2) = DATA(1)
          TX(3) = (X(1) + X(2))/2.
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 2, TX(2), TY(2))
C         !draw initial horizontal line
C         !now loop over bins
          DO 10 IBIN=2,NBIN-1
              TX(1) = TX(3)
              TX(2) = TX(1)
              TX(3) = ( X(IBIN) + X(IBIN+1) ) / 2.
              TY(1) = TY(3)
              TY(2) = DATA(IBIN)
              TY(3) = TY(2)
              CALL GRVCT0(2, .FALSE., 3, TX, TY)
   10     CONTINUE
C         !now draw last segment.
          TX(1) = TX(3)
          TX(2) = TX(1)
          TX(3) = (3.*X(NBIN) - X(NBIN-1) )/2.
          TY(1) = TY(3)
          TY(2) = DATA(NBIN)
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 3, TX, TY)
C
C               Uncentered bins
C
      ELSE
C         !set up first line.
          TX(2) = X(1)
          TY(2) = DATA(1)
          TX(3) = X(2)
          TY(3) = TY(2)
          CALL GRVCT0(2, .FALSE., 2, TX(2), TY(2))
          DO 20 IBIN=2,NBIN
              TX(1) = TX(3)
              TX(2) = TX(1)
              IF (IBIN.EQ.NBIN) THEN
                  TX(3) = 2.*X(NBIN) - X(NBIN-1)
              ELSE
                  TX(3) = X(IBIN+1)
              END IF
              TY(1) = TY(3)
C             !get height for last segment.
              TY(2) = DATA(IBIN)
              TY(3) = TY(2)
              CALL GRVCT0(2, .FALSE., 3, TX, TY)
   20     CONTINUE
      END IF
C
      CALL PGEBUF
      END
