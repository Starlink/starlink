      SUBROUTINE GRGRAY(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                  MINCI, MAXCI, MODE)
*+
*
*     - - - - - - - -
*       G R G R A Y     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Load a grey scale and plots a cell array
*
*   Given
*      A        r()   Data array
*      IDIM     i     First dimension of A
*      JDIM     i     Second dimension of A
*      I1       i     Array subset
*      I2       i       "     "
*      J1       i       "     "
*      J2       i       "     "
*      FOREGR   r     Data value to map to foreground colour
*      BACKGR   r     Data value to map to background colour
*      MINCI    i     Lowest colour index to use
*      MAXCI    i     Highest colour index to use
*      TR       r()   transformation from array indices to world coordinates
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRGRLO   l()   Grey scale loaded flag
*      GRVPVI   l()   Viewport visible
*
*   Written to COMMON
*      GRGRLO   l()   Grey scale loaded flag
*
*   Constants from GKS_PAR
*      GREALI   i     Realized value
*
*   D.L.Terrett  Starlink  Aug 1987
*       Fixed calculation of segment transformation
*         "        "       " world coordinates for no-rotation case
*         "   drawing of cell array outline
*         "   transformation in no rotation case
*   D.L.Terrett  Starlink  May 1988
*       Use GQSGA instead of GQASWK because of GKS bug
*       Change buffer size to avoid GKS cell array bug
*   D.L.Terrett  Starlink Jun 1988
*       Simulate with random dotting on non grey-scale devices
*   D.L.Terrett  Starlink May 1990
*       Increase buffer size
*   D.L.Terrett  Starlink Dec 1990
*       Remove redundant code
*       Attempt to work around stripes problems
*   D.L.Terrett  Starlink Feb 1991
*       Don't use segments for flipped case
*   D.L.Terrett  Starlink Oct 1991
*       Allocate dynamic workspace with PSX and nail the strips problem at
*       last
*   D.L.Terrett  Starlink Jan 1992
*       Plot cell arrays the correct way up
*   D.L.Terrett  Starlink Apr 1999
*       Correct rounding problems with colour table generation
*+
      IMPLICIT NONE

      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      INTEGER MINCI, MAXCI
      REAL A(IDIM,JDIM), BACKGR, FOREGR
      REAL TR(6)

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      REAL C0(3), C1(3), C3(3)
      INTEGER I, J, IERR

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRGRAY - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (GRVPVI(GRCIDE)) THEN
            IF (MINCI.GE.MAXCI) THEN

*      Not enough colour table to a grey scale - simulate
               CALL GRGRA1(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                     MODE)

            ELSE

*      Load grey scale if it hasn't been done yet
               IF (.NOT.GRGRLO(GRCIDE)) THEN
                  CALL GQCR(GRWKID(GRCIDE),0,GREALI,IERR,C0(1),C0(2),
     1               C0(3))
                  IF (IERR.NE.0) THEN
                     CALL GRQREP('GRGRAY', 'GQCR', IERR)
                     GO TO 9999
                  END IF
                  CALL GQCR(GRWKID(GRCIDE),1,GREALI,IERR,C1(1),C1(2),
     1               C1(3))
                  IF (IERR.NE.0) THEN
                     CALL GRQREP('GRGRAY', 'GQCR', IERR)
                     GO TO 9999
                  END IF

                  CALL GSCR(GRWKID(GRCIDE),MINCI,C0(1),C0(2),C0(3))
                  CALL GSCR(GRWKID(GRCIDE),MAXCI,C1(1),C1(2),C1(3))
                  DO 20 I = MINCI+1,MAXCI-1
                      DO 10 J = 1,3
                         C3(J) = C0(J) + (C1(J)-C0(J)) *
     :                           REAL(I-MINCI)/
     :                           REAL(MAXCI-MINCI)
   10                 CONTINUE
                      CALL GSCR(GRWKID(GRCIDE),I,C3(1),C3(2),C3(3))
   20             CONTINUE
                  GRGRLO(GRCIDE) = .TRUE.
               END IF

*           Plot the cell array.
               CALL GRGRA2(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                     MINCI, MAXCI, MODE)
            ENDIF
         ENDIF
      ENDIF

 9999 CONTINUE
      END
