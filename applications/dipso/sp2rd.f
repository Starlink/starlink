*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE SP2RD
*
*   Reads data in from 'SPECTRUM' format 2 files
*
*   IMPORTS:
*       ASIZE1       (INTEGER) Maximum size of FLUX, WAVE arrays
*       MAXBRK       (INTEGER) Maximum size of BREAK array
*       IUNIT        (INTEGER) (Already opened) read unit
*
*   EXPORTS:
*       WAVE         (REAL)    Array of X values
*       FLUX         (REAL)    Array of Y values
*       NPOINT       (INTEGER) No. of X,Y pairs
*       BREAK        (INTEGER) Array of 'break' points
*       NBREAK       (INTEGER) No. of 'break' points
*       TITLE        (CHAR)    Associated title
*       WORV         (REAL)    Wavelength OR Velocity
*       SUBCHK       (LOGICAL) TRUE on successful completion
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE SP2RD(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :                  TITLE,WORV,IUNIT,SUBCHK)
*
*
*
       IMPLICIT NONE
*
*
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER IUNIT
       INTEGER BREAK(MAXBRK)

       REAL WAVE(ASIZE1), FLUX(ASIZE1), WORV
       CHARACTER*(*) TITLE
       LOGICAL SUBCHK
*
*
       INTEGER I, J, IX, ICHK
       CHARACTER*80 IHHEAD
*
*
       SUBCHK = .TRUE.
       WORV = 1.0

       READ (IUNIT,'(A79)',ERR=200) TITLE(1:79)
       WRITE (*,'(''   SP2RD - title:  '',A50)') TITLE(1:50)
       READ (IUNIT,'(A79)',ERR=200) IHHEAD(1:79)
       READ (IUNIT,*,ERR=200) NPOINT
       IF (NPOINT.GE.ASIZE1) THEN
          NPOINT = ASIZE1 - 1
          WRITE (*,
     :    '(''   SP2RD:  too many points, only'',I5,'' read in'')')
     :    NPOINT
       ENDIF
       READ (IUNIT,*,ERR=200) (WAVE(I),FLUX(I),I=1,NPOINT)
       NPOINT = NPOINT + 1
       NPOINT = MIN(ASIZE1,NPOINT)
       NPOINT = MAX(1,NPOINT)
       FLUX(NPOINT) = 0.0
*
       NBREAK = 0
       I = 0
       ICHK = 1
*
       DO 100 J = 1, NPOINT
          IF (NBREAK.LT.MAXBRK) THEN
             IF (FLUX(J).NE.0.0) THEN
                I = I + 1
                WAVE(I) = WAVE(J)
                FLUX(I) = FLUX(J)
                ICHK = 1
             ELSEIF (I.NE.0) THEN
                IF (ICHK.NE.0) THEN
                   ICHK = 0
                   NBREAK = NBREAK + 1
                   BREAK(NBREAK) = I
                   IF (NBREAK.EQ.MAXBRK) THEN
                      WRITE (*,
     :                '(''   SP2RD:  maximu BRKSZE hit'')')
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
  100  CONTINUE
       NPOINT = I
*
       CLOSE (IUNIT)

       GOTO 300
  200  CONTINUE
       WRITE (*,'(''   SP2RD:  error reading from file'')')
       CLOSE (IUNIT)
       SUBCHK = .FALSE.

  300  CONTINUE

       END
