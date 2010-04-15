*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE OSP0RD
*
*   Reads data in from 'SPECTRUM' format 0 files
*   (LJACK=.TRUE. fudges override allowing data
*   with y vals of zero)
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
*   History:
*      30/8/94 - Name changed from SP0RD to OSP0RD (D. Berry)
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE OSP0RD(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :                  TITLE,WORV,IUNIT,LJACK,SUBCHK)
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
       LOGICAL SUBCHK, LJACK
*
*
       INTEGER I, J, IX, ICHK
       CHARACTER*80 IHHEAD
*
*
       SUBCHK = .TRUE.
       WORV = 1.0

       READ (IUNIT,IOSTAT=IX) TITLE(1:79)
       WRITE (*,'(''   OSP0RD - title:  '',A50)') TITLE(1:50)
       IF (IX.NE.0 .AND. IX.NE.67) THEN
          WRITE (*,'(''   OSP0RD:  error on read'')')
          CLOSE (IUNIT)
          SUBCHK = .FALSE.
          GOTO 300
       ENDIF
       READ (IUNIT,IOSTAT=IX) IHHEAD(1:79)
       IF (IX.NE.0 .AND. IX.NE.67) THEN
          WRITE (*,'(''   OSP0RD:  error on read'')')
          CLOSE (IUNIT)
          SUBCHK = .FALSE.
          GOTO 300
       ENDIF
       READ (IUNIT,ERR=200) NPOINT
       IF (NPOINT.EQ.0) THEN
          WRITE (*,'(''   OSP0RD:  file has no points'')')
          CLOSE (IUNIT)
          SUBCHK = .FALSE.
          GOTO 300
       ENDIF
       IF (NPOINT.GE.ASIZE1) THEN
          NPOINT = ASIZE1 - 1
          WRITE (*,
     :    '(''   OSP0RD:  too many points, only'',I5,'' read in'')')
     :    NPOINT
       ENDIF
       READ (IUNIT,ERR=200) (WAVE(I),FLUX(I),I=1,NPOINT)
       NPOINT = NPOINT + 1
       IF (LJACK) THEN
          NBREAK = 1
          BREAK(NBREAK) = NPOINT
          WRITE (*,99001)
          CLOSE (IUNIT)
          GOTO 300
       ENDIF
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
     :                '(''   OSP0RD:  maximum BRKSZE hit'')')
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
       WRITE (*,'(''   OSP0RD:  error reading from file'')')
       CLOSE (IUNIT)
       SUBCHK = .FALSE.
  300  CONTINUE
99001  FORMAT ('   Don''t PANIC!')
       END
