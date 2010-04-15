*  History:
*     25 Nov 1993 (hme):
*        Attempt to disuse IPUT_SCREEN.
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Don't split strings across lines
*        Unused XBUF
C-----------------------------------------------------------------------

      SUBROUTINE GETPTS (IPTS, NMIN, NMAX, NOLD, NR,
     &                   XSCALE, BUF, NCH, IFAIL)

C   Routine to get up to NMAX/2 point-pairs from terminal.

C     Formal parameters:
C                    IPTS    -   Points defining baseline regions, in order
C                                left boundary, right boundary, left.....etc.
C                    NMIN    -   Minimum acceptable number of baseline regions
C                                for this function
C                    NMAX    -   Maximum  "        "              "       "
C                    NOLD    -   Number of regions for which sensible defaults
C                                exist and are stored in IBLP
C                    NR      -   Final number of regions resulting from this
C                                call to GETPTS
C                    XSCALE  -   Array of X-values in region being examined
C                    BUF     -   Data array
C                    NCH     -   Number of values in XSCALE
C                    IFAIL   -   SPECX error return

      DIMENSION IPTS(*),XSCALE(*),BUF(*)

      LOGICAL   DONE
      LOGICAL   ITERM
      LOGICAL   OFFER
      INTEGER   TPAIR
      REAL*4    P(2), Q(2)
      CHARACTER PROMPT*30, FORMAT*20

      INCLUDE   'FLAGCOMM'

      INTEGER   GEN_ILEN

      EQUIVALENCE (PROMPT,FORMAT)

      IFAIL = 0

C   Put up plot if plot device is terminal.
      WRITE (ILOUT, *) NOLD, ' baseline regions currently defined'

      ITERM = (TERMINAL .AND. INTERACTIVE)

      IF (ITERM) THEN

        CALL ALLOCATE_DEVICE (IDEV,IFAIL)
        IF (IFAIL.NE.0) THEN
          IFAIL = 88
          RETURN
        END IF

        CALL PLOTXY (XSCALE, BUF, NCH, IFAIL)

      END IF

C   Calculate sense of XSCALE (i.e. sign of XFAC1)

      XFAC1 = (XSCALE(NCH)-XSCALE(1))/FLOAT(NCH-1)

C   Set up prompt ( first point pair only )

      PROMPT = ' '
      PROMPT(1:2) = '''"'
      CALL PPAIR (P, IPTS(1), NCH, XSCALE, IFAIL) ! Translate IPTS to current X-units

      IF (NOLD.EQ.0) THEN
        OFFER = .FALSE.
      ELSE IF (IFAIL.NE.0) THEN
        OFFER = .FALSE.
*       CALL IPUT_SCREEN ('**Invalid default interval**', 1, 1, 2)
        PRINT *,'**Invalid default interval**'
      ELSE
        IF (.NOT.ITERM) THEN
          WRITE(PROMPT, '(''[''F8.2'',''F8.2''] ''''"'')',IOSTAT=IERR) P
        END IF
        OFFER = .TRUE.
      END IF

      N    = 0
      DONE = .FALSE.

      IF (ITERM) THEN
        JDEF = TPAIR (XSCALE, BUF, NCH, OFFER, P, Q)
      ELSE
        CALL GEN_GETR4A(
     &       '"'' Type intervals, one at a time, EOF to finish''/'//
     &        ''' Current units are '//XAXIS_UNITS//'''//'//
     &        ''' # '//PROMPT(1:GEN_ILEN(PROMPT)), P, 2, ' ', P, JDEF)
      END IF

C   Then get remaining point pairs from terminal or cursor

      DO WHILE (.NOT.DONE .AND. N.LT.NMAX)

C-------------------------
C   End signalled (JDEF=2)
C-------------------------
        IF (JDEF.EQ.2) THEN
          IF (N.LT.NMIN) THEN
*            CALL IPUT_SCREEN ('**Insufficient baseline regions**',1,1,2)
            PRINT *,'**Insufficient baseline regions**'
            JDEF=-1
          ELSE
            DONE=.TRUE.
          END IF

C-------------------------
C   No input string ( <CR> ).....
C-------------------------
        ELSE IF (JDEF.EQ.1)   THEN

C   If defaults exist then use them.
          IF (OFFER)  THEN
            N = N+1
C   ..... no defaults set up => error
          ELSE
*           CALL IPUT_SCREEN ('** No default interval setup **',1,1,2)
            PRINT *,'** No default interval setup **'
          END IF

C-------------------------------------
C  Error in last attempt to input data?
C-------------------------------------

        ELSE IF (JDEF.EQ.-1)   THEN
          NOLD = MIN (N,NOLD)

C-------------------------
C  Else check input is OK.......
C-------------------------

        ELSE IF (JDEF.EQ.0) THEN
          CALL NTRANS (XSCALE, NCH, P(1), P(2), XFAC1, I1, I2, IFAIL)
          IF (IFAIL.NE.0)   THEN
*           CALL IPUT_SCREEN ('** Error in interval **', 1, 1, 2)
            PRINT *,'** Error in interval **'
          END IF

          IPTS(2*N+1) = I1
          IPTS(2*N+2) = I2
          N = N+1
        END IF

C  .......if not through go and get another pair.

        IF(N.LT.NMAX.AND..NOT.DONE)   THEN

          FORMAT = ' '
          OFFER  = .FALSE.
          IF (N.LT.NOLD)   THEN
            CALL PPAIR (P, IPTS(N*2+1), NCH, XSCALE, IFAIL)
            IF (IFAIL.EQ.0)   THEN
              FORMAT = 'F8.2,'','',F8.2'
              OFFER  = .TRUE.
            END IF
          END IF

          IF (ITERM) THEN
            JDEF = TPAIR (XSCALE, BUF, NCH, OFFER, P, Q)
          ELSE
            CALL GEN_GETR4A ('#', P, 2, FORMAT, P, JDEF)
          END IF

        END IF

      END DO

      NR   = N
      NOLD = N

      IF (ITERM) THEN
        CALL SXGTIDLE
      END IF

      RETURN
      END

