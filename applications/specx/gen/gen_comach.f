*  History:
*     19 Nov 1993 (hme):
*        Remove TABs.
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
C-----------------------------------------------------------------------

      SUBROUTINE GEN_COMACH (NCOM,NFUNC,COMMS,COM,IERR,IPRMAT,IPRERR)

C   Routine to check character string COM against first NFUNC strings held
C   in array COMMS and to establish a match. If there is an unambiguous
C   match the matching string index is returned in NCOM, otherwise
C   NCOM is set to zero.

      LOGICAL*4 EXACT
      INTEGER*4 GEN_ILEN, GEN_KNTWRD
      CHARACTER COMMS(1)*(*),COM*(*),COMX*40

      ILC    = MIN (LEN(COMMS(1)),LEN(COM))
      IERR   = 0
      NMATCH = 0
      NWRD1  = GEN_KNTWRD (COM)

      ICOM   = 0
      EXACT  = .FALSE.

      DO WHILE (ICOM.LT.NFUNC .AND. .NOT.EXACT)
        ICOM  = ICOM + 1
        COMX  = ' '
        COMX  = COMMS(ICOM)//' '

        IF (COM(:ILC).EQ.COMX(:ILC)) THEN
          EXACT  = .TRUE.
          NMATCH = 1
          NCOM   = ICOM
          IF (IPRMAT.EQ.1)   WRITE(6,1020) COMMS(ICOM)

        ELSE
          NWRD2 = GEN_KNTWRD (COMX)
          IF (NWRD2.GE.NWRD1)   THEN
            CALL GEN_MATCH (COM, COMX, NWRD1, IMATCH)
            IF (IMATCH.NE.0)   THEN
              NCOM   = ICOM
              NMATCH = NMATCH+1
              IF (IPRMAT.EQ.1)   WRITE(6,1020) COMMS(ICOM)
            END IF
          END IF
        END IF
      END DO

      IF (NMATCH.EQ.0)   THEN
          IF(IPRERR.EQ.1)   WRITE(6,1030)
          NCOM = 0
          IERR = 1
      ELSE IF(NMATCH.GT.1)   THEN
          IF(IPRERR.EQ.1)   WRITE(6,1040)
          NCOM = 0
          IERR = 2
      END IF

      IF (NCOM.NE.0) COM(:ILC) = COMMS(NCOM)(:ILC)
      RETURN

 1020 FORMAT(' 'A)
 1030 FORMAT(' *** COMMAND NOT FOUND ***',/)
 1040 FORMAT(' *** AMBIGUOUS ***',/)

      END
