*  History:
*     22 Nov 1993 (hme):
*        Remove TABs.
C--------------------------------------------------------------

      SUBROUTINE FROMAP (ICHAN, IPOS, IROFF2, IDOFF2,
     &                   DATA, LDATA, INDEX, IFAIL)

C   Routine to remove spectrum from map at specified position
C   (IROFF2,IDOFF2) and put into standard form in stack.
C
C   IFAIL = 0.........OK
C         = 59........Negative value of IPOS calculated
C         = 56........Desired pos'n (IROFF2,IDOFF2) off map edge.
C         = 60........Insufficient spectra present in file
C         = 89........No valid data at this point (deleted?)
C
      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   ICHAN
      INTEGER   IPOS
      INTEGER   IROFF2, IDOFF2
      INTEGER   LDATA
      REAL      DATA(LDATA)
      INTEGER   INDEX(*)
      INTEGER   IFAIL

*     Include files:

      INCLUDE 'MAPHD'
      INCLUDE 'FLAGCOMM'

*     Local variables:

      INTEGER   INS
      INTEGER   IIN,   JIN

*  Ok, go...

      IFAIL=0

C   If IPOS is already set then get spectrum from that position.

      IF (IPOS.GT.0) THEN

C   Check for errors

        IF (IPOS.GT.NSPEC)   THEN
          IFAIL = 60
          RETURN
        ENDIF

C   Scan index array so that we can find what RA and Dec this spectrum
C   refers to

        DO INS = 1, MSTEP*NSTEP
          IF (INDEX(INS).EQ.IPOS) GO TO 106
        END DO
        IFAIL = 89
        RETURN

  106   JIN    = (INS-1)/MSTEP+1
        IIN    = INS-(JIN-1)*MSTEP
        IROFF2 = (MSTEP+1-IIN)*2-MSTEP-1
        IDOFF2 = (NSTEP+1-JIN)*2-NSTEP-1

      ELSE IF (IPOS.LE.0) THEN

C   Else work out index position and check that it is on the map

        IIN = MSTEP+1-(IROFF2+MSTEP+1)/2
        JIN = NSTEP+1-(IDOFF2+NSTEP+1)/2
        IF ((IIN.LT.1.OR.IIN.GT.MSTEP)
     &       .OR.(JIN.LT.1.OR.JIN.GT.NSTEP)) THEN
          IFAIL = 56
          RETURN
        END IF
        INS = MSTEP*(JIN-1)+IIN

C   Check position of scan INS.

        IPOS = INDEX(INS)

C   Check IPOS for errors

        IF (IPOS.LE.0)   THEN
          IFAIL = 59
          RETURN
        ELSE IF (IPOS.GT.NSPEC) THEN
          IFAIL = 60
          RETURN
        END IF
      END IF

C   If OK then get actual data (header is in /PROTOTYPE/)

      CALL READ_CUBE_DATA (IIN, JIN, LDATA, DATA)

      RETURN
      END

C-----------------------------------------------------------------------
