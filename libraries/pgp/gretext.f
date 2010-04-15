      SUBROUTINE GRETXT
*+
*
*     - - - - - - - -
*       G R E T X T    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Erase text screen
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   D.L.Terrett  Starlink  Jul 1987
*+
      IMPLICIT NONE

      INCLUDE 'grecom.inc'

      INCLUDE 'SAE_PAR'

      INCLUDE 'PGP_ERR'


      CHARACTER*64 DEV, ETEXT
      INTEGER LETX, LUN, ISTAT, LDEV

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRETXT - No PGPLOT device open',
     :   GRNODO)
      ELSE

*     Get the host operating system device name of the current device
         CALL GRQDEV(DEV,LDEV)

*     Get the erase screen text string
         ISTAT = SAI__OK
         CALL gns_IETG(GRWKID(GRCIDE),ETEXT,LETX,ISTAT)

         IF (ISTAT.NE.SAI__OK) THEN
            CALL ERR_REP('GRUNET',
     :      'GRETXT - Unable to erase text screen', GRUNET)
         ELSE
            IF (LETX.GT.0) THEN

*     Get a free logical unit number
               CALL GRGLUN(LUN)

*     Open a channel to the device
               OPEN( UNIT=LUN, FILE=DEV(1:LDEV), STATUS='OLD',
     :               IOSTAT=ISTAT)
               IF (ISTAT.NE.0) THEN
                   CALL ERR_FIOER('IOSTAT', ISTAT)
                   CALL ERR_REP('GRUACH',
     :           'GRETXT - Unable assign channel to terminal, ^IOSTAT',
     :              GRUACH)
                   GO TO 9999
               ENDIF

*     Write the text string
               WRITE( UNIT=LUN, FMT='(A)', IOSTAT=ISTAT) ETEXT(1:LETX)
               IF (ISTAT.NE.0) THEN
                  CALL ERR_FIOER('IOSTAT', ISTAT)
                  CALL ERR_REP('GREWT',
     :             'GRETXT - Error writing to terminal, ^IOSTAT ',
     :              GREWT)
                  GO TO 9999
               ENDIF

*     Free the unit
               CLOSE( UNIT=LUN )
            ENDIF
         ENDIF
      ENDIF

 9999 CONTINUE
      END
