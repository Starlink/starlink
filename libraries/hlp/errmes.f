      SUBROUTINE hlp_ERRMES (J, MES)
*+
*  - - - - - - -
*   E R R M E S
*  - - - - - - -
*
*  Translate help system error code into message.
*
*  Given (argument):
*     J         i           status value
*
*  Returned (argument)
*     MES       c*(*)       message (may be up to 50 characters)
*
*  P.T.Wallace   Starlink   28 July 1992
*-

      IMPLICIT NONE

      INTEGER J
      CHARACTER*(*) MES



      IF (J.GT.0) THEN
         MES='HLP illegal status'
      ELSE IF (J.EQ.0) THEN
         MES='OK'
      ELSE IF (J.EQ.-1) THEN
         MES='Help system in wrong state'
      ELSE IF (J.EQ.-2) THEN
         MES='Help library error on OPEN'
      ELSE IF (J.EQ.-3) THEN
         MES='Help library error on WRITE'
      ELSE IF (J.EQ.-4) THEN
         MES='Help library error on READ'
      ELSE IF (J.EQ.-5) THEN
         MES='Help library error on CLOSE'
      ELSE IF (J.EQ.-6) THEN
         MES='Attempt to WRITE outside help library'
      ELSE IF (J.EQ.-7) THEN
         MES='Attempt to READ outside help library'
      ELSE IF (J.EQ.-8) THEN
         MES='Help record overflows supplied string'
      ELSE IF (J.EQ.-9) THEN
         MES='Help library creation failure'
      ELSE IF (J.EQ.-10) THEN
         MES='HLP illegal status'
      ELSE IF (J.EQ.-11) THEN
         MES='hlp_HELP internal error'
      ELSE IF (J.EQ.-12) THEN
         MES='Line output failure'
      ELSE IF (J.EQ.-13) THEN
         MES='Line input failure'
      ELSE IF (J.EQ.-14) THEN
         MES='Invalid index entry'
      ELSE IF (J.EQ.-15) THEN
         MES='Attempted switch to current library'
      ELSE IF (J.EQ.-16) THEN
         MES='String too small'
      ELSE IF (J.EQ.-17) THEN
         MES='File name translation error'
      ELSE
         MES='HLP illegal status'
      END IF

      END
