*+  CONV_MONTH - Converts a month character string to an integer (1-12)
      SUBROUTINE CONV_MONTH( CMONTH, IMONTH, STATUS )
*    Description :
*    History :
*     7 Jul 1990 Original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*3 CMONTH                  ! Character string  e.g. MAR
*    Import-Export :
*    Export :
      INTEGER IMONTH                      ! Month position e.g. MAR=3
*    Local constants :
*    Local variables :
*-
      CALL CHR_UCASE( CMONTH )
*
      IF ( CMONTH .EQ. 'JAN' ) THEN
        IMONTH=1
      ELSE IF (CMONTH .EQ. 'FEB') THEN
        IMONTH=2
      ELSE IF (CMONTH .EQ. 'MAR') THEN
        IMONTH=3
      ELSE IF (CMONTH .EQ. 'APR') THEN
        IMONTH=4
      ELSE IF (CMONTH .EQ. 'MAY') THEN
        IMONTH=5
      ELSE IF (CMONTH .EQ. 'JUN') THEN
        IMONTH=6
      ELSE IF ((CMONTH .EQ. 'JUL').OR.(CMONTH .EQ. 'JLY')) THEN
        IMONTH=7
      ELSE IF (CMONTH .EQ. 'AUG') THEN
        IMONTH=8
      ELSE IF (CMONTH .EQ. 'SEP') THEN
        IMONTH=9
      ELSE IF (CMONTH .EQ. 'OCT') THEN
        IMONTH=10
      ELSE IF (CMONTH .EQ. 'NOV') THEN
        IMONTH=11
      ELSE IF (CMONTH .EQ. 'DEC') THEN
        IMONTH=12
      ELSE
        CALL MSG_PRNT( 'Month not recognised' )
        STATUS = SAI__ERROR
      END IF
*
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'from CONV_MONTH', STATUS )
      END IF

      END
