*+  CONV_UNIT2R - Returns conversion factor of given units to radians
      SUBROUTINE CONV_UNIT2R( UNITS, CONV ,STATUS )
*    History :
*
*     07 Nov 89 : Original ( BHVAD :: DJA )
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      CHARACTER*(*) UNITS
*
*    Export :
*
      REAL CONV                             ! Conversion factor to radians
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      LOGICAL      STR_ABBREV
*-
      CONV=1.0

      IF ( STATUS .EQ. SAI__OK ) THEN

        IF ( STR_ABBREV( UNITS, 'DEGREES' ) ) THEN
          CONV = MATH__DTOR
        ELSE IF ( STR_ABBREV( UNITS, 'ARCMINUTES' ) .OR.
     :                          (UNITS.EQ.'ARCMINS') ) THEN
          CONV = MATH__DTOR / 60.0
        ELSE IF ( STR_ABBREV( UNITS, 'ARCSECONDS' ) .OR.
     :                          (UNITS.EQ.'ARCSECS') ) THEN
          CONV = MATH__DTOR / 3600.0
        ELSE IF ( STR_ABBREV(UNITS, 'RADIANS') ) THEN
          CONV = 1.0
        ELSE
          CALL MSG_SETC( 'UN', UNITS )
          CALL MSG_PRNT( '! unrecognised units ^UN' )
          STATUS = SAI__ERROR
        END IF

        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP(' ','from CONV_UNIT2R',STATUS)
        END IF

      END IF

      END
