*+  SSO_ADDMAP - Introduce a mapped item
      SUBROUTINE SSO_ADDMAP( LOC, FLD, TYPE, N, STATUS )
*
*    Description :
*
*     Look up table of existing datasets. If present, return the entry
*     number. If not present, add to table if the ADDIFNEW flag is true.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)     LOC                ! The dataset
      CHARACTER*(*)              FLD                ! The field name
      INTEGER                    TYPE               ! The type of the slot
*
*    Export :
*
      INTEGER                    N                  ! The mapped item slot
*
*    Local variables :
*
      INTEGER                    NDS                ! Dataset id
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Find slot for dataset
        CALL SSO_FINDDS( LOC, .TRUE., NDS, STATUS )

*      Find slot for mapped item
        CALL SSO_FINDMI( LOC, FLD, TYPE, .TRUE., N, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_ADDMAP', STATUS )
        END IF

      END IF

      END
