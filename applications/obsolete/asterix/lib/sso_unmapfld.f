*+  SSO_UNMAPFLD - Unmap a field
      SUBROUTINE SSO_UNMAPFLD( LOC, FLD, STATUS )
*
*    Description :
*
*     Look up mapped item table for appropriate item. Free all resources
*     associated with it.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Jul 91 : Original (DJA)
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
*    Global variables :
*
      INCLUDE 'SSO_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)     LOC                ! The dataset
      CHARACTER*(*)              FLD                ! The field name
*
*    Local variables :
*
      INTEGER                    NMI                ! Mapped item id
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Find slot for mapped item
        CALL SSO_FINDMI( LOC, FLD, SSO__MI_DATA, .FALSE., NMI, STATUS )

*      Reset flags
        IF ( STATUS .EQ. SAI__OK ) THEN

*        Unmap
          IF ( SSO.MI(NMI).MAPPED ) THEN
            CALL CMP_UNMAP( SSO.MI(NMI).FLOC, 'DATA_ARRAY', STATUS )
            SSO.MI(NMI).MAPPED = .FALSE.
          END IF

*        Free field locator
          CALL DAT_ANNUL( SSO.MI(NMI).FLOC, STATUS )

*        And reset slot
          SSO.MI(NMI).USED = .FALSE.

        END IF

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_UNMAPFLD', STATUS )
        END IF

      END IF

      END
