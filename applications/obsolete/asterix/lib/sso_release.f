*+  SSO_RELEASE - Release all mapped items
      SUBROUTINE SSO_RELEASE( LOC, STATUS )
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
*
*    Local variables :
*
      INTEGER                    DS                 ! Dataset id
      INTEGER                    I                  ! Mapped item id
*
*    Local data :
*
      CHARACTER*(DAT__SZNAM)     OBJS(2)
      DATA                       OBJS/'DATA_ARRAY', 'ERROR'/
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Find dataset
        CALL SSO_FINDDS( LOC, .FALSE., DS, STATUS )

*      Check all mapped items
        DO I = 1, SSO__MXMI

*        Belongs to this dataset?
          IF ( SSO.MI(I).USED .AND. ( DS .EQ. SSO.MI(I).DS ) ) THEN

*          Item is mapped?
            IF ( SSO.MI(I).MAPPED ) THEN
              CALL CMP_UNMAP( SSO.MI(I).FLOC, OBJS(SSO.MI(I).TYPE),
     :                                                     STATUS )
              SSO.MI(I).MAPPED = .FALSE.
            END IF

*          Free field locator
            CALL DAT_ANNUL( SSO.MI(I).FLOC, STATUS )

*          And reset
            SSO.MI(I).USED = .FALSE.

          END IF

        END DO

*      Free dataset slot
        SSO.DS(DS).USED = .FALSE.

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_RELEASE', STATUS )
        END IF

      END IF

      END
