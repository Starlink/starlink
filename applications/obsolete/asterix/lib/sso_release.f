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
          IF ( SSO_MI_USED(I) .AND. (DS .EQ. SSO_MI_DS(I) ) ) THEN

*          Item is mapped?
            IF ( SSO_MI_MAPPED(I) ) THEN
              CALL CMP_UNMAP( SSO_MI_FLOC(I), OBJS(SSO_MI_TYPE(I)),
     :                                                     STATUS )
              SSO_MI_MAPPED(I) = .FALSE.
            END IF

*          Free field locator
            CALL DAT_ANNUL( SSO_MI_FLOC(I), STATUS )

*          And reset
            SSO_MI_USED(I) = .FALSE.

          END IF

        END DO

*      Free dataset slot
        SSO_DS_USED(DS) = .FALSE.

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'SSO_RELEASE', STATUS )
        END IF

      END IF

      END
