*+  SSO_CHKFLDERR - Check existance of field error
      SUBROUTINE SSO_CHKFLDERR( LOC, FLD, OK, STATUS )
*    Description :
*
*     Check presence an old format SSDS field error. Assumes presence of
*     field itself.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to find
*
*    Export :
*
      LOGICAL                      OK            ! Field exists?
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Field exists?
        CALL SSO_CHKFLD( LOC, FLD, OK, STATUS )
        IF ( OK ) THEN

*        Locate field
          CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*        Does error exist?
          CALL HDX_OK( FLOC, 'ERROR', OK, STATUS )

*        Free locator
          CALL DAT_ANNUL( FLOC, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Field '//FLD//' does not exist', STATUS )
        END IF

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CHKFLDERR', STATUS )
        END IF

      END IF

      END
*+  SSO_CREFLDERR - Create an SSO field error component
      SUBROUTINE SSO_CREFLDERR( LOC, FLD, TYPE, NDAT, NLEV, STATUS )
*    Description :
*
*     Create an old format SSDS field error. A wrap up for all the old routines.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to find
      CHARACTER*(*)                TYPE          ! Field type
      INTEGER                      NDAT          ! # data items per level
      INTEGER                      NLEV          ! # levels
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
      CHARACTER*(DAT__SZLOC)       PLOC          ! POSIT structure

      INTEGER                      DIMS(3)       ! Error dimensions
      INTEGER                      NDIM          ! Error dimensionality
      INTEGER                      NSRC          ! Number of sources

      LOGICAL                      ETHERE        ! ERROR already exists?
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Get POSIT structure
        CALL DAT_FIND( LOC, 'POSIT', PLOC, STATUS )

*      Locate field
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Remove any existing ERROR object
        CALL DAT_THERE( FLOC, 'ERROR', ETHERE, STATUS )
        IF ( ETHERE ) THEN
          CALL DAT_ERASE( FLOC, 'ERROR', STATUS )
        END IF

*      Create error component
        CALL CMP_GET0I( PLOC, 'NSRC', NSRC, STATUS )
        NDIM = 1
        IF ( NDAT .GT. 1 ) THEN
          DIMS(NDIM) = NDAT
          NDIM = NDIM + 1
        END IF
        IF ( NLEV .GT. 1 ) THEN
          DIMS(NDIM) = NLEV
          NDIM = NDIM + 1
        END IF
        DIMS(NDIM) = NSRC
        CALL DAT_NEW( FLOC, 'ERROR', TYPE, NDIM, DIMS, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CREFLDERR', STATUS )
        END IF

      END IF

      END
*+  SSO_MAPFLDERR - Map an SSO field's error
      SUBROUTINE SSO_MAPFLDERR( LOC, FLD, TYPE, MODE, PTR, STATUS )
*    Description :
*
*     Map an old format SSDS field error. A wrap up for all the old routines.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to map
      CHARACTER*(*)                TYPE          ! Mapping type
      CHARACTER*(*)                MODE          ! Access mode
*
*    Export :
*
      INTEGER                      PTR           ! Ptr to mapped field
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
      CHARACTER*(DAT__SZLOC)       PLOC          ! POSIT structure

      INTEGER                      NELM          ! Number of mapped elements
      INTEGER                      NMI           ! Mapped item id
      INTEGER                      NSRC          ! Number of sources
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Number of sources
        CALL DAT_FIND( LOC, 'POSIT', PLOC, STATUS )
        CALL CMP_GET0I( PLOC, 'NSRC', NSRC, STATUS )

*      Locate the item
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Insert in mapped list
        CALL SSO_ADDMAP( LOC, FLD, SSO__MI_ERROR, NMI, STATUS )

*      Already mapped?
        IF ( SSO.MI(NMI).MAPPED ) THEN

*        Return pointer
          PTR = SSO.MI(NMI).PTR

        ELSE

*        Map its error array
          CALL CMP_MAPV( FLOC, 'ERROR', TYPE, MODE, PTR, NELM, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            SSO.MI(NMI).MAPPED = .TRUE.
            SSO.MI(NMI).PTR = PTR
            SSO.MI(NMI).FLOC = FLOC
          END IF

        END IF

*      Free POSIT
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_MAPFLDERR', STATUS )
        END IF

      END IF

      END
*+  SSO_PUTFITEM0C - Write a field item of type _CHAR*LEN
      SUBROUTINE SSO_PUTFITEM0C( LOC, FLD, ITEM, LEN, VALUE, STATUS )
*    Description :
*
*     Write a field item for field FLD.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
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
*    Import :
*
      CHARACTER*(DAT__SZLOC)       LOC           ! SSDS locator
      CHARACTER*(*)                FLD           ! Field to find
      CHARACTER*(*)                ITEM          ! Field item to create
      INTEGER                      LEN           ! Length of character item
      CHARACTER*(*)                VALUE         ! Field item value
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure

      LOGICAL                      THERE         ! Item already exists?
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Locate field
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Erase existing component if there
        CALL DAT_THERE( FLOC, ITEM, THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_ERASE( FLOC, ITEM, STATUS )
        END IF

*      Create new object
        CALL DAT_NEWC( FLOC, ITEM, LEN, 0, 0, STATUS )

*      Write value
        CALL CMP_PUT0C( FLOC, ITEM, VALUE, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_PUTFITEM0C', STATUS )
        END IF

      END IF

      END
*+  SSO_UNMAPFLDERR - Unmap a field
      SUBROUTINE SSO_UNMAPFLDERR( LOC, FLD, STATUS )
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
*     12 Jul 91 : Original (BHVAD::DJA)
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
        CALL SSO_FINDMI( LOC, FLD, SSO__MI_ERROR, .FALSE., NMI, STATUS )

*      Reset flags
        IF ( STATUS .EQ. SAI__OK ) THEN

*        Unmap
          IF ( SSO.MI(NMI).MAPPED ) THEN
            CALL CMP_UNMAP( SSO.MI(NMI).FLOC, 'ERROR', STATUS )
            SSO.MI(NMI).MAPPED = .FALSE.
          END IF

*        Free field locator
          CALL DAT_ANNUL( SSO.MI(NMI).FLOC, STATUS )

*        And reset slot
          SSO.MI(NMI).USED = .FALSE.

        END IF

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_UNMAPFLDERR', STATUS )
        END IF

      END IF

      END
