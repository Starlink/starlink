*+  SSO_CREFLD - Create an SSO field
      SUBROUTINE SSO_CREFLD( LOC, FLD, TYPE, STATUS )
*    Description :
*
*     Create an old format SSDS field. A wrap up for all the old routines.
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
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       FLOC          ! Field structure
      CHARACTER*(DAT__SZLOC)       PLOC          ! POSIT structure
      CHARACTER*(DAT__SZLOC)       TLOC          ! Temp structure

      INTEGER                      BPOS          ! "[" position
      INTEGER                      NDIM          ! Field dimensionality
      INTEGER                      DIMS(DAT__MXDIM)

      LOGICAL                      OK            ! POSIT ok?
      LOGICAL                      THERE         ! Object already there?
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Does field already exist
        CALL SSO_CHKFLD( LOC, FLD, THERE, STATUS )

*      Get POSIT structure
        CALL DAT_FIND( LOC, 'POSIT', PLOC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Fields inside IMGCOR
        IF ( .NOT. THERE ) THEN
          IF ( ( FLD .EQ. 'X_CORR' ) .OR. ( FLD .EQ. 'Y_CORR' ) ) THEN

*          Locate the IMGCOR box
            CALL DAT_THERE( PLOC, 'IMG_COORDS', OK, STATUS )
            IF ( .NOT. OK ) THEN
              CALL DAT_NEW( PLOC, 'IMG_COORDS','EXTENSION', 0, 0,
     :                                                   STATUS )
            END IF
            CALL DAT_FIND( PLOC, 'IMG_COORDS', TLOC, STATUS )

*          Create field
            CALL DAT_NEW( TLOC, FLD(1:1), 'LIST', 0, 0, STATUS )

*          Find field
            CALL DAT_FIND( TLOC, FLD(1:1), FLOC, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )

*        Fields inside CELCOR
          ELSE IF ( ( FLD .EQ. 'RA' ) .OR. ( FLD .EQ. 'DEC' ) ) THEN

*          Locate the CELCOR box
            CALL DAT_THERE( PLOC, 'CEL_COORDS', OK, STATUS )
            IF ( .NOT. OK ) THEN
              CALL DAT_NEW( PLOC, 'CEL_COORDS', 'EXTENSION', 0, 0,
     : 		  				        STATUS )
            END IF
            CALL DAT_FIND( PLOC, 'CEL_COORDS', TLOC, STATUS )

*          Create field
            CALL DAT_NEW( TLOC, FLD, 'LIST', 0, 0, STATUS )

*          Find field
            CALL DAT_FIND( TLOC, FLD, FLOC, STATUS )
            CALL DAT_ANNUL( TLOC, STATUS )

*        Stuff just one level down
          ELSE

*          Create the item
            CALL DAT_NEW( PLOC, FLD, 'LIST', 0, 0, STATUS )
            CALL DAT_FIND( PLOC, FLD, FLOC, STATUS )

          END IF

*      Already exists
        ELSE

*        So find it
          CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

        END IF

*      If field list already there, delete DATA_ARRAY if present
        IF ( THERE ) THEN
          CALL DAT_THERE( FLOC, 'DATA_ARRAY', THERE, STATUS )
          IF ( THERE ) THEN
            CALL DAT_ERASE( FLOC, 'DATA_ARRAY', STATUS )
          END IF
        END IF

*      Is field scalar?
        BPOS = INDEX( TYPE, '[' )
        IF ( BPOS .EQ. 0 ) THEN
          NDIM = 0
          BPOS = LEN(TYPE)+1

        ELSE

*        Extract dimensions
          CALL PRS_GETDIMS( TYPE(BPOS:), DAT__MXDIM-1, NDIM, DIMS,
     :                                                    STATUS )

        END IF
        NDIM = NDIM + 1
        CALL CMP_GET0I( PLOC, 'NSRC', DIMS(NDIM), STATUS )

*      Create value component
        CALL DAT_NEW( FLOC, 'DATA_ARRAY', TYPE(:BPOS-1), NDIM,
     :                                          DIMS, STATUS )

*      Free locators
        CALL DAT_ANNUL( FLOC, STATUS )
        CALL DAT_ANNUL( PLOC, STATUS )

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_CREFLD', STATUS )
        END IF

      END IF

      END
