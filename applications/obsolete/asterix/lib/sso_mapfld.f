*+  SSO_MAPFLD - Map an SSO field
      SUBROUTINE SSO_MAPFLD( LOC, FLD, TYPE, MODE, PTR, STATUS )
*    Description :
*
*     Map an old format SSDS field. A wrap up for all the old routines.
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

      INTEGER                      DIMS(DAT__MXDIM) ! Field dimensions
      INTEGER                      NDIM          ! Field dimensionality
      INTEGER                      NMI           ! Mapped item index
      INTEGER                      NSRC          ! Number of sources
      INTEGER                      TLEN          ! Useful length of TYPE
*-

*    Status ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Number of sources
        CALL SSO_GETNSRC( LOC, NSRC, STATUS )

*      Locate the item
        CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*      Add object to map list
        CALL SSO_ADDMAP( LOC, FLD, SSO__MI_DATA, NMI, STATUS )

*      Already mapped?
        IF ( SSO.MI(NMI).MAPPED ) THEN

*        Return stored pointer
          PTR = SSO.MI(NMI).PTR

        ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*        Map its data array
          TLEN = INDEX( TYPE, '[' )
          IF ( TLEN .EQ. 0 ) TLEN = LEN(TYPE)+1
          CALL CMP_SHAPE( FLOC, 'DATA_ARRAY', DAT__MXDIM, DIMS, NDIM,
     :                                                       STATUS )
          CALL CMP_MAPN( FLOC, 'DATA_ARRAY', TYPE(:TLEN-1), MODE, NDIM,
     :                                              PTR, DIMS, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            SSO.MI(NMI).PTR = PTR
            SSO.MI(NMI).MAPPED = .TRUE.
            SSO.MI(NMI).FLOC = FLOC
          END IF

        END IF

*      Tidy up
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', '...from SSO_MAPFLD', STATUS )
        END IF

      END IF

      END
