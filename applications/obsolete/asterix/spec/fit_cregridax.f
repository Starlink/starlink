*+  FIT_TCREGRIDAX - Create a dataset axis structure from grid axis blocks
      SUBROUTINE FIT_TCREGRIDAX( LOC, NAX, GAX, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)   LOC                     ! Output dataset
      INTEGER                  NAX                     ! Number of grid axes
      RECORD /GRID_AXIS/       GAX(NAX)                ! Grid axis block
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get locator
      CALL ADI1_GETLOC( ID, LOC, STATUS )

      CALL FIT_CREGRIDAX( LOC, NAX, GAX, STATUS )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_CREGRIDAX', STATUS )
      END IF

      END


*+  FIT_CREGRIDAX - Create a dataset axis structure from grid axis blocks
      SUBROUTINE FIT_CREGRIDAX( LOC, NAX, GAX, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)   LOC                     ! Output dataset
      INTEGER                  NAX                     ! Number of grid axes
      RECORD /GRID_AXIS/       GAX(NAX)                ! Grid axis block
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                     AVAL                    ! Axis value
      REAL                     AWID                    ! Axis width

      INTEGER                  BDA                     ! BDA identifier
      INTEGER                  IAX                     ! Loop over axes
      INTEGER                  IBIN                    ! Loop over axis values
      INTEGER                  LNAX                    ! Number of existing axes
      INTEGER                  PTR                     ! Ptr to axis data
      INTEGER                  WPTR                    ! Ptr to axis widths

      LOGICAL                  REG                     ! Regular NDF axis?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get BDA identifier
      CALL BDA_FIND( LOC, BDA, STATUS )

*    Create axes
      CALL BDA_CHKAXES_INT( BDA, LNAX, STATUS )
      IF ( LNAX .NE. NAX ) THEN
        CALL BDA_CREAXES_INT( BDA, NAX, STATUS )
      END IF

*    Loop over axes
      DO IAX = 1, NAX

*      Regular NDF axis if regular non-logarithmic grid axis
        REG = ( GAX(IAX).REGULAR .AND. .NOT. GAX(IAX).LOGARITHMIC )

*      Create structure
        CALL BDA_CREAXVAL_INT( BDA, IAX, REG, GAX(IAX).NVAL, STATUS )

*      Write parameters
        IF ( REG ) THEN
          CALL BDA_PUTAXVAL_INT( BDA, IAX, GAX(IAX).BASE,
     :            GAX(IAX).SCALE, GAX(IAX).NVAL, STATUS )
        ELSE

*        Create widths structure
          CALL BDA_CREAXWID_INT( BDA, IAX, .FALSE., GAX(IAX).NVAL,
     :                                                    STATUS )

*        Map axis arrays
          CALL BDA_MAPAXVAL_INT( BDA, 'WRITE', IAX, PTR, STATUS )
          CALL BDA_MAPAXWID_INT( BDA, 'WRITE', IAX, WPTR, STATUS )

*        Find axis values and widths and store in axis arrays
          DO IBIN = 1, GAX(IAX).NVAL
            CALL FIT_GRID_AXVAL( GAX(IAX), IBIN, AVAL, STATUS )
            CALL ARR_COP1R( 1, AVAL, %VAL(PTR+(IBIN-1)*4), STATUS )
            AWID = ( 10.0**(LOG10(AVAL)+GAX(IAX).SCALE/2.0) -
     :               10.0**(LOG10(AVAL)-GAX(IAX).SCALE/2.0) )
            CALL ARR_COP1R( 1, AWID, %VAL(WPTR+(IBIN-1)*4), STATUS )
          END DO

*        Unmap axis data
          CALL BDA_UNMAPAXVAL_INT( BDA, IAX, STATUS )
          CALL BDA_UNMAPAXWID_INT( BDA, IAX, STATUS )

        END IF

      END DO

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from FIT_CREGRIDAX', STATUS )
      END IF

      END
