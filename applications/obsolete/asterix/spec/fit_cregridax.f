*+  FIT_CREGRIDAX - Create a dataset axis structure from grid axis blocks
      SUBROUTINE FIT_CREGRIDAX( FID, NAX, GAX, STATUS )
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
*     24 Apr 95 : Updated to use BDI_ (DJA)
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
      INTEGER			FID			! Dataset id
      INTEGER                   NAX                     ! Number of grid axes
      RECORD /GRID_AXIS/        GAX(NAX)                ! Grid axis block
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                      AVAL                    ! Axis value
      REAL                      AWID                    ! Axis width

      INTEGER                   IAX                     ! Loop over axes
      INTEGER                   IBIN                    ! Loop over axis values
      INTEGER                   LNAX                    ! Number of existing axes
      INTEGER                   PTR                     ! Ptr to axis data
      INTEGER                   WPTR                    ! Ptr to axis widths

      LOGICAL                   REG                     ! Regular NDF axis?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Create axes
      CALL BDI_CHKAXES( FID, LNAX, STATUS )
      IF ( LNAX .NE. NAX ) THEN
        CALL BDI_CREAXES( FID, NAX, STATUS )
      END IF

*    Loop over axes
      DO IAX = 1, NAX

*      Regular NDF axis if regular non-logarithmic grid axis
        REG = ( GAX(IAX).REGULAR .AND. .NOT. GAX(IAX).LOGARITHMIC )

*      Create structure
        CALL BDI_CREAXVAL( FID, IAX, REG, GAX(IAX).NVAL, STATUS )

*      Write parameters
        IF ( REG ) THEN
          CALL BDI_PUTAXVAL( FID, IAX, GAX(IAX).BASE,
     :            GAX(IAX).SCALE, GAX(IAX).NVAL, STATUS )
        ELSE

*        Create widths structure
          CALL BDI_CREAXWID( FID, IAX, .FALSE., GAX(IAX).NVAL, STATUS )

*        Map axis arrays
          CALL BDI_MAPAXVAL( FID, 'WRITE', IAX, PTR, STATUS )
          CALL BDI_MAPAXWID( FID, 'WRITE', IAX, WPTR, STATUS )

*        Find axis values and widths and store in axis arrays
          DO IBIN = 1, GAX(IAX).NVAL
            CALL FIT_GRID_AXVAL( GAX(IAX), IBIN, AVAL, STATUS )
            CALL ARR_COP1R( 1, AVAL, %VAL(PTR+(IBIN-1)*4), STATUS )
            AWID = ( 10.0**(LOG10(AVAL)+GAX(IAX).SCALE/2.0) -
     :               10.0**(LOG10(AVAL)-GAX(IAX).SCALE/2.0) )
            CALL ARR_COP1R( 1, AWID, %VAL(WPTR+(IBIN-1)*4), STATUS )
          END DO

*        Unmap axis data
          CALL BDI_UNMAPAXVAL( FID, IAX, STATUS )
          CALL BDI_UNMAPAXWID( FID, IAX, STATUS )

        END IF

      END DO

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_CREGRIDAX', STATUS )
      END IF

      END
