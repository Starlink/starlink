*+  FIT_CREGRIDAX - Create a dataset axis structure from grid axis blocks
      SUBROUTINE FIT_CREGRIDAX( FID, NAX, STATUS )
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
*     20 Nov 95 : Full ADI port (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
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
c     RECORD /GRID_AXIS/        GAX(NAX)                ! Grid axis block
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                      AVAL, AWID              ! Axis value & width
      REAL			SPARR(2)		! Spaced array data

      INTEGER                   IAX                     ! Loop over axes
      INTEGER                   IBIN                    ! Loop over axis values
      INTEGER                   PTR                     ! Ptr to axis data
      INTEGER                   WPTR                    ! Ptr to axis widths
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over axes
      DO IAX = 1, NAX

*    Write parameters
        IF ( GRID_AXIS_REGULAR(IAX) .AND.
     :       .NOT. GRID_AXIS_LOGARITHMIC(IAX) ) THEN
          SPARR(1) = GRID_AXIS_BASE(IAX)
          SPARR(2) = GRID_AXIS_SCALE(IAX)
          CALL BDI_AXPUT1R( FID, IAX, 'SpacedData', 2, SPARR, STATUS )

        ELSE

*      Map axis arrays
          CALL BDI_AXMAPR( FID, IAX, 'Data', 'WRITE', PTR, STATUS )
          CALL BDI_AXMAPR( FID, IAX, 'Width', 'WRITE', WPTR, STATUS )

*      Find axis values and widths and store in axis arrays
          DO IBIN = 1, GRID_AXIS_NVAL(IAX)
            CALL FIT_GRID_AXVAL( IAX, IBIN, AVAL, STATUS )
            CALL ARR_SELEM1R( PTR, GRID_AXIS_NVAL(IAX), IBIN, AVAL,
     :                        STATUS )
            AWID = ( 10.0**(LOG10(AVAL)+GRID_AXIS_SCALE(IAX)/2.0) -
     :               10.0**(LOG10(AVAL)-GRID_AXIS_SCALE(IAX)/2.0) )
            CALL ARR_SELEM1R( WPTR, GRID_AXIS_NVAL(IAX), IBIN, AWID,
     :                        STATUS )
          END DO

*      Unmap axis data and widths
          CALL BDI_AXUNMAP( FID, IAX, 'Data', PTR, STATUS )
          CALL BDI_AXUNMAP( FID, IAX, 'Width', WPTR, STATUS )

        END IF

      END DO

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_CREGRIDAX', STATUS )
      END IF

      END
