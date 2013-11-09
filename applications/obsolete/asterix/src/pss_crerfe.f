*+  PSS_CRERFE - Create a results file field error
      SUBROUTINE PSS_CRERFE( SID, FLD, NDAT, NLEV, LEVS, PTR, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Jun 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER			SID			! Results file id
      CHARACTER*(*)            	FLD                     ! Field name
      INTEGER                   NDAT                    ! Items per level
      INTEGER                   NLEV                    ! Number of error levels
      DOUBLE PRECISION          LEVS(*)                 ! Error levels
*
*    Export :
*
      INTEGER                   PTR                     ! Ptr to mapped field
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Create field error
        CALL SSI_CREFLDERR( SID, FLD, '_REAL', NDAT, NLEV, STATUS )

*      Map it
        CALL SSI_MAPFLDERR( SID, FLD, '_REAL', 'WRITE', PTR, STATUS )

*      Write error levels
        CALL SSI_PUTFITEM1D( SID, FLD, 'ELEVS', NLEV, LEVS, STATUS )

*      Write null error value
        CALL SSI_PUTFITEM0R( SID, FLD, 'NULLERROR', -1.0, STATUS )

      END IF

      END
