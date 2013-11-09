*+  PSS_CRERF - Create a results file field
      SUBROUTINE PSS_CRERF( SID, FLD, TYPE, UNITS, PTR, STATUS )
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
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER			SID			! Results file id
      CHARACTER*(*)             FLD                     ! Field name
      CHARACTER*(*)             TYPE                    ! Field type
      CHARACTER*(*)             UNITS                   ! Field units
*
*    Export :
*
      INTEGER                  PTR                     ! Ptr to mapped field
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Create field
        CALL SSI_CREFLD( SID, FLD, TYPE, STATUS )

*      Map it
        CALL SSI_MAPFLD( SID, FLD, TYPE, 'WRITE', PTR, STATUS )

*      Write null error value
        CALL SSI_PUTFITEM0R( SID, FLD, 'NULLVALUE', -1.0, STATUS )

*      Write units if given
        IF ( UNITS .GT. ' ' ) THEN
          CALL SSI_PUTFITEM0C( SID, FLD, 'UNITS', 40, UNITS, STATUS )
        END IF

      END IF

      END
