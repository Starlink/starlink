*+  PSS_CRERF - Create a results file field
      SUBROUTINE PSS_CRERF( SLOC, FLD, TYPE, UNITS, PTR, STATUS )
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
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! Results file
      CHARACTER*(*)            FLD                     ! Field name
      CHARACTER*(*)            TYPE                    ! Field type
      CHARACTER*(*)            UNITS                   ! Field units
*
*    Export :
*
      INTEGER                  PTR                     ! Ptr to mapped field
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Create field
        CALL SSO_CREFLD( SLOC, FLD, TYPE, STATUS )

*      Map it
        CALL SSO_MAPFLD( SLOC, FLD, TYPE, 'WRITE', PTR, STATUS )

*      Write null error value
        CALL SSO_PUTFITEM0R( SLOC, FLD, 'NULLVALUE', -1.0, STATUS )

*      Write units if given
        IF ( UNITS .GT. ' ' ) THEN
          CALL SSO_PUTFITEM0C( SLOC, FLD, 'UNITS', 40, UNITS, STATUS )
        END IF

      END IF

      END
