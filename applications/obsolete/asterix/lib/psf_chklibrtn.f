*+  PSF_CHKLIBRTN - Check that named library and routine exist
      SUBROUTINE PSF_CHKLIBRTN( ROUT, TAG, STATUS )
*
*    Description :
*
*     Library and routine name are checked against the PSF system's
*     internal table. An error is returned if there is no match. If
*     the supplied library name is blank, all the libraries are
*     searched.
*
*    Method :
*
*     PSF_CHK requires an unambiguous library specification.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Mar 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER*(*)          	ROUT             	! Routine names
*
*    Export :
*
      CHARACTER*(*)		TAG			! Full name
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      LOGICAL                	STR_ABBREV
*
*    Local variables :
*
      INTEGER                   ICMP                    ! Loop over list
      INTEGER                   NPSF                    ! # psfs
      INTEGER                   PID                     ! A particular psf

      LOGICAL			FOUND			! Found a psf?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialised?
      IF ( .NOT. PSFINIT ) CALL PSF_INIT( STATUS )

*  Find number of psfs
      CALL ADI_NCMP( P_PLIST, NPSF, STATUS )

*  Scan list for a match
      FOUND = .FALSE.
      ICMP = 1
      DO WHILE ( (ICMP.LE.NPSF) .AND. .NOT. FOUND )

*    Index it
        CALL ADI_INDCMP( P_PLIST, ICMP, PID, STATUS )
        CALL ADI_NAME( PID, TAG, STATUS )
        CALL ADI_ERASE( PID, STATUS )

*    Match?
        IF ( STR_ABBREV( TAG, ROUT ) ) THEN
          FOUND = .FALSE.
        ELSE
          ICMP = ICMP + 1
        END IF

      END DO

*  Report error
      IF ( .NOT. FOUND ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'P', ROUT )
        CALL ERR_REP( ' ', 'Psf /^P/ not found in PSF_CHKLIBRTN',
     :                STATUS )
      END IF

      END
