*+  PSF_CHKLIBRTN - Check that named library and routine exist
      SUBROUTINE PSF_CHKLIBRTN( LIB, ROUT, LID, RID, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER*(*)          LIB,ROUT             ! Library/routine names
*
*    Export :
*
      INTEGER                LID, RID             ! Library/routine codes
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      LOGICAL                STR_ABBREV
*
*    Local variables :
*
      INTEGER                ILIB                  ! Loop over libraries
      INTEGER                IMOD                  ! Loop over library modules
      INTEGER                LIB1, LIB2            ! Libraries to search
      INTEGER                NMATCH                ! # matches to lib name
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialised?
      IF ( .NOT. PSFLIBINIT ) CALL PSF_LINIT( STATUS )

      LID = 0
      RID = 0

*    Look for library name
      IF ( LIB .GT. ' ' ) THEN
        NMATCH = 0
        DO ILIB = 1, L_NLIB
          IF ( STR_ABBREV(L_NAME(ILIB),LIB) ) THEN
            LIB1 = ILIB
            LIB2 = ILIB
            NMATCH = NMATCH + 1
          END IF
        END DO

*      No library found?
        IF ( NMATCH .EQ. 0 ) THEN
          CALL MSG_SETC( 'LIB', LIB )
          STATUS = SAI__ERROR
          CALL ERR_RPE( ' ', 'No such library ^LIB', STATUS )

*      Check for ambiguous definition
        ELSE IF ( NMATCH .GT. 1 ) THEN
          STATUS = SAI__ERROR
          CALL MSG_PRNT( ' ',
     :      'WARNING : Ambiguous library specification', STATUS )

        END IF

      ELSE
        LIB1 = 1
        LIB2 = L_NLIB

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Search for routine
      DO ILIB = LIB1, LIB2
        DO IMOD = 1, L_NMOD(ILIB)
          IF ( STR_ABBREV(L_MODN(IMOD,ILIB),ROUT) ) THEN
            RID = IMOD
            LID = ILIB
            GOTO 99
          END IF
        END DO
      END DO
      DO ILIB = LIB1, LIB2
        DO IMOD = 1, L_NMOD(ILIB)
          IF ( STR_ABBREV('PSF_'//L_MODN(IMOD,ILIB),ROUT) ) THEN
            RID = IMOD
            LID = ILIB
            GOTO 99
          END IF
        END DO
      END DO

*    Report error
      STATUS = SAI__ERROR
      CALL ERR_REP( ' ', 'Psf not found in PSF_CHKLIBRTN', STATUS )

 99   CONTINUE

      END
