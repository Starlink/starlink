*+  SIT_LOCKEY - Locate a model by its in key in a list of model names
      SUBROUTINE SFIT_LOCKEY( KEY, NKEY, MENUKEY, IKEY, STATUS )
*
*    Description :
*
*     Tests to see if KEY is present in the list of menu keys MENUKEY(NKEY)
*     by comparing case-insensitive. If not present, an error is reported.
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
*     10 Aug 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*)           KEY                ! Key to test
      INTEGER                 NKEY               ! Number of keys in menu
      CHARACTER*(*)           MENUKEY(*)         ! The menu keys
*
*    Export :
*
      INTEGER                 IKEY               ! Position of KEY in MENUKEY()
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL                 CHR_SIMLR
*
*    Local variables :
*
      LOGICAL                 FOUND              ! Found the key in the list?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      FOUND = .FALSE.

*    Loop over menu keys
      IKEY = 1
      DO WHILE ( (IKEY.LE.NKEY) .AND. .NOT. FOUND )
        IF ( CHR_SIMLR(MENUKEY(IKEY),KEY) ) THEN
          FOUND = .TRUE.
        ELSE
          IKEY = IKEY + 1
        END IF
      END DO

*    Issue warning if not found
      IF ( .NOT. FOUND ) THEN
        IKEY = 0
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'I', KEY )
        CALL ERR_REP( ' ', 'Invalid item /^I/ in model specification',
     :                                                        STATUS )
      END IF

      END
