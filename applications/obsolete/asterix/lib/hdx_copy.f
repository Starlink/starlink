*+  HDX_COPY - Copies all components of an input object into an output object
      SUBROUTINE HDX_COPY( ILOC, OLOC, STATUS )
*    Description :
*     Performs a recursive copy of all components in the structured object
*     pointed to by locator ILOC, into the object at location OLOC.
*    Parameters :
*    Method :
*     Invokes DAT_COPY.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*     18 June 84 original (BHVAD::TJP)
*     30/8/88: ASTERIX88 version (pla)
*    Type Definitions :
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
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) ILOC                          ! Locator to input object
      CHARACTER*(DAT__SZLOC) OLOC                          ! Locator to output object
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) CLOC                          ! Locator to component
      CHARACTER*(DAT__SZNAM) NAME                          ! Component name

      INTEGER                I                             ! Loop counter
      INTEGER                NCOMP                         ! Number of components
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check locators
      IF ( ILOC .EQ. OLOC ) RETURN

*    Find number of components
      CALL DAT_NCOMP( ILOC, NCOMP, STATUS )

      IF ( NCOMP .EQ. 0 ) THEN
        CALL MSG_PRNT( '! WARNING : No components to copy' )
        STATUS = SAI__OK
        GOTO 99
      END IF

      DO I = 1, NCOMP
        CALL DAT_INDEX( ILOC, I, CLOC, STATUS )
        CALL DAT_NAME( CLOC, NAME, STATUS )
        CALL DAT_COPY( CLOC, OLOC, NAME, STATUS )
        CALL DAT_ANNUL( CLOC, STATUS )
      END DO

 99   IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'from HDX_COPY', STATUS )
      END IF

      END
