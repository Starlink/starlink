      SUBROUTINE HDX_CLEAR( LOC, STATUS )
*+
* Name:
*    HDX_CLEAR

* Purpose:
*    To empty a structure

* Invocation:
*    CALL HDX_CLEAR( LOC, STATUS )

* Arguments:
*    LOC=CHARACTER*(DAT__SZLOC) (Given)
*       Locator to a structure (or structure array element)
*
*    STATUS-_INTEGER (Given and returned)
*       Global status

* Description:
*    The subroutine checks that the locator is for a structure and then
*    removes any components within it.

* Method:
*    The number of components (NCOMP) in the structure is found then the
*    first component is erased NCOMP times.

* Authors:
*    AJC: Alan J Chipperfield (Starlink, RAL)

* History:
*    19-NOV-2001 (AJC):
*       Original Version

*-
      IMPLICIT NONE

* Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

* Given Arguments:
      CHARACTER*(DAT__SZLOC) LOC

* Status Argument:
      INTEGER STATUS

* Local Variables:
      INTEGER NCOMP          ! Number of components
      INTEGER I              ! Component index
      LOGICAL STRUC          ! If object is structure
      CHARACTER*(DAT__SZLOC) CLOC   ! Component locator
      CHARACTER*(DAT__SZNAM) NAME   ! Component name
*.

      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL DAT_STRUC( LOC, STRUC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
*      Check if structure
         IF ( STRUC ) THEN
            CALL DAT_NCOMP( LOC, NCOMP, STATUS )
            DO I = 1, NCOMP
               CALL DAT_INDEX( LOC, 1, CLOC, STATUS )
               CALL DAT_NAME( CLOC, NAME, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
               CALL DAT_ERASE( LOC, NAME, STATUS )
            ENDDO

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Object is not a structure',
     &        STATUS )

         ENDIF

      ENDIF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'from HDX_CLEAR', STATUS )
      ENDIF

      END
