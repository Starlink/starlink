      SUBROUTINE GRP1_FCHK( SLOT, GRPEXP, FLAG, STATUS )
*+
*  Name:
*     GRP1_FCHK

*  Purpose:
*     See if a group expression is flagged, and remove comments.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_FCHK( SLOT, GRPEXP, FLAG, STATUS )

*  Description:
*     Any string following a comment character is removed from the 
*     supplied group exression (along with the comment character 
*     itself). The last non-blank character of the remaining group 
*     expression is compared with the groups current flag character. 
*     If it matches, the character is removed, and FLAG is returned 
*     .TRUE. Otherwise FLAG is returned .FALSE.

*  Arguments:
*     SLOT = INTEGER (Given)
*        A grp slot number for the group.
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression.
*     FLAG = LOGICAL (Returned)
*        Returned .TRUE. if the group expression was flagged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     9-SEP-1992 (DSB):
*        Call to MSG_SETC added to set up token F.
*     13-JAN-1994 (DSB):
*        Modified to look at the last character in a group expression
*        rather than at the last element in a group (argument GRPEXP
*        added).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      INTEGER SLOT
      CHARACTER GRPEXP*(*)

*  Arguments Returned:
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      INTEGER COM                ! Index of the comment character
      CHARACTER COMC*1           ! Groups current omment character.
      LOGICAL COMOK              ! .TRUE. if COMC is not NULL.
      CHARACTER FLAGC*1          ! Groups current flag character.
      LOGICAL FLAGOK             ! .TRUE. if FLAGC is not NULL.
      INTEGER LASTC              ! Position of last non-blank character.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise FLAG to be .FALSE.
      FLAG = .FALSE.

*  Get the group's current comment character.
      CALL GRP1_CONC( SLOT, GRP__PCOMC, COMC, COMOK, STATUS )

*  If a comment character is defined...
      IF( COMOK ) THEN

*  Search for the first occurrence of the comment character in the 
*  supplied group expression.
         COM = INDEX( GRPEXP, COMC )

*  If a comment character was found, set the rest of the group 
*  expression blank (including the comment character itself).
         IF( COM .GT. 0 ) GRPEXP( COM : ) = ' '

      END IF

*  Get the group's current flag character.
      CALL GRP1_CONC( SLOT, GRP__PFLGC, FLAGC, FLAGOK, STATUS )

*  Only proceed if the groups flag character is defined.
      IF( FLAGOK .AND. STATUS .EQ. SAI__OK ) THEN

*  Find the last non-blank character in the group expression.
         LASTC = CHR_LEN( GRPEXP )

*  Only proceed if the group expression is not blank.
         IF( LASTC .GT. 0 ) THEN

*  See if the last non-blank character is the same as the group's flag
*  character.
            FLAG = GRPEXP( LASTC : LASTC ) .EQ. FLAGC

*  If it is, remove the flag character.
            IF( FLAG ) GRPEXP( LASTC : LASTC ) = ' '            

         END IF

      END IF

      END
