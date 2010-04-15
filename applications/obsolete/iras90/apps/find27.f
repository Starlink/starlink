      SUBROUTINE FIND27( MAXLEN, SOPOS, STATUS )
*+
*  Name:
*     FIND27

*  Purpose:
*     To print a list of sources surrounding a deleted position

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND27( MAXLEN, SOPOS, STATUS )

*  Description:
*     To print a list of sources surrounding a deleted position
*     Only sources which are not to be deleted are displayed, while
*     at this stage source common contains both valid sources and
*     sources currently in process of deletion. The problem is to
*     get a complete page of valid sources centered if possible about
*     the positon at which the source has just been deleted.
*     The program first counts backwards through sources until either it
*     has half a page of sources which will be displayed, or it comes to
*     the first source. It the counts forward from this source until
*     it has either a full page of sources to be displayed, or it has
*     got to the end of the source list. It then calls the page listing
*     subroutine with the relevant parameters.

*  Arguments:
*     MAXLEN = INTEGER (Given)
*        Number of lines per page on display
*     SOPOS = INTEGER (Given)
*        Pointer to position in source common at which source is to
*        be deleted
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     FINDCRDD:
*        FIND39
*     ERR:
*        ERR_FLUSH, ERR_STAT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 general constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'CHR_ERR'          ! CHR routine errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER MAXLEN
      INTEGER SOPOS
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HALFML             ! Half the page length
      INTEGER SOTOP              ! First source to be printed
      INTEGER SOBOT              ! Last source to be printed
      INTEGER PAGLEN             ! Number of sources between the first
                                 ! and last source to be displayed
      INTEGER NSODIS             ! Number of lines to be displayed
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the number of sources to be displayed above the deleted source
*  position to be half the page length ( rounded down )
      HALFML = INT( MAXLEN / 2.0 )

*  Start with the first source to be displayed as the source currently
*  being deleted (actually this will not be displayed but its position
*  will be centered if possible)
      SOTOP = SOPOS

*  And the number of sources displayed as zero
      NSODIS = 0

*  First the program counts backwards until either the first source to
*  be displayed is the first source in the list or the number of sources
*  to be displayed is the half page length
 100  CONTINUE
      IF ( ( SOTOP .NE. 1) .AND. ( NSODIS .NE. HALFML ) )THEN

*  Move back to look at the preceeding source
         SOTOP = SOTOP - 1

*  Check whether this source will be displayed
         IF ( .NOT. SOMADE( SOTOP ) ) THEN
            NSODIS = NSODIS + 1
         END IF
         GO TO 100
      END IF

*  Then the program counts forwards from the first source to be printed
*  to find the last source to be printed. This will either give afull
*  page of printing or this will be the last source in the source list
*
*  The number of sources displayed is first reset to 1
      NSODIS = 1

*  And the current source considered is set to the first source
*  displayed
      SOBOT = SOTOP

*  Now the program goes forward through the sources checking to see
*  that it is not either a full page of MAXLEN lines, or the source
*  being considered is not the last (NOFSO)
 200  CONTINUE
      IF ( ( SOBOT .NE. NOFSO) .AND. ( NSODIS .NE. MAXLEN) ) THEN

*  Move forwards to look at the next source
         SOBOT = SOBOT + 1

*  Check whether this source will be displayed
         IF ( .NOT. SOMADE( SOBOT ) ) THEN
            NSODIS = NSODIS + 1
         END IF
         GO TO 200
      END IF

*  Now the program has the first and last lines to be displayed, we can
*  calculated how many sources both those to be printed and those to be
*  deleted (and therefore not printed) are spanned.
      PAGLEN = SOBOT - SOTOP + 1

* Display the list  (with display mode = terminal (.true), and a dummy
* file descriptor (1))
      CALL FIND39( .TRUE., 1, PAGLEN, MAXLEN, SOBOT, .FALSE., STATUS )

*  Check the output STATUS and flush the error buffer
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_STAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      END IF

      END
