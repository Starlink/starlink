      SUBROUTINE IRH1_FIND( SIZE, ARRAY, TEXT, INDEX, STATUS )
*+
*  Name:
*     IRH1_FIND

*  Purpose:
*     Find a name in a character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_FIND( SIZE, ARRAY, TEXT, INDEX, STATUS )

*  Description:
*     The array is searched for the given text, and the index of the
*     first occurence returned. If the text is not found an index of
*     zero is returned. 

*  Arguments:
*     SIZE = INTEGER (Given)
*        The size of the array specified by argument ARRAY.
*     ARRAY( SIZE ) = CHARACTER (Given)
*        The array. 
*     TEXT = CHARACTER (Given) 
*        The text to be searched for.
*     INDEX = INTEGER (Returned)
*        The index at which the text was found within ARRAY. A value of
*        zero is returned if the text is not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1991 (DSB):
*        Original version.
*     28-FEB-1992 (PDRAPER):
*        Added case sensitive switch - changed logic slightly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRH_PAR'          ! IRH constants

*  Arguments Given:
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      CHARACTER TEXT*(*)

*  Arguments Returned:
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! True if two strings are the same,
                                 ! ignoring case differences.
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      INTEGER ALEN               ! Used length of text from array.
      INTEGER I                  ! Loop count.
      INTEGER TLEN               ! Used length of given text.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned index to indicate that the given text was not
*  found.
      INDEX = 0

*  Find the length of the text to be searched for.
      TLEN = CHR_LEN( TEXT )

*  If no characters remain, return with INDEX = 0.
      IF( TLEN .EQ. 0 ) GO TO 999

*  Loop through the array until the text is found.
      DO I = 1, SIZE
         IF( INDEX .EQ. 0 ) THEN

*  Get the length of the array text.
            ALEN = CHR_LEN( ARRAY( I ) )

*  Proceed only if the string lengths are equal.
            IF ( TLEN .EQ. ALEN ) THEN 

*  If any characters remain in the array text, compare them with the
*  supplied text -- only ingnore case if enabled.
               IF ( IRH__UCASE ) THEN 
                  IF( CHR_SIMLR( ARRAY( I )(:ALEN), TEXT(:TLEN) ) )
     :            THEN
                     INDEX = I
                  END IF
               ELSE

*  Perform case sensitive comparison.
                  IF ( ARRAY( I )(:ALEN) .EQ. TEXT(: TLEN ) ) THEN 
                     INDEX = I
                  END IF
               END IF
            END IF
         END IF
      END DO

 999  CONTINUE

      END
* $Id$
