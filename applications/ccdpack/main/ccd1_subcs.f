      SUBROUTINE CCD1_SUBCS( PTYPE, BAD, IPSTK, NPIX, NLINES, CONST,
     :                       STATUS )
*+
*  Name:
*     CCD1_SUBCS

*  Purpose:
*     To subtract a series of constants from a stack of (vectorised)
*     data lines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_SUBCS( PTYPE, BAD, STACK, NPIX, NLINES, CONST, STATUS )

*  Description:
*      This routine calls the appropriate version of CCG1_SUBC to to the
*      work.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if BAD pixels are present.
*     IPSTK = INTEGER (Given and Returned)
*        Pointer to the stack of vectorised lines.
*     NPIX = INTEGER (Given)
*        Number of values in a line of data.
*     NLINES = INTEGER (Given)
*        The number of lines of vectorised data in STACK.
*     CONST( NLINES ) = DOUBLE PRECISION (Given)
*        The constants to subtract from the lines of data (one per
*        line).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUL-1991 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLINES
      DOUBLE PRECISION CONST( NLINES )
      LOGICAL BAD
      CHARACTER PTYPE * ( * )

*  Arguments Given and Returned:
      INTEGER IPSTK

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate routine.
      IF ( PTYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_SUBCB( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_SUBCUB( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                     STATUS )
      ELSE IF ( PTYPE .EQ. '_WORD' ) THEN
         CALL CCG1_SUBCW( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_SUBCUW( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                     STATUS )
      ELSE IF ( PTYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_SUBCI( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_REAL' ) THEN
         CALL CCG1_SUBCR( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                    STATUS )
      ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_SUBCD( BAD, %VAL( IPSTK ), NPIX, NLINES, CONST,
     :                    STATUS )
      END IF
      END
* $Id$
