         SUBROUTINE CCD1_PTINS( ITYPE, IPSTAK, NPIX, NLINES, ILINE,
     :                          IPLINE, STATUS )
*+
*  Name:
*     CCD1_PTINS

*  Purpose:
*     To copy a (vectorised) line of data into a given line of an array,
*     of given type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PTINS( ITYPE, IPSTAK, NPIX, NLINES, ILINE, IPLINE,
*                      STATUS )

*  Description:
*     This routine uses the pointers to the line stack and the lines to
*     pass the data to the appropriate typed routine.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The data type of the input and output arrays. Must be one
*        of the HDS types - '_BYTE, _UBYTE, _WORD, _UWORD, _INTEGER,
*        _REAL or  _DOUBLE'.
*     IPSTAK = INTEGER (Given and Returned)
*        Pointer to stack of lines into which the data line is to be
*        inserted.
*     NPIX = INTEGER (Given)
*        Number of pixels in a line of data.
*     NLINES = INTEGER (Given)
*        Number of lines in the input stack.
*     ILINE = INTEGER (Given)
*        Line to insert data at.
*     IPLINE = INTEGER (Given)
*        Pointer to line of data to insert.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays to defer explicit type referencing.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAY-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      INTEGER NPIX
      INTEGER NLINES
      INTEGER ILINE
      INTEGER IPLINE

*  Arguments Given and Returned:
      INTEGER IPSTAK

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate routine to insert the line of data.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_PTISB( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                    %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_PTISUB( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                     %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_PTISW( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                    %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_PTISUW( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                     %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_PTISI( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                    %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_PTISR( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                    %VAL( IPLINE ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_PTISD( %VAL( IPSTAK ), NPIX, NLINES, ILINE,
     :                    %VAL( IPLINE ), STATUS )
      END IF

      END
* $Id$
