      SUBROUTINE CCD1_STVB( ITYPE, EL, IPOINT, STATUS )
*+
*  Name:
*     CCD1_STVB

*  Purpose:
*     To set an array of a given type to BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_STVB( ITYPE, EL, IPOINT, STATUS )

*  Description:
*     This routine just defers typing by one level calling the
*     appropriate version of CCG1_STVx for the given type with the
*     appropriate BAD flag.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPOINT.
*     EL = INTEGER (Given)
*        The number of elements in to pointed to array.
*     IPOINT = INTEGER (Given and Returned)
*        Pointer to the data to be set to BAD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-AUG-1991 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! BAD values constants

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER IPOINT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate version of CCG1_STV to set the pointed to array
*  to BAD.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_STVB( VAL__BADB, EL, %VAL( IPOINT ) , STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_STVUB( VAL__BADUB, EL, %VAL( IPOINT ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_STVW( VAL__BADW, EL, %VAL( IPOINT ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_STVUW( VAL__BADUW, EL, %VAL( IPOINT ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_STVI( VAL__BADI, EL, %VAL( IPOINT ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_STVR( VAL__BADR, EL, %VAL( IPOINT ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_STVD( VAL__BADD, EL, %VAL( IPOINT ), STATUS )
      ELSE

*  Bad ITYPE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_STVB',
     :   '  Error setting array, bad numeric type '//
     :   '(possible programming error)', STATUS )
      END IF

      END
* $Id$
