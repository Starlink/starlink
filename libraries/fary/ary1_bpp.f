      SUBROUTINE ARY1_BPP( TYPE, EL, PNTR, BAD, STATUS )
*+
*  Name:
*     ARY1_BPP

*  Purpose:
*     Determine if bad pixels are present in a vectorised array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_BPP( TYPE, EL, PNTR, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised array of any
*     numeric type and returns a logical result BAD indicating whether
*     any element of the array contains the appropriate "bad" pixel
*     value VAL__BADx (where x corresponds with the array's data type).
*     The vectorised array to be examined is passed by pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the vectorised array; an HDS primitive
*        numeric data type string (case insensitive).
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     PNTR = INTEGER (Given)
*        Pointer to the array.
*     BAD = LOGICAL (Returned)
*        Whether any array element had the value VAL__BADx.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Make a copy of the TYPE argument and check it is not
*     truncated. If OK, then convert to upper case.
*     -  Compare the data type with each permitted value in turn,
*     calling the appropriate routine to examine the array for bad
*     pixels.
*     -  Note if the data type string was not recognised.
*     -  If the TYPE argument was not valid, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-NOV-1989 (RFWS):
*        Original version.
*     5-MAR-1990 (RFWS):
*        Added missing algorithm section to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER EL
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( ARY__SZTYP ) UTYPE ! Upper case version of TYPE
      LOGICAL TYPOK              ! Whether the TYPE argument is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a copy of the TYPE argument and check it is not truncated.
      UTYPE = TYPE
      TYPOK = UTYPE .EQ. TYPE

*  If OK, then convert to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( UTYPE )

*  Compare the data type with each permitted value in turn, calling the
*  appropriate routine to examine the array for bad pixels.

*  ...byte.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL ARY1_BPPB( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...unsigned byte.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL ARY1_BPPUB( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...double precision.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL ARY1_BPPD( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...integer.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL ARY1_BPPI( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...real.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL ARY1_BPPR( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...word.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL ARY1_BPPW( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )
 
*  ...unsigned word.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL ARY1_BPPUW( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  Note if the data type string was not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the TYPE argument was not valid, then report an error.
      IF ( .NOT. TYPOK ) THEN
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_BPP' )
         CALL MSG_SETC( 'BADTYPE', TYPE )
         CALL ERR_REP( 'ARY1_BPP_BAD',
     :   'Routine ^ROUTINE called with an invalid TYPE argument of ' //
     :   '''^BADTYPE'' (internal programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_BPP', STATUS )

      END
