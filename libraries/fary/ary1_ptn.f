      SUBROUTINE ARY1_PTN( BAD, NDIM, LBNDA, UBNDA, ATYPE, PNTR, LSUB,
     :                     USUB, LBNDD, UBNDD, HTYPE, LOC, DCE, STATUS )
*+
*  Name:
*     ARY1_PTN

*  Purpose:
*     Write an n-dimensional subregion to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ARY1_PTN( BAD, NDIM, LBNDA, UBNDA, ATYPE, PNTR, LSUB, USUB,
*      LBNDD, UBNDD, HTYPE, LOC, DCE, STATUS )

*  Description:
*     The routine writes to an n-dimensional subregion of a numeric HDS
*     array, taking the data from an n-dimensional subregion of a
*     Fortran array and making use of lower and upper bounds
*     information for both arrays. Data type conversion is performed if
*     necessary, with bad pixel testing if required. The input array is
*     passed by pointer.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to test for "bad" values during data
*        type conversion.
*     NDIM = INTEGER (Given)
*        Number of array (and HDS object) dimensions.
*     LBNDA( NDIM ) = INTEGER (Given)
*        Lower bounds of input array.
*     UBNDA( NDIM ) = INTEGER (Given)
*        Upper bounds of input array.
*     ATYPE = CHARACTER * ( * ) (Given)
*        The data type of the input array; a primitive numeric HDS data
*        type string (case insensitive).
*     PNTR = INTEGER (Given)
*        Pointer to the input array.
*     LSUB( NDIM ) = INTEGER (Given)
*        Lower bounds of subregion to be written.
*     USUB( NDIM ) = INTEGER (Given)
*        Upper bounds of subregion to be written.
*     LBNDD( NDIM ) = INTEGER (Given)
*        Lower bounds of the HDS object.
*     UBNDD( NDIM ) = INTEGER (Given)
*        Upper bounds of the HDS object.
*     HTYPE = CHARACTER * ( * ) (Given)
*        The data type of the HDS object; a primitive numeric HDS data
*        type string (case insensitive).
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the HDS object.
*     DCE = LOGICAL (Returned)
*        Whether an error occurred during data type conversion.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the input array and the output data object
*     have the same number of dimensions.  If this is not the case
*     intrinsically, then the NDIM argument should be set to match
*     whichever object has the larger dimensionality and the dimension
*     bounds of the other object (and possibly of the subregion also)
*     should be padded to match this dimensionality, normally with 1's.
*     It does not matter that the value of NDIM may not match the
*     actual dimensionality of the HDS object in such cases.
*     -  The lower and upper bounds of the subregion to be written must
*     lie within the bounds of both the input array and the output data
*     object, although the routine does not check for this.
*     -  The output data object must be suitable for vectorisation using
*     the HDS routine DAT_VEC.

*  Algorithm:
*     -  Check the data type string supplied via the ATYPE argument is
*     not too long and convert it to upper case.
*     -  Test this data type string against each permitted value in
*     turn, calling the appropriate routine to write the data
*     subregion.
*     -  Note if the data type is not recognised.
*     -  If the data type string is not valid, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUL-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     22-MAR-1990 (RFWS):
*        Added further information to the notes section.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      LOGICAL BAD
      INTEGER NDIM
      INTEGER LBNDA( NDIM )
      INTEGER UBNDA( NDIM )
      CHARACTER * ( * ) ATYPE
      INTEGER PNTR
      INTEGER LSUB( NDIM )
      INTEGER USUB( NDIM )
      INTEGER LBNDD( NDIM )
      INTEGER UBNDD( NDIM )
      CHARACTER * ( * ) HTYPE
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( ARY__SZTYP ) TYPE ! Data type of input array
      LOGICAL TYPOK              ! Whether the ATYPE argument is OK

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the string supplied for the ATYPE argument is not too
*  long.
      TYPE = ATYPE
      TYPOK = TYPE .EQ. ATYPE

*  If OK, then convert the string to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( TYPE )

*  Test the input data type string against each permitted value in
*  turn, calling the appropriate routine to write the data subregion.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL ARY1_PTNB( BAD, NDIM, LBNDA, UBNDA,
     :                      %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB, LBNDD,
     :                      UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL ARY1_PTNUB( BAD, NDIM, LBNDA, UBNDA,
     :                       %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB,
     :                       LBNDD, UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL ARY1_PTND( BAD, NDIM, LBNDA, UBNDA,
     :                      %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB, LBNDD,
     :                      UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL ARY1_PTNI( BAD, NDIM, LBNDA, UBNDA,
     :                      %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB, LBNDD,
     :                      UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL ARY1_PTNR( BAD, NDIM, LBNDA, UBNDA,
     :                      %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB, LBNDD,
     :                      UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL ARY1_PTNW( BAD, NDIM, LBNDA, UBNDA,
     :                      %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB, LBNDD,
     :                      UBNDD, HTYPE, LOC, DCE, STATUS )
 
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL ARY1_PTNUW( BAD, NDIM, LBNDA, UBNDA,
     :                       %VAL( CNF_PVAL( PNTR ) ), LSUB, USUB,
     :                       LBNDD, UBNDD, HTYPE, LOC, DCE, STATUS )
 
*  Note if the data type string is not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the string supplied for the ATYPE argument is not valid, then
*  report an error.
      IF ( .NOT. TYPOK ) THEN
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_PTN' )
         CALL MSG_SETC( 'BADATYPE', ATYPE )
         CALL ERR_REP( 'ARY1_PTN_TYPE',
     :   'Routine ^ROUTINE called with an invalid ATYPE argument ' //
     :   'of ''^BADATYPE'' (internal programming error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_PTN', STATUS )

      END
