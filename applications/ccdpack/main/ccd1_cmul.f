      SUBROUTINE CCD1_CMUL( BAD, ITYPE, IPIN, EL, CVAL, IPOUT, STATUS )
*+
*  Name:
*     CCD1_CMUL

*  Purpose:
*     To multiply a data array of a given type by a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CMUL( BAD, ITYPE, IPIN, EL, CVAL, IPOUT, STATUS )

*  Description:
*     This routine just dummys to the appropriate CCG1_CMLT routine.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPOINT.
*     BAD = LOGICAL (Given and Returned)
*        Flag for BAD pixels present.
*     IPIN = INTEGER (Given and Returned)
*        Pointer to input data array.
*     EL = INTEGER (Given)
*        Number of elements in array.
*     CVAL = DOUBLE PRECISION (Given)
*        Constant to multiply.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to output array containing the modified values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-APR-1991 (PDRAPER):
*        Original version.
*     29-MAY-1991 (PDRAPER):
*        Changed to return data in different array.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      DOUBLE PRECISION CVAL
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER IPIN
      INTEGER IPOUT
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NERR               ! Numeric error count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate subtraction routine by type.
      IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_CMLTUB( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                     NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_CMLTB( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_CMLTUW( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                     NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_CMLTW( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_CMLTI( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_CMLTR( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                    NERR, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_CMLTD( BAD, EL, %VAL( IPIN ), CVAL, %VAL( IPOUT ),
     :                    NERR, STATUS )
      ELSE

*  Unsupported numeric type, issue error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_CMUL1',
     :   '  CCD1_CMUL: Unsupported numeric type ^TYPE', STATUS )
      END IF

*  Update BAD flag.
      BAD = BAD .OR. ( NERR .NE. 0 )

      END
* $Id$
