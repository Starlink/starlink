      SUBROUTINE CCD1_STSAT( ITYPE, BAD, IPOINT, EL, SETSAT, SATVAL,
     :                       NSAT, STATUS )
*+
*  Name:
*     CCD1_STSAT

*  Purpose:
*     To set saturated values BAD or to a given value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_STSAT( ITYPE, BAD, IPOINT, EL, SETSAT, SATVAL, NSAT,
*                      STATUS )

*  Description:
*     Calls the appropriate type CCG1_SSAT routine to do the work.
*     Setting saturated pixels BAD or to the saturation value.
*     It also returns the number of saturated pixels.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The data type.
*     BAD = LOGICAL (Given and Returned)
*        Set for BAD pixels present.
*     IPOINT = INTEGER (Given and Returned)
*        Pointer to the data which is to be saturated.
*     EL = INTEGER (Given)
*        Size of data.
*     SETSAT = LOGICAL (Given)
*        Whether the saturated data is to be set to the saturation
*        value or not.
*     SATVAL = DOUBLE PRECISION (Given)
*        The saturation value.
*     NSAT = INTEGER (Returned)
*        The number of saturated pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-APR-1991 (PDRAPER):
*        Original version.
*     12-JUN-1991 (PDRAPER):
*        Added number of saturated pixels.
*     12-DEC-1991 (PDRAPER):
*        Made fully generic.
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
      INTEGER EL
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL

*  Arguments Given and Returned:
      INTEGER IPOINT
      LOGICAL BAD

*  Arguments Returned:
      INTEGER NSAT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate CCG1_STSAT routine.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_SSATB( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL, NSAT,
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_SSATUB( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL,
     :                     NSAT, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_SSATW( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL, NSAT,
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_SSATUW( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL,
     :                     NSAT, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_SSATI( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL, NSAT,
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_SSATR( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL, NSAT,
     :                    STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_SSATD( BAD, %VAL( IPOINT ), EL, SETSAT, SATVAL, NSAT,
     :                    STATUS )
      END IF

      END
* $Id$
