      SUBROUTINE CCG1_CLIPD( DARR, NENT, DMIN, DMAX , NGOOD,
     :                         STATUS )
*+
*  Name:
*     CCG1_CLIPD

*  Purpose:
*     To set any data outside a given range BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_CLIPD( DARR, NENT, DMIN, DMAX , STATUS )

*  Description:
*     This generic routine loops over all entries in DA. If any values
*     are outside the range DMIN to DMAX then they are set BAD.
*

*  Arguments:
*     DARR = DOUBLE PRECISION (Given and Returned)
*        The list of values to be clipped within the given range. On
*        output this contains the clipped list.
*     NENT = INTEGER (Given)
*        The number of entries in DARR.
*     DMIN = DOUBLE PRECISION (Given)
*        The minimum allowed value. All values below this are set BAD.
*     DMAX = DOUBLE PRECISION (Given)
*        The maximum allowed value. All values above this are set BAD.
*     NGOOD = INTEGER (Returned)
*        The number of values left after rejection.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (PDRAPER):
*        Original version.
*     15-MAR-1999 (MBT):
*        Modified to use PRIMDAT properly.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! BAD values

*  Arguments Given:
      INTEGER NENT
      DOUBLE PRECISION DMIN
      DOUBLE PRECISION DMAX

*  Arguments Given and Returned:
      DOUBLE PRECISION DARR( NENT )

*  Arguments Returned:
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! PRIMDAT generic declarations
      INCLUDE 'NUM_DEC_D'      ! PRIMDAT declarations for type DOUBLE PRECISION
      INCLUDE 'NUM_DEF_CVT'      ! PRIMDAT generic definitions
      INCLUDE 'NUM_DEF_D'      ! PRIMDAT definitions for type DOUBLE PRECISION

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for all input values
      NGOOD = NENT
      DO 1 I = 1, NENT

*  If this value is not BAD then test it.
         IF ( DARR( I ) .NE. VAL__BADD ) THEN
            IF ( NUM_LTD( DARR( I ), DMIN ) .OR.
     :           NUM_GTD( DARR( I ), DMAX ) ) THEN

*  Set it BAD.
               DARR( I ) = VAL__BADD

*  Decrement the valid values counter
               NGOOD = NGOOD - 1
            END IF
         END IF
 1    CONTINUE

      END
* $Id$
