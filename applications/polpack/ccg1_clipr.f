      SUBROUTINE CCG1_CLIPR( RARR, NENT, RMIN, RMAX , NGOOD,
     :                         STATUS )
*+
*  Name:
*     CCG1_CLIPR

*  Purpose:
*     To set any data outside a given range BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_CLIPR( RARR, NENT, RMIN, RMAX , STATUS )

*  Description:
*     This generic routine loops over all entries in RA. If any values
*     are outside the range RMIN to RMAX then they are set BAD.
*

*  Arguments:
*     RARR = REAL (Given and Returned)
*        The list of values to be clipped within the given range. On
*        output this contains the clipped list.
*     NENT = INTEGER (Given)
*        The number of entries in RARR.
*     RMIN = REAL (Given)
*        The minimum allowed value. All values below this are set BAD.
*     RMAX = REAL (Given)
*        The maximum allowed value. All values above this are set BAD.
*     NGOOD = INTEGER (Returned)
*        The number of values left after rejection.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-APR-1991 (PDRAPER):
*        Original version.
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
      REAL RMIN
      REAL RMAX

*  Arguments Given and Returned:
      REAL RARR( NENT )

*  Arguments Returned:
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for all input values
      NGOOD = NENT
      DO 1 I = 1, NENT

*  If this value is not BAD then test it.
         IF ( RARR( I ) .NE. VAL__BADR ) THEN
            IF ( RARR( I ) .LT. RMIN  .OR.
     :           RARR( I ) .GT. RMAX ) THEN

*  Set it BAD.
               RARR( I ) = VAL__BADR

*  Decrement the valid values counter
               NGOOD = NGOOD - 1
            END IF
         END IF
 1    CONTINUE

      END
