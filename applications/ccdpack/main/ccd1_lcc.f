      SUBROUTINE CCD1_LCC( ARRIN, NREC, NVAL, NCOLIN, NCOLO, ARROUT,
     :                     STATUS )
*+
*  Name:
*     CCD1_LCC

*  Purpose:
*     Copys a column of data into another array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LCC( ARRIN, NREC, NVAL, NCOLIN, NCOLO, ARROUT, STATUS )

*  Description:
*     This routine copies the specified column of data into
*     the specified output column of ARROUT.

*  Arguments:
*     ARRIN( NREC, NVAL ) = DOUBLE PRECISION (Given)
*        Array of data which is to have one column copied into
*        ARROUT.
*     NREC = INTEGER (Given)
*        First dimension of the input and output arrays.
*     NVAL =  INTEGER (Given)
*        Second dimension of the input and output arrays.
*     NCOLIN = INTEGER (Given)
*        Number of the column of data in ARRIN which is to be copied.
*     NCOLO = INTEGER (Given)
*        Number of the column into which the data is to be copied.
*     ARROUT( NREC, NVAL ) = DOUBLE PRECISION (Returned)
*        Output array containing the column of data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-JUL-1992 (PDRAPER):
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
      INTEGER NREC
      INTEGER NVAL
      DOUBLE PRECISION ARRIN( NREC, NVAL )
      INTEGER NCOLIN
      INTEGER NCOLO

*  Arguments Returned:
      DOUBLE PRECISION ARROUT( NREC, NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just loop transfering the data as appropriate.
      DO 1 I = 1, NREC
         ARROUT( I, NCOLIN ) = ARRIN( I, NCOLO )
 1    CONTINUE

      END
* $Id$
