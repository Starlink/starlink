      SUBROUTINE GAI_T2CAT( FI, CI, STATUS )
*+
*  Name:
*     GAI_T2CAT

*  Purpose:
*     Convert a tab table into a CAT catalogue.

*  Description:
*     This routine reads a file attached to a given FIO identifier and
*     attempts to convert it into a CAT catalogue, under the assumption
*     that the attached file contains a "tab table". If the table was
*     written by the associated application "cat2tab" then it will be
*     converted correctly (although the meta data will be lost),
*     otherwise the format for the data will be guessed.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI_T2CAT( FI, CI, STATUS )

*  Description:

*  Arguments:
*     FI = INTEGER (Given)
*        FIO identifier of the catalogue to be converted.
*     CI = INTEGER (Given)
*        Fortran unit number of output file.
*     STATUS = INTEGER (Given and Returned)
*        The global status on exit.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1998 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      INTEGER FI
      INTEGER CI

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 10 ) LINE   ! Line buffer

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  To convert the catalogue we check the "title" field (this must be the
*  first line). If this is "cat2tab" then we can assume that it contains
*  all of the information we require to do the conversion, otherwise we
*  will need to attempt a conversion using the fields themselves to
*  determine the data types.
      CALL FIO_READF( FI, LINE, STATUS )
      IF ( LINE .EQ. 'cat2tab' ) THEN 
         CALL GAI1_T2CN( CI, FI, STATUS )
      ELSE
         CALL GAI1_T2CG( CI, FI, 10, STATUS ) ! XXX 10 should be a parameter.
      END IF
      END
