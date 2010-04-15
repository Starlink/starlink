      BLOCK DATA IRM1_BLDAT
*+
*  Name:
*     IRM1_BLDAT

*  Purpose:
*     Initialises IRM common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 16 (MJC):
*        Original version.
*     1992 May 27 (DSB):
*        Name changed from IRM_BLDAT to IRM1_BLDAT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'IRM_COM'          ! IRM_ common blocks
*        MCM_STDAT = LOGICAL (Write)
*          True means that the pen-characteristics variables have
*          already been set, otherwise not yet.
*        MCM_LUTER = INTEGER (Write)
*          Logical unit number for output to the terminal. Initialised
*          to -1 to indicate that no paged text values have yet been
*          set up in common.

*  Global Data:
      DATA MCM_STDAT /.FALSE./
      DATA MCM_LUTER /-1/

*.

      END
