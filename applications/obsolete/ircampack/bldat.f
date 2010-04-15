      BLOCK DATA BLDAT
*+
*  Name:
*     BLDAT

*  Purpose:
*     Initialises IRCAMPACK common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (MJC):
*        Original version, based on IRAS90 routine IRM1_BLDAT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRCAMPACK common blocks
*        IRC_LUTER = INTEGER (Write)
*          Logical unit number for output to the terminal. Initialised
*          to -1 to indicate that no paged text values have yet been
*          set up in common.

*  Global Data:
      DATA IRC_LUTER /-1/

*.

      END
