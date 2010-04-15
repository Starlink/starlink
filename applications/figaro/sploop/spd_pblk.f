      BLOCK DATA SPD_PBLK
*+
*  Name:
*     SPD_PBLK

*  Purpose:
*     Initialise SPLOOP common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     EXTERNAL SPD_PBLK

*  Description:
*     This routine initialises the common block used by the SPLOOP
*     library to store information for the interactive graphic display.
*     The initialisation must reflect the fact that no data are
*     available and that no device has been opened.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Apr 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Global Data:
      DATA
     :   DEVOPN / .FALSE. /,
     :   IMXST  / .FALSE. /,
     :   SPXST  / .FALSE. /

*.

      END

