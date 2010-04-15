      BLOCK DATA SPD_FBLK
*+
*  Name:
*     SPD_FBLK

*  Purpose:
*     Initialise FITRES common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     EXTERNAL SPD_FBLK

*  Description:
*     This routine initialises the common block used by the FITRES
*     library to store information on accessed results structures. The
*     initialisation must reflect the fact that no such structures have
*     been accessed, i.e. it must set MNDF for all slots to NDF__NOID.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Feb 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Global Data:
      DATA MNDF / SPD__FMXR * NDF__NOID /

*.

      END
