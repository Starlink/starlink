      BLOCK DATA DSA_BLOCK
*+
*  Name:
*     DSA_BLOCK

*  Purpose:
*     Block data routine for the DSA package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     EXTERNAL DSA_BLOCK

*  Description:
*     Block data initialisation routine for DSA common variables.
*     This routine is 'invoked' by DSA_OPEN, which therefore should be
*     called early on in any application.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Jun 1987 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Local Constants:
      INTEGER AXBYRF             ! Axis by reference array size
      PARAMETER ( AXBYRF = NDF__MXDIM * DSA__MAXREF )

*  Local Data:
      DATA DSA__BADQUA / .FALSE. /
      DATA DSA__REFUSD / DSA__MAXREF * .FALSE. /
      DATA DSA__REFBAD / DSA__MAXREF * .FALSE. /
      DATA DSA__REFQUA / DSA__MAXREF * .FALSE. /
      DATA DSA__REFRSD / DSA__MAXREF * .FALSE. /
      DATA DSA__REFMDD / DSA__MAXREF * .FALSE. /
      DATA DSA__REFMDQ / DSA__MAXREF * .FALSE. /
      DATA DSA__REFMDV / DSA__MAXREF * .FALSE. /
      DATA DSA__REFRSA / AXBYRF * .FALSE. /
      DATA DSA__REFMDC / AXBYRF * .FALSE. /
      DATA DSA__REFMDW / AXBYRF * .FALSE. /
      DATA DSA__REFID1 / DSA__MAXREF * NDF__NOID /
      DATA DSA__REFID2 / DSA__MAXREF * NDF__NOID /
      DATA DSA__REFID3 / DSA__MAXREF * NDF__NOID /
      DATA DSA__REFPLC / DSA__MAXREF * NDF__NOPL /
      DATA DSA__REFDPT / DSA__MAXREF * 0 /
      DATA DSA__REFQPT / DSA__MAXREF * 0 /
      DATA DSA__REFFPT / DSA__MAXREF * 0 /
      DATA DSA__REFFNE / DSA__MAXREF * 0 /
      DATA DSA__REFNDF / DSA__MAXREF * ' ' /
      DATA DSA__REFNAM / DSA__MAXREF * ' ' /
      DATA DSA__REFFLO / DSA__MAXREF * DAT__NOLOC /
      DATA DSA__REFLOC / DSA__MAXREF * DAT__NOLOC /
      DATA DSA__MAPUSD / DSA__MAXMAP * .FALSE. /
      DATA DSA__MAPID1 / DSA__MAXMAP * NDF__NOID /
      DATA DSA__MAPAXI / DSA__MAXMAP * 0 /
      DATA DSA__MAPREF / DSA__MAXMAP * 0 /
      DATA DSA__MAPPT1 / DSA__MAXMAP * 0 /
      DATA DSA__MAPPT2 / DSA__MAXMAP * 0 /
      DATA DSA__MAPNAM / DSA__MAXMAP * ' ' /
      DATA DSA__MAPLO1 / DSA__MAXMAP * DAT__NOLOC /
      DATA DSA__MAPLO2 / DSA__MAXMAP * DAT__NOLOC /
      DATA DSA__LUUSD  / DSA__MAXLU  * .FALSE. /
      DATA DSA__LUNUM  / DSA__MAXLU  * 0 /

*.
      END
