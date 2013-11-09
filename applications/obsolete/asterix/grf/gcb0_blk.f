      BLOCK DATA GCB0_BLK
*+
*  Name:
*     GCB0_BLK

*  Purpose:
*     Initialise GCB common block

*  Language:
*     Starlink Fortran

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Set the initialised flag to false so that GCB class definitions will
*     be loaded properly

*  Notes:
*     {routine_notes}...

*  Side Effects:
*     {routine_side_effects}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private, common, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'
*       G_MTHINIT = LOGICAL (returned)
*         GCB system is initialised?

*  Global Data:
      DATA G_MTHINIT / .FALSE. /
*.

      END
