      BLOCK DATA ADP0_BLK
*+
*  Name:
*     ADP0_BLK

*  Purpose:
*     Initialise ADP common block

*  Language:
*     Starlink Fortran

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Set the indentation to zero

*  Notes:
*     {routine_notes}...

*  Side Effects:
*     {routine_side_effects}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADP Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adp.html

*  Keywords:
*     package:adp, usage:private, common, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'ADP_CMN'
*       ADP_INDENT = INTEGER (returned)
*         ADP diagnostic indentation

*  Global Data:
      DATA ADP_INDENT / 0 /
*.

      END
