      BLOCK DATA DYN0_BLK
*+
*  Name:
*     DYN0_BLK

*  Purpose:
*     Initialise DYN common block

*  Language:
*     Starlink Fortran

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Set the initialised flag to false so that DYN class definitions will
*     be loaded properly

*  Notes:
*     {routine_notes}...

*  Side Effects:
*     {routine_side_effects}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private, common, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'DYN_CMN'
*       DYN_ISINIT = LOGICAL (returned)
*         DYN system is initialised?
*       DYS_PTR = INTEGER (returned)
*         Mapped section memory addresses
*       DYS_ISEQ = INTEGER (returned)
*         File sequence number

*  Global Data:
      DATA DYN_ISINIT / .FALSE. /
      DATA DYS_PTR / DYN__NMAX*0 /
      DATA DYS_ISEQ / 1 /
*.

      END
