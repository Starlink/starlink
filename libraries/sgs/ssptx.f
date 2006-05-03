      SUBROUTINE sgs_SSPTX (SP)
*+
*  Name:
*     SSPTX

*  Purpose:
*     Specify text spacing.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     SP = REAL (Given)
*         Spacing factor

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_OTEXT, sgs_1SETTX

*  Written To Common:
*     STX      r     current character spacing

*-

      IMPLICIT NONE

      REAL SP

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save spacing factor
      STX=SP

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
