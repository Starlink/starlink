      SUBROUTINE sgs_SHTX (H)
*+
*  Name:
*     SHTX

*  Purpose:
*     Specify text height.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     H = REAL (Given)
*         Text height (sign ignored)

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
*     HTX       r      current txt height

*-

      IMPLICIT NONE

      REAL H

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save height
      HTX=ABS(H)

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
