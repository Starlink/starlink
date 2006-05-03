      SUBROUTINE sgs_SARTX (AR)
*+
*  Name:
*     SARTX

*  Purpose:
*     Specify text aspect ratio.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     AR = REAL (Given)
*         Text aspect ratio (W/H)

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
*     ARTX    r      text aspect ratio

*-

      IMPLICIT NONE

      REAL AR

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Normalise & save aspect ratio
      ARTX=MAX(ABS(AR),1E-6)

*  Translate SGS text parameters to GKS
      CALL sgs_1SETTX

      END
