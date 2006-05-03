      SUBROUTINE sgs_SPREC (NPR)
*+
*  Name:
*     SPREC

*  Purpose:
*     Select text precision.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NPR = INTEGER (Given)
*         Text precision

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
*     sgs_OTEXT, GSTXFP

*  Read From Common:
*     IFONT     i      current font

*  Written To Common:
*     IPREC     i      current text precision

*-

      IMPLICIT NONE

      INTEGER NPR

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save precision
      IPREC=NPR

*  Set font and precision
      CALL GSTXFP(IFONT,NPR)

      END
