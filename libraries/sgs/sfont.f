      SUBROUTINE sgs_SFONT (NF)
*+
*  Name:
*     SFONT

*  Purpose:
*     Select text font.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NF = INTEGER (Given)
*         Font number

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
*     IPREC    i      text precision

*  Written To Common:
*     IFONT    i      Current font number

*-

      IMPLICIT NONE

      INTEGER NF

      INCLUDE 'sgscom'




*  Flush any existing text string
      CALL sgs_OTEXT

*  Save font number
      IFONT=NF

*  Set font and precision
      CALL GSTXFP(NF,IPREC)

      END
