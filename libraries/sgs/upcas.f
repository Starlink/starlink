      SUBROUTINE sgs_1UPCAS (IN, OUT)
*+
*  Name:
*     UPCAS

*  Purpose:
*     Convert character string to uppercase.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     IN = CHAR (Given)
*         String to be converted
*     OUT = CHAR (Returned)
*         Converted string

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
*     chr_UCASE

*-

      IMPLICIT NONE

      CHARACTER*(*) IN,OUT



      OUT = IN
      CALL chr_UCASE(OUT)

      END
