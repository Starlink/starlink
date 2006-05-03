      SUBROUTINE sgs_1NORM (X,Y, XN,YN)
*+
*  Name:
*     NORM

*  Purpose:
*     Normalize a pair of numbers which specify the extent of a rectangle.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     The routine returns a pair of numbers with the same quotient but
*     neither of which is greater than unity.

*  Arguments:
*     X,Y = REAL (Given)
*         Number pair to be normalised
*     XN,YN = REAL (Returned)
*         Normalised number pair

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

*-

      IMPLICIT NONE

      REAL X,Y,XN,YN

      REAL XA,YA,D



      XA=ABS(X)
      YA=ABS(Y)
      D=MAX(XA,YA)
      XN=X/D
      YN=Y/D

      END
