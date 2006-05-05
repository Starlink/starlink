      SUBROUTINE snx_EZRXY (XDRA, YDRA, NPTS, XLAB, YLAB, GLAB)

*+
*  Name:
*     EZRXY

*  Purpose:
*     Version of NCAR AUTOGRAPH routine EZXY which includes
*     axis labelling.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XDRA() = REAL (Given)
*         Array of x data
*     YDRA() = REAL (Given)
*         Array of y data
*     NPTS = INTEGER (Given)
*         Number of points
*     XLAB = CHAR (Given)
*         X axis label (bottom)
*     YLAB = CHAR (Given)
*         Y axis label (left)
*     GLAB = CHAR (Given)
*         Graph label (top)
*
*  Externals:
*     snx_AGLAB, EZXY

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      REAL XDRA,YDRA
      INTEGER NPTS
      CHARACTER*(*) XLAB,YLAB,GLAB



*  Set up axis labels
      CALL snx_AGLAB('B',XLAB)
      CALL snx_AGLAB('L',YLAB)
      CALL snx_AGLAB('T',GLAB)

*  Plot the graph
      CALL EZXY(XDRA,YDRA,NPTS,CHAR(0))

      END
