      SUBROUTINE SCULIB_EXTRACT_2DIM_R(X, NX, NY, TWOD, ONED, STATUS)
*+
*  Name:
*     SCULIB_EXTRACT_2DIM_R

*  Purpose:
*     To extract the second dimension (at a given X) from a 2d array

*  Invocation:
*     CALLSCULIB_EXTRACT_2DIM_R(X, NX, NY, TWOD, ONED, STATUS)

*  Description:
*     This routine extracts a second dimension from a 2D REAL array.

*  Arguments:
*     X = INTEGER (Given)
*        The X index in the 2D array to be extracted
*     NX = INTEGER (Given)
*        Size of the array in x
*     NY = INTEGER (Given)
*        Size of the array in y
*     TWOD = REAL (Given)
*        2D array input dataset
*     ONED = REAL (Returned)
*        1D strip extracted from 2D array
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Notes:
*     This is the REAL routine.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 November 9 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions

*  Arguments Given:
      INTEGER X
      INTEGER NX
      INTEGER NY
      REAL    TWOD(NX, NY)

*  Arguments Returned:
      REAL    ONED(NY)

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                        ! Loop counter
*.
      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NY
         ONED(I) = TWOD(X, I)
      END DO

      END
