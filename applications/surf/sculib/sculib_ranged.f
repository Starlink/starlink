      SUBROUTINE SCULIB_RANGED(ARRAY,IST,IEN,VMAX,VMIN, STATUS)
*+
*  Name:
*     SCULIB_RANGED

*  Purpose:
*     Finds the maximum and minimum values in a double precision array.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Find the maximum and minimum values in a double precision array.

*  Invocation:
*     CALL SCULIB_RANGED( ARRAY, IST, IEN, VMAX, VMIN, STATUS)

*  Arguments:
*     ARRAY( IEN ) = DOUBLE PRECISION (Given)
*        Array containing the values to be checked.
*     IST = INTEGER (Given)
*        The first element of ARRAY to be examined.
*     IEN = INTEGER (Given)
*        The last element of ARRAY to be examined.
*     VMAX = DOUBLE PRECISION (Returned)
*        The maximum value of those examined
*     VMIN = DOUBLE PRECISION (Returned)
*        The minumum value of those examined
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     Keith Shortridge (AAO)
*     John Lightfoot (ROE)
*     Tim Jenness (JAC)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/06 02:24:47  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.3  1999/08/03 19:35:20  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.2  1999/07/29 21:52:29  timj
*     Rewrite header to Starlink standard. Add comments. Add status checking.
*
*     20 Sep 1991 (JFL)
*        Adapt from GEN_RANGEF by Keith Shortridge

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants

*  Arguments Given:
      INTEGER IEN
      INTEGER IST
      DOUBLE PRECISION ARRAY(IEN)

*  Arguments Returned:
      DOUBLE PRECISION VMIN
      DOUBLE PRECISION VMAX

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Retrieve the first reference value
      VMIN=ARRAY(IST)

*     Set the max to min
      VMAX=VMIN

*     Check that the start position is less than the end position
      IF (IST.LT.IEN) THEN

*     Loop over all elements, comparing
         DO I=IST+1,IEN
            VMIN=MIN(VMIN,ARRAY(I))
            VMAX=MAX(VMAX,ARRAY(I))
         END DO
      END IF

      END
