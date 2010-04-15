      SUBROUTINE SURFLIB_CALC_IJPOS (N_PTS, PIXEL_SZ,
     :     I_CENTRE, J_CENTRE, X_POS, Y_POS, IJ,
     :     STATUS )
*+
*  Name:
*     SURFLIB_CALC_IJPOS

*  Purpose:
*     Calculate the position of data on an XY grid

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURFLIB_CALC_IJPOS (N_PTS, PIXEL_SZ,
*     :     I_CENTRE, J_CENTRE, X_POS, Y_POS, IJ,
*     :     STATUS )


*  Description:
*     Given an XY position, calculates the IJ position in the grid.
*     The I,J's are then placed into the output array (of dimension
*     2, N_PTS)

*  Arguments:
*     N_PTS = INTEGER (Given)
*       Number of X,Y pairs supplied
*     PIXEL_SZ = DOUBLE (Given)
*       Size of each pixel (radians)
*     I_CENTRE = INTEGER (Given)
*       Location of reference pixel in X
*     J_CENTRE = INTEGER (Given)
*       Location of reference pixel in Y
*     X_POS = DOUBLE PRECISION (Given)
*       X positions
*     Y_POS = DOUBLE PRECISION (Given)
*       Y positions
*     IJ ( 2, N_PTS ) = INTEGER (Given)
*       Positions of each point (I,J) in output grid
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)
*     John Lightfoot (jfl@roe.ac.uk)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.3  1999/08/06 02:29:04  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.2  1999/08/03 19:32:47  timj
*     Add copyright message to header.
*
*     Revision 1.1  1997/10/21 20:17:57  timj
*     Initial revision
*
*

*  Notes:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values

*  Arguments Given:
      INTEGER I_CENTRE
      INTEGER J_CENTRE
      INTEGER N_PTS
      DOUBLE PRECISION X_POS(N_PTS)
      DOUBLE PRECISION Y_POS(N_PTS)
      DOUBLE PRECISION PIXEL_SZ

*  Arguments Returned:
      INTEGER IJ(2, N_PTS)

*     Status
      INTEGER STATUS

*     Local Variables:
      INTEGER I               ! Loop variable
      DOUBLE PRECISION XINC                      ! the x-axis pixel increment
      DOUBLE PRECISION YINC                      ! the y-axis pixel increment


*.
      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXEL_SZ
      YINC = PIXEL_SZ


*     Loop through all points in the input data

      DO I = 1, N_PTS

*     Calculate IJ and place in array
         IJ (1, I) = NINT (X_POS(I)/XINC) + I_CENTRE
         IJ (2, I) = NINT (Y_POS(I)/YINC) + J_CENTRE

      END DO


      END
