      SUBROUTINE CAP_CLVAL (X, Y, XA, YA, XB, YB, VAL, STATUS)
*+
*  Name:
*     CAP_CLVAL
*  Purpose:
*     On which side of a line does a point lie?
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CLVAL (X, Y, XA, YA, XB, YB; VAL; STATUS)
*  Description:
*     On which side of a given line segment does a given point lie?
*     (y-YA) * (XB-XA)  -  (x-XA) * (YB-YA) = 0 defines the straight
*     line through (XA,YA) and (XB,YB). The expression is evaluated for
*     some given point (X,Y) to decide on which side of the line (X,Y)
*     lies.
*  Arguments:
*     X  =  REAL (Given)
*           X-coord of given point.
*     Y  =  REAL (Given)
*           Y-coord of given point.
*     XA  =  REAL (Given)
*           X-coord point 1 on line.
*     YA  =  REAL (Given)
*           Y-coord point 1 on line.
*     XB  =  REAL (Given)
*           X-coord point 2 on line.
*     YB  =  REAL (Given)
*           Y-coord point 2 on line.
*     VAL  =  REAL (Returned)
*           Value of line expression at (X,Y).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute VAL.  Note that checks are made to prevent division by
*     zero.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ASOC1: D.R.K.Brownrigg  (Edinburgh)
*     ACD:   A C Davenhall    (Edinburgh)
*  History:
*     16/7/82 (ASOC1): Original version.
*     16/6/86   (ACD): Converted from Haggis to SCAR and changed and
*         converted to the Starlink style.
*     17/6/96   (ACD): Converted from SCAR to CURSA and removed VMS
*         specific handling of arithmetic errors.
*     24/10/96  (ACD): Removed (un-necessary) traps for arithmetic
*         errors.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  X,     ! X-coord of given point.
     :  Y,     ! Y-coord of given point.
     :  XA,    ! X-coord point 1 on line.
     :  YA,    ! Y-coord point 1 on line.
     :  XB,    ! X-coord point 2 on line.
     :  YB     ! Y-coord point 2 on line.
*  Arguments Returned:
      REAL
     :  VAL    ! Value of line expression at (X,Y).
*  Status:
      INTEGER STATUS   ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         VAL = (Y-YA) * (XB-XA)  -  (X-XA) * (YB-YA)

      END IF

      END
