      SUBROUTINE CAP_CLINT (XIM1, YIM1, XI, YI, X0, Y0, XP, YP, INT,
     :  STATUS)
*+
*  Name:
*     CAP_CLINT
*  Purpose:
*     Do two straight line segments intersect?
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CLINT (XIM1, YIM1, XI, YI, X0, Y0, XP, YP; INT;
*       STATUS)
*  Description:
*     Do two straight line segments intersect?
*     If the line segment from (XIM1,YIM1) to (XI,YI) intersects the
*     line segment from (X0,Y0) to (XP,YP) then INT=1, otherwise INT=0.
*  Arguments:
*     XIM1  =  REAL (Given)
*           X-coord end 1 of line segment 1.
*     YIM1  =  REAL (Given)
*           Y-coord end 1 of line segment 1.
*     XI  =  REAL (Given)
*           X-coord end 2 of line segment 1.
*     YI  =  REAL (Given)
*           Y-coord end 2 of line segment 1.
*     X0  =  REAL (Given)
*           X-coord end 1 of line segment 2.
*     Y0  =  REAL (Given)
*           Y-coord end 1 of line segment 2.
*     XP  =  REAL (Given)
*           X-coord end 2 of line segment 2.
*     YP  =  REAL (Given)
*           Y-coord end 2 of line segment 2.
*     INT  =  INTEGER (Returned)
*           1 if segments cross, 0 if not.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     (No description available.)
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ASOC1: D.R.K.Brownrigg  (Edinburgh)
*     ACD:   A C Davenhall    (Edinburgh)
*  History:
*     16/7/82 (ASOC1): Original version.
*     16/7/86 (ACD):   Converted from Haggis to SCAR and changed to the
*        full Starlink style.
*     17/6/96 (ACD):   Converted from SCAR to CURSA.
*     18/6/96 (ACD):   First stable CURSA version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  XIM1,   ! X-coord end 1 of line segment 1.
     :  YIM1,   ! Y-coord end 1 of line segment 1.
     :  XI,     ! X-coord end 2 of line segment 1.
     :  YI,     ! Y-coord end 2 of line segment 1.
     :  X0,     ! X-coord end 1 of line segment 2.
     :  Y0,     ! Y-coord end 1 of line segment 2.
     :  XP,     ! X-coord end 2 of line segment 2.
     :  YP      ! Y-coord end 2 of line segment 2.
*  Arguments Returned:
      INTEGER
     :  INT     ! 1 if segments cross, 0 if not.
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      REAL
     :  VAL1,
     :  VAL2,
     :  VAL3,
     :  VAL4
*.

      IF (STATUS .EQ. SAI__OK) THEN

         CALL CAP_CLVAL (XIM1, YIM1, X0, Y0, XP, YP, VAL1, STATUS)
         CALL CAP_CLVAL (XI, YI, X0, Y0, XP, YP, VAL2, STATUS)
         CALL CAP_CLVAL (X0, Y0, XIM1, YIM1, XI, YI, VAL3, STATUS)
         CALL CAP_CLVAL (XP, YP, XIM1, YIM1, XI, YI, VAL4, STATUS)

*
*       The following condition avoids erroneously computing two
*       crossing points when a line crosses a polygon vertex. That is,
*       when the end of one line segment is crossed by a line, and the
*       same line segment end is an end of another line segment.

         IF ( (VAL1*VAL2 .LT. 0.0E0  .OR.  VAL1 .EQ. 0.0E0)  .AND.
     :     (VAL3*VAL4 .LT. 0.0E0 .OR. VAL3 .EQ. 0.0E0) ) THEN
            INT = 1
         ELSE
            INT = 0
         END IF

      END IF

      END
