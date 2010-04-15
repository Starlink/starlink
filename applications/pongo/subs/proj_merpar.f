      SUBROUTINE PROJ_MERPAR( THETA0, GPAR )
*+
*  Name:
*     PROJ_MERPAR

*  Purpose:
*     generate scaling parameters for the MERCATOR projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_MERPAR( THETA0, GPAR )

*  Description:
*     {routine_description}

*  Arguments:
*     THETA0 = DOUBLE PRECISION (Given)
*        latitude/declination of rerefence point
*     GPAR( 3 ) = DOUBLE PRECISION (Returned)
*        array containing f$_\alpha$, f$_\delta$ and M$_0$

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JAN-1990 (JBVAD::PAH):
*        Original version.
*     3-JUN-1994 (PDRAPER):
*        Removed unused variables.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'ASTRO_PAR'

*  Arguments Given:
      DOUBLE PRECISION THETA0

*  Arguments Returned:
      DOUBLE PRECISION GPAR( 3 )


*  Global Variables
      INCLUDE 'PROJ_CMN'         ! Common block

*  Local Variables:
      DOUBLE PRECISION TEMP      ! [local_variable_description]
      DOUBLE PRECISION DT

*.
      DT=(XINC*SIN(DDEG2R*IMROT) + YINC*COS(DDEG2R*IMROT))*DAS2R
      IF (DT.EQ.0) THEN
         DT=DAS2R
! WRITE AN ERROR MESSAGE  (WARNING)
      ENDIF

      TEMP=THETA0/2+DPI/4

      GPAR(1)=COS(THETA0)
      IF(GPAR(1).LE.0D0)  GPAR(1)=1D0

      GPAR(2)=DT/(LOG(TAN(TEMP+DT/2))
     :                 -LOG(TAN(TEMP)))

      GPAR(3)=GPAR(2)*LOG(TAN(TEMP))

      END
* $Id$
