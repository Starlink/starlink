      SUBROUTINE PROJ_AITPAR( THETA0, GPAR )
*+
*  Name:
*     PROJ_AITPAR

*  Purpose:
*     generate scaling parameters for the Aitoff projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PROJ_AITPAR( THETA0, GPAR )

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
*        Removed unused and set and not used variables.
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
      DOUBLE PRECISION SINT      ! SIN(THETA0)
      DOUBLE PRECISION COST      ! COS(THETA0)
      DOUBLE PRECISION TEMP      ! [local_variable_description]
      DOUBLE PRECISION DT

*.

      SINT=SIN(THETA0)
      COST=COS(THETA0)

      DT=(XINC*SIN(DDEG2R*IMROT) + YINC*COS(DDEG2R*IMROT))*DAS2R
      IF (DT.EQ.0) THEN
         DT=DAS2R
! WRITE AN ERROR MESSAGE  (WARNING)
      ENDIF

      TEMP=SIN(THETA0+DT)/SQRT((1D0+COS(THETA0+DT))/2)
     :              -SINT/SQRT((1D0+COST)/2)
      GPAR(2)=DT/TEMP

      DT=(XINC*COS(DDEG2R*IMROT) - YINC*SIN(DDEG2R*IMROT))*DAS2R
      IF (DT.EQ.0) THEN
         DT=DAS2R
! WRITE AN ERROR MESSAGE  (WARNING)
      ENDIF
      TEMP=2*COST*SIN(DT/2)
      IF(TEMP.EQ.0) TEMP=1D0
      GPAR(1)=DT*SQRT((1+COST*COS(DT/2D0))/2D0)/TEMP

      GPAR(3)=GPAR(1)*SINT/SQRT((1+COST)/2D0)

      END
* $Id$
