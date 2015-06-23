#include "f77.h"
#include "polsub.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(pol1_pa2gr)( INTEGER(IWCS), INTEGER(AXIS), DOUBLE(GX0), DOUBLE(GY0),
                            DOUBLE(ANGLE), INTEGER(STATUS) ){
/*
*+
*  Name:
*     POL1_PA2GR

*  Purpose:
*     Convert a position angle from current Frame to base Frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PA2GR( IWCS, AXIS, GX0, GY0, ANGLE, STATUS )

*  Description:
*     The routine transforms a position angle from the current Frame of a
*     FrameSet to the base Frame (GRID coords).

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST identifier for a WCS FrameSet. Base Frame should be GRID.
*     AXIS = INTEGER (Given)
*        The index of the axis (0 or 1) defining the position angle to be
*        converted.
*     GX0 = DOUBLE PRECISION (Given)
*        The GRID X position at which the conversion is to be performed.
*     GY0 = DOUBLE PRECISION (Given)
*        The GRID Y position at which the conversion is to be performed.
*     ANGLE = DOUBLE PRECISION (Given and Returned)
*        On entry, the angle from the axis given by "axis" to the required
*        position angle, in radians, measured positive in the same sense as
*        rotation from the first axis to the second axis. On exit, it will
*        be the angle in radians from the GRID X axis to the required position
*        angle, measured positive in the same sense as rotation from GRID X
*        to GRID Y.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(IWCS)
   GENPTR_INTEGER(AXIS)
   GENPTR_DOUBLE(GX0)
   GENPTR_DOUBLE(GY0)
   GENPTR_REAL(ANGLE)
   GENPTR_INTEGER(STATUS)

   double oldang = *ANGLE;

   if( oldang != AST__BAD ) {
      AstFrameSet *iwcs = astI2P( *IWCS );
      pol1Pa2gr( iwcs, *AXIS, 1, GX0, GY0, ANGLE, STATUS );
      *ANGLE += oldang;
   }
}


