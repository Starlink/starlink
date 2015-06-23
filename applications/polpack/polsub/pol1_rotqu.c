#include "f77.h"
#include "polsub.h"
#include "ast.h"

F77_SUBROUTINE(pol1_rotqu)( INTEGER(NROW), INTEGER(NCOL), INTEGER(IWCS),
                            DOUBLE(ANGLE), LOGICAL(VAR), DOUBLE_ARRAY(QIN),
                            DOUBLE_ARRAY(UIN), DOUBLE_ARRAY(QOUT),
                            DOUBLE_ARRAY(UOUT), INTEGER(STATUS) ){
/*
*+
*  Name:
*     POL1_ROTQU

*  Purpose:
*     Rotate arrays of Q and U to refer to a different reference direction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_ROTQU( NROW, NCOL, IWCS, ANGLE, VAR, QIN, UIN, QOUT, UOUT,
*                      STATUS )

*  Description:
*     The routine creates new Q and U values by rotating the reference
*     direction by a given angle.

*  Arguments:
*     NROW = INTEGER (Given)
*        The number of rows of pixels in each Q/U map.
*     NCOL = INTEGER (Given)
*        The number of columns of pixels in each Q/U map.
*     IWCS = INTEGER (Given)
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the Q (or U) array and a POLANAL Frame in
*        which the first axis defines the reference direction. It is
*        assumed that the Q and U arrays use identical FrameSets.
*     ANGLE = DOUBLE PRECISION (Given)
*        The anti-clockwise angle from the GRID X axis to the required
*        reference direction.
*     VAR = _LOGICAL (Given)
*        If .TRUE., then the supplied arrays hold variance values.
*     QIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of Q values.
*     UIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of U values.
*     QOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of Q values.
*     UOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of U values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-2012 (DSB):
*        Original version.
*     12-MAR-2013 (DSB):
*        Correct rotation of variance values.
*     22-JUN-2015 (DSB):
*        Re-write in C to allow north to vary across the map, and to use
*        threads.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_INTEGER(NROW)
   GENPTR_INTEGER(NCOL)
   GENPTR_INTEGER(IWCS)
   GENPTR_DOUBLE(ANGLE)
   GENPTR_LOGICAL(VAR)
   GENPTR_DOUBLE_ARRAY(QIN)
   GENPTR_DOUBLE_ARRAY(UIN)
   GENPTR_DOUBLE_ARRAY(QOUT)
   GENPTR_DOUBLE_ARRAY(UOUT)
   GENPTR_INTEGER(STATUS)

   AstFrameSet *iwcs = astI2P( *IWCS );
   pol1Rotqu( *NROW, *NCOL, iwcs, *ANGLE, F77_ISTRUE( *VAR ), QIN, UIN,
              QOUT, UOUT, STATUS );
}


