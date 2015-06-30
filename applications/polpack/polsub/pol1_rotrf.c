#include "f77.h"
#include "polsub.h"
#include "ast.h"

F77_SUBROUTINE(pol1_rotrf)( INTEGER(NROW), INTEGER(NCOL), INTEGER(WCS),
                            INTEGER(TWCS), INTEGER(IFRM),
                            INTEGER(IAXIS), LOGICAL(VAR),
                            DOUBLE_ARRAY(QIN), DOUBLE_ARRAY(UIN),
                            DOUBLE_ARRAY(QOUT), DOUBLE_ARRAY(UOUT),
                            DOUBLE_ARRAY(QINV), DOUBLE_ARRAY(UINV),
                            DOUBLE_ARRAY(QOUTV), DOUBLE_ARRAY(UOUTV),
                            INTEGER(STATUS) ){
/*
*+
*  Name:
*     POL1_ROTRF

*  Purpose:
*     Rotate arrays of Q and U to refer to a different reference direction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_ROTRF( NROW, NCOL, WCS, TWCS, IFRM, IAXIS, VAR, QIN,
*                      UIN, QOUT, UOUT, QIN, UIN, QOUT, UOUT, STATUS )

*  Description:
*     The routine creates new Q and U values by rotating the reference
*     direction either to a specified axis within some specified Frame, or
*     so that each pixel uses the same reference direction as the
*     corresponding pixel in a supplied template NDF.

*  Arguments:
*     NROW = INTEGER (Given)
*        The number of rows of pixels in each Q/U map.
*     NCOL = INTEGER (Given)
*        The number of columns of pixels in each Q/U map.
*     WCS = INTEGER (Given)
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the Q (or U) array and a POLANAL Frame in
*        which the first axis defines the reference direction used by the
*        input Q and U values. It is assumed that the Q and U arrays use
*        identical FrameSets. Modified on exit to include a new POLANAL
*        Frame.
*     TWCS = INTEGER (Given)
*        A FrameSet containing a base Frame corresponding to GRID
*        coordinates within the tempate NDF and a POLANAL Frame in
*        which the first axis defines the reference direction used by the
*        template Q and U values. Modified on exit to include all the
*        Frames from WCS. If TWCS is AST__NULL, then the position angle
*        specified by arguments IFRM and IAXIS is used.
*     IFRM = INTEGER (Given)
*        Only used if TWCS is NULL. It is the one-based index of the
*        Frame within WCS that is used to define the required reference
*        direction.
*     IAXIS = INTEGER (Given)
*        Only used if TWCS is NULL. It is the zero-based index of the
*        axis within the Frame specified by IFRM that is used to define
*        the required reference direction.
*     VAR = _LOGICAL (Given)
*        If .TRUE., then the supplied variance arrays should be used.
*     QIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of Q data values.
*     UIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of U data values.
*     QOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of Q data values.
*     UOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of U data values.
*     QINV( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of Q variance values. Only used if VAR is
*        .TRUE.
*     UINV( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of U variance values. Only used if VAR is
*        .TRUE.
*     QOUTV( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of Q variance values. Only used if VAR is
*        .TRUE.
*     UOUTV( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of U variance values. Only used if VAR is
*        .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     26-JUN-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_INTEGER(NROW)
   GENPTR_INTEGER(NCOL)
   GENPTR_INTEGER(WCS)
   GENPTR_INTEGER(TWCS)
   GENPTR_INTEGER(IFRM)
   GENPTR_INTEGER(IAXIS)
   GENPTR_LOGICAL(VAR)
   GENPTR_DOUBLE_ARRAY(QIN)
   GENPTR_DOUBLE_ARRAY(UIN)
   GENPTR_DOUBLE_ARRAY(QOUT)
   GENPTR_DOUBLE_ARRAY(UOUT)
   GENPTR_DOUBLE_ARRAY(QINV)
   GENPTR_DOUBLE_ARRAY(UINV)
   GENPTR_DOUBLE_ARRAY(QOUTV)
   GENPTR_DOUBLE_ARRAY(UOUTV)
   GENPTR_INTEGER(STATUS)

   AstFrameSet *wcs = astI2P( *WCS );
   AstFrameSet *twcs = astI2P( *TWCS );

   if( F77_ISTRUE( *VAR ) ) {
      pol1Rotrf( *NROW, *NCOL, wcs, twcs, *IFRM, *IAXIS, QIN, UIN,
                 QOUT, UOUT, QINV, UINV, QOUTV, UOUTV, STATUS );
   } else {
      pol1Rotrf( *NROW, *NCOL, wcs, twcs, *IFRM, *IAXIS, QIN, UIN,
                 QOUT, UOUT, NULL, NULL, NULL, NULL, STATUS );
   }

}


