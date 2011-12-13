      SUBROUTINE CCD1_FITLM( X, Y, ID, OK, NXY, NIM, NALN, IFIT,
     :                       RELTOL, TR, XOUT, YOUT, IDOUT, XW, YW,
     :                       IDW, NOUT, RMS, STATUS )
*+
*  Name:
*     CCD1_FITLM

*  Purpose:
*     Determines the linear transformations between lists of positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FITLM ( X, Y, ID, OK, NXY, NIM, NALN, IFIT, TR,
*                       RELTOL, TR, XOUT, YOUT, IDOUT, XW, YW, IDW, NOUT,
*                       RMS, STATUS )

*  Description:
*     This routine finds a set of transformation coefficients which map
*     a set of different positions on to a single reference set of
*     positions. The method allows for all combinations of missing
*     positions.
*
*     The basic method is:
*     First the standard positions are identified using NALN and formed
*     into an initial reference set. Then for each additional list a
*     transformation is calculated between these and the reference set.
*     If this step is successful these positions are added to the
*     reference set to increase the number of positions with which to
*     align. When all the input lists have been processed, the
*     alignment transformations are corrected with the transformation
*     for the standard list such that this transformation becomes the
*     identity (0,1,0,0,0,1). The lists for which transformations have
*     been calculated are then used to estimate an RMS alignment error.
*     The whole process is then repeated using the enlarged reference
*     set until the alignment error converges within a set tollerence
*     or until the maximum number of iterations has been exceeded
*     (error) or until it is established that an list of positions
*     cannot be aligned (error).

*  Arguments:
*     X( * ) = DOUBLE PRECISION (Given)
*        Input X positions. These are used to calculate the alignment
*        transformations.
*     Y( * ) = DOUBLE PRECISION (Given)
*        Input Y positions. These are used to calculate the alignment
*        transformations.
*     ID( * ) = INTEGER (Given)
*        Identifiers for the input positions.
*     OK( * ) = LOGICAL (Given and Returned)
*        Workspace in which to store flags controlling the processing
*        of the input positions.
*     NXY( MAX( NALN, NIM ) ) = INTEGER (Given)
*        The number of entries in the position lists. Each set of
*        positions are added sequentially to the X and Y arrays. This
*        array holds the number of entries from each such list in the
*        order in which they were entered.
*     NIM = INTEGER (Given)
*        The number of input lists for which alignment transformations
*        are required. This may be a number less than the number of
*        sequences of coordinates entered into X and Y but must run
*        from 1 to NIM.
*     NALN = INTEGER (Given)
*        The number of the list to which all the other lists are to
*        aligned too. This
*        list is referred to as the standard list and can be one of
*        the lists 1 -> NIM or a separate reference list. In the
*        latter case positions corresponding to the reference list
*        should appear in the position lists after those to be aligned.
*     IFIT = INTEGER (Given and Returned)
*        An integer specifying the type of linear transformation
*        required to fit the data.
*        It's values represent transformations:
*          1 = shift of origin only
*          2 = shift of origin and rotation
*          3 = shift of origin and magnification
*          4 = shift of origin, rotation and magnification (solid body)
*          5 = full six parameter fit
*     RELTOL = DOUBLE PRECISION (Given)
*         Maximum relative change between iterations which must be
*         achieved before routine is considered to have converged.
*     TR( 6, MAX( NIM, NALN ) ) = DOUBLE PRECISION (Returned)
*        The alignment transformation which is determined for each
*        set of positions.
*     XOUT( * ) = DOUBLE PRECISION (Returned)
*        The list of X reference positions used to calculate the
*        alignment transformations on the last iteration. These
*        positions will have been transformed so that the alignment
*        transformation for the standard list is the identity
*        transformation.
*     YOUT( * ) = DOUBLE PRECISION (Returned)
*        The list of Y reference positions used to calculate the
*        alignment transformations on the last iteration. These
*        positions will have been transformed so that the alignment
*        transformation for the standard list is the identity
*        transformation.
*     IDOUT( * ) = INTEGER (Returned)
*        Identifiers for the reference positions.
*     XW( * ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for holding X positions. Must be the same size as X.
*     YW( * ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for holding Y positions. Must be the same size as Y.
*     IDW( * ) = INTEGER (Given and Returned)
*        Workspace for holding identifiers. Must be the same size as ID.
*     NOUT = INTEGER (Returned)
*        Number of positions in output lists.
*     RMS = DOUBLE PRECISION (Returned)
*        RMS alignment error on last iteration. The alignment error is
*        iteratively refined until it converges within a relative
*        tolerence (RELTOL) or until all the iterations are used up
*        (MAXIT), resulting in this value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The positions should be arranged consecutively in the input
*     lists (X, Y, ID). These lists will then be accessed using the
*     information in NXY which gives the number of positions
*     corresponding to each particular original list.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DUVAD::RFWS: R.F.Warren-Smith (Durham Polarimetry Group)
*     DUVAD::TMG: Tim Gledhill (Durham Polarimetry Group)
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     (DUVAD::RFWS):
*        Original version.
*     28-MAY-1992 (DUVAD::TMG):
*        Include ADAM status and error reporting.
*        Include standard prologue with description of routine.
*        No implicit typing.
*        Use MSG routines to output to user.
*        This routine now reports its own errors.
*        Improve comments.
*        Parameterise convergence tollerence.
*     24-JUL-1992 (PDRAPER):
*        Removed polarimetric references, ILEVEL argument. Changed
*        to process lists not paired left-right lists.
*     12-JUL-1993 (PDRAPER):
*        Added numeric handler for rms sums.
*     4-OCT-1996 (PDRAPER):
*        Changed LIB$ calls to NUM_.
*     1-NOV-1999 (MBT):
*        Removed output part (now done elsewhere).
*     22-FEB-2001 (MBT):
*        Removed LISTID parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION X( * )
      DOUBLE PRECISION Y( * )
      INTEGER ID( * )
      INTEGER NIM
      INTEGER NALN
      INTEGER NXY( * )
      DOUBLE PRECISION RELTOL

*  Arguments Given and Returned:
      LOGICAL OK( * )
      INTEGER IFIT
      DOUBLE PRECISION XW( * )
      DOUBLE PRECISION YW( * )
      INTEGER IDW( * )

*  Arguments Returned:
      DOUBLE PRECISION TR( 6, * )
      DOUBLE PRECISION XOUT( * )
      DOUBLE PRECISION YOUT( * )
      INTEGER IDOUT( * )
      INTEGER NOUT
      DOUBLE PRECISION RMS

*  Status:
      INTEGER STATUS             ! Global status

*  Global variables:

                                 ! defines NUM_ERROR

*  Local Constants:
      INTEGER MAXIT              ! maximum number of iterations to
                                 ! refine the alignment transformations.
      PARAMETER ( MAXIT = 50 )

*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numeric error trap
      EXTERNAL NUM_WASOK
      LOGICAL NUM_WASOK          ! Was numeric operation ok?

*  Local Variables:
      DOUBLE PRECISION CHANGE, TARGET ! change on last iteration and target
      DOUBLE PRECISION RMSSUM   ! sum for RMS error
      DOUBLE PRECISION TR1( 6 ) ! identity transformation
      DOUBLE PRECISION TRINV( 6 ) ! inverse transformation
      DOUBLE PRECISION TRS( 6 ) ! Spare transformation buffer
      DOUBLE PRECISION XX, YY   ! transformed positions
      INTEGER I, J, K           ! loop counters
      INTEGER IIFIT             ! fit type
      INTEGER IP                ! pointer into lists
      INTEGER ITER              ! iteration counter line
      INTEGER NMTCH             ! number of matched positions
      INTEGER NSUM              ! for calculating RMS error
      LOGICAL ALDONE            ! transformations have been calculated
                                ! for all the lists?
      LOGICAL CONVRG            ! the iterations to refine the
                                ! transformations have converged?
      LOGICAL FAILED            ! the routine has failed because one
                                ! of the lists could not be aligned?
      LOGICAL FITOK             ! a transformation has been
                                ! calculated for this list?
      LOGICAL STOP              ! Stop processing numeric error has
                                ! occurred


*  Local Data:
                                 ! identity transformation.
      DATA TR1 / 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0 /

*.

*  Check the status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the numeric error handler
      CALL NUM_HANDL( NUM_TRAP )

*  Initiallize the transformation for the standard list.
      DO 1 I = 1, 6
         TR( I, NALN ) = TR1( I )
    1 CONTINUE

*  Count through the positions, initiallizing flags for positions used.
*  Initially use only the standard list.
      IP = 0
      DO 2 I = 1, MAX( NALN, NIM )
         DO 3 J = 1, NXY( I )
            IP = IP + 1
            OK( IP ) = ( I .EQ. NALN )

*  Transfer the input positions to the workspace.
            XW( IP ) = X( IP )
            YW( IP ) = Y( IP )
            IDW( IP ) = ID( IP )
 3       CONTINUE
 2    CONTINUE

*  Initiallize the flags for terminating iterations.
      ITER = 0
      ALDONE = .FALSE.
      CONVRG = .FALSE.
      FAILED = .FALSE.
      STOP = .FALSE.

*  Perform iterations to refine the transformations.
    4 CONTINUE
      IF (  ( ( .NOT. ALDONE ) .OR.  ( .NOT. CONVRG ) ) .AND.
     :        ( ITER .LT. MAXIT ) .AND. ( .NOT. FAILED )  .AND.
     :        ( .NOT.STOP ) ) THEN
         ITER = ITER + 1

*  Combine all the currently aligned positions into a single set of
*  positions (to which the remaining positions are to be aligned). Note
*  that this routine applies the transformation to get the flagged
*  positions into the reference frame.
         CALL CCD1_MXYL( XW, YW, IDW, OK, NXY, TR, MAX( NALN, NIM ),
     :                   XOUT, YOUT, IDOUT, NOUT, STATUS )

*  Scan through each list in turn, setting a pointer to the start of
*  the positions
         IP = 1
         ALDONE = .TRUE.
         DO 5 J = 1, MAX( NALN, NIM )
            IF ( J .GT. 1 ) IP = IP + NXY( J - 1 )

*  Sort the positions to match the reference set.
            CALL CCD1_MTCHL( IDOUT, XOUT, YOUT,
     :                       NOUT, IDW( IP ), XW( IP ), YW( IP ),
     :                       NXY( J ), NMTCH, STATUS )

*  Reset the flags.
            DO 7 K = 1, NXY( J )
               OK( IP + K - 1 ) = .FALSE.
 7          CONTINUE

*  If any positions matched the reference set find the transformation
*  required to align with the reference positions.
            FITOK = .FALSE.
            IF ( NMTCH .GE. 1 ) THEN
               DO 8 K = 1, NMTCH
                  OK( IP + K - 1 ) = .TRUE.
 8             CONTINUE
               IIFIT = IFIT
               CALL CCD1_DTRN( XW( IP ), YW( IP ), XOUT, YOUT,
     :                         OK( IP ), NMTCH, IIFIT, TR( 1, J ),
     :                         STATUS )
               DO 9 K = 1, NMTCH
                  OK( IP + K - 1 ) = .FALSE.
 9             CONTINUE

*  If the fit was successfully obtained set flags to indicate these
*  positions can contribute to the reference set in the next iteration.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  FITOK = .TRUE.
                  DO 10 K = 1, NXY( J )
                     OK( IP + K - 1 ) = .TRUE.
 10               CONTINUE
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

*  Note if all the lists could be aligned this iteration.
            ALDONE = ALDONE .AND. FITOK
 5       CONTINUE

*  Find the inverse transformation between the standard list and the
*  set of reference positions, then transform the reference set so this
*  transformation becomes the identity.
         CALL CCD1_LINV( TR( 1, NALN ), TRINV, STATUS )

*  Transform the positions
         CALL CCD1_LXYT3( XOUT, YOUT, NOUT, TRINV, STATUS )

*  Apply this inverse as a correction to all the other transformations.
         DO 11 I = 1, MAX( NALN, NIM )
            DO 17 J = 1, 6
               TRS( J ) = TR( J, I )
 17         CONTINUE
            CALL CCD1_LTRCM( TRS, TRINV, TR( 1, I ), STATUS )
 11      CONTINUE

*  Now scan through the positions to find the rms alignment error.
         RMSSUM = 0.0
         NSUM = 0
         IP = 1
         DO 12 I = 1, MAX( NALN, NIM )
            IF ( I .GE. 2 ) IP = IP + NXY( I - 1 )

*  Only include those lists which have been aligned so far.
            IF ( OK( IP ) .AND. NXY( I ) .GT. 0 ) THEN

*  Sort the reference set to match each set of positions.
               CALL CCD1_MTCHL( ID( IP ), X( IP ), Y( IP ), NXY( I ),
     :                          IDOUT, XOUT, YOUT, NOUT, NMTCH, STATUS )

*  Transform the positions to the reference frame and find the
*  alignment error.
               DO 13 J = 1, NMTCH
                  CALL CCD1_LXYT2( X( IP + J - 1 ), Y( IP + J - 1 ), 1,
     :                           TR( 1, I ), XX, YY, STATUS )
                  RMSSUM = RMSSUM + ( XOUT( J ) - XX ) ** 2
     :                            + ( YOUT( J ) - YY ) ** 2
                  NSUM = NSUM + 1
 13            CONTINUE
            ENDIF
 12      CONTINUE
         RMSSUM = SQRT( RMSSUM / MAX( NSUM, 1 ) )

*  The process has converged if the fractional change in the rms
*  alignment error is sufficiently small. This should only be calculated
*  if all the lists have been included in the reference set. This is
*  indicated by the ALDONE flag and means that each list has at least
*  one position in common with the reference set, which could occur on
*  the first iteration. However, by completing MAX( NALN, NIM ) - 1
*  iterations before calculating the convergence it is ensured that all
*  the positions on each list which can contribute to the calculations
*  do so.
         IF ( ITER .GE. ( MAX( NALN, NIM ) - 1 ) ) THEN
            CHANGE = ABS( RMSSUM - RMS )
            TARGET = MAX( RELTOL * RMSSUM, RELTOL )
            CONVRG = CHANGE .LE. TARGET
         ENDIF
         RMS = RMSSUM

*  The routine has failed if all the lists are not included in the
*  reference set after MAX( NALN, NIM ) - 1 iterations since after this
*  number of iterations all the possibilities for resolving missing
*  positions on lists are exhausted.
         FAILED = ( ITER .GE. ( MAX( NALN, NIM ) - 1 ) .AND.
     :            ( .NOT. ALDONE ) )

*  Check for any numeric errors (overflows have occurred occasionally)
         IF ( .NOT. NUM_WASOK() ) THEN
            CALL NUM_GETERR()
            CALL ERR_REP( 'CCD1_FITLM_NERR',
     :      'CCD1_FITLM: Numeric error during fit.', STATUS )
            STOP = .TRUE.
         END IF
         GO TO 4
      ENDIF

*  On exit from the iteration loop, if the routine has failed because an
*  list could not be aligned, return an error message indicating the
*  offending list.
      IF ( FAILED ) THEN
         IP = 1
         DO 14 I = 1, MAX( NALN, NIM )
            IF ( I .GE. 2 ) IP = IP + NXY( I - 3 ) + NXY( I - 2 )
            IF ( .NOT. OK( IP ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'LISTID', I )
               CALL ERR_REP( 'CCD1_FITLM_FAIL', '  Could not '//
     :         'align list ^LISTID)', STATUS )
               GO TO 15
            ENDIF
 14      CONTINUE
 15      CONTINUE

*  If the iterations have not converged give an error message.
         IF ( .NOT. CONVRG ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'CHANGE', CHANGE )
            CALL MSG_SETD( 'TARGET', TARGET )
            CALL ERR_REP( 'CCD1_FITLM_NCNVRG', '  Iterations '//
     :      'to calculate the transformations have not converged, '//
     :      'change on last iteration was ^CHANGE with target of '//
     :      '^TARGET', status )
         ENDIF
      ENDIF

*  Cancel the condition handler.
      CALL NUM_REVRT
      END
* $Id$
