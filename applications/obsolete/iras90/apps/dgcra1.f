      SUBROUTINE DGCRA1( NSMP, NDET, DATA, BOXSZ, NITER, CLIP, OUT,
     :                   WORK1, WORK2, WORK3, STATUS )
*+
*  Name:
*     DGCRA1

*  Purpose:
*     Reject samples staying too far away from local average.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DGCRA1( NSMP, NDET, DATA, BOXSZ, NITER, CLIP, OUT,
*                  WORK1, WORK2, WORK3, STATUS )

*  Description:
*     This subroutine rejects the those samples in the input data array
*     which deviate too far away from their local averag. The average
*     will be performed by a smoothing box filter along each row of the
*     input data array. The RMS residual of all samples in the box is
*     obtained. Any sample which is away from the average more than CLIP
*     time the RMS will be rejected, set as BAD. Such average-reject
*     procedure will be performed NITER time.

*  Arguments:
*     NSMP, NDET = INTEGER (Given)
*        Number of samples and number of detectors in the input array.
*     DATA( NSMP, NDET ) = REAL (Given)
*        Input data array.
*     BOXSZ = INTEGER (Given)
*        Size of the smoothing filter in samples.
*     NITER = INTEGER (Given)
*        Number of average-reject iterations
*     CLIP( NITER ) = REAL (Given)
*        The times of the RMS more than this dose a sample deviate
*        from its local average will be rejected (set as BAD in the
*        output array) in each iteration.
*     OUT( NSMP, NDET ) = REAL (Returned)
*        The output array.
*     WORK1( NSMP) = REAL (Returned)
*        The temporary working space.
*     WORK2( NSMP ) = REAL (Returned)
*        The temporary working space.
*     WORK3( NSMP ) = INTEGER (Returned)
*        The temporary working space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     26-MAY-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NSMP, NDET
      REAL DATA( NSMP, NDET )
      INTEGER BOXSZ
      INTEGER NITER
      REAL CLIP( NITER )

*  Arguments Returned:
      REAL OUT( NSMP, NDET )
      REAL WORK1( NSMP )
      REAL WORK2( NSMP )
      INTEGER WORK3( NSMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BOX( 2 )           ! Smoothing box
      INTEGER DETX               ! Detector index
      INTEGER I                  ! Do loop index
      INTEGER ILEVEL             ! Information report level
      REAL INVAR( 1 ), OUTVAR( 1 ) ! Dummy input and output variance
      REAL LIMITS( 2 )           ! Data range limits
      INTEGER NVAR               ! Number of element in variance
      INTEGER NLIM               ! Valid sample limit
      INTEGER NGOOD              ! Number of valid samples after reject
      LOGICAL SAMBAD             ! BAD sample Propagating flag
      DOUBLE PRECISION SIGMA     ! RMS of the row after rejecting
      LOGICAL VAR                ! Variance component flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  To use subroutine IRM_CFF2D,( originally KPS1_CFF2D in Kappa) some
*  arguments of IRM_CFF2D are irrelavent to this routine have to assign
*  particular values.
*  No variance component associated with the data.
      VAR = .FALSE.
      NVAR = 1
      INVAR( 1 ) = 1.0
      OUTVAR( 1 ) = 1.0

*  Doing smoothing row by row, the filter box is an one dimension box.
      BOX( 1 ) = BOXSZ
      BOX( 2 ) = 1

*  All data in the array will be considered.
      LIMITS( 1 ) = VAL__MINR
      LIMITS( 2 ) = VAL__MAXR

*  Do not produce and message.
      ILEVEL = 1

*  Bad samples in the input array will be propagate to the output.
      SAMBAD = .TRUE.

*  Only one valid sample is necessary in the smoothing box.
      NLIM = 1

*  Begin a error context.
      CALL ERR_MARK

*  Processing the input data array row by row.
      DO I = 1, NDET
         DETX = I
         CALL IRM_CFF2R( NSMP, 1, DATA( 1, DETX ), VAR, NVAR, INVAR,
     :                    BOX, NITER, CLIP, LIMITS, ILEVEL, SAMBAD,
     :                    NLIM, WORK1( 1 ), OUT( 1, DETX ), OUTVAR,
     :                    NGOOD, SIGMA, WORK2, WORK3, STATUS )

*  Ignor the error produced by IRM_CFF2R, and continue the processing.
         IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END DO

*  Release the error context
      CALL ERR_RLSE

      END
