      SUBROUTINE ESP1_INPOS(INDF,PARAM,XB,YB,STATUS)
*+
*  Name:
*     ESP1_INPOS
*
*  Purpose:
*     Get a position within the NDF from the environment.
*
*  Language:
*     Starlink Fortran 77.
*
*  Invocation:
*     CALL ESP1_INPOS(INDF,PARAM,XB,YB,STATUS)
*
*  Description:
*     This routine takes an NDF identifier and the name of an ADAM
*     parameter, and returns a position in Base coordinates within the
*     bounds of the NDF.  The user inputs the values in the
*     coordinates of the Current frame of the NDF's WCS component.
*
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF which the coordinates describe.
*     PARAM = CHARACTER*(*) (Given)
*        ADAM _CHAR parameter through which to get the coordinates.
*     XB = REAL (Returned)
*        The X coordinate in Base coordinates of the NDF's WCS component.
*     YB = REAL (Returned)
*        The Y coordinate in Base coordinates of the NDF's WCS component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Notes:
*     Encapsulating the whole transformation in one routine like this
*     keeps things tidy, but it means that the extraction of the WCS
*     component from the NDF and of the mapping from the WCS component
*     may have to be done multiple times, while if it were done in
*     the calling routine it need only be done once.  However, the
*     time taken as a proportion of the overall running time for any
*     current ESP application is always(?) going to be negligable,
*     so this trade-off seems worthwhile.
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     25-OCT-1999 (MBT):
*        Original version.
*-

*  Type definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'AST_PAR'               ! Standard AST system constants
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER*(*) PARAM

*  Arguments Returned:
      REAL XB
      REAL YB

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER IWCS                    ! AST pointer to WCS component frameset
      INTEGER DIM(2)                  ! Dimensions of the NDF
      INTEGER NDIM                    ! Number of NDF dimensions
      DOUBLE PRECISION CC(2)          ! Coordinates in Current frame
      DOUBLE PRECISION BC(2)          ! Coordinates in Base frame
      DOUBLE PRECISION XMAX           ! Highest permissible value in X direction
      DOUBLE PRECISION XMIN           ! Lowest permissible value in X direction
      DOUBLE PRECISION YMAX           ! Highest permissible value in Y direction
      DOUBLE PRECISION YMIN           ! Lowest permissible value in Y direction
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Enter AST context.
      CALL AST_BEGIN(STATUS)

*   Get the World Coordinate System component of the NDF.
      CALL NDF_GTWCS(INDF,IWCS,STATUS)

*   Get the dimensions of the NDF.
      CALL NDF_DIM(INDF,2,DIM,NDIM,STATUS)
      XMIN=1D0
      YMIN=1D0
      XMAX=DBLE(DIM(1))
      YMAX=DBLE(DIM(2))

*   Get the coordinates from the parameter.
 10   CONTINUE
      CALL ESP1_GTPOS(PARAM,IWCS,CC,BC,STATUS)
      IF (STATUS.NE.SAI__OK) GO TO 99

*   Check that the coordinates given fall within the bounds of the NDF.
*   Note this check restricts the coordinates to be between the centre
*   of the lowest pixel and the centre of the highest pixel in each
*   dimension, i.e. not within half a pixel of the edge of the array.
*   It's not clear to me (MBT) that this is correct, but I'm
*   re-implementing the constraints in the same way that they have been
*   previously operating within ESP.
      IF (BC(1).LT.XMIN.OR.BC(1).GT.XMAX.OR.
     :    BC(2).LT.YMIN.OR.BC(2).GT.YMAX) THEN
         CALL MSG_SETR ('X', BC(1))
         CALL MSG_SETR ('Y', BC(2))
         CALL MSG_SETR ('XMIN', XMIN)
         CALL MSG_SETR ('XMAX', XMAX)
         CALL MSG_SETR ('YMIN', YMIN)
         CALL MSG_SETR ('YMAX', YMAX)
         CALL MSG_OUT(' ','inpos: coords (^X,^Y)'//
     :        ' outside NDF (^XMIN,^YMIN)..(^XMAX,^YMAX).',
     :        STATUS)
         CALL PAR_CANCL('ORIGIN',STATUS)
         GO TO 10
      END IF

*   Convert from Base coordinates to pixel coordinates.
      XB=REAL(BC(1))
      YB=REAL(BC(2))

*   Error exit label.
 99   CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

*   Return.
      END

