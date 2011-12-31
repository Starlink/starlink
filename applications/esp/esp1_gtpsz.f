      SUBROUTINE ESP1_GTPSZ(INDF,PSIZE,STATUS)
*+
*  Name:
*     ESP1_GTPSZ
*
*  Purpose:
*     Get the value of the PSIZE parameter.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL ESP1_GTPSZ(INDF,PSIZE,STATUS)
*
*  Description:
*     This routine gets a value for the PSIZE ADAM parameter, which
*     represents pixel size in arcseconds.  If the parameter is in the
*     ACTIVE state (presumably because it has been specified on the
*     command line) then its current value will be used.  Otherwise,
*     the routine will search for a frame in the SKY domain in the
*     WCS component of the NDF given by the INDF argument, and if
*     one exists it will work out a value from this.  Failing either
*     of these, the user will be prompted for a value.
*
*     The pixels are assumed to be square.  If the WCS component
*     indicates that they are in fact not square (have an aspect ratio
*     outside the range 0.95-1.05) then the user is warned of this,
*     but no further action is taken.
*
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF whose pixel size is to be determined.
*     PSIZE = REAL (Returned)
*        The width (and height) of each pixel in arcseconds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
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
      INCLUDE 'PAR_PAR'               ! Standard PAR system constants
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      REAL PSIZE

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER IPERM(2)                ! Indices for PermMap
      INTEGER IWCS                    ! AST pointer to WCS component of NDF
      INTEGER JGRID                   ! Index of GRID-domain frame in frameset
      INTEGER JSKY                    ! Index of SKY-domain frame in frameset
      INTEGER DIM(2)                  ! Lower bounds of NDF
      INTEGER FSET                    ! AST pointer to conversion frameset
      INTEGER GSMAP                   ! AST pointer to GRID->SKY frame mapping
      INTEGER MAP                     ! AST pointer to normalised mapping
      INTEGER NDIM                    ! Number of dimensions of NDF
      INTEGER OPERM(2)                ! Indices for PermMap
      INTEGER PMAP                    ! AST pointer to permutation mapping
      INTEGER SKYFRM                  ! AST pointer to SkyFrame
      INTEGER STATE                   ! Initial state of PSIZE parameter
      LOGICAL INOKAY                  ! Have we got a good value for PSIZE?
      LOGICAL SWAP                    ! Have SkyFrames axes been swapped?
      DOUBLE PRECISION ARCSEC         ! Size of an arcsecond in radians
      DOUBLE PRECISION CENX           ! X position of NDF centre in Sky coords
      DOUBLE PRECISION CENY           ! Y position of NDF centre in Sky coords
      DOUBLE PRECISION DISTY          ! Y dimension of transformed box in radian
      DOUBLE PRECISION PI             ! Pi
      DOUBLE PRECISION TRANX(2)       ! X coords of transformed points
      DOUBLE PRECISION TRANY(2)       ! Y coords of transformed points
      DOUBLE PRECISION UNITX(2)       ! X coords of untransformed points
      DOUBLE PRECISION UNITY(2)       ! Y coords of untransformed points

      DATA CENX,CENY /0D0,0D0/        ! Prevent arithmetic exceptions in
      DATA TRANX /0D0,0D0/            ! event of failure.
      DATA TRANY /0D0,0D0/            !
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   See if the PSIZE parameter is in the ACTIVE state.  If so, use this
*   value.  Otherwise we try to derive it automatically.

*   Find out what state the PSIZE parameter is in.
      STATE=PAR__GROUND
      CALL PAR_STATE('PSIZE',STATE,STATUS)

*   If it is in the ACTIVE state (presumably because it has been set on
*   the command line) then we use its set value.
      IF (STATE.EQ.PAR__ACTIVE) THEN
         CALL PAR_GET0R('PSIZE',PSIZE,STATUS)

*   Otherwise we try to find its value from the WCS component of the NDF.
      ELSE

*      Start an AST context.
         CALL AST_BEGIN(STATUS)

*      Get the WCS component of the NDF.
         CALL NDF_GTWCS(INDF,IWCS,STATUS)

*      Get indices for GRID- and SKY-domain frames from the WCS component.
         CALL ESP1_ASFFR(IWCS,'GRID',JGRID,STATUS)
         CALL ESP1_ASFFR(IWCS,'SKY',JSKY,STATUS)

*      Get the SKY domain frame and check it is a SkyFrame (otherwise
*      it's been mislabelled and we can't use it).
         IF (JSKY.NE.AST__NOFRAME) THEN
            SKYFRM=AST_GETFRAME(IWCS,JSKY,STATUS)
            IF (.NOT.AST_ISASKYFRAME(SKYFRM,STATUS)) SKYFRM=AST__NULL
         ELSE
            SKYFRM=AST__NULL
         END IF

*      If there is a sky frame, use it to work out pixel size.
         IF (SKYFRM.NE.AST__NULL .AND. JGRID.NE.AST__NOFRAME) THEN

*         Work out whether the SkyFrame has had its axes swapped over.
*         Normally the first axis is longitude and the second is
*         latitude.  However it may have had them swapped over.
*         This code is pinched from KAPPA, where it is commented that
*         doing this is more complicated than it ought to be.
*         We compare it with a newly created SkyFrame to see if the
*         axes have been swapped.
            FSET=AST_FINDFRAME(SKYFRM,AST_SKYFRAME(' ',STATUS),' ',
     :                         STATUS)
            SWAP=.NOT.AST_ISAUNITMAP(AST_GETMAPPING(FSET,AST__BASE,
     :                                              AST__CURRENT,
     :                                              STATUS),
     :                               STATUS)

*         Construct a suitable PermMap (i.e. one which either does or
*         does not switch the Sky axes around) so that in either case
*         the mapping we have looks like it's to a SkyFrame with axes
*         in the conventional order.
            IF (SWAP) THEN
               IPERM(1)=2
               IPERM(2)=1
            ELSE
               IPERM(1)=1
               IPERM(2)=2
            END IF
            OPERM(1)=IPERM(1)
            OPERM(2)=IPERM(2)
            PMAP=AST_PERMMAP(2,IPERM,2,OPERM,0D0,' ',STATUS)

*         Get the mapping from the GRID domain to the SKY domain frames.
            GSMAP=AST_GETMAPPING(IWCS,JGRID,JSKY,STATUS)

*         Combine and simplify the mappings so we have one which maps
*         from the GRID domain to a normalised (latitude=second axis)
*         SkyFrame.
            MAP=AST_SIMPLIFY(AST_CMPMAP(GSMAP,PMAP,.TRUE.,' ',STATUS),
     :                       STATUS)

*         Find the bounds of the NDF.
            CALL NDF_DIM(INDF,2,DIM,NDIM,STATUS)

*         Find the centre of the NDF in SKY coordinates.
            CALL AST_TRAN2(MAP,1,DBLE(DIM(1)/2),DBLE(DIM(2)/2),.TRUE.,
     :                     CENX,CENY,STATUS)

*         Get the size of an arcsecond in radians.
            PI=4D0*ATAN(1D0)
            ARCSEC=2D0*PI/(36D1*6D1*6D1)

*         Construct two points an arcsecond away from each other in
*         latitude near the centre of the NDF.
            UNITX(1)=CENX
            UNITY(1)=CENY
            UNITX(2)=CENX
            UNITY(2)=CENY+ARCSEC

*         Transform the points from SKY to GRID coordinates.
            CALL AST_TRAN2(MAP,2,UNITX,UNITY,.FALSE.,TRANX,TRANY,STATUS)

*         Work out the distance in pixels that one arcsecond in latitude
*         transforms to.
            DISTY=SQRT((TRANX(2)-TRANX(1))**2 + (TRANY(2)-TRANY(1))**2)

*         Get the pixel size in arcseconds.
            PSIZE=1D0/DISTY

*         Inform the user what size the pixels are.
            CALL MSG_FMTR('PSIZE','G10.3',PSIZE)
            CALL MSG_OUT(' ','Using info from SKY frame - pixels are '
     :                   //'^PSIZE arcseconds square.',STATUS)
            CALL MSG_BLANK(STATUS)

*   The WCS component was not suitable for working out the pixel size.
*   Ask the user to enter it instead.  We require it to be at least
*   a micro-arcsecond; if this is altered the magic value of a
*   1e-6 arcsec in gaufit must be modified also.
         ELSE
            INOKAY=.FALSE.
            DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
               CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
               IF (PSIZE.LT.1E-6.AND.STATUS.EQ.SAI__OK) THEN
                  CALL MSG_OUT(' ','The pixel size supplied is too '
     :                         //'small.',STATUS)
                  CALL PAR_CANCL('PSIZE',STATUS)
               ELSE
                  INOKAY=.TRUE.
               END IF
            END DO
         END IF

*      End the AST context.
         CALL AST_END(STATUS)

      END IF

*   Return.
      END

