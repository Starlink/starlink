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
      CHARACTER*(AST__SZCHR) DOMAIN   ! Domain of frame in WCS component
      INTEGER FRAME                   ! AST pointer to frame in frameset
      INTEGER I                       ! Loop index
      INTEGER IWCS                    ! AST pointer to WCS component of NDF
      INTEGER JGRID                   ! Index of GRID-domain frame in frameset
      INTEGER JSKY                    ! Index of SKY-domain frame in frameset
      INTEGER MAP                     ! AST pointer to GRID->SKY mapping
      INTEGER STATE                   ! Initial state of PSIZE parameter
      LOGICAL INOKAY                  ! Have we got a good value for PSIZE?
      DOUBLE PRECISION ASRAD          ! Number of arcseconds in a radian
      DOUBLE PRECISION DISTX          ! X dimension of transformed box in radian
      DOUBLE PRECISION DISTY          ! Y dimension of transformed box in radian
      DOUBLE PRECISION PI             ! Pi
      DOUBLE PRECISION PSZX           ! X dimension of transformed box in arcsec
      DOUBLE PRECISION PSZY           ! Y dimension of transformed box in arcsec
      DOUBLE PRECISION SHAPE          ! Aspect ratio of transformed box
      DOUBLE PRECISION TRANX(4)       ! X coords of transformed box
      DOUBLE PRECISION TRANY(4)       ! Y coords of transformed box
      DOUBLE PRECISION UNITX(4)       ! X coords of box in GRID-domain frame
      DOUBLE PRECISION UNITY(4)       ! Y coords of box in GRID-domain frame

      DATA UNITX /0D0,1D0,0D0,1D0/    ! X coords of unit box
      DATA UNITY /0D0,0D0,1D0,1D0/    ! Y coords of unit box
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

*      Get the pixel size.
         CALL NDF_GTWCS(INDF,IWCS,STATUS)

*      Get indices for GRID- and SKY-domain frames from the WCS component.
*      There may be no SKY one.  Work backwards just in case there is a 
*      spurious duplicate frame in the GRID domain.
         JSKY=0
         JGRID=0
         DO 10 I=AST_GETI(IWCS,'Nframe',STATUS),1,-1
            FRAME=AST_GETFRAME(IWCS,I,STATUS)
            DOMAIN=AST_GETC(FRAME,'Domain',STATUS)
            IF (STATUS.EQ.SAI__OK .AND. DOMAIN.EQ.'SKY') JSKY=I
            IF (STATUS.EQ.SAI__OK .AND. DOMAIN.EQ.'GRID') JGRID=I
 10      CONTINUE

*      If there is a sky frame, use it to work out pixel size.
         IF (JSKY.GT.0 .AND. JGRID.GT.0) THEN
         
*         Get the mapping from the GRID domain to the SKY domain frames.
            MAP=AST_GETMAPPING(IWCS,JGRID,JSKY,STATUS)

*         Transform a unit box (pixel) from GRID to SKY coordinates.  We use 
*         a pixel near the origin - one near the middle of the NDF would 
*         perhaps be better but the difference is going to be very tiny.
            CALL AST_TRAN2(MAP,4,UNITX,UNITY,.TRUE.,TRANX,TRANY,STATUS)

*         Work out the distance in radians that unit X and Y lengths
*         transform to.
            DISTX=SQRT((TRANX(2)-TRANX(1))**2 + (TRANY(2)-TRANY(1))**2)
            DISTY=SQRT((TRANX(3)-TRANX(1))**2 + (TRANY(3)-TRANY(1))**2)

*         Get radian to arcsecond conversion factor.
            PI=4D0*ATAN(1D0)
            ASRAD=36D1*6D1*6D1/2D0/PI

*         Get the pixel dimensions in arcseconds.
            PSZX=DISTX*ASRAD
            PSZY=DISTY*ASRAD

*         Check that pixels are of similar size in the X and Y directions.
*         If not we can't do anything much about it, but the user should be
*         warned.
            SHAPE=PSZX/PSZY
            IF (SHAPE.LT.0.95D0 .OR. SHAPE.GT.1.05D0) THEN
               CALL MSG_OUT(' ','WARNING!',STATUS)
               CALL MSG_FMTD('X','G9.2',PSZX)
               CALL MSG_FMTD('Y','G9.2',PSZY)
               CALL MSG_OUT(' ','   Pixels are not square:^X arcsec'
     :                      //' in X and^Y arcsec in Y',STATUS)
               CALL MSG_OUT(' ','   - using average.',STATUS)
               CALL MSG_BLANK(STATUS)
            END IF

*         Average over size in X and size in Y in any case.
            PSIZE=0.5*REAL(PSZX+PSZY)

*         Inform the user what size the pixels are.
            CALL MSG_FMTR('PSIZE','G9.2',PSIZE)
            CALL MSG_OUT(' ','Using info from SkyFrame - pixels are '
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

