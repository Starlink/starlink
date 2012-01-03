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
*     The pixels are assumed to be square.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*
*  History:
*     25-OCT-1999 (MBT):
*        Original version.
*     30-DEC-2011 (TIMJ):
*        Use KPG1_SCALE for pixel scale calculation rather than
*        calculating the value directly in ESP.
*     3-JAN-2012 (DSB):
*        KPG1_SCALE requires double precision (not integer) bounds.

*  Copyright:
*     Copyright (C) 2011-2012 Science & Technology Facilities Council.
*     Copyright (C) 1999 Council for the Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
      INTEGER IWCS                    ! AST pointer to WCS component of NDF
      INTEGER JGRID                   ! Index of GRID-domain frame in frameset
      INTEGER JSKY                    ! Index of SKY-domain frame in frameset
      INTEGER LBND(2)                 ! Lower bounds of NDF
      INTEGER NDIM                    ! Number of dimensions of NDF
      INTEGER SKYFRM                  ! AST pointer to SkyFrame
      INTEGER STATE                   ! Initial state of PSIZE parameter
      INTEGER UBND(2)                 ! Upper bounds of NDF
      LOGICAL INOKAY                  ! Have we got a good value for PSIZE?
      DOUBLE PRECISION DLBND(2)       ! Double precision lower bounds
      DOUBLE PRECISION DUBND(2)       ! Double precision upper bounds
      DOUBLE PRECISION SCALE(2)       ! Pixel scales in radians
      CHARACTER *15 UNIT(2)           ! Unit string of each axis
      CHARACTER *15 VALUE(2)          ! String form of pixel scales

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

*         Get the double precision pixel coordinate bounds of the NDF
            CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
            DLBND( 1 ) = DBLE( LBND( 1 ) ) - 1.0D0
            DUBND( 1 ) = DBLE( UBND( 1 ) )
            DLBND( 2 ) = DBLE( LBND( 2 ) ) - 1.0D0
            DUBND( 2 ) = DBLE( UBND( 2 ) )

            CALL KPG1_SCALE( IWCS, DLBND, DUBND, SCALE, VALUE,
     :           UNIT, STATUS )

*         Get the mean pixel size in arcseconds.
            PSIZE= AST__DR2D * 3600.0D0 * (SCALE(1) + SCALE(2)) / 2.0D0

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

