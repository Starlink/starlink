


      SUBROUTINE ELF1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
*+
*  Name:
*     ELF1_GRBIT

*  Purpose:
*     Generates the graphics required when the galaxy origin and maximum
*     permitted ellipse radius are specified by the user and when the
*     quadrant to be used for the results graph is cleared.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*  Description:
*     Generates the graphics showing the gaaxy origin and the maximum
*     permitted ellipse radius as specified by the user.
*     Employs SGS to do so.
*
*     The variable MODE defines what is to be drawn: ie
*
*        MODE=0  Open file, save results and close file in one go
*        MODE=1  Draw the cross at the centre of the chosen sector
*        MODE=2  Draw the circle showing the radius limit for profiling
*        MODE=3  Draw the window within which the results graph will be
*                  displayed


*  Arguments:
*     MODE = INTEGER (Given)
*        Indicates what sort of graphics output is to take place.
*     COLOUR = INTEGER (Given)
*        Colour of the pen marking the galaxy centre.
*     CURSIZ = REAL (Given)
*        The size of the cross to be drawn at the first point specified.
*     X(10) = REAL (Given)
*        X co-ordinates derived from the input cursor positions.
*     Y(10) = REAL (Given)
*        Y co-ordinates derived from the input cursor positions.
*     RLIM = REAL (Given)
*        Sampling radius maximum. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     06-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER COLOUR                  ! Pen colour used to show galaxy marker
      INTEGER MODE                    ! Which part of the sector drawing
                                      ! is to take place.
      REAL CURSIZ                     ! Size of the cross to be drawn at
                                      ! the first point specified.
      REAL RLIM                       ! Sampling radius maximum
      REAL X(10)                      ! X co-ordinates for various parts of
                                      ! the sector to be drawn.
      REAL Y(10)                      ! Y co-ordinates for various parts of
                                      ! the sector to be drawn.

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Draw the cross at the centre of the indicated galaxy.
      IF (MODE.EQ.1) THEN
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
         CALL SGS_LINE(X(1),Y(1),X(1),Y(1))
      END IF

*   Draw a circle defining the maximum radius required..
      IF (MODE.EQ.3) THEN
         TEMP1=0.0
         TEMP2=2.*ELF__PIVAL
         CALL SGS_SPEN(COLOUR)
         CALL SGS_ARC(X(1),Y(1),RLIM,TEMP1,TEMP2)
      END IF

*   Clear the quadrant of the window where the results will be displayed
*   and then draw a border around it.
      IF (MODE.EQ.7) THEN
         CALL SGS_SPEN(1)
         CALL SGS_CLRBL(X(6),X(7),Y(6),Y(7))
         CALL SGS_BOX(X(6),X(7),Y(6),Y(7))
      END IF

*   Flush any SGS errors.
 9999 CALL SGS_FLUSH

      END



      SUBROUTINE ELP1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
*+
*  Name:
*     ELP1_GRBIT

*  Purpose:
*     Generates the graphics required when the galaxy origin and maximum
*     permitted ellipse radius are specified by the user and when the
*     quadrant to be used for the results graph is cleared.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*  Description:
*     Generates the graphics showing the gaaxy origin and the maximum
*     permitted ellipse radius as specified by the user.
*     Employs SGS to do so.
*
*     The variable MODE defines what is to be drawn: ie
*
*        MODE=0  Open file, save results and close file in one go
*        MODE=1  Draw the cross at the centre of the chosen sector
*        MODE=2  Draw the circle showing the radius limit for profiling
*        MODE=3  Draw the window within which the results graph will be
*                  displayed


*  Arguments:
*     MODE = INTEGER (Given)
*        Indicates what sort of graphics output is to take place.
*     COLOUR = INTEGER (Given)
*        Colour of the pen used to draw the galaxy centre marker.
*     CURSIZ = REAL (Given)
*        The size of the cross to be drawn at the first point specified.
*     X(10) = REAL (Given)
*        X co-ordinates derived from the input cursor positions.
*     Y(10) = REAL (Given)
*        Y co-ordinates derived from the input cursor positions.
*     RLIM = REAL (Given)
*        Radius of largest permitted ellipse. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     06-Jan-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER COLOUR                  ! Galaxy marker colour
      INTEGER MODE                    ! Which part of the sector drawing
                                      ! is to take place.
      REAL CURSIZ                     ! Size of the cross to be drawn at
                                      ! the first point specified.
      REAL RLIM                       ! Radius of the largest permitted ellipse
      REAL X(10)                      ! X co-ordinates for various parts of
                                      ! the sector to be drawn.
      REAL Y(10)                      ! Y co-ordinates for various parts of
                                      ! the sector to be drawn.

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Draw the cross at the centre of the indicated galaxy.
      IF (MODE.EQ.1) THEN
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
      END IF

*   Draw a circle defining the maximum radius required..
      IF (MODE.EQ.3) THEN
         TEMP1=0.0
         TEMP2=2.*ELP__PIVAL
         CALL SGS_SPEN(COLOUR)
         CALL SGS_ARC(X(1),Y(1),RLIM,TEMP1,TEMP2)
      END IF

*   Clear the quadrant of the window where the results will be displayed
*   and then draw a border around it.
      IF (MODE.EQ.7) THEN
         CALL SGS_SPEN(1)
         CALL SGS_CLRBL(X(6),X(7),Y(6),Y(7))
         CALL SGS_BOX(X(6),X(7),Y(6),Y(7))
      END IF

*   Flush any SGS errors.
 9999 CALL SGS_FLUSH

      END


      SUBROUTINE SEC1_GRBIT(MODE,CURSIZ,X,Y,POSANG,
     :                      ANGWID,RADIUS,COLOUR,STATUS)
*+
*  Name:
*     SEC1_GRBIT

*  Purpose:
*     Generates the graphics required when the sector is specified by
*     the user and when the quadrant to be used fro the results graph is
*     cleared.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_GRBIT(MODE,CURSIZ,X,Y,POSANG,ANGWID,RADIUS,COLOUR,STATUS)

*  Description:
*     Generates the graphics showing the sector specified by the user.
*     Employs SGS to do so.
*
*     The variable MODE defines whcih part of the sector is to be
*     drawn: ie
*
*        MODE = 1  Draw the cross at the centre of the chosen sector
*        MODE = 2  Draw a line outward from the sector centre to its
*                  edge in the direction specified
*        MODE = 3  Not implemented (no drawing required)
*        MODE = 4  Draw the sector to be used and highlight its centre
*                  and the ends of the arc
*        MODE = 5  As for MODE=4 but for the equivalent sector placed
*                  at an angle increased by 180 degrees
*        MODE = 6  Not implemented
*        MODE = 7  Draw the window within which the results graph will be
*                  displayed


*  Arguments:
*     MODE = INTEGER (Given)
*        Indicates which part of the sector drawing is to take place.
*     CURSIZ = REAL (Given)
*        The size of the cross to be drawn at the first point specified.
*     X(10) = REAL (Given)
*        X co-ordinates for various parts of the sector to be drawn.
*     Y(10) = REAL (Given)
*        Y co-ordinates for various parts of the sector to be drawn.
*     POSANG = REAL (Given)
*        Position angle of the sector. Units radians.
*     ANGWID = REAL (Given)
*        Angular width of the sector. Units radians.
*     RADIUS = REAL (Given)
*        Radius out from the chosen sector origin that the sector
*        should extend.
*     COLOUR = REAL (Given)
*        Colour to be used when drawing the galaxy centre marker.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     06-Jan-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER COLOUR                  ! The colour of the galaxy centre marker
      INTEGER MODE                    ! Which part of the sector drawing
                                      ! is to take place.
      REAL ANGWID                     ! Angular width of the sector
      REAL CURSIZ                     ! Size of the cross to be drawn at
                                      ! the first point specified.
      REAL POSANG                     ! Position angle of the sector
      REAL RADIUS                     ! Radius out from the chosen sector
                                      ! origin that the sector should extend.
      REAL X(10)                      ! X co-ordinates for various parts of
                                      ! the sector to be drawn.
      REAL Y(10)                      ! Y co-ordinates for various parts of
                                      ! the sector to be drawn.

*  Arguments Given and Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
      REAL X4                         ! X Co-ordinate of one end of the arc
      REAL X5                         ! X Co-ordinate of the other
                                      ! end of the arc
      REAL Y4                         ! Y Co-ordinate of one end of the arc
      REAL Y5                         ! Y Co-ordinate of the other
                                      ! end of the arc

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN


*   Draw the cross at the centre of the proposed sector.
      IF (MODE.EQ.1) THEN
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
      END IF

*   Draw a line from the centre of the sector to its proposed end.
      IF (MODE.EQ.2) THEN
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1),Y(1),X(2),Y(2))
      END IF

*   Draw lines showing the edge of the sector required and then join
*   them together with an arc.
      IF ((MODE.EQ.4).OR.(MODE.EQ.5)) THEN

*      Determine the endpoints of each side of the arc.
         TEMP1=POSANG-ANGWID/2.
         TEMP2=POSANG+ANGWID/2.
         CALL SGS_SPEN(COLOUR)
         IF (MODE.EQ.4) THEN
            X4=X(1)+SIN(TEMP1)*RADIUS
            Y4=Y(1)+COS(TEMP1)*RADIUS
            X5=X(1)+SIN(TEMP2)*RADIUS
            Y5=Y(1)+COS(TEMP2)*RADIUS
         ELSE
            X4=X(1)-SIN(TEMP1)*RADIUS
            Y4=Y(1)-COS(TEMP1)*RADIUS
            X5=X(1)-SIN(TEMP2)*RADIUS
            Y5=Y(1)-COS(TEMP2)*RADIUS
         END IF

*      Draw the sides of the sector requested.
         CALL SGS_LINE(X(1),Y(1),X4,Y4)
         CALL SGS_LINE(X(1),Y(1),X5,Y5)

*      Calculate the angles of the sides in the angular convention
*      required for SGS_ARC and then draw the arc.
         IF (MODE.EQ.4) THEN
            TEMP1=-TEMP1+SEC__PIVAL/2.
            TEMP2=-TEMP2+SEC__PIVAL/2.
         ELSE
            TEMP1=-TEMP1+SEC__PIVAL*3./2.
            TEMP2=-TEMP2+SEC__PIVAL*3./2.
         END IF
         CALL SGS_ARC(X(1),Y(1),RADIUS,TEMP1,TEMP2)

*      Draw dots at the end of the arcs.
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X4,Y4,X4,Y4)
         CALL SGS_LINE(X5,Y5,X5,Y5)

      END IF

*   Clear the quadrant of the window where the results will be displayed
*   and then draw a border around it.
      IF (MODE.EQ.7) THEN
         CALL SGS_SPEN(1)
         CALL SGS_CLRBL(X(6),X(7),Y(6),Y(7))
         CALL SGS_BOX(X(6),X(7),Y(6),Y(7))
      END IF


*   Flush any SGS errors.
 9999 CALL SGS_FLUSH

      END
