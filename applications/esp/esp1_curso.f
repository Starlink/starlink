


      SUBROUTINE ELF1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
*+
*  Name:
*     ELF1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning
*     the co-ordinate for a given type of image and also controls all SGS
*     graphics displays (such as that displaying the galaxy origin)
*
*     The routine is used for more than one purpose to
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ELF1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,
*                     RLIM,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device being used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     NAME = INTEGER (Given)
*        Defines whether or not pictures of name DATA or ELLFOU will
*        be located.
*     COLOUR = INTEGER (Given)
*        Pen colour used to mark the galaxy centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     RLIM = REAL (Given and Returned)
*        Sampling radius maximum.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of
*     the galaxy to be used and the quadrant in which the graphical results
*     display are to be shown. The routines include text messages to be
*     shown to instruct the user.
*
*     - return a locator/identifier value from the AGI database, this allows
*     the NDF that was used to generate the most recently displayed image
*     named DATA, to be accessed.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - inspecting the AGI database to ensure that (as required) the co-ordinate
*     values are being returned for the file most recently stored with
*     the database name DATA.
*
*     - sets up a new AGI databse entry (ELLFOU) to define part of the screen
*     so that PGPLOT routines may be used to update the display and show
*     the results graphically in a form more sophisticated than SGS would
*     normally allow.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there.
*
*     The application only acts on the most recent picture in the
*     graphics database named 'DATA' and also an entry called 'ELLFOU' which
*     contains a graphical display of the profile results.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Pen colour used to mark the galaxy
                                 ! centre
      INTEGER NAME               ! Whether pictures of name DATA or ELLFOU
                                 ! are to be used 0=DATA 1=ELLFOU
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL X(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstatio
      REAL RLIM                  ! Sampling radius maximum

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal

      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER IWCS               ! AST pointer to NDF's WCS frameset
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL ONE                   ! One
      REAL TEMPX                 ! Temporary variable
      REAL TEMPY                 ! Temporary variable
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor
      REAL ZERO                  ! Zero

      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.

      IF (STATUS.NE.SAI__OK) RETURN

*   Begin AST context.
      CALL AST_BEGIN(STATUS)

*   Create informational messages for use with the cursor.
      CALL ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*   Start the graphics system. If this is the first time the routine has
*   been used then an identifier/locator to the NDF for the displayed
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN
         CALL ELF1_AGIC2(GRADEV,0,1,NAME,NDF1,DEVCAN,PICID,STATUS)
         POINT=1
      ELSE
         CALL ELF1_AGIC2(GRADEV,0,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When identifying the
*   galaxy to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF ((POINT.NE.2).AND.(POINT.NE.9)) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Draw the radius limit for the profiling. Is done here so that
*   the value for X1 and Y1 need not be retained between calls to this
*   routine.
      IF (POINT.EQ.3) THEN
         CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
         GOTO 980
      END IF


*   Set up the screen sector that will be used to display the graph results.
      IF (POINT.EQ.7) THEN

*      Set up temporary stores for the x and y range divided by 2.
         TEMPX=(X2-X1)/2.
         TEMPY=(Y2-Y1)/2.

*      Sort out the x co-ordinates for the quadrant position required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1
            X(7)=X1+TEMPX
         ELSE
            X(6)=X1+TEMPX
            X(7)=X2
         END IF

*      Sort out the y co-ordinates for the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1
            Y(7)=Y1+TEMPY
         ELSE
            Y(6)=Y1+TEMPY
            Y(7)=Y2
         END IF

*      Draw the box showing the quadrant being used.
         CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*      Sort out the x co-ordinates for within the quadrant required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1+TEMPX*.15
            X(7)=X1+TEMPX*.9
         ELSE
            X(6)=X1+TEMPX*1.15
            X(7)=X1+TEMPX*1.9
         END IF

*      Sort out the y co-ordinates for within the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1+TEMPY*.15
            Y(7)=Y1+TEMPY*.85
         ELSE
            Y(6)=Y1+TEMPY*1.15
            Y(7)=Y1+TEMPY*1.85
         END IF

*      Setup the new entry in the database.
         ZERO=0.0
         ONE= 1.0
         CALL AGI_NUPIC(X(6),X(7),Y(6),Y(7),'ELLFOU','Galaxy Profile',
     :                 ZERO,ONE,ZERO,ONE,PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Could not create a new picture in the'/
     :                   /' AGI database.',STATUS)
         END IF
         GOTO 980

      END IF

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL ELF1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Get WCS component of NDF, used for reporting cursor position.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Loop until the point is selected.
*   Values 4 taken as the select.
*   Value 2 as an emergency exit.
*   Values 1 and 3 used to show the current position.
      DO WHILE ((HITVAL.NE.4).AND.(STATUS.EQ.SAI__OK))

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)

*      Emergency exit.
         IF (HITVAL.EQ.2) THEN
            CALL MSG_BLANK(STATUS)
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','You have opted to leave the'/
     :                   /' program.',STATUS)
            GOTO 980
         END IF

*         Convert the world co-ordinates to data system.
         IF ((HITVAL.EQ.1).OR.(HITVAL.EQ.3).OR.(HITVAL.EQ.4)) THEN
            X(POINT)=XIN
            Y(POINT)=YIN
*         Display the cursor results if necessary.
            IF (POINT.LT.6) THEN
               CALL ESP1_CRPT(IWCS,XIN-X1,YIN-Y1,STATUS)
            END IF
         END IF

*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Draw the galaxy origin.
      IF (POINT.EQ.1) CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,
     :                                RLIM,STATUS)

*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to ELF1_CMODE.
      X(10)=REAL(INT(X(1)-X1+1.))
      Y(10)=REAL(INT(Y(1)-Y1+1.))

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL ELF1_AGIC2(GRADEV,1,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Exit point for errors that occurred before the graphics device
*   was opened.

 9999 CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

      END


      SUBROUTINE ELP1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
*+
*  Name:
*     ELP1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning
*     the co-ordinate for a given type of image and also controls all SGS
*     graphics displays (such as that displaying the galaxy origin)
*
*     The routine is used for more than one purpose to
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ELP1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,RLIM,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device to be used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     NAME = INTEGER (Given)
*        Defines whether or not pictures of name DATA or ELLPRO will
*        be located.
*     COLOUR = INTEGER (Given)
*        Pen colour marking the galaxy centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     RLIM = REAL (Given and Returned)
*        Radius of the biggest ellipse allowed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of
*     the galaxy to be used and the quadrant in which the graphical results
*     display are to be shown. The routines include text messages to be
*     shown to instruct the user.
*
*     - return a locator/identifier value from the AGI database, this allows
*     the NDF that was used to generate the most recently displayed image
*     named DATA, to be accessed.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - inspecting the AGI database to ensure that (as required) the co-ordinate
*     values are being returned for the file most recently stored with
*     the database name DATA.
*
*     - sets up a new AGI databse entry (ELLPRO) to define part of the screen
*     so that PGPLOT routines may be used to update the display and show
*     the results graphically in a form more sophisticated than SGS would
*     normally allow.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there.
*
*     The application only acts on the most recent picture in the
*     graphics database named 'DATA' and also an entry called 'ELLPRO' which
*     contains a graphical display of the profile results.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Pen colour of the galaxy marker
      INTEGER NAME               ! Whether pictures of name DATA or ELLPRO
                                 ! are to be used 0=DATA 1=ELLPRO
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL X(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstatio
      REAL RLIM                  ! Radius of the most distant permitted
                                 ! profile ellipse

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal

      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER IWCS               ! AST pointer to WCS frameset of NDF
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL TEMPX                 ! Temporary variable
      REAL TEMPY                 ! Temporary variable
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor

      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.

      IF (STATUS.NE.SAI__OK) RETURN

*   Begin AST context.
      CALL AST_BEGIN(STATUS)

*   Create informational messages for use with the cursor.
      CALL ELP1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*   Start the graphics system. If this is the first time the routine has
*   been used then an identifier/locator to the NDF for the displayed
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN
         CALL ELP1_AGIC2(GRADEV,0,1,NAME,NDF1,DEVCAN,
     :                   PICID,STATUS)
         POINT=1
      ELSE
         CALL ELP1_AGIC2(GRADEV,0,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When identifying the
*   galaxy to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF ((POINT.NE.2).AND.(POINT.NE.9)) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Draw the radius limit for the profiling. Is done here so that
*   the value for X1 and Y1 need not be retained between calls to this
*   routine.
      IF (POINT.EQ.3) THEN
         CALL ELP1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
         GOTO 980
      END IF

*   Set up the screen sector that will be used to display the graph results.
      IF (POINT.EQ.7) THEN

*      Set up temporary stores for the x and y range divided by 2.
         TEMPX=(X2-X1)/2.
         TEMPY=(Y2-Y1)/2.

*      Sort out the x co-ordinates for the quadrant position required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1
            X(7)=X1+TEMPX
         ELSE
            X(6)=X1+TEMPX
            X(7)=X2
         END IF

*      Sort out the y co-ordinates for the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1
            Y(7)=Y1+TEMPY
         ELSE
            Y(6)=Y1+TEMPY
            Y(7)=Y2
         END IF

*      Draw the box showing the quadrant being used.
         CALL ELP1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*      Sort out the x co-ordinates for within the quadrant required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1+TEMPX*.15
            X(7)=X1+TEMPX*.9
         ELSE
            X(6)=X1+TEMPX*1.15
            X(7)=X1+TEMPX*1.9
         END IF

*      Sort out the y co-ordinates for within the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1+TEMPY*.15
            Y(7)=Y1+TEMPY*.85
         ELSE
            Y(6)=Y1+TEMPY*1.15
            Y(7)=Y1+TEMPY*1.85
         END IF

*      Setup the new entry in the database.
         CALL AGI_NUPIC(X(6),X(7),Y(6),Y(7),'ELLPRO','Galaxy Profile',
     :                 0.0,1.0,0.0,1.0,PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Could not create a new picture in the'/
     :                   /' AGI database.',STATUS)
         END IF
         GOTO 980

      END IF

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL ELP1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Get WCS component from NDF, used for reporting cursor position to user.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Loop until the point is selected.
*   Values 4 taken as the select.
*   Value 2 as an emergency exit.
*   Values 1 and 3 used to show the current position.
      DO WHILE ((HITVAL.NE.4).AND.(STATUS.EQ.SAI__OK))

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)

*      Emergency exit.
         IF (HITVAL.EQ.2) THEN
            CALL MSG_BLANK(STATUS)
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','You have opted to leave the'/
     :                   /' program.',STATUS)
            GOTO 980
         END IF

*      Convert the world co-ordinates to data system.
         IF ((HITVAL.EQ.1).OR.(HITVAL.EQ.3).OR.(HITVAL.EQ.4)) THEN
            X(POINT)=XIN
            Y(POINT)=YIN
*         Display the cursor results if necessary.
            IF (POINT.LT.6) THEN
               CALL ESP1_CRPT(IWCS,XIN-X1,YIN-Y1,STATUS)
            END IF
         END IF

*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Draw the galaxy origin.
      IF (POINT.EQ.1) CALL ELP1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,
     :                                RLIM,STATUS)

*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to ELP1_CMODE.
      X(10)=REAL(INT(X(1)-X1+1.))
      Y(10)=REAL(INT(Y(1)-Y1+1.))

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL ELP1_AGIC2(GRADEV,1,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Exit point for errors that occurred before the graphics device
*   was opened.

 9999 CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

      END


      SUBROUTINE GAU1_CURSO(GRADEV,POINT,COLOUR,NDF1,X,Y,
     :                      ISTAT,STATUS)
*+
*  Name:
*     GAU1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning
*     the co-ordinate for a given type of image and also controls all SGS
*     graphics displays (such as that displaying the source origin)
*
*     The routine is used for more than one purpose to
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GAU1_CURSO(GRADEV,POINT,COLOUR,NDF1,X,Y,ISTAT,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device to be used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     COLOUR = INTEGER (Given)
*        Pen colour marking the source centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     ISTAT = INTEGER (Returned)
*        End of cursor selection?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of
*     and size the sources to be used. The routines include text messages
*     to be shown to instruct the user.
*
*     - return a locator/identifier value from the AGI database, this
*     ensures that the NDF and co-ordinates used are those of the
*     most recently displayed image named DATA.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there.
*
*     The application only acts on the most recent picture in the
*     graphics database named 'DATA'.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}
*     ISTAT added: 03/03/96 GJP

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Pen colour of the source marker
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL X(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstatio

*  Arguments Returned.
      INTEGER ISTAT              ! Indicates if selection was aborted.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal
      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER IWCS               ! AST pointer to WCS frameset of NDF
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL RADIUS                ! Radius of the source
      REAL TEMP1                 ! Temporary variable
      REAL TEMP2                 ! Temporary variable
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor
*.

*   Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Enter AST context.
      CALL AST_BEGIN(STATUS)

*   Create informational messages for use with the cursor.
      CALL GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Start the graphics system. If this is the first time the routine has
*   been used then an identifier/locator to the NDF for the displayed
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN
         CALL GAU1_AGIC2(GRADEV,0,1,NDF1,DEVCAN,
     :                   PICID,STATUS)
         POINT=1
      ELSE
         CALL GAU1_AGIC2(GRADEV,0,0,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When identifying the
*   source to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)

      IF (POINT.LT.2) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL GAU1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Get WCS component of NDF, used for reporting cursor position.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Loop until the point is selected.
*   Value 3 taken as the select.
*   Value -1 or -9999 as an emergency exit.
*   Values 0 used to show the current position.
      DO WHILE ((HITVAL.NE.3).AND.(STATUS.EQ.SAI__OK))

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)
         X(POINT)=XIN
         Y(POINT)=YIN

*      Convert CTRL-C input.
         IF(HITVAL.EQ.-9999) HITVAL=-1

*      Emergency exit.
         IF (HITVAL.EQ.-1) THEN
            ISTAT=1
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','You have opted to quit source'/
     :                   /' selection.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 980
         END IF

*      Display the cursor results if necessary.
         IF (HITVAL.EQ.0.OR.HITVAL.EQ.3) THEN
            CALL MSG_BLANK(STATUS)
            CALL ESP1_CRPT(IWCS,XIN-X1,YIN-Y1,STATUS)
            CALL MSG_BLANK(STATUS)
         END IF

*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to GAU1_CMODE.
      X(POINT)=REAL(INT(X(POINT)-X1+1.))
      Y(POINT)=REAL(INT(Y(POINT)-Y1+1.))

*   Draw the source origin.
      IF (POINT.EQ.1) THEN

*      Draw the cross at the centre of the indicated source.
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
         CALL SGS_FLUSH
      END IF

*   Draw the radius limit of the source.
      IF (POINT.EQ.2) THEN
         RADIUS=SQRT
     :        ((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
         IF (RADIUS.LT.2.) RADIUS=2.
         TEMP1=0.0
         TEMP2=2.*3.1415926
         CALL SGS_SPEN(COLOUR)
         CALL SGS_ARC(X(1),Y(1),RADIUS,TEMP1,TEMP2)
         CALL SGS_FLUSH
      END IF

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL GAU1_AGIC2(GRADEV,1,0,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Exit point for errors that occurred before the graphics device
*   was opened.

 9999 CONTINUE

*   Exit AST context.
      CALL AST_END(STATUS)

      END


      SUBROUTINE GRA1_CURSO(POINT,X,Y,STATUS)
*+
*  Name:
*     GRA1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning
*     the co-ordinate for a given type of image and also controls all SGS
*     graphics displays (such as that displaying the shape/position of the
*     user defined sector). The routine is used for more than one purpose to
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GRA1_CURSO(POINT,X,Y,STATUS)

*  Arguments:
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     X = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor.
*     Y = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine allows the user to use the SGS cursor to specify the
*     radius range to be used.
*
*     It inspects the AGI database to ensure that the co-ordinate
*     system values are being returned for the file most recently stored
*     within the database name GRAPHS.
*
*     It closes down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there.
*
*     The application only acts on the most recent picture in the
*     graphics database called 'GRAPHS' which contains a graphical display
*     of the profile results.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned:
      REAL X                     ! Position information from the cursor
                                 ! or to be displayed on the workstation
      REAL Y                     ! Position information from the cursor
                                 ! or to be displayed on the workstatio

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER *80 IMGMES(3)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(3)    ! Informational messages if device is
                                 ! a terminal

      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      INTEGER USED               ! Was a point defined? USED<>0 True.
      REAL CURSIZ                ! Size of the graphics cursor
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! X co-ord as measured by cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! Y co-ord as measured by cursor
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Create informative messages for use with the cursor.

*   Select a point defining the lower limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.1) THEN
         TERMES(1)='Select a point defining the lower limit for '//
     :             'radius using the space bar.'
         TERMES(2)='Type . or use button to exit.'
         IMGMES(1)='Select a point defining the lower limit for'/
     :    /' the radius required.'
         IMGMES(2)='To exit press the right button.'
      END IF

*   Select a point defining the upper limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.2) THEN
         TERMES(1)='Select a point defining the upper limit for '//
     :             'radius using the space bar.'
         TERMES(2)='Type . or use button to exit.'
         IMGMES(1)='Select a point defining the upper limit for'/
     :    /' the radius required.'
         IMGMES(2)='To exit press the right button.'
      END IF

      NTERMS=2
      NIMGMS=2

*   Start the graphics system.
      CALL GRA1_AGIC2(0,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When defining the
*   sector to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF (POINT.EQ.8) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      END IF

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL GRA1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=1

*   Initialise flag to show that the cursor was used.
      USED=0

*   Loop until the escape choice is selected
      DO WHILE (HITVAL.GT.0.AND.HITVAL.LT.4.AND.STATUS.EQ.SAI__OK)

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)
         IF(HITVAL.EQ.-1) HITVAL=4

*      Get the result from the cursor.
         IF ((HITVAL.GT.0).AND.(HITVAL.LT.4)) THEN

*         Note that an input was obtained.
            USED=USED+1

            X=XIN
            Y=YIN

         ELSE
            IF (USED.EQ.0) THEN
               CALL MSG_OUT(' ','No points selected!!!',STATUS)
               IF (STATUS.EQ.SAI__OK) HITVAL=1
            END IF
         END IF

*      Release the new error context.
         CALL ERR_RLSE

      END DO

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL GRA1_AGIC2(1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CURSO(GRADEV,POINT,NAME,POSANG,COLOUR,NDF1,
     :                      X,Y,RADIUS,ANGWID,STATUS)
*+
*  Name:
*     SEC1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning
*     the co-ordinate for a given type of image and also controls all SGS
*     graphics displays (such as that displaying the shape/position of the
*     user defined sector). The routine is used for more than one purpose to
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SEC1_CURSO(GRADEV,POINT,NAME,POSANG,COLOUR,NDF1,X,Y,
*                     RADIUS,ANGWID,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device to be used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     NAME = INTEGER (Given)
*        Defines whether or not pictures of name DATA or SECTOR will
*        be located.
*     POSANG = REAL (Given)
*        Position angle of the sector. Units radians.
*     COLOUR = INTEGER (Given)
*        Colour to be used when drawing the galaxy centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     RADIUS = REAL (Given and Returned)
*        Radius of the sector to be used.
*     ANGWID = REAL (Given and Returned)
*        Angular width of the sector. Units radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of
*     the sector to be used and the quadrant in which the graphical results
*     display are to be shown. The routines include text messages to be
*     shown to instruct the user.
*
*     - return a locator/identifier value from the AGI database, that allows
*     the NDF that was used to generate the most recently displayed image
*     named DATA, to be accessed.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - inspecting the AGI database to ensure that (as required) the co-ordinate
*     values are being returned for the file most recently stored with
*     the database name DATA (as with an image) or from a database entry named
*     SECTOR (as with a results graph).
*
*     - sets up a new AGI databse entry (SECTOR) to define part of the screen
*     so that PGPLOT routines may be used to update the display and show
*     the results graphically in a form more sophisticated than SGS would
*     normally allow.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there.
*
*     The application only acts on the most recent picture in the
*     graphics database named 'DATA' and also an entry called 'SECTOR' which
*     contains a graphical display of the profile results.
*
*    Within ESP the scale lengths are calculated by assuming an
*     exponential brightness profile for spiral galaxies and a
*     quarter power law for elliptical galaxies. The scale length
*     value given is derived from the decay constant of the
*     exponential functions.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Colour of the galaxy centre marker
      INTEGER NAME               ! Whether pictures of name DATA or SECTOR
                                 ! are to be used 0=DATA 1=SECTOR
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected
      REAL POSANG                ! Position angle of the sector

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL ANGWID                ! Angular width of the sector
      REAL X(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor
                                 ! or to be displayed on the workstatio
      REAL RADIUS                ! Radius of the sector to be used in
                                 ! world units

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal

      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER IWCS               ! AST pointer to WCS component of NDF
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL TEMPX                 ! Temporary storage
      REAL TEMPY                 ! Temporary storage
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor

      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.

      IF (STATUS.NE.SAI__OK) RETURN

*   Begin AST context.
      CALL AST_BEGIN(STATUS)

*   Create informational messages for use with the cursor.
      CALL SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*   Start the graphics system. If this is the first time the routine has
*   been used then an identifier/locator to the NDF for the displayed
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN
         CALL SEC1_AGIC2(GRADEV,0,1,NAME,NDF1,DEVCAN,
     :                   PICID,STATUS)
         POINT=1
      ELSE
         CALL SEC1_AGIC2(GRADEV,0,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When defining the
*   sector to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF ((POINT.NE.2).AND.(POINT.NE.3).AND.(POINT.NE.9)) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Draw the sector(s) that will be used and then exit. Is done here so that
*   the value for X1 and Y1 need not be retained between calls to this
*   routine.
      IF ((POINT.EQ.4).OR.(POINT.EQ.5)) THEN
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)
         GOTO 980
      END IF


*   Set up the screen sector that will be used to display the graph results.
      IF (POINT.EQ.7) THEN

*      Set up the x and y range divided by 2.
         TEMPX=(X2-X1)/2.
         TEMPY=(Y2-Y1)/2.

*      Sort out the x co-ordinates for the quadrant position required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1
            X(7)=X1+TEMPX
         ELSE
            X(6)=X1+TEMPX
            X(7)=X2
         END IF

*      Sort out the y co-ordinates for the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1
            Y(7)=Y1+TEMPY
         ELSE
            Y(6)=Y1+TEMPY
            Y(7)=Y2
         END IF

*      Draw the box showing the quadrant being used.
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)

*      Sort out the x co-ordinates for within the quadrant required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1+TEMPX*.15
            X(7)=X1+TEMPX*.9
         ELSE
            X(6)=X1+TEMPX*1.15
            X(7)=X1+TEMPX*1.9
         END IF

*      Sort out the y co-ordinates for within the quadrant required.
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1+TEMPY*.15
            Y(7)=Y1+TEMPY*.85
         ELSE
            Y(6)=Y1+TEMPY*1.15
            Y(7)=Y1+TEMPY*1.85
         END IF

*      Setup the new entry in the database.
         CALL AGI_NUPIC(X(6),X(7),Y(6),Y(7),'SECTOR','Galaxy Profile',
     :                 0.0,1.0,0.0,1.0,PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Could not create a new picture in the'/
     :                   /' AGI database.',STATUS)
         END IF
         GOTO 980

      END IF

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL SEC1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Get the WCS component of the NDF, used for outputting messages about
*   the cursor position.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Loop until the point is selected.
*   Values 4 taken as the select.
*   Value 2 as an emergency exit.
*   Values 1 and 3 used to show the current position.
      DO WHILE ((HITVAL.NE.4).AND.(STATUS.EQ.SAI__OK))

*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)

*      Emergency exit.
         IF (HITVAL.EQ.2) THEN
            CALL MSG_BLANK(STATUS)
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','You have opted to leave the'/
     :                   /' program.',STATUS)
            GOTO 980
         END IF

*      Convert the world co-ordinates to data system.
         IF ((HITVAL.EQ.1).OR.(HITVAL.EQ.3).OR.(HITVAL.EQ.4)) THEN
            X(POINT)=XIN
            Y(POINT)=YIN
*         Display the cursor results if necessary.
            IF (POINT.LT.5) THEN
               CALL ESP1_CRPT(IWCS,XIN-X1+1.0,YIN-Y1+1.0,STATUS)
            END IF
         END IF

*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Draw the currently required part of the sector.
*   1 = cross at centre, 2 = line in correct direction.
      IF ((POINT.EQ.1).OR.(POINT.EQ.2)) THEN
         CALL SEC1_GRBIT(POINT,CURSIZ,X,Y,POSANG,
     :                   ANGWID,RADIUS,COLOUR,STATUS)
      END IF

*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to SEC1_CMODE.
      X(10)=REAL(INT(X(1)-X1+1.))
      Y(10)=REAL(INT(Y(1)-Y1+1.))

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL SEC1_AGIC2(GRADEV,1,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*    Exit point for errors that occurred before the graphics device
*    was opened.

 9999 CONTINUE

*    End AST context.
       CALL AST_END(STATUS)

      END
