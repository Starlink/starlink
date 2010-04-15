

      SUBROUTINE ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     ELF1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currently being
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)


*  Description:
*     Depending on the value of POINT the routine assigns values to two
*     character arrays. These are then used by subroutine ELF1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display.
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Feb-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN

         TERMES(1)='Select the centre of the galaxy to be profiled.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the galaxy.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'

      END IF

*   Select a point defining the maximum permitted radius.
      IF (POINT.EQ.2) THEN

         TERMES(1)='Indicate the outer limit of the galaxy.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the outer limit of the'/
     :             /' galaxy.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'

      END IF

*   Select a point defining the quadrant in which a graph should be
*   displayed.
      IF (POINT.EQ.6) THEN

         TERMES(1)='Select a point defining the quadrant of the window'/
     :             /' in which to plot.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the quadrant.'
         IMGMES(3)=TERMES(3)

         TERMES(4)=' '
         IMGMES(4)=' '

      END IF

      NTERMS=4
      NIMGMS=4

      END



      SUBROUTINE ELP1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     ELP1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currently being
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)


*  Description:
*     Depending on the value of POINT the routine assigns values to two
*     character arrays. These are then used by subroutine ELP1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display.
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Feb-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Selecting the galaxy centre centre.
      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN

         TERMES(1)='Select the centre of the galaxy to be profiled.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the galaxy.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'

      END IF

*   Select a point defining the maximum permitted radius.
      IF (POINT.EQ.2) THEN

         TERMES(1)='Indicate the outer limit of the galaxy.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the outer limit of the'/
     :             /' galaxy.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'

      END IF

*   Select a point defining the quadrant in which a graph should be
*   displayed.
      IF (POINT.EQ.6) THEN

         TERMES(1)='Select a point defining the quadrant of the window'/
     :             /' in which to plot.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Keyboard "." key:   Select the quadrant.'
         IMGMES(3)=TERMES(3)

         TERMES(4)=' '
         IMGMES(4)=' '

      END IF

      NTERMS=4
      NIMGMS=4

      END


      SUBROUTINE GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     GAU1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currently being
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*  Description:
*     Depending on the value of POINT the routine assigns values to two
*     character arrays. These are then used by subroutine GAU1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display.
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Selecting the source centre centre.
      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN

         TERMES(1)='Select the source location'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Left mouse button:        Select location.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Middle mouse button:      Show cursor coordinates.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Right button or CTRL-C:  Quit selection.'
         IMGMES(4)=TERMES(4)

      END IF

*   Select a point defining the maximum permitted radius.
      IF (POINT.EQ.2) THEN

         TERMES(1)='Indicate the outer limit of the source.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Left mouse button:        Select location.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Middle mouse button:      Show cursor coordinates.'
         IMGMES(3)=TERMES(3)

         TERMES(4)='Right button or CTRL-C:  Quit selection.'
         IMGMES(4)=TERMES(4)

      END IF

      NTERMS=4
      NIMGMS=4

      END


      SUBROUTINE SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     SEC1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currenlty being
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)


*  Description:
*     Depending on the value of POINT the routine assigns values to two
*     character arrays. These are then used by subroutine SEC1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display.
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Feb-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default to no values.
         TERMES(1)=' '
         TERMES(2)=' '
         TERMES(3)=' '
         TERMES(4)=' '
         IMGMES(1)=' '
         IMGMES(2)=' '
         IMGMES(3)=' '
         IMGMES(4)=' '

*   Selecting the sector centre.
      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN
         TERMES(1)='Select the centre of the galaxy.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select the galaxy.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the direction and extent of the proposed sector.
      IF (POINT.EQ.2) THEN
         TERMES(1)='Indicate centre of the outer limit of the sector.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select centre of the outer '/
     :             /' limit of the sector.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the angular width of the sector.
      IF (POINT.EQ.3) THEN
         TERMES(1)='Select the sector angular width.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select width of the sector.'
         IMGMES(3)=TERMES(3)
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
      END IF

*   Select a point defining the quadrant in which a graph should be
*   displayed.
      IF (POINT.EQ.6) THEN
         TERMES(1)='Select a point defining the quadrant of the window'/
     :             /' in which to plot.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select the quadrant.'
         IMGMES(3)=TERMES(3)
      END IF

*   Select a point defining the lower limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.8) THEN
         TERMES(1)='Select a point defining the lower radius limit.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select lower radius limit.'
         IMGMES(3)=TERMES(3)
       END IF

*   Select a point defining the upper limit for the radius of data
*   points used in the analysis of the scale length.
      IF (POINT.EQ.9) THEN
         TERMES(1)='Select a point defining the upper radius limit.'
         IMGMES(1)=TERMES(1)
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
         TERMES(3)='Keyboard "." key:   Select upper radius limit.'
         IMGMES(3)=TERMES(3)
      END IF

      NTERMS=4
      NIMGMS=4

      END
