************************************************************************

      SUBROUTINE CUSHAP ( BZONE, IZONE, CLEAR, NE, ELLIPS, A, E, THETA,
     :                    IMGDIS, STATUS )

*+
*  Name :
*     CUSHAP
*
*  Purpose :
*     This uses a display to interactively select the shape parameters for
*     the aperture.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL CUSHAP( BZONE, IZONE, CLEAR, NE, ELLIPS, A, E, THETA,
*                  IMGDIS, STATUS )
*
*  Description :
*     This uses a display to interactively select the shape parameters for
*     the aperture. The size of the aperture is selected from a list which
*     is a multiplicative factor of the current size. The orientation is
*     replicated around 90 degrees to increase the resolution. On entering
*     the routine the look up tables of e and theta are examined to find
*     the nearest, lower, value in the table.
*
*  Arguments :
*     BZONE = INTEGER (Given)
*        Base zone of display
*     IZONE = INTEGER (Given)
*        Image zone
*     CLEAR = LOGICAL (Given)
*        Flag to indicate use of block clear
*     NE = INTEGER (Given)
*        Number of vertices in ellipse
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertises of ellipse
*     A = REAL (Given and Returned)
*        Semi-major axis of ellipse
*     E = REAL (Given and Returned)
*        Eccentricity of ellipse
*     THETA = REAL (Given and Returned)
*        Orientation of ellipse
*     IMGDIS = LOGICAL (Given)
*        Display is an image type and has a mouse.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1987 (NE):
*        Original version.
*     10-OCT-1989 (NE):
*        Added clear flag
*     10-AUG-1990 (NE):
*        Call to OUTMEN
*     8-NOV-1996 (PWD):
*        Added IMGDIS argument and associated changes to support mice
*        interactors.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER BZONE
      INTEGER IZONE
      LOGICAL CLEAR
      INTEGER NE
      REAL ELLIPS( 2, NE )
      LOGICAL IMGDIS

*  Arguments Given and Returned :
      REAL A
      REAL E
      REAL THETA

*  Status :
      INTEGER STATUS

*  Local Variables :
      INTEGER CHOICE, CZONE, IA, IE, IP, IT

      REAL A1, A2, ALIST( 0:11 ), ELIST( 0:11 ), TADD, TLIST( 0:11 ),
     :     WX1, WX2, WY1, WY2, XCEN, XCEN1, XM, YCEN, YCEN1, YM

      CHARACTER CPARAM( 0:2 ) * 12

*  Local Data :
      DATA CPARAM / 'SEMI-MAJOR  ', 'ECCENTRICITY', 'ORIENTATION ' /

      DATA ALIST / 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1,
     :             1.2, 1.4, 1.6, 1.8, 2.0 /
      DATA ELIST / 0.0, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9,
     :              0.95, 0.97, 0.99 /
      DATA TLIST / 0.0, 7.5, 15.0, 22.5, 30.0, 37.5, 45.0, 52.5,
     :             60.0, 67.5, 75.0, 82.5 /

      SAVE ALIST, ELIST, TLIST
*.

*   Do some initialisations
      IA = 5
      CHOICE = 99
      IP = 0

*   Find the nearest look up table entry to the input values of e and theta
      IE = 0
      DO WHILE ( ( ELIST( IE + 1 ) .LE. E ) .AND. ( IE .LT. 11 ) )
         IE = IE + 1
      ENDDO

*   Make sure theta lies between 0 and 180
      DO WHILE ( THETA .LT. 0.0 )
         THETA = THETA + 180.0
      ENDDO
      DO WHILE ( THETA .GT. 180.0 )
         THETA = THETA - 180.0
      ENDDO

      IT = 0
      IF ( THETA .GT. 90.0 ) THEN
         TADD = 90.0
      ELSE
         TADD = 0.0
      ENDIF

      DO WHILE ( ( TLIST( IT + 1 ) + TADD .LE. THETA ) .AND.
     :           ( IT .LT. 11 ) )
         IT = IT + 1
      ENDDO

*   Put the choice key meanings on the screen
      CALL SGS_SELZ ( BZONE, STATUS )
      IF ( CLEAR ) THEN
         CALL SGS_CLRZ
      ENDIF
      CALL SGS_ZSHAP ( 1.0, 'CC', CZONE, STATUS )
      CALL SGS_SHTX ( 0.015 )
      CALL SGS_BOX ( 0.04, 0.31, 0.075, 0.1 )
      CALL SGS_TX ( 0.06, 0.08, 'CHANGE PARAMETER' )
      CALL SGS_BOX ( 0.36, 0.63, 0.075, 0.1 )
      CALL SGS_TX ( 0.41, 0.08, 'CHANGE VALUE' )
      CALL SGS_BOX ( 0.68, 0.95, 0.075, 0.1 )
      CALL SGS_TX ( 0.685, 0.08, 'RETURN TO KEYBOARD' )

*   Put the current parameter and its value on the screen
      CALL SGS_BOX ( 0.04, 0.31, 0.145, 0.17 )
      CALL SGS_TX ( 0.1, 0.15, CPARAM( 0 ) )
      CALL SGS_BOX ( 0.36, 0.63, 0.145, 0.17 )
      CALL SGS_TXR ( 0.37, 0.15, A * ALIST( IA ), 6, 2 )

*   Set up the initial values of the menu parameters
      A1 = A * ALIST( IA )
      A2 = A1
      E = ELIST( IE )
      THETA = TLIST( IT ) + TADD

*   Inform the user of the choices
      CALL OUTMEN( IMGDIS )

*   Set the cursor to the middle of the image zone
      CALL SGS_SELZ ( IZONE, STATUS )
      CALL SGS_IZONE ( WX1, WX2, WY1, WY2, XM, YM )
      XCEN1 = ( WX2 - WX1 ) / 2.0
      YCEN1 = ( WY2 - WY1 ) / 2.0
      CALL MAKELL ( XCEN1, YCEN1, A1, E, THETA, NE, ELLIPS )
      CALL PLOTEL ( IZONE, NE, ELLIPS )
      CALL SGS_SETCU ( XCEN1, YCEN1 )

      DO WHILE ( CHOICE .GT. 0 )

*   Request the choice from the input device. Flush the SGS buffer first
         CALL SGS_FLUSH
         CALL SGS_SELZ ( IZONE, STATUS )
         CALL SGS_REQCU ( XCEN, YCEN, CHOICE )

*   If the cursor is outside the current zone then go back to SGS_REQCU
*   But not if the choice value is less than 1
         IF ( CHOICE .GT. 0 ) THEN
            IF ( ( XCEN .LT. WX1 ) .OR. ( XCEN .GT. WX2 ) .OR.
     :           ( YCEN .LT. WY1 ) .OR. ( YCEN .GT. WY2 ) ) THEN
               CHOICE = 99
            ENDIF
         ENDIF

*   Convert break into middle mouse button.
         IF ( CHOICE .EQ. 0 ) CHOICE = 2

*   Convert mouse button 1 into same as keyboard 1
         IF ( CHOICE .EQ. 5 ) CHOICE = 1

*   0 and . mean exit as well as mouse button 3.
         IF ( CHOICE .EQ. 3 .OR. CHOICE .EQ. 4 ) THEN
            CHOICE = -1
         END IF

*   If choice = 1 change parameter
         IF ( CHOICE .EQ. 1 ) THEN
            CALL SGS_SELZ ( CZONE, STATUS )
            IP = MOD( IP + 1, 3 )
            IF ( CLEAR ) THEN
               CALL SGS_CLRBL ( 0.04, 0.31, 0.145, 0.17 )
            ENDIF
            CALL SGS_BOX ( 0.04, 0.31, 0.145, 0.17 )
            CALL SGS_TX ( 0.1, 0.15, CPARAM( IP ) )

*   Show the current value of this parameter
            IF ( CLEAR ) THEN
               CALL SGS_CLRBL ( 0.36, 0.63, 0.145, 0.17 )
            ENDIF
            CALL SGS_BOX ( 0.36, 0.63, 0.145, 0.17 )
            IF ( IP .EQ. 0 ) THEN
               CALL SGS_TXR ( 0.37, 0.15, A * ALIST( IA ), 6, 2 )
            ELSEIF ( IP .EQ. 1 ) THEN
               CALL SGS_TXR ( 0.37, 0.15, ELIST( IE ), 4, 2 )
            ELSEIF ( IP .EQ. 2 ) THEN
               CALL SGS_TXR ( 0.37, 0.15, TLIST( IT ) + TADD, 5, 1 )
            ENDIF

*   If choice = 2 then update the parameter value
         ELSE IF ( CHOICE .EQ. 2 ) THEN

*   Clear the box containing the parameter value
            CALL SGS_SELZ ( CZONE, STATUS )
            IF ( CLEAR ) THEN
               CALL SGS_CLRBL ( 0.36, 0.63, 0.145, 0.17 )
            ENDIF
            CALL SGS_BOX ( 0.36, 0.63, 0.145, 0.17 )

*   Change semi-major axis
            IF ( IP .EQ. 0 ) THEN
               IA = MOD( IA + 1, 12 )
               A1 = A * ALIST( IA )
               CALL SGS_TXR ( 0.37, 0.15, A1, 6, 2 )

*   Change eccentricity
            ELSEIF ( IP .EQ. 1 ) THEN
               IE = MOD( IE + 1, 12 )
               E = ELIST( IE )
               CALL SGS_TXR ( 0.37, 0.15, ELIST( IE ), 4, 2 )

*   Change orientation
            ELSEIF ( IP .EQ. 2 ) THEN
               IT = MOD( IT + 1, 12 )
               IF ( IT .EQ. 0 ) THEN
                  IF ( TADD .LT. 45.0 ) THEN
                     TADD = 90.0
                  ELSE
                     TADD = 0.0
                  ENDIF
               ENDIF
               THETA = TLIST( IT ) + TADD
               CALL SGS_TXR ( 0.37, 0.15, THETA, 5, 1 )
            ENDIF

         ENDIF

*   Erase the old elipse and draw the new ellipse. Have to use the old
*   value of the centre and the semi-major axis for clearing the box
         CALL SGS_SELZ ( IZONE, STATUS )
         IF ( CLEAR ) THEN
            CALL SGS_CLRBL ( XCEN1 - A2, XCEN1 + A2,
     :                       YCEN1 - A2, YCEN1 + A2 )
         ENDIF
         A2 = A1
         XCEN1 = XCEN
         YCEN1 = YCEN
         CALL MAKELL ( XCEN, YCEN, A1, E, THETA, NE, ELLIPS )
         CALL PLOTEL ( IZONE, NE, ELLIPS )

      ENDDO

*   Return the new value of the semi-major axis
      A = A * ALIST( IA )

*   Report the current values to the parameter system
      CALL PAR_PUT0R( 'SEMIM', A, STATUS )
      CALL PAR_PUT0R( 'ECCEN', E, STATUS )
      CALL PAR_PUT0R( 'ANGLE', THETA, STATUS )

*   Release the character zone
      CALL SGS_RELZ( CZONE )

      END

* $Id$
