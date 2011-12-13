      SUBROUTINE CURPTS( NPTS, EXACT, MAXCHO, MARK, ERASE, CROSHT,
     :                   CONECT, DSTNCT, XLOW, XHIGH, YLOW, YHIGH,
     :                   XCUR, YCUR, NOBT, X, Y, STATUS )
*+
*  Name:
*     CURPTS

*  Purpose:
*     define points in an image via the cursor.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CURPTS( NPTS, EXACT, MAXCHO, MARK, ERASE, CROSHT, CONECT,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This subroutine enables use of the cursor of the current graphics
*     device to select a defined number of points whose co-ordinates are
*     returned.  Only positions between defined limits are accepted.
*     Optionally a cross may be drawn at each point, and each point may
*     be forced to be distinct.  Also lines may be drawn connecting
*     successive points.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Inform the user what is expected depending on the number of
*       points, whether or not an exact number required, whether or not
*       distinct points required
*     If points are to be marked then
*        Store current pen number
*        If marked points are to be erased then
*           Store a current polyline representation
*           Replace it with the background colour
*        Endif
*     Endif
*     Repeat until the required number of positions have been chosen or
*       an exit button has been depressed or there is an error
*        Repeat until a valid position has been chosen or an exit button
*             has been pressed or there is an error
*           Set cursor to the last position
*           Obtain a cursor position
*           If the button pressed was to accept the position then
*              If it lies within the defined region then
*              Reset last cursor position to its current position
*              If distinct points required and this is not the first
*                position then
*                 Check that the point is new
*              Endif
*              If there is no duplication (when distinct points are
*                required) then
*                 Increment count of points chosen
*                 Store the point in the output arrays
*                 Connect to the previous point with a straight line if
*                   required
*                 Mark with a cross (green on colour devices) if
*                   required
*                 Set repeat switch to off
*              Endif
*           Else
*              Set repeat switch to off
*           Endif
*        Enddo
*     Enddo
*     If points were marked then
*        If marked points are to be erased then
*           Mark the crosses again, but with the background pen
*           Restore input polyline representation
*        Endif
*        Restore input pen number
*     Endif
*     End

*  Copyright:
*     Copyright (C) 1989, 1990, 1991, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Malcolm Currie   STARLINK (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1989 Nov 10: Original (RAL::CUR).
*     1990 Feb 1 : Added optional lines between points, accidently
*        omitted in the original (RAL::CUR).
*     1990 May 2 : Added EXACT and ERASE arguments and functionality
*        (RAL::CUR).
*     1991 Mar 13: Used aspect source flags set to individual to erase
*        crosses, rather than set polyline representation
*        (RAL::CUR).
*     1993 Nov 10: Made erasure dependent on the ERASE argument.
*        (RAL::CUR).
*     1993 Dec  6: Fixed bug that could give an incorrection rejection
*        of distinct points when x or y positions were the
*        the same. (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SSE definitions


*  Status:
      INTEGER STATUS


*  Arguments Given:
      INTEGER
     :  NPTS,                   ! Number of points to be obtained
     :  MAXCHO                  ! Maximum choice number to accept a
                                ! point

      LOGICAL                   ! True if :
     :  CONECT,                 ! Successive points are to be connected
                                ! by straight lines
     :  DSTNCT,                 ! Distinct points obtained, i.e. no
                                ! duplicates
     :  ERASE,                  ! Marked points are to be erased on exit
     :  EXACT,                  ! An exact number of points are required
     :  MARK                    ! Chosen points are to be marked with a
                                ! cross

      REAL
     :  CROSHT,                 ! Height of the marking crosses
     :  XCUR, YCUR,             ! Start position of the cursor
     :  XLOW, YLOW,             ! Lower bound of the region for
                                ! valid cursor positions
     :  XHIGH, YHIGH            ! Upper bound of the region for
                                ! valid cursor positions


*  Arguments Returned:
      INTEGER
     :  NOBT                    ! Number of points obtained

      REAL
     :  X( NPTS ),              ! x co-ordinates of chosen points
     :  Y( NPTS )               ! y co-ordinates of chosen points


*  Local Variables:
      INTEGER
     :  COLI,                   ! Polyline colour index
     :  GSTAT,                  ! GKS status
     :  HITVAL,                 ! Number of button pressed on the
                                ! choice device to get a position
     :  I, N,                   ! Loop counters
     :  LASF( 13 ),             ! GKS list of aspect source flags
     :  LNTYPE,                 ! Polyline type
     :  PEN,                    ! Input SGS pen
     :  WKID                    ! Workstation identifier

      REAL
     :  RESOLX,                 ! Resolution of the device in x
     :  RESOLY,                 ! Resolution of the device in y
     :  XIN,                    ! x co-ordinate from the cursor
     :  YIN,                    ! y co-ordinate from the cursor
     :  XL, YL                  ! Last valid cursor position

      LOGICAL                   ! True if :
     :  AGAIN,                  ! When a simulated REPEAT..UNTIL loop
                                ! is to continue
     :  DELX,                   ! Absolute difference between the
                                ! current x position and an earlier one
                                ! is not zero
     :  DELY,                   ! Absolute difference between the
                                ! current y position and an earlier one
                                ! is not zero
     :  DUPLIC,                 ! The current cursor position has
                                ! already been selected and points must
                                ! be distinct
     :  DYNAMC                  ! Polyline colour representation is
                                ! dynamic


*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inform the user what is expected of him or her.

      IF ( NPTS .EQ. 1 ) THEN
         CALL MSG_OUT( 'CURSORINF',
     :     'Use the cursor to select one point.', STATUS )
      ELSE IF ( EXACT ) THEN
         IF ( DSTNCT ) THEN
            CALL MSG_SETI( 'NPTS', NPTS )
            CALL MSG_OUT( 'CURSORINF',
     :        'Use the cursor to select ^NPTS distinct points.',
     :        STATUS )
         ELSE
            CALL MSG_SETI( 'NPTS', NPTS )
            CALL MSG_OUT( 'CURSORINF',
     :        'Use the cursor to select ^NPTS points.', STATUS )
         END IF
      ELSE
         IF ( DSTNCT ) THEN
            CALL MSG_SETI( 'NPTS', NPTS )
            CALL MSG_OUT( 'CURSORINF',
     :        'Use the cursor to select up to ^NPTS distinct points.',
     :        STATUS )
         ELSE
            CALL MSG_SETI( 'NPTS', NPTS )
            CALL MSG_OUT( 'CURSORINF',
     :        'Use the cursor to select up to ^NPTS points.', STATUS )
         END IF
      END IF

*    Initialise the number of points obtained here in case there is a
*    GKS error.

      N = 0

      DYNAMC = .FALSE.

*    Store the current SGS pen as the crosses will be in green (if
*    colour is available).

      IF ( MARK ) THEN
         CALL SGS_IPEN( PEN )

         IF ( ERASE ) THEN

*          Does the device have a dynamic colour representation?

            CALL SGS_ICURW( WKID )
            CALL DYNCLR( WKID, DYNAMC, STATUS )

*          Display surface will clear at the end of an application
*          unless it is dynamic.

            IF ( DYNAMC .AND. STATUS .EQ. SAI__OK ) THEN

*             Inquire the GKS aspect source flags.

               CALL GQASF( GSTAT, LASF )

*             Watch out for any error.

               CALL GKS_GSTAT( STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 999

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*             Get plotting resolution for later deletion of the cross.

               CALL SGS_IDUN( RESOLX, RESOLY )
               RESOLX = 0.5 * RESOLX
               RESOLY = 0.5 * RESOLY
            END IF

            IF ( STATUS .NE. SAI__OK ) GOTO 999
         END IF
      END IF

*    Get the initial cursor position.

      XL = XCUR
      YL = YCUR

      HITVAL = 1

*    Loop until the escape choice is selected or the required number
*    of selections made.

      DO WHILE ( HITVAL .GT. 0 .AND. HITVAL .LE. MAXCHO .AND.
     :           N .LT. NPTS .AND. STATUS .EQ. SAI__OK )

*       If a message has already been displayed, and then the cursor
*       is used, the next message is no longer in synchronisation
*       with the cursor. So synchronise the message system just in case.

         CALL MSG_SYNC( STATUS )

*       Get the co-ordinates of a point.

         AGAIN = .TRUE.

         DO WHILE ( AGAIN )

*          If a message has already been displayed, and then the cursor
*          is used, the next message is no longer in synchronisation
*          with the cursor. So synchronise the message system just in
*          case.

            CALL MSG_SYNC( STATUS )

*          Read cursor position and button value.

            CALL SGS_SETCU( XL, YL )
            CALL SGS_REQCU( XIN, YIN, HITVAL )

*          See if a button to select a point has been given, as
*          opposed to an exit call.

            IF ( HITVAL .GT. 0 .AND. HITVAL .LE. MAXCHO ) THEN

*             Check whether the cursor position lies within the
*             defined region.

               IF ( ( XIN .GE. XLOW .AND. XIN .LE. XHIGH )
     :              .AND. ( YIN .GE. YLOW .AND.
     :                      YIN .LE. YHIGH ) ) THEN

                  DUPLIC = .FALSE.

*                Reset last cursor position.

                  XL = XIN
                  YL = YIN

*                Check for distinct values...

                  IF ( N .GT. 0 .AND. DSTNCT ) THEN
                     I = 1
                     DELX = ABS( X(I) - XIN ) .GT. 1.E-6 *
     :                      MAX( ABS( XIN ), ABS( X(I) ) )
                     DELY = ABS( Y(I) - YIN ) .GT. 1.E-6 *
     :                      MAX( ABS( YIN ), ABS( Y(I) ) )

*                   by checking every previous point unless a
*                   duplicate is found.

                     DO WHILE ( ( DELX .OR. DELY ) .AND. I .LE. N )
                        I = I + 1

*                      No more comparisons to do when I = N.

                        IF ( I .LE. N ) THEN
                           DELX = ABS( X(I) - XIN ) .GT. 1.E-6 *
     :                            MAX( ABS( XIN ), ABS( X(I) ) )
                           DELY = ABS( Y(I) - YIN ) .GT. 1.E-6 *
     :                            MAX( ABS( YIN ), ABS( Y(I) ) )
                        END IF
                     END DO

*                   If a duplicate has been found, I will be
*                   equal to the index of the duplicate element.

                     IF ( I .LT. N ) THEN
                        DUPLIC = .TRUE.
                        CALL MSG_OUT( 'CURPTS_TPR', 'Two distinct '/
     :                    /'points required.', STATUS )
                     END IF
                  ELSE

*                   No duplicates.

                     DUPLIC = .FALSE.

*                End of the distinct-positions check.

                  END IF

                  IF ( .NOT. DUPLIC ) THEN

*                   This point is valid so increment the count of
*                   points and store the co-ordinates in the arrays.

                     N = N + 1
                     X( N ) = XIN
                     Y( N ) = YIN

*                   Join two succesive points with a straight line if
*                   requested.

                     IF ( CONECT .AND. N .GT. 1 ) THEN
                        CALL SGS_LINE( X( N - 1 ), Y( N - 1 ), XIN,
     :                                 YIN )
                        CALL SGS_FLUSH
                     END IF

*                   Mark with a cross if requested.

                     IF ( MARK ) THEN
                        CALL SGS_SPEN ( 3 )
                        CALL KPG1_CROSS( XIN, YIN, CROSHT, STATUS )

*                      Want to plot the cross immediately.

                        CALL SGS_FLUSH
                     END IF

*                   Point obtained successfully so we want to exit the
*                   inner loop.

                     AGAIN = .FALSE.
                  END IF

               ELSE

                  CALL MSG_OUT( 'CURPTS_POZ', 'Point lies outside '/
     :              /'the allowed region.', STATUS )

*             End of check that the point lies within the allowed region.
*             AGAIN is still true, so will loop again, moving the cursor
*             to the last valid point or the region centre.

               END IF

            ELSE

*             An exit button has been pressed so exit from the main
*             loop.

               AGAIN = .FALSE.
            END IF

*       End of loop to get a single point.

         END DO

*    End of loop to get a series of points.

      END DO

*    Delete crosses when requested to do so.
      IF ( MARK .AND. ERASE ) THEN

         IF ( DYNAMC ) THEN

*          Set the linetype and colour index aspect source flags to
*          individual.

            LASF( 1 ) = 1
            LASF( 3 ) = 1
            CALL GSASF( LASF )

*          Store the current linetype and colour index.

            CALL GQLN( GSTAT, LNTYPE )
            CALL GQPLCI( GSTAT, COLI )

*          Watch out for any error.

            CALL GKS_GSTAT( STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*          Want a solid line in the background colour (pen 0) for
*          erasing crosses.

            CALL GSLN( 1 )
            CALL GSPLCI( 0 )

*          Erase the crosses.

            DO  I = 1, N
               CALL KPG1_CROSS( X( I ), Y( I ), CROSHT, STATUS )
            END DO

*          Restore the input linetype and colour index.

            CALL GSLN( LNTYPE )
            CALL GSPLCI( COLI )

*          Set the linetype and colour index aspect source flags to
*          bundled.

            LASF( 1 ) = 0
            LASF( 3 ) = 0
            CALL GSASF( LASF )

*          Watch out for any error.

            CALL GKS_GSTAT( STATUS )
         ELSE

*          Erasing the earlier crosses by a cruder method.

            DO  I = 1, N
               CALL SGS_CLRBL( X( I ) - 0.5 * CROSHT,
     :                         X( I ) + 0.5 * CROSHT,
     :                         Y( I ) - RESOLY, Y( I ) + RESOLY )
               CALL SGS_CLRBL( X( I ) - RESOLX, X( I ) + RESOLX,
     :                         Y( I ) - 0.5 * CROSHT,
     :                         Y( I ) + 0.5 * CROSHT )
            END DO
            CALL SGS_FLUSH
         END IF

*       Reset the SGS pen to its value on input.

         CALL SGS_SPEN( PEN )

      END IF

  999 CONTINUE
      NOBT = N

      END
