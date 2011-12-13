      SUBROUTINE CURRE( CURSOR, PNLOW, PNUPP, LBND, UBND, STATUS )
*+
*  Name:
*     CURRE

*  Purpose:
*     Defines a region within the current zone

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*      CALL CURRE( CURSOR, PNLOW, PNUPP, LBND, UBND, STATUS )

*  Description:
*     This subroutine defines a 2-d region in the current zone. If a
*     cursor is available and required, then it is used to define the
*     rectangular area, otherwise, the co-ordinates of the region come
*     from the environment.

*  Arguments:
*     CURSOR = LOGICAL ( READ )
*         True if the cursor is required
*     PNLOW = CHARACTER ( READ )
*         Parameter name of lower-bound co-ordinates that defines a
*         region (if %CURSOR is false)
*     PNUPP = CHARACTER ( READ )
*         Parameter name of lower-bound co-ordinates that defines a
*         region (if %CURSOR is false)
*     LBND( 2 ) = REAL( WRITE )
*         Co-ordinates of the lower bound that defines a region (if
*         %CURSOR is false).
*     UBND( 2 ) = REAL( WRITE )
*         Co-ordinates of the upper bound that defines a region (if
*         %CURSOR is false).
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Get bounds of the zone and the position of its centre
*     If cursor mode then
*        Show functions of the choice device's buttons
*        Repeat until two final positions have been chosen or exit
*          button has been depressed or there is an error
*           Repeat until the first position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within current zone
*              Reset last cursor position to its current position
*              If exit button has been pressed set repeat switch to off
*              Else if first button has been pressed then
*                 Describe the next functions of the choice device and
*                   locator
*                 Mark the chosen point with a cross
*                 Switch off first-point repeat
*              Else
*                 Describe next functions of the choice device and
*                   locator
*              Endif
*           End repeat
*           Repeat until the second position has been chosen or there
*             is an error
*              Obtain a valid cursor position, i.e. within current zone
*              Reset last cursor position to its current position
*              If exit button has been pressed set repeat switch to
*                off and clear the crosses and box
*              Else if first button has been pressed then
*                 If the second position is different from the
*                   first then
*                    Erase previous cross
*                    Report the co-ordinates of the region
*                    Switch off second-point and main repeat switches
*                 Else
*                    Report the error and try again
*                 Endif
*              Else
*                 Try again
*              Endif
*           End repeat
*        End repeat
*     Else
*        Report bounds of the current zone
*        Obtain the four co-ordinates from the terminal such that
*          the two positions are different
*        If an error other than abort occurs, annul error and set the
*          region to be the full array
*        Tidy parameters
*     Endif
*     End

*  Copyright:
*     Copyright (C) 1989-1990, 1992 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*     1989 Apr 30: Original based on INRE (RAL::CUR).
*     1989 Oct 25: Removed some commentary for use with PRPCUR
*                  (RAL::CUR).
*     1990 Aug 6 : Used upper and lower bounds rather than x,y limits
*                  (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE

*  Global Constants:

      INCLUDE 'SAE_PAR'         ! SSE definitions
      INCLUDE 'PAR_ERR'         ! Parameter-system errors

*  Status:

      INTEGER STATUS

*  Arguments Given:

      LOGICAL CURSOR

      CHARACTER*(*)
     :  PNLOW, PNUPP

*  Arguments Returned:

      REAL
     :  LBND( 2 ),
     :  UBND( 2 )

*  Local Variables:

      INTEGER
     :  BUTTN1,                 ! Number of button pressed on the
                                ! choice device to get first x,y
     :  BUTTN2                  ! Number of choice pressed
                                ! to get second x,y co-ordinate pair

      REAL
     :  A, B,                   ! General variables
     :  DELTA,                  ! Width of the crosses
     :  LIML( 2 ),              ! Minimum permitted lower bound
     :  LIMU( 2 ),              ! Maximum permitted upper bound
     :  RESOLX,                 ! Resolution of the device in x
     :  RESOLY,                 ! Resolution of the device in y
     :  XCEN, YCEN,             ! Centre of the current zone
     :  XLOW, YLOW,             ! Lower bound of the current zone
     :  XHIGH, YHIGH,           ! Upper bound of the current zone
     :  XCUR01,                 ! First x co-ordinate from cursor
                                ! that defines the region
     :  XCUR02,                 ! Second x co-ordinate from cursor
                                ! that defines the region
     :  XL, YL,                 ! Last valid cursor position
     :  XM,                     ! Frame zone size in x (dummy)
     :  YCUR01,                 ! First y co-ordinate from cursor
                                ! that defines the region
     :  YCUR02,                 ! Second y co-ordinate from cursor
                                ! that defines the region
     :  YM                      ! Frame zone size in y (dummy)

      LOGICAL                   ! true if :
     :  AGAIN, AGAIN1,          ! when a simulated REPEAT..UNTIL loop
                                ! is to continue
     :  AGAIN2, REPEAT          ! when a simulated REPEAT..UNTIL loop
                                ! is to continue

*.
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get bounds of zone

      CALL SGS_IZONE( XLOW, XHIGH, YLOW, YHIGH, XM, YM )
      XCEN = 0.5 * ( XLOW + XHIGH )
      YCEN = 0.5 * ( YLOW + YHIGH )

*    If the cursor is available, use it to define two distinct, valid
*    points.

      IF ( CURSOR ) THEN

*       Get plotting resolution for later deletion of the cross

         CALL SGS_IDUN( RESOLX, RESOLY )
         RESOLX = 0.5 * RESOLX
         RESOLY = 0.5 * RESOLY
         DELTA = 0.005 * MIN( XHIGH - XLOW, YHIGH - YLOW )
         REPEAT = .TRUE.
         AGAIN1 = .TRUE.
         AGAIN2 = .TRUE.

*       Give actions of choices

         CALL MSG_OUT( 'CHOICE0', 'Get first point defining the '/
     :     /'region via the cursor.', STATUS )

         XL = XCEN
         YL = YCEN

         DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

*          Get the first co-ordinates.

            DO WHILE ( AGAIN1 .AND. STATUS .EQ. SAI__OK )

               AGAIN = .TRUE.

               DO WHILE ( AGAIN )

*                If a message has already been displayed, and then the
*                cursor is used, the next message is no longer in
*                synchronisation with the cursor. So synchronise the
*                message system.

                  CALL MSG_SYNC( STATUS )

                  CALL SGS_SETCU( XL, YL )
                  CALL SGS_REQCU( XCUR01, YCUR01, BUTTN1 )

*                Check whether they lie within the array.

                  IF ( ( XCUR01 .GE. XLOW .AND. XCUR01 .LE. XHIGH )
     :                 .AND. ( YCUR01 .GE. YLOW .AND.
     :                         YCUR01 .LE. YHIGH ) ) THEN

                     AGAIN = .FALSE.

*                   Reset last cursor position

                     XL = XCUR01
                     YL = YCUR01

                  ELSE

                     CALL MSG_OUT( 'CURRE_POZ', 'Point lies outside '/
     :                 /'the current zone.', STATUS )

*                   Reset choice

                     BUTTN1 = 0
                  END IF
               END DO

*             If the button is BREAK, then exit.

               IF ( BUTTN1 .EQ. 0 ) THEN

                   AGAIN1 = .FALSE.
                   AGAIN2 = .FALSE.
                   REPEAT = .FALSE.

*             If the button is CHOICE 1, then put a cross at the point.

               ELSE IF ( BUTTN1 .EQ. 1 ) THEN

*                Show new meanings

                  CALL MSG_OUT( 'CHOICE2', 'Get second point defining '/
     :              /'the region via the cursor', STATUS )

                  CALL SGS_SPEN ( 3 )
                  CALL KPG1_CROSS( XCUR01, YCUR01, DELTA, STATUS )
                  AGAIN1 = .FALSE.

*             Otherwise put a cross at the point return for another
*             point until the user presses the CHOICE 1 button.

               ELSE
                  CALL MSG_OUT( 'CHOICE0', 'Get first point defining '/
     :              /'the region', STATUS )

*             End of which-button-pressed check

               END IF

*          End of loop to get first point

            END DO

*          Get the second set of co-ordinates.

            A = XCUR01
            B = YCUR01
            XCUR02 = A
            YCUR02 = B
            XL = XCUR01
            YL = YCUR01

            DO WHILE ( AGAIN2 .AND. STATUS .EQ. SAI__OK )

               AGAIN = .TRUE.

               DO WHILE ( AGAIN )

*                If a message has already been displayed, and then the
*                cursor is used, the next message is no longer in
*                synchronisation with the cursor. So synchronise the
*                message system.

                  CALL MSG_SYNC( STATUS )

*                Set and get cursor position and button pressed

                  CALL SGS_SETCU( XL, YL )
                  CALL SGS_REQCU( XCUR02, YCUR02, BUTTN2 )

*                Check whether they lie within the array.

                  IF ( ( XCUR02 .GE. XLOW .AND. XCUR02 .LE. XHIGH )
     :                 .AND. ( YCUR02 .GE. YLOW .AND.
     :                         YCUR02 .LE. YHIGH ) ) THEN

                     AGAIN = .FALSE.

*                   Reset last cursor position

                     XL = XCUR02
                     YL = YCUR02

                  ELSE

                     CALL MSG_OUT( 'CURRE_POZ', 'Point lies outside '/
     :                  /'the current zone.', STATUS )

*                   Reset trackerball

                     BUTTN2 = 0
                     XCUR02 = A
                     YCUR02 = B

                  END IF
               END DO

*             If the button is BREAK, then exit.

               IF ( BUTTN2 .EQ. 0 ) THEN

                  AGAIN2 = .FALSE.
                  REPEAT = .FALSE.

*             If the button is CHOICE 1, then accept the region.

               ELSE IF ( BUTTN2 .EQ. 1 ) THEN

*                Check that the points are distinct.

                  IF ( ( XCUR01 .NE. XCUR02 ) .OR.
     :                 ( YCUR01 .NE. YCUR02 ) ) THEN

*                   Erasing the earlier cross.

                     CALL SGS_CLRBL( XCUR01-0.5*DELTA, XCUR01+0.5*DELTA,
     :                               YCUR01-RESOLY, YCUR01+RESOLY )
                     CALL SGS_CLRBL( XCUR01-RESOLX, XCUR01+RESOLX,
     :                               YCUR01-0.5*DELTA, YCUR01+0.5*DELTA)
                     CALL SGS_FLUSH

                     LBND( 1 ) = MIN( XCUR01, XCUR02 )
                     LBND( 2 ) = MIN( YCUR01, YCUR02 )
                     UBND( 1 ) = MAX( XCUR01, XCUR02 )
                     UBND( 2 ) = MAX( YCUR01, YCUR02 )

*                   Report the co-ordinates of the region

                     CALL MSG_SETR( 'CURRE_X1', LBND( 1 ) )
                     CALL MSG_SETR( 'CURRE_Y1', LBND( 2 ) )
                     CALL MSG_SETR( 'CURRE_X2', UBND( 1 ) )
                     CALL MSG_SETR( 'CURRE_Y2', UBND( 2 ) )
                     CALL MSG_OUT( 'CURRE_RES', 'Co-ordinates '/
     :                 /'are ( ^CURRE_X1, ^CURRE_Y1 ) and '/
     :                 /'( ^CURRE_X2, ^CURRE_Y2 )', STATUS )
                     AGAIN2 = .FALSE.
                     REPEAT = .FALSE.

                  ELSE

*                   Otherwise, if the points are indistinct, then
*                   get new points.

                     CALL MSG_OUT( 'CURRE_TPR', 'Two distinct '/
     :                 /'points required.', STATUS )
                     AGAIN1 = .TRUE.
                     AGAIN2 = .TRUE.

                  END IF

               ELSE

*             If any other button is pressed

                  A = XCUR02
                  B = YCUR02
                  AGAIN2 = .TRUE.

*             End of which-button-pressed check

               END IF

*          End of repeat until loop for second co-ordinate pair

            END DO

*       End of main repeat until loop

         END DO

         CALL SGS_SPEN( 1 )

*    the region is to be defined from the environment

      ELSE

*       Set defaults to current region
*       Report the co-ordinates of the current zone

         CALL MSG_SETR( 'CURRE_X1', XLOW )
         CALL MSG_SETR( 'CURRE_Y1', YLOW )
         CALL MSG_SETR( 'CURRE_X2', XHIGH )
         CALL MSG_SETR( 'CURRE_Y2', YHIGH )
         CALL MSG_OUT( 'CURRE_RES', 'Co-ordinates are ( ^CURRE_X1, '/
     :     /'^CURRE_Y1 ) and ( ^CURRE_X2, ^CURRE_Y2 )', STATUS )

         REPEAT = .TRUE.

         DO WHILE ( REPEAT )

*          Store the maximum and minimum values permitted for passing
*          to the PAR routine.  Also use these as the suggested
*          defaults.

            LIML( 1 ) = XLOW
            LIML( 2 ) = YLOW
            LIMU( 1 ) = XHIGH
            LIMU( 2 ) = YHIGH

*          Obtain the lower bound of the region.

            CALL PAR_GRM1R( PNLOW, 2, LIML, LIML, LIMU, .FALSE., LBND,
     :                      STATUS )

*          Revise minimum values permitted for the upper bound of the
*          region.

            LIML( 1 ) = LBND( 1 )
            LIML( 2 ) = LBND( 2 )

*          Obtain the upper bound of the region.

            CALL PAR_GRM1R( PNUPP, 2, LIMU, LIML, LIMU, .FALSE., UBND,
     :                      STATUS )

            IF ( STATUS .EQ. PAR__ABORT ) THEN
               REPEAT = .FALSE.

*          If the points are not distinct, then report the error.

            ELSE IF ( ( LBND( 1 ) .EQ. UBND( 1 ) ) .AND.
     :                ( LBND( 2 ) .EQ. UBND( 2 ) ) ) THEN

*             Start new error context.

               CALL ERR_MARK

*             Flush the error so the user can try again.

               STATUS = SAI__ERROR
               CALL ERR_REP( 'CURRE_DPR',
     :           'CURRE: Error in data. Two distinct points '/
     :           /'required.', STATUS )
               CALL ERR_FLUSH( STATUS )

*             Close the error context.

               CALL ERR_RLSE

            ELSE

*             Success --- end loop

               REPEAT = .FALSE.

            END IF

*          Tidy up so that subroutine may be called again

            CALL PAR_CANCL( PNLOW, STATUS )
            CALL PAR_CANCL( PNUPP, STATUS )
         END DO

*    end of cursor-mode check

      END IF

      END
