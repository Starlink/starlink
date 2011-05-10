      SUBROUTINE KPG1_QVID( PNDEV, PKG, CLASS, CRITER, MININT, STATUS )
*+
*  Name:
*     KPG1_QVID

*  Purpose:
*     Tests whether the current graphics device has suitable
*     image-display characteristics.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QVID( PNDEV, PKG, CLASS, CRITER, MININT, STATUS )

*  Description:
*     This routine determines whether the current graphics device is an
*     image display with suitable characteristics.  The checks are
*     performed in the following order: the class of the device is
*     checked against a supplied list of acceptable classes; the
*     presence of certain attributes, given in a supplied list are
*     checked in the order colour, input and open with reset; and
*     finally a minimum number of colour indices must be exceeded.

*  Arguments:
*     PNDEV = CHARACTER * ( * ) (Given)
*        The ADAM parameter associated with the current graphics
*        workstation.  It is used to generate error messages.
*     PKG = CHARACTER * ( * ) (Given)
*        The graphics system.  It must be 'SGS' or 'GKS'.
*     CLASS = CHARACTER * ( * ) (Given)
*        A list of acceptable workstation classes as defined by GNS,
*        each separated by a comma.  See SUN/57 for a list.
*     CRITER = CHARACTER * ( * ) (Given)
*        A list of criteria that the workstation must pass in order to
*        be an acceptable image display.  The options are: 'COLOUR' if
*        colour must be available, 'CURSOR' if a cursor must be
*        available on the workstation, 'INPUT' if the workstation must
*        support input, and 'RESET' if the workstation must be capable
*        of being opened without reset.  Non-standard criteria will be
*        ignored.
*     MININT = INTEGER (Given)
*        Minimum number of intensities or colour indices required.  The
*        routine ensures that there are at least the reserved number
*        plus eight, except for workstations in the WINDOW_OVERLAY
*        class, when 1 is the minimum.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  An SGS workstation must be open.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 10 (MJC):
*        Original version.
*     1991 July 5 (MJC):
*        Added cursor availability to the list of criteria.  Made to
*        test all criteria even if one has already not been achieved.
*     1992 June 16 (MJC):
*        Removed the constraint that the minimum number of colours
*        indices is the size of the palette.  Set minimum at 1 for
*        the WINDOW_OVERLAY class of device.
*     2011 May 10 (MJC):
*        Set mandatory bad status before calling ERR_REP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GKS_PAR'          ! GKS parameter definitions
      INCLUDE 'CTM_PAR'          ! Colour-table-management definitions
      INCLUDE 'GNS_PAR'          ! GNS constants

*  Arguments Given:
      CHARACTER * ( * )
     :  PNDEV,
     :  PKG,
     :  CLASS,
     :  CRITER

      INTEGER
     :  MININT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length ignoring trailing
                                 ! blanks

*  Local Constants:
      INTEGER MAXOPT             ! Maximum number of options
      PARAMETER( MAXOPT = 40 )

*  Local Variables:
      LOGICAL                    ! True if :
     :  CURSOR,                  ! A cursor is available
     :  LOOP                     ! Loop searching for a class to match

      INTEGER
     :  CONID,                   ! Connection identifier
     :  GSTAT,                   ! Graphics status
     :  I,                       ! Loop counter
     :  ICOL,                    ! Index of colour keyword in criteria
     :  ICUR,                    ! Index of cursor keyword in criteria
     :  IINPUT,                  ! Index of input keyword in criteria
     :  IRESET,                  ! Index of reset keyword in criteria
     :  MAXNOC,                  ! Maximum length of the array elements
     :  MCH( MAXOPT ),           ! Number of initial characters to
                                 ! compare to obtain each class option
                                 ! uniquely
     :  MINCH                    ! Number of initial characters to
                                 ! compare to obtain a class uniquely

      INTEGER
     :  MTX,                     ! Max.no. of text  bundle table entries
     :  MPL,                     ! Max.no. of polyline   "    "     "
     :  MPM,                     ! Max.no. of polymarker "    "     "
     :  MFA,                     ! Max.no. of fill-area  "    "     "
     :  MPI,                     ! Max.no. of pattern indices
     :  NCLASS,                  ! Number of acceptable classes supplied
     :  NCRIT,                   ! Number of characters in the criteria
     :  NCOLS,                   ! Maximum number of colours available
                                 ! on the device
     :  NINTS,                   ! Number of colour indices on the
                                 ! workstation
     :  NPCI,                    ! Number of predefined colour indices
     :  SWCOL,                   ! Colour availability on the device
     :  TSTAT,                   ! Temporary status
     :  WKID,                    ! GKS workstation identifier
     :  WKCAT,                   ! Workstation category
     :  WSTYPE                   ! Workstation type

      CHARACTER * ( GNS__SZKEY )
     :  ACLASS( MAXOPT ),        ! The array of class options
     :  PACKAGE * 3,             ! Graphics system
     :  WKCLAS,                  ! The name of the workstation
     :  WKOPEN                   ! Workstation reset flag

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Validate the input parameters.
*    ==============================

*    First the graphics system.  Later IDI will be added.

      PACKAGE = PKG
      CALL CHR_UCASE( PACKAGE )
      IF ( PACKAGE .NE. 'GKS' .AND. PACKAGE .NE. 'SGS' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'PKG', PKG )
         CALL ERR_REP( 'KPG1_QVID',
     :     'Unable to assess the image-display characteristics due to '/
     :     /'an invalid graphics system (^PKG) being specified.',
     :     STATUS )
         GOTO 999
      END IF

*    Extract the classes from the comma-separated string.
*    ====================================================

*    Separate the classes into an array and find the minimum number
*    of characters to compare.

      CALL KPG1_ABSET( ',', CLASS, ACLASS, NCLASS, MCH, MINCH, MAXNOC,
     :                 STATUS )

*    Get an uppercase set of the array for comparisons.

      IF ( STATUS .EQ. SAI__OK ) THEN
         DO  I = 1, NCLASS
            CALL CHR_UCASE( ACLASS( I ) )
         END DO
      END IF

*    Start of the section to handle the GKS system.
*    ==============================================

      IF ( PACKAGE .EQ. 'GKS' .OR. PACKAGE .EQ. 'SGS' ) THEN

*       Start the GNS system for GKS.

         CALL GNS_START( 'GKS', STATUS )

*       Inquire the workstation identifier for GKS inquiries.

         CALL SGS_ICURW( WKID )

*       Check that the device is one of the requested workstation
*       classes.
*       =========================================================

         IF ( NCLASS .GT. 0 ) THEN

*          Find the class of the workstation.

            CALL GNS_IWCG( WKID, 'CLASS', WKCLAS, STATUS )

*          Loop until a match is found incrementing the array counter.

            LOOP = .TRUE.
            I = 1
            DO WHILE ( LOOP .AND. I .LE. NCLASS )
               LOOP = .NOT. ( WKCLAS .EQ. ACLASS( I ) )
               IF ( LOOP ) I = I + 1
            END DO

*          Test whether a match was found.  If not, make a helpful error
*          report.

            IF ( I .GT. NCLASS ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'NAME', PNDEV )
               CALL MSG_SETC( 'CLASS', WKCLAS )
               CALL ERR_REP( 'KPG1_QVID_CLASS',
     :           '$^NAME workstation has an unsuitable class (^CLASS).',
     :           STATUS )
               GOTO 980
            END IF
         END IF

*       Check that the device has all the requested workstation
*       characteristics.
*       =======================================================

*       Get the workstation type.

         CALL GQWKC( WKID, GSTAT, CONID, WSTYPE )

*       Inquire whether GKS/SGS has reported an error.

         CALL GKS_GSTAT( STATUS )

*       Are there any criteria to test?

         NCRIT = CHR_LEN( CRITER )
         IF ( NCRIT .GT. 0 ) THEN

*          Look for whether colour is a requirement or not.
*          ================================================

            ICOL = INDEX( CRITER, 'COLOUR' )
            IF ( ICOL .GT. 0 ) THEN

*             Inquire whether colour is supported on the device.

               CALL GQCF( WSTYPE, GSTAT, NCOLS, SWCOL, NPCI )

*             Inquire whether GKS has reported an error

               CALL GKS_GSTAT( STATUS )

*             Colour is not available but was required so report a
*             helpful error message.  Note do not exit as the
*             workstation may fail on more than one criterion.

               IF ( SWCOL .EQ. GMONOC ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', PNDEV )
                  CALL ERR_REP( 'KPG1_QVID_COLOUR',
     :              '$^NAME workstation does not support colour.',
     :              STATUS )
               END IF
            END IF

*          Look for whether a cursor is a requirement or not.
*          ==================================================

            ICUR = INDEX( CRITER, 'CURSOR' )
            IF ( ICUR .GT. 0 ) THEN

*             Inquire whether there is a cursor.

               CALL SGS_ICUAV( CURSOR )

*             Inquire whether GKS has reported an error

               CALL GKS_GSTAT( STATUS )

*             A cursor is not available but was required so report a
*             helpful error message.

               IF ( .NOT. CURSOR ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', PNDEV )
                  CALL ERR_REP( 'KPG1_QVID_COLOUR',
     :              '$^NAME workstation does not have a cursor.',
     :              STATUS )
               END IF
            END IF

*          Look for whether input is a requirement or not.
*          ===============================================

            IINPUT = INDEX( CRITER, 'INPUT' )
            IF ( IINPUT .GT. 0 ) THEN

*             Inquire the category of the workstation.

               CALL GQWKCA( WSTYPE, GSTAT, WKCAT )

*             Inquire whether GKS has reported an error.

               CALL GKS_GSTAT( STATUS )

*             Colour is not available but was required so report a
*             helpful error message.

               IF ( WKCAT .LT. GINPUT ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', PNDEV )
                  CALL ERR_REP( 'KPG1_QVID_INPUT',
     :              '$^NAME workstation does not support input.',
     :              STATUS )
               END IF
            END IF

*          Look for whether no-reset on opening is a requirement or not.
*          =============================================================

            IRESET = INDEX( CRITER, 'RESET' )
            IF ( IRESET .GT. 0 ) THEN

*             Find the reset keyword of the workstation.  Note in order
*             to list all reasons for failing to be suitable a temporary
*             status is required.  This was not needed before because
*             for once it was advantageous for GKS not to use inherited
*             status.

               IF ( STATUS .NE. SAI__OK ) THEN
                  TSTAT = STATUS
                  STATUS = SAI__OK
               ELSE
                  TSTAT = SAI__OK
               END IF
               CALL GNS_IWCG( WKID, 'OPEN', WKOPEN, STATUS )
               IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT

*             The device resets which is not required so report a
*             helpful error message.

               IF ( WKOPEN .NE. 'NORESET' ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'NAME', PNDEV )
                  CALL ERR_REP( 'KPG1_QVID_RESET',
     :              '$^NAME workstation is not suitable since it '/
     :              /'cannot be opened without resetting.',
     :              STATUS )
               END IF
            END IF
         END IF

*       Inquire the number of greyscale intensities that are available
*       on the specified device.

         CALL GQLWK( WSTYPE, GSTAT, MPL, MPM, MTX, MFA, MPI, NINTS )

*       Inquire whether GKS/SGS has reported an error

         CALL GKS_GSTAT( STATUS )

*       See whether number-of-colour-indices criterion is satisfied or
*       not.

*         IF ( NINTS .LT. MAX( CTM__RSVPN, MININT ) ) THEN
         IF ( NINTS .LT. MININT ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PNDEV )
            CALL MSG_SETI( 'NINTS', NINTS )
            CALL MSG_SETI( 'MINN', MININT )
            CALL ERR_REP( 'KPG1_QVID_NINTS',
     :        '$^NAME workstation has only ^NINTS colour indices, '/
     :        /'when at least ^MINN are required.', STATUS )
            GOTO 980
         END IF

  980    CONTINUE

*       Stop the GNS system for GKS.

         CALL GNS_STOP( 'GKS', STATUS )
      END IF

  999 CONTINUE

      END
