      SUBROUTINE KPG1_PQVID( PNDEV, CLASS, CRITER, MININT, UP,
     :                       STATUS )
*+
*  Name:
*     KPG1_PQVID

*  Purpose:
*     Tests whether the current graphics device has suitable
*     image-display characteristics.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PQVID( PNDEV, CLASS, CRITER, MININT, UP, STATUS )

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
*     CLASS = CHARACTER * ( * ) (Given)
*        A list of acceptable workstation classes as defined by GNS,
*        each separated by a comma.  See SUN/57 for a list. Note, as
*        GNS does not yet support PGPLOT, no checks are mode on GNS
*        class when using PGPLOT. Class checks are ignored if a blank
*        string is supplied.
*     CRITER = CHARACTER * ( * ) (Given)
*        A list of criteria that the workstation must pass in order to
*        be an acceptable image display.  The options are: 'COLOUR' if
*        colour must be available, 'CURSOR' if a cursor must be
*        available on the workstation. Non-standard criteria will be
*        ignored.
*     MININT = INTEGER (Given)
*        Minimum number of intensities or colour indices required.  The
*        routine ensures that there are at least the specified number.
*     UP = INTEGER (Returned)
*        The highest available colour index available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  A PGPLOT workstation must be open.

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-AUG-1998 (DSB):
*        Original version, based on KPG1_QVID by MJC.
*     30-SEP-1999 (DSB):
*        DO not report a "no colour" error if colour has been requested
*        but only 1 colour is required.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table-management definitions
c      INCLUDE 'GNS_PAR'          ! GNS constants

*  Arguments Given:
      CHARACTER PNDEV*(*)
      CHARACTER CLASS*(*)
      CHARACTER CRITER*(*)
      INTEGER MININT

*  Arguments Returned:
      INTEGER UP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length ignoring trailing
                                 ! blanks

*  Local Constants:
      INTEGER MAXOPT             ! Maximum number of options
      PARAMETER( MAXOPT = 40 )

*  Local Variables:
c     CHARACTER ACLASS( MAXOPT )*( GNS__SZKEY) ! The array of class options
      CHARACTER VAL*1            ! PGPLOT information string
      INTEGER CI1                ! Minimum colour index
c     INTEGER I                  ! Loop counter
      INTEGER ICOL               ! Index of colour keyword in criteria
      INTEGER ICUR               ! Index of cursor keyword in criteria
c     INTEGER MAXNOC             ! Maximum length of the array elements
c     INTEGER MCH( MAXOPT )      ! Number of initial characters to compare to
c                                ! obtain each class option uniquely
c     INTEGER MINCH              ! Number of initial characters to compare to
c                                ! obtain a class uniquely
c     INTEGER NCLASS             ! Number of acceptable classes supplied
      INTEGER NCRIT              ! Number of characters in the criteria
      INTEGER VLEN               ! Length of PGPLOT information string
c     LOGICAL LOOP               ! Loop searching for a class to match?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  >>>> The following code which checks the GNS classes is commented out
*  >>>> because GNS does not yet support PGPLOT.

c      IF( CLASS .NE. ' ' ) THEN
c
*  Extract the classes from the comma-separated string.
*  ====================================================
*  Separate the classes into an array and find the minimum number
*  of characters to compare.
c         CALL KPG1_ABSET( ',', CLASS, ACLASS, NCLASS, MCH, MINCH,
c     :                    MAXNOC, STATUS )
c
*  Get an uppercase set of the array for comparisons.
c         IF ( STATUS .EQ. SAI__OK ) THEN
c            DO  I = 1, NCLASS
c               CALL CHR_UCASE( ACLASS( I ) )
c            END DO
c         END IF
c
*  Start the GNS system for PGPLOT.
c         CALL GNS_START( 'PGPLOT', STATUS )
c
*  Check that the device is one of the requested workstation
*  classes.
*  =========================================================
c         IF ( NCLASS .GT. 0 ) THEN
c
*  Find the class of the workstation.
c            CALL GNS_IWCG( WKID, 'CLASS', WKCLAS, STATUS )
c
*  Loop until a match is found incrementing the array counter.
c            LOOP = .TRUE.
c            I = 1
c            DO WHILE ( LOOP .AND. I .LE. NCLASS )
c               LOOP = .NOT. ( WKCLAS .EQ. ACLASS( I ) )
c               IF ( LOOP ) I = I + 1
c            END DO
c
*  Test whether a match was found.  If not, make a helpful error report.
c            IF ( I .GT. NCLASS ) THEN
c               STATUS = SAI__ERROR
c               CALL MSG_SETC( 'NAME', PNDEV )
c               CALL MSG_SETC( 'CLASS', WKCLAS )
c               CALL ERR_REP( 'KPG1_QVID_CLASS',
c        :           '$^NAME workstation has an unsuitable class (^CLASS).',
c        :           STATUS )
c               GOTO 980
c            END IF
c         END IF
c      END IF

*  Check that the device has all the requested workstation
*  characteristics.
*  =======================================================

*  Inquire the number of greyscale intensities that are available
*  on the specified device.
      CALL PGQCOL( CI1, UP )

*  Are there any criteria to test?
      NCRIT = CHR_LEN( CRITER )
      IF ( NCRIT .GT. 0 ) THEN

*  Look for whether colour is a requirement or not.
*  ================================================
         ICOL = INDEX( CRITER, 'COLOUR' )
         IF ( ICOL .GT. 0 ) THEN

*  If colour is not available but was required, report a helpful error
*  message.  Note do not exit as the workstation may fail on more than
*  one criterion.
            IF ( UP .EQ. 1 .AND. MININT .GT. 1 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'NAME', PNDEV )
               CALL ERR_REP( 'KPG1_PQVID_COLOUR',
     :              '$^NAME workstation does not support colour.',
     :                       STATUS )
            END IF
         END IF

*  Look for whether a cursor is a requirement or not.
*  ==================================================
         ICUR = INDEX( CRITER, 'CURSOR' )
         IF ( ICUR .GT. 0 ) THEN

*  Inquire whether there is a cursor.
            CALL PGQINF( 'CURSOR', VAL, VLEN )

*  A cursor is not available but was required so report a helpful error
*  message.
            IF( VLEN .EQ. 0 .OR. VAL( 1 : 1 ) .NE. 'Y' ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'NAME', PNDEV )
               CALL ERR_REP( 'KPG1_PQVID_COLOUR',
     :              '$^NAME workstation does not have a cursor.',
     :                       STATUS )
            END IF

         ENDIF

      END IF

*   See whether number-of-colour-indices criterion is satisfied or not.
      IF ( UP .LT. MININT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', PNDEV )
         CALL MSG_SETI( 'NINTS', UP )
         CALL MSG_SETI( 'MINN', MININT )
         CALL ERR_REP( 'KPG1_PQVID_NINTS',
     :        '$^NAME workstation has only ^NINTS colour indices, '/
     :        /'when at least ^MINN are required.', STATUS )
         GOTO 980
      END IF

  980    CONTINUE

*  Stop the GNS system for PGPLOT (PGPLOT is not yet supported by GNS, so
*  this is commented out).
c      CALL GNS_STOP( 'PGPLOT', STATUS )

  999 CONTINUE

      END
