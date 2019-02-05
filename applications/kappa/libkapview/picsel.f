      SUBROUTINE PICSEL( STATUS )
*+
*  Name:
*     PICSEL

*  Purpose:
*     Selects a graphics-database picture by its label.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICSEL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application selects by label a graphics-database picture of a
*     specified device.  If such a picture is found then it becomes the
*     current picture on exit, otherwise the input picture remains
*     current.  Labels in the database are stored in the case supplied
*     when they were created.  However, the comparisons of the label you
*     supply with the labels in the database are made in uppercase, and
*     leading spaces are ignored.
*
*     Should the label not be found the current picture is unchanged.

*  Usage:
*     picsel label [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device. [Current graphics device]
*     LABEL = LITERAL (Read)
*        The label of the picture to be selected.

*  Examples:
*     picsel GALAXY
*        This makes the picture labelled "GALAXY" the current picture on
*        the current graphics device.  Should there be no picture of
*        this name, the current picture is unchanged.
*     picsel A3 xwindows
*        This makes the picture labelled "A3" the current picture on the
*        xwindows device.  Should there be no picture of this name, the
*        current picture is unchanged.

*  Algorithm:
*     -  Get the label of the picture to be selected and converted to
*     uppercase.
*     -  Open graphics device and start database activity.  Get the base
*     picture and its label.  See whether or not the search need
*     continue.  If it does find the last picture in the database.
*     -  Loop until the picture with the required label is found or
*     the database is exhausted of pictures.  If a picture does not
*     have the required label re-select the base picture so the search
*     includes all pictures, and recall the picture preceding the last
*     one tested.
*     -  If the label was not found report what happened, and reset
*     the current picture to the input picture.
*     -  Annul the AGI device.

*  Related Applications:
*      KAPPA: PICDATA, PICDEF, PICEMPTY, PICENTIRE, PICFRAME, PICLABEL,
*      PICLAST, PICVIS.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2019 Science and Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     TIMJ: Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     1990 January 14 (MJC):
*        Original.
*     1990 April 12 (MJC):
*        Bug fix so that the last picture's label is tested, and
*        improved the closedown operation when there is no match with
*        the label.
*     1991 February 12 (MJC):
*        Annulled pictures to enable an unlimited number of pictures
*        to be searched.
*     1991 March 19 (MJC):
*        Converted to the SST prologue.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2019 February 1 (MJC):
*        Activate and deactivate PGPLOT.  This plaster fix appears to
*        prevent an AGI__DIFDV error in ORAC-DR when displaying to
*        KAPVIEW and then switching PGPLOT to create a PNG.  The
*        underlying cause has escaped detection for a few years.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'AGI_ERR'        ! AGI error definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  PICID,                 ! AGI input picture identifier
     :  PICIDB,                ! Base picture's identifier
     :  PICIDN,                ! Recalled picture's identifier
     :  PICIDS,                ! Start picture's identifier
     :  TSTAT                  ! Temporary status

      LOGICAL                  ! True if :
     :  DEVCAN,                ! Image-display parameter is to be
                               ! cancelled
     :  FIRST,                 ! The current picture is the last picture
                               ! i.e. it is the first time around the
                               ! search loop
     :  SEARCH                 ! The picture with the required label
                               ! has not been found

      CHARACTER*( DAT__SZNAM )
     :  LABEL,                 ! Picture label to be found
     :  PLABEL                 ! Picture label of current picture


*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the label of the picture to be selected.  It can be
*    mixed case, though the testing is in uppercase.

      CALL PAR_GET0C( 'LABEL', LABEL, STATUS )
      CALL CHR_UCASE( LABEL )
      CALL CHR_LDBLK( LABEL )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Start an AGI scope.

      CALL AGI_BEGIN

*    Open GKS workstation to reset device.

      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*    Activate PGPLOT.

      CALL AGP_ACTIV( STATUS )

*    If the graphics device was not available, report the error and
*    leave the programme.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_PICSEL_NID',
     :        'PICSEL: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 999

      END IF

*    Get the base picture, as all pictures lie within it.

      CALL AGI_IBASE( PICIDB, STATUS )
      CALL AGI_SELP( PICIDB, STATUS )

*    Get its label.

      CALL AGI_ILAB( PICIDB, PLABEL, STATUS )
      CALL CHR_UCASE( PLABEL )
      CALL CHR_LDBLK( PLABEL )

*    Test whether the search is over already.

      SEARCH = .NOT. ( PLABEL .EQ. LABEL )

*    Search picture identifier initialised. Start the search from the
*    last picture.

      IF ( SEARCH ) THEN
         CALL AGI_RCL( ' ', PICIDN, STATUS )

*       To prevent the last picture being annulled in the loop use a
*       flag.

         FIRST = .TRUE.
      END IF

*    Loop through the database to find a picture of the supplied label.

      DO WHILE ( STATUS .EQ. SAI__OK .AND. SEARCH )

*       Get the label of the recalled picture.  First time around it
*       will be the last picture in the database.

         CALL AGI_ILAB( PICIDN, PLABEL, STATUS )
         CALL CHR_UCASE( PLABEL )
         CALL CHR_LDBLK( PLABEL )

*       Test whether the search is over.

         SEARCH = .NOT. ( PLABEL .EQ. LABEL )

         IF ( SEARCH ) THEN

*          The label does not match the current picture, so step back
*          to the previous picture.  Again we must first select the base
*          picture so the search includes all pictures. (AGI_RCP/L made
*          PICIDN current.)

            CALL AGI_SELP( PICIDB, STATUS )

*          Avoid exhausting the available picture identifiers by
*          annulling each one as we are finished with it.

            IF ( .NOT. FIRST ) CALL AGI_ANNUL( PICIDS, STATUS )

*          Start the search from the last picture recalled.

            PICIDS = PICIDN

*          Search the database from last to first, ignoring picture
*          name.

            CALL AGI_RCP( ' ', PICIDS, PICIDN, STATUS )

*          The search picture is no longer the last picture.

            FIRST = .FALSE.
         END IF

      END DO

*    A slightly confusing error message can result if the label is not
*    found, namely "no picture of that name", whereas 'no picture with
*    that label" is more understandable. So look for an unsuccessful
*    search and annul the unhelpful error report.  If this error has
*    not occurred, let the user know what has happened, but not be
*    dogmatic.

      IF ( SEARCH ) THEN
         IF ( STATUS .EQ. AGI__NONAM ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PICSEL_NOLABEL',
     :        'PICSEL: There is no picture with the given label.',
     :        STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PICSEL_NOLABEL',
     :        'PICSEL: Unable to find a picture with the given label.',
     :         STATUS )

         END IF

*       Rather than be left in some arbitrary picture, though it will
*       usually be the base, make the current picture the input picture.

         IF ( STATUS .NE. SAI__OK ) THEN
            TSTAT = STATUS
            STATUS = SAI__OK

            CALL AGI_SELP( PICID, STATUS )
            IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT
         ELSE
            CALL AGI_SELP( PICID, STATUS )
         END IF

      END IF

 999  CONTINUE

*    Deactivate PGPLOT and close AGI workstation.

      CALL AGP_DEACT( STATUS )

      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*    End the AGI scope.

      CALL AGI_END( -1, STATUS )

      END
