      SUBROUTINE PICLIST( STATUS )
*+
*  Name:
*     PICLIST

*  Purpose:
*     Lists the pictures in the graphics database for a device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produces a summary of the contents of the
*     graphics database for a graphics device, and/or permits a picture
*     specified by its order in the list to be made the new current
*     picture.  The list may either be reported or written to a text
*     file.
*
*     The headed list has one line per picture.  Each line comprises
*     a reference number; the picture's name, comment (up to 24
*     characters), and label; and a flag to indicate whether or not
*     there is a reference data object associated with the picture.  A
*     `C' in the first column indicates that the picture that was
*     current when this application was invoked.  In the text file,
*     because there is more room, the name of the reference object is
*     given (up to 64 characters) instead of the reference flag.
*     Pictures are listed in chronological order of their creation.

*  Usage:
*     piclist [name] [logfile] [device] picnum=?

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics workstation. [The current graphics device]
*     LOGFILE = FILENAME (Write)
*        The name of the text file in which the list of pictures will
*        be made.  A null string (!) means the list will be reported
*        to you.  The suggested default is the current value. [!]
*     NAME = LITERAL (Read)
*        Only pictures of this name are to be selected.  A null string
*        (!) or blanks means that pictures of all names may be selected.
*        [!]
*     PICNUM = LITERAL (Read)
*        The reference number of the picture to be made the current
*        picture when the application exits.  PICNUM="Last" selects the
*        last picture in the database.  Parameter PICNUM is not accessed
*        if the list is written to the text file.  A null (!) or any
*        other error causes the current picture on entry to be current
*        again on exit.  The suggested default is null.

*  Examples:
*     piclist
*        This reports all the pictures in the graphics database for the
*        current graphics device.
*     piclist device=ps_l
*        This reports all the pictures in the graphics database for the
*        ps_l device.
*     piclist data
*        This reports all the DATA pictures in the graphics database for
*        the current graphics device.
*     piclist data logfile=datapic.dat
*        This lists all the DATA pictures in the graphics database for
*        the current graphics device into the text file datapic.dat.
*     piclist frame picnum=5
*        This selects the fifth most ancient FRAME picture (in the
*        graphics database for the current graphics device) as the
*        current picture.  The pictures are not listed.
*     piclist picnum=last
*        This makes the last picture in the graphics database for the
*        current graphics device current.  The pictures are not listed.

*  Notes:
*     -  The list is not reported to the user when PICNUM is specified
*     on the command line.  This feature is useful where a procedure
*     just wants to select a new current picture (hiding the details
*     from the user).  A new current picture cannot be selected with
*     text-file output, and so the presence of PICNUM on the command
*     line does not affect writing to a text file.

*  Related Applications:
*     KAPPA: PICBASE, PICDATA, PICEMPTY, PICENTIRE, PICFRAME, PICIN,
*     PICLAST, PICSEL, PICVIS.

*  Implementation Status:
*     -  Assumes that there are no more than 9999 pictures in the
*     database.

*  Timing:
*     The execution time is approximately proportional to the number of
*     pictures in the database for the chosen graphics device.
*     Selecting only a subset by name is slightly faster.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 24 (MJC):
*        Original version.
*     1992 January 29 (MJC):
*        Correctly Handles the case when there are no pictures of the
*        given name in the database.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 December 2 (MJC):
*        Added the current-picture indicator.
*     1993 August 18 (MJC):
*        Prevented reporting of the list when PICNUM is on the command
*        line, and allowed PICNUM to select the last picture.
*     1995 January 13 (MJC):
*        Made examples and usage lowercase.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI system errors

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length sans trailing blanks

*  Local Variables:
      INTEGER
     :  FD,                      ! Logfile descriptor
     :  I,                       ! Loop counter
     :  NCREF,                   ! Number of characters in reference
                                 ! name
     :  PICID,                   ! Current-picture identifier
     :  PICIDB,                  ! Base-picture identifier
     :  PICIDC,                  ! Graphics' database identifier on
                                 ! input
     :  PICIDE,                  ! Current picture identifier on exit
     :  PICIDS,                  ! Picture identifier to search from
     :  PICNO,                   ! Picture number
     :  PICOUT,                  ! Picture number to be made current
     :  STATE                    ! State of parameter PICNUM

      CHARACTER
     :  BUFFER * ( 132 ),        ! Buffer to create the output message
                                 ! or line in the text file
     :  COMMNT * ( 24 ),         ! Current search picture comment
     :  CPICOT * ( 4 ),          ! Picture to be selected
     :  CURFLA * ( 1 ),          ! Flag to indicate the input current
                                 ! picture
     :  LABEL * ( DAT__SZNAM ),  ! Current search picture label
     :  NAME * ( DAT__SZNAM ),   ! Current search picture name
     :  REF * ( 3 ),             ! Reference object present
     :  REFNAM * ( 64 ),         ! Reference name (only 64 characters
                                 ! available to fit on a line)
     :  SNAME * ( DAT__SZNAM )   ! Picture name to be searched for

      LOGICAL                    ! True if:
     :  CURFND,                  ! The current picture has been found
     :  CURRNT,                  ! The listed picture is the current one
     :  REPORT,                  ! The list is reported
     :  LOGF,                    ! The log file is open
     :  REFOBJ,                  ! There is a reference object
                                 ! associated with the current picture
     :  VALID                    ! Reference object is via a locator

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name parameter.  A null value is made equivalent to a blank
*  string, i.e. all pictures of any name may be selected.
      CALL ERR_MARK
      CALL PAR_DEF0C( 'NAME', ' ', STATUS )
      CALL PAR_GET0C( 'NAME', SNAME, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         SNAME = ' '
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Open the log file.  If null is returned from the parameter system,
*  the list of pictures are reported to the user directly.
      LOGF = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGF = .TRUE.
      END IF
      CALL ERR_RLSE

*  Determine if the PICNUM parameter was specified on the command line.
*  If it is not active, it has not been supplied.  Set the flag to
*  decide whether of not to report the list.
      CALL LPG_STATE( 'PICNUM', STATE, STATUS )
      REPORT = STATE .NE. PAR__ACTIVE

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Open the graphics database for the required device.
      CALL AGI_ASSOC( 'DEVICE', 'READ', PICIDC, STATUS )

*  Start a new AGI context.
      CALL AGI_BEGIN

*  Inquire the base picture for current workstation.
      CALL AGI_IBASE( PICIDB, STATUS )

*  Select this as the current picture.
      CALL AGI_SELP( PICIDB, STATUS )

*  Write the heading to the logfile or report it to the user.  Put a
*  blank line after it in the file and a heading for the Reference
*  object.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( LOGF ) THEN
            WRITE( BUFFER, '( ''  No.'',1X,''Name'',13X,''Comment'','/
     :        /'19X,''Label'',12X,''Reference Object'')' )
            CALL FIO_WRITE( FD, BUFFER, STATUS )
            WRITE( BUFFER, '(132(''-''))' )
            CALL FIO_WRITE( FD, BUFFER, STATUS )
            CALL FIO_WRITE( FD, ' ', STATUS )
         ELSE IF ( REPORT ) THEN
            WRITE( BUFFER, '( ''  No.'',1X,''Name'',13X,''Comment'','/
     :        /'19X,''Label'',11X,'' Ref'')' )
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'PICTHEAD', '^BUF', STATUS )
            WRITE( BUFFER, '(70(''-''))' )
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'PICTHEAD', '^BUF', STATUS )
         END IF
      END IF

*  Initialise the flags for indicating the input current picture.
      CURFND = .FALSE.
      CURRNT = .FALSE.

*  Recall all the pictures from the most ancient.
      PICNO = 1

*  Recall the first (most ancient) picture.
      CALL AGI_RCF( SNAME, PICID, STATUS )

      CALL ERR_MARK
      DO WHILE ( STATUS .EQ. SAI__OK )

         IF ( PICNO .GT. 1 ) THEN

*  Search for the next picture (except for the first occurrence where
*  it is known).  Again we must first select the base picture so the
*  search includes all pictures. (AGI_RCF/S made PICID current.)
            CALL AGI_SELP( PICIDB, STATUS )

*  Avoid exhausting the available picture identifiers by annulling each
*  one as we are finished with it unless it is the base picture or the
*  current picture when the application began.
            IF ( PICNO .GT. 2 .AND. .NOT. CURRNT )
     :        CALL AGI_ANNUL( PICIDS, STATUS )

*  Start the search from the last picture recalled.
            PICIDS = PICID

*  Recall the next picture.
            CALL AGI_RCS( SNAME, PICIDS, PICID, STATUS )
         END IF

*  Find out if this was the current picture on entry to this routine.
*  Since there was only one current picture, this comparison need only
*  be made until the current picture is located.
         IF ( REPORT .OR. LOGF ) THEN
            IF ( CURFND ) THEN
               CURRNT = .FALSE.
            ELSE
               CALL AGI_ISAMP( PICIDC, CURRNT, STATUS )
               CURFND = CURRNT
            END IF

*  Assign the current-picture indicator.
            IF ( CURRNT ) THEN
               CURFLA = 'C'
            ELSE
               CURFLA = ' '
            END IF

*  Get the name of the picture.
            CALL AGI_INAME( NAME, STATUS )

*  Get the comment of the picture.
            CALL AGI_ICOM( COMMNT, STATUS )

*  Obtain the label associated with the picture, if a label exists.
            CALL AGI_ILAB( PICID, LABEL, STATUS )

*  Determine whether or not there is a reference object associated with
*  the current picture.
            CALL KPG1_AGREF( PICID, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator to a token containing the path
*  name and file name, and tidy the reference locator; or if the
*  reference is just a name, write it to a token.  Note that the token
*  is renewed if it is to be used twice.  Put a flag to indicate there
*  is a reference.
            IF ( REFOBJ ) THEN
               CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
               IF ( VALID ) THEN
                  CALL KPG1_HMSG( 'RNAME', REFNAM( :DAT__SZLOC ) )
                  CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
                  CALL MSG_LOAD( 'REFNAME', '^RNAME', REFNAM, NCREF,
     :                           STATUS )
               ELSE
                  NCREF = CHR_LEN( REFNAM )
               END IF
               REF = 'Ref'

            ELSE
               REF = ' '
               REFNAM = ' '
               NCREF = 1
            END IF

*  Create a line summarising the picture details.  Write to the text
*  file or report the information to the user.  Note for the user there
*  is no room to give the reference object's name.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( LOGF ) THEN
                  WRITE( BUFFER, '(A,I4,1X,A,2X,A,2X,A,2X,A)' )
     :              CURFLA, PICNO, NAME, COMMNT, LABEL, REFNAM( :NCREF )
                  CALL FIO_WRITE( FD, BUFFER( :68+NCREF ), STATUS )
               ELSE
                  WRITE( BUFFER, '(A,I4,1X,A,2X,A,2X,A,2X,A)' )
     :              CURFLA, PICNO, NAME, COMMNT, LABEL, REF
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL MSG_OUT( 'PICTSUM', '^BUF', STATUS )
               END IF
            END IF
         END IF

*  Increment the picture count.
         IF ( STATUS .EQ. SAI__OK ) PICNO = PICNO + 1
      END DO

*  The last picture does not exist so decrement the picture count.
      PICNO = PICNO - 1

*  When the database of pictures is exhausted handle the error
*  invisibly.
      IF ( STATUS .EQ. AGI__NONAM .OR. STATUS .EQ. AGI__PICNF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Initialise the exit picture identifier.
*  =======================================

*  Ensure that the current picture on entrance to the application is
*  the current picture on exit, unless a new current picture is
*  selected.  A selection occurs when there is no logfile and no error.
      PICIDE = -1

*  Look out for no pictures of the supplied name.
*  ==============================================

*  Inform the user of the error.  The name cannot be null, since a null
*  name would always find at least one picture---the base.
      IF ( PICNO .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', SNAME )
         CALL ERR_REP( 'PICLIST_NOPIC',
     :     'PICLIST: There are no pictures with name ^NAME in the '/
     :     /'database.', STATUS )

      ELSE

*  Select a picture to be the current picture on exit.
*  ===================================================

*  A null response means the current picture on input is to be
*  restored, so merely annul the error.
         IF ( .NOT. LOGF ) THEN
            CALL ERR_MARK
            CALL PAR_MIX0I( 'PICNUM', '1', 1, PICNO, 'LAST', .FALSE.,
     :                      CPICOT, STATUS )

*  Translate the picture to a number.
            IF ( CPICOT .EQ. 'LAST' ) THEN
               PICOUT = PICNO
            ELSE
               CALL CHR_CTOI( CPICOT, PICOUT, STATUS )
            END IF

*  Handle a null status transparently.
            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Recall the required picture.
*  ============================

            ELSE

*  Recall the first (most ancient) picture.
               CALL AGI_RCF( SNAME, PICID, STATUS )

*  If it is not the first picture repeat the loop of earlier until we
*  reach the requested picture.
               IF ( PICOUT .GT. 1 ) THEN

                  DO I = 2, PICOUT

*  Select the base picture so there is a current picture that
*  encompasses all pictures.
                     CALL AGI_SELP( PICIDB, STATUS )

*  Avoid exhausting the available picture identifiers by annulling each
*  one as we are finished with it, but do not annul the base picture or
*  the current picture when the application began.
                     IF ( I .GT. 2 .AND. PICIDS .NE. PICIDC )
     :                  CALL AGI_ANNUL( PICIDS, STATUS )

*  Start the search from the last picture recalled.
                     PICIDS = PICID

*  Recall the next picture.
                     CALL AGI_RCS( SNAME, PICIDS, PICID, STATUS )
                  END DO
               END IF

*  Copy the identifier of the required picture to the exit.
               PICIDE = PICID
            END IF
            CALL ERR_RLSE
         END IF
      END IF

*  Close the AGI context.  Make the chosen picture current.
      CALL AGI_END( PICIDE, STATUS )

*  Close the database.
      CALL AGI_ANNUL( PICIDC, STATUS )

*  Close the text file.
      IF ( LOGF ) CALL FIO_ANNUL( FD, STATUS )

 999  CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICLIST_ERR',
     :     'PICLIST: Unable to list or select from the database '/
     :     /'pictures for the $DEVICE.', STATUS )
      END IF

      END
