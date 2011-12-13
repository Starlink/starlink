      SUBROUTINE SHL_GETHLP( HELPLB, KEYWRD, INTER, STATUS )
*+
*  Name:
*     SHL_GETHLP

*  Purpose:
*     Prints help text from a library of help information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SHL_GETHLP( HELPLB, KEYWRD, INTER, STATUS )

*  Arguments:
*     HELPLB = CHARACTER (Given)
*        Name of help file to open. See SHL_TRNVAR in order to
*        obtain this filename from an environment variable.
*     KEYWRD = CHARACTER (Given)
*        Keyword to use to search help system. Space separated
*        hierarchy. Can be blank.
*     INTER = LOGICAL (Given)
*        If true then an interactive help session is entered. Otherwise,
*        control is returned to the calling routine when the help item
*        specified by KEYWRD has been displayed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Prints help text from an help library. A specific keyword/topic
*     can be specified.

*  Copyright:
*     Copyright (C) 1984, 1986, 1988, 1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1984 Nov 3 (MJC):
*        Original.
*     1986 Nov 14 (MJC):
*        Converted to ADAM style prologue and status argument added.
*     1988 Sept 7 (MJC):
*        Used KAPPA input and output routines for LIB$GET_INPUT and
*        LIB$PUT_OUTPUT.
*     1992 June 17 (MJC):
*        Uses portable help system.  Converted to SST prologue and
*        documented global parameters.
*     1992 August 4 (MJC):
*        Incorporate revisions to the portable help system.
*     15 July 2004 (TIMJ):
*        Import into shared SHL library
*     14 August 2004 (TIMJ/DSB):
*        Add INTER flag to disable interactivity (from IRCAMPACK)
*     14 August 2004 (TIMJ):
*        Use FIO to get unit number
*     4 Oct 2004 (TIMJ):
*        Use SHL_PAGRST to initialise common block
*     19 Oct 2004 (TIMJ):
*        Over zealous with WIDTH removal. Fix things.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      LOGICAL   INTER
      CHARACTER * ( * )
     :  HELPLB,
     :  KEYWRD

*  Status:
      INTEGER STATUS             ! Inherited global status

*  External References:
      INTEGER
     :  CHR_LEN,                 ! Character string length ignoring
                                 ! trailing blanks
     :  SHL_GTHLPI,             ! Routine for reading help command
     :  HLP_HELP,                ! Interactive help
     :  SHL_PTHLPO              ! Routine for outputting help

      EXTERNAL
     :  SHL_GTHLPI,             ! Gets the help information
     :  HLP_NAMETR,             ! Interactive help library-name
                                ! translation
     :  SHL_PTHLPO              ! Outputs the help information

*  Local Variables:
      CHARACTER * ( 80 )
     :  ERRMSG                   ! Error message

      INTEGER
     :  HFLAGS,                  ! Help flags
     :  HLPLEN,                  ! Length of the help library name
     :  ISTAT,                   ! Local status
     :  LUHLP,                   ! Logical unit for reading the help
                                 ! library
     :  KWRDLN,                  ! Length of the keyword
     :  LBOT,                    ! Dummy variable
     :  WIDTH                    ! Width of the screen in characters

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the lengths of the input strings.
      HLPLEN = CHR_LEN( HELPLB )
      KWRDLN = CHR_LEN( KEYWRD )

*  Initialise COMMON-block variables.
*  ==================================

      CALL SHL_PAGRST( STATUS )

*  Get help.
*  =========

*  Set logical unit number for help
      CALL FIO_GUNIT( LUHLP, STATUS )

*  Set the flags to enable prompting if required.
      IF( INTER ) THEN
         HFLAGS = 1
      ELSE
         HFLAGS = 0
      END IF

*  We need the screen width
      CALL ONE_SCRSZ( WIDTH, LBOT, STATUS )
      IF (STATUS .NE. SAI__OK) RETURN

*  Initiate interactive help session.
      ISTAT = HLP_HELP( SHL_PTHLPO, WIDTH, KEYWRD( 1:KWRDLN ), LUHLP,
     :                  HELPLB( 1:HLPLEN ), HFLAGS, SHL_GTHLPI,
     :                  HLP_NAMETR )


*  Return the unit number to the pool (before we check status from HLP)
      CALL FIO_PUNIT( LUHLP, STATUS )

*  Watch for an error status.
      IF ( ISTAT .NE. 1 ) THEN
         CALL HLP_ERRMES( ISTAT, ERRMSG )
         CALL MSG_SETC( 'ERR', ERRMSG )
         CALL MSG_SETC( 'TOPIC', KEYWRD )
         CALL MSG_SETC( 'LIB', HELPLB )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SHL_GETHLP_ERR',
     :     'Error accessing help on topic ''^TOPIC'' in library ^LIB. '/
     :     /'Reason was "^ERR". ', STATUS )
      END IF

      END
