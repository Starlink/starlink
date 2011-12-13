      SUBROUTINE CCDNDFAC( STATUS )
*+
*  Name:
*     CCDNDFAC

*  Purpose:
*     Accesses a list of NDFs and writes their names to a file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDNDFAC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine accesses a list of NDFs and writes their names to a
*     text file. It is intended to be used as an aid to producing
*     procedures which require the facilities of NDF list access used
*     in CCDPACK. For this reason the usual application introductory
*     message is suppressed. The names of the NDFs may be written
*     out to the terminal as an aid to memory. If no NDFs are accessed
*     then the output file will not be created, testing for the
*     existence of this file is a platform independent way of
*     determining if the invocation has been successful.

*  Usage:
*     ccdndfac namelist echo

*  ADAM Parameters:
*     ECHO = _LOGICAL (Read)
*        If TRUE then the names of the NDFs will be written to the
*        terminal unless there is only one input NDF.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     IN = LITERAL (Read)
*        A list of NDF names. The NDF names should be separated
*        by commas and may include wildcards.
*        [!]
*     MAXNDF = _INTEGER (Read)
*        The maximum number of NDFs which should be accessed. If a null
*        return "!" is given for this parameter then the normal CCDPACK
*        limit will be applied.
*        [!]
*     NAMELIST = LITERAL (Read)
*        The name of the output file to contain the names of the
*        accessed NDFs.
*        [CCDNDFAC.LIS]

*  Examples:
*     ccdndfac ndf_name_list true
*        In this example the list of NDF names is written to
*        ndf_name_list and the NDF names are echoed to the terminal. No
*        constraint is placed on the number of NDFs accessed (other than
*        the normal CCDPACK limit).
*
*     ccdndfac ndf_name true maxndf=1
*        In this example only a single NDF name is accessed. The name is
*        not echoed to the terminal (even though echo is set TRUE).

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line (you may well want to do this
*     when using the application from a procedure).
*
*     Certain parameters (LOGTO and LOGFILE) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line. Global values may be set
*     and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1997, 2000 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1993 (PDRAPER):
*        Original version.
*     5-JUN-1997 (PDRAPER):
*        Added PAR_CANCL for IN parameter. This is to force reprompt
*        when used in I task mode.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'MSG_PAR'          ! Message system parameters

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding blanks

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) NAME ! Name of NDF
      INTEGER FD                 ! Output file descriptor
      INTEGER GID                ! NDG groups of NDF names
      INTEGER I                  ! Loop variable
      INTEGER NNDF               ! Number of NDFs in input group
      INTEGER MAXNDF             ! Maximum number of NDFs
      LOGICAL ECHO               ! Whether to echo NDF names
      LOGICAL OPEN               ! Output names list is open
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up CCDPACK. Do not advertise this as a genuine application.
*  Direct use of CCD1_OLOG is safe for this application as no use of the
*  NDF top-level locator common block is made.
      CALL CCD1_OPLOG( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Does the user want the NDF names echoed? Note we only echo if more
*  than one NDF name is given.
      CALL PAR_GET0L( 'ECHO', ECHO, STATUS )

*  How many NDFs are we allowed to access. Make sure that it is a
*  reasonable number.
      CALL PAR_GET0I( 'MAXNDF', MAXNDF, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAXNDF = CCD1__MXNDF
      ELSE
         MAXNDF = MAX( 1, MIN( CCD1__MXNDF, MAXNDF ) )
      END IF

*  Get the list of NDF names.
      CALL CCD1_NDFGL( 'IN', 1, MAXNDF, GID, NNDF, STATUS )
      CALL PAR_CANCL( 'IN', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Open the output file which will contain the list of NDF names.
         CALL CCD1_ASFIO( 'NAMELIST', 'WRITE', 'LIST', 0, FD, OPEN,
     :                    STATUS )

*  Write message about NDFs
         IF ( ECHO .AND. NNDF .GT. 1 ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
         END IF

*  Loop over all members of the group writing their names to the output
*  file and to the user if requred.
         DO 1 I = 1, NNDF
            NAME = ' '
            CALL GRP_GET( GID, I, 1, NAME, STATUS )

*  Now write out the name.
            CALL FIO_WRITE( FD, NAME( : CHR_LEN( NAME ) ), STATUS )

*  Echo this though the logging system if required.
            IF ( ECHO .AND. NNDF .GT. 1 ) THEN
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_SETI( 'N', I )
               CALL CCD1_MSG( ' ', '  ^N) ^NAME', STATUS )
            END IF
 1       CONTINUE

*  Close the output file.
         CALL FIO_CLOSE( FD, STATUS )


      ELSE  IF ( STATUS .EQ. PAR__NULL ) THEN

*  A null is acceptable, just get rid of the errors and continue.
         CALL ERR_ANNUL( STATUS )
      END IF

*  Annul group resources.
      CALL CCD1_GRDEL( GID, STATUS )

*  Release any NDF resources.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCDNDFAC_ERR',
     :   '  Error accessing a list of NDF names.',
     :   STATUS )
      END IF

*  Close the CCDPACK logging system.
      CALL CCD1_CLLOG( STATUS )

      END

* $Id$
