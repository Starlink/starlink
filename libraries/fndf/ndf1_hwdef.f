      SUBROUTINE NDF1_HWDEF( IDCB, APPN, STATUS )
*+
*  Name:
*     NDF1_HWDEF

*  Purpose:
*     Write default history information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HWDEF( IDCB, APPN, STATUS )

*  Description:
*     The routine writes default history information to the current
*     history record of an NDF, creating a new record if necessary. It
*     returns without action if (a) the NDF does not have a history
*     component, (b) it has been accessed read-only, (c) default
*     history information has already been written to the current
*     history record or, (d) the history update mode is 'DISABLED'.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the data object whose history is to be
*        updated.
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the currently-executing application. This is used if a
*        new history record has to be created by this routine,
*        otherwise it is ignored. If a blank value is given, then a
*        suitable default will be used instead.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine has to create a new history record, then the text
*     width used for this record will be equal to NDF__SZHIS. If it is
*     appending information to a pre-existing current history record,
*     then the new text will be formatted to the pre-existing text
*     width.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1993 (RFWS):
*        Original version.
*     20-MAY-1993 (RFWS):
*        Added support for history update modes.
*     2-JUN-1993 (RFWS):
*        Now adapts to all the possible history update modes.
*     16-JUN-1993 (RFWS):
*        Removed defaulting of application name (now handled at a lower
*        level).
*     4-AUG-1993 (RFWS):
*        Added mark and release of error stack.
*     7-OCT-1993 (RFWS):
*        Added executing file name to history text.
*     26-APR-1994 (RFWS):
*        Split verbose mode history text into two lines.
*     16-OCT-2009 (DSB):
*        Raise an NDF event after the default history has been written.
*     24-APR-2019 (DSB):
*        If tuning parameter FIXSW is defined, remove the path from the
*        software file name. This is to make it easier to compare NDFs
*        compared using different versions of the software.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether default history information is to be written.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           The NDF's access mode.

*  Arguments Given:
      INTEGER IDCB
      CHARACTER * ( * ) APPN

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_FIXSW = LOGICAL (Read)
*           Use a blank path for the software?

*  Local Variables:
      CHARACTER * ( 1 ) NODE     ! Node name (junk)
      CHARACTER * ( 64 ) MACH    ! Machine name
      CHARACTER * ( 64 ) RELE    ! System release number
      CHARACTER * ( 64 ) SYST    ! Operating system name
      CHARACTER * ( 64 ) VERS    ! System version number
      CHARACTER * ( NDF__SZFIL ) FILE ! Executing file name
      CHARACTER * ( NDF__SZHIS ) TEXT( 2 ) ! History text buffer
      INTEGER IAT                ! Position of final "/"
      INTEGER LFILE              ! Length of file name
      INTEGER NLINES             ! Number of lines of text to write

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack to prevent use of message tokens from affecting
*  any which may already be defined.
      CALL ERR_MARK

*  Ensure that history information is available in the DCB.
      CALL NDF1_DH( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component exists. Otherwise there is nothing to
*  do.
         IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Check that the NDF has been accessed for modification, that the
*  default history writing flag is still set, and that the update mode
*  is not 'DISABLED'. Otherwise there is nothing to do.
            IF ( ( DCB_MOD( IDCB ) .NE. 'READ' ) .AND.
     :           DCB_HDEF( IDCB ) .AND.
     :           ( DCB_HUMOD( IDCB ) .NE. NDF__HDISA ) ) THEN

*  Write information specific to the current software environment into
*  the history record.
               CALL NDF1_HWENV( IDCB, APPN, STATUS )

*  Now write additional environment independent information. Obtain
*  general system details (if required) and the name of the currently
*  executing file, indicating if it cannot be determined.
               IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HVERB ) THEN
                  CALL PSX_UNAME( SYST, NODE, RELE, VERS, MACH, STATUS )
               END IF
               CALL NDF1_GTFIL( FILE, LFILE, STATUS )
               IF ( LFILE .EQ. 0 ) THEN
                  LFILE = 9
                  FILE( : LFILE ) = '<unknown>'

*  If the FIXSW tuning flag indicates that we are to use a blank path for
*  the software, remove the path. This is intended to facilitate regression
*  testing, where you may want to compare results from two versions of the
*  software installed in different places.
               ELSE IF( TCB_FIXSW ) THEN
                  CALL CHR_LASTO( FILE, '/', IAT )
                  IF( IAT .GT. 0 ) THEN
                     FILE( : IAT ) = ' '
                     CALL CHR_LDBLK( FILE )
                  END IF
               END IF

*  Define message tokens for these values.
               IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HVERB ) THEN
                  CALL MSG_SETC( 'M', MACH )
                  CALL MSG_SETC( 'R', RELE )
                  CALL MSG_SETC( 'S', SYST )
                  CALL MSG_SETC( 'V', VERS )
               END IF
               CALL MSG_SETC( 'F', FILE( : LFILE ) )

*  Set up history text containing these values, adapting to the amount
*  of verbosity required.
               IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HVERB ) THEN
                  NLINES = 2
                  TEXT( 1 ) = 'Software: ^F'
                  TEXT( 2 ) = 'Machine: ^M, System: ^S ^V (release ^R)'
               ELSE
                  NLINES = 1
                  TEXT( 1 ) = 'Software: ^F'
               END IF

*  Append the text to the history record.
               CALL NDF1_HFWRT( IDCB, APPN, NLINES, TEXT, .TRUE.,
     :                          .FALSE., .FALSE., STATUS )

*  If OK (and history information was written), then clear the default
*  history writing flag.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DCB_HDEF( IDCB ) = .FALSE.

*  Use NDF1_EVENT to flag a "default history written" event. If the caller
*  has registered a handler for this type of event (using NDF_HNDLR), it
*  will be called. First, assign the name of the NDF to the MSG token
*  "NDF_EVENT", then raise the event.
                  CALL NDF1_EVMSG( 'NDF_EVENT', IDCB )
                  CALL NDF1_EVENT( 'DEF_HISTORY', STATUS )

               END IF
            END IF
         END IF
      END IF

*  Release the error stack.
      CALL ERR_RLSE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HWDEF', STATUS )

      END
