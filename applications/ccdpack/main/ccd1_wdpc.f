      SUBROUTINE CCD1_WDPC( LIST, PROG, TYPE, PREFIX, USEPRO, PROTEC,
     :                      CONTIN, TRAIL, FD, FTYPES, GIDIN, NNDF,
     :                      VALID, POINT, NFRAME, TEMP, STATUS )
*+
*  Name:
*     CCD1_WDPC

*  Purpose:
*     Writes a command to invoke a CCDPACK routine with multiple input
*     and outputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_WDPC( LIST, PROG, TYPE, PREFIX, USEPRO, PROTEC, CONTIN,
*                      TRAIL, FD, FTYPES, GIDIN, NNDF, VALID,
*                      POINT, NFRAME, TEMP, STATUS )

*  Description:
*     This routine locates all the frame of type TYPE within FTYPES.
*     Unless the TYPE argument is blank in which case all frames are
*     used (except those indicated by the VALID flags). The names of
*     these NDFs are then accessed. These are then written into a file
*     "PROG{n}" (where n is a unique number for this application run).
*     A command is then written to the file FD
*
*       PROG IN=^PROG{n} OUT=*//TRAIL
*
*     which is terminated with a continuation character. The command is
*     written in a style suitable for the type of script using the
*     arguments PREFIX, USEPRO, PROTEC and CONTIN.

*  Arguments:
*     LIST = LOGICAL (Given)
*        Whether or not the list of NDFs to be processed should be
*        reported via the logging system or not.
*     PROG = CHARACTER * ( * ) (Given)
*        The name of the program to be invoked by this command.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of frame to be selected for processing (one of those
*        in FTYPES or ' '. If ' ' then all valid non-master frames are
*        used -- it would be pointless process masters as they have
*        already been done).
*     PREFIX = CHARACTER * ( * ) (Given)
*        The prefix for commands.
*     USEPRO = LOGICAL (Given)
*        Whether quoting protection should be used (in case special
*        characters are present).
*     PROTEC( 2 ) = CHARACTER * ( * ) (Given)
*        The protection characters (if used).
*     CONTIN = CHARACTER * ( * ) (Given)
*        The continuation character for this script type.
*     TRAIL = CHARACTER * ( * ) (Given)
*        The trailing modification element to be used when forming the
*        output file names.
*     FD = INTEGER (Given)
*        The FIO file descriptor of the script.
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        The frame and filter types of the input NDFs. (1,*) are the
*        frame types, (2,*) are the filters.
*     GIDIN = INTEGER (Given)
*        GRP identifier of the input group of NDF names. On exit the
*        group is modified so that the names of the NDFs output from
*        the call produced by this routine are present instead of
*        original input names.
*     NNDF = INTEGER (Given)
*        The number of NDFs.
*     VALID( NNDF ) = LOGICAL (Given)
*        Mask of flags indicating which NDFs are valid (i.e. those which
*        should be processed, the others are ignored).
*     POINT( NNDF ) = INTEGER (Given and Returned)
*        Workspace for pointer to frames of given type. On exit this
*        will contain the indices of the frames selected.
*     NFRAME = INTEGER (Returned)
*        Number of frames of the given type located.
*     TEMP = CHARACTER * ( * ) (Returned)
*        The name of the temporary file that is used to contain the
*        NDF names. You should arrange to delete this.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
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
*     22-SEP-1993 (PDRAPER):
*        Original version.
*     31-JAN-1994 (PDRAPER):
*        Added ' ' notation for using all valid frames.
*     31-JAN-1994 (PDRAPER):
*        Added LIST argument.
*     16-APR-1997 (PDRAPER):
*        Changed to accomodate foreign file formats and slices.
*        Note this now uses IRG1_ routines!
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      LOGICAL LIST
      CHARACTER * ( * ) PROG
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) PREFIX
      LOGICAL USEPRO
      CHARACTER * ( * ) PROTEC( 2 )
      CHARACTER * ( * ) CONTIN
      CHARACTER * ( * ) TRAIL
      INTEGER FD
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      INTEGER GIDIN
      LOGICAL VALID( NNDF )

*  Arguments Given and Returned:
      INTEGER POINT( NNDF )

*  Arguments Returned:
      INTEGER NFRAME
      CHARACTER * ( * ) TEMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing blanks

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) MESS ! Output buffer
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( GRP__SZNAM ) OUT ! Output NDFs specification
      CHARACTER * ( GRP__SZNAM ) TMPNAM ! Temorary name
      CHARACTER COMC            ! GRP comment character
      CHARACTER INDC            ! GRP indirection character
      INTEGER START             ! Position in string
      INTEGER IAT               ! Position in string
      INTEGER I                 ! Loop variable
      INTEGER FDTMP             ! Temporary file FIO descriptor
      INTEGER LCONT             ! Used length of CONTIN
      LOGICAL OPEN              ! Temporary file is opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate pointers to those elements of the FTYPES array which have
*  frame types to be processed.
      NFRAME = 0
      IF ( TYPE .EQ. ' ' ) THEN

*  No types specified, just generate lists of pointers to all valid
*  frames which are not masters.
         DO 1 I = 1, NNDF
            IF ( VALID( I ) ) THEN
               IF ( FTYPES( 1, I )( 1 : 6 ) .NE. 'MASTER' ) THEN
                  NFRAME = NFRAME + 1
                  POINT( NFRAME ) = I
               END IF
            END IF
 1       CONTINUE
      ELSE

*  Get a list of pointers to all the frames of the specified type.
         CALL CCD1_LOCS2( FTYPES, 2, NNDF, 1, VALID, TYPE, POINT,
     :                    NFRAME, STATUS )
      END IF

*  If we havn't found any frames to process then set the status and
*  exit with an error.
      IF ( NFRAME .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'PROG', PROG )
         IF ( TYPE .EQ. ' ' ) THEN
            CALL ERR_REP( 'CCD1_WDPC1',
     :'  CCD1_WDPC: Unable to locate any valid frames for processing'//
     :' by routine ^PROG', STATUS )
         ELSE
            CALL MSG_SETC( 'TYPE', TYPE )
            CALL ERR_REP( 'CCD1_WDPC2',
     :'  CCD1_WDPC: Unable to locate any frames of type: ^TYPE, for'//
     :' processing by routine ^PROG', STATUS )
         END IF
         GO TO 99
      ELSE

*  Get a name for the indirection file.
         CALL CCD1_TMPNM( PROG, TEMP( 2: ), STATUS )

*  Get required GRP control characters.
         CALL GRP_GETCC( GIDIN, 'COMMENT', COMC, STATUS )
         CALL GRP_GETCC( GIDIN, 'INDIRECTION', INDC, STATUS )

*  Open the file.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         OPEN = .FALSE.
         CALL CCD1_OPFIO( TEMP( 2: ), 'WRITE', 'LIST', 0, FDTMP,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK ) OPEN = .TRUE.
         MESS = COMC // ' List of names used by ' // PROG
         CALL FIO_WRITE( FDTMP, MESS( :CHR_LEN( MESS ) ), STATUS )

*  Now extract the name of the NDFs and write these into the file.
         DO 3 I = 1, NFRAME
            NDFNAM = ' '
            CALL GRP_GET( GIDIN, POINT( I ), 1, NDFNAM, STATUS )

*  List the names of the NDFs using the logging system if requested
            IAT = CHR_LEN( NDFNAM )
            IF ( LIST ) THEN
               CALL MSG_SETC( 'NDFNAM', NDFNAM( :IAT ) )
               CALL CCD1_MSG( ' ', '    ^NDFNAM', STATUS )
            END IF

*  Write the name into a file.
            CALL FIO_WRITE( FDTMP, NDFNAM( :IAT ), STATUS )

*  Update the name in the group by adding the trailing modification
*  to the NDF name. Note if files have an extension then we need to
*  modify the name so that the trailing componeny comes before
*  the extension (this happens when using foreign formats).
            TMPNAM = ' '
            CALL CCD1_SLICE( NDFNAM( :IAT ), TMPNAM, START, STATUS )
            TMPNAM = ' '
            CALL CCD1_FSPEC( NDFNAM( :START ), ' ', 'TYPE', TMPNAM,
     :                       STATUS )
            IF ( TMPNAM .NE. ' ' ) THEN
               START = INDEX( NDFNAM( :START ),
     :                        TMPNAM( : CHR_LEN( TMPNAM ) ) )
            END IF
            TMPNAM = ' '
            CALL CCD1_INSER( TRAIL( :CHR_LEN( TRAIL ) ), NDFNAM( :IAT ),
     :                       START, TMPNAM, STATUS )
            CALL GRP_PUT( GIDIN, 1, TMPNAM, POINT( I ), STATUS )
 3       CONTINUE

*  Close the temporary file.
         CALL FIO_CLOSE( FDTMP, STATUS )

*  Now create the command for the script file. First add the routine
*  name. Note we're using the standard characterisations for the script
*  type.
         MESS = PREFIX//PROG//CONTIN
         CALL FIO_WRITE( FD, MESS( :CHR_LEN( MESS ) ), STATUS )
         LCONT = CHR_LEN( CONTIN )

*  Now the input and output file specifiers.
         IAT = 3
         MESS = ' '
         TEMP( 1: 1 ) = INDC
         CALL CCD1_ADKEY( 'IN', TEMP, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )
         TEMP( 1: 1 ) = ' '
         MESS = MESS( :IAT )//CONTIN
         CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )

         IAT = 3
         MESS = ' '
         OUT = '*'//TRAIL
         CALL CCD1_ADKEY( 'OUT', OUT, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )
         MESS = MESS( :IAT )//CONTIN
         CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )
      END IF

*  Exit label.
 99   CONTINUE
      END
* $Id$
