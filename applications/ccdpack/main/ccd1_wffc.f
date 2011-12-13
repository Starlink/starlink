      SUBROUTINE CCD1_WFFC( LIST, PREFIX, USEPRO, PROTEC, CONTIN, TRAIL,
     :                      FILT, FD, FTYPES, GIDIN, NNDF, VALID,
     :                      POINT, NFRAME, TEMP, STATUS )
*+
*  Name:
*     CCD1_WFFC

*  Purpose:
*     Writes a command to flatfield all TARGET frames with a given
*     filter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_WFFC( LIST, PREFIX, USEPRO, PROTEC, CONTIN, TRAIL,
*                      FILT, FD, FTYPES, GIDIN, NNDF, VALID, POINT,
*                      NFRAME, TEMP, STATUS )

*  Description:
*     This routine locates all the frame of type TARGET within FTYPES
*     which have the correct filter. The names of these NDFs are then
*     accessed (via the group GIDIN). These names are then written
*     into a file "flatcor{n}" (where n is a unique number for this
*     application run). A command is then written to the file FD
*
*       flatcor IN=^flatcor{n} OUT=*//TRAIL
*
*     which is terminated with a continuation character. TRAIL is the
*     current modification to the original NDF names.
*
*     The command is written in a style suitable for the type of script
*     (arguments PREFIX, USEPRO, PROTEC and CONTIN).

*  Arguments:
*     LIST = LOGICAL (Given)
*        Whether or not to write the NDF names out via the logging
*        system.
*     PREFIX = CHARACTER * ( * ) (Returned)
*        The prefix for commands.
*     USEPRO = LOGICAL (Returned)
*        Whether quoting protection should be used (in case special
*        characters are present).
*     PROTEC( 2 ) = CHARACTER * ( * ) (Returned)
*        The protection characters (if used).
*     CONTIN = CHARACTER * ( * ) (Returned)
*        The continuation character for this script type.
*     TRAIL = CHARACTER * ( * ) (Given)
*        The modification expression to be applied to the NDF names
*        after they have been flatfielded.
*     FILT = CHARACTER * ( * ) (Given)
*        The type of filter for this flatfield.
*     FD = INTEGER (Given)
*        The FIO file descriptor of the script.
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        The frame and filter types of the input NDFs. (1,*) are the
*        frame types, (2,*) are the filters.
*     GIDIN = INTEGER (Given)
*        GRP identifier of the input group of NDF names. On exit the
*        names of any target frames are modified to be the names of the
*        NDFs output from FLATCOR.
*     NNDF = INTEGER (Given)
*        The number of NDFs.
*     VALID( NNDF ) = LOGICAL (Given)
*        Mask of flags indicating which NDFs are valid (i.e. those which
*        should be processed, the others are ignored).
*     POINT( NNDF ) = INTEGER (Given and Returned)
*        Workspace for pointer to frames of given type. On exit this
*        contain the indices of the frames selected for flatfielding.
*     NFRAME = INTEGER (Returned)
*        Number of frames located.
*     TEMP = CHARACTER * ( * ) (Returned)
*        The name of the temporary file that is used to contain the
*        NDF names. You should arrange to delete this.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*        Changed to modify the IRG group of NDF names.
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
      INTEGER NNDF
      CHARACTER * ( * ) PREFIX
      LOGICAL USEPRO
      CHARACTER * ( * ) PROTEC( 2 )
      CHARACTER * ( * ) CONTIN
      CHARACTER * ( * ) TRAIL
      CHARACTER * ( * ) FILT
      INTEGER FD
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      INTEGER GIDIN
      LOGICAL VALID( NNDF )

*  Arguments Given and Returned:
      INTEGER POINT( NNDF )

*  Arguments Returned:
      INTEGER NFRAME
      CHARACTER * ( *) TEMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) MESS ! Output buffer
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( GRP__SZNAM ) OUT ! Output NDFs specification
      CHARACTER COMC             ! GRP comment character
      CHARACTER INDC             ! GRP indirection character
      INTEGER IAT                ! Position in string
      INTEGER I                  ! Loop variable
      INTEGER FDTMP              ! Temporary file FIO descriptor
      INTEGER LCONT              ! Used length of CONTIN
      LOGICAL OPEN               ! Temporary file is opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First get a list of pointers to all the frames of type TARGET and
*  with filter FILT.
      CALL CCD1_LOCS3( FTYPES, 2, NNDF, 1, 2, VALID, 'TARGET', FILT,
     :                 POINT, NFRAME, STATUS )

*  If we havn't found any frames then set status and exit.
      IF ( NFRAME .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_WFFC1',
     :   '  CCD1_WFFC: Unable to locate any frames to flatfield.',
     :      STATUS )
         GO TO 99
      ELSE

*  Get a name for the indirection file.
         CALL CCD1_TMPNM( 'flatcor', TEMP( 2: ), STATUS )

*  Get required GRP control characters.
         CALL GRP_GETCC( GIDIN, 'COMMENT', COMC, STATUS )
         CALL GRP_GETCC( GIDIN, 'INDIRECTION', INDC, STATUS )

*  Open the file.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         OPEN = .FALSE.
         CALL CCD1_OPFIO( TEMP( 2: ), 'WRITE', 'LIST', 0, FDTMP,
     :                    STATUS )
         IF ( STATUS .EQ. SAI__OK ) OPEN = .TRUE.
         MESS = COMC // ' List of names used by flatcor (filter '
     :          //FILT( :CHR_LEN( FILT ) )//')'
         CALL FIO_WRITE( FDTMP, MESS( :CHR_LEN( MESS ) ), STATUS )

*  Now extract the name of the NDFs and write these into the file.
         DO 3 I = 1, NFRAME
            NDFNAM = ' '
            CALL GRP_GET( GIDIN, POINT( I ), 1, NDFNAM, STATUS )
            IAT = CHR_LEN( NDFNAM )
            CALL FIO_WRITE( FDTMP, NDFNAM( :IAT ), STATUS )

*  Write the names out to user if required.
            IF ( LIST ) THEN
               CALL MSG_SETC( 'NDFNAM', NDFNAM( :IAT ) )
               CALL CCD1_MSG( ' ', '    ^NDFNAM', STATUS )
            END IF

*  Modify the NDF group names to those of the output NDFs.
            CALL CHR_APPND( TRAIL, NDFNAM, IAT )
            CALL GRP_PUT( GIDIN, 1, NDFNAM( :IAT ), POINT( I ), STATUS )
 3       CONTINUE

*  Close the temporary file.
         CALL FIO_CLOSE( FDTMP, STATUS )

*  Now create the command for the script file. First add the routine
*  name. Note we're using the standard characterisations for the script
*  type.
         MESS = PREFIX//'flatcor'//CONTIN
         CALL FIO_WRITE( FD, MESS( :CHR_LEN( MESS ) ), STATUS )
         LCONT = CHR_LEN( CONTIN )

*  Now the input and output file specifiers.
         TEMP( 1: 1 ) = INDC
         IAT = 3
         MESS = ' '
         CALL CCD1_ADKEY( 'IN', TEMP, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )
         TEMP( 1: 1 ) = ' '
         MESS = MESS( :IAT )//CONTIN
         CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )

         OUT = '*'//TRAIL( :CHR_LEN( TRAIL ) )
         IAT = 3
         MESS = ' '
         CALL CCD1_ADKEY( 'OUT', OUT, USEPRO, PROTEC, MESS, IAT,
     :                    STATUS )
         MESS = MESS( :IAT )//CONTIN
         CALL FIO_WRITE( FD, MESS( :IAT+LCONT ), STATUS )
      END IF

*  Exit label.
 99   CONTINUE
      END
* $Id$
