      SUBROUTINE ATL_RDCH( VFS, IAST, STATUS )
*+
*  Name:
*     ATL_RDCH

*  Purpose:
*     Read an AST Object from a VFS using a Channel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDCH( VFS, IAST, STATUS )

*  Description:
*     Read an AST Object from a VFS (see atl2.c) using a Channel. The Channel
*     can be configured using a set of attribute settings specified in
*     the environment variable ATOOLS_CHATT_IN.

*  Arguments:
*     VFS = INTEGER (Given)
*        An identifier for the VFS holding the text, such as created by
*        routine ATL_RDVFS.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the VFS contains the dump of a Channel (of any class),
*     then the Object returned via IAST will be the Channel itself. The
*     exception to this is that if the "Begin " line at the start of
*     the dump ends with the string "(Read)", then the returned IAST
*     Object will be the Object read from the Channel, rather than the
*     Channel itself. For instance, if the VFS contains the dump of a
*     FitsChan, and the first line of the dump is "Begin FitsChan(Read)",
*     then the returned IAST object will be the Object read from the
*     FitsChan, rather than the FitsChan itself. This facility is only
*     available for top level objects (e.g. FitsChans contained within
*     FitsChans cannot be read in this way).

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2019 East Asian Observatory.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     15-OCT-2010 (DSB):
*        If the Object read from the dump is itself a Channel, and the
*        "Begin" line for the Channel ends with "(Read)", then read an
*        Object from the Channel and return it rather than returning the
*        Channel itself. Only one level of Channel nesting is allowed.
*     7-NOV-2017 (DSB):
*        Allow Channel attributes to be set using environment variable
*        ATOOLS_CHATT_IN.
*     16-SEP-2019 (DSB):
*        Changed to use a VFS as input rather than a GRP group. This
*        allows it to read long lines (such as used sometimes to store
*        MOCs).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants

*  Arguments Given:
      INTEGER VFS

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER VFSC
      INTEGER NEXT
      INTEGER SIZE
      LOGICAL READCH
      LOGICAL BEGIN
      INTEGER MXLEN
      INTEGER IPLINE
      COMMON /ATLSRC/ VFSC, NEXT, SIZE, READCH, BEGIN, MXLEN, IPLINE

*  External References:
      EXTERNAL ATL_SRC1

*  Local Variables:
      INTEGER CHAN
      INTEGER NEWCHAN
      CHARACTER ATTRS*500
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Store the VFS pointer in common so that the source function can
*  get at it.
      VFSC = VFS

*  Indicate we have not yet read a "Begin " line from the VFS.
      BEGIN = .FALSE.

*  If the object in the VFS is a dump of a Channel or any class, then
*  READCH indicates whether the object returned by this function should
*  be the Channel itself or the object contained within the channel
      READCH = .FALSE.

*  Initialise the next VFS element to be read.
      NEXT = 1

*  Store the size of the VFS.
      CALL ATL2_GTSIZ( VFS, SIZE, STATUS )

*  Get the maximum length of a line in the VFS and allocate a buffer to
*  store one line.
      CALL ATL2_GTMXL( VFS, MXLEN, STATUS )
      CALL PSX_CALLOC( MXLEN, '_BYTE', IPLINE, STATUS )

*  Create a Channel through which to read the Objects stored in the
*  VFS.
      CHAN = AST_CHANNEL( ATL_SRC1, AST_NULL, ' ', STATUS )

*  See if any attributes should be set in the channel.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PSX_GETENV( 'ATOOLS_CHATT_IN', ATTRS, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL AST_SET( CHAN, ATTRS, STATUS )
            IF( STATUS .EQ. AST__BADAT ) CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Attempt to read an object from the current channel.
      IAST = AST_READ( CHAN, STATUS )
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ATL_RDCH_ERR1', 'No AST Object could be '//
     :                 'read from the supplied file.', STATUS )
      END IF

*  If the object read from the Channel is itself a Channel, and the
*  READCH flag is set, then read an Object from the Channel, and return
*  it in place of the original Channel.
      IF( READCH .AND. AST_ISACHANNEL( IAST ) ) THEN
         NEWCHAN = IAST
         IAST = AST_READ( NEWCHAN, STATUS )
         IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', AST_GETC( NEWCHAN, 'Class', STATUS ) )
            CALL ERR_REP( 'ATL_RDCH_ERR2', 'No AST Object could be '//
     :                    'read from the ^C read from the supplied '//
     :                    'file.', STATUS )
         END IF
      END IF

*  Free line buffer.
      CALL PSX_FREE( IPLINE, STATUS )

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END


      SUBROUTINE ATL_SRC1( STATUS )
*+
*  Name:
*     ATL_SRC1

*  Purpose:
*     A source function for use with a standard AST Channel.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'

*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER VFSC
      INTEGER NEXT
      INTEGER SIZE
      LOGICAL READCH
      LOGICAL BEGIN
      INTEGER MXLEN
      INTEGER IPLINE
      COMMON /ATLSRC/ VFSC, NEXT, SIZE, READCH, BEGIN, MXLEN, IPLINE

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER BUF*255
      INTEGER BLEN
      INTEGER F
      INTEGER L
      LOGICAL TRUNC

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no more elements in the VFS, return a length of -1.
      IF( NEXT .GT. SIZE ) THEN
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  Otherwise, get the element from the VFS.
      ELSE
         CALL ATL2_GET( VFSC, NEXT, 1, %VAL(CNF_PVAL(IPLINE)),
     :                  TRUNC, STATUS, %VAL(MXLEN) )
         IF( TRUNC .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'ATL: Text truncated whilst reading an'//
     :                    ' AST object from a text file:', STATUS )
            CALL ERR_REP( ' ', BUF, STATUS )
         ELSE

*  Get the indices of the first and last non-blank characters.
            CALL CHR_FANDL( %VAL(CNF_PVAL(IPLINE)), F, L, %VAL(MXLEN) )

*  Copy the first part of the line into a local buffer.
            CALL CHR_COPY( %VAL(CNF_PVAL(IPLINE)), .FALSE., BUF, LSTAT,
     :                     %VAL(MXLEN) )

*  If we have not yet read a "Begin " line, and the current line
*  starts with "Begin ", indicate we have now read a "Begin " line
*  and go on to see if it is a Channel of any class that is to be read.

            IF( .NOT. BEGIN .AND. BUF( F : F + 5 ) .EQ. 'Begin ' ) THEN
               BEGIN = .TRUE.

*  Ensure "Begin" is followed by only a single space, and get the
*  potentially modified length.
               CALL CHR_LDBLK( BUF )
               CALL CHR_LDBLK( BUF( 7 : ) )
               L = CHR_LEN( BUF )

*  If the line is now "Begin <class>(Read)...", where <class> is
*  the name of a Channel class, then strip off the (Read) and set a
*  flag in common that indicates that the object returned by ATL_RDCH
*  should be the object read from the Channel, rather than the Channel
*  itself. Note, we drop any comment at the end of the line.
               IF( BUF( 7 : 19 ) .EQ. 'Channel(Read)' ) THEN
                  BUF = 'Begin Channel'
                  L = 13
                  READCH = .TRUE.

               ELSE IF( BUF( 7 : 20 ) .EQ. 'FitsChan(Read)' ) THEN
                  BUF = 'Begin FitsChan'
                  L = 14
                  READCH = .TRUE.

               ELSE IF( BUF( 7 : 20 ) .EQ. 'StcsChan(Read)' ) THEN
                  BUF = 'Begin StcsChan'
                  L = 14
                  READCH = .TRUE.

               ELSE IF( BUF( 7 : 20 ) .EQ. 'YamlChan(Read)' ) THEN
                  BUF = 'Begin YamlChan'
                  L = 14
                  READCH = .TRUE.

               ELSE IF( BUF( 7 : 19 ) .EQ. 'MocChan(Read)' ) THEN
                  BUF = 'Begin MocChan'
                  L = 13
                  READCH = .TRUE.

               ELSE IF( BUF( 7 : 19 ) .EQ. 'XmlChan(Read)' ) THEN
                  BUF = 'Begin XmlChan'
                  L = 13
                  READCH = .TRUE.

               END IF


*  Store the line in the channel, and increment the index of the next
*  element to be read from the VFS.
               CALL AST_PUTLINE( BUF, L, STATUS )
            ELSE
               CALL AST_PUTLINE( %VAL(CNF_PVAL(IPLINE)), L, STATUS,
     :                           %VAL(MXLEN) )
            END IF

            NEXT = NEXT + 1
         END IF
      END IF

      END
