      SUBROUTINE KPG1_RCATW( CI, IAST, STATUS )
*+
*  Name:
*     KPG1_RCATW

*  Purpose:
*     Attempt to read an AST Object from a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RCATW( CI, IAST, STATUS )

*  Description:
*     This routine attempts to read an AST Object from the textual
*     information stored with the supplied catalogue (see SUN/181).
*     Reading of the textual information in the catalogue commences
*     at the current line (i.e. access to the textual information is not
*     reset before reading commences).
*
*     AST Objects can be written to a catalogue using routine KPG1_WCATW.

*  Arguments:
*     CI = INTEGER (Given)
*        A CAT identifier (see SUN/181) for the supplied catalogue.
*     IAST = INTEGER (Returned)
*        An AST pointer to the returned Object. AST__NULL is returned if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 1999, 2001, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1998 (DSB):
*        Original version.
*     26-MAY-1999 (DSB):
*        Only read the required length from each GRP element.
*     8-DEC-1999 (DSB):
*        Put in hack to allow for CAT changing WCS text class from
*        COMMENT to AST.
*     6-APR-2001 (DSB):
*        Limit max length of a line of text to no more than the size of a
*        GRP element.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants (needed by KPG_AST)
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER CI

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_SRCTA
      INTEGER CHR_LEN

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTGRP = INTEGER (Write)
*           GRP identifier for group holding AST_ data.
*        ASTLN = INTEGER (Write)
*           Next element to use in group holding AST_ data.
*        ASTTSZ = INTEGER (Write)
*           Max length of text to read from a GRP element.

*  Local Variables:
      CHARACTER CLASS*8          ! CAT text class
      CHARACTER NAME*(CAT__SZCNM)! Catalogue name
      CHARACTER TXT*40           ! Buffer for starting text
      INTEGER IAT                ! Index into string
      INTEGER CHAN               ! Pointer to AST Channel for reading catalogue
      INTEGER IPBUF              ! Pointer to text buffer
      INTEGER LINESZ             ! Length of text buffer
      INTEGER LSTAT              ! Local status returned by CHR routines
      INTEGER TLEN               ! Significant length of text buffer
      LOGICAL DONE               ! Has all textual information been used?

*.

*  Initialise returned pointer.
      IAST = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transfer all remaining textual information of class COMMENT to a
*  GRP group in order that we can have random access to it.
*  ================================================================

*  Create a buffer for a single line of textual information, giving it
*  the maximum length allowed by the catalogue format.
      CALL CAT_SZTXT( CI, 'READ', LINESZ, STATUS )
      CALL PSX_CALLOC( LINESZ, '_CHAR', IPBUF, STATUS )

*  Check the pointer can be used.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create a new group. The returned identifier is stored in common so
*  that it can be accessed by the AST_CHANNEL source function.
      CALL GRP_NEW( 'CAT textual information', ASTGRP, STATUS )

*  Initialise the maximum line length.
      ASTTSZ = 0

*  Loop until the ned of the textual information has been reached.
      DONE = .FALSE.
      DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  Read the next line of textual information. Add extra trailing
*  arguments which pass (by value, not reference) the length of the
*  character arguments. These are needed by Unix compilers because
*  we are using a pointer (IPBUF) instead of a genuine CHARACTER
*  variable.
         CALL CAT_GETXT( CI, DONE, CLASS, %VAL( CNF_PVAL( IPBUF ) ),
     :                   STATUS, %VAL( CNF_CVAL( LEN( CLASS ) ) ),
     :                   %VAL( CNF_CVAL( LINESZ ) ) )

*  Ignore it if the class is not COMMENT.
         IF( .NOT. DONE .AND. ( CLASS .EQ. 'COMMENT' .OR.
     :       CLASS .EQ. 'AST' ) ) THEN

*  Find "!!" in the string. This is added to the start of each line of AST
*  information when it is written to the catalogue. It marks the start of the
*  actual AST information (CAT can add leading spaces to the start of the
*  line which disrupts the mechanism for finding continuation lines).
            IAT = 1
            CALL CHR_FIND( %VAL( CNF_PVAL( IPBUF ) ), '!!', .TRUE., IAT,
     :                     %VAL( CNF_CVAL( LINESZ ) ) )

*  Shift the string to the left in order to remove everything upto the final
*  character in "!!".
            CALL KPG1_CSHFT( -( IAT + 1 ), %VAL( CNF_PVAL( IPBUF ) ),
     :                          %VAL( CNF_CVAL( LINESZ ) ) )

*  Update the maximum line length after removal of everything upto the
*  final character in "!!".
            ASTTSZ = MAX( ASTTSZ, LINESZ - IAT - 1 )

*  Report an error if the used length of the text is too long to be
*  stored in a GRP group without truncation.
            TLEN = CHR_LEN( %VAL( CNF_PVAL( IPBUF ) ),
     :                      %VAL( CNF_CVAL( LINESZ ) ) )
            IF( TLEN .GT. GRP__SZNAM .AND. STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIQAC( CI, 'NAME', NAME, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'CAT', NAME )
               CALL MSG_SETI( 'TLEN', TLEN )
               CALL MSG_SETI( 'GLEN', GRP__SZNAM )
               CALL ERR_REP( 'KPG1_RCATW_1', 'Textual information in '//
     :                       'in catalogue ''^CAT'' is too long to '//
     :                       'process. The following line has ^TLEN '//
     :                       'characters but only ^GLEN can be '//
     :                       'processed:',STATUS )
               CALL CHR_COPY( %VAL( CNF_PVAL( IPBUF ) ),
     :                        .FALSE., TXT, LSTAT,
     :                        %VAL( CNF_CVAL( LINESZ ) ) )
               CALL MSG_SETC( 'TXT', TXT )
               CALL ERR_REP( 'KPG1_RCATW_2', '   ^TXT...', STATUS )

*  If the text is not too long, but is not of zero length, append it to the
*  end of the group.
            ELSE IF( TLEN .GT. 0 ) THEN
               CALL GRP_PUT( ASTGRP, 1, %VAL( CNF_PVAL( IPBUF ) ),
     :                       0, STATUS, %VAL( CNF_CVAL( LINESZ ) ) )
            END IF
         END IF

      END DO

*  The length to read from each line should not exceed the length of a
*  GRP element. STL catalogues have a maximum of 1500 characters per line
*  of textual information.
      ASTTSZ = MIN( ASTTSZ, GRP__SZNAM )

*  Now read an AST Object from the text in the GRP group.
*  ======================================================

*  Create an AST Channel through which the text stored in the group can be
*  read, and converted into an AST Object. The subroutine KPG1_SRCTA
*  extracts the text from the group, concatenates continuation lines, and
*  supplies the total line to the AST library. Textual information not
*  related to AST is skipped over without reporting errors.
      CHAN = AST_CHANNEL( KPG1_SRCTA, AST_NULL, 'SKIP=1', STATUS )

*  Initialise the index of the first element to be read from the group.
      ASTLN = 1

*  Read an Object from the Channel.
      IAST = AST_READ( CHAN, STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  Free the buffer.
      CALL PSX_FREE( IPBUF, STATUS )

*  Annul the Channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Delete the GRP group.
      CALL GRP_DELET( ASTGRP, STATUS )

      END
