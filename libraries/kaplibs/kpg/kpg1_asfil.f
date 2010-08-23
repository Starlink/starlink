      SUBROUTINE KPG1_ASFIL( PARAM1, PARAM2, FRM, NP, IPOUT, FNAME,
     ;                       STATUS )
*+
*  Name:
*     KPG1_ASFIL

*  Purpose:
*     Reads spatial positions from a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFIL( PARAM1, PARAM2, FRM, NP, IPOUT, FNAME, STATUS )

*  Description:
*     This routine obtains formatted positions from a text file
*     specified by an environment parameter. The positions are assumed
*     to represent axis values in the supplied Frame.
*
*     The file should contain one position per line. Each position is
*     given by a set of strings delimited by comma, space or tab (the
*     first gives the value for Axis 1, the second for Axis 2, etc.). The
*     number of strings per line should equal the number of axes in the
*     supplied Frame.  The user can specify the columns to use using
*     Parameter PARAM2.
*
*     The file may contain blank lines, and comment lines commencing with
*     "!" or "#".

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The name of an environment parameter to use to get the file.
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The name of an environment parameter to use to get the indices of
*        the columns within the text file which are to be used. If blank,
*        the file must contain a column for every axis in FRM, all of which
*        are used in the order 1, 2, 3, etc.
*     FRM = INTEGER (Given)
*        A pointer to an AST Frame.
*     NP = INTEGER (Returned)
*        The number of positions read from the file.
*     IPOUT = INTEGER (Returned)
*        A pointer to an _DOUBLE array "COR( NP, * )" holding the obtained
*        co-ordinates. The second dimension of the array is equal to the
*        number of axes in the suppleid Frame. Should be released using
*        PSX_FREE when no longer needed.
*     FNAME = CHARACTER * ( * ) (Returned)
*        The file's name. Not accesed if the declared length of the
*        supplied string is 1 character.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 1999, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     17-SEP-1999 (DSB):
*        Added arguments PARAM2 and FNAME.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)
      INTEGER FRM

*  Arguments Returned:
      INTEGER NP
      INTEGER IPOUT
      CHARACTER FNAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*(GRP__SZNAM)! AST attribute name
      INTEGER FD                 ! FIO file descriptor for input file
      INTEGER IGRP               ! GRP identifier for group holding file contents
      INTEGER NAX                ! No. of axes in supplied Frame
      INTEGER NCHAR              ! Length of text read from file
*.

*  Initialise.
      NP = 0
      IPOUT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain the input text file, and save its name.
      CALL FIO_ASSOC( PARAM1, 'READ', 'LIST', 0, FD, STATUS )
      IF( LEN( FNAME ) .GT. 1 ) CALL FIO_FNAME( FD, FNAME, STATUS )

*  Create a GRP group to hold the file contents.
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Loop round until an error is encountered (this will happen when the end of
*  file is reached, if not before).
      DO WHILE ( STATUS .EQ. SAI__OK )

*  Read a record of the text file.
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  Remove leading spaces.
         CALL CHR_LDBLK( BUFFER )

*  Append the record to the end of the group if it is not a comment line.
         IF( BUFFER .NE. ' ' .AND. BUFFER( 1 : 1 ) .NE. '#' .AND.
     :                             BUFFER( 1 : 1 ) .NE. '!' ) THEN
            CALL GRP_PUT( IGRP, 1, BUFFER( : NCHAR ), 0, STATUS )

         END IF

      END DO

*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )

*  Get the number of positions in the file.
      CALL GRP_GRPSZ( IGRP, NP, STATUS )

*  Return a zero pointer if the file was empty.
      IF( NP .GT. 0 ) THEN

*  Get the number of axes in the supplied Frame.
         NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  Allocate memory to hold the returned co-ordinates.
         CALL PSX_CALLOC( NAX*NP, '_DOUBLE', IPOUT, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the positions from the group into the array allocated above.
         CALL KPG1_ASGRP( PARAM2, FRM, IGRP, NP, NAX,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    STATUS )

      END IF

 999  CONTINUE

*  Delete the GRP group.
      CALL GRP_DELET( IGRP, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occurrred, release the returned pointers.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL PSX_FREE( IPOUT, STATUS )
         IPOUT = 0
         NP = 0

*  Give a context message.
         CALL MSG_SETC( 'PARAM1', PARAM1 )
         CALL ERR_REP( 'KPG1_ASFIL_ERR3', 'Error obtaining a set of '//
     :                 'formatted positions using parameter %^PARAM1.',
     :                  STATUS )

      END IF

      END
