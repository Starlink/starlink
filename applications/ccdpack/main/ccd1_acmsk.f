      SUBROUTINE CCD1_ACMSK( PARAM, ID, ISARD, FILENM, STATUS )
*+
*  Name:
*     CCD1_ACMSK

*  Purpose:
*     Access a mask file (NDF or ARD) or an ARD expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ACMSK( PARAM, ID, ISARD, FILENM, STATUS )

*  Description:
*     The routine accesses an ARD expression via the parameter PARAM. If
*     this consists of only one element it assumes that this may be a
*     file and attempts to open an NDF of the same name. If this fails
*     an attempt to open a formatted file is made. If this fails the
*     routine assumes that the input is a valid ARD expression.  If a
*     successful open is achieved then the identifier to the NDF or an
*     ARD expression consisting of the ARD file contents is returned. If
*     the ARD expression does expand to an ARD file then FILENM is set
*     to the name, otherwise it is set to the name of the NDF or remains
*     blank if the input is an ARD expression.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of ADAM parameter to get user expression.
*     ID = INTEGER (Returned)
*        Either an NDF identifier or an ARD expression identifier.
*     ISARD = LOGICAL (Returned)
*        True if the user input converts to an ARD expression.
*     FILENM = CHARACTER * ( * ) (Returned)
*        If the input is the name of an ARD file or NDF then this argument
*        is set to  the file name, otherwise it is returned blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-SEP-1991 (PDRAPER):
*        Original version.
*     31-MAR-1992 (PDRAPER):
*        Added trap for unwanted messages from IRG_GIN.
*     5-SEP-1995 (PDRAPER):
*        Converted to use the official release of ARD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'MSG_PAR'          ! EMS constants.
      INCLUDE 'NDF_PAR'          ! NDF parameter
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'PAR_ERR'          ! Parameter system constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER ID
      LOGICAL ISARD
      CHARACTER * ( * ) FILENM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) INFILE ! Input file name
      CHARACTER * ( GRP__SZNAM ) BUFFER ! Input buffer
      CHARACTER * ( 1 ) INDIR    ! ARD indirection character
      INTEGER IAT                ! Position in string
      INTEGER MASKGR             ! group identifier for name string
      INTEGER NENT               ! Number of entries in group
      INTEGER FIOID              ! FIO file descriptor
      LOGICAL FLAG               ! GRPEX dummy
      LOGICAL MORE               ! True if a prompt is to be made
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defaults.
      ISARD = .FALSE.
      FILENM = ' '
      INFILE = ' '

*  Get a group of ARD expressions or a filename.
      CALL ERR_MARK
      MORE = .TRUE.
      NENT = 0
      MASKGR = GRP__NOID
 1    CONTINUE                  ! Start of DO WHILE loop
      IF ( STATUS .EQ. SAI__OK .AND. MORE ) THEN
         CALL PAR_GET0C( PARAM, BUFFER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            NENT = NENT + 1
            CALL ARD_GRPEX( BUFFER, GRP__NOID, MASKGR, FLAG, STATUS )

*  See if a continuation line is required.
            IF ( FLAG ) THEN
               CALL PAR_CANCL( PARAM, STATUS )
            ELSE
               MORE = .FALSE.
            END IF
         ELSE
            IF ( NENT .NE. 0 .AND. STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
            MORE = .FALSE.
         END IF
         GO TO 1
      END IF
      CALL ERR_RLSE

*  Retrieve first element as may need to see if this is a file (extract
*  now so that this string may be used as a context for any error messages).
      CALL GRP_GET( MASKGR, 1, 1, INFILE, STATUS )

*  If NENT is one then we probably have the name of a file. This could be
*  and NDF or an ASCII file containing an ARD description (we need this
*  functionality for compatibility).
      IF ( NENT .EQ. 1 .AND. STATUS .EQ. SAI__OK ) THEN

*  Only one element so may be a file name.
*  Set up an error context to stop messages which are used or might
*  confuse the user as to the actual intent of this routine.
         CALL ERR_MARK

*  Try to access the NDF. Failure returns a bad status and NDF__NOID.
         CALL NDF_FIND( DAT__ROOT, INFILE, ID, STATUS )

*  If have gotten the mask file as an NDF.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CCD1_GRDEL( MASKGR, STATUS )
            CALL NDF_MSG( 'NAME', ID )
            CALL MSG_LOAD( ' ', '^NAME', FILENM, IAT, STATUS )
         ELSE

*  Must either be an ARD expression or an ARD file.
            ISARD = .TRUE.

*  Cancel error from NDF.
            CALL ERR_ANNUL( STATUS )

*  Try to open a file for formatted access.
            CALL FIO_OPEN( INFILE, 'READ', 'LIST', 0, FIOID, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               FILENM = INFILE
               CALL FIO_CLOSE( FIOID, STATUS )

*  This is an ARD file so create a new group for this.
               CALL GRP_GETCC( MASKGR, 'INDIRECTION', INDIR, STATUS )
               CALL CCD1_GRDEL( MASKGR, STATUS )
               CALL CHR_PREFX( INDIR, INFILE, IAT )
               ID = GRP__NOID
               CALL ARD_GRPEX( INFILE, GRP__NOID, ID, FLAG, STATUS )
            ELSE

*  Assume single ARD expression.
               CALL ERR_ANNUL( STATUS )
               ID = MASKGR
            END IF
         END IF

*  Release the error context before issuing the required error message.
         CALL ERR_RLSE
      ELSE

*  Straight-forward ARD expression.
         ID = MASKGR
         ISARD = .TRUE.
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN

*  Issue an error message.
         CALL MSG_SETC( 'ONE', INFILE )
         CALL ERR_REP( 'CCD1_ACMSK',
     :'  Not a valid NDF name, ARD file or ARD expression (^ONE ...) ',
     :   STATUS)
      END IF

      END
* $Id$
