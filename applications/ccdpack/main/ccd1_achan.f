      SUBROUTINE CCD1_ACHAN( FD, OPTIONS, CHAN, STATUS )
*+
*  Name:
*     CCD1_ACHAN

*  Purpose:
*     Create an AST channel.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_ACHAN( FD, OPTIONS, CHAN, STATUS )

*  Description:
*     This routine creates an AST channel for use within CCDPACK using
*     a given valid FIO file descriptor.  It is a wrapper for AST_CHANNEL
*     whose purpose is to shield the calling routine from knowledge of
*     the source and sink routines and global variables required to
*     make this work.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor of an open file to be used for the channel.
*     OPTIONS = CHARACTER * ( * ) (Given)
*        A character string containing an optional comma-separated list
*        of attribute assignments for passing to AST_CHANNEL.
*     CHAN = INTEGER (Returned)
*        AST pointer to a new channel reading/writing from the file
*        referred to by FD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Restrictions:
*     As currently implemented, only one AST channel created by this
*     routine may be in use at once.  No check is performed that the
*     channel is not already in use.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     05-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants

*  Global Variables:
      INCLUDE 'CCD1_FDCM'        ! File descriptor for AST channel CCD1_ASTFD

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) OPTIONS

*  Arguments Returned:
      INTEGER CHAN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_ASRC         ! Source routine for AST channel
      EXTERNAL CCD1_ASNK         ! Sink routine for AST channel

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set file descriptors in common block to correct values.
      CCD1_ASTFD = FD

*  Create the AST channel.
      CHAN = AST_CHANNEL( CCD1_ASRC, CCD1_ASNK, OPTIONS, STATUS )

      END
* $Id$
