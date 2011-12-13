      SUBROUTINE CCD1_HCHAN( LOC, ACMODE, CHAN, STATUS )
*+
*  Name:
*     CCD1_HCHAN

*  Purpose:
*     Create an AST channel.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_HCHAN( LOC, ACMODE, CHAN, STATUS )

*  Description:
*     This routine creates an AST channel for use within CCDPACK using
*     a given HDS locator.  It is a wrapper for AST_CHANNEL
*     whose purpose is to shield the calling routine from knowledge of
*     the source and sink routines and global variables required to
*     make this work.  The routine maps the data and sets up the common
*     block for later use by CCD1_CASRC and CCD1_CASNK.
*
*     The routine should be entered with the locator LOC referring to
*     a CHAR_ component of any (reasonable) length and any (reasonable)
*     line length.  It should not be mapped on entry, but this routine
*     will map it.  After data has been written to it using AST_WRITE
*     (via CCD1_CASNK) the component will be mapped, and may have many
*     (up to half) blank, i.e.  space-filled, entries at the end of it,
*     which the calling routine may wish to truncate.  After the is
*     finished with, the locator should be annulled (which will unmap
*     the character array) and the Channel should be annulled.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for the component to be read from/written to.
*        It must be a 1-dimensional array of type _CHAR*N.  N and the
*        extent of the array can both be any small integer larger
*        than 1; lines are broken into several array elements and
*        the array extended if necessary.
*     ACMODE = CHARACTER * ( * ) (Given)
*        Access mode, 'READ' or 'WRITE' according to whether you want
*        to read from the Channel or write to it.
*     CHAN = INTEGER (Returned)
*        AST pointer to a new channel reading from/writing to the HDS
*        locator referred to by LOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Restrictions:
*     As currently implemented, only one AST channel created by this
*     routine may be in use at once.  No check is performed that the
*     channel is not already in use.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2001 (MBT):
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
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CCD1_PAR'         ! Local CCDPACK constants

*  Global Variables:
      INCLUDE 'CCD1_CACM'        ! CCD1_CALEN = INTEGER
                                 !    Length of elements in character array
                                 ! CCD1_CANUM = INTEGER
                                 !    Number of elements in character array
                                 ! CCD1_CAPOS = INTEGER
                                 !    Current position in array
                                 ! CCD1_CAPTR = INTEGER
                                 !    Pointer to character array
                                 ! CCD1_CALOC = CHARACTER * ( DAT__SZLOC )
                                 !    Locator for HDS component storing array

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC ) LOC
      CHARACTER * ( * ) ACMODE

*  Arguments Returned:
      INTEGER CHAN

*  Local variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_CASRC        ! Source routine for AST Channel
      EXTERNAL CCD1_CASNK        ! Sink routine for AST Channel

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the access mode.
      IF ( ACMODE .NE. 'READ' .AND. ACMODE .NE. 'WRITE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_HCHAN_BADMODE', 'CCD1_HCHAN: ' //
     :                 'Bad access mode specified (programming error)',
     :                 STATUS )
      END IF

*  Get the type and shape of the HDS component.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF ( TYPE( 1:6 ) .EQ. '_CHAR*' ) THEN
         CALL DAT_CLEN( LOC, CCD1_CALEN, STATUS )
      END IF

*  Validate shape.
      IF ( TYPE( 1:6 ) .NE. '_CHAR*' .OR. CCD1_CALEN .GT. CCD1__BLEN )
     :   THEN
         STATUS = SAI__OK
         CALL ERR_REP( 'CCD1_HCHAN_BADSHAPE', 'CCD1_HCHAN: '
     :              // 'HDS component unsuitable for AST storage',
     :                 STATUS )
      END IF

*  Store the locator.
      CCD1_CALOC = LOC

*  Map the component.
      CALL DAT_MAPV( LOC, '_CHAR', ACMODE, CCD1_CAPTR, CCD1_CANUM,
     :               STATUS )

*  Create the AST channel.
      IF ( ACMODE .EQ. 'READ' ) THEN
         CHAN = AST_CHANNEL( CCD1_CASRC, AST_NULL, ' ', STATUS )
      ELSE IF ( ACMODE .EQ. 'WRITE' ) THEN
         CHAN = AST_CHANNEL( AST_NULL, CCD1_CASNK, ' ', STATUS )
      END IF

*  Initialise the position in the array.
      CCD1_CAPOS = 1

      END
* $Id$
