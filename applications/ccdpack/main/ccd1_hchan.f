      SUBROUTINE CCD1_HCHAN( LOC, OPTIONS, CHAN, STATUS )
*+
*  Name:
*     CCD1_HCHAN

*  Purpose:
*     Create an AST channel.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_HCHAN( LOC, OPTIONS, CHAN, STATUS )

*  Description:
*     This routine creates an AST channel for use within CCDPACK using 
*     a given valid FIO file descriptor.  It is a wrapper for AST_CHANNEL 
*     whose purpose is to shield the calling routine from knowledge of 
*     the source and sink routines and global variables required to 
*     make this work.  The routine maps the data and sets up the common
*     block for later use by CCD1_CASRC and CCD1_CASNK.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for the component to be read from/written to.
*        It must be a 1-dimensional array of type _CHAR.
*     OPTIONS = CHARACTER * ( * ) (Given)
*        A character string containing an optional comma-separated list
*        of attribute assignments for passing to AST_CHANNEL.
*     CHAN = INTEGER (Returned)
*        AST pointer to a new channel reading from/writing to the HDS
*        locator referred to by LOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Restrictions:
*     As currently implemented, only one AST channel created by this 
*     routine may be in use at once.  No check is performed that the 
*     channel is not already in use.  It is probably not a good idea
*     to read and write on the same channel.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC ) LOC
      CHARACTER * ( * ) OPTIONS
      
*  Arguments Returned:
      INTEGER CHAN

*  Local variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CCD1_CASRC        ! Source routine for AST channel
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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

*  Map the component.
      CALL DAT_MAPV( LOC, '_CHAR', 'UPDATE', CCD1_CAPTR, CCD1_CANUM,
     :               STATUS )

*  Create the AST channel.
      CHAN = AST_CHANNEL( CCD1_CASRC, AST_NULL, OPTIONS, STATUS )

*  Initialise the position in the array.
      CCD1_CAPOS = 1

      END
* $Id$
