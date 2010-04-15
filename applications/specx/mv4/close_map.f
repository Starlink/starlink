      SUBROUTINE CLOSE_SPECX_MAP( IFAIL )
*+
*  Name:
*     CLOSE_SPECX_MAP

*  Purpose:
*     Close a new map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CLOSE_SPECX_MAP( IFAIL )

*  Description:
*     Routine to close the Specx map file.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     22 Nov 1993 (hme):
*        Replace LIB$FREE_LUN with FIO_PUNIT.
*     13 Dec 1993 (hme):
*        This routine may be called gratuitously, so any error report
*        from FIO_PUNIT should be supressed. Instead (and to avoid calls
*        to ERR_) we call FIO_PUNIT only if MAP_OPEN is currently true.
*     09 Jan 1994 (rp):
*        Replace FIO_ calls with IGET/IFREE
*     12 Aug 1994 (hme):
*        NDF-based map format, mv4 library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'FLAGCOMM'

*  Status:
      INTEGER IFAIL

*  Local Variables:
      INTEGER ISTAT

*  Internal References:
      INTEGER IFREEVM

*.

*  Reset status.
      IFAIL = 0

*  Release second (internal-only) cube.
      CALL RELEASE_NEW_CUBE( )

*  If external cube accessed.
      IF ( CUBE_IN_MEMORY .AND. NCUBE .NE. 0 ) THEN

*     Release index.
         ISTAT = IFREEVM( INDEX_ADDRESS )
         IF ( ISTAT .NE. 0 ) WRITE( *, * )
     :      ' -- close_specx_map -- error releasing VM for index'

*     Release cube.
         ISTAT = IFREEVM( CUBE_ADDRESS )
         IF ( ISTAT .NE. 0 ) WRITE( *, * )
     :      ' -- close_specx_map -- error releasing VM for cube'

*     Release inverted index.
         ISTAT = IFREEVM( INVINDEX_ADDRESS )
         IF ( ISTAT .NE. 0 ) WRITE( *, * )
     :      ' -- close_specx_map -- error releasing VM for inv.index'

*     Convert absence flag from -1000 to VAL__BADI.
*     Release index NDF.
*     Release file by annulling the only HDS locator.
         CALL MV4_MAPCLS( IFAIL )

*     Update some globals.
         NCUBE  = 0
         NINDEX = 0
         CUBE_IN_MEMORY = .FALSE.

      END IF

*  Flag map as closed.
      MAP_OPEN = .FALSE.

*  Return.
      END
