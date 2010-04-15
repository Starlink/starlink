      SUBROUTINE MV4_INDXWR( MEMIDX )
*+
*  Name:
*     MV4_INDXWR

*  Purpose:
*     Copy index from memory to file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_INDXWR( MEMIDX )

*  Description:
*     This routine copies the Specx map index from memory to the map
*     file. In the process absence signals (-1000) are replaced with bad
*     values (VAL__BADI).

*  Arguments:
*     MEMIDX( MSTEP * NSTEP ) = INTEGER (Given)
*        The array as in virtual memory.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     16 Aug 2004 (timj):
*        Add CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Global Variables:
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Arguments Given:
      INTEGER MEMIDX( MSTEP * NSTEP )

*.

      CALL MV4_INDXW2( MEMIDX, %VAL(CNF_PVAL(IDXPTR)) )

      END



      SUBROUTINE MV4_INDXW2( MEMIDX, FILIDX )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'MAPHD'

      INTEGER FILIDX( MSTEP * NSTEP )
      INTEGER MEMIDX( MSTEP * NSTEP )

      INTEGER I

      DO 1 I = 1, MSTEP * NSTEP
         IF ( MEMIDX(I) .EQ. -1000 ) THEN
            FILIDX(I) = VAL__BADI
         ELSE
            FILIDX(I) = MEMIDX(I)
         END IF
 1    CONTINUE

      END
