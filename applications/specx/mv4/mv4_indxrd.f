      SUBROUTINE MV4_INDXRD( MEMIDX )
*+
*  Name:
*     MV4_INDXRD

*  Purpose:
*     Copy index from file to memory.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_INDXRD( MEMIDX )

*  Description:
*     This routine copies the Specx map index from the map file to
*     memory. In the process bad values (VAL__BADI) are replaced with
*     -1000, which internally signals an absent spectrum.

*  Arguments:
*     MEMIDX( MSTEP * NSTEP ) = INTEGER (Returned)
*        The array as in virtual memory.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Arguments Returned:
      INTEGER MEMIDX( MSTEP * NSTEP )

*.

      CALL MV4_INDXR2( %VAL(IDXPTR), MEMIDX )

      END



      SUBROUTINE MV4_INDXR2( FILIDX, MEMIDX )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'MAPHD'

      INTEGER FILIDX( MSTEP * NSTEP )
      INTEGER MEMIDX( MSTEP * NSTEP )

      INTEGER I

      DO 1 I = 1, MSTEP * NSTEP
         IF ( FILIDX(I) .EQ. VAL__BADI ) THEN
            MEMIDX(I) = -1000
         ELSE
            MEMIDX(I) = FILIDX(I)
         END IF
 1    CONTINUE

      END
