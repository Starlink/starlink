      SUBROUTINE MV4_SPECRD( I, J, INDEX, MEMCUB )
*+
*  Name:
*     MV4_SPECRD

*  Purpose:
*     Copy a spectrum from file to memory.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_SPECRD( I, J, INDEX, MEMCUB )

*  Description:
*     This routine copies the spectrum for position (I,J) from the
*     the map file to the data cube in memory. If INDEX(I,J)
*     indicates that the spectrum does not exist, then zeros will be
*     supplied instead. Any spectrum values that are bad (VAL__BADR) are
*     copied as BADPIX_VAL to the memory cube.

*  Arguments:
*     I, J = INTEGER (Given)
*        The position in INDEX to be copied.
*     INDEX( MSTEP, NSTEP ) = INTEGER (Given)
*        The index in memory (i.e. -1000 signal absent spectrum).
*     MEMCUB( NPTS1, MSTEP, NSTEP ) = REAL (Returned)
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Arguments Given:
      INTEGER I, J
      INTEGER INDEX( MSTEP, NSTEP )

*  Arguments Returned:
      REAL MEMCUB( NPTS1, MSTEP, NSTEP )

*  Local Variables:
      INTEGER STATUS             ! Starlink status
      INTEGER K                  ! Temporary integer

*.

*  If the spectrum is absent, supply zeros.
      IF ( INDEX(I,J) .EQ. -1000 ) THEN
         DO 1 K = 1, NPTS1
            MEMCUB(K,I,J) = 0.0
 1       CONTINUE

*  Else (spectrum exists), copy from file, replacing VAL__BADR.
      ELSE

*     Begin Starlink error context.
         STATUS = SAI__OK
         CALL ERR_MARK

* Data is already mapped with POSPTR and POSNDF

*     Copy the data into the (I,J)-th row of the cube.
*     Includes conversion between HDS and Specx bad values.
         IF ( STATUS .EQ. SAI__OK )
     :      CALL MV4_SPECR2( I, J, %VAL(POSPTR), MEMCUB, INDEX(I, J) )


*     Close Starlink error context.
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE

      END IF

*  Return
      END



      SUBROUTINE MV4_SPECR2( I, J, FILDAT, MEMCUB, IDXNUM )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'

      INTEGER I, J, IDXNUM
      REAL FILDAT( NPTS1, NSPEC )
      REAL MEMCUB( NPTS1, MSTEP, NSTEP )

      INTEGER K

      DO 2 K = 1, NPTS1
         IF ( FILDAT(K, IDXNUM) .EQ. VAL__BADR ) THEN
            MEMCUB(K,I,J) = BADPIX_VAL
         ELSE
            MEMCUB(K,I,J) = FILDAT(K, IDXNUM)
         END IF
 2    CONTINUE

      END
