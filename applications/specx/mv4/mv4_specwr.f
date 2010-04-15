      SUBROUTINE MV4_SPECWR( I, J, INDEX, MEMCUB )
*+
*  Name:
*     MV4_SPECWR

*  Purpose:
*     Copy a spectrum from memory to file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_SPECWR( I, J, INDEX, MEMCUB )

*  Description:
*     This routine copies the spectrum for position (I,J) from the data
*     cube in memory to the map file. If INDEX(I,J) indicates that the
*     spectrum does not exist, then this routine will do nothing. Any
*     spectrum values that are bad (BADPIX_VAL) are copied as bad values
*     (VAL__BADR).
*
*     This routine does not check whether the spectrum in the position
*     array was used previously. The calling routine must have searched
*     for a free position and updated INDEX accordingly before calling
*     this routine.
*
*     If the position in the array is one beyond the current length of
*     the position array, then the array size is altered to accommodate
*     one more spectrum.
*
*     It is important that the calling routine increments NSPEC _after_
*     calling this routine.

*  Arguments:
*     I, J = INTEGER (Given)
*        The position in INDEX to be copied.
*     INDEX( MSTEP, NSTEP ) = INTEGER (Given)
*        The index in memory (i.e. -1000 signal absent spectrum).
*     MEMCUB( NPTS1, MSTEP, NSTEP ) = REAL (Given)
*        The array as in virtual memory.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*    timj: Tim Jenness (JACH)
*     {enter_new_authors_here}

*  History:
*     12 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Forgot to allow writing last plus one spectrum.
*     10 Oct 1995 (timj):
*        Map format 4.2
*     21 Sep 2000 (ajc):
*        Unused TNDF, DATPTR
*     16 Aug 2004 (timj):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

*  Global Variables:
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Arguments Given:
      INTEGER I, J
      INTEGER INDEX( MSTEP, NSTEP )
      REAL MEMCUB( NPTS1, MSTEP, NSTEP )

*  Local Variables:
      INTEGER STATUS             ! Starlink status
      INTEGER K                  ! Temporary integer
*.

*  If the spectrum is exists.
      IF ( INDEX(I,J) .NE. -1000 ) THEN

*     Begin Starlink error context.
         STATUS = SAI__OK
         CALL ERR_MARK

*     If the spectrum has to be appended to the position array, alter
*     the length of the array and create the NDF in the new cell.
*     There is a catch, because the size of the array is actually
*     max(1,NSPEC).

       IF ( INDEX(I,J) .EQ. MAPUBND(2) + 1 .AND. NSPEC .GE. 1 ) THEN

* New format method
* First unmap the POSN array
            CALL NDF_UNMAP( POSNDF, 'DATA', STATUS )

* Number of spectra goes up by 100 (to prevent changing bounds every time!)
            MAPUBND(2) = MAPUBND(2) + 100

* Then change the bounds
            CALL NDF_SBND( 2, MAPLBND, MAPUBND, POSNDF, STATUS )



* ...and remap
            CALL NDF_MAP( POSNDF, 'DATA', '_REAL', 'UPDATE',
     :      POSPTR, K, STATUS )


         END IF

*     Map the data of the spectrum in question.


*     Copy the data from the (I,J)-th row of the cube.
*     Includes conversion between HDS and Specx bad values.
         IF ( STATUS .EQ. SAI__OK )
     :      CALL MV4_SPECW2( I, J, MEMCUB, %VAL(CNF_PVAL(POSPTR)),
     :        INDEX(I, J))



*     Close Starlink error context.
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE

      END IF

*  Return
      END



      SUBROUTINE MV4_SPECW2( I, J, MEMCUB, FILDAT, IDXNUM )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'

      INTEGER I, J, IDXNUM
      REAL MEMCUB( NPTS1, MSTEP, NSTEP )
      REAL FILDAT( NPTS1, IDXNUM )

      INTEGER K

      DO 2 K = 1, NPTS1
         IF ( MEMCUB(K,I,J) .EQ. BADPIX_VAL ) THEN
            FILDAT(K, IDXNUM) = VAL__BADR
         ELSE
            FILDAT(K, IDXNUM) = MEMCUB(K,I,J)
         END IF
 2    CONTINUE

      END
