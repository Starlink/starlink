      SUBROUTINE MV4_SPECDL( I, J )
*+
*  Name:
*     MV4_SPECDL

*  Purpose:
*     Delete a spectrum in the file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_SPECDL( I, J )

*  Description:
*     This routine deletes the spectrum for position (I,J) from the map
*     file. Usually, this means that nothing is done. Only if the last
*     spectrum of the position array is deleted, is the length of the
*     position array reduced by 1.
*
*     That is to say that spectra are usually not really deleted.
*     Mostly, deletion of a spectrum mean only changing the index to
*     mark the spectrum as absent. That is not done by this routine.
*
*     The calling routine should first call this routine, then modify
*     the index to mark the spectrum as deleted. If the deleted spectrum
*     was actually the last, then the calling routine should finally
*     reduce NSPEC by 1 and write the updated map header to the file.

*  Arguments:
*     I, J = INTEGER (Given)
*        The position in INDEX to be copied.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     01 Sep 1994 (hme):
*        Take care that new size is not zero.
*        Also, cannot reduce length of array before the cell is empty.
*     16 Aug 2004 (timj):
*        Use CNF_PVAL
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
      INCLUDE 'MAPV4'

*  Arguments Given:
      INTEGER I, J

*.

*  Compare shape of position array with INDEX(I,J).
*  If they are equal, reshape the position array to be one element
*  shorter than so far.
*  We assume that the current shape is what is NSPEC in the memory copy
*  of the map header.
      CALL MV4_SPECD2( I, J, %VAL(CNF_PVAL(IDXPTR)) )

*  Return.
      END



      SUBROUTINE MV4_SPECD2( I, J, INDEX )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MAPV4'
      INCLUDE 'MAPHD'

      INTEGER I, J, K
      INTEGER INDEX( MSTEP, NSTEP )
      INTEGER DIMBNDS( 2 )
      INTEGER ONE( 2 )           ! what it says

      INTEGER STATUS

*  Local Data:
      DATA ONE / 1, 1 /

      IF ( INDEX(I,J) .EQ. NSPEC .AND. NSPEC .GT. 1 ) THEN
         STATUS = SAI__OK
         CALL ERR_MARK
* First unmap the POSN array
         CALL NDF_UNMAP( POSNDF, 'DATA', STATUS )

* Number of points is unchanged
         DIMBNDS(1) = NPTS1

* Number of spectra goes up by 1
         DIMBNDS(2) = NSPEC - 1

* Then change the bounds
         CALL NDF_SBND( 2, ONE, DIMBNDS, POSNDF, STATUS )
         CALL NDF_MAP( POSNDF, 'DATA', '_REAL', 'UPDATE',
     :      POSPTR, K, STATUS )



         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
      END IF

      END
