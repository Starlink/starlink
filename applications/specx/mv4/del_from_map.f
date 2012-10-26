*  History:
*     20 Dec 1993 (hme):
*        Use RAM rather than NSPEC in WFILE, since it is now at the
*        start of the common block.
*     14 Aug 1994 (hme):
*        Review for file format 4.1, library mv4.
C-----------------------------------------------------------------------

      SUBROUTINE DELETE_FROM_MAP (IFAIL)

C   Routine to delete a nominated spectrum from a map file

      IMPLICIT NONE

C   Formal parameter:

      INTEGER*4 IFAIL             ! SPECX error return

C   Include files

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'CUBE'
      INCLUDE 'CNF_PAR'

C   Other variables

      INTEGER*4 I,J               ! Counters
      INTEGER*4 ISTAT             ! GEN_ routine status return
      INTEGER*4 ISCAN             ! Scan # to delete

      IFAIL = 0
      CALL GEN_GETI4 ('Spectrum # to delete', ISCAN, 'I4', ISCAN, ISTAT)

      CALL SEARCH_INDEX (%VAL(CNF_PVAL(INDEX_ADDRESS)), 
     :                   ISCAN, I, J, IFAIL)

      IF (IFAIL.EQ.0) THEN

C       Delete spectrum from file. This will do nothing unless it is the
C       last spectrum of the sequence that is being deleted. In that
C       case the POSN() array of NDFs will be altered.
C       It is important to do this before the updated index is written
C       to the file.

        CALL MV4_SPECDL( I, J )

C       Set relevant index element to absence signal.
C       Then write index to file.

        CALL XCOPY (4, -1000,
     :       %VAL(CNF_PVAL(INDEX_ADDRESS)+4*((J-1)*MSTEP + (I-1))))
        CALL MV4_INDXWR( %VAL(CNF_PVAL(INDEX_ADDRESS)) )

C       Update map header if we have deleted the last entry, otherwise
C       don't bother: the value of -1000 in INDEX will suffice to mark
C       that entry null.

        IF (ISCAN.EQ.NSPEC) THEN
          NSPEC = NSPEC - 1
          CALL MV4_HEADWR( )
        END IF

      ELSE
        WRITE( *, * ) 'No such spectrum in file'
      END IF

      RETURN
      END
