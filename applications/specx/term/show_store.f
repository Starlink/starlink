*  History:
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     21 Sep 2000 (ajc):
*        Unused I
*-----------------------------------------------------------------------

      SUBROUTINE SHOW_STORE (ILOUT, IFAIL)

*  Routine to write out information about the spectra in the storage
*  registers (similar to DISPST for the data stack).

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER*4 ILOUT
      INTEGER*4 IFAIL

*     Include files:

      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STAKPAR'
      INCLUDE 'CNF_PAR'

*     Local variables:

      INTEGER*4   J
      INTEGER*4   IPTR
      INTEGER*4   ISTAT

*     Functions

      INTEGER*4   IGETVM
      INTEGER*4   IFREEVM

*  Ok, go...

*     Write a header to the output file

      WRITE (ILOUT,'(/'' Store posn''4X''Scan no''4X''Title'')')
      WRITE (ILOUT,'(1X,52(''-''))')

*     Save the current header

      ISTAT = IGETVM (LHEAD*4, .TRUE., 'SHOW_STORE', IPTR)
      CALL XCOPY  (LHEAD*4, SCAN_HEADER, %VAL(CNF_PVAL(IPTR)))

*     Recall the headers of each store position in turn

      DO J = 1,5
        CALL XCOPY (LHEAD*4, STORE(1,J), SCAN_HEADER)
        IF (LSCAN.GE.0)   THEN
          WRITE (ILOUT,'(4X,I2,9X,I4.1,6X,A26)') J, LSCAN, ITITLE
        END IF
      END DO

*     Restore the current header

      CALL XCOPY (LHEAD*4, %VAL(CNF_PVAL(IPTR)), SCAN_HEADER)
      ISTAT = IFREEVM (IPTR)

      RETURN
      END

*-----------------------------------------------------------------------
