*-----------------------------------------------------------------------

      SUBROUTINE CONCATSP (IFAIL)

*   Routine to concatenate spectra in X and Y into one (quadrants not
*   merged however)

*   History:
*      6-JUN-2000 (AJC):
*        Replace 'TYPE *' with 'PRINT *'

      IMPLICIT    NONE

*     Formal parameters:

      INTEGER     IFAIL

*     Include files:

      INCLUDE    'SPECX_PARS'
      INCLUDE    'STACKCOMM'
      INCLUDE    'STAKPAR'


*     Local variables:

      INTEGER     NQUAD2
      INTEGER     NQTOT
      INTEGER     NDATTOT
      INTEGER     IOFF4, IOFFD

      REAL        VLSR2

*     Functions

      INTEGER     NTOT
      INTEGER     NTOT2

*  Ok, go...

      IFAIL = 0

*     Check that there are not too many quadrants for the final spectrum
*     Swap this spectrum with one in Y to get various header parameters:

      CALL XY
      NQUAD2 = NQUAD
      VLSR2  = VLSR
      CALL XY

*     Check VLSR the same

      IF (VLSR .ne. VLSR2) THEN
        PRINT *, 'Can''t CONCAT; different values of VLSR'
        PRINT *, '... Failing; will report unknown error.'
        IFAIL = 18
        GO TO 999
      END IF

      PRINT *, 'Number of quadrants in X-scan = ', NQUAD
      PRINT *, 'Number of quadrants in Y-scan = ', NQUAD2

*     New data size:

      NQTOT   = NQUAD + NQUAD2
      NDATTOT = NTOT(NQUAD) + NTOT2(NQUAD2)

      PRINT *, 'Total quadrants in final spectrum = ', NQTOT

      IF (NQTOT .gt. NQMAX) THEN
        IFAIL = 113
        RETURN
      END IF

*     Also check final data array not too long

      IF (NDATTOT .gt. LSTK - LHEAD) THEN
        IFAIL = 114
        RETURN
      END IF

*     Do the concatenation  ---  first the header info...

      IOFF4 = NQUAD2

      CALL XCOPY (4*NQUAD, TSYS,   TSYS   (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, JFREST, JFREST (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, JFCEN,  JFCEN  (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, JFINC,  JFINC  (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, ITREC,  ITREC  (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, ITSKY,  ITSKY  (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, ITTEL,  ITTEL  (LSTK + IOFF4 + 1))
      CALL XCOPY (4*NQUAD, NPTS,   NPTS   (LSTK + IOFF4 + 1))

      CALL XCOPY (8*NQUAD, LOFREQ, LOFREQ (LSTK/2 + IOFF4 + 1))
      CALL XCOPY (8*NQUAD, IFFREQ, IFFREQ (LSTK/2 + IOFF4 + 1))

*     ... then the data

      IOFFD = NTOT2 (NQUAD2)
      CALL XCOPY (4*NTOT(NQUAD), DATA, DATA(LSTK + IOFFD + 1))

*     Lose the original X spectrum

      CALL POP

      NQUAD = NQTOT

*     Don't need to sort quadrants --- done by MERGE anyway, and this
*     preserves the original order.

      RETURN

*     Standard error return

  999 CONTINUE
      RETURN

      END


*-----------------------------------------------------------------------
