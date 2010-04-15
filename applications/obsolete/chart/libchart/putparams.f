      SUBROUTINE PUTPARAMS(PARAMS,NVALS, STATUS )
*+
*   Subroutine PUTPARAMS
*
*   This is the converse of GETPARAMS, and must only be called
*   AFTER a call to GETPARAMS, because that subroutine opens the
*   file to which the records are to be written.
*
*   Gets
*   ----
*      PARAMS is an array of 25 elements, each of which contains 70
*             character records, one for each parameter.
*      NVALS  specifies the number of records to be written.
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*    Written by K F Hartley at RGO on 23-12-82
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*70 PARAMS(25)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (NVALS.GT.25) NVALS=25
      DO I=1,NVALS
         WRITE (1,REC=I,FMT='(A70)') PARAMS(I)
      END DO
      CLOSE (UNIT=1)
      END
