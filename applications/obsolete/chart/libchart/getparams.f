      SUBROUTINE GETPARAMS(PARAMS,NVALS, STATUS )
*+
*   This subroutine opens a file assigned to logical name PAR
*   and reads a set of records from it into the character
*   array PARAMS, and returns the number of records found.
*
*   Gets
*   ----
*      PARAMS  25 x CHARACTER*70 Array into which Parameters
*              are read
*
*   Returns
*   -------
*      NVALS   Number of Records found.

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   Written by K F Hartley at RGO on 23-12-82

*   History:

*   11-DEC-1991: (PMA)
*      Changed the name of the file in the OPEN statement from PAR to
*      CHARTPAR.DAT. This is in fact the usual name of the file and
*      previously a logical name translated from PAR to CHARTPAR.DAT.
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statement, with call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the CARRIAGECONTROL specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     30-MAR-1993 (AJJB):
*        Changed the name of the file in the FILEOPEN call back from
*        CHARTPAR.DAT to PAR, as during porting of the code we found
*        that to allow the use of parameter files other than
*        CHARTPAR.DAT, on the Unix machines, we had to use a softlink,
*        PAR, pointing to the desired file, or to chartpar.dat as the
*        default.
*     22-APR-1993 (AJJB):
*        Removed defunct error handling code.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHT_ERR'          ! Chart error constants

      CHARACTER*70 PARAMS(25)
*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   This rather complicated OPEN statement is needed for the direct
*   access which allows the corresponding subroutine PUTPARAMS
*   to update the file without creating a new file.
*
*     OPEN (UNIT=1,FILE='PAR',ACCESS='DIRECT',STATUS='OLD',
*    :      CARRIAGECONTROL='LIST',RECL=70, FORM='FORMATTED')
*
* 22-MAR-1993 (AJJB): This OPEN statement has been replaced by the
* following call - see History section.

      CALL FILEOPEN( 1, 'PAR', 'OLD', 'DIRECT', 'FORMATTED',
     :              .TRUE., 70, .FALSE., STATUS )

      IF (STATUS .NE. SAI__OK) RETURN

      DO I=1,25
         READ (1,REC=I,FMT='(A70)') PARAMS(I)
      END DO

      NVALS=I-1

      END
