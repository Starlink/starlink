      SUBROUTINE FIOTEST(STATUS)
*+
*  Name:
*    fiotest

*  Purpose:
*    Task to test the FIO package

*  Description:
*    A fairly thorough test of the FIO package is performed.
*    Messages chart the progress - some deliberate errors are introduced
*    but the terminating message will say whether or not the test was
*    successful overall.
*    Two files CREATEFILE.DAT and NEWFILE.DAT are created and should be
*    listed after the run as a final check on the carriage control
*    settings.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     A.Chipperfield (RLVAD::AJC)

*  History:
*    18-FEB-1988 (AJC):
*        Include environment layer
*    28-JUN-1988 (AJC):
**       Add FORM argument and test unformatted (RLVAD::AJC)
*     3-APR-1992 (PMA):
*        Change ERR_OUT to ERR_REP.
*        Change SAI_PAR to sae_par.
*        Change the names of INCLUDE files to lower case.
*     8-NOV-2000 (AJC):
*        Correct args for MSG_SETx
*        Cosmetic changes

*-
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'FIO_PAR'

      INTEGER STATUS
      INTEGER FDN,FDC
      INTEGER UNIT, RECSZ
      CHARACTER*50 BUFF3,BUFF4
      CHARACTER*(FIO__SZFNM) FILE
      CHARACTER*12 FORM
      DATA BUFF3/'123456789012345'/, BUFF4/' '/

*    Ensure directory is clean to start
      DOWHILE (STATUS .EQ. SAI__OK)
        CALL FIO_ERASE( 'CREATEFILE.DAT', STATUS )
      ENDDO
      IF ( .NOT. ( (STATUS .EQ. FIO__FILNF)
     :         .OR. (STATUS .EQ. FIO__CFOLF) ) ) GOTO 100

      CALL ERR_ANNUL( STATUS )

      DOWHILE (STATUS .EQ. SAI__OK)
        CALL FIO_ERASE( 'NEWFILE.DAT', STATUS )
      ENDDO
      IF ( .NOT. ( (STATUS .EQ. FIO__FILNF)
     :         .OR. (STATUS .EQ. FIO__CFOLF) ) ) GOTO 100

      CALL ERR_ANNUL( STATUS )

*    Create and open for writing a file called NEWFILE.DAT
*    This should force activation of FIO
      CALL FIO_ASSOC( 'NEWFILE', 'WRITE', 'LIST', 0, FDN, STATUS )
      CALL STATWRITE( 'OPENING NEWFILE.DAT TO WRITE:', STATUS )
*    Now check the record size of the created file
*    Also checks FIO_UNIT
      CALL FIO_UNIT( FDN, UNIT, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         INQUIRE(UNIT, RECL=RECSZ)
!         IF (RECSZ .EQ. 133) THEN
            CALL MSG_SETI( 'RECSZ', RECSZ )
            CALL MSG_OUT( ' ', 'File record length is ^RECSZ', STATUS )
!         ELSE
!            CALL MSG_OUT( ' ', 'File record length is NOT 133', STATUS )
!            STATUS = FIO__INCRC
!            CALL STATWRITE( 'WRONG RECORD LENGTH SET:', STATUS )
!         ENDIF
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Create and open for writing the CREATED file. Record size specified
      CALL FIO_ASSOC(
     :  'CREATEFILE', 'WRITE', 'LIST', 10, FDC, STATUS )
      CALL STATWRITE( 'OPENING CREATEFILE.DAT TO WRITE:', STATUS )
*    Now check the record size of the created file
*    Also checks FIO_UNIT
      CALL FIO_UNIT( FDC, UNIT, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL MSG_SETI( 'FD', FDC )
         CALL MSG_SETI( 'UNIT', UNIT )
         CALL MSG_OUT( ' ', 'Second file opened FD= ^FD,  UNIT= ^UNIT',
     :                STATUS)
         IF((FDC .NE. 2) .OR. (UNIT .NE. 42)) THEN
            STATUS = SAI__ERROR
            CALL STATWRITE(
     :        'FILE DESCRIPTOR OR UNIT NUMBER ALLOCATION:', STATUS )
*       Skip out as the above section failed
            GOTO 100
         ENDIF

         INQUIRE( UNIT, RECL=RECSZ )
!         IF (RECSZ .EQ. 10) THEN
            CALL MSG_SETI( 'RECSZ', RECSZ )
            CALL MSG_OUT( ' ', 'File record length is ^RECSZ', STATUS )
!         ELSE
!            CALL MSG_OUT( ' ', 'File record length is NOT 10', STATUS )
!            STATUS = FIO__INCRC
!            CALL STATWRITE( 'WRONG RECORD LENGTH SET:' ,STATUS )
!         ENDIF
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now write 5 records to both
      CALL RECWRITE( FDN, FDC, 'WR1', 5, STATUS )
      CALL STATWRITE( 'WRITING RECORDS:', STATUS )

*    Now close both
      CALL FIO_CLOSE( FDN, STATUS )
      CALL FIO_CLOSE( FDC, STATUS )
      CALL STATWRITE( 'CLOSING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now open for reading
*    Use invalid recsz to check no effect
      CALL FIO_ASSOC( 'NEWFILE', 'READ', 'none', 10, FDN, STATUS )
*    Require ACMODE conversion to upper case
      CALL FIO_ASSOC( 'CREATEFILE', 'Read', 'none', 0, FDC, STATUS )
      CALL STATWRITE( 'OPENING TO READ:', STATUS )

*    Force error by attempting to write
*    Also checks ACMODE was converted to upper case
      CALL FIO_WRITE( FDN, BUFF3, STATUS )
      CALL STATWRITE( 'WRITING TO ''READ'' FILE:', STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Unexpected success = failure', STATUS )
      ELSEIF (STATUS .EQ. FIO__ILLAC) THEN
         CALL ERR_FLUSH( STATUS )
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Compare the records
      CALL COMPARE( FDN, FDC, 5, STATUS )
      CALL STATWRITE( 'COMPARING:', STATUS )
*    and close
      CALL FIO_CLOSE( FDN, STATUS )
      CALL FIO_CLOSE( FDC, STATUS )
      CALL STATWRITE( 'CLOSING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now open to update
*    Also checks that record size and FORM do not matter here.
      CALL FIO_ASSOC( 'NEWFILE', 'UPDATE', 'LIST', 10, FDN, STATUS )
      CALL FIO_ASSOC(
     :  'CREATEFILE', 'UPDATE', 'LIST', 0, FDC, STATUS )
      CALL STATWRITE( 'OPENING FOR UPDATE:', STATUS )
*    Write 2 records
      CALL RECWRITE( FDN, FDC, 'UP2', 2, STATUS )
      CALL STATWRITE( 'WRITING 2 RECORDS:', STATUS )
*    and close
      CALL FIO_CLOSE( FDN, STATUS )
      CALL FIO_CLOSE( FDC, STATUS )
      CALL STATWRITE( 'CLOSING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now open to append
*    Recsz 10 doesn't matter here
      CALL FIO_ASSOC(
     :   'NEWFILE', 'APPEND', 'LIST', 10, FDN, STATUS )
*    Try a faulty call - illegal access mode
      CALL FIO_ASSOC( 'CREATEFILE', 'XXXX', 'none', 0, FDC, STATUS )
      CALL STATWRITE( 'OPEN WITH ILLEGAL ACMODE:', STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Unexpected success = failure', STATUS )
      ELSEIF (STATUS .EQ. FIO__IVACM) THEN
         CALL ERR_FLUSH( STATUS )
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Try again with default record size. Should prompt as parameter
*    cancelled in FIO_ASSOC upon error.
      CALL MSG_OUT( ' ', '**********************************', STATUS )
      CALL MSG_OUT( ' ', 'EXPECT A PROMPT HERE - TYPE RETURN', STATUS )
      CALL MSG_OUT( ' ', '**********************************', STATUS )
      CALL FIO_ASSOC(
     :   'CREATEFILE', 'APPEND', 'LIST', 0, FDC, STATUS )
      CALL STATWRITE( 'OPENING TO APPEND:', STATUS )

*    Write 3 more records
      CALL RECWRITE( FDN, FDC, 'AP2', 3, STATUS )
      CALL STATWRITE( 'WRITING 3 MORE RECORDS:', STATUS )
*    and close
      CALL FIO_CLOSE( FDN, STATUS )
      CALL FIO_CLOSE( FDC, STATUS )
      CALL STATWRITE( 'CLOSING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now re-open for reading
      CALL FIO_ASSOC( 'NEWFILE', 'READ', 'none', 10, FDN, STATUS )
      CALL FIO_ASSOC( 'CREATEFILE', 'READ', 'none', 10, FDC, STATUS )
      CALL STATWRITE( 'OPENING FOR READ:', STATUS )

*    Compare the two files
      CALL COMPARE( FDN, FDC, 5, STATUS )
      CALL STATWRITE( 'COMPARING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    CANCL and erase NEWFILE
*    Also checks FIO_FNAME
      CALL FIO_FNAME( FDN, FILE, STATUS )
      CALL FIO_CANCL( 'NEWFILE', STATUS )
      CALL STATWRITE( 'CANCELLING NEWFILE:', STATUS )
      CALL FIO_ERASE( FILE, STATUS )
      CALL STATWRITE( 'ERASING NEWFILE:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Force an error by deleting the file just deleted
      CALL FIO_ERASE( FILE, STATUS )
      CALL STATWRITE( 'ERASING NON-EXISTENT FILE:', STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Unexpected success = failure', STATUS )
      ELSEIF ( (STATUS .EQ. FIO__FILNF)
     :    .OR. (STATUS .EQ. FIO__CFOLF) ) THEN
         CALL ERR_FLUSH( STATUS )
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now OPEN 'APPEND' to re-create NEWFILE with FORM 'LIST to check
*    it is created and with carriagecontrol 'LIST'
*    Should prompt as parameter cancelled.
      CALL MSG_OUT( ' ', '**********************************', STATUS )
      CALL MSG_OUT( ' ', 'EXPECT A PROMPT HERE - TYPE RETURN', STATUS )
      CALL MSG_OUT( ' ', '**********************************', STATUS )
      CALL FIO_ASSOC( 'NEWFILE', 'APPEND', 'LIST', 10, FDN, STATUS )
      CALL STATWRITE( 'RE-CREATE NEWFILE USING APPEND:', STATUS )
*    Write three 10 byte records
      CALL FIO_WRITE( FDN, BUFF3(1:10), STATUS )
      CALL FIO_WRITE( FDN, BUFF3(1:10), STATUS )
      CALL FIO_WRITE( FDN, BUFF3(1:10), STATUS )
      CALL STATWRITE( 'WRITING 3 RECORDS TO NEWFILE:', STATUS )
*    And close it again
      CALL FIO_CLOSE( FDN, STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    CREATEFILE still exists (record size 10) use it to check update
*    CLOSE and OPEN it with a larger record size.
      CALL FIO_CLOSE( FDC, STATUS )
      CALL FIO_ASSOC(
     :  'CREATEFILE', 'UPDATE', 'FORTRAN', 20, FDC, STATUS )
*    Now check the record size of the created file
*    Also checks FIO_UNIT
      CALL FIO_UNIT( FDC, UNIT, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         INQUIRE( UNIT, RECL=RECSZ )
!         IF (RECSZ .EQ. 20) THEN
            CALL MSG_SETI( 'RECSZ', RECSZ )
            CALL MSG_OUT( ' ', 'File record length is ^RECSZ', STATUS)
!         ELSE
!            CALL MSG_OUT(' ', 'File record length is NOT 20', STATUS)
!            STATUS = FIO__INCRC
!            CALL STATWRITE( 'WRONG RECORD LENGTH SET:', STATUS)
!         ENDIF
      ENDIF
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Write a 20 byte record with trailing spaces
      CALL FIO_WRITE( FDC, BUFF3(1:20), STATUS )
      CALL STATWRITE( 'WRITING RECORD, LENGTH 20:', STATUS )

*    Unless RECL is 0 (assume this means platform doesn't check),
*    write 50 byte record (TOO BIG) Expect FIO__OUTOV
      IF ( RECSZ .NE. 0 ) THEN
         CALL FIO_WRITE( FDC, FILE(1:50), STATUS )
         CALL STATWRITE( 'WRITING RECORD, LENGTH 50:', STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
C    Not an error for gfortran, so push on.
            CALL MSG_OUT( ' ', 'Unexpected success = failure', STATUS )
         ELSEIF (STATUS .EQ. FIO__OUTOV) THEN
            CALL ERR_FLUSH( STATUS )
         ENDIF
      ENDIF

*    and close
      CALL FIO_CLOSE( FDC, STATUS )
      CALL STATWRITE( 'CLOSING:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Re-open for reading
      CALL FIO_ASSOC( 'CREATEFILE', 'READ', 'none', 0, FDC, STATUS )
      CALL STATWRITE( 'OPENING FOR READ:', STATUS )
*    Read and check the input
      CALL FIO_READ( FDC, BUFF4, RECSZ, STATUS )
      CALL STATWRITE( 'READING:', STATUS )
      CALL MSG_SETI( 'RECSZ', RECSZ )
      CALL MSG_OUT( ' ', 'Record size is ^RECSZ', STATUS )
      IF (RECSZ .NE. 15) THEN
         STATUS = SAI__ERROR
      ENDIF

      CALL MSG_OUT( ' ', BUFF4, STATUS )
      CALL STATWRITE( 'CHECKING DATA:', STATUS )
*    Skip out if the above section failed
      IF ( STATUS .NE. SAI__OK ) GOTO 100

*    Now check out opening UNFORMATTED
      CALL FIO_ASSOC(
     :   'NEWFILE', 'READ', 'UNFORMATTED', 10, FDN, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         CALL FIO_UNIT( FDN, UNIT, STATUS )
         INQUIRE( UNIT=UNIT, FORM=FORM )
         IF (FORM .NE. 'UNFORMATTED') THEN
            CALL MSG_SETC( 'FORM', FORM )
            CALL MSG_OUT( ' ', 'NEWFILE FORM IS ^FORM', STATUS )
            STATUS = FIO__OPNER
         ENDIF
      ENDIF
      CALL FIO_CLOSE( FDN, STATUS )
      CALL STATWRITE( 'OPEN NEWFILE UNFORMATTED TO READ:', STATUS )

100   CONTINUE
*    Deactivate FIO package
      CALL FIO_DEACT( STATUS )
      CALL STATWRITE( 'DEACTIVATING FIO:', STATUS )
*    Skip out if the above section failed
      IF (STATUS .EQ. SAI__OK) THEN

*       Check this has closed file
*       Write a 20 byte record expecting an error
         CALL FIO_WRITE( FDC, BUFF3(1:20), STATUS )
         CALL STATWRITE( 'WRITING TO CLOSED FILE:', STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unexpected success = failure', STATUS )
         ELSEIF (STATUS .EQ. FIO__NTOPN) THEN
            CALL ERR_FLUSH( STATUS )
         ENDIF
      ENDIF

*    check that everything that should have worked did
      IF (STATUS.NE.SAI__OK)THEN
         CALL ERR_REP( ' ', ' ', STATUS )
         CALL ERR_REP( ' ', 'FIO PACKAGE TEST FAILS', STATUS )
      ELSE
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_OUT( ' ', 'FIO PACKAGE TEST SUCCESSFUL', STATUS)
         CALL MSG_OUT( ' ', 'CREATEFILE.DAT should exist',STATUS)
         CALL MSG_OUT( ' ', 'and contain 123456789012345',STATUS)
         CALL MSG_OUT(
     :     ' ', 'with the 1 being taken as carriage control', STATUS)
         CALL MSG_OUT( ' ', 'NEWFILE.DAT should exist', STATUS )
         CALL MSG_OUT( ' ', 'and contain 3 records 1234567890', STATUS )
         CALL MSG_OUT( ' ', 'with the single spacing and the 1 printed',
     :     STATUS)
      ENDIF
      END

      SUBROUTINE RECWRITE( FDN, FDC, RECHD, N, STATUS)
*    Write a record number into files with descriptors
*    FDN and FDC using FIO_WRITE
      INCLUDE 'SAE_PAR'
      INTEGER FDN,FDC,N,STATUS
      CHARACTER*(*) RECHD
      INTEGER I
      CHARACTER*10 BUFF1

      IF (STATUS.NE.SAI__OK) RETURN
      I=1
      DOWHILE ((STATUS.EQ.SAI__OK) .AND. (I.LE.N))
         WRITE(BUFF1,10) RECHD,I
10       FORMAT(' ',A3,I3)
         CALL FIO_WRITE( FDN, BUFF1, STATUS )
         CALL FIO_WRITE( FDC, BUFF1, STATUS )
         I=I+1
      ENDDO
      END

      SUBROUTINE COMPARE( FDN, FDC, NREC, STATUS )
*    Read NREC records from the files with descriptors FDN and FDC
*    using FIO_READ for the FDN file and Fortran for the FDC file.
*    First get the LUN of FDC
      INCLUDE 'SAE_PAR'
      INTEGER FDN,FDC,NREC,STATUS
      INTEGER I,LUN,NCHAR
      CHARACTER*10 BUFF1,BUFF2

      IF (STATUS.NE.SAI__OK) RETURN
      CALL FIO_UNIT( FDC, LUN, STATUS )
      I=1
      DOWHILE ((STATUS.EQ.SAI__OK) .AND. (I.LE.NREC))
         CALL FIO_READ( FDN, BUFF1, NCHAR, STATUS )
         READ(LUN,20) BUFF2
20       FORMAT(A10)
         CALL MSG_SETC( 'BUFF1', BUFF1(1:NCHAR) )
*       Compare the results
         IF(BUFF1.NE.BUFF2) THEN
            CALL MSG_SETC( 'BUFF2', BUFF2(1:NCHAR) )
            CALL MSG_OUT( ' ', 'BUFFERS:^BUFF1:^BUFF2', STATUS )
            STATUS=SAI__ERROR
         ELSE
            CALL MSG_OUT( ' ', 'BUFFER:^BUFF1', STATUS )
         ENDIF
         I=I+1
      ENDDO
      END

      SUBROUTINE STATWRITE( KEY, STATUS )
*    Display the key and whether or not status is OK
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      CHARACTER*(*) KEY
      INTEGER TSTAT

      CALL MSG_SETC( 'KEY', KEY )
      IF (STATUS .EQ. SAI__OK) THEN
          CALL MSG_OUT( ' ', '^KEY STATUS OK', STATUS )
      ELSE
          TSTAT=STATUS
          CALL ERR_FACER( 'STAT', STATUS )
          CALL ERR_REP(' ', '^KEY ^STAT', STATUS )
          STATUS=TSTAT
      ENDIF
      END
