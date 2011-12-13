      SUBROUTINE MAGTEST(STATUS)
*+
*  Name:
*     magtest

*  Purpose:
*     ADAM test of MAG and MIO

*  Description:
*     A test of the MAG and MIO packages.
*     The program allocates and mounts the tape drive defined
*     by %TAPE, then repeats it to check on the `already done'
*     status.
*     The program then opens the tape with MAG_ASSOC.
*     It then writes 2 files of 5 records each to the tape,
*     terminating the tape after the second file.
*     The records are 80 bytes in length and contain the values
*     1-80, 2-81 etc. in the first file and 6-86, 7-87 etc. in
*     the second file.
*     The tape is then rewound and the second records of each
*     file are read and the first 10 bytes printed.

*  ADAM Parameters
*     TAPE = TAPEDRIVE(WRITE)
*       name of the tape drive to be used

*  Method:
*     The program uses the following MAG_ routines:
*     MAG_ALOC
*     MAG_MOUNT
*     MAG_ASSOC
*     MAG_WRITE
*     MAG_WTM
*     MAG_REW
*     MAG_MOVE
*     MAG_READ
*     MAG_DISM
*     MAG_ANNUL
*     MAG_DEAL

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council.
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
*     AJC: Alan Chipperfield

*  History:
*     05-JUN-1986 (AJC):
*        Original.

*-
*    Global constants:
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'MAG_ERR'

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*    Status
      INTEGER STATUS
*    Local variables
      INTEGER TP              !Tape index
      INTEGER IBYTE           !Byte number within record
      INTEGER IREC            !Record number absolute
      INTEGER JREC            !Record number within file
      INTEGER IFILE           !File number
      INTEGER NWRIT           !Number of bytes actually in record
      BYTE OBUFF(65536)       !Output buffer
*.
      PRINT *,'STARTED'
*    Allocate and mount
      CALL MAG_ALOC('TAPE', STATUS)
      CALL STATPRINT('ALOC', STATUS)
      CALL MAG_MOUNT('TAPE', 'UPDATE', STATUS)
      CALL STATPRINT('MOUNT', STATUS)

*    Repeat allocate and mount
      CALL MAG_ALOC('TAPE', STATUS)
      CALL STATPRINT('ALOC', STATUS)
      IF (STATUS .EQ. MAG__DVALL) STATUS = SAI__OK

      CALL MAG_MOUNT('TAPE', 'UPDATE', STATUS)
      CALL STATPRINT('MOUNT', STATUS)
!      IF (STATUS .EQ. MAG__DVMNT) THEN
!         STATUS = SAI__OK
!      ELSE
!         GOTO 999
!      ENDIF

*    Associate tape drive
      CALL MAG_ASSOC('TAPE', 'UPDATE', TP, STATUS)
      CALL STATPRINT( 'ASSOC', STATUS)
      IF (STATUS.EQ.SAI__OK) THEN
         IREC = 1
         DO IFILE = 1, 2
            JREC = 1
            DOWHILE ((JREC .LE. 5) .AND. (STATUS .EQ. SAI__OK))
*             Set up byte array buffer
               IBYTE = 1
               DO IBYTE = 1, 80
                  OBUFF(IBYTE) = IBYTE + IREC -1
               ENDDO
*             Write a record
               CALL MAG_WRITE(TP, 80, OBUFF, NWRIT, STATUS)
               IREC = IREC + 1
               JREC = JREC + 1
            ENDDO
            CALL STATPRINT( 'FILE WRITE', STATUS)
*
*          Write a tape mark
            CALL MAG_WTM(TP,STATUS)
            CALL STATPRINT( 'WTM', STATUS)
         ENDDO
*
*       Write a tape mark to terminate tape
         CALL MAG_WTM(TP,STATUS)
         CALL STATPRINT( 'WTM', STATUS)
*
*       Now set up for reading the blocks just written.
*       First:
*       Find 2nd block of 1st file. This will involve rewind
         CALL MAG_MOVE( TP, 1, .TRUE., 2, STATUS)
         CALL STATPRINT( 'MOVE', STATUS)
*       Read and print record
         CALL READPRINT( TP, 2 ,STATUS)
*
*       Move to 4th record of 2nd file. (Reference from end of file)
         CALL MAG_MOVE( TP, 2, .FALSE., 2, STATUS)
         CALL STATPRINT( 'MOVE', STATUS)
*       Read and print record
         CALL READPRINT( TP, 10 ,STATUS)
*
*       Now cause End-of Volume action
         CALL MAG_MOVE( TP, 4, .TRUE., 1, STATUS)
*       Check that End-of-Volume was detected
         IF (STATUS .EQ. MAG__EOV) THEN
            CALL ERR_ANNUL( STATUS )
         ELSEIF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
         ENDIF
         CALL STATPRINT( 'MOVE (TO EOV)', STATUS)

*       reset status
         STATUS = SAI__OK
*
*       Now rewind tape
         CALL MAG_REW( TP, STATUS )
         CALL STATPRINT( 'REW', STATUS)
*
*       Now write extra-long block
         NWRIT = 0
         OBUFF(65535) = 127
         CALL MAG_WRITE( TP, 65535, OBUFF, NWRIT, STATUS )
         IF ( (STATUS .EQ. SAI__OK) .AND. (NWRIT .NE. 65535) )THEN
            STATUS = SAI__ERROR
         ENDIF
         CALL MSG_SETI('NWRIT', NWRIT)
         CALL STATPRINT( 'WRITING EXTRA-LONG BLOCK - NWRIT is ^NWRIT',
     :    STATUS)
         IF (STATUS .NE. SAI__OK) GO TO 999

*
*       Now rewind tape
         CALL MAG_REW( TP, STATUS )
         CALL STATPRINT( 'REW', STATUS)
*
*       Now read the extra-long block
         NWRIT = 0
         OBUFF(65536) = 0
         CALL MAG_READ( TP, 65535, OBUFF, NWRIT, STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            IF ( (NWRIT .NE. 65535)
     :      .OR.  (OBUFF(65535) .NE. 127) )THEN
*           Something wrong with the values returned - print them
               CALL MSG_SETI('NWRIT', NWRIT)
               CALL MSG_SETI('ENDVAL', NUM_BTOI(OBUFF(65535)) )
               CALL MSG_OUT(' ',
     :         'NWRIT is ^NWRIT (65535); Last byte is ^ENDVAL (255)',
     :          STATUS )
*           and set error status
               STATUS = SAI__ERROR
            ENDIF
         ENDIF

         CALL STATPRINT( 'READING EXTRA-LONG BLOCK', STATUS)
         IF (STATUS .NE. SAI__OK) GO TO 999
*
*       Now rewind tape
         CALL MAG_REW( TP, STATUS )
         CALL STATPRINT( 'REW', STATUS)
         IF (STATUS .NE. SAI__OK) GO TO 999
*
*       Now dismount - expect failure (is active)
*
         CALL MAG_DISM( 'TAPE', .FALSE., STATUS)
         CALL STATPRINT( 'DISMOUNT when active', STATUS)
         IF (STATUS .EQ. MAG__ISACT) STATUS = SAI__OK
         IF (STATUS .NE. SAI__OK) GO TO 999

*       Annul the tape descriptor
         CALL MAG_ANNUL( TP, STATUS )
*
*       Try an action to check for failure
         CALL MAG_REW( TP, STATUS )
         CALL STATPRINT( 'REW when not active', STATUS)
         IF (STATUS .EQ. MAG__NTOPN) STATUS = SAI__OK
         IF (STATUS .NE. SAI__OK) GO TO 999

*       Try DISMOUNT again
         CALL MAG_DISM( 'TAPE', .FALSE., STATUS)
         CALL STATPRINT( 'DISMOUNT', STATUS)
         IF (STATUS .NE. SAI__OK) GO TO 999

*       and again - expect error this time
         CALL MAG_DISM( 'TAPE', .FALSE., STATUS)
         CALL STATPRINT( 'DISMOUNT when dismounted', STATUS )
         IF (STATUS .EQ. MAG__DVNMT) STATUS = SAI__OK
         IF (STATUS .NE. SAI__OK) GO TO 999

      ENDIF

999   CONTINUE
*   Check the final error status
      IF (STATUS .EQ.SAI__OK) THEN
         CALL MSG_OUT('OK', 'MAGTEST SUCCESSFUL', STATUS)
      ELSE
         CALL ERR_OUT('FAIL', 'MAGTEST FAILURE', STATUS)
      ENDIF

      END

      SUBROUTINE READPRINT(TP, IVAL, STATUS)
*    Subroutine to read and print a record. Also compares the
*    first byte with the value given as a parameter.
      INCLUDE 'SAE_PAR'
*    Status
      INTEGER STATUS
*    Input
      INTEGER TP        !Tape pointer
      INTEGER IVAL      !First byte value
*    Local variables
      BYTE IBUFF(80)    !Input buffer
      INTEGER NWRIT     !Actual size of block read
      INTEGER WRSTAT    !Status for MSG_OUT
      CHARACTER*80 LINE !Internal file
*
*    Read record
      CALL MAG_READ(TP, 80, IBUFF, NWRIT, STATUS)
      CALL STATPRINT( 'READ', STATUS)
*    Print record
      IF (STATUS.EQ.SAI__OK) THEN
*       Print the record
         CALL MSG_SETI('NWRIT',NWRIT)
         CALL MSG_OUT(' ', 'Record read is ^NWRIT bytes Starting:',
     :   STATUS)
         WRITE (LINE,10) (IBUFF(I),I=1,10)
10       FORMAT(10I4)
         CALL MSG_OUT(' ', LINE, STATUS)
      ENDIF
*    Check record
      IBYTE = IBUFF(1)
      IF (IBYTE .NE. IVAL) THEN
         CALL MSG_OUT( ' ', 'INCORRECT BLOCK CONTENTS', STATUS)
      ENDIF
      END


      SUBROUTINE STATPRINT (KEY, STATUS)
*    To print sensible messages depending on key and status
*    Global constants
      INCLUDE 'SAE_PAR'
*    Status
      INTEGER STATUS
*    Input
      CHARACTER*(*) KEY
*    Local variables
      INTEGER WRSTAT

      CALL MSG_SETC('KEY', KEY)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL MSG_OUT(' ', '^KEY: OK', STATUS)
      ELSE
         WRSTAT = STATUS
         CALL ERR_OUT(' ', '^KEY: ^STATUS', STATUS)
         STATUS = WRSTAT
      ENDIF
      END
