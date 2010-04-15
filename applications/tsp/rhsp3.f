C+
      SUBROUTINE RHSP3(STATUS)
C
C            R H S P 3
C
C     Command name:
C        RHSP3
C
C     Function:
C        Read an HSP3 tape
C
C     Description:
C        RHSP3 reads tapes produced by the HSP3 high speed photometry
C        software at the AAT. The current version is limited to 16 bit
C        single channel data, and handles a maximum of 200000 time bins.
C
C     Parameters:
C        DRIVE      (Device)   The tape drive to read from.
C        MJDZERO    (Double)   The MJD at 0h U.T. on the night of observation.
C        OUTPUT     (TSP, 2D)  The output time series dataset.
C
C     Support:
C        Jeremy Bailey, AAO
C
C     Version date:
C        27/2/1988
C
C-
C
C  History:
C    27/11/1987   Original Version.   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE

*  Status argument
      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

      INTEGER TD            ! Tape Descriptor

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC1,LOC2
      INTEGER P1,P2


      IF (STATUS .EQ. SAI__OK) THEN

*  Mount the tape and get a tape descriptor

         CALL MAG_MOUNT('DRIVE','READ',STATUS)
         CALL MAG_ASSOC('DRIVE','READ',TD,STATUS)

*  Get temporary arrays for the data and time axis

         CALL TSP_TEMP(200000,'_REAL',P1,LOC1,STATUS)
         CALL TSP_TEMP(200000,'_DOUBLE',P2,LOC2,STATUS)

*  If OK read tape

         IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_RHSP3_MAIN(TD,%VAL(P1),%VAL(P2),STATUS)
            CALL MAG_ANNUL(TD,STATUS)
         ELSE
            CALL ERR_REP('MSG','Unable to mount tape ^STATUS',
     :          STATUS)
         ENDIF

*  Unmap data arrays

         CALL TSP_UNMAP(LOC1,STATUS)
         CALL TSP_UNMAP(LOC2,STATUS)
      ELSE
         CALL ERR_REP('MSG',
     :    'Unable to Allocate Tape Drive ^STATUS',STATUS)
      ENDIF
      END


      SUBROUTINE TSP_RHSP3_MAIN(TD,DATA,TIMES,STATUS)
*+
*
*   T S P _ R H S P 3 _ M A I N
*
*   RHSP3 command
*
*   Subroutine to do the work of the RHSP3 command. Read runs
*   from the HSP3 tape creating a new TSP output file for each
*   run until the end-of-file mark on the tape is reached.
*
*   Parameters:
*
*   (>)   TD       (Integer)            Tape descriptor for the input tape
*   (W)   DATA     (Real array(200000)  Workspace array to hold the data
*   (W)   TIMES    (Double array(200000) Workspace array to hold the times
*   (!)   STATUS   (Integer)            Status value
*
*   Jeremy Bailey   27/11/1987
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INTEGER TD,STATUS
      BYTE BUFFER(4016)                    ! Tape Buffer

      CHARACTER*80 CBUF                    ! Character string version
      INTEGER*2 BUF(2013)                  ! Integer*2 version
      EQUIVALENCE(BUF,BUFFER)
      EQUIVALENCE(CBUF(1:1),BUFFER(1))
      INTEGER INT                          ! Integration time
      INTEGER NCHANS                       ! Number of Channels
      INTEGER BUF1                         ! First word in buffer
      INTEGER EXP                          ! Experiment number
      LOGICAL END_OF_SESSION               ! End of Session Flag
      LOGICAL BIT16                        ! 16 Bit Flag
      INTEGER ACTVAL                       ! Actual number of values read
      REAL DATA(200000)                    ! Data array
      DOUBLE PRECISION TIMES(200000)       ! Times array
      LOGICAL END_OF_EXP                   ! End of Experiment flag
      INTEGER HOURS                        ! UT Hours
      INTEGER MINUTES                      ! UT Minutes
      INTEGER SECONDS                      ! UT Seconds
      INTEGER MILLISEC                     ! UT Millisec
      DOUBLE PRECISION T                   ! Time
      DOUBLE PRECISION JDZ                 ! JD Zero point
      INTEGER BIN                          ! Bin number
      INTEGER I                            ! Loop counter
      INTEGER DIMS(2)                      ! Dimensions of array
      CHARACTER*(DAT__SZLOC) LOC,DLOC      ! HDS Locators
      CHARACTER*40 LINE                    ! Output line
      REAL LAMBDA                          ! Wavelength
      INTEGER DPTR                         ! Data pointer

      IF (STATUS .EQ. SAI__OK) THEN

*  Read a block

         CALL MAG_READ(TD,4016,BUFFER,ACTVAL,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('MSG','Error Reading Tape ^STATUS',STATUS)
            RETURN
         ELSE

*  Check block size

            IF (ACTVAL .NE. 80) THEN
               CALL MSG_OUT('MSG',
     :            'Not an HSP3 Tape - Incorrect Block Size',STATUS)
               RETURN
            ENDIF

*  Check that it is an HSP3 tape header

            IF (CBUF(1:27) .NE. 'TAPE  HIGH SPEED PHOTOMETRY') THEN
               CALL MSG_OUT('MSG',
     :              'Not an HSP3 Tape - Incorrect Header',STATUS)
               RETURN
            ENDIF
            CALL MSG_OUT('MSG',CBUF(1:64),STATUS)

*  Loop until end of session

            END_OF_SESSION = .FALSE.
            DO WHILE (.NOT. END_OF_SESSION)

*  Read a block

                CALL MAG_READ(TD,4016,BUFFER,ACTVAL,STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                    CALL ERR_REP('MSG',
     :               'Error Reading Tape ^STATUS',STATUS)
                    RETURN
                ENDIF

*  Check for end of session

                BUF1 = ISHFTC(BUF(1),8,16)
                IF (BUF1 .EQ. -2) THEN
                    CALL MSG_OUT('MSG','End of Session',STATUS)
                    END_OF_SESSION = .TRUE.

*  Otherwise should be an experiment header

                ELSE IF(BUF1 .EQ. 0) THEN

*  Get experiment number

                    EXP = ISHFTC(BUF(5),8,16)
                    CALL MSG_SETI('EXP',EXP)
                    CALL MSG_OUT('MSG', 'EXPERIMENT ^EXP',STATUS)

*  Get integration time

                    INT = ISHFTC(BUF(6),8,16)
                    CALL MSG_SETI('INT',INT)
                    CALL MSG_OUT('MSG',
     :               'Integration Time ^INT milliseconds',STATUS)

*  Get number of channels

                    NCHANS = ISHFTC(BUF(7),8,16)
                    CALL MSG_SETI('NCHANS',NCHANS)
                    CALL MSG_OUT('MSG', '^NCHANS Channels',STATUS)

*  Get word size (16 or 32 bits)

                    BIT16 = ISHFTC(BUF(8),8,16) .EQ. 1
                    IF (BIT16) THEN
                        CALL MSG_OUT('MSG', '16 Bits ',STATUS)
                    ELSE
                        CALL MSG_OUT('MSG', '32 Bits ',STATUS)
                    ENDIF

*  Get User comment record

                    CALL MSG_OUT('MSG',CBUF(17:76),STATUS)
                    BIN = 1
                    END_OF_EXP = .FALSE.

*  Get JD of 0h U.T.

                    CALL PAR_GET0D('MJDZERO',JDZ,STATUS)
                    CALL PAR_CANCL('MJDZERO',STATUS)

*  Loop reading data blocks

                    DO WHILE (.NOT. END_OF_EXP)
                        CALL MAG_READ(TD,4016,BUFFER,ACTVAL,STATUS)
                        IF (ISHFTC(BUF(1),8,16) .NE. -1 .AND.
     :                      ISHFTC(BUF(1),8,16) .NE. 0) THEN

*  Get start UT for block

                          HOURS = ISHFTC(BUF(5),8,16)
                          MINUTES = ISHFTC(BUF(6),8,16)
                          SECONDS = ISHFTC(BUF(7),8,16)
                          MILLISEC = ISHFTC(BUF(8),8,16)
                          WRITE (LINE,'(I2.2,'':'',I2.2,'':''
     :                      ,I2.2,''.'',I3.3)')
     :                    HOURS,MINUTES,SECONDS,MILLISEC
                          CALL MSG_OUT('MSG',LINE,STATUS)

*  Calculate MJD of mid point of first bin

                          T = REAL(SECONDS) + REAL(MILLISEC)/1000.0
                          T = REAL(MINUTES) + T/60.0
                          T = REAL(HOURS) + T/60.0
                          T = T/24.0
                          T = T+JDZ
                          T = T+0.5D0*DBLE(INT)/86400.0D3

*  Loop getting data - byte swap it - scale to counts per sec
*  and calculate bin time

                          DO I=1,2000
                            DATA(BIN) = REAL(ISHFTC(BUF(I+8),8,16))
                            DATA(BIN) = DATA(BIN)*1000.0/REAL(INT)
                            TIMES(BIN) = T
                            T = T + DBLE(INT)/(86400.0D3)
                            BIN = BIN+1
                          ENDDO
                        ELSE

*  If experiment header found, the run has aborted
*  Skip back 1 block

                          IF (ISHFTC(BUF(1),8,16) .EQ. 0) THEN
                              CALL MSG_OUT('MSG','Aborted Run',STATUS)
                              CALL MAG_JUMP(TD,-1,STATUS)
                          ENDIF
                          END_OF_EXP = .TRUE.
                          BIN=BIN-1

*  Create output file and get locator

                          CALL DAT_CREAT('OUTPUT','NDF',
     :                     0,0,STATUS)
                          CALL DAT_ASSOC('OUTPUT','WRITE',LOC,STATUS)
                          DIMS(1)=1
                          DIMS(2)=BIN

*  Build the data array and write data

                          CALL TSP_CREATE_2D(LOC,1,BIN,' ',
     :                        .FALSE.,.FALSE.,STATUS)
                          CALL TSP_MAP_DATA(LOC,'WRITE',DPTR,DLOC,
     :                        STATUS)
                          IF (STATUS .EQ. SAI__OK) THEN
                             CALL TSP_GEN_MOVE(BIN,DATA,%VAL(DPTR))
                          ENDIF
                          CALL TSP_UNMAP(DLOC,STATUS)


*  Build the axis array and write times

                          CALL TSP_MAP_LAMBDA(LOC,'WRITE',DPTR,DLOC,
     :                        STATUS)
                          LAMBDA = 5000.0
                          IF (STATUS .EQ. SAI__OK) THEN
                             CALL TSP_GEN_MOVE(1,LAMBDA,%VAL(DPTR))
                          ENDIF
                          CALL TSP_UNMAP(DLOC,STATUS)
                          CALL TSP_MAP_TIME(LOC,'WRITE',DPTR,DLOC,
     :                        STATUS)
                          IF (STATUS .EQ. SAI__OK) THEN
                             CALL TSP_GEN_MOVE(2*BIN,TIMES,%VAL(DPTR))
                          ENDIF
                          CALL TSP_UNMAP(DLOC,STATUS)
                          CALL TSP_WLU_TIME(LOC,'MJD(UTC)','Days',
     :                        STATUS)

*  Annul locator and cancel parameter

                          CALL DAT_ANNUL(LOC,STATUS)
                          CALL DAT_CANCL('OUTPUT',STATUS)
                          IF (STATUS .NE. SAI__OK) THEN
                             RETURN
                          ENDIF
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
         ENDIF
      ENDIF
      END



