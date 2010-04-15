C+
C                          G E N _ T I M E
C
C  Routine name:
C     GEN_TIME
C
C  Function:
C     Formats current day and time in a friendly way.
C
C  Description:
C     Returns the current day and time formatted in a friendly
C     way - ie as 'Wednesday 25th December 1985 11:25 am' rather
C     than as '25-DEC-85 11:25:00'.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL GEN_TIME(STYLE,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
C
C  Parameters -    (">" input, "<" output)
C     (>) STYLE    (Integer, ref) The low bits of STYLE control the
C                  format of the result.
C                  Bit 0 (the least significant bit) should be
C                  set if the seconds are to be included in the
C                  time.
C                  Bit 1 should be set if the time of day is to
C                  be given as 'am' or 'pm' rather than in 24
C                  hour format.
C                  Bit 2 should be set if the month is to precede
C                  the day, ie 'December 25th, 1985', rather than
C                  '25th December 1985'.
C     (<) DAY      (Fixed string, descr) Returned with the string
C                  giving the day of the week. Should be at least 9
C                  characters.
C     (<) LDAY     (Integer, ref) Returned with the number of characters
C                  used for DAY.
C     (<) DATE     (Fixed string, descr) Returned with the string
C                  giving the date.  Should be at least 20 characters.
C     (<) LDATE    (Integer, ref) Returned with the number of characters
C                  used for DATE.
C     (<) HOUR     (Fixed string, descr) Returned with the string giving
C                  the time of day.  Should be at least 12 characters.
C     (<) LHOUR    (Integer, ref) Returned with the number of characters
C                  used for the time.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C           Clive Davenhall, UoE, Starlink
C
C  Internal declaration:
C     SUBROUTINE GEN_TIME(STYLE,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
C     INTEGER STYLE, LDAY, LDATE, LHOUR
C     CHARACTER*(*) DAY, DATE, HOUR
C
C  Example:
C     CALL GEN_TIME(6,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
C     PRINT *,'This program was run on ',DAY(:LDAY),' ',DATE(:LDATE),
C                                     ' at ',HOUR(:LHOUR)
C     will print something like:
C
C     This program was run on Thursday January 17th, 1985 at 2:15 pm
C
C  History:
C      8th Apr 1987   KS/AAO. Original version.
C      ??  ??  1992   HME/UoE. Modified to use PSX calls for portablility.
C      2nd Jun 1993   KS/AAO. Added calls to EMS to control any PSX error
C                     reporting.
C      30th Jun 1993  KS/AAO. Unused variables removed.
C      5th Sep 2000   ACD/UoE. Fixed Y2K bug in calculating argument DATE.
C+
      SUBROUTINE GEN_TIME(STYLE,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STYLE, LDAY, LDATE, LHOUR
      CHARACTER*(*) DAY, DATE, HOUR
C
C     Local variables
C
      INTEGER EMS_STATUS         ! Status used by EMS routines
      INTEGER STATUS             ! PSX status
      INTEGER NTICKS             ! Ticks since calendar start
      INTEGER PSXYDY, PSXDST     ! Day of year, DST flag
      INTEGER TSTRCT             ! Pointer to time structure
C
      LOGICAL AMPM, SECS, USORDR
      INTEGER DAYSL(7), IDAY, IGNORE, IH, IHC, IM, IPTR, IS
      INTEGER MLEN(12), MONTH, NDAY, TENS, UNITS, YEAR
      CHARACTER ABBR*2, ABBRS(3)*2, AMSTR*2, DAYS(7)*9
      CHARACTER DAYSTR*20, MONTHS(12)*9, TIMSTR*20
C
      DATA ABBRS/'st','nd','rd'/
      DATA DAYS/'Sunday','Monday','Tuesday','Wednesday','Thursday',
     :          'Friday','Saturday'/
      DATA DAYSL/6,6,7,9,8,6,8/
      DATA MONTHS/'January','February','March','April','May',
     :            'June','July','August','September','October',
     :            'November','December'/
      DATA MLEN/7,8,5,5,3,4,4,6,9,7,8,8/
C
C     Get the basic data.
C
      STATUS = 0
      EMS_STATUS=0
      CALL EMS_BEGIN(EMS_STATUS)
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, IS, IM, IH,
     :   IDAY, MONTH, YEAR, NDAY, PSXYDY, PSXDST, TSTRCT, STATUS )
      EMS_STATUS=0
      CALL EMS_ANNUL(EMS_STATUS)
      CALL EMS_END(EMS_STATUS)
      NDAY = NDAY + 1
      MONTH = MONTH + 1
C
C     Set the style flags
C
      USORDR=IAND(STYLE,4).NE.0
      AMPM=IAND(STYLE,2).NE.0
      SECS=IAND(STYLE,1).NE.0
C
C     Return day of the week
C
      DAY = DAYS(NDAY)
      LDAY = MIN(DAYSL(NDAY),LEN(DAY))
C
C     Format the day
C
      DAYSTR=' '
      IF (USORDR) THEN
         DAYSTR=MONTHS(MONTH)
         IPTR=MLEN(MONTH)+2
      ELSE
         IPTR=1
      END IF
      TENS=IDAY/10
      UNITS=IDAY-10*TENS
      IF (TENS.GT.0) THEN
         DAYSTR(IPTR:IPTR)=CHAR(TENS+ICHAR('0'))
         IPTR=IPTR+1
      END IF
      DAYSTR(IPTR:IPTR)=CHAR(UNITS+ICHAR('0'))
      IF ((UNITS.GE.4).OR.(UNITS.EQ.0).OR.(TENS.EQ.1)) THEN
         ABBR='th'
      ELSE
         ABBR=ABBRS(UNITS)
      END IF
      DAYSTR(IPTR+1:IPTR+2)=ABBR
      IPTR=IPTR+3
      IF (USORDR) THEN
         DAYSTR(IPTR:IPTR)=','
         IPTR=IPTR+2
      ELSE
         IPTR=IPTR+1
         DAYSTR(IPTR:)=MONTHS(MONTH)
         IPTR=IPTR+MLEN(MONTH)+1
      END IF
C
C     Convert the returned year to the genuine year AD.
C
      YEAR = YEAR + 1900
      WRITE(DAYSTR(IPTR:IPTR+3),'(I4)',IOSTAT=IGNORE) YEAR
      IPTR = IPTR+4
C
C     Return formatted day to caller.
C
      DATE=DAYSTR
      LDATE=MIN(IPTR-1,LEN(DATE))
C
C     Now format the time
C
      TIMSTR=' '
      IHC=IH
      IF (AMPM) THEN
         IF (IH.LT.12) THEN
            AMSTR='am'
         ELSE
            AMSTR='pm'
            IF (IH.GT.12) IHC=IH-12
         END IF
      END IF
      TENS=IHC/10
      UNITS=IHC-10*TENS
      IF ((.NOT.AMPM).OR.(TENS.GT.0)) THEN
         TIMSTR(1:1)=CHAR(TENS+ICHAR('0'))
         IPTR=2
      ELSE
         IPTR=1
      END IF
      TIMSTR(IPTR:IPTR)=CHAR(UNITS+ICHAR('0'))
      IPTR=IPTR+1
      WRITE(TIMSTR(IPTR:IPTR+2),'(A,I2.2)',IOSTAT=IGNORE) ':',IM
      IPTR=IPTR+3
      IF (SECS) THEN
         WRITE(TIMSTR(IPTR:IPTR+2),'(A,I2.2)',IOSTAT=IGNORE) ':',IS
         IPTR=IPTR+3
      END IF
      IF (AMPM) THEN
         TIMSTR(IPTR+1:IPTR+2)=AMSTR
         IPTR=IPTR+3
         IF (IH+IM+IS.EQ.0) THEN
            TIMSTR='12 midnight'
            IPTR=12
         ELSE IF ((IH.EQ.12).AND.(IM+IS.EQ.0)) THEN
            TIMSTR='12 noon'
            IPTR=8
         END IF
      END IF
C
C     Return time of day to caller
C
      HOUR=TIMSTR
      LHOUR=MIN(IPTR-1,LEN(HOUR))
C
      END
