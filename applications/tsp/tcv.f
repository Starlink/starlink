C+
      SUBROUTINE TCV_S2MJD(STRING,MJD,STATUS)
C
C            T C V _ S 2 M J D
C
C     Routine Name:
C        TCV_S2MJD
C
C     Function:
C        Convert a string containing a date to an MJD
C
C     Description:
C        Decode a string containing a date in the form year, month, day
C        and derive the corresponding MJD.
C
C     Call:
C        CALL TCV_S2MJD(STRING,MJD,STATUS)
C
C     Parameters:
C    (>) STRING      (Fixed string, descr) - The string to be converted.
C    (<) MJD         (Double) - The resulting MJD.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C         SLA_INTIN,
C         SLA_CALDJ
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 28/6/1988
C
C-
C
C  History:
C    28/6/1988   Original Version.   JAB/AAO
C+
      IMPLICIT NONE
      CHARACTER*(*) STRING
      DOUBLE PRECISION MJD
      INTEGER STATUS

      INTEGER NSTRT,IYEAR, IMONTH, IDAY, JFLAG

      IF (STATUS .EQ. 0) THEN
          CALL CHR_UCASE(STRING)
          NSTRT = 1
          CALL SLA_INTIN(STRING,NSTRT,IYEAR,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          CALL SLA_INTIN(STRING,NSTRT,IMONTH,JFLAG)
          IF (JFLAG .GE. 1) THEN
              IF (STRING(NSTRT:NSTRT+2) .EQ. 'JAN') THEN
                  IMONTH = 1
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'FEB') THEN
                  IMONTH = 2
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'MAR') THEN
                  IMONTH = 3
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'APR') THEN
                  IMONTH = 4
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'MAY') THEN
                  IMONTH = 5
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'JUN') THEN
                  IMONTH = 6
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'JUL') THEN
                  IMONTH = 7
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'AUG') THEN
                  IMONTH = 8
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'SEP') THEN
                  IMONTH = 9
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'OCT') THEN
                  IMONTH = 10
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'NOV') THEN
                  IMONTH = 11
              ELSE IF (STRING(NSTRT:NSTRT+2) .EQ. 'DEC') THEN
                  IMONTH = 12
              ELSE
                  STATUS = JFLAG
                  RETURN
              ENDIF
              DO WHILE (STRING(NSTRT:NSTRT) .GE. 'A'
     :            .AND. STRING(NSTRT:NSTRT) .LE. 'Z')
                  NSTRT = NSTRT+1
              ENDDO
          ENDIF
          IDAY = 0
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          CALL SLA_INTIN(STRING,NSTRT,IDAY,JFLAG)
          IF (JFLAG .EQ. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          CALL SLA_CALDJ(IYEAR,IMONTH,IDAY,MJD,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
      ENDIF
      END


C+
      SUBROUTINE TCV_MJD2S(MJD,STRING,STATUS)
C
C            T C V _ M J D 2 S
C
C     Routine Name:
C        TCV_MJD2S
C
C     Function:
C        Convert a modified Julian Date to a string
C
C     Description:
C        Given a modified Julian Date, derive a string containing
C        the corresponding calendar date.
C
C     Call:
C        CALL TCV_MJD2S(MJD,STRING,STATUS)
C
C     Parameters:
C    (>) MJD         (Double) - The MJD to be converted.
C    (<) STRING      (Fixed string, descr) - The resulting string.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C        SLA_DJCL
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 29/6/1988
C
C-
C
C  History:
C    29/6/1988   Original Version.   JAB/AAO
C+
      IMPLICIT NONE
      DOUBLE PRECISION MJD
      CHARACTER*(*) STRING
      INTEGER STATUS

      INTEGER IYEAR, IMONTH, IDAY, JFLAG
      DOUBLE PRECISION FD
      CHARACTER*3 CMONTH(12)

      DATA CMONTH /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     :    'Sep','Oct','Nov','Dec'/

      IF (STATUS .EQ. 0) THEN
          CALL SLA_DJCL(MJD,IYEAR,IMONTH,IDAY,FD,JFLAG)
          IF (JFLAG .GE. 1) THEN
               STATUS = JFLAG
               RETURN
          ENDIF
          WRITE(STRING,'(I5,'' '',A3,'' '',I2)',ERR=100) IYEAR,
     :         CMONTH(IMONTH),IDAY
      ENDIF
100   CONTINUE
      END



C+
      SUBROUTINE TCV_S2TIME(STRING,D,STATUS)
C
C            T C V _ S 2 T I M E
C
C     Routine Name:
C        TCV_S2TIME
C
C     Function:
C        Convert a string containing a time in HMS to days
C
C     Description:
C        Decode a string containing a time in hours, minutes and seconds
C        and derive the corresponding time in days.
C
C     Call:
C        CALL TCV_S2TIME(STRING,D,STATUS)
C
C     Parameters:
C    (>) STRING      (Fixed string, descr) - The string to be converted.
C    (<) D           (Double) - The resulting time in days.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C         SLA_INTIN,
C         SLA_DFLTIN,
C         SLA_DTF2D
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+

      IMPLICIT NONE
      CHARACTER*(*) STRING
      DOUBLE PRECISION D
      INTEGER STATUS

      INTEGER NSTRT,IHOUR, IMIN, JFLAG
      DOUBLE PRECISION SEC

      IF (STATUS .EQ. 0) THEN
          NSTRT = 1
          CALL SLA_INTIN(STRING,NSTRT,IHOUR,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          IMIN = 0
          CALL SLA_INTIN(STRING,NSTRT,IMIN,JFLAG)
          IF (JFLAG .EQ. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          SEC = 0.0D0
          CALL SLA_DFLTIN(STRING,NSTRT,SEC,JFLAG)
          IF (JFLAG .GE. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          CALL SLA_DTF2D(IHOUR,IMIN,SEC,D,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
      ENDIF
      END



C+
      SUBROUTINE TCV_TIME2S(D,NDP,SEP,STRING,STATUS)
C
C            T C V _ T I M E 2 S
C
C     Routine Name:
C        TCV_TIME2S
C
C     Function:
C        Convert a time as a fraction of a day to a string in HMS
C
C     Description:
C        Given a time expressed as a fraction of a day derive a string
C        in Hours, minutes and seconds.
C
C     Call:
C        CALL TCV_TIME2S(D,NDP,SEP,STRING,STATUS)
C
C     Parameters:
C    (>) MJD         (Double) - The time to be converted.
C    (>) NDP         (Integer) - Number of decimal places on seconds.
C    (>) SEP         (Character)  -  Seperator character
C    (<) STRING      (Fixed string, descr) - The resulting string.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C        SLA_DD2TF
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+
      IMPLICIT NONE
      DOUBLE PRECISION D
      INTEGER NDP
      CHARACTER*1 SEP
      CHARACTER*(*) STRING
      INTEGER STATUS
      CHARACTER*1 SIGN
      CHARACTER*20 SECFORMAT
      INTEGER IHMSF(4)

      IF (STATUS .EQ. 0) THEN
        IF (NDP .GE. 1 .AND. NDP .LT. 10) THEN
          WRITE(SECFORMAT,'('',''''.'''',I'',I1,''.'',I1,'')'')')
     :        ,NDP,NDP
        ELSE
          SECFORMAT = ' '
        ENDIF
        CALL SLA_DD2TF(NDP,D,SIGN,IHMSF)
        WRITE(STRING,'(I2.2,A1,I2.2,A1,I2.2'//SECFORMAT,ERR=100)
     :         IHMSF(1),SEP,IHMSF(2),SEP,IHMSF(3),IHMSF(4)
      ENDIF
100   CONTINUE
      END


C+
      SUBROUTINE TCV_S2RA(STRING,RA,STATUS)
C
C            T C V _ S 2 R A
C
C     Routine Name:
C        TCV_S2RA
C
C     Function:
C        Convert a string containing an RA in HMS to radians
C
C     Description:
C        Decode a string containing a Right Ascension in hours, minutes and
C        seconds and derive the corresponding angle in radians.
C
C     Call:
C        CALL TCV_S2RA(STRING,RA,STATUS)
C
C     Parameters:
C    (>) STRING      (Fixed string, descr) - The string to be converted.
C    (<) RA          (Double) - The resulting RA in radians.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C         SLA_INTIN,
C         SLA_DFLTIN,
C         SLA_DTF2R
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+

      IMPLICIT NONE
      CHARACTER*(*) STRING
      DOUBLE PRECISION RA
      INTEGER STATUS

      INTEGER NSTRT,IHOUR, IMIN, JFLAG
      DOUBLE PRECISION SEC

      IF (STATUS .EQ. 0) THEN
          NSTRT = 1
          CALL SLA_INTIN(STRING,NSTRT,IHOUR,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          IMIN = 0
          CALL SLA_INTIN(STRING,NSTRT,IMIN,JFLAG)
          IF (JFLAG .EQ. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          SEC = 0.0D0
          CALL SLA_DFLTIN(STRING,NSTRT,SEC,JFLAG)
          IF (JFLAG .GE. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          CALL SLA_DTF2R(IHOUR,IMIN,SEC,RA,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
      ENDIF
      END



C+
      SUBROUTINE TCV_RA2S(RA,NDP,SEP,STRING,STATUS)
C
C            T C V _ R A 2 S
C
C     Routine Name:
C        TCV_RA2S
C
C     Function:
C        Convert an RA in radians to a string in HMS
C
C     Description:
C        Given an angle in radians derive a string
C        in Hours, minutes and seconds.
C
C     Call:
C        CALL TCV_RA2S(RA,NDP,SEP,STRING,STATUS)
C
C     Parameters:
C    (>) RA          (Double) - The angle to be converted.
C    (>) NDP         (Integer) - Number of decimal places on seconds.
C    (>) SEP         (Character) - Seperator character
C    (<) STRING      (Fixed string, descr) - The resulting string.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C        SLA_DRANRM,
C        SLA_DR2TF
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+
      IMPLICIT NONE
      DOUBLE PRECISION RA
      INTEGER NDP
      CHARACTER*1 SEP
      CHARACTER*(*) STRING
      INTEGER STATUS
      CHARACTER*1 SIGN
      INTEGER IHMSF(4)
      CHARACTER*20 SECFORMAT
      DOUBLE PRECISION SLA_DRANRM,D

      IF (STATUS .EQ. 0) THEN
        D = SLA_DRANRM(RA)
        IF (NDP .GE. 1 .AND. NDP .LT. 10) THEN
          WRITE(SECFORMAT,'('',''''.'''',I'',I1,''.'',I1,'')'')')
     :        ,NDP,NDP
        ELSE
          SECFORMAT = ' '
        ENDIF
        CALL SLA_DR2TF(NDP,D,SIGN,IHMSF)
        WRITE(STRING,'(I2.2,A1,I2.2,A1,I2.2'//SECFORMAT,ERR=100)
     :         IHMSF(1),SEP,IHMSF(2),SEP,IHMSF(3),IHMSF(4)
      ENDIF
100   CONTINUE
      END



C+
      SUBROUTINE TCV_S2DEC(STRING,DEC,STATUS)
C
C            T C V _ S 2 D E C
C
C     Routine Name:
C        TCV_S2DEC
C
C     Function:
C        Convert a string containing a Dec in DMS to radians
C
C     Description:
C        Decode a string containing a Declination in degrees, minutes and
C        seconds and derive the corresponding angle in radians.
C
C     Call:
C        CALL TCV_S2DEC(STRING,DEC,STATUS)
C
C     Parameters:
C    (>) STRING      (Fixed string, descr) - The string to be converted.
C    (<) RA          (Double) - The resulting Dec in radians.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C         SLA_INTIN,
C         SLA_DFLTIN,
C         SLA_DAF2R
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+

      IMPLICIT NONE
      CHARACTER*(*) STRING
      DOUBLE PRECISION DEC
      INTEGER STATUS

      INTEGER NSTRT,IDEG, IMIN, JFLAG
      DOUBLE PRECISION SEC
      LOGICAL NEGATIVE

      IF (STATUS .EQ. 0) THEN
          NSTRT = 1
          CALL SLA_INTIN(STRING,NSTRT,IDEG,JFLAG)
          NEGATIVE = JFLAG .EQ. -1
          IF (NEGATIVE) IDEG = - IDEG
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          IMIN = 0
          CALL SLA_INTIN(STRING,NSTRT,IMIN,JFLAG)
          IF (JFLAG .EQ. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (STRING(NSTRT:NSTRT) .EQ. ':' .OR.
     :        STRING(NSTRT:NSTRT) .EQ. '/') NSTRT = NSTRT + 1
          SEC = 0.0
          CALL SLA_DFLTIN(STRING,NSTRT,SEC,JFLAG)
          IF (JFLAG .GE. 2) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          CALL SLA_DAF2R(IDEG,IMIN,SEC,DEC,JFLAG)
          IF (JFLAG .GE. 1) THEN
              STATUS = JFLAG
              RETURN
          ENDIF
          IF (NEGATIVE) DEC = -DEC
      ENDIF
      END



C+
      SUBROUTINE TCV_DEC2S(DEC,NDP,SEP,STRING,STATUS)
C
C            T C V _ D E C 2 S
C
C     Routine Name:
C        TCV_DEC2S
C
C     Function:
C        Convert a Dec in radians to a string in DMS
C
C     Description:
C        Given an angle in radians derive a string
C        in Degrees, minutes and seconds.
C
C     Call:
C        CALL TCV_DEC2S(DEC,NDP,SEP,STRING,STATUS)
C
C     Parameters:
C    (>) DEC         (Double) - The angle to be converted.
C    (>) NDP         (Integer) - Number of decimal places for seconds.
C    (>) SEP         (Character) - Seperator character.
C    (<) STRING      (Fixed string, descr) - The resulting string.
C    (!) STATUS      (Integer) - Status value.
C
C     External Routines called:
C        SLA_DR2AF
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 27/7/1988
C
C-
C
C  History:
C    27/7/1988   Original Version.   JAB/AAO
C+
      IMPLICIT NONE
      DOUBLE PRECISION DEC
      INTEGER NDP
      CHARACTER*1 SEP
      CHARACTER*(*) STRING
      INTEGER STATUS
      CHARACTER*1 SIGN
      CHARACTER*20 SECFORMAT
      INTEGER IDMSF(4)

      IF (STATUS .EQ. 0) THEN
        IF (NDP .GE. 1 .AND. NDP .LT. 10) THEN
          WRITE(SECFORMAT,'('',''''.'''',I'',I1,''.'',I1,'')'')')
     :        ,NDP,NDP
        ELSE
          SECFORMAT = ' '
        ENDIF
        CALL SLA_DR2AF(NDP,DEC,SIGN,IDMSF)
        WRITE(STRING,'(A1,I2.2,A1,I2.2,A1,I2.2'//SECFORMAT,
     :         ERR=100)
     :         SIGN,IDMSF(1),SEP,IDMSF(2),SEP,IDMSF(3),IDMSF(4)
      ENDIF
100   CONTINUE
      END

