*     File = IRPHOT.FOR
*
*     Read in ASCII file (output from cgs3_phred) and do preliminary
*     reduction on it

*     24-Aug-1995 : original release
*       includes features: 1) output runs flagged as stds, with std mags
*       2) iterate on list of standards
*       3) give derived extinction for 10 and 20 microns (if both observed)
*       4) you decide what extinction to use
*       5) error on reduced values is sqrt of sum of squares of internal
*          error of individ obs and uncertainty in zero point
*       6) option to give output in Jy
*     21-Nov-1995 : in case the ASCII file that this works on is created
*       one run at a time, by hand, and out of order regarding run number,
*       make sure you process all the runs here.  Also, warn user if input
*       file contains multiple occurrences of a given run.
*     22-Nov-95 : rename to IRPHOT

      IMPLICIT NONE

      INTEGER MAX_RECORDS
      PARAMETER (MAX_RECORDS = 300)

      CHARACTER*10 OBJECT_NAME(MAX_RECORDS)
      CHARACTER*8  UTSTART(MAX_RECORDS)
      REAL         AMASS(MAX_RECORDS)
      REAL         WAVE_ST(MAX_RECORDS)
      REAL         WAVE_END(MAX_RECORDS)
      REAL         COUNTS(MAX_RECORDS)
      REAL         ERROR(MAX_RECORDS)
      LOGICAL      IF_RUN(MAX_RECORDS)
      LOGICAL      IF_STD(MAX_RECORDS)

      REAL         MAG(MAX_RECORDS)
      REAL         MAG_ERROR(MAX_RECORDS)
      REAL         CAT_VAL(MAX_RECORDS)
      REAL         STDIZED_MAG(MAX_RECORDS)
      REAL         STDIZED_ERR(MAX_RECORDS)
      REAL         X10(30), Y10(30), X20(30), Y20(30)
      CHARACTER*1  FILTER(MAX_RECORDS)

      CHARACTER IN_FILE*60, OUT_FILE*60
      CHARACTER OBJ*10, UT*8
      LOGICAL INY, NY, MORE
      INTEGER I, LU, N, LAST_RUN, RUN, NSTDS,NUM_NS,NUM_QS
      REAL TEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5
      REAL N_EXT, Q_EXT, NSUM, NSUM2, QSUM, QSUM2,SIGMAN,SIGMAQ
      REAL ZP, NZP, QZP, EXT, DIFF, ZP_ERR, EXT_ERR, RMS_RES
      REAL N_DERIVED, Q_DERIVED, QEXT_ERR, QRMS_RES
      REAL FLUX_MULT, FLUX, FLUX_ERR
      LOGICAL IF_CHANGED, IF_TEN, IF_TWENTY

      DATA LU/6/, N_EXT/0.15/, Q_EXT/0.42/

10    continue

      NSTDS = 0
      IF_TEN = .FALSE.
      IF_TWENTY = .FALSE.
      DO I = 1,MAX_RECORDS
        IF_RUN(I) = .FALSE.
        IF_STD(I) = .FALSE.
      ENDDO

      WRITE ( LU, 23 )
23    FORMAT ( / ' Enter name of input file: ', $ )
      READ ( 5, 33 ) IN_FILE
      OPEN ( UNIT=1,FILE=IN_FILE(1:60),STATUS='OLD',ERR=960)
33    FORMAT (A60)

      I = 0
      LAST_RUN = 0
40    I = I + 1
      READ ( 1, 43, END=50, ERR=970) RUN,OBJ,UT,TEMP1,TEMP2,
     :   TEMP3,TEMP4,TEMP5
      IF ( TEMP4 .GT. 0.0 ) THEN	! only apparent detections

        IF ( IF_RUN(RUN) ) THEN
          WRITE ( LU, 41 ) RUN
41        FORMAT ( ' *** Warning! Multiple occurrence of run ', I4, /
     :             '     Only the last one will be used.' )
        ENDIF

        IF_RUN(RUN) = .TRUE.


        IF ( RUN .GT. LAST_RUN ) THEN
          LAST_RUN = RUN
        ENDIF

        OBJECT_NAME(RUN)(1:10) = OBJ(1:10)
        UTSTART(RUN)(1:8) = UT(1:8)
        AMASS(RUN) = TEMP1
        WAVE_ST(RUN) = TEMP2
        IF ( TEMP2 .LT. 10.0 ) IF_TEN = .TRUE.
        WAVE_END(RUN) = TEMP3
        IF ( TEMP3 .GT. 20.0 ) IF_TWENTY = .TRUE.
        COUNTS(RUN) = TEMP4
        ERROR(RUN) = TEMP5
        WRITE ( LU, 43 ) RUN,OBJECT_NAME(RUN),UTSTART(RUN),
     :    AMASS(RUN),WAVE_ST(RUN),WAVE_END(RUN),COUNTS(RUN),ERROR(RUN)
      ELSE
        WRITE ( LU, 42 ) RUN
42      FORMAT ( ' *** Warning! Run ', I4, ' was not a detection.' /
     :   '     Negative signals are excluded from further analysis.')
      ENDIF

      GOTO 40
43    FORMAT(I4,1X,A10,1X,A8,1X,F8.4,2X,F8.4,2X,F8.4,2X,G12.6,
     :   1X,G12.6)

50    CLOSE ( UNIT = 1, ERR=980)

      DO I = 1,LAST_RUN
        IF ( IF_RUN(I) ) THEN
          IF ( COUNTS(I) .GT. 0.0 ) THEN
            MAG(I) = -2.5 * ALOG10(COUNTS(I))
            TEMP = -2.5 * ALOG10( COUNTS(I) + ERROR(I) )
            MAG_ERROR(I) = ABS ( TEMP - MAG(I) )
          ELSE
            MAG(I) = -99.0
            MAG_ERROR(I) = 0.0
          ENDIF
        ENDIF
      ENDDO

60    IF ( NSTDS .GT. 0 ) THEN
      NSUM = 0.0
      NSUM2 = 0.0
      QSUM = 0.0
      QSUM2 = 0.0
      NUM_NS = 0
      NUM_QS = 0
      WRITE ( LU, 63 )
63    FORMAT ( ' Run Object     UTstart    Amass     Raw Mag'
     :    '    Error  Filt Std Mag' )

        DO I = 1,LAST_RUN
          IF ( IF_STD(I) ) THEN
            WRITE ( LU, 65 ) I,OBJECT_NAME(I),UTSTART(I),
     :      AMASS(I),MAG(I),MAG_ERROR(I),FILTER(I),CAT_VAL(I)
          ENDIF
        ENDDO

        WRITE ( LU, 313 )

        DO I = 1,LAST_RUN
          IF ( IF_STD(I) ) THEN
            IF ( FILTER(I) .EQ. 'N' ) THEN
              NUM_NS = NUM_NS + 1
              TEMP = MAG(I) - N_EXT * AMASS(I)  ! extinction corrected
              ZP = CAT_VAL(I) - TEMP
              NSUM = NSUM + ZP
              NSUM2 = NSUM2 + ZP*ZP
              Y10(NUM_NS) = MAG(I) - CAT_VAL(I)
              X10(NUM_NS) = AMASS(I)
            ELSE IF ( FILTER(I) .EQ. 'Q' ) THEN
              NUM_QS = NUM_QS + 1
              TEMP = MAG(I) - Q_EXT * AMASS(I)
              QSUM = QSUM + ZP
              QSUM2 = QSUM2 + ZP*ZP
              Y20(NUM_QS) = MAG(I) - CAT_VAL(I)
              X20(NUM_QS) = AMASS(I)
            ENDIF
          ENDIF
        ENDDO

        IF ( NUM_NS .EQ. 1 ) THEN
          NZP = ZP
          SIGMAN = 0.0
        ELSE IF ( NUM_NS .GE. 2 ) THEN
          NZP = NSUM / NUM_NS
          TEMP = SQRT (( NSUM2 - NUM_NS * NZP * NZP )/( NUM_NS -1.0 ))
          SIGMAN = TEMP / SQRT ( FLOAT (NUM_NS) )
          DO I = 1,LAST_RUN
            IF ( FILTER(I) .EQ. 'N' .AND.
     :           IF_STD(I) ) THEN
              TEMP1 = MAG(I) - N_EXT * AMASS(I)
              ZP = CAT_VAL(I) - TEMP1
              DIFF = NZP - ZP	! zero point differential from mean
              WRITE ( LU, 66 ) I, OBJECT_NAME(I), ZP, DIFF
            ENDIF
          ENDDO
        ENDIF
        IF ( IF_TEN ) THEN
          WRITE ( LU, 67 ) NZP, SIGMAN, TEMP
        ENDIF

        IF ( NUM_QS .EQ. 1 ) THEN
          QZP = ZP
          SIGMAQ = 0.0
        ELSE IF ( NUM_QS .GE. 2 ) THEN
          QZP = QSUM / NUM_QS
          TEMP = SQRT (( QSUM2 - NUM_QS * QZP * QZP )/( NUM_QS -1.0 ))
          SIGMAQ = TEMP / SQRT ( FLOAT (NUM_QS) )
          DO I = 1,LAST_RUN
            IF ( FILTER(I) .EQ. 'Q' .AND.
     :           IF_STD(I) ) THEN
              TEMP1 = MAG(I) - N_EXT * AMASS(I)
              ZP = CAT_VAL(I) - TEMP1
              DIFF = QZP - ZP	! zero point differential from mean
              WRITE ( LU, 66 ) I, OBJECT_NAME(I), ZP, DIFF
            ENDIF
          ENDDO
        ENDIF
        IF ( IF_TWENTY ) THEN
          WRITE ( LU, 69 ) QZP, SIGMAQ, TEMP
        ENDIF

      ENDIF

65    FORMAT(I4,1X,A10,1X,A8,1X,F8.4,2X,F8.3,2X,F8.3,2X,A,2X,F8.3)
66    FORMAT(I4,1X,A10,1X,'zero point value = ',F6.3,'  diff = ', F6.3)
67    FORMAT (/ ' N-band ZP = ', F6.3, ' +/- ', F6.3, '  (sigma_x = ',
     :     F6.3,')'/ )
69    FORMAT (/ ' Q-band ZP = ', F6.3, ' +/- ', F6.3, '  (sigma_x = ',
     :     F6.3,')'/ )

      IF_CHANGED = .FALSE.

70    WRITE ( LU, 73 )
73    FORMAT ( ' Flag any (more) runs as STANDARDS? ', $ )
      MORE = NY(5)
      IF ( .NOT. MORE ) GOTO 110
80    WRITE ( LU, 83 )
83    FORMAT ( ' Enter run number: ', $ )
      READ ( 5, * ) RUN
      IF ( .NOT. IF_RUN(RUN) ) THEN
        WRITE(*,'(1X,A)') 'Invalid run # (undefined or negative signal)'
        GOTO 80
      ENDIF
      IF ( WAVE_END(RUN) .LT. 16.0 ) THEN
        WRITE ( LU, 93 ) OBJECT_NAME(RUN)
93      FORMAT ( ' Enter 10 micron magnitude for ',A,' : ', $ )
        FILTER(RUN) = 'N'
      ELSE
        WRITE ( LU, 103 ) OBJECT_NAME(RUN)
103     FORMAT ( ' Enter 20 micron magnitude for ',A,' : ', $ )
        FILTER(RUN) = 'Q'
      ENDIF
      READ ( 5, * ) CAT_VAL(RUN)
      IF_STD(RUN) = .TRUE.
      NSTDS = NSTDS + 1
      IF_CHANGED = .TRUE.
      GOTO 70


110   WRITE ( LU, 113 )
113   FORMAT ( ' Unflag any standards? ', $ )
      MORE = NY(5)
      IF ( MORE  ) THEN
120     WRITE ( LU, 123 )
123     FORMAT ( ' Enter run number: ', $ )
        READ ( 5, * ) RUN
        IF ( .NOT. IF_RUN(RUN) ) THEN
          WRITE(*,'(1X,A)')
     :          'Invalid run # (non-defined or negative signal)'
          GOTO 120
        ENDIF
        IF ( IF_STD(RUN)) THEN
          IF_STD(RUN) = .FALSE.
          NSTDS = NSTDS - 1
          IF_CHANGED = .TRUE.
        ELSE
          WRITE(*,'(1X,A)')
     :          'That run is not presently flagged as a standard'
        ENDIF
        GOTO 110
      ELSE
        IF ( IF_CHANGED ) GOTO 60
      ENDIF

      IF ( NSTDS .LE. 0 ) THEN
        WRITE(*,'(1X,A)')
     :        'You have not flagged any observations as stds!'
        GOTO 70
      ENDIF

      WRITE ( LU, 127 )
127   FORMAT ( ' Derive/fix extinction value(s)? ', $ )
      IF (.NOT. NY(5)  ) GOTO 180

C     Decide on extinctions

      IF ( IF_TEN ) THEN
        IF ( NUM_NS .GE. 2 ) THEN	! try to derive extinction
          CALL REGR ( X10, Y10, NUM_NS, ZP, N_DERIVED, ZP_ERR, EXT_ERR,
     :       RMS_RES )
          WRITE ( LU, 133 ) N_DERIVED, EXT_ERR, RMS_RES, NUM_NS
133       FORMAT ( ' Derived extinction = ', F6.3, ' +/- ', F6.3,/
     :             ' RMS residual = ', F6.3, ' based on 'I3' standards')
          N_EXT = N_DERIVED
        ENDIF
        WRITE ( LU, 143 ) N_EXT
143     FORMAT ( ' Enter 10 micron extinction (',F6.3,') :', $ )
        READ ( 5, * ) N_EXT
      ENDIF

      IF ( IF_TWENTY ) THEN
        IF ( NUM_QS .GE. 2 ) THEN	! try to derive extinction
          CALL REGR ( X20, Y20, NUM_QS, ZP, Q_DERIVED, ZP_ERR, QEXT_ERR,
     :       QRMS_RES )
          WRITE ( LU, 133 ) Q_DERIVED, QEXT_ERR, QRMS_RES, NUM_QS
          Q_EXT = Q_DERIVED
        ENDIF
        WRITE ( LU, 153 ) Q_EXT
153     FORMAT ( ' Enter 20 micron extinction (',F6.3,') :', $ )
        READ ( 5, * ) Q_EXT
      ENDIF
      GOTO 60

180     WRITE ( LU, 183 )
183     FORMAT ( ' Enter name of output file for reduced mags: ', $ )
        READ ( 5, 187 ) OUT_FILE
187     FORMAT ( A60 )

        OPEN ( UNIT=1,FILE=OUT_FILE(1:60),STATUS='NEW',ERR=960 )

        WRITE ( 1, 193 ) IN_FILE
193     FORMAT ( ' Input file was: ', A /
     :           ' Adopted standards and catalog magnitudes: ' )

        DO I = 1,LAST_RUN
          IF ( IF_STD(I) ) THEN
            WRITE ( 1, 203 ) I, OBJECT_NAME(I), FILTER(I), CAT_VAL(I)
203         FORMAT (I4,1X,A10,2X,A,2X,F6.3)
          ENDIF
        ENDDO

        IF ( NUM_NS .GE. 2 ) THEN
          WRITE ( 1, 213 ) N_DERIVED, EXT_ERR, RMS_RES, NUM_NS
213       FORMAT ( ' Derived 10 micron extinction = ', F6.3, ' +/- ',
     :    F6.3 / ' RMS residual = ', F6.3, ' based on 'I3' standards' )
        ENDIF

        IF ( NUM_QS .GE. 2 ) THEN
          WRITE ( 1, 223 ) Q_DERIVED, QEXT_ERR, QRMS_RES, NUM_QS
223       FORMAT ( ' Derived 20 micron extinction = ', F6.3, ' +/- ',
     :    F6.3 / ' RMS residual = ', F6.3, ' based on 'I3' standards' )
        ENDIF

        IF ( IF_TEN ) THEN
          WRITE ( 1, 233 ) N_EXT, NZP, SIGMAN
233       FORMAT ( ' Adopted 10 micron extinction = ', F6.3 /
     :             ' 10 micron zero point = ', F6.3, ' +/- ', F6.3 / )
        ENDIF

        IF ( IF_TWENTY ) THEN
          WRITE ( 1, 243 ) Q_EXT, QZP, SIGMAQ
243       FORMAT ( ' Adopted 20 micron extinction = ', F6.3 /
     :             ' 20 micron zero point = ', F6.3, ' +/- ', F6.3 / )
        ENDIF

        WRITE ( 1, 273 )
273    FORMAT ( ' Run Object     UTstart    Amass     Wave_ST'
     :    '  Wave_END Std Mag Error Obs-Cat'/ )


      N = 0
      DO I = 1,LAST_RUN
        IF ( IF_RUN(I) ) THEN
          IF ( WAVE_ST(I) .LT. 10.0 ) THEN
            EXT = N_EXT
            ZP = NZP
            TEMP = SIGMAN
          ELSE
            EXT = Q_EXT
            ZP = QZP
            TEMP = SIGMAQ
          ENDIF

          STDIZED_MAG(I) = MAG(I) - EXT * AMASS(I) + ZP
          TEMP2 = MAG_ERROR(I)
          STDIZED_ERR(I) = SQRT ( TEMP * TEMP + TEMP2 * TEMP2 )

          IF ( IF_STD(I) ) THEN
            TEMP = STDIZED_MAG(I) - CAT_VAL(I)
            WRITE ( 1, 283, ERR=970 ) I,OBJECT_NAME(I),UTSTART(I),
     :      AMASS(I),WAVE_ST(I),WAVE_END(I),STDIZED_MAG(I),
     :      STDIZED_ERR(I),TEMP
            N = N + 1
          ELSE
            WRITE ( 1, 293, ERR=970 ) I,OBJECT_NAME(I),UTSTART(I),
     :      AMASS(I),WAVE_ST(I),WAVE_END(I),STDIZED_MAG(I),
     :      STDIZED_ERR(I)
            N = N + 1
          ENDIF
          IF ( MOD (N,5) .EQ. 0 ) WRITE ( 1, 313 )

        ENDIF

      ENDDO


283     FORMAT(I4,1X,A10,1X,A8,1X,F8.4,2X,F8.4,2X,F8.4,1X,F6.3,
     :   1X,F6.3,' * ',F6.3)

293     FORMAT(I4,1X,A10,1X,A8,1X,F8.4,2X,F8.4,2X,F8.4,1X,F6.3,
     :   1X,F6.3)

313     FORMAT ( ' ' )

      WRITE ( LU, 323 )
323   FORMAT ( ' Output the results in Janskys? ', $ )
      IF ( .NOT. NY(5)  ) GOTO 400

*     Output in Jy, given alpha Lyr calibration

      WRITE ( 1, 333 )
333   FORMAT ( / ' Results in Janskys,'
     :       ' given IRTF alpha Lyrae calibration:' /)

      WRITE ( 1, 343 )
343    FORMAT ( ' Run Object     UTstart    Amass     Wave_ST'
     :    '  Wave_END     Jy        Error'/ )

      N = 0
      DO I = 1,LAST_RUN
        IF ( IF_RUN(I) ) THEN
          IF ( WAVE_ST(I) .LT. 10.0 ) THEN
            FLUX_MULT = 39.8	! 10 micron calibr
          ELSE
            FLUX_MULT = 10.4	! 20 micron calibr
          ENDIF

          FLUX = 10.0 ** ( -0.4 * STDIZED_MAG(I) ) * FLUX_MULT
          TEMP1 = 10.0 ** ( -0.4 * ( STDIZED_MAG(I) + STDIZED_ERR(I)))
          TEMP2 = 10.0 ** ( -0.4 * ( STDIZED_MAG(I) - STDIZED_ERR(I)))
          FLUX_ERR = ABS ( TEMP1 - TEMP2 ) * 0.5 * FLUX_MULT

          WRITE ( 1, 353, ERR=970 ) I, OBJECT_NAME(I), UTSTART(I),
     :      AMASS(I),WAVE_ST(I),WAVE_END(I),FLUX, FLUX_ERR

          N = N + 1
          IF ( MOD (N,5) .EQ. 0 ) WRITE ( 1, 313 )

        ENDIF

      ENDDO

353     FORMAT(I4,1X,A10,1X,A8,1X,F8.4,2X,F8.4,2X,F8.4,1X,F10.4,
     :   1X,F10.4)


400   CLOSE ( UNIT=1, ERR=980 )

      WRITE ( LU, 433 )
433   FORMAT ( ' Continue? ', $ )
      INY = NY(5)
      IF ( INY  ) GOTO 10
      GOTO 990

C
C	FILE ACCESS ERROR MESSAGES **************************************
C
960	WRITE (LU,963)
963	FORMAT (' Error opening file ')
	GOTO 990
*
970     WRITE ( LU, 973)
973     FORMAT (' Error accessing file' )
        goto 990
C
980	WRITE (LU,983)
983	FORMAT (' Error closing file' )

990     CONTINUE

        END
