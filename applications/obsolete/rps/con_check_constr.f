*+CON_CHECK_CONSTR Does a consistency check on the constraints form.
      SUBROUTINE CON_CHECK_CONSTR(REF_NO,MESID,SCREEN,XTYPE,FIELD_NO)
      IMPLICIT NONE

*   Input :
      INTEGER REF_NO						!Database reference number.
      INTEGER MESID						!Screen number.
      LOGICAL SCREEN						!Set to TRUE if in screen mode.

*   Output :
      CHARACTER*1 XTYPE
      INTEGER FIELD_NO						!Field number being referenced.
 
*  Global Variables
      INCLUDE 'com_form_qual.inc'
      DOUBLE PRECISION AO_MJD0, AO_PERIOD, AO_DELAY
      COMMON /AO_SPECS/ AO_MJD0, AO_PERIOD, AO_DELAY


*  History
*     1988 Sept		M Bush		1st version
*     1989 Jan, Feb	M Ricketts	Mods - set qual flag, etc.
*     1992 JUNE		M DUESTERHAUS   PORT TO UNIX
*     1996 Mar          M Ricketts      Update date constraints
****************************************************************************-
*   Functions :
      INTEGER DBS_FIELDNO					!Gets field number from the database.
      INTEGER DBS_GETI						!Gets integer value from the database.
      REAL DBS_GETR						!Gets real value from the database.
      LOGICAL DBS_GETL						!Gets logical value from user (screen mode).
      CHARACTER*1 CON_CHECK_ERR, CON_CHECK_WARN
      LOGICAL CON_RECHECK_TARG
      
*   Local :
      REAL RVAL							!Real value.
      INTEGER IVAL						!Integer value.
      INTEGER ERRF1, ERRF2, ERRF3, ERRF4			!Error flags.
      CHARACTER MESSAGE*30					!Error message output to the screen.

      INTEGER START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MINUTE
      INTEGER END_YEAR, END_MONTH, END_DAY, END_HOUR, END_MINUTE
      LOGICAL LVAL1,LVAL2,LVAL3,LVAL4				!Set to TRUE on selection of a given constraint.
      DOUBLE PRECISION MJD1,MJD2				!Start and end times in MJD.
      REAL FMJD1,FMJD2						!
      DOUBLE PRECISION DRA,DDEC, MJD_VIS(2,2), mid_vis, diff_vis(2)
      INTEGER NVIS, IVIS, VIS_PERIOD, NPER_MOON, PCNT_LOST
      REAL DAYS_MOONC, DAYS_TOTAL
      DOUBLE PRECISION MJD_MOONC(2)
      CHARACTER*9 CHAR_VIS(2,2), WARNING*1

* ______________________________ Executable Code _______________________________

      FIELD_NO=DBS_FIELDNO(REF_NO,'COORD.OBSERVATION')
      LVAL1=DBS_GETL(REF_NO,FIELD_NO)

      FIELD_NO=DBS_FIELDNO(REF_NO,'MONITOR')
      LVAL2=DBS_GETL(REF_NO,FIELD_NO)

      FIELD_NO=DBS_FIELDNO(REF_NO,'CONTIGUOUS.OBS')
      LVAL3=DBS_GETL(REF_NO,FIELD_NO)

      FIELD_NO=DBS_FIELDNO(REF_NO,'PHASE.DEPENDENT')
      LVAL4=DBS_GETL(REF_NO,FIELD_NO)

      IF((.NOT.LVAL1).AND.(.NOT.LVAL2).AND.
     &  (.NOT.LVAL3).AND.(.NOT.LVAL4))THEN
         FIELD_NO=DBS_FIELDNO(REF_NO,'COORD.OBSERVATION')
         MESSAGE='no constraint set'
         GO TO 10
      END IF

      IF(LVAL1)THEN

         FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
         START_YEAR=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'START.MONTH')
         START_MONTH=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'START.DAY')
         START_DAY=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'START.HOUR')
         START_HOUR=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'START.MINUTE')
         START_MINUTE=DBS_GETR(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'END.YEAR')
         END_YEAR=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'END.MONTH')
         END_MONTH=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'END.DAY')
         END_DAY=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'END.HOUR')
         END_HOUR=DBS_GETI(REF_NO,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_NO,'END.MINUTE')
         END_MINUTE=DBS_GETR(REF_NO,FIELD_NO)

         CALL SLA_CALDJ(START_YEAR,START_MONTH,START_DAY,MJD1,ERRF1)

         CALL SLA_CALDJ(END_YEAR,END_MONTH,END_DAY,MJD2,ERRF2)

         CALL SLA_CTF2D(START_HOUR,START_MINUTE,0.0,FMJD1,ERRF3)

         CALL SLA_CTF2D(END_HOUR,END_MINUTE,0.0,FMJD2,ERRF4)

         IF(ERRF1.EQ.1)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
            MESSAGE='Start year'
            GO TO 10
         ELSE IF(ERRF1.EQ.2)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.MONTH')
            MESSAGE='Start month'
            GO TO 10
         ELSE IF(ERRF1.EQ.3)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.DAY')
            MESSAGE='Start day'
            GO TO 10
         END IF

         IF(ERRF2.EQ.1)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'END.YEAR')
            MESSAGE='End year'
            GO TO 10
         ELSE IF(ERRF2.EQ.2)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'END.MONTH')
            MESSAGE='End month'
            GO TO 10
         ELSE IF(ERRF2.EQ.3)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'END.DAY')
            MESSAGE='End day'
            GO TO 10
         END IF
     
         IF(ERRF3.EQ.1)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.HOUR')
            MESSAGE='Start hour'
            GO TO 10
         ELSE IF(ERRF3.EQ.2)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.MIN')
            MESSAGE='Start minute'
            GO TO 10
         END IF
     
         IF(ERRF4.EQ.1)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'END.HOUR')
            MESSAGE='End hour'
            GO TO 10
         ELSE IF(ERRF4.EQ.2)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'END.MINUTE')
            MESSAGE='End minute'
            GO TO 10
         END IF

         MJD1=MJD1+FMJD1
         MJD2=MJD2+FMJD2
         IF(MJD2.LT.MJD1)THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
            MESSAGE='End before start time'
            GO TO 10
         END IF	

* check within AO
         IF (MJD1 .LT. AO_MJD0) THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
            MESSAGE='Start time before AO start'
            GO TO 10
         END IF	

         IF (MJD2 .GT. AO_MJD0 + AO_PERIOD + AO_DELAY) THEN
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
            MESSAGE='End time after AO end'
            GO TO 10
         END IF	

* check visibility

         CALL DBS_HIDDEND(REF_NO,'TARGET.RA',DRA)
         CALL DBS_HIDDEND(REF_NO,'TARGET.DEC',DDEC)
         CALL ROS_VIEW(DRA,DDEC, NVIS, MJD_VIS, CHAR_VIS)		! check sun-constraint
         VIS_PERIOD = 0
         DO IVIS = 1,NVIS
            IF ( MJD1 .GE. MJD_VIS(1,IVIS) .AND. MJD2 .LE. MJD_VIS(2,IVIS) ) VIS_PERIOD = IVIS
         END DO
         IF (VIS_PERIOD .EQ. 0 ) THEN					! select nearest for display
            DO IVIS = 1,NVIS
               MID_VIS = ( MJD_VIS(1,IVIS) + MJD_VIS(2,IVIS) ) / 2.D0
               DIFF_VIS(IVIS) = ABS(MJD1 - MID_VIS)
            END DO
            IVIS = 1
            IF (NVIS.EQ.2 .AND. DIFF_VIS(2) .LT. DIFF_VIS(1) ) IVIS = 2
            WRITE(MESSAGE, '(A)' ) 'Visible '//CHAR_VIS(1,IVIS)//' to '//CHAR_VIS(2,IVIS)
            FIELD_NO=DBS_FIELDNO(REF_NO,'START.YEAR')
            GOTO 10
         END IF

         CALL MAN_MOONC(MJD1, MJD2, DRA, DDEC, 0.2518, 1, MJD_MOONC, NPER_MOON )
         IF (NPER_MOON .EQ.1) THEN					! Moon prevents some of chosen slot
            DAYS_MOONC = MJD_MOONC(2) - MJD_MOONC(1)
            DAYS_TOTAL = MJD2 - MJD1
            PCNT_LOST = 100.0 * DAYS_MOONC / DAYS_TOTAL 
            IF (PCNT_LOST .GT. 25 ) THEN
               WRITE( MESSAGE,'(A,I3,A)' ) 'Moon may stop ', PCNT_LOST, ' percent!'
5              CONTINUE							! until acceptable reply
               WARNING = CON_CHECK_WARN( MESID, MESSAGE )
               IF (WARNING .EQ.'R' .OR. WARNING .EQ.'r') THEN
                  XTYPE = 'R'
                  QUAL_TARGET(QTARGET) = .FALSE.
                  GOTO 20
               ELSE IF (WARNING .NE. 'A' .AND. WARNING .NE. 'a') THEN
                  GOTO 5
               END IF
            END IF
         END IF

      END IF

      IF(LVAL2)THEN

         FIELD_NO=DBS_FIELDNO(REF_NO,'TIME.INTERVAL')
         RVAL=DBS_GETR(REF_NO,FIELD_NO)
         IF(RVAL.LE.0.0)THEN
            MESSAGE='Time interval'
            GO TO 10
         END IF


      END IF

      IF(LVAL3)THEN

         FIELD_NO=DBS_FIELDNO(REF_NO,'NUMBER.INTERVALS')
         IVAL=DBS_GETI(REF_NO,FIELD_NO)
         IF(IVAL.LE.0.0)THEN
            MESSAGE='Number intervals'
            GO TO 10
         END IF

      END IF

      IF(LVAL4)THEN						! Phase dependent

         FIELD_NO=DBS_FIELDNO(REF_NO,'EPOCH')
         RVAL=DBS_GETR(REF_NO,FIELD_NO)
         IF(RVAL.LE.0.0)THEN
            MESSAGE='Phase dependent - epoch'
            GO TO 10
         END IF

         FIELD_NO=DBS_FIELDNO(REF_NO,'PERIOD')
         RVAL=DBS_GETR(REF_NO,FIELD_NO)
         IF(RVAL.LE.0.0 .OR. RVAL .GT.180)THEN
            MESSAGE='Phase dependent - period'
            GO TO 10
         END IF

      END IF

*  No errors found ( if constraint was set then flag was set .F. in Target check
      FIELD_NO = 1
      IF (CON_RECHECK_TARG(REF_NO)) THEN
         QUAL_TARGET(QTARGET) = .TRUE.
      ELSE
         QUAL_TARGET(QTARGET) = .FALSE.
      END IF
      GOTO 20
 
10    CONTINUE
      XTYPE = CON_CHECK_ERR(MESID,MESSAGE)
      QUAL_TARGET(QTARGET) = .FALSE.
20    CONTINUE

      END
