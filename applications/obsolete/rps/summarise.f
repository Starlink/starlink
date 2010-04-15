*+SUMMARISE        Produses a summary of the Proposal for lineprinter o/p
*  Aug 1992	M. Duesterhaus 	Remove VAX specific code
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
**************************************************************************
      SUBROUTINE SUMMARISE(DISPLAY,LUN)
      IMPLICIT NONE

*   Input :
      LOGICAL DISPLAY			! True for Screen display

      INTEGER LUN				! If SMG gives Vunit
						! if .not. DISPLAY gives print unit

*  Global Variables
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_points.inc'		! Gives Constraints field
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_target.inc'
      INCLUDE 'zpidata.inc'
      DOUBLE PRECISION DECLONG, DECLAT
      COMMON / ECL_COORD/ DECLONG, DECLAT	! Available from ROS_VIEW
      LOGICAL SMG
      COMMON / SMG_KEEP / SMG

*   Functions :
      INTEGER DBS_FIELDNO					!Gets field number from the database.
      INTEGER DBS_GETI						!Gets integer value from the database.
      CHARACTER*60 DBS_GETC					!Gets character value from the database.
      REAL DBS_GETR						!Gets real value from the database.
      INTEGER MDH_ENDWORD					!Used to check for blank character variables.
      INTEGER FORM_GECHEK
      LOGICAL DBS_GETL
      REAL WFC_SENS, HRI_SENS, WFC_SURV, XRT_SURV				! Sensitivity, survey exposure
*-
*   Local :
      INTEGER FIELD_NO
      DOUBLE PRECISION DECRADS					!DEC in radians.
      DOUBLE PRECISION RARADS					!RA in radians.
      CHARACTER*11 TARG_RA, TARG_DEC				! RA, dec in characters.
      CHARACTER*20 TOTOBS*3, CODES*3, NOBS*2
      INTEGER HRI, PSPC, WFC
      CHARACTER*3 WFC_FILTER(8)					!Filter use order.
      CHARACTER*5 PSPC_FILTER(2)				!Filter use order.
      CHARACTER*3 WFC_PERCENT(8)				!Filter use percent of time.
      CHARACTER*3 PSPC_PERCENT(2)				!Filter use percent of time.
      INTEGER I							!Array counter.
      INTEGER IERR						!Error flag.
      INTEGER NCHAR						!Number of characters in a string.
      INTEGER LSTATUS
      DOUBLE PRECISION MJD_VIS(2,2)					! Possible dates visible
      CHARACTER*9 DSTRING(2,2)					!	::
      INTEGER NVIS_PERIODS
      REAL SURVEXP, SURVXRT
      LOGICAL ZOOM_ON
      LOGICAL LVAL1,LVAL2,LVAL3,LVAL4				!Set to TRUE on selection of a given constraint.
      CHARACTER*4 CON_TYPE					! Type of constraint
      CHARACTER*20 FLD, NUM*2, QAL*2, RECNUM*4, DETECT*5, SENS*11, ZOOM*1, TF*5
      REAL TFILT(8), WFC_MIN(8), HRI_MIN, PSPC_MIN(2), PTFILT(2), SSEC, AKSEC
      INTEGER PCT
      INTEGER ITARGET, NTARGS, NFILT_WFC, NFILT_PSPC, ILINE, NLINE, IPSPC
      INTEGER KSEC, NHRI, REF_GECHEK
      CHARACTER*127 LINE, TARG_HEAD(2)*97, BLANK*62, RULE*62
      CHARACTER*28 TARG_REM(2)
      DATA TARG_HEAD/
     &' Rec   Target       Targ     Direction, J2000.0        Sun constraint   No.      Time 2*Srv.exp  ',
     &' No     Name         No Qual R.A.        Dec          Start      End    Obs  TC  Ksec XRT   WFC  '/
      DATA TARG_REM/'     Filter Data         Det','  Instr Flt   %    sec   cps'/
      DATA BLANK/'                                                              '/
      DATA RULE /'--------------------------------------------------------------'/

*  __________________________ Executable Code __________________________________

      IF (REF_FORM .LE.0 ) THEN							! Open files if necessary
         CALL FORM_OPEN( 'R', IERR)
         IF (IERR .NE. 0) GOTO 90
         CALL FORM_READ(REF_FORM,1,IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF

      REF_GECHEK = FORM_GECHEK(REF_FORM)					! Set cover qual
C         QUAL_COVER = .FALSE.
C      ELSE
C         QUAL_COVER = .TRUE.
C      END IF

      FIELD_NO = DBS_FIELDNO(REF_FORM,'PROPOSAL.TITLE(1)' )
      LINE = ' Proposal: '// DBS_GETC(REF_FORM, FIELD_NO)
      NCHAR = MDH_ENDWORD(LINE)
      IF (DISPLAY) THEN
            WRITE( * ,'(A)') LINE
            IF (REF_GECHEK .LT. 0) WRITE( *, '(A)' ) ' Error - Cover not correct: '
            WRITE( * ,'(A)' ) RULE//RULE
            WRITE( * ,'(A/A)') TARG_HEAD(1)//TARG_REM(1),TARG_HEAD(2)//TARG_REM(2)			! Target heading
      ELSE
         WRITE(LUN,'(A)') LINE
         IF (REF_GECHEK .LT. 0) WRITE( LUN, '(A)' ) ' Error - Cover not correct: '
         WRITE(LUN,'(A/A)') RULE//RULE, TARG_HEAD							! Target heading
      END IF

      NTARGS = DBS_GETI(REF_FORM,FLD_NTARGETS)
      DO ITARGET = 1,NTARGS
         CALL TARG_READ(ITARGET,IERR)
         IF (IERR.NE.0) GOTO 90

      FIELD_NO=DBS_FIELDNO(REF_TARGET,'TARGET.RA')
      TARG_RA=DBS_GETC(REF_TARGET,FIELD_NO)
      CALL RA_CONVERT(TARG_RA, RARADS, LSTATUS)

      FIELD_NO=DBS_FIELDNO(REF_TARGET,'TARGET.DEC')
      TARG_DEC =DBS_GETC(REF_TARGET,FIELD_NO)
      CALL DEC_CONVERT(TARG_DEC, DECRADS, LSTATUS)

      CALL ROS_VIEW(RARADS, DECRADS, NVIS_PERIODS, MJD_VIS, DSTRING)
      SURVEXP =  WFC_SURV(DECLAT ) / 500.0					! want 2*
      SURVXRT =  XRT_SURV(DECLAT ) / 1000.0					! table has 2*
      WRITE(SENS,'(F5.1,1X,F5.1)') SURVXRT, SURVEXP

      FIELD_NO=DBS_FIELDNO(REF_TARGET,'TOTAL.OBS.TIME')
      AKSEC = DBS_GETR(REF_TARGET,FIELD_NO)
      KSEC = INT( AKSEC)
      SSEC = AKSEC * 1000.
      WRITE(TOTOBS,'(I3)') KSEC

      FIELD_NO=DBS_FIELDNO(REF_TARGET,'NUMBER.OBS')
      NOBS = DBS_GETC(REF_TARGET, FIELD_NO)

      IF (DBS_GETL(REF_TARGET, FLD_CONSTRAINTS) ) THEN			! Time constraint set

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'COORD.OBSERVATION')
         LVAL1=DBS_GETL(REF_TARGET,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'MONITOR')
         LVAL2=DBS_GETL(REF_TARGET,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'CONTIGUOUS.OBS')
         LVAL3=DBS_GETL(REF_TARGET,FIELD_NO)

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'PHASE.DEPENDENT')
         LVAL4=DBS_GETL(REF_TARGET,FIELD_NO)

         IF (LVAL1) THEN
            Con_type = 'Cord'
         ELSE IF (LVAL2) THEN
            CON_TYPE = 'Mon '
         ELSE IF (LVAL3) THEN
            CON_TYPE = 'Cntg'
         ELSE IF (LVAL4) THEN
            CON_TYPE = 'Phas'
         ELSE
            CON_TYPE = ' ?  '
         END IF
      ELSE
         CON_TYPE = 'None'
      END IF

      NFILT_WFC = 0
        DO I=1,8
         WRITE(FLD, '(A,I1,A)' ) 'WFC.FILT.CODE(', I, ')'
         FIELD_NO=DBS_FIELDNO(REF_TARGET,FLD)
	 WFC_FILTER(I) = DBS_GETC(REF_TARGET,FIELD_NO)
	 CALL UPC (WFC_FILTER(I))
         NCHAR = MDH_ENDWORD( WFC_FILTER(I) )
*         WFC_FILTER(I) = WFC_FILTER(I) //'   '
         IF (NCHAR .GT. 0) THEN
            NFILT_WFC = I
            WRITE(FLD(10:13), '(A)') 'PCNT'
            FIELD_NO = DBS_FIELDNO(REF_TARGET,FLD)
            WFC_PERCENT(I) = DBS_GETC(REF_TARGET,FIELD_NO)
            READ(WFC_PERCENT(I), * ) PCT
            TFILT(I) = REAL(PCT) * AKSEC * 10.				! % * 1000 / 100
            WFC_MIN(I) = WFC_SENS(5.0,0.01,TFILT(I),0.7,10.0)
         END IF
        END DO

      HRI_MIN = HRI_SENS(5.0, 0.000013, SSEC)

      WRITE(NUM ,'(I2)') TARG_NO(ITARGET)
      IF (QUAL_TARGET(ITARGET)) THEN
         WRITE(QAL,'(A)') 'OK'
      ELSE
         WRITE(QAL,'(A)') 'F'
      END IF
      RECNUM = '    '
      WRITE(RECNUM(2:3), '(I2)') ITARGET

      WRITE(LINE,'(A)') RECNUM//TARG_NAME(ITARGET)(:16)//' '//NUM//'  '//QAL//' '//TARG_RA//' '//TARG_DEC//' '//
     &   DSTRING(1,1)//' '//DSTRING(2,1)//' '//NOBS//' '// CON_TYPE //' '//TOTOBS//
     &   ' '//SENS

* rest of filters
      NLINE = MAX( NFILT_WFC + 1, NVIS_PERIODS)
      DO ILINE = 1,NLINE
         IF (ILINE.GT.1) THEN
           LINE = BLANK // BLANK
           IF (ILINE .EQ. NVIS_PERIODS) THEN
              LINE(53:71) = DSTRING(1,2) // ' ' // DSTRING(2,2)
           END IF
         END IF
         IF (ILINE .LE. NFILT_WFC ) THEN
            LINE(101:114) = 'WFC  ' // WFC_FILTER(ILINE) // '   '// WFC_PERCENT(ILINE)
            IF (TFILT(ILINE) .GT. 99999. ) THEN
               WRITE(TF,'(I4,A1)') INT( TFILT(ILINE)/1000. ), 'k'
            ELSE IF (TFILT(ILINE) .GT. 9999. ) THEN
               WRITE(TF,'(F4.1,A1)') TFILT(ILINE)/1000., 'k'
            ELSE
               WRITE(TF, '(F5.0)') TFILT(ILINE)
            END IF
            WRITE(DETECT,'(F5.3)') WFC_MIN(ILINE) 		! cts / sec
            LINE(116:126) = TF//' '//DETECT

         ELSE IF (ILINE .LE. NFILT_WFC + 1) THEN

            LINE(101:103) = 'HRI'
            IF (SSEC .GT. 99999. ) THEN
               WRITE(TF,'(I4,A1)') KSEC, 'k'
            ELSE IF (SSEC .GT. 9999. ) THEN
               WRITE(TF,'(F4.1,A1)') AKSEC, 'k'
            ELSE
               WRITE(TF, '(F5.0)' ) SSEC
            END IF
            WRITE(DETECT, '(F5.4)') HRI_MIN
            LINE(116:126) = TF //' '// DETECT

         END IF

         IF (DISPLAY) THEN
            WRITE( * ,'(A)') LINE
         ELSE
            WRITE(LUN,'(A)') LINE
         END IF
      END DO							! End filter lines for each target

      END DO							! End loop for each target
90    CONTINUE
      END
