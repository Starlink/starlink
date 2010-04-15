*+TARGET_SUMMARY        Produces a summary of the Proposal for lineprinter o/p
*  Aug 1992 	M. Duesterhaus	Remove VAX specific code
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1994 JAN		M Ricketts	Mods for AO5
*************************************************************************
      SUBROUTINE TARGET_SUMMARY(LUN_OUT,BSLASH,MORE_SUMMARY, CON_COUNT,
     &           REM_COUNT)
      IMPLICIT NONE

*   Input :
      INTEGER LUN_OUT, CON_COUNT, REM_COUNT
      LOGICAL MORE_SUMMARY			! False on 1st call, then true if target list fills more than one page
      CHARACTER*1 BSLASH

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
      LOGICAL DBS_GETL
      INTEGER FIND_FIRST
*-
*   Local :
      INTEGER FIELD_NO, I, IERR, LSTATUS, START, LENS, ITEST, FIRST_TARGET
      INTEGER SUBINDEX,LOC, wfc, HRI, IPERCENT, NLINE, NEXT_TARGET, NCHAR, NWFILT
      common /keep_targ/ NEXT_TARGET
      INTEGER ITARGET, NTARGS, KSEC, NHRI, LINELOC
      INTEGER JTARGET, TARG_NUM
      LOGICAL ZOOM_ON, REMARKS
      LOGICAL LVAL1,LVAL2,LVAL3,LVAL4				!Set to TRUE on selection of a given constraint.
      REAL AKSEC, MINTIME
      DOUBLE PRECISION DECRADS					!DEC in radians.
      DOUBLE PRECISION RARADS					!RA in radians.
      CHARACTER*11 TARG_RA, TARG_DEC				! RA, dec in characters.
      CHARACTER*20 TOTOBS*3, NOBS*2, YTEXT*3
      CHARACTER*3 WFC_FILTER(8)					!Filter use order.
      CHARACTER*12 WSTRING(8), WFC_CODE*1, PRIME_INSTR*1
      CHARACTER*1 CON_TYPE*4, REQ*10 			        ! Type of constraint
      CHARACTER*20 FLD, NUM*2, TTARG_NAME

*  __________________________ Executable Code __________________________________

      IF (REF_FORM .LE.0 ) THEN							! Open files if necessary
         CALL FORM_OPEN( 'R', IERR)
         IF (IERR .NE. 0) GOTO 90
         CALL FORM_READ(REF_FORM,1,IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF

      FIELD_NO = DBS_FIELDNO(REF_FORM,'PROPOSAL.TITLE(1)' )

      NTARGS = DBS_GETI(REF_FORM,FLD_NTARGETS)
      lineloc = 198
      IF (MORE_SUMMARY) THEN
         FIRST_TARGET = NEXT_TARGET
      ELSE
         FIRST_TARGET = 1
         CON_COUNT = 0
         REM_COUNT = 0
      END IF

      DO ITARGET = FIRST_TARGET, NTARGS

         DO JTARGET = 1,NTARGS
           CALL TARG_READ(JTARGET,IERR)
           IF (IERR.NE.0) GOTO 90
           TARG_NUM = DBS_GETI(REF_TARGET,1)

           IF (TARG_NUM.EQ.ITARGET) THEN
             GOTO 60
           END IF
         END DO

         GOTO 90

 60      CONTINUE

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'TARGET.RA')
         TARG_RA=DBS_GETC(REF_TARGET,FIELD_NO)
         CALL RA_CONVERT(TARG_RA, RARADS, LSTATUS)

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'TARGET.DEC')
         TARG_DEC =DBS_GETC(REF_TARGET,FIELD_NO)
         CALL DEC_CONVERT(TARG_DEC, DECRADS, LSTATUS)

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'TOTAL.OBS.TIME')
         AKSEC = DBS_GETR(REF_TARGET,FIELD_NO)
         KSEC = INT( AKSEC)
         WRITE(TOTOBS,'(I3)') KSEC

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'NUMBER.OBS')
         NOBS = DBS_GETC(REF_TARGET, FIELD_NO)
         FIELD_NO=DBS_FIELDNO(REF_TARGET,'HRI.CODE')
         HRI = DBS_GETI(REF_TARGET,FIELD_NO)
         IF (HRI .GT. 0 ) THEN
            NHRI = 1
         ELSE
            NHRI = 0
         END IF

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'WFC.CODE')
         WFC = DBS_GETI(REF_TARGET,FIELD_NO)
         WRITE(WFC_CODE,'(I1)' ) WFC

         PRIME_INSTR = '?'
         IF (HRI .EQ. 1)  PRIME_INSTR = 'H'
         IF (WFC .EQ. 1)  PRIME_INSTR = 'W'

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'WFC.ZOOM.ON')
         ZOOM_ON = DBS_GETL(REF_TARGET, FIELD_NO)

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
               CON_TYPE = 'Y'
            END IF
            CON_COUNT= CON_COUNT +1
         ELSE
            CON_TYPE = 'N'
         END IF
         NWFILT = 0
         DO I=1,8
            WRITE(FLD, '(A,I1,A)' ) 'WFC.FILT.CODE(', I, ')'
            FIELD_NO=DBS_FIELDNO(REF_TARGET,FLD)
            WFC_FILTER(I) = DBS_GETC(REF_TARGET,FIELD_NO)
            call upc( WFC_FILTER(I))
            NCHAR = MDH_ENDWORD( WFC_FILTER(I) )

            IF (NCHAR .GT. 0) THEN
               NWFILT = I
               WRITE(FLD, '(A,I1,A)' ) 'WFC.FILT.PCNT(', I, ')'
               FIELD_NO=DBS_FIELDNO(REF_TARGET,FLD)
               IPERCENT = DBS_GETI(REF_TARGET, FIELD_NO)
               WRITE(FLD, '(A,I1,A)' ) 'WFC.FILT.MINT(', I, ')'
               FIELD_NO=DBS_FIELDNO(REF_TARGET,FLD)
               MINTIME = DBS_GETR(REF_TARGET, FIELD_NO)
               WRITE(WSTRING(I),'(A2,I4,F5.1)') WFC_FILTER(I)(:2), IPERCENT, MINTIME
            END IF
         END DO
         NLINE = NWFILT									! number of filters used
         IF (LINELOC - NLINE*6 .LT. 10) THEN						!end this page if not enough space
            NEXT_TARGET = ITARGET
            MORE_SUMMARY = .TRUE.
            GOTO 99
         END IF

         FIELD_NO=DBS_FIELDNO(REF_TARGET,'REMARKS(1)')
         REQ = DBS_GETC(REF_TARGET, FIELD_NO)
         IF (MDH_ENDWORD(REQ) .GT. 0) THEN
            REMARKS = .TRUE.
            REM_COUNT = REM_COUNT+1
         ELSE
            REMARKS = .FALSE.
         END IF

         WRITE(NUM ,'(I2)') TARG_NO(JTARGET)

         LENS = 20
         START = 1
         ITEST = 1
         TTARG_NAME = TARG_NAME(JTARGET)

         DO WHILE (ITEST .NE. 0)
            ITEST = FIND_FIRST(TTARG_NAME(START:LENS),
     &               '$&%#_{}',SUBINDEX  )
            IF (ITEST .NE. 0) THEN
               LOC = START + ITEST - 1
               TTARG_NAME= TTARG_NAME(:LOC-1) // BSLASH
     &         // TTARG_NAME(LOC:LENS)
               LENS = LENS + 1
               START = LOC + 2
            END IF
         END DO


* write to latex file

         LINELOC = LINELOC - 6
         WRITE(YTEXT,'(I3)' ) LINELOC
         WRITE(LUN_OUT,'(A)') BSLASH//'put(25,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tr]{'//BSLASH//'tt '// NUM //'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(27,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// TTARG_NAME//'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(68,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// TARG_RA(:8) //'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(103,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tr]{'//BSLASH//'tt '// TARG_DEC(:9) //'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(112,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tr]{'//BSLASH//'tt '// TOTOBS //'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(118,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tr]{'//BSLASH//'tt '// NOBS //'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(122,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// CON_TYPE //'}}'

         WRITE(LUN_OUT,'(A)') BSLASH//'put(134,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// PRIME_INSTR//'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(144,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(1)(:2)//'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(153,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(1)(4:6)//'}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(162,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(1)(8:12)//'}}'
         IF (ZOOM_ON) THEN
            WRITE(LUN_OUT,'(A)') BSLASH//'put(172,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt Yes}}'
         ELSE
            WRITE(LUN_OUT,'(A)') BSLASH//'put(172,'// YTEXT //'){'//BSLASH//
     &	'makebox(0,0)[tl]{'//BSLASH//'tt No}}'
         END IF
         IF (REMARKS) THEN
            WRITE(LUN_OUT,'(A)') BSLASH//'put(182.5,'// YTEXT //'){'//BSLASH//
     &	    'makebox(0,0)[tl]{'//BSLASH//'tt Yes}}'
         ELSE
            WRITE(LUN_OUT,'(A)') BSLASH//'put(182.5,'// YTEXT //'){'//BSLASH//
     &	    'makebox(0,0)[tl]{'//BSLASH//'tt No}}'
         END IF

         IF (NLINE .GT.1) THEN
            DO I=2, NLINE
               LINELOC = LINELOC - 6
               WRITE(YTEXT,'(I3)' ) LINELOC
               IF (NWFILT.GE.I) THEN
                  WRITE(LUN_OUT,'(A)') BSLASH//'put(144,'// YTEXT //'){'//BSLASH//
     &			'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(I)(:2)//'}}'
                  WRITE(LUN_OUT,'(A)') BSLASH//'put(153,'// YTEXT //'){'//BSLASH//
     &			'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(I)(4:6)//'}}'
                  WRITE(LUN_OUT,'(A)') BSLASH//'put(162,'// YTEXT //'){'//BSLASH//
     &			'makebox(0,0)[tl]{'//BSLASH//'tt '// WSTRING(I)(8:12)//'}}'
               END IF
            END DO
         END IF

      END DO							! End loop for each target
90    CONTINUE

      MORE_SUMMARY = .FALSE.
99    CONTINUE
      END
