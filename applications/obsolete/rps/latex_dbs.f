*+LATEX_DBS        Makes up strings, writes to .tex file
      SUBROUTINE LATEX_DBS(IPAGE,LUN_OUT)

*  Type Declaration
      IMPLICIT NONE

*  Calling Arguments
      INTEGER IPAGE
      INTEGER LUN_OUT

*  Global Variables
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_files.inc'

      INTEGER CHECKSUM
      COMMON /KEEPCHECK/ CHECKSUM
*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1989 Jan				Mods for split form, skipping unused filter slots
*     1992 Aug		M. Duesterhaus  Remove VAX specific code
*     1994 Jan		M Ricketts	RAL version
*******************************************************************************
*-
*  Functions
      INTEGER DBS_INFOI, FIND_FIRST
      INTEGER MDH_ENDWORD, MDH_CTOI
      CHARACTER*5 MDH_ITOC
      CHARACTER*60 DBS_GETC
      CHARACTER*50 DBS_INFOC

*  Local Parameters
      CHARACTER*5  S1/'put('/
      CHARACTER*24 S2/'makebox(0,0)[bl]{'/
      CHARACTER*2  S3/'}}'/

*  Local Variables
      INTEGER IFILE, NFIELD, REF_NO
      INTEGER I, NULSIZE, UNITSIZE, LENGTH, LENS
      INTEGER STATUS, START, ITEST, LOC, SUBINDEX, SKIPNEXT
      CHARACTER*128 LINE
      CHARACTER*128 STRING
      CHARACTER*19 FIELD
      CHARACTER*8 UNITS
      CHARACTER*6 SX,SY
      LOGICAL PSPC_FILT(2), WFC_FILT(8)		! Show if filter slot used
      data pspc_filt,wfc_filt/10*.true./
      INTEGER IFILT				! Filter number

*  Executable Code

      SKIPNEXT = 0
      DO IFILE = 1,3				! Go thru all fields, both files
       IF (IFILE .EQ.1) THEN
         NFIELD = NFIELD_FORM
         REF_NO = REF_FORM
       ELSE IF (IFILE .EQ. 2) THEN
         NFIELD = NFIELD_TARGET
         REF_NO = REF_TARGET
       END IF

       DO I=1,NFIELD
         IF (SKIPNEXT .GT. 0 ) THEN
            SKIPNEXT = SKIPNEXT - 1
         ELSE
            IF (PR_ON(I,IPAGE,IFILE)) THEN
               FIELD = DBS_INFOC(REF_NO, I, 'FIELDNAME')
               IF (FIELD(1:1) .EQ.'$' ) FIELD = FIELD(2:)

               NULSIZE = DBS_INFOI(REF_NO, I, 'NULLENGTH' )		! Asci length string
               UNITS = DBS_INFOC( REF_NO , I, 'UNITS' )
               UNITSIZE = MDH_ENDWORD(UNITS)
               STRING = DBS_GETC(REF_NO,I)
               LENS = MDH_ENDWORD(STRING)

               IF (FIELD(:9) .EQ. 'PSPC.FILT' ) THEN			! If PSPC Filter data
                  READ ( FIELD(16:16), '(I1)' ) IFILT
                  IF ( FIELD(11:14) .EQ. 'CODE' ) THEN			! See if this filter used
                     IF ( LENS.EQ.0 ) THEN
                        PSPC_FILT(IFILT) = .FALSE.
                     ELSE
                        PSPC_FILT(IFILT) = .TRUE.
                     END IF
                  END IF
                  IF (.NOT.PSPC_FILT(IFILT) ) GOTO 50			! Go to end loop as not writing data
               END IF

               IF (FIELD(:8) .EQ. 'WFC.FILT' ) THEN			! If WFC filter data
                  READ ( FIELD(15:15), '(I1)' ) IFILT
                  IF ( FIELD(10:13) .EQ. 'CODE' ) THEN			! See if this filter used
                     IF ( LENS.EQ.0 ) THEN
                        WFC_FILT(IFILT) = .FALSE.
                     ELSE
                        WFC_FILT(IFILT) = .TRUE.
                     END IF
                  END IF
                  IF (.NOT.WFC_FILT(IFILT) ) GOTO 50			! Go to end loop as not writing data
               END IF

               STRING = STRING(:LENS) // ' '// UNITS(:UNITSIZE)
               LENS = MDH_ENDWORD(STRING)

               START = 1
               ITEST = 1
               DO WHILE (ITEST .NE. 0)
                  ITEST = FIND_FIRST(STRING(START:LENS),
     &               '$&%#_{}',SUBINDEX  )
                  IF (ITEST .NE. 0) THEN
                     LOC = START + ITEST - 1
                     STRING = STRING(:LOC-1) // BSLASH // STRING(LOC:LENS)
                     LENS = LENS + 1
                     START = LOC + 2
                  END IF
               END DO

               IF (FIELD.EQ.'SUBJECT.CATEGORY') THEN			! Separate 'IF's prevents print for page = 2
                  IF (IPAGE.EQ.1) THEN
                     CALL LTX_CATEGORY(I,IFILE,STRING(:NULSIZE),SX,SY,STATUS)
                     STRING = 'X'
                     STATUS = 0						! So we don't get the normal print
                     LENS = 1
                  ELSE
                     STATUS = -1
                  END IF

c               ELSE IF (FIELD .EQ.'TARGET.RA'. OR .FIELD. EQ.'TARGET.DEC')THEN	! redundant
c                  IF(IPAGE.EQ.3)THEN
c                     CALL LTX_WRITE_RADEC(IFILE,FIELD,LUN_OUT,I)
c                  END IF
c                  STATUS=-1

C               ELSE IF (FIELD.EQ.'COORD.OBSERVATION' .OR. FIELD.EQ.'MONITOR'
C     &            .OR.  FIELD.EQ.'CONTIGUOUS.OBS'
C     &            .OR. FIELD.EQ.'PHASE.DEPENDENT' ) THEN
C
C                  IF (IPAGE.EQ.4) THEN
C                     CALL LTX_CONSTR_BOX(I,IFILE,FIELD,STRING(:1),SX,SY,SKIPNEXT)
C                     STATUS = 0
C                  ELSE
C                     STATUS = -1
C                  END IF
C
               ELSE
                  STATUS = 0
                  SX = XSTART(I,IPAGE,IFILE)
                  SY = YSTART(I,IPAGE,IFILE)
               END IF
               IF (STATUS.EQ.0) THEN
                  LINE = BSLASH//S1//SX//','//SY//'){'//BSLASH//S2
     &              //BSLASH//'tt '//STRING(:LENS)//S3
                  LENGTH = MDH_ENDWORD(LINE)
                  WRITE(LUN_OUT,'(A)') LINE(:LENGTH)
               END IF
            END IF
         END IF

50     CONTINUE
       END DO					! for each field
      END DO				! For cover, target files

      IF (IPAGE.EQ.1) CALL LTX_ABSTRACT(LUN_OUT,STATUS)		! Position fixed - see subroutine

      END
