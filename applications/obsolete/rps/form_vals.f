*+FORM_VALS        Does Form Data Entry
      SUBROUTINE FORM_VALS(FORM_PART,RUN_TYPE,MESID,HDID,TITLE,XTYPE)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) FORM_PART   ! Indicates form section
      CHARACTER*(*) TITLE	! Title of form.
      INTEGER       HDID	! Header ID.
     & ,            MESID	! Message ID.
      CHARACTER*(*) RUN_TYPE	! NEW, OLD, or DEF (get defaults for name etc.)
      CHARACTER*1 XTYPE		! Q(uit) or Exit - more targets or not

********************************************************************************
*  History
*     1987	Mark Harris	1st Version
*     1988	M Bush		Mods for Consistency check in separate routines
*     1988 Oct	M Ricketts	Passing In RUN_TYPE
*          Dec    ::		Take defaults from record if 'OLD'
*     1992 APR  M. DUESTERHAUS  REMOVE VAX RTL CALLS
*     1993 June         P. Brisco       Got rid of SMG stuff.
*     1993 July		P. Brisco	for the help file where the logical is
*					defined.
*     1994 Jan		M Ricketts	RAL version
********************************************************************************

*  Global Variables
      INTEGER PBID	! Pasteboard IDs. SMGLIB
     & ,      KPID	! Keypad ID.
      COMMON / SMG_PANDK / PBID , KPID

      LOGICAL SMG
      COMMON / SMG_KEEP / SMG
      INTEGER WIDTH,START_WIDTH
      LOGICAL COLS132
      COMMON /WIDTH_KEEP/ WIDTH,COLS132,START_WIDTH

      CHARACTER*8 HELPLIB
      COMMON / HELP_LIB_NAME / HELPLIB

      CHARACTER*48 MESF1/'Ctrl/Z > next form/exit, F2 for help            '/
      CHARACTER*48 MESF2/' <ret>, Cursor up/down changes field            '/
      INTEGER MESTART		! Mid point across screen
      COMMON/MES_FIELD/ MESF1,MESF2,MESTART

      INCLUDE 'com_form_data.inc'
      INCLUDE 'com_form_points.inc'
*-

*  Local Variables
      INTEGER LIMITS(2)		! Form section limits
      INTEGER KFILE		! 1 - 4: cover, gen, target, constr.
      INTEGER REFDB		! Database ref no.
      INTEGER       CONTINUE	! Indicates if more forms wanted.
      INTEGER IPOS
      INTEGER NFAIL		! counts number of check failures
      INTEGER FORM_FIELD	! Field at which to start Form input
      INTEGER START		! Start point for initialising target rec.
      CHARACTER*18 LIB_PART	! Supplies section name to help library
      LOGICAL LTEST		! Check wot's for
      INTEGER TARG_NO		! skip query if 1st target of new form

* We are'nt sure why cval and val both exist - rearrange elements of cval ?

      CHARACTER*60 CVAL(100,6) / 600 * ' ' / 	! Values of fields.
*     & ,           VAL(100)			! Temporary variable.
     & ,           CTEMP
      INTEGER      I 		 		! Loop variables.
     & ,           BEG			! Temporary variables.
D       & ,           HRI , PSP
     & ,           N				! Number of fields.
      INTEGER VECTOR_FIELD			! Equivalent of FORM_FIELD
D      LOGICAL      PASTED			! Dummy variable.
      INTEGER END_COP				! Last field to be copied from current record

*  Functions
      INTEGER FIND_NOT_BLANK
      INTEGER      DBS_FIELDNO
      CHARACTER*60 DBS_INFOC	! Gets character information.
      CHARACTER*60 DBS_GETC	! Gets character value
      LOGICAL      MDH_GETL

*  Subroutines
*     [MDH.DBS] DBS_PUTC			! Puts values into a record.
*    & ,        DBS_PUTL			! Puts logical values into a record.
*     [MDH.SMG] SMG_FORM			! Gets values via a form.
*     PACKAGE   FORM_CNVRT			! Converts angles to the correct format.
*    &  ,       FORM_INPUT			! Gets input while in line mode.

*-  Author M.D.C.Harris ( R.A.L )                    10th August 1987.
*  ________________________________  Executable Code  __________________________

      LIB_PART = FORM_PART //'         '					! Set up a name in case help required
      IF (LIB_PART(:3) .EQ.'GEN') THEN
         LIB_PART = 'GENERAL     '
      ELSE IF (LIB_PART(:6) .EQ. 'CONSTR' ) THEN
         LIB_PART = 'CONSTRAINTS '
      END IF

      IF (FORM_PART .EQ. 'COVER') THEN						! Get section-dependent parameters

         LIMITS(1) = FLD_LIMS_COVER(1)
         LIMITS(2) = FLD_LIMS_COVER(2)
         KFILE = 1
         REFDB = 1
      ELSE IF (FORM_PART .EQ. 'GEN') THEN
         LIMITS(1) = FLD_LIMS_GEN(1)
         LIMITS(2) = FLD_LIMS_GEN(2)
         KFILE = 2
         REFDB = 1
      ELSE IF (FORM_PART .EQ. 'TARGET') THEN
         LIMITS(1) = FLD_LIMS_TARG(1)
         LIMITS(2) = FLD_LIMS_TARG(2)
         KFILE = 3
         REFDB = 2
      ELSE IF (FORM_PART .EQ. 'CONSTR') THEN
         LIMITS(1) = FLD_LIMS_CONS(1)
         LIMITS(2) = FLD_LIMS_CONS(2)
         KFILE = 4
         REFDB = 2
      END IF
      I = 0
      MESTART = WIDTH - 50
      XTYPE = 'E'
      BEG = LIMITS( 1 ) - 1							! Initialise variables.
      N = LIMITS( 2 ) - BEG
      IF ( KFILE .LE. 2 ) THEN							! If 'Cover' , General pages

         IF (RUN_TYPE .EQ. 'NEW' ) THEN
            DO I = 1 , N							!  Initialise variables using defaults if needed.
*               VAL( I ) = CVAL( I,KFILE ) 					! but this hasn't been set?
               IF (CVAL(I,KFILE).EQ.' ') CVAL(I,KFILE) = DBS_INFOC(REFDB,I+BEG,'NULVALUE')
            END DO
         ELSE
            IF (RUN_TYPE.EQ.'DEF') THEN
               IF ( KFILE.EQ.1) THEN						 ! Cover defaults ( also Gen page?)
                  END_COP = DBS_FIELDNO(REFDB,'NUMBER.OF.TARGETS') - 1
               ELSE
                  END_COP = 0
               END IF
            ELSE
               END_COP = N
            END IF
            DO I = 1 , END_COP							! Set these fields from record
               CTEMP = DBS_GETC(REFDB,I+BEG)
               IPOS = MAX(FIND_NOT_BLANK(CTEMP) , 1 )		! Skip blanks
               CVAL(I,KFILE) = CTEMP(IPOS:)
*               VAL( I ) = CVAL( I,KFILE )
            END DO

            IF (END_COP .LT. N) THEN
               DO I = END_COP + 1 , N						!  Initialise variables using defaults if needed.
*                 VAL( I ) = CVAL( I,KFILE )
                  IF (CVAL(I,KFILE).EQ.' ') CVAL(I,KFILE) = DBS_INFOC(REFDB,I+BEG,'NULVALUE')
               END DO
            END IF
         END IF
      ELSE									! Other pages, target, constraints

        IF (RUN_TYPE .NE. 'OLD' ) THEN
           IF (KFILE .EQ.3 ) THEN						! Get target number only
              START = 2
              CTEMP = DBS_GETC(REFDB,1+BEG)
              IPOS = MAX(FIND_NOT_BLANK(CTEMP) , 1 )
              CVAL(1,KFILE) = CTEMP(IPOS:)
*              VAL( 1 ) = CVAL( 1,KFILE )
           ELSE
              START = 1
           END IF
           DO I = START , N							!  Initialise variables without defaults.
             CVAL( I,KFILE ) = DBS_INFOC( REFDB , I + BEG , 'NULVALUE' ) 		!
           END DO
        ELSE
           DO I = 1 , N								!  Initialise variables with values from rec.
               CTEMP = DBS_GETC(REFDB,I+BEG)
               IPOS = MAX(FIND_NOT_BLANK(CTEMP) , 1 )		! Skip blanks
               CVAL(I,KFILE) = CTEMP(IPOS:)
*               VAL( I ) = CVAL( I,KFILE)
           END DO
        END IF
      END IF									! End, different pages

      CONTINUE = .TRUE.
      VECTOR_FIELD = 1
      IF (KFILE .EQ.3 .AND. RUN_TYPE.NE.'OLD') VECTOR_FIELD = 2			! Skip Target no. on new targets

         WRITE( * , '( 3X , A )' ) TITLE					!  Put title on screen.

      NFAIL = 0
      DO WHILE ( CONTINUE )							!   Do until OK.

         CONTINUE=.FALSE.

         IF (FORM_PART .EQ. 'TARGET') THEN
            READ( TITLE(16:18),'(I3)') TARG_NO
            IF ((RUN_TYPE.NE.'OLD') .and. (TARG_NO.GT.1) .AND.(MESID.EQ.-99)) THEN
               LTEST = MDH_GETL(' Target Form', .FALSE.)
               MESID = 0
               IF (.NOT. LTEST) THEN
                  XTYPE = 'Q'
                  GOTO 30
               END IF
            END IF
         END IF

* modified 2.2.94: add flenth
            CALL FORM_INPUT( KFILE, CVAL(1,KFILE), FIELDS(1,KFILE), UNITS(1,KFILE) , FRMT(1,KFILE),
     &         FLENTH(1,KFILE), CONDOFF(1,KFILE), CONDNULL(1,KFILE), N , HELPLIB, LIB_PART , VECTOR_FIELD )
 20      CONTINUE
         CALL PUT_VALS(REFDB,LIMITS, CVAL(1,KFILE) )				! Put values in database
         XTYPE = 'E'

         IF ( KFILE .EQ. 1 ) THEN
               CALL CON_CHECK_COVER(REFDB,MESID,SMG,XTYPE,FORM_FIELD)	!   Consistency check on Cover form
         ELSE IF ( KFILE .EQ. 2 ) THEN
               CALL CON_CHECK_GEN(REFDB,MESID,SMG,XTYPE,FORM_FIELD)	!   Consistency check on General form
         ELSE IF ( KFILE .EQ. 3 ) THEN
               CALL CON_CHECK_TARG (REFDB,MESID,SMG,XTYPE ,FORM_FIELD)	!    Consistency check on Target form
         ELSE IF ( KFILE .EQ. 4 ) THEN
               CALL CON_CHECK_CONSTR(REFDB,MESID,SMG,XTYPE,FORM_FIELD)	!   Consistency check on Constr. form
         END IF
         NFAIL = NFAIL + 1
         IF ((XTYPE.EQ.'R').OR.(XTYPE.EQ.'r')) THEN						! Return to form
            IF (KFILE.EQ.4 .AND. NFAIL.EQ.3) THEN
               CONTINUE = .FALSE.
               XTYPE = 'T'						! Send it back to target form
            ELSE
               CONTINUE = .TRUE.
            END IF
         ELSE
            CONTINUE = .FALSE.
         END IF
         IF (CONTINUE) VECTOR_FIELD = FORM_FIELD - BEG

      END DO									!   End do.
30    CONTINUE

      END									! End.
