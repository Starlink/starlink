*+FORM_REFILL      Provides a form filling facility when editing records
      SUBROUTINE FORM_REFILL( FORM_PART,KTARGET,STATUS)
      IMPLICIT NONE

* Calling Arguments
      CHARACTER*(*) FORM_PART	! In	Indicates part of form to edit
      INTEGER KTARGET		!	0 - Cover section, n - Target record no.
				!	-ve - new target
      INTEGER STATUS		! Out	Exit status, 0 = OK
 
********************************************************************************
*  History
*     1988 Dec	M Ricketts	Based on Mark Harris' FORM_FILL
*     1989 Jan	   ::		Revised for split form
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 June         P. Brisco       Got rid of SMG junque.
*	Called by:		FORM_EDIT
********************************************************************************

*  Global Variables
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_files.inc'
 
      INTEGER CHECKSUM
      COMMON /KEEPCHECK/ CHECKSUM
 
* Functions

      CHARACTER*17 DBS_INFOC	! Gets character information about the record.
      LOGICAL      DBS_GETL	! Gets a logical value from the record.
 
*  Local Variables
      CHARACTER*3  CNUM		! Character version of target number.
      INTEGER      HDID		! Header ID.
     & ,           J	! Loop variables.
     & ,           IERR		! Error indicator.
     & ,           MESID	! Display area for messages.
      INTEGER KTARG, IREC
      CHARACTER*3 ED_TYPE, XTYPE*1
      CHARACTER*19 TITLE

*  Data
      CHARACTER*19 TITLE_DATA(4)/'     Cover Page','  General Page',
     &   'Target Record','     Constraints'/

* _______________________ Executable Code ______________________________________
 
      ED_TYPE = 'OLD'						! If adding new recs, set NEW******************
 
      IF (form_part .EQ. 'COVER') THEN
         title = title_data(1)
       ELSE IF (form_part .EQ. 'GEN') THEN
         title = title_data(2)
       ELSE IF (form_part .EQ. 'TARGET') THEN
         irec = ktarget
       ELSE
         GOTO 99
      END IF
 
      IF (KTARGET.GT.0 ) THEN							! If on Target form read the record
         CALL FORM_READ(REF_TARGET,IREC,STATUS)
         IF (STATUS.NE.0) GOTO 99
      END IF
 

      IF (FORM_PART.EQ.'COVER' .OR. FORM_PART.EQ.'GEN') THEN
 
         CALL FORM_INIT(FORM_PART) ! Initialise data collection routines.
         CALL FORM_VALS( FORM_PART, ED_TYPE, MESID, HDID, TITLE, XTYPE)		!   Get the proposal values.
 
      ELSE
 
         IF (KTARGET .LT. 0) THEN						! Either a new target or old rec to edit
            ED_TYPE = 'NEW'
            KTARG = -KTARGET
            WRITE( CNUM , '(I3)' )KTARG 
            CALL DBS_PUTC(REF_TARGET,FLD_TARG_NUMBER,CNUM,IERR)			! Put Target no. in form
         ELSE
            ED_TYPE = 'OLD'
            KTARG = KTARGET
         END IF
         CALL FORM_INIT( 'TARGET')
         CALL FORM_INIT( 'CONSTR')
         WRITE( TITLE , '( A, I3 )' ) TITLE_DATA(3)(:15), KTARG			! Target no. in heading

15       CONTINUE
         CALL FORM_VALS( 'TARGET', ED_TYPE, MESID, HDID, TITLE, XTYPE )		!    Get target form values.

         IF ( DBS_GETL( REF_TARGET , FLD_CONSTRAINTS ) ) THEN			!    If constraints then.
           CALL FORM_VALS( 'CONSTR', ED_TYPE, MESID, HDID, TITLE_DATA(4),XTYPE)	!     Get constraints values.
           IF (XTYPE .EQ. 'T') GOTO 15						! go back to target

         ELSE									!    Else then.

            DO J = FLD_LIMS_CONS(1) , FLD_LIMS_CONS(2)				!     Put in default values.
              CALL DBS_PUTC( REF_TARGET , J
     &           ,   DBS_INFOC( REF_TARGET , J , 'NULVALUE' ) , IERR )
            END DO

         END IF									!    Getting constraints or not
 
      END IF									! Type of record, Cover or target, etc

99    CONTINUE

      END
