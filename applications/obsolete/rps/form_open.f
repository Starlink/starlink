*+FORM_OPEN        Opens database files
********************************************************************************
*  History
*     1989 Jan    ::		1st vsn for separate Cover and Target Files
* 	8 Apr 1992 	M. Duesterhaus	port to UNIX
*     1993 Jun  P. Brisco    Added informational message for record structure
*                           initialization.
*     1993 Jun  P. Brisco    Restructured code to get rid of GOTO statements.
*     1993 June P. Brisco       Recompile with new com_form_files.inc
*     1994 jan	M Ricketts	RAL version
********************************************************************************
      SUBROUTINE FORM_OPEN( OPEN_TYPE_REQ, STATUS)

      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*1 OPEN_TYPE_REQ		! 'C' for creating new files,
					! 'W' for editing old files
					! 'D' for accessing descriptor only
      INTEGER STATUS		! 	Exit status, 0 = OK

*-
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'

      INTEGER NCHAR_DEF
      CHARACTER*128 DEFAULT_FILE
      CHARACTER*3 FILL_TYPE
      COMMON /DEFILE/ NCHAR_DEF, DEFAULT_FILE, FILL_TYPE

*  Function
      CHARACTER*1 MDH_GETCN

*  Local Variables
      CHARACTER*1 CDUMMY
      LOGICAL skip_to_status, skip_to_fill_type, skip_to_ref_test

* _____________________________ Executable Code _______________________________
      skip_to_status = .FALSE.
      skip_to_fill_type = .FALSE.
      skip_to_ref_test = .FALSE.
      status = 0

      IF ((ref_form .NE. 0) .OR. (ref_target .NE. 0) ) THEN

         IF ((ref_form .NE. 0) .AND. (ref_target .NE. 0) ) THEN
            IF (open_type_req .EQ. 'D' .AND. array(ref_form) .EQ. 2 .AND.
     &           array(ref_target) .EQ. 2 )
     &           skip_to_status = .TRUE.
         END IF

         IF (ref_form .GT. 0 .AND. .NOT. skip_to_status) THEN
            IF (array(ref_form) .EQ. 2 .AND. OPEN_TYPE(ref_form) .NE.
     &           open_type_req) CALL DBS_CLOSE(ref_form,'E')
         END IF

       ELSE
          skip_to_ref_test = .TRUE.
      END IF     ! ref_form .ne. 0

      IF (.NOT. skip_to_status) THEN

         IF (ref_target .GT. 0 .AND. .NOT. skip_to_ref_test) THEN
            IF (array(ref_target) .EQ. 2 .AND. OPEN_TYPE(ref_target) .NE.
     &           open_type_req) CALL DBS_CLOSE(ref_target,'E')
         END IF

         IF (ref_form .GT. 0 .AND. ref_target .GT. 0) THEN
            IF (array(ref_form) .EQ. 2 .AND. array(ref_target) .EQ. 2)
     &            skip_to_fill_type = .TRUE.
         END IF     ! ref_form .gt. 0 ...

         IF (.NOT. skip_to_fill_type) THEN
            WRITE (*,*)'Initializing record structures...'
            CALL DBS_OPEN(ref_form,form,open_type_req)

            IF (ref_form .LT. 0) THEN

               IF (ref_form .EQ. -1) THEN
                  CALL FORM_ERR('Cant find file')
                  status = 1
               ELSE
                  CALL DBS_ERROR(ref_form,form)
                  cdummy = MDH_GETCN( 'Return to continue')
                  status = -1
                  skip_to_status = .TRUE.
               END IF           ! ref_form .eq. -1

            END IF              ! ref_form .lt. 0

         END IF                 ! .not. skip_to_fill_type

         IF (.NOT. skip_to_fill_type .AND. .NOT. skip_to_status) THEN
            CALL DBS_OPEN(ref_target,form_target,open_type_req)

            IF (ref_target .LT. 0) THEN
               CALL DBS_ERROR(ref_target,form_target)
               cdummy = MDH_GETCN( 'Return to continue')
               status = -1
               skip_to_status = .TRUE.
            END IF              ! ref_target .lt. 0

         END IF                 ! .not. skip_to_fill_type

         IF (.NOT. skip_to_fill_type .AND. .NOT. skip_to_status)
     &        CALL FORM_PAGE_LIMITS(status)
      END IF                    ! .not. skip_to_status

c
c     Check for the fill type.
c
      IF (.NOT. skip_to_status) THEN
         IF (fill_type .EQ. 'DEF')  CALL COVER_DEFAULTS(
     &        default_file(:nchar_def),status)
      ENDIF


c
c     Check for the status.
c
      IF (status .EQ. 0) THEN
         mtext(loc_mstatus:) = ' Files opened, (type '//open_type_req//
     &        ')                          '
       ELSE IF (status .LT. 0) THEN
         mtext(loc_mstatus:) = 'Error opening files'//
     &        '                                                 '
       ELSE
         mtext(loc_mstatus:) = 'Can''t find file'//
     &         '                                                    '
      END IF

      END
