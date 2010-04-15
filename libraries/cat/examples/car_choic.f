*+  CAR_CHOIC - Obtain response from a set of choices.
      SUBROUTINE CAR_CHOIC (PARAM, CASE, PROMPT, NUMRSP, DESCR, RESPS,
     :  REPLY, STATUS)
*    Description :
*     Subroutine to prompt the user for one choice between a list of
*     possible responses. Prompting continues until a valid entry is
*     input. Abbreviated responses are permitted at the programmer's
*     discretion.
*
*     Notes;
*     1.   The overall prompt string (PROMPT) should fit on one line.
*     2.   The explanation for each choice must be no more than 50
*          characters.
*     3.   The response for each choice must be no more than 10
*          characters.
*     4.   The (arbitary) maximum number of possible responses is 100.
*     5.   Prompting continues until a valid response is given. The
*          default response is the first option given.
*     6.   At the programmer's discretion abbreviations for the input
*          responses are allowed. These are indicated by placing a
*          hash ("#") character in the given response after the last
*          character that may not be abbreviated. Care must be taken
*          that ambiguities are not introduced.
*            If abbreviations are allowed any abbreviation between the
*          minimum specified and the full response is accepted if it
*          corresponds on a character to character basis with the full
*          response.
*            When abbreviations are specified the value returned by the
*          routine is always the full respose given by the calling
*          routine WITHOUT the #.
*     7.   If the routine is input with a non-zero status it immediately
*          returns with REPLY set to blank.
*     8.   The string "HELP" should not be included in the list of
*          possible responses. It is a reserved word used internally
*          by the routine.
*    Invocation :
*     CALL CAR_CHOIC (PARAM, CASE, PROMPT, NUMRSP, DESCR, RESPS; REPLY;
*       STATUS)
*    Parameters :
*     PARAM  =  CHARACTER*(*) (READ)
*           Name of the environment parameter from which the response
*           is to be obtained.
*     CASE  =  LOGICAL (READ)
*           A flag indicating whether the responses are to be forced
*           into upper case, coded as follows:
*           .TRUE.  -  force into upper case,
*           .FALSE. -  do not force into upper case (that is, allow
*                      both cases).
*     PROMPT  =  CHARACTER*(*) (READ)
*           Prompt string.
*     NUMRSP  =  INTEGER (READ)
*           Number of possible responses.
*     DESCR(NUMRSP)  =  CHARACTER*(*) (READ)
*           Explanation for each possible response.
*     RESPS(NUMRSP)  =  CHARACTER*(*) (READ)
*           Possible responses.
*     REPLY  =  CHARACTER*(*) (WRITE)
*           Selected response.
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     Determine the length of each response in its most abbreviated
*     form.
*     Assemble the prompt string.
*     Check if running interactively or in batch.
*     do while (a valid response has not been given)
*       output the prompt.
*       accept a reply
*       If required then
*         convert the reply to upper case.
*       end if
*       if the reply is "HELP" then
*         output the options available
*       else
*         for all the options
*           if the reply equals the current option then
*             set the return choice to the reply
*             set the termination flag.
*           end if
*         end for
*       end if
*     end do
*    Deficiencies :
*     "HELP" cannot be given as one of the responses.
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall.      (ROE::ACD, LEI::ACD)
*     J H Fairclough      (STADAT::JHF)
*    History :
*     30/4/86:  Original version (based on CHOICE, which      (ROE::ACD)
*               interfaced to the interim environment 21/7/83).
*     31/5/89:  Change OPTIONS to HELP for consistency     (STADAT::JHF)
*     24/9/93:  Converted to StarBase and separate behaviour  (LEI::ACD)
*               batch removed.
*     14/10/93: First working StarBase version.               (LEI::ACD)
*     31/1/94:  Trap termination in a case-insensitive way.   (LEI::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NUMRSP       ! Number of possible responses.
      LOGICAL
     :  CASE
      CHARACTER
     :  PARAM*(*),   ! Name of environment parameter for the response.
     :  PROMPT*(*),        ! Prompt string.
     :  DESCR(NUMRSP)*(*), ! Explanation for each possible response.
     :  RESPS(NUMRSP)*(*)  ! Possible responses.
*    Export :
      CHARACTER
     :  REPLY*(*)    ! Selected response.
*    Status :
      INTEGER
     :  STATUS       ! Running status.
*    External references :
      INTEGER
     :  CHR_LEN
      LOGICAL
     :  CHR_SIMLR
*    Local Constants :
      INTEGER
     :  MAXRSP     ! Max. permitted no. of responses.
      PARAMETER
     : (MAXRSP=100)
      CHARACTER
     :  SHORT*1    ! Delimiter for abbreviated responses.
      PARAMETER
     : (SHORT='#')
*    Local variables :
      INTEGER
     :  NCHOIC,         ! Actual number of choices.
     :  RESLEN(MAXRSP), ! No. of characters in each choice.
     :  LOOP,           ! Loop index.
     :  LOOP1,          !  "     "  .
     :  LOOP2,          !  "     "  .
     :  LEN,            ! Length of the current response.
     :  POSN            ! Current position in a character string.
      CHARACTER
     :  RESPSW(MAXRSP)*10, ! Choices without abbreviation delimiters.
     :  REPBUF*10,         ! Response obtained from the environment.
     :  OUTBUF*75          ! Output buffer.
      LOGICAL
     :  VALID,   ! Flag; has a valid response yet been given?
     :  ABBR     ! Flag; abbreviations permitted in the response?
*-

      IF (STATUS .EQ. SAI__OK) THEN
         NCHOIC = MIN(NUMRSP, MAXRSP)

*
*       Determine the length of each response in its most abbreviated
*       form.

         ABBR = .FALSE.
         DO LOOP = 1, NCHOIC
            RESPSW(LOOP) = RESPS(LOOP)
            RESLEN(LOOP) = CHR_LEN (RESPS(LOOP))
            DO LOOP1 = RESLEN(LOOP), 2, -1
               IF (RESPS(LOOP)(LOOP1 : LOOP1) .EQ. SHORT) THEN
                  ABBR = .TRUE.
                  DO LOOP2 =1+LOOP1, RESLEN(LOOP)
                     RESPSW(LOOP)(LOOP2-1 : LOOP2-1) =
     :                 RESPSW(LOOP)(LOOP2 : LOOP2)
                  END DO
                  RESPSW(LOOP)(RESLEN(LOOP) : RESLEN(LOOP)) = ' '
                  RESLEN(LOOP) = LOOP1 - 1
               END IF
            END DO
         END DO

*
*       Assemble the output prompt buffer.

         POSN = 0
         OUTBUF = ' '

         CALL CHR_PUTC ('Choose the required value (HELP for a '/
     :     /'list of available options);', OUTBUF, POSN)

*
*       Running interactively.

         VALID = .FALSE.

         DO WHILE (.NOT. VALID)

*
*          Output the prompts.

            CALL MSG_OUT (' ', PROMPT, STATUS)
            CALL MSG_OUT (' ', OUTBUF(1 : POSN), STATUS)

*
*          Accept reply and convert to upper case.

            CALL PAR_GET0C (PARAM, REPBUF, STATUS)
            CALL PAR_CANCL (PARAM, STATUS)

            IF (CASE) THEN
               CALL CHR_UCASE (REPBUF)
            END IF

            IF (CHR_SIMLR(REPBUF, 'HELP') ) THEN

*
*             Help requested; list the options.

               CALL CAR_CHOI1 (NCHOIC, DESCR, RESPSW, ABBR, STATUS)
            ELSE IF (CHR_SIMLR(REPBUF, 'EXIT') ) THEN

*
*             Trap requests for termination in a case-insensitive way.

               VALID = .TRUE.
               REPLY = 'EXIT'
            ELSE

*
*             Check to see if a valid response has been given.

               DO LOOP = 1, NCHOIC
                  LEN = RESLEN(LOOP)
                  IF (REPBUF(1 : LEN) .EQ. RESPSW(LOOP)(1 : LEN)) THEN
                     VALID = .TRUE.
                     REPLY = RESPSW(LOOP)
                  END IF
               END DO

            END IF
         END DO

      END IF

      END
