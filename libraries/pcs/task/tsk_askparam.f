*+  TASK_ASKPARAM - ask user for parameter value
      SUBROUTINE TASK_ASKPARAM ( PATH, VALUE, MESSID, STATUS )
*    Description :
*     Given parameter info in "value", ask user (by prompting) for
*     a value for the parameter.
*    Invocation :
*     CALL TASK_ASKPARAM ( PATH, VALUE, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (Given)
*           path to requesting task
*     VALUE=CHARACTER*(*) (Given)
*           message value string
*     MESSID=INTEGER (Given)
*           ID for reply message
*     STATUS=INTEGER
*    Method :
*     The value string contains substrings separated by nulls. The 
*     substrings are the parameter name, its prompt string, its default 
*     value, its help information, and an error message if an 
*     earlier attempt to get the parameter has failed. This information 
*     is used by ASKPARAM to prompt the user. 
*     The value obtained is then sent to the task which requested the 
*     parameter.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVA::ADAM) 7Nov84
*    History :
*     07.11.1984: Original version (REVAD::JAC)
*     29.03.1985: UFACE version (REVAD::BDK)
*     18.10.1985: version for cli running as a task (REVAD::BDK)
*     23.07.1987: make PARHELP 132 long (REVAD::BDK)
*     05.11.1987: version for TASK (REVAD::BDK)
*     05.12.1990: call SUBPAR_SPLITVAL to split revised message
*                 value structure (RLVAD::AJC)
*     06.05.1991: revise include files (REVAD::BDK)
*     25.11.1991: use ADAM_ACKNOW (REVAD::BDK)
*      4.10.1992: add PAR_PAR for porting
*                 replace STR$TRIM with CHR_LEN (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR,
*                  SUBPAR__NAMELEN not PAR__SZNAM
*                  and SUBPAR_PARERR not PAR_ERR (RLVAD::AJC)
*     11.06.2001: Use AMS (FAMS) _REPLY not ADAM_ACKNOW
*                 Use AMS (FAMS) _PLOOKUP not MESSYS_PLOOKUP
*                 Change MSG_VAL_LEN to MESSYS__VAL_LEN (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER PATH                 ! path to requesting task
      CHARACTER VALUE*(*)          ! message value string
      INTEGER MESSID               ! ID for reply message

*    Status :
      INTEGER STATUS

*    External Routines :
      INTEGER CHR_LEN              ! used length of string
      EXTERNAL CHR_LEN

*    Local constants :
      INTEGER NOCONTEXT
      PARAMETER ( NOCONTEXT = 0 )
      CHARACTER NONAME
      PARAMETER ( NONAME = ' ' )

*    Local variables :
      CHARACTER*(MESSYS__TNAME) TASKNAME ! name of task which requested
                                         ! value 
      INTEGER TNAMELEN                   ! length of TASKNAME
      CHARACTER*(SUBPAR__NAMELEN) PARAM  ! parameter name
      INTEGER PARLEN                     ! length of parameter name 
      CHARACTER*80 PROMPT                ! parameter prompt string
      INTEGER PRMPLEN                    ! length of parameter prompt
                                         ! string 
      CHARACTER*80 DEFAULT               ! default value string
      INTEGER DEFLEN                     ! length of default value
                                         ! string 
      CHARACTER*132 PARHELP              ! one-line help specifier
      INTEGER HELPLEN                    ! length of one-line help
                                         ! specifier 
      CHARACTER*132 HLPKEY               ! full-help specifier
      INTEGER HKYLEN                     ! length of full-help specifier
      CHARACTER*100 ERRMESS              ! error message
      INTEGER ERRLEN                     ! length of error message
      CHARACTER*(MESSYS__VAL_LEN) INVAL  ! value from user
      INTEGER MESSTATUS                  ! forwarded message status
*-

      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Find name of task which sent the request.
*
      CALL FAMS_PLOOKUP ( PATH, TASKNAME, STATUS )
      TNAMELEN = CHR_LEN( TASKNAME )
*
*   Extract parameter info components.
*
      CALL SUBPAR_SPLITVAL( VALUE, PARAM, PARLEN, PROMPT, PRMPLEN,
     :  DEFAULT, DEFLEN, PARHELP, HELPLEN, HLPKEY, HKYLEN, ERRMESS,
     :  ERRLEN, STATUS) 

*
*   Send the prompt to the user interface.
*
      CALL SUBPAR_REQUEST ( PARAM(1:PARLEN), PROMPT(1:PRMPLEN),
     :  DEFAULT(1:DEFLEN), PARHELP(1:HELPLEN), HLPKEY(1:HKYLEN),
     :  ERRMESS(1:ERRLEN), INVAL, STATUS )
*
*   Trap non-ok conditions. These have to be translated into things 
*   which the parameter system at the receiving task will interpret 
*   correctly. In particular, it will convert timeout into PAR__NOUSR.
*
      MESSTATUS = MESSYS__PARAMREP
      IF ( STATUS .EQ. PAR__NULL ) THEN
         INVAL = '!'
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         INVAL = '!!'
      ELSE IF ( STATUS .EQ. PAR__NOUSR ) THEN
         MESSTATUS = MESSYS__TIMEOUT
      ENDIF

      STATUS = SAI__OK

*
*   Have got value, send it back to the requesting task.
*
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSTATUS,
     :  NOCONTEXT, NONAME, MESSYS__VAL_LEN, INVAL, STATUS )

      END
