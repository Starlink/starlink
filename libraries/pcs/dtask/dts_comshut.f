*+  DTASK_COMSHUT - shut-down communications for a transaction
      SUBROUTINE DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
     :  AKEY, VALUE, STATUS )
*    Description :
*     Shut-down communications for a transaction, including sending the
*     final acknowledgement. Operate even on bad entry status.
*    Invocation :
*     CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, 
*    :  AKEY, VALUE, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           message path needed for reply
*     MESSID=INTEGER given)
*           transaction number needed for reply
*     MESSTATUS=INTEGER ( given)
*           status to be returned in completion message
*     CONTEXT=INTEGER (given)
*           context to be returned in completion message
*     AKEY=CHARACTER*(*) (given)
*           action keyword
*     VALUE=CHARACTER*(*) (given and returned)
*           string to be returned in completion message
*     STATUS=INTEGER (returned)
*           status is returned OK if at all possible.
*    Method :
*     Flush the ERR and MSG systems. Send the final acknowledgment.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.05.1991:  original (REVAD::BDK)
*     28.05.1991:  use ERR_CLEAR (REVAD::BDK)
*     07.06.1991:  change comments (REVAD::BDK)
*     11.06.1991:  report failure in shutdown (REVAD::BDK)
*     25.11.1991:  use ADAM_ACKNOW (REVAD::BDK)
*     14.10.1992:  get ^STATUS via DTASK_ESETK (RLVAD::AJC)
*     11.06.2001:  call AMS_REPLY/PLOOKUP (FAMS) directly
*                  ADAM_PRCNAM now DTASK_PRCNAM (AJC)
*                  
*    endhistory

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'

*    Import :
      INTEGER PATH               ! message path needed for reply
      INTEGER MESSID             ! transaction number needed for reply 
      INTEGER MESSTATUS          ! status to be returned in completion
                                 ! message 
      INTEGER CONTEXT            ! context to be returned in completion
                                 ! message 
      CHARACTER*(*) AKEY         ! keyword of action required
      CHARACTER*(*) VALUE        ! command line parameter string

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MESLEN                     ! length of value
      CHARACTER*(MESSYS__TNAME) MYNAME   ! name of this task
      INTEGER NLENGTH                    ! actual length of MYNAME
      CHARACTER*(MESSYS__TNAME) BADNAME  ! name of other task when 
                                         ! failed to close 
                                         ! communications
      INTEGER ISTAT                      ! local status
*-


*
*   Clear-out the error reporting system, then send the final 
*   acknowledgement which shuts down the communications for
*   the transaction.
*
      CALL ERR_CLEAR ( STATUS )
      MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
      CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE, MESSTATUS,
     :  CONTEXT, AKEY, MESLEN, VALUE, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
*
*      Bad status from closing-down transaction
*
         ISTAT = SAI__OK
         CALL DTASK_PRCNAM ( MYNAME, NLENGTH, ISTAT )
         CALL DTASK_ESETK ( 'STAT', STATUS )
         CALL ERR_REP ( ' ', MYNAME(1:NLENGTH) //
     :     ' failed to return acknowledgment, ^STAT', STATUS)
         ISTAT = SAI__OK
         CALL FAMS_PLOOKUP ( PATH, BADNAME, ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            BADNAME = 'unknown'
         ENDIF
         CALL MSG_SETC ( 'BADNAME', BADNAME )            
         CALL MSG_SETC ( 'AKEY', AKEY )
         IF ( VALUE .EQ. ' ' ) THEN
            CALL ERR_REP ( ' ', 'to task ^BADNAME, action '//
     :        '^AKEY', STATUS )
         ELSE
            CALL MSG_SETC ( 'VALUE', VALUE )
            CALL ERR_REP ( ' ', 'to task ^BADNAME, action '//
     :        '^AKEY, value ^VALUE', STATUS )
         ENDIF
*
*      Clear-out ERR and MSG.
*
         CALL ERR_CLEAR ( STATUS )
      ENDIF

      END
