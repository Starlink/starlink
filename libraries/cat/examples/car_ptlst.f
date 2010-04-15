*+  CAR_PTLST - outputs list of field values for objects in catalogue
      SUBROUTINE CAR_PTLST (BUFFER, MODE, UNIT, STATUS)
*    Description :
*     Outputs buffer to file or environment, specified by mode.
*    Invocation :
*     CALL CAR_PTLST (BUFFER, MODE, UNIT, STATUS)
*    Parameters :
*     BUFFER =  CHARACTER *(*) (READ)
*           Output buffer
*     MODE   =  CHARACTER *(*) (READ)
*           Mode for output
*     UNIT   =  INTEGER (READ)
*           unit number for output file
*     STATUS =  INTEGER (UPDATE)
*           Running status
*    Method :
*     If (status ok) then
*       if (output to the environment is required ) then
*         output buffer to the environment
*       end if
*       if (output to a file is required ) then
*         attempt to write buffer to file
*         if (write fails) then
*           report error
*           set status
*         end if
*       end if
*     end if
*    Deficiencies
*    Bugs :
*     None known.
*    Authors :
*     S K Leggett          (ROE::SKL)
*     A C Davenhall        (LEI::ACD)
*    History :
*     30/3/87:  Original version.                        (ROE::SKL)
*     24/9/93:  Converted to StarBase                    (LEI::ACD)
*     16/12/96: Removed non-standard features revealed   (ROE::ACD)
*               by the port to Linux (based on
*               modifications by BKM).
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER
     :  BUFFER*(*),    ! Output buffer
     :  MODE*(*)       ! Mode for output, file, environment, both
      INTEGER
     :  UNIT           ! Unit number for output file
*    Import-Export :
*    Export :
*    Status :
      INTEGER
     :  STATUS         ! Running status.
*    External references :
      INTEGER
     :  CHR_LEN        ! Length of character string.
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER
     :  BUFLEN,        ! Length of output buffer.
     :  WSTAT          ! File write status.
      CHARACTER
     :  RTFMT*80       ! Run-time format string.
*    Internal References :
*    Local data :
*-

      IF (STATUS .EQ. SAI__OK) THEN


*       Get length of buffer
          IF (BUFFER .NE. ' ') THEN
              BUFLEN = CHR_LEN(BUFFER)
          ELSE
              BUFLEN = 1
          END IF

          IF (MODE .EQ. 'SCREEN' .OR. MODE .EQ. 'BOTH') THEN
              CALL MSG_OUT (' ',  BUFFER(1:BUFLEN), STATUS)
          END IF

          IF (MODE .EQ. 'BOTH' .OR. MODE .EQ. 'FILE') THEN
              WRITE (RTFMT, '(''(1X, A'', I4, '')'')' ) BUFLEN
              WRITE (UNIT, RTFMT, IOSTAT=WSTAT)
     :          BUFFER(1:BUFLEN)
              CALL FIO_SERR (WSTAT, STATUS)

              IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_REP (' ', 'ERROR writing output file',
     :              STATUS)
              ENDIF
          END IF
      END IF

      END
