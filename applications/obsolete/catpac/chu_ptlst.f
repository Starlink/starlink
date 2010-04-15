*+  CHU_PTLST - outputs list of column values for rows in a table.
      SUBROUTINE CHU_PTLST (BUFFER, MODE, UNIT, STATUS)
*    Description :
*     Outputs buffer to file or environment, specified by mode.
*    Invocation :
*     CALL CHU_PTLST (BUFFER, MODE, UNIT, STATUS)
*    Parameters :
*     BUFFER =  CHARACTER *(*) (READ)
*           Output buffer
*     MODE   =  CHARACTER *(*) (READ)
*           Mode for output
*     UNIT   =  INTEGER (READ)
*           FIO unit number for output file
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
*    History :
*     30/3/87:  Original version.                        (ROE::SKL)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER
     :  BUFFER*(*)    ! Output buffer
      LOGICAL
     :  MODE       ! Mode for output, file, environment, both
      INTEGER
     :  UNIT           ! Unit number for output file
*    Import-Export :
*    Export :
*    Status :
      INTEGER
     :  STATUS         ! Running status.
*    External references :
      INTEGER
     :  CHR_LEN        ! Length of character string
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER
     :  TEXT*(80)      ! Error message string
      INTEGER
     :  BUFLEN,        ! Length of output buffer
     :  WSTAT,         ! File write status
     :  POS            ! Position in error message string
*    Internal References :
*    Local data :
*-

      IF (STATUS .EQ. SAI__OK) THEN


*       Get length of buffer
          BUFLEN = CHR_LEN(BUFFER)

          IF (MODE) THEN
              CALL MSG_OUT ('MESSAGE 1', BUFFER(1:BUFLEN), STATUS)
          ELSE
              CALL FIO_WRITE(UNIT, BUFFER(1:BUFLEN), STATUS)
          END IF
      END IF

      END
