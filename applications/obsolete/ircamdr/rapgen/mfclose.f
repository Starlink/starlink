
*+  MFCLOSE - closes file used for mosaic information input

      SUBROUTINE MFCLOSE ( LUN, STATUS )

*    Description :
*
*     Merely closes a file that had been used as a source of mosaic
*     information.
*
*    Invocation :
*
*     CALL MFCLOSE( LUN, STATUS )
*
*    Parameters :
*
*     LUN  =  INTEGER( READ )
*         Unit number of open file
*     STATUS  =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Close the file associated with the unit number
*     Free the unit number back to the process pool
*     Return
*
*    Deficiencies :
*
*     Uses Fortran i/o
*     Uses Vax/VMS LIB$_ call
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     29-12-1986 : First implementation (REVA::MJM)
*     14-SEP-1994  Changed LIB$ to FIO_ (SKL@JACH)
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'FIO_ERR'

*    Import :

      INTEGER
     :    LUN                     ! unit number of open file

*    Status :

      INTEGER  STATUS             ! global status parameter

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    just close the file and free the unit number
      CLOSE( UNIT=LUN )
      CALL FIO_PUNIT( LUN, STATUS )


*    return and end
      END
