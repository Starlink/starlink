*+DBS_ERROR      Converts error number to text message, write to LUN (set?)
*-  Author M.D.C.Harris ( R.A.L )                    22nd April 1987.
*	16 Apr 1992	M. Duesterhaus		Remove VAX specific stuff
***************************************************************************
      SUBROUTINE DBS_ERROR( ERRIN , FILE )

*  Calling Arguments
      CHARACTER*(*) FILE	! In	Name of file.
      INTEGER       ERRIN	! 	Error number.

*  Local Variables
      CHARACTER*60  MESSAGE(11)	! Data for message.
      INTEGER       ERR		! Error number.
     & ,            SIZE	! Length of the file name.

*  GLobal Variables
      INTEGER  LUN	! Logical unit number of output.
      COMMON / DBS_LUN / LUN

*  Local Data
      DATA LUN / 0 / , MESSAGE /
     &   'The file has not been found.'
     & , 'The DSCF file is not available for the file.'
     & , 'Both the DSCF file and the actual file are unavailable.'
     & , 'The field does not exist.'
     & , 'Wrong data type.' , 'End of file.'
     & , 'The fields given are incompatible.'
     & , 'The data is too large for the storage location.'
     & , 'The input parameter is non-existant.'
     & , 'The field number given is too large.'
     & , 'The reference number has no data associated with it.' /


      IF ( ERRIN .LT. 0 ) THEN							! If there is an error then.

        ERR = - ERRIN								!  Get negative of error number.
        SIZE = LEN( FILE )

        IF ( LUN .NE. 0 ) THEN							!  If user has specified a file to put the message -

          WRITE( LUN ,'(A)')
     &          ' ERROR - '// FILE(1:SIZE) //' - '// MESSAGE( ERR )		!   to then write the error to it.

        ELSE									!  Else -

          WRITE( * , '(A)' )
     &          ' ERROR - '// FILE(1:SIZE) //' - '// MESSAGE( ERR )		!   write the message to the screen.

        END IF									!  Fi.

      END IF									! Fi.

      END									! End.
