
*+  GLITCHLIST - gets glitch positions from a file and deglitches an image

      SUBROUTINE GLITCHLIST ( DIMS1, DIMS2, ARRAY, FILENAME, STATUS )

*    Description :
*
*     This routine deglitches an image. The glitch positions are defined
*     according to values found in a free-format file, the name of which
*     is given as input. Returned is the deglitched image, and the number
*     of pixels listed in the free-format file to be deglitched, and the
*     number that were actually valid and consequently deglitched, are
*     both output.
*     If the specified file could not be opened, the status parameter is
*     set to error and an immediate return follows.
*     The glitch list file should comprise the following
*
*       Glitch list for SBRC FPA#005
*       22  45
*       19  56
*       2   30
*       .   .
*       .   .
*       .   .
*       <EOF>
*
*     i.e. a header string that is output to the user, followed by x,y
*     free-format pixel position pairs, terminated just by the end-of-
*     file marker. The header string is output to the user.
*
*    Invocation :
*
*     CALL GLITCHLIST( DIMS, ARRAY, FILENAME; STATUS )
*
*    Parameters :
*
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of image
*     ARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( WRITE )
*         Output deglitched image
*     FILENAME  =  CHAR( READ )
*         Name of file containing free-format glitch list
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get a free unit number from process pool
*     Open given file name as this unit number
*     If not successful on opening then
*        Set status to indicate error
*        Free unit number
*        Return
*     Endif
*     Get header string - output to user
*     Do while more pixel positions in file
*        Get next pixel position from file as free-format integers
*        Increment number of pixels tried by one
*        If pixel has a valid position on input array then
*           Call GLITCHSUB to do the business
*           Increment valid pixel counter by one
*        Endif
*     Enddo
*     Output values of pixel counters
*     Close file
*     Free unit number back into process pool
*     Return
*
*    Deficiencies :
*
*     Uses Fortran file i/o
*     Uses Vax run time library routines LIB$GET_LUN and LIB$FREE_LUN
*     Uses statement labels and format statements
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
*     24-06-1986 : First implementation (REVA::MJM)
*     26-May-1994  Replaced lib$ calls with fio_ (SKL@JACH)
*     14-July-1994 Changed arguments to input DIMS separately
*                  so that routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'FIO_PAR'

*    Import :

      INTEGER
     :    DIMS1,               ! dimensions of input array
     :    DIMS2                ! dimensions of input array

      CHARACTER*(*)
     :    FILENAME                ! name of VMS file holding glitch list

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )  ! array holding deglitched data

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    STAT,                   ! for IOSTAT
     :    LUN,                    ! file unit number
     :    XPOS,                   ! x position of current pixel
     :    YPOS,                   ! y     "     "    "      "
     :    TRIED,                  ! number of pixel positions read in
     :    DONE                    ! number of valid positions deglitched

      REAL
     :    OLDVAL,                 ! old value for current pixel
     :    NEWVAL                  ! new (deglitched) value for current pixel

      CHARACTER*80
     :    HEADER                  ! header string in glitch list file

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get a free unit number for the file opening from the process pool
      CALL FIO_GUNIT( LUN, STATUS )

*    open the requested named file as the unit specified above
      OPEN( UNIT = LUN, FILE = FILENAME, STATUS = 'OLD', IOSTAT=STAT,
     :      ERR = 999 )

*    get the header string from the file
      READ( LUN, '(A)', IOSTAT=STAT, END = 999, ERR = 999 ) HEADER

*    write out the header to the user
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'LINE1',
     : 'Title of glitch list file :', STATUS )
      CALL MSG_SETC( 'HEADER', HEADER )
      CALL MSG_OUT( 'LINE2', '^HEADER', STATUS )

*    initialise the counters
      TRIED  =  0
      DONE   =  0

*    scan round the file until the end is reached
      DO WHILE ( STATUS .EQ. SAI__OK )

*       read in the current pixel position as a free-format integer pair -
*       jump out of loop if the end of file is found
         READ( LUN, *, IOSTAT=STAT, END = 998, ERR = 999 )
     :         XPOS, YPOS

*       increment the counter keeping track of the number of requested
*       pixels
         TRIED  =  TRIED + 1

*       check that given pixel position lies on the input data array
         IF ( XPOS .GE. 1 .AND. XPOS .LE. DIMS1 .AND.
     :        YPOS .GE. 1 .AND. YPOS .LE. DIMS2 ) THEN

*          position is valid - deglitch it
            CALL GLITCHSUB( DIMS1, DIMS2, ARRAY, XPOS, YPOS, OLDVAL,
     :                      NEWVAL, STATUS )

*          increment valid deglitch counter by one
            DONE  =  DONE + 1

*       end of if-pixel-position-is-on-array check
         END IF

*    end of loop round glitch list file
      END DO

*    come here when the glitch file has no more entries
998   CONTINUE

*    output messages to give the number of pixel positions read in and
*    actually deglitched
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETI( 'TRIED', TRIED )
      CALL MSG_OUT( 'NTRIED',
     : ' Number of pixel positions read in from file    =  ^TRIED',
     :   STATUS )
      CALL MSG_SETI( 'DONE', DONE )
      CALL MSG_OUT( 'NDONE',
     : ' Number of valid positions actually deglitched  =  ^DONE',
     :   STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    close the file
      CLOSE( UNIT = LUN )

*    free the unit number back to the process pool
      CALL FIO_PUNIT( LUN, STATUS )


*    return
      RETURN


*    come here if there is an error on opening the requested file -
*    return an error status and free the unit number
999   CALL FIO_REP( LUN, FILENAME, STAT, ' ', STATUS )
      CALL FIO_PUNIT( LUN, STATUS )

      RETURN


*    return and end
      END
