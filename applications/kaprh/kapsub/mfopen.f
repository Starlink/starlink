*+  MFOPEN - does opening and initial reading from file for mosaic
*            applications

      SUBROUTINE MFOPEN ( FILNAM, PNINPI, FD, LOC, NUMBER, MAXI,
     :                    MINI, STATUS )

*    Description :
*
*     This routine opens a file that contains information required to
*     build up a mosaic image, i.e. the names and offsets of data
*     arrays (in IMAGE structures) from a central one.
*
*     The routine takes a filename and a parameter, opens the file,
*     associates the parameter and a locator with the given central
*     image, returning the locator. Also returned are the opened file
*     descriptor (for subsequent access to the names and offsets of the
*     remaining data arrays), the number of subsequent images to be read
*     in, and the maximum and minimum x,y offsets that any of the
*     remaining data arrays has from the central one.
*
*     The mosaic file should have the format :
*
*        Mosaic information for Orion K map      ! header
*        central_image                           ! name of central image
*        125                                     ! total no. frames
*        345  229                                ! max x,y offsets
*        -356  -232                              ! min x,y offsets
*        image_2                                 ! subsequent image and
*        35  34                                  ! its x,y offsets
*        image_3
*        36  -33
*        .
*        .
*        .
*        .
*
*    Invocation :
*
*     CALL MFOPEN( FILNAM, PNINPI; FD, LOC, NUMBER, MAXI, MINI, STATUS )
*
*    Parameters :
*
*     FILNAM  =  CHAR( READ )
*         Parameter name of file containing the required mosaic
*           information.
*     PNINPI  =  CHAR( READ )
*         Parameter to be associated with the central IMAGE structure
*     FD   =  INTEGER( WRITE )
*         Fortran descriptor associated with the open file
*     LOC  =  CHAR( WRITE )
*         Locator associated with central IMAGE structure
*     NUMBER  =  INTEGER( WRITE )
*         Total number of frames to form the mosaic
*     MAXI( 2 )  =  INTEGER( WRITE )
*         Maximum x-y offsets of any array from the central one
*     MINI( 2 )  =  INTEGER( WRITE )
*         Minimum x-y offsets of any array from the central one
*     STATUS  =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get file name and open file
*     If error then abort
*     Read the first header line
*     If error then
*        Report context and abort
*     Endif
*     Output the header to the user
*     Read the name of the central image
*     If error then
*        Report context and abort
*     Endif
*     Get the image code associated with the image parameter
*     Put the central image name into this image code
*     Do a DAT_EXIST on the image
*     If status not ok then tidy, report error and return
*     Read the total number of frames (including central one)
*     If error then
*        Report context and abort
*     Endif
*     Read the max and min x,y offsets to be returned
*     If error then
*        Report context and abort
*     Endif
*     Close file if error occurred reading the file
*     End
*
*    Deficiencies :
*
*     Uses SUBPAR_ calls directly.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie RAL ( RAL::CUR )
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     29-12-1986 : First implementation (REVA::MJM)
*     1988 May 30: Largely rewritten using FIO package and more error
*                  reporting.  Fixed bugs (extra argument after STATUS,
*                  2nd argument was not being used) ( RAL::CUR )
*     1989 Aug  8: Converted to packaged FIO opening ( RAL::CUR )
*     1990 Feb 20: AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1990 Feb 22: Replaced SUBPAR calls by AIF_PTFNM (RAL::CUR).
*     1993 Feb 9 : Used the improved FIO_ASSOC and the new FIO_ANNUL.
*                  (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants

*    Import :

      CHARACTER*(*)
     :  FILNAM,                ! name of file containing information
     :  PNINPI                 ! parameter to be associated with central
                               ! IMAGE structure

*    Export :

      INTEGER
     :  FD,                    ! file description
     :  NUMBER,                ! total number of images to be mosaiced
     :  MAXI( 2 ),             ! max x,y offsets from central data array
     :  MINI( 2 )              ! min  "     "      "     "      "

      CHARACTER*(DAT__SZLOC)   ! locator for :
     :  LOC                    ! central IMAGE structure

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local Constants :

      INTEGER
     :  NCHLIN                 ! maximum number of characters in a
                               ! an input record
      PARAMETER ( NCHLIN = 132 )

*    Local variables :

      INTEGER
     :  NCHAR,                 ! number of characters in a record
     :  NCO,                   ! Character column counter
     :  NCV,                   ! number of characters in value
     :  SHRIEK                 ! column location of comment indicator

      CHARACTER*80
     :  BUFFER*(NCHLIN),       ! buffer to hold records read
     :  HEADER,                ! header string in mosaic file
     :  CENTRAL                ! name of central image structure

      REAL
     :  VR( 2 )                ! Dummy array to hold the values read
                               ! from the file
*-
*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Attempt to obtain and open a data file of pixel positions

      CALL FIO_ASSOC( FILNAM, 'READ', 'LIST', 0, FD, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Get the header string from the file

      CALL FIO_READ( FD, HEADER, NCHAR, STATUS )

*    Abort if there has been an error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', HEADER( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFOPEN_HEADER',
     :     'MFOPEN: Error whilst getting the header from the file. '/
     :     /'Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Output this header line

      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'OUT1', ' - Title of mosaic file is :', STATUS )
      CALL MSG_SETC( 'HEAD', HEADER )
      CALL MSG_OUT( 'OUT2', ' - ^HEAD', STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Read name of central image

      CALL FIO_READ( FD, CENTRAL, NCHAR, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', CENTRAL( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFOPEN_CENTRAL',
     :     'MFOPEN: Error whilst getting the name of the central '/
     :     /'IMAGE structure. Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Ignore any comments.

      SHRIEK = INDEX( CENTRAL, '!' )
      IF ( SHRIEK .EQ. 0 ) THEN
         NCV = 80
      ELSE
         NCV = SHRIEK - 1
      END IF

*    Write the image name to the input parameter.

      CALL AIF_PTFNM( PNINPI, CENTRAL( :NCV ), STATUS )

*    Get a locator to the structure now associated with the IMAGE
*    parameter, using DAT_EXIST rather than DAT_ASSOC, as the latter
*    would re-prompt you if the image was discovered not to exist

      CALL DAT_EXIST( PNINPI, 'READ', LOC, STATUS )

*    Check status on return and jump out if there is an error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_MFOPEN_IMLOC',
     :     'MFOPEN: Error whilst trying to get a locator to the '/
     :     /'central IMAGE structure', STATUS )
         GOTO 990
      END IF

*    Read the total number of frames to form mosaic

      CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*    Ignore any comments.

      SHRIEK = INDEX( BUFFER, '!' )
      IF ( SHRIEK .EQ. 0 ) THEN
         NCV = 80
      ELSE
         NCV = SHRIEK - 1
      END IF

*    Extract integer number from the string

      CALL CHR_CTOI( BUFFER( :NCV ), NUMBER, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', BUFFER( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFOPEN_NFRAME',
     :     'MFOPEN: Error whilst getting number of frames. '/
     :     /'Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Read in the maximum x-y offsets that any following image has
*    from the central one as a free-format integer pair

      CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*    Extract integer values from the string

      NCO = 1
      CALL KPG1_FFRR( BUFFER, 2, NCO, VR, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*       Report the error and context.

         CALL MSG_SETC( 'BUFFER', BUFFER( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFOPEN_MAXOFF',
     :     'MFOPEN: Error whilst getting maximum x,y offsets. '/
     :     /'Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Convert the floating-point values to nearest integers as the
*    mosaicking only permits pixel offsets.

      MAXI( 1 )= NINT( VR( 1 ) )
      MAXI( 2 )= NINT( VR( 2 ) )

*    Read in the minimum x-y offsets that any following image has
*    from the central one as a free-format integer pair

      CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*    Extract integer values from the string

      NCO = 1
      CALL KPG1_FFRR( BUFFER, 2, NCO, VR, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*       Report the error and context.

         CALL MSG_SETC( 'BUFFER', BUFFER( :NCHAR ) )
         CALL ERR_REP( 'ERR_MFOPEN_MINOFF',
     :     'MFOPEN: Error whilst getting minimum x,y offsets. '/
     :     /'Buffer was : ^BUFFER', STATUS )
         GOTO 990
      END IF

*    Convert the floating-point values to nearest integers as the
*    mosaicking only permits pixel offsets.

      MINI( 1 )= NINT( VR( 1 ) )
      MINI( 2 )= NINT( VR( 2 ) )

      GOTO 999

*    Close the file when something has gone wrong.

 990  CONTINUE
      CALL FIO_ANNUL( FD, STATUS )

 999  CONTINUE

      END
