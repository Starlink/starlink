
*+  MFOPEN - does opening and initial reading from file for mosaic routine

      SUBROUTINE MFOPEN ( FNAME, PARAM, LUN, LOC, NUMBER, MAXI,
     :                    MINI, STATUS )

*    Description :
*
*     This routine opens a file that contains information required to
*     build up a mosaic image, i.e. the names and offsets of images
*     from a central one.
*     The routine takes a filename and a parameter, opens the file,
*     associates the parameter and a locator with the given central
*     image, returning the locator. Also returned are the opened file
*     unit number, the number of subsequent images to be read in, and
*     the read-in maximum and minimum x,y offsets that any of the images
*     has from the central one.
*     The mosaic file should have the format :
*
*        Mosaic information for Orion K map      ! header
*        central_image                           ! name of central image
*        125                                     ! total no. frames
*        345  229                                ! max x,y offsets
*        -356  -232                              ! min x,y offsets
*        image_2                                 ! subsequent image and
*        35  34                                  ! its offsets
*        image_3
*        36  -33
*        .
*        .
*        .
*        .
*
*    Invocation :
*
*     CALL MFOPEN( FNAME, PARAM; LUN, LOC, NUMBER, MAXI, MINI, STATUS )
*
*    Parameters :
*
*     FNAME  =  CHAR( READ )
*         Name of file containing required mosaic information
*     PARAM  =  CHAR( READ )
*         Parameter to be associated with central image
*     LUN  =  INTEGER( WRITE )
*         Fortran unit number associated with open file
*     LOC  =  INTEGER( WRITE )
*         Locator associated with central image structure
*     NUMBER  =  INTEGER( WRITE )
*         Total number of frames to be mosaiced
*     MAXI( 2 )  =  INTEGER( WRITE )
*         Maximum x,y offsets of any image from the central one
*     MINI( 2 )  =  INTEGER( WRITE )
*         Minimum x,y offsets of any image from the central one
*     STATUS  =  INTEGER( UPDATE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get a free unit number from the process pool
*     Open the file
*     Read the first header line
*     Output the header to the user
*     Read the name of the central image
*     Get the image code associated with the image parameter
*     Put the central image name into this image code
*     Do a DAT_EXIST on the image
*     If status not ok then tidy and return
*     Read the total number of frames (including central one)
*     Read the max and min x,y offsets to be returned
*     Return
*
*    Deficiencies :
*
*     Uses Fortran i/o
*     Uses Vax/VMS specific LIB$_ routines
*     Uses SUBPAR_ calls direct
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
*     10-MAR-94    Changed DAT_ calls to NDF_ (SKL@JACH)
*     14-JUL-1994  LIB$ to FIO_ (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'FIO_PAR'
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Import :

      CHARACTER*(*)
     :    FNAME,                  ! name of file containing information
     :    PARAM                   ! parameter to be associated with central
                                  ! image
*    Export :

      INTEGER
     :    LUN,                    ! unit number associated with open file
     :    LOC,                    ! locator for central image structure
     :    NUMBER,                 ! total number of images to be mosaiced
     :    MAXI( 2 ),              ! max x,y offsets from central image
     :    MINI( 2 )               ! min  "     "      "     "      "


*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    IMCODE                  ! code associated with image parameter

      CHARACTER*80
     :    HEADER,                 ! header string in mosaic file
     :    CENTRAL                 ! name of central image structure

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    get a free Fortran unit number from the process pool
      CALL FIO_GUNIT( LUN, STATUS )

*    open the specified file
      OPEN( UNIT=LUN, FILE=FNAME, STATUS='OLD', ERR=999 )

*    read the header line and output it
      READ( LUN, '(A)', END=999, ERR=999 ) HEADER

*    output this header line
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'OUT1', ' - Title of mosaic file is :', STATUS )
      CALL MSG_SETC( 'HEAD', HEADER )
      CALL MSG_OUT( 'OUT2', ' - ^HEAD', STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    read name of central image
      READ( LUN, '(A)', END=999, ERR=999 ) CENTRAL

*    get the image code associated with the image parameter
      CALL SUBPAR_FINDPAR( 'INPICI', IMCODE, STATUS )

*    push the central frame name into that code position
      CALL SUBPAR_PUTNAME( IMCODE, CENTRAL, STATUS )

*    get a locator to the structure now associated with the image
*    parameter, using NDF_EXIST rather than NDF_ASSOC, as the latter
*    would re-prompt you if the image was discovered not to exist
      CALL NDF_EXIST( 'INPICI', 'READ', LOC, STATUS )

*    check status on return and jump out if there is an error
      IF ( (LOC .EQ. NDF__NOID) .OR.
     :     (STATUS .NE. SAI__OK) ) GOTO 999

*    read the total number of frames to be mosaiced
      READ( LUN, *, END=999, ERR=999 ) NUMBER

*    read the max x,y offsets that any following image has from the
*    central one
      READ( LUN, *, END=999, ERR=999 ) MAXI( 1 ), MAXI( 2 )

*    read the min x,y offsets that any following image has from the
*    central one
      READ( LUN, *, END=999, ERR=999 ) MINI( 1 ), MINI( 2 )

*    return
      RETURN


*    come here if there has been any error, tidying up and returning
*    with an error flagged
999   STATUS  =  SAI__ERROR
      CLOSE( UNIT=LUN )
      CALL FIO_PUNIT( LUN, STATUS )
      END
