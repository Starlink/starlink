*+  TRANDAT - converts free-format data into an .SDF image

      SUBROUTINE TRANDAT ( STATUS )

*    Description :
*
*     This routine takes mapping data contained in a free-format text
*     file and converts the data into an image structure under the
*     HDS data storage system, such that the image is then compatible
*     with SSE and Adam data processing packages.
*     The user is prompted for the name of a free-format data file
*     ( default extension .DAT ), which contains ( for example )
*     mapping data which he wants converted into an image. The format
*     of the data should be one record (line) per point of input data,
*     each record containing the x and y coordinates of the point, and
*     the intensity value at that point - the positions of each value
*     on the record line are specified by the user. E.g.
*
*         -40.0   40.0   1121.9
*           0.0   30.0     56.3
*         100.0   20.0   2983.2
*         120.0   80.0    339.3     etc..
*
*     The records do not need to be ordered, as the program searches
*     for the maximum and minimum x and y coordinates so that it can
*     define the size of the output image. The user is also prompted
*     for the distance between adjacent pixels in the same units as
*     the read-in x and y coordinates. For example, the data above may
*     have been taken using a 20" beam stepped at 10" intervals, and
*     thus the pixel-to-pixel distance input should be 10.0.
*
*    Invocation :
*
*     CALL TRANDAT( STATUS )
*
*    Parameters :
*
*     FREENAME = CHARACTER( READ )
*           Name of file containing free-format data
*     XPOS = INTEGER( READ )
*           Position of x coordinates in input record
*     YPOS = INTEGER( READ )
*           Position of y coordinates in input record
*     INTENSPOS = INTEGER( READ )
*           Position of intensity values in input record
*     PSCALE = REAL( READ )
*           Pixel to pixel distance in x,y units
*
*    Method :
*
*     Check status on entry - return if not ok
*     Get name of free-format file holding input data
*     If no error so far then
*        Open free-format file using Fortran i/o statement
*        Get position of x coordinate in free-format records
*        Get position of x coordinate in free-format records
*        Get position of intensity value in free-format records
*        Get pixel-to-pixel scale distance
*        Do for maximum allowable number of input points or until
*          the end of the data file is reached
*           Read next record from the free-format file putting values
*            into a data array
*           Increment counter by one
*           Update the x and y coordinate maxima and minima
*        Enddo
*        Output information on number of points read and x,y max and min
*        Calculate and output size of image to be created
*        If output image is too big then
*           Output error message
*           Close free-format file
*           Return
*        Endif
*        Create a structure to hold the output image
*        If no error so far then
*           Map a data array component to hold the data
*           If no error so far then
*              Call subroutine TRANDATSUB to create an image from
*                the data read in from the free-format file
*           Endif
*           Tidy up the output structure
*        Endif
*        Close the free-format file
*     Endif
*     Return
*
*    Deficiencies :
*
*     Uses Fortran statement labels for i/o from free-format data file
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
*     21-10-1985 : First SSE/Adam implementation from the original
*                : free-format -> .BDF file program written by CAA and
*                : MJM for the interim environment (REVA::MJM)
*     07-10-1986 : Bug fix in free-format file name getting and
*                : general tidy up (REVA::MJM)
*     24-11-1986 : Bug fix in max and min initialisations (HILO::MJM)
*     30-05-1987 : Bug fix in argument passing to TRANDATSUB (REVS::MJM)
*     12-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     15-AUG-1994  Changed input DIM arguments for TRANDATSUB (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local Constants :

      INTEGER
     :    NDIMS,                  ! dimensionality of image
     :    MAXDIM,                 ! maximum size of output array
     :    MAXPTS,                 ! maximum number of points allowed
     :    MAXREC                  ! maximum number of values per record

      PARAMETER ( NDIMS = 2 )     ! 2-dimensional images only
      PARAMETER ( MAXDIM = 1024 ) ! maximum of 1024x1024
      PARAMETER ( MAXPTS = 100000 )! upto 100,000 points allowed
      PARAMETER ( MAXREC = 10 )   ! upto 10 values per record

*    Local variables :

      INTEGER
     :    XPOS,                   ! position of x coordinates in records
     :    YPOS,                   !     "     " y      "       "    "
     :    INTENSPOS,              !     "     " intensity values in records
     :    MAXPOS,                 ! largest position in record accessed
     :    COUNT,                  ! number of data points input
     :    ODIMS( 2 ),             ! dimensions of output image
     :    NELEMENTS,              ! number of elements mapped by NDF_MAP
     :    PNTRO,                  ! pointer to output DATA_ARRAY
     :    LOCO,                   ! locator to output structure
     :    I, J                    ! counters

      REAL
     :    DATA( MAXREC, MAXPTS ), ! array to hold input data points
     :    XMAX,                   ! maximum x coordinate input
     :    YMAX,                   !    "    y      "       "
     :    XMIN,                   ! minimum x      "       "
     :    YMIN,                   !    "    y      "       "
     :    XDIST,                  ! distance between XMAX and XMIN
     :    YDIST,                  !     "       "    YMAX  "  YMIN
     :    PSCALE                  ! pixel-to-pixel scale in x,y units

      CHARACTER*80
     :    FREENAME                ! name of input free-format file


*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    entry point if error is found during OPEN statement
100   CONTINUE

*    start by asking user for name of free-format data file
      CALL PAR_GET0C( 'FREENAME', FREENAME, STATUS )

*    check for error here
      IF ( STATUS .EQ. SAI__OK ) THEN

*       attempt to open this file name using direct Fortran statement -
*       if no file of this name exists, then go to 999 and quit
         OPEN( UNIT=10, FILE=FREENAME, STATUS='OLD', ERR=999 )

*       file exists, so ask user for positions of x, y, and i on each
*       line of the input free-format file - first the x coordinate
         CALL AIF_GET0I( 'XPOS', 1, 1, MAXREC, XPOS, STATUS )

*       now the y coordinate
         CALL AIF_GET0I( 'YPOS', 2, 1, MAXREC, YPOS, STATUS )

*       now the intensity
         CALL AIF_GET0I( 'INTENSPOS', 3, 1, MAXREC, INTENSPOS, STATUS )

*       work out which position is furthest along each record
         MAXPOS  =  MAX( XPOS, YPOS, INTENSPOS )

*       now the pixel-to-pixel distance
         CALL AIF_GET0R( 'PSCALE', 1.0, 1.0E-6, 1.0E6, PSCALE, STATUS )

*       initialise counter and maxima and minima
         COUNT  =  0
         XMAX   =  -1.0E37
         YMAX   =  -1.0E37
         XMIN   =   1.0E37
         YMIN   =   1.0E37

*       now run through free-format file picking up the required
*       x, y and i values ( upto MAXPTS points allowed )
         DO  J  =  1, MAXPTS

*          read from the free-format file, going to label 200 when the
*          last record is met and to 998 if an error is met - x, y and i
*          values go into the array DATA
            READ( 10, *, END=200, ERR=998 ) ( DATA(I,J), I=1,MAXPOS )

*          increment counter by one
            COUNT  =  COUNT + 1

*          update max and min x and y values
            XMAX  =  MAX( XMAX, DATA( XPOS, J ) )
            YMAX  =  MAX( YMAX, DATA( YPOS, J ) )
            XMIN  =  MIN( XMIN, DATA( XPOS, J ) )
            YMIN  =  MIN( YMIN, DATA( YPOS, J ) )

*       end of loop until no more input points allowed
         END DO

*       exit here when no more points in file
200      CONTINUE

*       tell user number of data points found, and the max and min x and
*       y coordinate values
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETI( 'COUNT', COUNT )
         CALL MSG_OUT( 'HOW_MANY',
     :     'Number of data points input was ^COUNT', STATUS )
         CALL MSG_SETR( 'XMIN', XMIN )
         CALL MSG_SETR( 'XMAX', XMAX )
         CALL MSG_OUT( 'X_LIMITS',
     :     'x coordinate minimum was ^XMIN, maximum was ^XMAX',
     :      STATUS )
         CALL MSG_SETR( 'YMIN', YMIN )
         CALL MSG_SETR( 'YMAX', YMAX )
         CALL MSG_OUT( 'Y_LIMITS',
     :     'y coordinate minimum was ^YMIN, maximum was ^YMAX',
     :      STATUS )


*       now calculate distance between maximum and minimum x and y points
         XDIST  =  XMAX - XMIN
         YDIST  =  YMAX - YMIN

*       from these and the input pixel-to-pixel distance, we can calculate
*       the dimensions of the output image
         ODIMS( 1 )  =  IFIX( ( XDIST / PSCALE ) + 1.0 )
         ODIMS( 2 )  =  IFIX( ( YDIST / PSCALE ) + 1.0 )

*       tell user the output image size
         CALL MSG_SETI( 'XDIM', ODIMS( 1 ) )
         CALL MSG_SETI( 'YDIM', ODIMS( 2 ) )
         CALL MSG_OUT( 'OUT_DIMS',
     :        'Output image size is ^XDIM x ^YDIM', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )


*       if this is larger than MAXDIM in either dimension, inform the
*       user and abort
         IF( ODIMS( 1 ) .GT. MAXDIM .OR. ODIMS( 2 ) .GT. MAXDIM ) THEN
            CALL MSG_SETI( 'MAXDIM', MAXDIM )
            CALL MSG_OUT( 'TOO_BIG',
     :      'Maximium output array dimension is ^MAXDIM - aborting',
     :       STATUS )
            CLOSE( UNIT=10 )
            RETURN
         END IF


*       now get the output image
         CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

*       check for error before continuing
         IF ( STATUS .EQ. SAI__OK ) THEN

*          map the output DATA_ARRAY component
            CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :               PNTRO, NELEMENTS, STATUS )

*          check for error before accessing pointer
            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine that does the work of filling
*             the image with the input data
               CALL TRANDATSUB( %VAL( PNTRO ), ODIMS(1), ODIMS(2),
     :                      DATA, MAXREC, COUNT, XPOS, YPOS,
     :                      INTENSPOS, PSCALE, XMIN, YMIN, STATUS )

*          end of if-no-error-before-accessing-pointer check
            END IF

*          clear up output image
            CALL NDF_ANNUL( LOCO, STATUS )

*       end of if-no-error-after-creating-output-image check
         END IF

*       close free-format data file
         CLOSE( UNIT=10 )

*    end of if-no-error-after-getting-free-format-filename check
      END IF


*    return
      RETURN


*-----------------------------------------------------------------------
*    error messages

*    here if problem encountered whilst reading data from file
*    no need to flush error as quitting
998   CALL MSG_SETC( 'FNAME', FREENAME )
      CALL ERR_REP( 'BAD_DATA',
     :    'Error found whilst accessing file ^FNAME - aborting',
     :     STATUS )
      CLOSE( UNIT=10 )

*    return
      RETURN

*    here if file not found - go back to top of routine and try again
*    need to flush error here, as not quitting routine
999   STATUS  =  SAI__ERROR
      CALL MSG_SETC( 'FNAME', FREENAME )
      CALL ERR_REP( 'NO_FILE',
     :    'Could not find file ^FNAME - try again', STATUS )
      CALL ERR_FLUSH( STATUS )
      CALL PAR_CANCL( 'FREENAME', STATUS )
      GOTO 100

*    return and end
      END
