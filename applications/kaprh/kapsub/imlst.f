*+  IMLST - Produces a listing of an image.

      SUBROUTINE IMLST( INARR, LBND, DIM1, DIM2, X1, Y1, X2, Y2, FNAME,
     :                  FACTOR, STATUS )
*
*    Description :
*
*     This routine generates a formatted listing of a 1-D or a 2-D
*     image. The resulting file may be typed or printed in the usual
*     manner.
*
*    Invocation :
*
*     CALL IMLST( INARR, LBND, DIM1, DIM2, X1, Y1, X2, Y2, FNAME,
*    :            FACTOR, STATUS )
*
*    Arguments :
*
*     INARR( DIM1, DIM2 ) = REAL( READ )
*           This array contains the image to be listed.
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*           The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*           The second dimension of the 2-d array.
*     X1 = INTEGER( READ )
*           The lower X bound of the region of the image.
*     Y1 = INTEGER( READ )
*           The lower Y bound of the region of the image.
*     X2 = INTEGER( READ )
*           The upper X bound of the region of the image.
*     Y2 = INTEGER( READ )
*           The upper Y bound of the region of the image.
*     FNAME = CHAR*(*)( READ )
*           The name of the parameter for the file that is to hold the
*           listing.
*     FACTOR = REAL( READ )
*           The factor by which the pixel values are to be multiplied.
*     STATUS = INTEGER( READ, WRITE )
*           Value of the status on entering this subroutine.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     A new file is created with the supplied filename. The image is
*     divided into vertical strips of 15 pixels wide, so that the first
*     strip runs from pixel 1 to 15. Each horizontal line of data within
*     the range of the strip is taken and put into a buffer called LINE
*     and written to the file. If a FACTOR is specified, then the data
*     are multiplied by that figure before writing it to file.
*
*    Authors :
*
*     P.T.Wallace
*     K.F.Hartley
*     S.Chan
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     7 January 1982
*     26 September 1983
*     1986 Sep 22 : Renamed from KFH_NLIST. Standardised to RAPI2D
*                   style; renamed parameters section to arguments,
*                   added access and deficiencies; added global status
*                   and Fortran open status; added bad-pixel handling;
*                   region limits now unaltered within routine;
*                   removed tabs; relocated 'local' variables to import
*                   etc.; stripped of trailing blanks and tidied
*                   (RL.STAR::CUR).
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*     1988 Jun 29 : Converted to FIO, added further error reporting and
*                   made FNAME the parameter name for the file rather
*                   than the file name itself (RL.STAR::CUR).
*     1989 May 24 : Fixed bug writing blank record after header;
*                   improved the formatting both across the page by
*                   having half as many numbers (but floating-point
*                   instead of integers), and down the page by noting
*                   the number of strips that fit onto a page
*                   (RL.STAR::CUR).
*     1989 Jul 27 : Used packaged FIO_ASSOC to open the x,y file, and
*                   passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*     1989 Sep 18 : Changed co-ordinate annotations to start from 1
*                   instead of 0 (RL.STAR::CUR).
*     1990 Feb 20 : AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF (RAL::CUR).
*     1993 Feb 9  : Used the improved FIO_ASSOC and the new FIO_ANNUL.
*                   (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*    Status :

      INTEGER STATUS

*    Import :

      INTEGER
     :    LBND( 2 ),
     :    DIM1, DIM2,
     :    X1, Y1,
     :    X2, Y2

      REAL
     :    INARR( DIM1, DIM2 ),
     :    FACTOR

      CHARACTER*(*) FNAME

*    Local Constants :

      INTEGER
     :    NCHLIN,              ! maximum number of characters in a
                               ! an output record
     :    LNSPPA               ! Number of lines per page for output
      PARAMETER ( NCHLIN = 128 )
      PARAMETER ( LNSPPA = 56 )

*    Local variables :

      INTEGER
     :    AX, AY,              ! Array indices
     :    FD,                  ! file description
     :    IX, IY,              ! Loop counters
     :    IXE,                 ! The last pixel of the strip
     :    IXS,                 ! The first pixel of the strip
     :    K,                   ! Position pointer of the buffer
     :    NC,                  ! Number of characters
     :    NOSTRP,              ! Number of strips on the page so far
     :    NSTRIP,              ! Strip count
     :    STRPPA,              ! Maximum number of strips on a page
     :    XX1, YY1,            ! Lower x,y bound of the region
     :    XX2, YY2             ! Upper x,y bound of the region

      CHARACTER*(NCHLIN)
     :    LINE                 ! Line buffer which takes the image data
                               ! and transfers it to the file

      REAL Z                   ! holds the result of FACTOR pixel value

*-
*    If the status is bad on entry, then return to the calling program.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Copy input co-ordinates in case the pair are transposed

      XX1 = MIN( X1, X2 )
      YY1 = MIN( Y1, Y2 )
      XX2 = MAX( X1, X2 )
      YY2 = MAX( Y1, Y2 )

*    Attempt to obtain and open a free-format data file.

      CALL FIO_ASSOC( FNAME, 'WRITE', 'FORTRAN', NCHLIN, FD, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999
      CALL MSG_SETC( 'FILNAM', FNAME )
      CALL MSG_OUT( 'LOG', 'Logging to $^FILNAM.', STATUS )

*    Number of strips per page

      STRPPA = LNSPPA / ( 6 + ( YY2 - YY1 ) )

*    Initialise strip counts.

      NSTRIP = 0
      NOSTRP = 0

*    List in vertical strips each of 15 pixels wide.

      DO  IXS = XX1, XX2, 15

*       Initialise strip count and text buffer

         NOSTRP = NOSTRP + 1
         NSTRIP = NSTRIP + 1
         LINE = ' '

*       To save paper a new page is only thrown after a strip if there
*       is no more room on the page for another strip.

         IF ( NSTRIP .GT. STRPPA ) THEN

            WRITE ( LINE, '( ''1Strip'', I5, 28X,'/
     :        /'''divide listed values by'', G12.4 )' ) NSTRIP, FACTOR

            NOSTRP = 1
         ELSE

            WRITE( LINE, '( ''0Strip'', I5, 28X,'/
     :        /'''divide listed values by'', G12.4 )' ) NSTRIP, FACTOR

         END IF

*       Write buffer to file

         CALL FIO_WRITE( FD, LINE, STATUS )

*       Write blank line to file

         IF ( STATUS .EQ. SAI__OK ) THEN
            LINE = ' '
            CALL FIO_WRITE( FD, LINE, STATUS )
         END IF

*       report and error, close file and return

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'LINE', LINE )
            CALL ERR_REP( 'ERR_IMLST_WHEAD',
     :        'IMLST: Error writing header to file.  Buffer was: '/
     :        /'^LINE', STATUS )
            GOTO 999
         END IF

*       Report first and last X.

         IXE = MIN( IXS+14, XX2 )
         WRITE ( LINE( :12 ), '( I12 )' ) IXS
         K = 4 + 8 * ( IXE - IXS )
         WRITE ( LINE( K+1:K+8 ), '( I8 )' ) IXE

*       Write buffer to file

         CALL FIO_WRITE( FD, LINE, STATUS )

*       Write blank line to file

         IF ( STATUS .EQ. SAI__OK ) THEN
            LINE = ' '
            CALL FIO_WRITE( FD, LINE, STATUS )
         END IF

*       report and error, close file and return

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'LINE', LINE )
            CALL ERR_REP( 'ERR_IMLST_WHEAD',
     :        'IMLST: Error writing header to file.  Buffer was: '/
     :        /'^LINE', STATUS )
            GOTO 999
         END IF

*       Line by line.

         DO IY = YY2, YY1, -1

*          Reset line buffer.

            LINE = ' '

*          Format line co-ordinate.

            WRITE ( LINE( :5 ), '( I5 )' ) IY

*          Allow for the origin offset.

            AY = IY - LBND( 2 ) + 1

*          Pixel by pixel.

            DO  IX = IXS, IXE

*             Buffer pointer.

               K = 9 + 8 * ( IX - IXS )

*             Allow for the origin offset.

               AX = IX - LBND( 1 ) + 1

*             Test for invalid pixel

               IF ( INARR( AX, AY ) .NE. VAL__BADR ) THEN

*                Format pixel.

                  Z = FACTOR * INARR( AX, AY )
                  CALL CHR_RTOC( Z, LINE( K:K+7 ), NC )

               ELSE

                  LINE( K:K+7 ) = ' INVALID'

               END IF

*          Next pixel.

            END DO

*          Write buffer to file

            CALL FIO_WRITE( FD, LINE, STATUS )

*          report and error, close file and return

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'LINE', LINE )
               CALL ERR_REP( 'ERR_IMLST_WDATA',
     :           'IMLST: Error writing data to file.  Buffer was: '/
     :           /'^LINE', STATUS )
               GOTO 999

            END IF

*       Next line.

         END DO

*    Next strip.

      END DO

*    Close report.

      LINE = ' '
      WRITE ( LINE, '( ''1'' )' )

*    Write buffer to the file.

      CALL FIO_WRITE( FD, LINE, STATUS )

*    Close the file.

      CALL FIO_ANNUL( FD, STATUS )

 999  CONTINUE

*    Exit.

      END
