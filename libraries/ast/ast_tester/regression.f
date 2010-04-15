	PROGRAM REGRESSION
*+
*  Name:
*     REGRESSION

*  Purpose:
*     Tests many aspects of the AST library (Fortran interface).

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Fortran program

*  Invocation:
*     regression

*  Description:
*     This application utilizes many aspects of the AST library, producing
*     textual output on standard output. The output should be redirected
*     to a text file and compared to the output from previous runs to
*     detect any changes in functionality.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-2002 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INTEGER CMN_FTEST          ! Which FITS test are we doing?
      INTEGER CMN_LINE           ! The index of the next header to read
      COMMON /REG/ CMN_FTEST, CMN_LINE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and declarations

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

      EXTERNAL REG_SOURCE
      EXTERNAL REG_SINK

      EXTERNAL REG_ATTR
      EXTERNAL REG_FLUSH
      EXTERNAL REG_LINE
      EXTERNAL REG_MARK
      EXTERNAL REG_TEXT
      EXTERNAL REG_TXEXT
      EXTERNAL REG_CAP
      EXTERNAL REG_QCH
      EXTERNAL REG_SCALES

*  Local Constants:
      INTEGER NFITS_TESTS         ! How many FITS tests?
      PARAMETER ( NFITS_TESTS = 3 )

      INTEGER NCAT, NRAT, NLAT, NDAT, NIAT ! Numbers of Attributes of each type
      PARAMETER ( NCAT = 4,
     :            NRAT = 2,
     :            NLAT = 2,
     :            NDAT = 1,
     :            NIAT = 5 )

*  Local Variables:
      CHARACTER ATTRS( NFITS_TESTS )*255 ! Plot attributes for each FITS test
      CHARACTER CARDS*(7*80)             ! Used fot testing ast_putcards
      INTEGER FC, FS, PLOT, I, J, OC

      REAL GBOX( 4 )              ! Area of graphics coords to use
      DOUBLE PRECISION BBOX( 4, NFITS_TESTS ) ! Base Frame area to be
				  ! mapped onto GBOX for each FITS test

      CHARACTER*20 CAT(NCAT), RAT(NRAT), LAT(NLAT), DAT(NDAT), IAT(NIAT)
      CHARACTER CV*50
      REAL RV
      LOGICAL LV
      DOUBLE PRECISION DV
      INTEGER IV, VERS, MAJ, MIN, REV

*  Data initialization:
      DATA CAT / 'Colour(axis1)', 'Font(Stri)', 'Nout', 'Class' /
      DATA RAT / 'Tol', 'Gap(1)' /
      DATA LAT / 'Border', 'Invert' /
      DATA DAT / 'TextLabGap' /
      DATA IAT / 'Nin', 'Current', 'Base', 'Nobject', 'RefCOUNT' /
      DATA GBOX /-100.0, -200.0, 150.0, 300.0/



      DATA BBOX / 10.0, -10.0, 290.0, 300.0,
     :            -300.0, -300.0, 500.0, 500.0,
     :            1.0, 1.0, 1787.0, 447.0 /



      DATA ATTRS/ 'Grid=1,tickall=0',
     :            'Grid=1,labelling=interior',
     :            'Grid=0' /


*.

*  Initialize inherited global status.
      STATUS = SAI__OK

*  Use object caching to minimise allocation of new memory
      OC = AST_TUNE( 'ObjectCaching', 1, STATUS )
      IF( OC .NE. 0 ) THEN
         WRITE(*,'(A,I2)') 'Default ObjectCaching VALUE is ',OC
      END IF

      IF( AST_TUNE( 'ObjectCaching', AST__TUNULL, STATUS ) .NE. 1 ) THEN
         WRITE(*,'(A,I2)') 'Set ObjectCaching VALUE is ',OC
      END IF

*  Display the AST version number.
      VERS = AST_VERSION()
      MAJ = VERS/1000000
      VERS = VERS - 1000000*MAJ
      MIN = VERS/1000
      REV = VERS - 1000*MIN
      WRITE(*,'(A,I2,A,I1,A,I2)') 'AST version ',MAJ,'.',MIN,'-',REV

*  First do a test of the AST_PUTCARDS routine.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

      CARDS = 'NAXIS   = 1'
      CARDS( 81: ) = 'NAXIS1  = 100'
      CARDS( 2*80 + 1: ) = 'CTYPE1  = ''fred'''
      CARDS( 3*80 + 1: ) = 'CDELT1  = 0.0'
      CARDS( 4*80 + 1: ) = 'CRPIX1  = 50'
      CARDS( 5*80 + 1: ) = 'CUNIT1  = ''GHz'''

      CALL AST_PUTCARDS( FC, CARDS, STATUS )
      WRITE(*,'(A,I2)') 'PutCards Ncards = ',AST_GETI( FC, 'NCARD',
     :                                                 STATUS )
      WRITE(*,'(A,I2)') 'PutCards Card = ',AST_GETI( FC, 'CARD',
     :                                               STATUS )

      CALL AST_SETI( FC, 'CARD', 10, STATUS )
      WRITE(*,'(A,I2)') 'PutCards Card = ',AST_GETI( FC, 'CARD',
     :                                               STATUS )

      CALL AST_PUTCARDS( FC, CARDS, STATUS )
      WRITE(*,'(A,I2)') 'PutCards Ncards = ',AST_GETI( FC, 'NCARD',
     :                                                 STATUS )
      WRITE(*,'(A,I2)') 'PutCards Card = ',AST_GETI( FC, 'CARD',
     :                                               STATUS )
      CALL AST_SHOW( FC, STATUS )

*  We loop round testing several sorts of FITS Headers.
      DO I = 1, NFITS_TESTS
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Tell the REG_SOURCE function which FITS header to load.
	 CMN_FTEST = I
         CMN_LINE = 1

         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') ' '
         WRITE(*,'(A,I2)') ' FITS test number ',I
         WRITE(*,'(A)') ' ===================='
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') ' '

*  Create a FitsChan, read an Object from it, and dump the Object
*  to standard output. The Object should be a FrameSet if all is OK.
         FC = AST_FITSCHAN( REG_SOURCE, REG_SINK, ' ', STATUS )
         FS = AST_READ( FC, STATUS )
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') 'AST_SHOW:'
         CALL AST_SHOW( FS, STATUS )

*  Annul the FitsChan. This will cause the unused contents (if any) to
*  be written out using REG_SINK.
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') 'REG_SINK:'
         CALL AST_ANNUL( FC, STATUS )

* Create another FrameSet with Native encoding. Write the FrameSet to
* it, and then annul the FitsChan (this will cause the FITS cards to be
* written to stdout).
         FC = AST_FITSCHAN( AST_NULL, REG_SINK, 'Encoding=native',
     :                      STATUS )
         WRITE(*,'(A)') ' '
         WRITE(*,'(A,I2)') 'Objects written: ', AST_WRITE( FC, FS,
     :                                                     STATUS )
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') 'Native Encoding:'
         CALL AST_ANNUL( FC, STATUS )

*  Create a Plot which maps the area specified by BBOX the Base Frame
*  of the FrameSet onto the GBOX area in graphics coords.
         PLOT = AST_PLOT( FS, GBOX, BBOX( 1, I), ' grf = 1 , '//
     :                    'title = A FITS test', STATUS )

*  Annul the FrameSet.
	 CALL AST_ANNUL( FS, STATUS )

*  Tell the Plot to use the REG_... routines included in this file to
*  do the drawing.
	 CALL AST_GRFSET( PLOT, 'Attr', REG_ATTR, STATUS )
	 CALL AST_GRFSET( PLOT, 'Flush', REG_FLUSH, STATUS )
	 CALL AST_GRFSET( PLOT, 'Line', REG_LINE, STATUS )
	 CALL AST_GRFSET( PLOT, 'Mark', REG_MARK, STATUS )
	 CALL AST_GRFSET( PLOT, 'Text', REG_TEXT, STATUS )
	 CALL AST_GRFSET( PLOT, 'TxExt', REG_TXEXT, STATUS )
	 CALL AST_GRFSET( PLOT, 'Scales', REG_SCALES, STATUS )
	 CALL AST_GRFSET( PLOT, 'Cap', REG_CAP, STATUS )
	 CALL AST_GRFSET( PLOT, 'Qch', REG_QCH, STATUS )

*  Set some attributes.
	 CALL AST_SET( PLOT, ATTRS( I ), STATUS )

*  Get some attributes (separate the AST_GET calls and the WRITEs in order
*  to avoid recursive I/O due to the REG_xxx routines trying to write to
*  standard output).
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') 'ATTRIBUTES:'

         DO J = 1, NCAT
            CV = AST_GETC( PLOT, CAT(J), STATUS )
   	    WRITE(*,'(A,I10)') ' '//CAT(J)//': ',CHR_LEN(CV)
         END DO

         DO J = 1, NRAT
            RV = AST_GETR( PLOT, RAT(J), STATUS )
   	    WRITE(*,'(A,G13.6)') ' '//RAT(J)//': ',RV
         END DO

         DO J = 1, NLAT
            LV = AST_GETL( PLOT, LAT(J), STATUS )
            IF( LV ) THEN
               IV = 1
            ELSE
               IV = 0
            END IF
   	    WRITE(*,'(A,I1)') ' '//LAT(J)//': ',IV
         END DO

         DO J = 1, NDAT
            DV = AST_GETD( PLOT, DAT(J), STATUS )
   	    WRITE(*,'(A,G13.6)') ' '//DAT(J)//': ',DV
         END DO

         DO J = 1, NIAT
            IV = AST_GETI( PLOT, IAT(J), STATUS )
   	    WRITE(*,'(A,I4)') ' '//IAT(J)//': ',IV
         END DO


*  Draw a grid.
         WRITE(*,'(A)') ' '
         WRITE(*,'(A)') 'AST_GRID:'
         CALL AST_GRID( PLOT, STATUS )

*  Annul the Plot.
	 CALL AST_ANNUL( PLOT, STATUS )

      END DO

 999  CONTINUE

      END





*  Grf plotting routines for the Plot tests. These are used in preference
*  to the grf routines specified at link time.
*  ======================================================================

*  Flush graphics.
*  ---------------
      INTEGER FUNCTION REG_FLUSH()
      WRITE(*,'(A)') 'REG_FLUSH:'
      REG_FLUSH = 1
      END

*  Set or get a Plot graphics attribute.
*  -------------------------------------
      INTEGER FUNCTION REG_ATTR(  ATT, VAL, OLDVAL, PRIM )
      IMPLICIT NONE

*  Includes:
      INCLUDE 'AST_PAR'
      INCLUDE 'GRF_PAR'

*  Arguments:
      INTEGER ATT
      DOUBLE PRECISION VAL
      INTEGER PRIM
      DOUBLE PRECISION OLDVAL

*  Local Variables:
      INTEGER I, J
      DOUBLE PRECISION ATTRS( 5, 3 )

*  Initialization:
      DATA ATTRS /15*0.0D0/

*  Log this call.
      WRITE(*,'(I4,1X,G10.3,1X,I4)') 'REG_GATTR: ', ATT, VAL, PRIM

*  Identify the required element.
      IF( ATT .EQ. GRF__STYLE ) THEN
         I = 1
      ELSE IF( ATT .EQ. GRF__WIDTH ) THEN
         I = 2
      ELSE IF( ATT .EQ. GRF__SIZE ) THEN
         I = 3
      ELSE IF( ATT .EQ. GRF__FONT ) THEN
         I = 4
      ELSE IF( ATT .EQ. GRF__COLOUR ) THEN
         I = 5
      ELSE
         WRITE(*,'(A,I2)') 'Bad ATT value: ', ATT
      END IF

      IF( PRIM .EQ. GRF__LINE ) THEN
         J = 1
      ELSE IF( PRIM .EQ. GRF__MARK ) THEN
         J = 2
      ELSE IF( PRIM .EQ. GRF__TEXT ) THEN
         J = 3
      ELSE
         WRITE(*,'(A,I2)') 'Bad PRIM value: ', PRIM
      END IF

*  Return the old value.
      OLDVAL = ATTRS( I, J )

*  Store the new value if not bad.
      IF( VAL .NE. AST__BAD ) ATTRS( I, J ) = VAL

*  Initialize the returned value to indicate success.
      REG_ATTR = 1

      END


*  Draw a polyline.
*  ----------------
      INTEGER FUNCTION REG_LINE( N, X, Y )
      IMPLICIT NONE

      INTEGER N
      REAL X( N )
      REAL Y( N )
      INTEGER I

      WRITE(*,'(A,I4)') 'REG_LINE: ',N
      DO I = 1, N
         WRITE(*,'(3X,G10.3,1X,G10.3)') X(I),Y(I)
      END DO

      REG_LINE = 1
      END

*  Draw a set of markers.
*  ----------------------
      INTEGER FUNCTION REG_MARK( N, X, Y, TYPE )
      IMPLICIT NONE

      INTEGER N, TYPE
      REAL X( N )
      REAL Y( N )
      INTEGER I

      WRITE(*,'(A,I4,I2)') 'REG_MARK: ', N, TYPE
      DO I = 1, N
         WRITE(*,'(3X,G10.3,1X,G10.3)') X(I),Y(I)
      END DO

      REG_MARK = 1
      END

*  Draw a text string.
*  -------------------
      INTEGER FUNCTION REG_TEXT( TEXT, X, Y, JUST, UPX, UPY )
      IMPLICIT NONE

      CHARACTER TEXT*(*), JUST*(*)
      REAL X, Y, UPX, UPY

      WRITE(*,'(A,A,A)') 'REG_TEXT: ''', TEXT,''''
      WRITE(*,'(3X,G10.3,1X,G10.3,1X,A,1X,G10.3,1X,G10.3)')
     :      X, Y, JUST, UPX, UPY

      REG_TEXT = 1
      END

*  Return the extent of a text string.
*
*  For some reason, the arguments to this function seem particularly
*  prone to random rounding errors, resulting in the regression test
*  always failing when run twice in succession, even if not changes
*  have been made to the code in plot.c. For this reason this function
*  does not write out its argument to standard output.
*  --------------------------------------------------------------------
      INTEGER FUNCTION REG_TXEXT( TEXT, X, Y, JUST, UPX, UPY, XB, YB )
      IMPLICIT NONE

      CHARACTER TEXT*(*), JUST*(*)
      REAL X, Y, UPX, UPY, XB(4), YB(4)

c      WRITE(*,*) 'REG_TXEXT: ''', TEXT,''''
c      WRITE(*,*) '   ', X, Y, ' ''', JUST,''' ', UPX, UPY

      XB( 1 ) = X - LEN( TEXT )*0.5
      XB( 2 ) = X + LEN( TEXT )*0.5
      XB( 3 ) = XB( 2 )
      XB( 4 ) = XB( 1 )

      YB( 1 ) = Y - 0.5
      YB( 2 ) = YB( 1 )
      YB( 3 ) = Y + 0.5
      YB( 4 ) = YB( 3 )

      REG_TXEXT = 1
      END

*  Inquire a capability
*  ---------------------
      INTEGER FUNCTION REG_CAP( CAP, VALUE )
      IMPLICIT NONE

      INCLUDE 'GRF_PAR'

      INTEGER CAP, VALUE

      WRITE(*,'(A,I2)') 'REG_CAP: ', CAP

      REG_CAP = 0
      IF( CAP .EQ. GRF__SCALES ) REG_CAP = 1

      END

*  Inquire axis scales
*  ---------------------
      INTEGER FUNCTION REG_SCALES( ALPHA, BETA )
      IMPLICIT NONE
      REAL ALPHA, BETA

      WRITE(*,'(A)') 'REG_SCALES: '

      ALPHA = 1.0
      BETA = 1.0

      REG_SCALES = 1

      END

*  Inquire character size
*  ----------------------
      INTEGER FUNCTION REG_QCH( CHV, CHH )
      IMPLICIT NONE
      REAL CHV, CHH

      WRITE(*,'(A)') 'REG_QCH: '

      CHV = 0.01
      CHH = 0.01

      REG_QCH = 1

      END



*  A Sink funtion for use with the FitsChan class. It writes the FitsChan
*  contents to standard output.
*  ======================================================================
      SUBROUTINE REG_SINK( CARD, STATUS )
      IMPLICIT NONE
      CHARACTER CARD*80
      INTEGER STATUS
      WRITE(*,'(A)') CARD
      END



*  A Source funtion for use with the FitsChan class. It returns a different
*  header for each value of REG_FTEST.
*  ======================================================================
      INTEGER FUNCTION REG_SOURCE( CARD, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER CMN_FTEST          ! Which FITS test are we doing?
      INTEGER CMN_LINE           ! The index of the next header to read
      COMMON /REG/ CMN_FTEST, CMN_LINE

      CHARACTER CARD*80
      INTEGER STATUS

* Check the inherited status
      REG_SOURCE = 0
      IF( STATUS .NE. SAI__OK ) RETURN

*  Assume more cards will be returned.
      REG_SOURCE = 1

*  The following code defines the FITS headers and is generated automatically
*  from FITS header files using script make_regtest (in the AST development
*  system)....


*  FITS headers from cobe.head (Tue Jan 29 13:37:07 2002)
      IF( CMN_FTEST .EQ. 1 ) THEN
         IF( CMN_LINE .EQ. 1 ) THEN
            CARD = 'SIMPLE  =                    T / Written by I'//
     :        'DL:  30-Jul-1997 05:35:42.00'
         ELSE IF( CMN_LINE .EQ. 2 ) THEN
            CARD = 'BITPIX  =                  -32 / Bits per pix'//
     :        'el.'
         ELSE IF( CMN_LINE .EQ. 3 ) THEN
            CARD = 'NAXIS   =                    2 / Number of di'//
     :        'mensions'
         ELSE IF( CMN_LINE .EQ. 4 ) THEN
            CARD = 'NAXIS1  =                  300 / Length of x '//
     :        'axis.'
         ELSE IF( CMN_LINE .EQ. 5 ) THEN
            CARD = 'NAXIS2  =                  300 / Length of y '//
     :        'axis.'
         ELSE IF( CMN_LINE .EQ. 6 ) THEN
            CARD = 'CTYPE1  = ''GLON-ZEA''           / X-axis typ'//
     :        'e'
         ELSE IF( CMN_LINE .EQ. 7 ) THEN
            CARD = 'CTYPE2  = ''GLAT-ZEA''           / Y-axis typ'//
     :        'e'
         ELSE IF( CMN_LINE .EQ. 8 ) THEN
            CARD = 'CRVAL1  =           -149.56866 / Reference pi'//
     :        'xel value'
         ELSE IF( CMN_LINE .EQ. 9 ) THEN
            CARD = 'CRVAL2  =           -19.758201 / Reference pi'//
     :        'xel value'
         ELSE IF( CMN_LINE .EQ. 10 ) THEN
            CARD = 'CRPIX1  =              150.500 / Reference pi'//
     :        'xel'
         ELSE IF( CMN_LINE .EQ. 11 ) THEN
            CARD = 'CRPIX2  =              150.500 / Reference pi'//
     :        'xel'
         ELSE IF( CMN_LINE .EQ. 12 ) THEN
            CARD = 'CDELT1  =             -1.20000 / Degrees/pixe'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 13 ) THEN
            CARD = 'CDELT2  =              1.20000 / Degrees/pixe'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 14 ) THEN
            CARD = 'CROTA1  =              0.00000 / Rotation in '//
     :        'degrees.'
         ELSE IF( CMN_LINE .EQ. 15 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 16 ) THEN
            CARD = 'COMMENT This file was produced by the SkyView'//
     :        ' survey analysis system from'
         ELSE IF( CMN_LINE .EQ. 17 ) THEN
            CARD = 'COMMENT available astronomical surveys.  The '//
     :        'data are formatted'
         ELSE IF( CMN_LINE .EQ. 18 ) THEN
            CARD = 'COMMENT as a simple two-dimensional FITS imag'//
     :        'e with the same units as'
         ELSE IF( CMN_LINE .EQ. 19 ) THEN
            CARD = 'COMMENT the orginal survey.  A single ASCII t'//
     :        'able extension may be present'
         ELSE IF( CMN_LINE .EQ. 20 ) THEN
            CARD = 'COMMENT which describes catalog objects found'//
     :        ' within the field of view.'
         ELSE IF( CMN_LINE .EQ. 21 ) THEN
            CARD = 'COMMENT Copies of relevant copyright notices '//
     :        'are included in this file.'
         ELSE IF( CMN_LINE .EQ. 22 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 23 ) THEN
            CARD = 'COMMENT Questions should be directed to:'
         ELSE IF( CMN_LINE .EQ. 24 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 25 ) THEN
            CARD = 'COMMENT     scollick@skyview.gsfc.nasa.gov'
         ELSE IF( CMN_LINE .EQ. 26 ) THEN
            CARD = 'COMMENT          or'
         ELSE IF( CMN_LINE .EQ. 27 ) THEN
            CARD = 'COMMENT     mcglynn@grossc.gsfc.nasa.gov'
         ELSE IF( CMN_LINE .EQ. 28 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 29 ) THEN
            CARD = 'COMMENT     SkyView'
         ELSE IF( CMN_LINE .EQ. 30 ) THEN
            CARD = 'COMMENT     Code 668.1'
         ELSE IF( CMN_LINE .EQ. 31 ) THEN
            CARD = 'COMMENT     Goddard Space Flight Center, Gree'//
     :        'nbelt, MD 20771'
         ELSE IF( CMN_LINE .EQ. 32 ) THEN
            CARD = 'COMMENT     301-286-7780'
         ELSE IF( CMN_LINE .EQ. 33 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 34 ) THEN
            CARD = 'COMMENT SkyView is supported by NASA ADP gran'//
     :        't NAS 5-32068.'
         ELSE IF( CMN_LINE .EQ. 35 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 36 ) THEN
            CARD = 'SURVEY  = ''COBE DIRBE'''
         ELSE IF( CMN_LINE .EQ. 37 ) THEN
            CARD = 'BUNITS  = ''MJy/sr  ''           /'
         ELSE IF( CMN_LINE .EQ. 38 ) THEN
            CARD = 'ORIGIN  = ''CDAC    ''           / Cosmology '//
     :        'Data Analysis Center'
         ELSE IF( CMN_LINE .EQ. 39 ) THEN
            CARD = 'TELESCOP= ''COBE    ''           / COsmic Bac'//
     :        'kground Explorer satellite'
         ELSE IF( CMN_LINE .EQ. 40 ) THEN
            CARD = 'INSTRUME= ''DIRBE   ''           / COBE instr'//
     :        'ument [DIRBE, DMR, FIRAS]'
         ELSE IF( CMN_LINE .EQ. 41 ) THEN
            CARD = 'PIXRESOL=                    9 / Quad tree pi'//
     :        'xel resolution [6, 9]'
         ELSE IF( CMN_LINE .EQ. 42 ) THEN
            CARD = 'DATE    = ''27/09/94''           / FITS file '//
     :        'creation date (dd/mm/yy)'
         ELSE IF( CMN_LINE .EQ. 43 ) THEN
            CARD = 'DATE-MAP= ''16/09/94''           / Date of or'//
     :        'iginal file creation (dd/mm/yy)'
         ELSE IF( CMN_LINE .EQ. 44 ) THEN
            CARD = 'COMMENT     COBE specific keywords'
         ELSE IF( CMN_LINE .EQ. 45 ) THEN
            CARD = 'DATE-BEG= ''08/12/89''           / date of in'//
     :        'itial data represented (dd/mm/yy)'
         ELSE IF( CMN_LINE .EQ. 46 ) THEN
            CARD = 'DATE-END= ''25/09/90''           / date of fi'//
     :        'nal data represented   (dd/mm/yy)'
         ELSE IF( CMN_LINE .EQ. 47 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 48 ) THEN
            CARD = 'COMMENT THE COBE DIRBE map is a combination o'//
     :        'f the original ten'
         ELSE IF( CMN_LINE .EQ. 49 ) THEN
            CARD = 'COMMENT band passes with the following wavele'//
     :        'ngths:'
         ELSE IF( CMN_LINE .EQ. 50 ) THEN
            CARD = 'COMMENT     Band 1  - 1.25 microns'
         ELSE IF( CMN_LINE .EQ. 51 ) THEN
            CARD = 'COMMENT     Band 2  - 2.2  microns'
         ELSE IF( CMN_LINE .EQ. 52 ) THEN
            CARD = 'COMMENT     Band 3  - 3.5  microns'
         ELSE IF( CMN_LINE .EQ. 53 ) THEN
            CARD = 'COMMENT     Band 4  - 4.9  microns'
         ELSE IF( CMN_LINE .EQ. 54 ) THEN
            CARD = 'COMMENT     Band 5  -  12  microns'
         ELSE IF( CMN_LINE .EQ. 55 ) THEN
            CARD = 'COMMENT     Band 6  -  25  microns'
         ELSE IF( CMN_LINE .EQ. 56 ) THEN
            CARD = 'COMMENT     Band 7  -  60  microns'
         ELSE IF( CMN_LINE .EQ. 57 ) THEN
            CARD = 'COMMENT     Band 8  - 100  microns'
         ELSE IF( CMN_LINE .EQ. 58 ) THEN
            CARD = 'COMMENT     Band 9  - 140  microns'
         ELSE IF( CMN_LINE .EQ. 59 ) THEN
            CARD = 'COMMENT     Band 10 - 240  microns'
         ELSE IF( CMN_LINE .EQ. 60 ) THEN
            CARD = 'COMMENT'
         ELSE IF( CMN_LINE .EQ. 61 ) THEN
            CARD = 'END'
            REG_SOURCE = 0
         ELSE
            REG_SOURCE = 0
         END IF

*  FITS headers from polco.head (Tue Jan 29 15:06:35 2002)
      ELSE IF( CMN_FTEST .EQ. 2 ) THEN
         IF( CMN_LINE .EQ. 1 ) THEN
            CARD = 'COMMENT AST +++++++++++++++++++++++++++++++++'//
     :        '+++++++++++++++++++++++++++++++'
         ELSE IF( CMN_LINE .EQ. 2 ) THEN
            CARD = 'AST'
         ELSE IF( CMN_LINE .EQ. 3 ) THEN
            CARD = 'COMMENT AST            Beginning of AST data '//
     :        'for FrameSet object'
         ELSE IF( CMN_LINE .EQ. 4 ) THEN
            CARD = 'AST'
         ELSE IF( CMN_LINE .EQ. 5 ) THEN
            CARD = 'COMMENT AST .................................'//
     :        '...............................'
         ELSE IF( CMN_LINE .EQ. 6 ) THEN
            CARD = 'AST'
         ELSE IF( CMN_LINE .EQ. 7 ) THEN
            CARD = 'BEGAST_A= ''FrameSet''           / Set of int'//
     :        'er-related coordinate systems'
         ELSE IF( CMN_LINE .EQ. 8 ) THEN
            CARD = 'NFRAME_A=                    2 / Number of Fr'//
     :        'ames in FrameSet'
         ELSE IF( CMN_LINE .EQ. 9 ) THEN
            CARD = 'CURRNT_A=                    2 / Index of cur'//
     :        'rent Frame'
         ELSE IF( CMN_LINE .EQ. 10 ) THEN
            CARD = 'NOD1_A  =                    2 / Frame 1 is a'//
     :        'ssociated with node 2'
         ELSE IF( CMN_LINE .EQ. 11 ) THEN
            CARD = 'NOD2_A  =                    1 / Frame 2 is a'//
     :        'ssociated with node 1'
         ELSE IF( CMN_LINE .EQ. 12 ) THEN
            CARD = 'LNK2_A  =                    1 / Node 2 is de'//
     :        'rived from node 1'
         ELSE IF( CMN_LINE .EQ. 13 ) THEN
            CARD = 'FRM1_A  = ''        ''           / Frame numb'//
     :        'er 1'
         ELSE IF( CMN_LINE .EQ. 14 ) THEN
            CARD = 'BEGAST_B= ''Frame   ''           / Coordinate'//
     :        ' system description'
         ELSE IF( CMN_LINE .EQ. 15 ) THEN
            CARD = 'TITLE_A = ''Data grid indices; first pixel at'//
     :        ' (1&''/ Title of coordinate system'
         ELSE IF( CMN_LINE .EQ. 16 ) THEN
            CARD = 'CONTINUE  '',1)     '''
         ELSE IF( CMN_LINE .EQ. 17 ) THEN
            CARD = 'NAXES_A =                    2 / Number of co'//
     :        'ordinate axes'
         ELSE IF( CMN_LINE .EQ. 18 ) THEN
            CARD = 'DOMAIN_A= ''GRID    ''           / Coordinate'//
     :        ' system domain'
         ELSE IF( CMN_LINE .EQ. 19 ) THEN
            CARD = 'AX1_A   = ''        ''           / Axis numbe'//
     :        'r 1'
         ELSE IF( CMN_LINE .EQ. 20 ) THEN
            CARD = 'BEGAST_C= ''Axis    ''           / Coordinate'//
     :        ' axis'
         ELSE IF( CMN_LINE .EQ. 21 ) THEN
            CARD = 'LABEL_A = ''Data grid index 1''  / Axis Label'
         ELSE IF( CMN_LINE .EQ. 22 ) THEN
            CARD = 'SYMBOL_A= ''g1      ''           / Axis symbo'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 23 ) THEN
            CARD = 'UNIT_A  = ''pixel   ''           / Axis units'
         ELSE IF( CMN_LINE .EQ. 24 ) THEN
            CARD = 'FORMAT_A= ''%3.1f   ''           / Format spe'//
     :        'cifier'
         ELSE IF( CMN_LINE .EQ. 25 ) THEN
            CARD = 'ENDAST_A= ''Axis    ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 26 ) THEN
            CARD = 'AX2_A   = ''        ''           / Axis numbe'//
     :        'r 2'
         ELSE IF( CMN_LINE .EQ. 27 ) THEN
            CARD = 'BEGAST_D= ''Axis    ''           / Coordinate'//
     :        ' axis'
         ELSE IF( CMN_LINE .EQ. 28 ) THEN
            CARD = 'LABEL_B = ''Data grid index 2''  / Axis Label'
         ELSE IF( CMN_LINE .EQ. 29 ) THEN
            CARD = 'SYMBOL_B= ''g2      ''           / Axis symbo'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 30 ) THEN
            CARD = 'UNIT_B  = ''pixel   ''           / Axis units'
         ELSE IF( CMN_LINE .EQ. 31 ) THEN
            CARD = 'FORMAT_B= ''%3.1f   ''           / Format spe'//
     :        'cifier'
         ELSE IF( CMN_LINE .EQ. 32 ) THEN
            CARD = 'ENDAST_B= ''Axis    ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 33 ) THEN
            CARD = 'ENDAST_C= ''Frame   ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 34 ) THEN
            CARD = 'FRM2_A  = ''        ''           / Frame numb'//
     :        'er 2'
         ELSE IF( CMN_LINE .EQ. 35 ) THEN
            CARD = 'BEGAST_E= ''Frame   ''           / Coordinate'//
     :        ' system description'
         ELSE IF( CMN_LINE .EQ. 36 ) THEN
            CARD = 'TITLE_B = ''Pixel coordinates; first pixel at'//
     :        ' (-&''/ Title of coordinate system'
         ELSE IF( CMN_LINE .EQ. 37 ) THEN
            CARD = 'CONTINUE  ''100.5,-200.5)'''
         ELSE IF( CMN_LINE .EQ. 38 ) THEN
            CARD = 'NAXES_B =                    2 / Number of co'//
     :        'ordinate axes'
         ELSE IF( CMN_LINE .EQ. 39 ) THEN
            CARD = 'DOMAIN_B= ''POLAR   ''           / Coordinate'//
     :        ' system domain'
         ELSE IF( CMN_LINE .EQ. 40 ) THEN
            CARD = 'AX1_B   = ''        ''           / Axis numbe'//
     :        'r 1'
         ELSE IF( CMN_LINE .EQ. 41 ) THEN
            CARD = 'BEGAST_F= ''Axis    ''           / Coordinate'//
     :        ' axis'
         ELSE IF( CMN_LINE .EQ. 42 ) THEN
            CARD = 'LABEL_C = ''Pixel coordinate 1'' / Axis Label'
         ELSE IF( CMN_LINE .EQ. 43 ) THEN
            CARD = 'SYMBOL_C= ''p1      ''           / Axis symbo'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 44 ) THEN
            CARD = 'UNIT_C  = ''pixel   ''           / Axis units'
         ELSE IF( CMN_LINE .EQ. 45 ) THEN
            CARD = 'FORMAT_C= ''%3.1f   ''           / Format spe'//
     :        'cifier'
         ELSE IF( CMN_LINE .EQ. 46 ) THEN
            CARD = 'ENDAST_D= ''Axis    ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 47 ) THEN
            CARD = 'AX2_B   = ''        ''           / Axis numbe'//
     :        'r 2'
         ELSE IF( CMN_LINE .EQ. 48 ) THEN
            CARD = 'BEGAST_G= ''Axis    ''           / Coordinate'//
     :        ' axis'
         ELSE IF( CMN_LINE .EQ. 49 ) THEN
            CARD = 'LABEL_D = ''Pixel coordinate 2'' / Axis Label'
         ELSE IF( CMN_LINE .EQ. 50 ) THEN
            CARD = 'SYMBOL_D= ''p2      ''           / Axis symbo'//
     :        'l'
         ELSE IF( CMN_LINE .EQ. 51 ) THEN
            CARD = 'UNIT_D  = ''pixel   ''           / Axis units'
         ELSE IF( CMN_LINE .EQ. 52 ) THEN
            CARD = 'FORMAT_D= ''%3.1f   ''           / Format spe'//
     :        'cifier'
         ELSE IF( CMN_LINE .EQ. 53 ) THEN
            CARD = 'ENDAST_E= ''Axis    ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 54 ) THEN
            CARD = 'ENDAST_F= ''Frame   ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 55 ) THEN
            CARD = 'MAP2_A  = ''        ''           / Mapping be'//
     :        'tween nodes 1 and 2'
         ELSE IF( CMN_LINE .EQ. 56 ) THEN
            CARD = 'BEGAST_H= ''CmpMap  ''           / Compound M'//
     :        'apping'
         ELSE IF( CMN_LINE .EQ. 57 ) THEN
            CARD = 'NIN_A   =                    2 / Number of in'//
     :        'put coordinates'
         ELSE IF( CMN_LINE .EQ. 58 ) THEN
            CARD = 'ISA_A   = ''Mapping ''           / Mapping be'//
     :        'tween coordinate systems'
         ELSE IF( CMN_LINE .EQ. 59 ) THEN
            CARD = 'INVA_A  =                    1 / First Mappin'//
     :        'g used in inverse direction'
         ELSE IF( CMN_LINE .EQ. 60 ) THEN
            CARD = 'INVB_A  =                    1 / Second Mappi'//
     :        'ng used in inverse direction'
         ELSE IF( CMN_LINE .EQ. 61 ) THEN
            CARD = 'MAPA_A  = ''        ''           / First comp'//
     :        'onent Mapping'
         ELSE IF( CMN_LINE .EQ. 62 ) THEN
            CARD = 'BEGAST_I= ''MathMap ''           / Transforma'//
     :        'tion using mathematical functions'
         ELSE IF( CMN_LINE .EQ. 63 ) THEN
            CARD = 'NIN_B   =                    2 / Number of in'//
     :        'put coordinates'
         ELSE IF( CMN_LINE .EQ. 64 ) THEN
            CARD = 'INVERT_A=                    0 / Mapping not '//
     :        'inverted'
         ELSE IF( CMN_LINE .EQ. 65 ) THEN
            CARD = 'ISA_B   = ''Mapping ''           / Mapping be'//
     :        'tween coordinate systems'
         ELSE IF( CMN_LINE .EQ. 66 ) THEN
            CARD = 'FWD1_A  = ''r=sqrt(x*x+y*y)''    / Forward fu'//
     :        'nction 1'
         ELSE IF( CMN_LINE .EQ. 67 ) THEN
            CARD = 'FWD2_A  = ''theta=atan2(y,x)''   / Forward fu'//
     :        'nction 2'
         ELSE IF( CMN_LINE .EQ. 68 ) THEN
            CARD = 'INV1_A  = ''x=r*cos(theta)''     / Inverse fu'//
     :        'nction 1'
         ELSE IF( CMN_LINE .EQ. 69 ) THEN
            CARD = 'INV2_A  = ''y=r*sin(theta)''     / Inverse fu'//
     :        'nction 2'
         ELSE IF( CMN_LINE .EQ. 70 ) THEN
            CARD = 'SIMPFI_A=                    1 / Forward-inve'//
     :        'rse pairs may simplify'
         ELSE IF( CMN_LINE .EQ. 71 ) THEN
            CARD = 'SIMPIF_A=                    1 / Inverse-forw'//
     :        'ard pairs may simplify'
         ELSE IF( CMN_LINE .EQ. 72 ) THEN
            CARD = 'ENDAST_G= ''MathMap ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 73 ) THEN
            CARD = 'MAPB_A  = ''        ''           / Second com'//
     :        'ponent Mapping'
         ELSE IF( CMN_LINE .EQ. 74 ) THEN
            CARD = 'BEGAST_J= ''WinMap  ''           / Map one wi'//
     :        'ndow on to another'
         ELSE IF( CMN_LINE .EQ. 75 ) THEN
            CARD = 'NIN_C   =                    2 / Number of in'//
     :        'put coordinates'
         ELSE IF( CMN_LINE .EQ. 76 ) THEN
            CARD = 'INVERT_B=                    0 / Mapping not '//
     :        'inverted'
         ELSE IF( CMN_LINE .EQ. 77 ) THEN
            CARD = 'ISA_C   = ''Mapping ''           / Mapping be'//
     :        'tween coordinate systems'
         ELSE IF( CMN_LINE .EQ. 78 ) THEN
            CARD = 'SFT1_A  =               -101.5 / Shift for ax'//
     :        'is 1'
         ELSE IF( CMN_LINE .EQ. 79 ) THEN
            CARD = 'SFT2_A  =               -201.5 / Shift for ax'//
     :        'is 2'
         ELSE IF( CMN_LINE .EQ. 80 ) THEN
            CARD = 'ENDAST_H= ''WinMap  ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 81 ) THEN
            CARD = 'ENDAST_I= ''CmpMap  ''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 82 ) THEN
            CARD = 'ENDAST_J= ''FrameSet''           / End of obj'//
     :        'ect definition'
         ELSE IF( CMN_LINE .EQ. 83 ) THEN
            CARD = 'COMMENT AST .................................'//
     :        '...............................'
         ELSE IF( CMN_LINE .EQ. 84 ) THEN
            CARD = 'AST'
         ELSE IF( CMN_LINE .EQ. 85 ) THEN
            CARD = 'COMMENT AST               End of AST data for'//
     :        ' FrameSet object'
         ELSE IF( CMN_LINE .EQ. 86 ) THEN
            CARD = 'AST'
         ELSE IF( CMN_LINE .EQ. 87 ) THEN
            CARD = 'COMMENT AST ---------------------------------'//
     :        '-------------------------------'
         ELSE IF( CMN_LINE .EQ. 88 ) THEN
            CARD = 'AST'
            REG_SOURCE = 0
         ELSE
            REG_SOURCE = 0
         END IF


*  FITS headers from scp.head (Tue Jan 29 15:17:50 2002)
      ELSE IF( CMN_FTEST .EQ. 3 ) THEN
         IF( CMN_LINE .EQ. 1 ) THEN
            CARD = 'SIMPLE  =                    T / file does co'//
     :        'nform to FITS standard'
         ELSE IF( CMN_LINE .EQ. 2 ) THEN
            CARD = 'BITPIX  =                   16 / number of bi'//
     :        'ts per data pixel'
         ELSE IF( CMN_LINE .EQ. 3 ) THEN
            CARD = 'NAXIS   =                    2 / number of da'//
     :        'ta axes'
         ELSE IF( CMN_LINE .EQ. 4 ) THEN
            CARD = 'NAXIS1  =                 1787 / length of da'//
     :        'ta axis 1'
         ELSE IF( CMN_LINE .EQ. 5 ) THEN
            CARD = 'NAXIS2  =                  447 / length of da'//
     :        'ta axis 2'
         ELSE IF( CMN_LINE .EQ. 6 ) THEN
            CARD = 'EXTEND  =                    T / FITS dataset'//
     :        ' may contain extensions'
         ELSE IF( CMN_LINE .EQ. 7 ) THEN
            CARD = 'COMMENT   FITS (Flexible Image Transport Syst'//
     :        'em) format defined in Astronomy and'
         ELSE IF( CMN_LINE .EQ. 8 ) THEN
            CARD = 'COMMENT   Astrophysics Supplement Series v44/'//
     :        'p363, v44/p371, v73/p359, v73/p365.'
         ELSE IF( CMN_LINE .EQ. 9 ) THEN
            CARD = 'COMMENT   Contact the NASA Science Office of '//
     :        'Standards and Technology for the'
         ELSE IF( CMN_LINE .EQ. 10 ) THEN
            CARD = 'COMMENT   FITS Definition document #100 and o'//
     :        'ther FITS information.'
         ELSE IF( CMN_LINE .EQ. 11 ) THEN
            CARD = 'PLATENUM= ''3665    ''           / Plate numb'//
     :        'er'
         ELSE IF( CMN_LINE .EQ. 12 ) THEN
            CARD = 'EMULSION= ''IIIaJ   ''           / Kodak emul'//
     :        'sion type'
         ELSE IF( CMN_LINE .EQ. 13 ) THEN
            CARD = 'FILTER  = ''GG395   ''           / Schott gla'//
     :        'ss filter type'
         ELSE IF( CMN_LINE .EQ. 14 ) THEN
            CARD = 'PLTSCALE= ''67.14   ''           / [arcsec/mm'//
     :        '] plate scale'
         ELSE IF( CMN_LINE .EQ. 15 ) THEN
            CARD = 'FIELDNUM= ''1       ''           / Sky survey'//
     :        ' field number'
         ELSE IF( CMN_LINE .EQ. 16 ) THEN
            CARD = 'EPOCH   =         1.977780E+03 / Epoch of obs'//
     :        'ervation'
         ELSE IF( CMN_LINE .EQ. 17 ) THEN
            CARD = 'DATE-OBS= ''1977-10-11''         / [yyyy-mm-d'//
     :        'd] UT date of observation'
         ELSE IF( CMN_LINE .EQ. 18 ) THEN
            CARD = 'TELESCOP= ''UKST    ''           / Telescope '//
     :        'on which the plate was taken'
         ELSE IF( CMN_LINE .EQ. 19 ) THEN
            CARD = 'TELETYPE= ''SCHM    ''           / Type of te'//
     :        'lescope'
         ELSE IF( CMN_LINE .EQ. 20 ) THEN
            CARD = 'SITELAT =  -5.458410576565E-01 / [radians] la'//
     :        'titude of telescope'
         ELSE IF( CMN_LINE .EQ. 21 ) THEN
            CARD = 'SITELONG=   2.601766194458E+00 / [radians] lo'//
     :        'ngitude of telescope'
         ELSE IF( CMN_LINE .EQ. 22 ) THEN
            CARD = 'LST     = ''00:20   ''           / [hh:mm] lo'//
     :        'cal sidereal time at start of obs'
         ELSE IF( CMN_LINE .EQ. 23 ) THEN
            CARD = 'MJD-OBS =   4.342657300880E+04 / Modified Jul'//
     :        'ian Date of observation'
         ELSE IF( CMN_LINE .EQ. 24 ) THEN
            CARD = 'INSTRUME= ''SuperCOSMOS I''      / Measuring '//
     :        'machine'
         ELSE IF( CMN_LINE .EQ. 25 ) THEN
            CARD = 'DATE-MES= ''2000-11-04''         / [yyyy-mm-d'//
     :        'd] Date of this plate measurement'
         ELSE IF( CMN_LINE .EQ. 26 ) THEN
            CARD = 'RADECSYS= ''FK5     ''           / Reference '//
     :        'frame for RA/DEC in original file'
         ELSE IF( CMN_LINE .EQ. 27 ) THEN
            CARD = 'NHKLINES=                  146 / Number of li'//
     :        'nes from house-keeping file'
         ELSE IF( CMN_LINE .EQ. 28 ) THEN
            CARD = 'HKLIN001= ''JOB.JOBNO                UKJ001'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 29 ) THEN
            CARD = 'HKLIN002= ''JOB.DATE-MES             2000:11:'//
     :        '04'' /'
         ELSE IF( CMN_LINE .EQ. 30 ) THEN
            CARD = 'HKLIN003= ''JOB.TIME                 12:51:09'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 31 ) THEN
            CARD = 'HKLIN004= ''JOB.INSTRUME             SuperCOS'//
     :        'MOS I'' /'
         ELSE IF( CMN_LINE .EQ. 32 ) THEN
            CARD = 'HKLIN005= ''JOB.ORIGIN               Royal Ob'//
     :        'servatory Edinburgh'' /'
         ELSE IF( CMN_LINE .EQ. 33 ) THEN
            CARD = 'HKLIN006= ''JOB.SOFTWARE             /home/sc'//
     :        'osdev/v033'' /'
         ELSE IF( CMN_LINE .EQ. 34 ) THEN
            CARD = 'HKLIN007= ''JOB.OPERATOR             ebt'' /'
         ELSE IF( CMN_LINE .EQ. 35 ) THEN
            CARD = 'HKLIN008= ''JOB.USER                 htm'' /'
         ELSE IF( CMN_LINE .EQ. 36 ) THEN
            CARD = 'HKLIN009= ''JOB.USERREF              NONE'' /'
         ELSE IF( CMN_LINE .EQ. 37 ) THEN
            CARD = 'HKLIN010= ''JOB.UORIGIN              ROE'' /'
         ELSE IF( CMN_LINE .EQ. 38 ) THEN
            CARD = 'HKLIN011= ''JOB.UCOUNTRY             uk'' /'
         ELSE IF( CMN_LINE .EQ. 39 ) THEN
            CARD = 'HKLIN012= ''JOB.COMMENT              Digital '//
     :        'catalogue of the Sky'' /'
         ELSE IF( CMN_LINE .EQ. 40 ) THEN
            CARD = 'HKLIN013= ''JOB.IAM_FILE             iam.srt''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 41 ) THEN
            CARD = 'HKLIN014= ''PLATE.TELESCOP           UKST'' /'
         ELSE IF( CMN_LINE .EQ. 42 ) THEN
            CARD = 'HKLIN015= ''PLATE.TELTYPE            SCHM'' /'
         ELSE IF( CMN_LINE .EQ. 43 ) THEN
            CARD = 'HKLIN016= ''PLATE.PLATE              3665'' /'
         ELSE IF( CMN_LINE .EQ. 44 ) THEN
            CARD = 'HKLIN017= ''PLATE.MATERIAL           3mm glas'//
     :        's'' /'
         ELSE IF( CMN_LINE .EQ. 45 ) THEN
            CARD = 'HKLIN018= ''PLATE.EMULSION           IIIaJ'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 46 ) THEN
            CARD = 'HKLIN019= ''PLATE.FILTER             GG395'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 47 ) THEN
            CARD = 'HKLIN020= ''PLATE.PSCALE             67.14'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 48 ) THEN
            CARD = 'HKLIN021= ''PLATE.FIELD              1'' /'
         ELSE IF( CMN_LINE .EQ. 49 ) THEN
            CARD = 'HKLIN022= ''PLATE.RA_PNT             0'' /'
         ELSE IF( CMN_LINE .EQ. 50 ) THEN
            CARD = 'HKLIN023= ''PLATE.DEC_PNT            -90'' /'
         ELSE IF( CMN_LINE .EQ. 51 ) THEN
            CARD = 'HKLIN024= ''PLATE.RADECSYS           FK4'' /'
         ELSE IF( CMN_LINE .EQ. 52 ) THEN
            CARD = 'HKLIN025= ''PLATE.EQUINOX            1950'' /'
         ELSE IF( CMN_LINE .EQ. 53 ) THEN
            CARD = 'HKLIN026= ''PLATE.TIMESYS            BESSELIA'//
     :        'N'' /'
         ELSE IF( CMN_LINE .EQ. 54 ) THEN
            CARD = 'HKLIN027= ''PLATE.EPOCH              1977.78''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 55 ) THEN
            CARD = 'HKLIN028= ''PLATE.EXPOSURE           75'' /'
         ELSE IF( CMN_LINE .EQ. 56 ) THEN
            CARD = 'HKLIN029= ''PLATE.UTDATE             771011'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 57 ) THEN
            CARD = 'HKLIN030= ''PLATE.LST                0020'' /'
         ELSE IF( CMN_LINE .EQ. 58 ) THEN
            CARD = 'HKLIN031= ''PLATE.MJD                43426.57'//
     :        '3008796'' /'
         ELSE IF( CMN_LINE .EQ. 59 ) THEN
            CARD = 'HKLIN032= ''PLATE.TELLAT             -0.54584'//
     :        '105765654'' /'
         ELSE IF( CMN_LINE .EQ. 60 ) THEN
            CARD = 'HKLIN033= ''PLATE.TELLONG            2.601766'//
     :        '1944583'' /'
         ELSE IF( CMN_LINE .EQ. 61 ) THEN
            CARD = 'HKLIN034= ''PLATE.TELHT              1145'' /'
         ELSE IF( CMN_LINE .EQ. 62 ) THEN
            CARD = 'HKLIN035= ''PLATE.TEMP               273.155''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 63 ) THEN
            CARD = 'HKLIN036= ''PLATE.ATMOSP             1013.25''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 64 ) THEN
            CARD = 'HKLIN037= ''PLATE.HUMID              0.5'' /'
         ELSE IF( CMN_LINE .EQ. 65 ) THEN
            CARD = 'HKLIN038= ''PLATE.WAVE               4500'' /'
         ELSE IF( CMN_LINE .EQ. 66 ) THEN
            CARD = 'HKLIN039= ''PLATE.TROPL              0.0065'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 67 ) THEN
            CARD = 'HKLIN040= ''CALIBRATION.CALTYPE      SPLINE'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 68 ) THEN
            CARD = 'HKLIN041= ''CALIBRATION.STEPWEDG     KPNO'' /'
         ELSE IF( CMN_LINE .EQ. 69 ) THEN
            CARD = 'HKLIN042= ''CALIBRATION.NSTEPS       8'' /'
         ELSE IF( CMN_LINE .EQ. 70 ) THEN
            CARD = 'HKLIN043= ''MEASUREMENT.ORIENTAT     news'' /'
         ELSE IF( CMN_LINE .EQ. 71 ) THEN
            CARD = 'HKLIN044= ''MEASUREMENT.EMULPOS      UP'' /'
         ELSE IF( CMN_LINE .EQ. 72 ) THEN
            CARD = 'HKLIN045= ''MEASUREMENT.SCANFILT     14'' /'
         ELSE IF( CMN_LINE .EQ. 73 ) THEN
            CARD = 'HKLIN046= ''MEASUREMENT.SOSP         552'' /'
         ELSE IF( CMN_LINE .EQ. 74 ) THEN
            CARD = 'HKLIN047= ''MEASUREMENT.STEPSIZE     10'' /'
         ELSE IF( CMN_LINE .EQ. 75 ) THEN
            CARD = 'HKLIN048= ''MEASUREMENT.SCANLEN      1152'' /'
         ELSE IF( CMN_LINE .EQ. 76 ) THEN
            CARD = 'HKLIN049= ''MEASUREMENT.A-XMIN       1622000''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 77 ) THEN
            CARD = 'HKLIN050= ''MEASUREMENT.A-YMIN       1622000''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 78 ) THEN
            CARD = 'HKLIN051= ''MEASUREMENT.A-XMAX       33878000'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 79 ) THEN
            CARD = 'HKLIN052= ''MEASUREMENT.A-YMAX       33878000'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 80 ) THEN
            CARD = 'HKLIN053= ''MEASUREMENT.X_PNT        17500000'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 81 ) THEN
            CARD = 'HKLIN054= ''MEASUREMENT.Y_PNT        18000000'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 82 ) THEN
            CARD = 'HKLIN055= ''ANALYSIS.NPARAMS         32'' /'
         ELSE IF( CMN_LINE .EQ. 83 ) THEN
            CARD = 'HKLIN056= ''ANALYSIS.AREACUT         8'' /'
         ELSE IF( CMN_LINE .EQ. 84 ) THEN
            CARD = 'HKLIN057= ''ANALYSIS.AP-PARAM        1.07'' /'
         ELSE IF( CMN_LINE .EQ. 85 ) THEN
            CARD = 'HKLIN058= ''DEBLEND.DB-PARAM         1.05'' /'
         ELSE IF( CMN_LINE .EQ. 86 ) THEN
            CARD = 'HKLIN059= ''DEBLEND.DB-AMIN          16'' /'
         ELSE IF( CMN_LINE .EQ. 87 ) THEN
            CARD = 'HKLIN060= ''DEBLEND.DB-AMAX          100000'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 88 ) THEN
            CARD = 'HKLIN061= ''DEBLEND.DB-ACUT          8'' /'
         ELSE IF( CMN_LINE .EQ. 89 ) THEN
            CARD = 'HKLIN062= ''DEBLEND.DB-LEVEL         16'' /'
         ELSE IF( CMN_LINE .EQ. 90 ) THEN
            CARD = 'HKLIN063= ''DEBLEND.SELECT           PARENT+C'//
     :        'HILD'' /'
         ELSE IF( CMN_LINE .EQ. 91 ) THEN
            CARD = 'HKLIN064= ''SKY.SKYSQUAR             64'' /'
         ELSE IF( CMN_LINE .EQ. 92 ) THEN
            CARD = 'HKLIN065= ''SKY.SKYDEFN              MEDIAN'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 93 ) THEN
            CARD = 'HKLIN066= ''SKY.SKYFILTR             bdkjunk''//
     :        '' /'
         ELSE IF( CMN_LINE .EQ. 94 ) THEN
            CARD = 'HKLIN067= ''SKY.F-THRESH             8'' /'
         ELSE IF( CMN_LINE .EQ. 95 ) THEN
            CARD = 'HKLIN068= ''SKY.F-SCLEN              4'' /'
         ELSE IF( CMN_LINE .EQ. 96 ) THEN
            CARD = 'HKLIN069= ''THRESHOLDING.PCUT        10'' /'
         ELSE IF( CMN_LINE .EQ. 97 ) THEN
            CARD = 'HKLIN070= ''IAMQC.AREAMIN            8'' /'
         ELSE IF( CMN_LINE .EQ. 98 ) THEN
            CARD = 'HKLIN071= ''IAMQC.AREAMAX            77346'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 99 ) THEN
            CARD = 'HKLIN072= ''IAMQC.MINMAG             -30515'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 100 ) THEN
            CARD = 'HKLIN073= ''IAMQC.MAXMAG             -17954'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 101 ) THEN
            CARD = 'HKLIN074= ''IAMQC.MINELL             0.000415'//
     :        '6232'' /'
         ELSE IF( CMN_LINE .EQ. 102 ) THEN
            CARD = 'HKLIN075= ''IAMQC.MAXELL             1'' /'
         ELSE IF( CMN_LINE .EQ. 103 ) THEN
            CARD = 'HKLIN076= ''IAMQC.MODELL             0.14'' /'
         ELSE IF( CMN_LINE .EQ. 104 ) THEN
            CARD = 'HKLIN077= ''IAMQC.MODOR              91'' /'
         ELSE IF( CMN_LINE .EQ. 105 ) THEN
            CARD = 'HKLIN078= ''IAMQC.MIDELL             0.21'' /'
         ELSE IF( CMN_LINE .EQ. 106 ) THEN
            CARD = 'HKLIN079= ''IAMQC.MIDOR              93'' /'
         ELSE IF( CMN_LINE .EQ. 107 ) THEN
            CARD = 'HKLIN080= ''IAMQC.MEANELL            0.246703'//
     :        '7'' /'
         ELSE IF( CMN_LINE .EQ. 108 ) THEN
            CARD = 'HKLIN081= ''IAMQC.MEANOR             91.63474'//
     :        ''' /'
         ELSE IF( CMN_LINE .EQ. 109 ) THEN
            CARD = 'HKLIN082= ''IAMQC.NUMOBJ             556985'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 110 ) THEN
            CARD = 'HKLIN083= ''IAMQC.PARENTS            486656'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 111 ) THEN
            CARD = 'HKLIN084= ''IAMQC.RANGING            TRUE'' /'
         ELSE IF( CMN_LINE .EQ. 112 ) THEN
            CARD = 'HKLIN085= ''IAMQC.LANE_1             15571'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 113 ) THEN
            CARD = 'HKLIN086= ''IAMQC.LANE_2             33207'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 114 ) THEN
            CARD = 'HKLIN087= ''IAMQC.LANE_3             51478'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 115 ) THEN
            CARD = 'HKLIN088= ''IAMQC.LANE_4             69944'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 116 ) THEN
            CARD = 'HKLIN089= ''IAMQC.LANE_5             89236'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 117 ) THEN
            CARD = 'HKLIN090= ''IAMQC.LANE_6             108416'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 118 ) THEN
            CARD = 'HKLIN091= ''IAMQC.LANE_7             127481'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 119 ) THEN
            CARD = 'HKLIN092= ''IAMQC.LANE_8             146699'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 120 ) THEN
            CARD = 'HKLIN093= ''IAMQC.LANE_9             166380'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 121 ) THEN
            CARD = 'HKLIN094= ''IAMQC.LANE_10            186126'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 122 ) THEN
            CARD = 'HKLIN095= ''IAMQC.LANE_11            205946'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 123 ) THEN
            CARD = 'HKLIN096= ''IAMQC.LANE_12            225915'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 124 ) THEN
            CARD = 'HKLIN097= ''IAMQC.LANE_13            245926'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 125 ) THEN
            CARD = 'HKLIN098= ''IAMQC.LANE_14            266574'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 126 ) THEN
            CARD = 'HKLIN099= ''IAMQC.LANE_15            287150'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 127 ) THEN
            CARD = 'HKLIN100= ''IAMQC.LANE_16            308087'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 128 ) THEN
            CARD = 'HKLIN101= ''IAMQC.LANE_17            328830'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 129 ) THEN
            CARD = 'HKLIN102= ''IAMQC.LANE_18            350253'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 130 ) THEN
            CARD = 'HKLIN103= ''IAMQC.LANE_19            370738'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 131 ) THEN
            CARD = 'HKLIN104= ''IAMQC.LANE_20            391722'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 132 ) THEN
            CARD = 'HKLIN105= ''IAMQC.LANE_21            412801'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 133 ) THEN
            CARD = 'HKLIN106= ''IAMQC.LANE_22            433795'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 134 ) THEN
            CARD = 'HKLIN107= ''IAMQC.LANE_23            454383'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 135 ) THEN
            CARD = 'HKLIN108= ''IAMQC.LANE_24            474711'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 136 ) THEN
            CARD = 'HKLIN109= ''IAMQC.LANE_25            495108'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 137 ) THEN
            CARD = 'HKLIN110= ''IAMQC.LANE_26            515755'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 138 ) THEN
            CARD = 'HKLIN111= ''IAMQC.LANE_27            536499'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 139 ) THEN
            CARD = 'HKLIN112= ''IAMQC.LANE_28            556985'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 140 ) THEN
            CARD = 'HKLIN113= ''XYTORADEC.STARCAT        /sdata/s'//
     :        'cos/refcats/tycho2.FIT'' /'
         ELSE IF( CMN_LINE .EQ. 141 ) THEN
            CARD = 'HKLIN114= ''XYTORADEC.BRIGHTLIM      9'' /'
         ELSE IF( CMN_LINE .EQ. 142 ) THEN
            CARD = 'HKLIN115= ''XYTORADEC.C-EQUIN        2000'' /'
         ELSE IF( CMN_LINE .EQ. 143 ) THEN
            CARD = 'HKLIN116= ''XYTORADEC.C-EQTSYS       JULIAN'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 144 ) THEN
            CARD = 'HKLIN117= ''XYTORADEC.C-EPOCH        2000'' /'
         ELSE IF( CMN_LINE .EQ. 145 ) THEN
            CARD = 'HKLIN118= ''XYTORADEC.C-EPTSYS       JULIAN'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 146 ) THEN
            CARD = 'HKLIN119= ''XYTORADEC.R-EQUIN        2000'' /'
         ELSE IF( CMN_LINE .EQ. 147 ) THEN
            CARD = 'HKLIN120= ''XYTORADEC.R-TSYS         JULIAN'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 148 ) THEN
            CARD = 'HKLIN121= ''XYTORADEC.MAXITER        5000'' /'
         ELSE IF( CMN_LINE .EQ. 149 ) THEN
            CARD = 'HKLIN122= ''XYTORADEC.RCRITINI       500000'''//
     :        ' /'
         ELSE IF( CMN_LINE .EQ. 150 ) THEN
            CARD = 'HKLIN123= ''XYTORADEC.RCRITABS       50000'' '//
     :        '/'
         ELSE IF( CMN_LINE .EQ. 151 ) THEN
            CARD = 'HKLIN124= ''XYTORADEC.RCRITREL       1'' /'
         ELSE IF( CMN_LINE .EQ. 152 ) THEN
            CARD = 'HKLIN125= ''XYTORADEC.RCRITFIN       3'' /'
         ELSE IF( CMN_LINE .EQ. 153 ) THEN
            CARD = 'HKLIN126= ''XYTORADEC.HARDCOPY       /scos1/s'//
     :        'cos/UKJ001/UKJ001.ps'' /'
         ELSE IF( CMN_LINE .EQ. 154 ) THEN
            CARD = 'HKLIN127= ''XYTORADEC.REFSMULT       5'' /'
         ELSE IF( CMN_LINE .EQ. 155 ) THEN
            CARD = 'HKLIN128= ''XYTORADEC.RESDMULT       1000'' /'
         ELSE IF( CMN_LINE .EQ. 156 ) THEN
            CARD = 'HKLIN129= ''XYTORADEC.RACOL          RA'' /'
         ELSE IF( CMN_LINE .EQ. 157 ) THEN
            CARD = 'HKLIN130= ''XYTORADEC.DECOL          DEC'' /'
         ELSE IF( CMN_LINE .EQ. 158 ) THEN
            CARD = 'HKLIN131= ''XYTORADEC.RAPMCOL        PMRA'' /'
         ELSE IF( CMN_LINE .EQ. 159 ) THEN
            CARD = 'HKLIN132= ''XYTORADEC.DECPMCOL       PMDE'' /'
         ELSE IF( CMN_LINE .EQ. 160 ) THEN
            CARD = 'HKLIN133= ''XYTORADEC.PLXCOL         NONE'' /'
         ELSE IF( CMN_LINE .EQ. 161 ) THEN
            CARD = 'HKLIN134= ''XYTORADEC.RVCOL          NONE'' /'
         ELSE IF( CMN_LINE .EQ. 162 ) THEN
            CARD = 'HKLIN135= ''XYTORADEC.MAGCOL         VT'' /'
         ELSE IF( CMN_LINE .EQ. 163 ) THEN
            CARD = 'HKLIN136= ''XYTORADEC.STARSC         2374'' /'
         ELSE IF( CMN_LINE .EQ. 164 ) THEN
            CARD = 'HKLIN137= ''XYTORADEC.STARSU         1727'' /'
         ELSE IF( CMN_LINE .EQ. 165 ) THEN
            CARD = 'HKLIN138= ''XYTORADEC.COEFFS_1       17.64034'//
     :        '3856524'' /'
         ELSE IF( CMN_LINE .EQ. 166 ) THEN
            CARD = 'HKLIN139= ''XYTORADEC.COEFFS_2       -260.441'//
     :        '51995641'' /'
         ELSE IF( CMN_LINE .EQ. 167 ) THEN
            CARD = 'HKLIN140= ''XYTORADEC.COEFFS_3       -163.091'//
     :        '55572601'' /'
         ELSE IF( CMN_LINE .EQ. 168 ) THEN
            CARD = 'HKLIN141= ''XYTORADEC.COEFFS_4       17.50423'//
     :        '0442205'' /'
         ELSE IF( CMN_LINE .EQ. 169 ) THEN
            CARD = 'HKLIN142= ''XYTORADEC.COEFFS_5       -163.086'//
     :        '76953832'' /'
         ELSE IF( CMN_LINE .EQ. 170 ) THEN
            CARD = 'HKLIN143= ''XYTORADEC.COEFFS_6       260.4881'//
     :        '7907668'' /'
         ELSE IF( CMN_LINE .EQ. 171 ) THEN
            CARD = 'HKLIN144= ''XYTORADEC.DISTR          -0.33333'//
     :        '333333333'' /'
         ELSE IF( CMN_LINE .EQ. 172 ) THEN
            CARD = 'HKLIN145= ''XYTORADEC.RA_PNT         0.549249'//
     :        '96662137'' /'
         ELSE IF( CMN_LINE .EQ. 173 ) THEN
            CARD = 'HKLIN146= ''XYTORADEC.DEC_PNT        -1.56849'//
     :        '31501781'' /'
         ELSE IF( CMN_LINE .EQ. 174 ) THEN
            CARD = 'HISTORY = ''SuperCOSMOS image analysis and ma'//
     :        'pping mode (IAM and MM)'' /'
         ELSE IF( CMN_LINE .EQ. 175 ) THEN
            CARD = 'HISTORY = ''data written by xydcomp_ss.'' /'
         ELSE IF( CMN_LINE .EQ. 176 ) THEN
            CARD = 'HISTORY = ''Any questions/comments/suggestion'//
     :        's/bug reports should be sent'' /'
         ELSE IF( CMN_LINE .EQ. 177 ) THEN
            CARD = 'HISTORY = ''to N.Hambly@roe.ac.uk'' /'
         ELSE IF( CMN_LINE .EQ. 178 ) THEN
            CARD = 'ASTSIGX =         3.700000E-01 / [arcsec] std'//
     :        '. dev. of astrometric fit in X'
         ELSE IF( CMN_LINE .EQ. 179 ) THEN
            CARD = 'ASTSIGY =         3.800000E-01 / [arcsec] std'//
     :        '. dev. of astrometric fit in Y'
         ELSE IF( CMN_LINE .EQ. 180 ) THEN
            CARD = 'CRVAL1  =   0.000000000000E+00 / Axis 1 refer'//
     :        'ence value'
         ELSE IF( CMN_LINE .EQ. 181 ) THEN
            CARD = 'CRPIX1  =   8.936318379289E+02 / Axis 1 pixel'//
     :        ' value'
         ELSE IF( CMN_LINE .EQ. 182 ) THEN
            CARD = 'CTYPE1  = ''RA---TAN''           / Quantity r'//
     :        'epresented by axis 1'
         ELSE IF( CMN_LINE .EQ. 183 ) THEN
            CARD = 'CRVAL2  =  -9.000000018364E+01 / Axis 2 refer'//
     :        'ence value'
         ELSE IF( CMN_LINE .EQ. 184 ) THEN
            CARD = 'CRPIX2  =   2.238380193875E+02 / Axis 2 pixel'//
     :        ' value'
         ELSE IF( CMN_LINE .EQ. 185 ) THEN
            CARD = 'CTYPE2  = ''DEC--TAN''           / Quantity r'//
     :        'epresented by axis 2'
         ELSE IF( CMN_LINE .EQ. 186 ) THEN
            CARD = 'CD1_1   =  -1.864642639667E-04 / Co-ordinate '//
     :        'transformation matrix'
         ELSE IF( CMN_LINE .EQ. 187 ) THEN
            CARD = 'CD1_2   =  -9.188369023766E-07 / Co-ordinate '//
     :        'transformation matrix'
         ELSE IF( CMN_LINE .EQ. 188 ) THEN
            CARD = 'CD2_1   =  -1.038232462415E-06 / Co-ordinate '//
     :        'transformation matrix'
         ELSE IF( CMN_LINE .EQ. 189 ) THEN
            CARD = 'CD2_2   =   1.866269837741E-04 / Co-ordinate '//
     :        'transformation matrix'
         ELSE IF( CMN_LINE .EQ. 190 ) THEN
            CARD = 'CDELT1  =  -1.864665278217E-04 / DEPRECATED -'//
     :        ' Increment per pixel on axis 1'
         ELSE IF( CMN_LINE .EQ. 191 ) THEN
            CARD = 'CDELT2  =   1.866298716692E-04 / DEPRECATED -'//
     :        ' Increment per pixel on axis 2'
         ELSE IF( CMN_LINE .EQ. 192 ) THEN
            CARD = 'PC001001=   9.999878591881E-01 / DEPRECATED -'//
     :        ' Axis rotation matrix'
         ELSE IF( CMN_LINE .EQ. 193 ) THEN
            CARD = 'PC001002=   4.927623810613E-03 / DEPRECATED -'//
     :        ' Axis rotation matrix'
         ELSE IF( CMN_LINE .EQ. 194 ) THEN
            CARD = 'PC002001=  -5.563056187788E-03 / DEPRECATED -'//
     :        ' Axis rotation matrix'
         ELSE IF( CMN_LINE .EQ. 195 ) THEN
            CARD = 'PC002002=   9.999845260832E-01 / DEPRECATED -'//
     :        ' Axis rotation matrix'
         ELSE IF( CMN_LINE .EQ. 196 ) THEN
            CARD = 'CROTA2  =   3.005532298491E-01 / DEPRECATED -'//
     :        ' rotation of axis 2'
         ELSE IF( CMN_LINE .EQ. 197 ) THEN
            CARD = 'EQUINOX =         2.000000E+03 / Julian refer'//
     :        'ence frame equinox'
         ELSE IF( CMN_LINE .EQ. 198 ) THEN
            CARD = 'DATATYPE= ''INTEGER*2''          / Type of da'//
     :        'ta'
         ELSE IF( CMN_LINE .EQ. 199 ) THEN
            CARD = 'DATUNITS= ''DENSITY ''           / Units: tra'//
     :        'nsmission, density or intensity'
         ELSE IF( CMN_LINE .EQ. 200 ) THEN
            CARD = 'XPIXELSZ=   9.997114974000E+00 / [microns] X '//
     :        'pixel size'
         ELSE IF( CMN_LINE .EQ. 201 ) THEN
            CARD = 'YPIXELSZ=   1.000000000000E+01 / [microns] Y '//
     :        'pixel size'
         ELSE IF( CMN_LINE .EQ. 202 ) THEN
            CARD = 'OBJCTRA = ''  0  0  0.000''      / Centre Rig'//
     :        'ht Ascension (J2000)'
         ELSE IF( CMN_LINE .EQ. 203 ) THEN
            CARD = 'OBJCTDEC= ''-90  0  0.00''       / Centre Dec'//
     :        'lination (J2000)'
         ELSE IF( CMN_LINE .EQ. 204 ) THEN
            CARD = 'OBJCTX  =   1.636863183793E+04 / [pixels] Cen'//
     :        'tre X on plate'
         ELSE IF( CMN_LINE .EQ. 205 ) THEN
            CARD = 'OBJCTY  =   1.474083801939E+04 / [pixels] Cen'//
     :        'tre Y on plate'
         ELSE IF( CMN_LINE .EQ. 206 ) THEN
            CARD = 'END'
            REG_SOURCE = 0
         ELSE
            REG_SOURCE = 0
         END IF


*  Insert new header code here.... (create new header code using script
*  "make_regtest" in the AST development archive).
      ELSE
         REG_SOURCE = 0
         STATUS = SAI__ERROR
         WRITE(*,'(A,I2)') 'REG_SOURCE: No such test: ',CMN_FTEST
      END IF

      CMN_LINE = CMN_LINE + 1

      END


