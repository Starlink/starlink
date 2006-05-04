      SUBROUTINE NDF2TIFF( STATUS )
*+
*  Name:
*     NDF2TIFF

*  Purpose:
*     Converts an NDF into an 8-bit TIFF-6.0-format file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation
*     CALL NDF2TIFF( STATUS )

*  Description: 
*     This application converts an NDF to a Image File Format (TIFF).
*     One- or two-dimensional arrays can be handled and various methods
*     of scaling the data are provided.

*     The routine first finds the brightest and darkest pixel values
*     required by the particular scaling method in use. It then uses
*     these to determine suitable scaling factors and converts the image
*     into an 8-bit representation which is then output to a simple
*     greyscale TIFF-6.0 file.

*     If the 'high' scaling value is less than the 'low' value, the output
*     image will be a negative. Bad values are set to 0 for positives and
*     255 for negatives.

*  Usage:
*     ndf2gif in out [scale] {high=? low=?
*                            {percentiles=[?,?], [numbin=?]
*                            {sigmas=[?,?]

*  Parameters:
*     HIGH = _DOUBLE (Read)
*        Only required if SCALE is "scale".
*        The array value that scales to 255 in the TIFF file.
*        All larger array values are set to 255 when HIGH is greater than
*        LOW, otherwise all array values less than HIGH are set to 255.
*        The dynamic default is the maximum data value.  There is an
*        efficiency gain when both LOW and HIGH are given on the
*        command line, because the extreme values need not be computed.
*        The highest data value is suggested in prompts.
*     IN = NDF (Read)
*        Input NDF data structure containing the image to be displayed.
*     LOW = _DOUBLE (Read)
*        Only required if SCALE is "scale".
*        The array value that scales to 0 in the TIFF file.
*        All smaller array values are also set to 0 when LOW is less than
*        HIGH, otherwise all array values greater than LOW are set to 0.
*        The dynamic default is the minimum data value.  There is an
*        efficiency gain when both LOW and HIGH are given on the
*        command line, because the extreme values need not be computed.
*        The lowest data value is suggested in prompts.
*     MSG_FILTER = The output message filtering level, QUIET, NORMAL or
*        VERBOSE. If set to verbose, the scaling limits used will be
*        displayed. [NORMAL]
*     NUMBIN  =  _INTEGER (Read)
*        Only used if SCALE is "Percentiles".
*        The number of histogram bins used to compute percentiles for
*        scaling. [2048]
*     OUT = _CHAR (Read)
*        The name of the TIFF file to be generated.  (A .tif name
*        extension is added if it is omitted. Any existing file with
*        the same name will be overwritten.
*     PERCENTILES( 2 ) = _REAL (Read)
*        Only required if SCALE is "Percentiles".
*        The percentiles that define the scaling limits. For example,
*        [25,75] would scale between the quartile values.
*     SCAHIGH = _DOUBLE (Write)
*        The array value scaled to the maximum colour index.
*     SCALE = LITERAL (Read)
*        The type of scaling to be applied to the array.  [Range]
*        The options, which may be abbreviated to an unambiguous string
*        and are case-insensitive, are described below:
*          "Range"       - The image is scaled between the minimum and
*                          maximum data values. (This is the default.)
*          "Faint"       - The image is scaled from the mean minus one
*                          standard deviation to the mean plus seven
*                          standard deviations.  
*          "Percentiles" - The image is scaled between the values
*                          corresponding to two percentiles.  
*          "Scale"       - You define the upper and lower limits
*                          between which the image is to be scaled.  The
*                          application suggests the maximum and the
*                          minimum values when prompting.
*          "Sigmas"      - The image is scaled between two standard-
*                          deviation limits.  
*     SCALOW = _DOUBLE (Write)
*        The array value scaled to the minimum colour index.
*     SIGMAS( 2 ) = _REAL (Read)
*        Only required if SCALE is "Sigmas".
*        The standard-deviation bounds that define the scaling limits.
*        To obtain values either side of the mean both a negative and
*        a positive value are required.  Thus [-2,3] would scale
*        between the mean minus two and the mean plus three standard
*        deviations.  [3,-2] would give the negative of that.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     ndf2tiff old new
*        This converts the NDF called old (in file old.sdf)
*        into a TIFF file new.tif.
*     ndf2tiff horse horse pe
*        This converts the NDF called horse (in file horse.sdf)
*        into a TIFF file horse.tif using percentile scaling.
*        The user will be prompted for the percentiles to use.

*  Related Applications:
*     TIFF2NDF

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-Nov-1995 (GJP):
*        Original version.
*     19-FEB-1996 (MJC):
*        Tidied to standard style.
*     04-FEB-1999 (AJC):
*        Revised version based on KAPPA DISPLAY.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'MSG_PAR'        ! MSG constants
      INCLUDE 'PRM_PAR'        ! Magic-value definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN          ! Used length of string
      EXTERNAL CHR_LEN

*  Local Constants:

      INTEGER NDIM             ! Dimensionality required
      PARAMETER( NDIM = 2 )

      INTEGER MAXBIN           ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 2048 )! should be enough

      INTEGER NPRCTL           ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

      INTEGER LP               ! Lowest pen number
      PARAMETER( LP = 0 )

      INTEGER NINTS            ! Number of intensity levels
      PARAMETER( NINTS = 256)

*  Local Variables:
      LOGICAL                  ! True if :
     :  BAD                    ! The array may contain bad pixels and
                               ! therefore testing should occur

      LOGICAL                  ! True if:
     :  FNDRNG,                ! Find the data range for scaling
     :  POSTIV                 ! The scaling of the array is to be
                               ! positive
      REAL
     :  DUMMY                  ! Used to swap percentiles

      REAL
     :  PERCNT( NPRCTL ),      ! Percentiles
     :  PERDEF( NPRCTL )      ! Suggested values for percentiles

      DOUBLE PRECISION
     :  PERVAL( NPRCTL )       ! Values at the percentiles

      REAL
     :  SIGDEF( 2 ),           ! Suggested default standard-deviation
                               ! limits
     :  SIGRNG( 2 )            ! Standard-deviation limits

      CHARACTER*72
     :  COMP * 8,              ! Component to be displayed
     :  ITYPE * ( NDF__SZTYP ),! Processing type of the image
     :  MODE,                  ! Manner in which the array is to be
                               ! scaled
     :  OUT,                   ! Output filename
     :  TEMP                   ! Temp buffer

      INTEGER
     :  ACTHIG,                ! The HIGH parameter state
     :  ACTLOW,                ! The LOW parameter state
     :  BPCI,                  ! Bad-pixel colour index
     :  DIMS( NDF__MXDIM ),    ! Dimensions of input array
     :  EL,                    ! Number of elements in the input and 
                               ! cell arrays
     :  FIOD,                  ! FIO File Descriptor
     :  FLEN,                  ! Size of output file
     :  HIST( MAXBIN )         ! Array containing histogram

      INTEGER
     :  I, NC,                 ! General variables
     :  MAXPOS,                ! Position of the maximum (not used)
     :  MINPOS                 ! Position of the minimum (not used)

      INTEGER
     :  NDF,                   ! Identifier for input NDF
     :  NDIMS,                 ! Total number of NDF dimensions
     :  NINVAL,                ! Number of bad values in the input array
     :  NUMBIN,                ! Number of bins in histogram
     :  OPNTR                  ! Pointer to output array data

      INTEGER
     :  PNTRI( 1 )            ! Pointer to image data

      DOUBLE PRECISION
     :  DIMHI,                 ! Upper limit used for scaling the array
     :  DIMLO,                 ! Lower   "     "   "     "     "    "
     :  DMAXV,                 ! Minimum value in the array
     :  DMINV                  ! Maximum value in the array
*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Determine the MSG Filter level
*    ==============================

      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*    Begin an NDF context.

      CALL NDF_BEGIN

*    Obtain the identifier of the NDF to be displayed.

      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 980

      COMP = 'DATA'
      BPCI = 0
      ITYPE = '_DOUBLE'

*    Obtain the dimensions of the image.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIMS, STATUS )

*    Must have a 2-d.
      IF ( NDIMS .GT. 2 ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'IVDIM',
     :     'NDF2TIFF: NDF ^NDF is more than two-dimensional.', STATUS )
         GOTO 980
      END IF

*    Check whether or not bad pixels may be present.      
      CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )


*    Map the image with type _DOUBLE
      CALL NDF_MAP( NDF, COMP, ITYPE, 'READ', PNTRI, EL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980
      
*    Get the image name.
      CALL PAR_GET0C( 'OUT', OUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*     Append the .tif name if not there.
       TEMP = OUT
       CALL CHR_UCASE( TEMP )
       NC = CHR_LEN( TEMP )
       IF ( TEMP( NC - 3:NC ) .NE. '.TIF' ) OUT = OUT( 1:NC )//'.tif'
         
*    Open the file. 
*    Use of access mode 'APPEND' with RIO just has the effect of creating
*    the file if it does not exist.
       FLEN = EL + 122
       CALL RIO_OPEN( OUT, 'APPEND', 'UNFORMATTED', FLEN, FIOD,
     :                  STATUS )
       IF ( STATUS .NE. SAI__OK ) GOTO 999


*    Determine the type of scaling.
*    ==============================

*    If scaling, get the type of scaling to be done on the array
*    No scaling is the same as RANGE.
      CALL PAR_CHOIC( 'SCALE', 'Scale',
     :                   'Scale,Flash,Faint,Percentiles,Range,Sigma',
     :                   .TRUE., MODE, STATUS )
      CALL CHR_UCASE( MODE )

*    Faint (positive) scaling of the array.
*    ======================================

      IF ( MODE(1:2) .EQ. 'FA' .OR. MODE(1:2) .EQ. 'SI' ) THEN

*       Obtain the standard-deviation limits if not predefined.  There
*       is no dynamic default.
         IF ( MODE(1:2) .EQ. 'SI' ) THEN
            SIGDEF( 1 ) = VAL__BADR
            SIGDEF( 2 ) = VAL__BADR
            CALL PAR_GDR1R( 'SIGMAS', 2, SIGDEF, -1000., 10000.,
     :                      .FALSE., SIGRNG, STATUS )

         ELSE IF ( MODE(1:2) .EQ. 'FA' ) THEN
*          Fixed range in standard-deviation units.
            SIGRNG( 1 ) = -1.0
            SIGRNG( 2 ) = 7.0
         END IF

*       Select appropriate routine for the data type chosen and scale
*       the image between the standard-deviation limits into the cell
*       array.  The cell array has values between the colour-index
*       limits LP and the largest colour index for the device.  (This
*       will change when a colour-table management scheme is
*       introduced.)
*       =============================================================

         CALL CON_FAIND( BAD, DIMS( 1 ), DIMS( 2 ),
     :                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ), SIGRNG,
     :                   DIMLO, DIMHI, STATUS )

*    Do scaling with extreme values in the array as the limits.
*    ==========================================================

      ELSE IF ( MODE(1:2) .EQ. 'RA' ) THEN

*       Select appropriate routine for the data type chosen and scale
*       the image between user-defined limits.   The cell array
*       has values between the colour-index limits LP and the largest
*       colour index for the device.
*       =============================================================


*       Obtain the maximum and minimum values.

         CALL CON_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 
     :                   NINVAL,
     :                       DIMHI, DIMLO, MAXPOS, MINPOS, STATUS )

*       The number of bad pixels has been counted so it might be
*       possible to save future processing.
         BAD = BAD .OR. ( NINVAL .EQ. 0 )

*    Do scaling with limits from the environment.
*    ============================================

      ELSE IF ( MODE(1:2) .EQ. 'SC' ) THEN

*       Determine whether or not the scaling parameters have been
*       found, to avoid finding the maximum and minimum values when
*       they are not required.

         CALL PAR_STATE( 'LOW', ACTLOW, STATUS )
         CALL PAR_STATE( 'HIGH', ACTHIG, STATUS )
         FNDRNG = ACTLOW .EQ. SUBPAR__ACTIVE .AND.
     :            ACTHIG .EQ. SUBPAR__ACTIVE

*       Select appropriate routine for the data type chosen and scale
*       the image between user-defined limits.  The cell array
*       has values between the colour-index limits LP and the largest
*       colour index for the device.
*       =============================================================

*       Set the scaling limits to the extreme values.

         IF ( FNDRNG ) THEN
            DMINV = VAL__MIND
            DMAXV = VAL__MAXD
         ELSE

*          Obtain the maximum and minimum values to be used as
*          dynamic defaults.
            CALL CON_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 
     :                      NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Get any tighter limits from the environment
            CALL PAR_DEF0D( 'HIGH', DMAXV, STATUS )
            CALL PAR_DEF0D( 'LOW', DMINV, STATUS )

         END IF
         CALL PAR_GET0D( 'HIGH', DIMHI, STATUS )
         CALL PAR_GET0D( 'LOW', DIMLO, STATUS )

*    Do scaling with percentile limits.
*    ==================================

      ELSE IF ( MODE(1:2) .EQ. 'PE' ) THEN

*       Find the percentiles required.  There is no dynamic default.
         DO I = 1, NPRCTL
            PERDEF( I ) = VAL__BADR
         END DO
         CALL PAR_GDR1R( 'PERCENTILES', NPRCTL, PERDEF, 0.0, 100.0,
     :                   .FALSE., PERCNT, STATUS )

*       Convert percentiles to fractions.
         DO  I = 1, NPRCTL
            PERCNT( I ) = PERCNT( I ) * 0.01
         END DO

*       Record the polarity.
         POSTIV = PERCNT( 2 ) .GT. PERCNT( 1 )

*       Also get the number of histogram bins to be used - suggest
*       the maximum allowable as the default.
         CALL PAR_GDR0I( 'NUMBIN', MAXBIN, 1, MAXBIN, .TRUE., NUMBIN,
     :                   STATUS )

*       Since NUMBIN is passed as an adjustable-array dimension we have
*       to check it has been obtained.
         IF ( STATUS .EQ. SAI__OK ) THEN

*          Select appropriate routine for the data type chosen and scale
*          the image between user-defined limits.  The cell array
*          has values between the colour-index limits LP and the largest
*          colour index for the device.
*          =============================================================

*          Obtain the maximum and minimum values to define the bounds
*          of the histogram.
            CALL CON_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 
     :                      NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*          The number of bad pixels has been counted so it might be
*          possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*          Generate the histogram between those bounds.
            CALL CON_GHSTD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NUMBIN, DMAXV, DMINV, HIST, STATUS )

*          Estimate the values at the percentiles.
            CALL CON_HSTFD( NUMBIN, HIST, DMAXV, DMINV,
     :                          NPRCTL, PERCNT, PERVAL, STATUS )

*          Swap the percentiles back if they were flipped.
            IF ( .NOT. POSTIV ) THEN
               DUMMY = PERVAL( 1 )
               PERVAL( 1 ) = PERVAL( 2 )
               PERVAL( 2 ) = DUMMY
            END IF

*          Scale the data values using the percentile values.
*          Copy the scaling values for output.
            DIMLO = PERVAL( 1 )
            DIMHI = PERVAL( 2 )


*       End of status check.

         END IF

      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Write the TIFF File
*       ===================
       
*       Report the scaling limits for future use if MSG_FILTER VERBOSE
         CALL MSG_SETD( 'MINVAL', DIMLO )
         CALL MSG_SETD( 'MAXVAL', DIMHI )
         CALL MSG_OUTIF( MSG__VERB, 
     :                    'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*       Create space for the output image.
         CALL PSX_CALLOC( FLEN, '_CHAR', OPNTR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

         CALL CON_WRTIF( EL, %VAL(CNF_PVAL(PNTRI(1))), DIMS, FLEN, FIOD,
     :                    DIMLO, DIMHI, %VAL(CNF_PVAL(OPNTR)), STATUS )

*       Finished with the input array so unmap it.
         CALL NDF_UNMAP( NDF, COMP, STATUS )

*       Store the scaling values.
*       =========================

         CALL PAR_PUT0D( 'SCALOW', DIMLO, STATUS )
         CALL PAR_PUT0D( 'SCAHIGH', DIMHI, STATUS )

*       Abort to prevent spurious error messages.
         IF ( STATUS .NE. SAI__OK ) GOTO 940

*       End the error context.
         CALL ERR_RLSE

      END IF
 940  CONTINUE

*    Unmap and annul NDF data.
*    =========================
 980  CONTINUE
      CALL NDF_END( STATUS )

 999  CONTINUE

      END
