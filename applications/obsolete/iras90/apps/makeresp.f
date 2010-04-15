      SUBROUTINE MAKERESP( STATUS )
*+
*  Name:
*     MAKERESP

*  Purpose:
*     Create NDFs holding IRAS spectral response data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKERESP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates an NDF holding the spectral response curve
*     for a specified IRAS survey band. The NDF is one dimensional, and
*     gives the normalised system response against wavelength. The
*     wavelength corresponding to each element is stored in the NDFs
*     AXIS component, in microns. The values in the DATA component are
*     normalised to that the peak response throughout all four bands is
*     unity. The data is taken from the IRAS Catalogs and Atlasses
*     Explanatory Supplement (second edition), table II.C.5.
*
*     The NDF contains a VARIANCE component which holds values
*     corresponding to a 1% error on the response values. The AXIS
*     component also has a variance array, holding the uncertainties in
*     the wavelength at each element, corresponding to an error of 0.3
*     microns.  The size of these errors are not well known, but the
*     Exp. Supp.  says these are the maximum expected values (paragraph
*     C.3, page VI-28).

*  Usage:
*     MAKERESP OUT BAND

*  ADAM Parameters:
*     BAND = _INTEGER (Read)
*        The wavelength (in microns) of the IRAS survey band for which
*        spectral response data is required.
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90", for more
*        information on history.               [current history setting]
*     OUT = NDF (Write)
*        The name of the output NDF.

*  Examples:
*     MAKERESP SPECRESP60 60
*        Create an NDF called SPECRESP60 containing the spectral
*        response for the 60 um band.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXSIZ             ! Max. no. of points per wave band.
      PARAMETER ( MAXSIZ = 32 )

      REAL LERR                  ! Error in wavelength values (in um).
      PARAMETER ( LERR = 0.3 )

      REAL VERR                  ! Fractional error on gain values.
      PARAMETER ( VERR = 0.01 )

*  Local Variables:
      INTEGER BAND               ! Waveband index.
      CHARACTER BUF*23           ! Buffer for NDF title.
      REAL DWAVEL( I90__BANDS )  ! Wavelength step in each response
                                 ! curve, in um.
      INTEGER EL                 ! No. of elements mapped.
      INTEGER INDF               ! NDF identifier for output.
      INTEGER IPCV( 2 )          ! Pointers to axis CENTRE and VARIANCE.
      INTEGER IPDV( 2 )          ! Pointers to DATA and VARIANCE.
      REAL NRESP( MAXSIZ, I90__BANDS )! Response normalised to a peak
                                 ! in each band of unity. Taken from
                                 ! Exp. Supp. Table II.C.5 "Relative
                                 ! system resp.".
      REAL PRESP( I90__BANDS )   ! Peak response in each band. Taken
                                 ! from Exp.  Supp. Table II.C.5
                                 ! (product of "Trans." and "Relative
                                 ! Det. resp." at "Relative system
                                 ! resp." = 1.000, normalised to a peak
                                 ! of unity).
      INTEGER SIZE( I90__BANDS ) ! No. of entries in each wavebands
                                 ! response curve.
      REAL WAVEL0( I90__BANDS )  ! Wavelength at start of each response
                                 ! curve, in um.

*  Local Data:
      DATA DWAVEL / 0.5, 0.5, 3.0, 5.0 /

      DATA NRESP   / 0.000, 0.008, 0.535, 0.689, 0.735, 0.815, 0.900,
     : 0.904, 0.834, 0.816, 0.793, 0.854, 0.938, 0.991, 1.000, 0.934,
     : 0.388, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,

     :               0.007, 0.101, 0.288, 0.388, 0.452, 0.521, 0.562,
     : 0.626, 0.683, 0.729, 0.778, 0.832, 0.912, 0.914, 0.938, 0.933,
     : 0.875, 0.910, 1.000, 0.911, 0.840, 0.763, 0.749, 0.829, 0.914,
     : 0.790, 0.877, 0.558, 0.274, 0.069, 0.012, 0.000,

     :               0.000, 0.010, 0.036, 0.068, 0.174, 0.315, 0.483,
     : 0.585, 0.658, 0.716, 0.824, 0.915, 0.987, 0.990, 1.000, 0.946,
     : 0.713, 0.531, 0.174, 0.047, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,

     :               0.000, 0.010, 0.113, 0.306, 0.505, 0.695, 0.824,
     : 0.947, 0.939, 1.000, 0.631, 0.319, 0.195, 0.106, 0.053, 0.010,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000 /

      DATA PRESP / 0.923, 1.000, 0.373, 0.725 /

      DATA SIZE / 18, 32, 21, 16 /

      DATA WAVEL0 / 7.0, 16.0, 27.0, 65.0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See which waveband is required.
      CALL IRM_GTBND( 'BAND', 1, .FALSE., BAND, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Create the output NDF.
      CALL NDF_CREAT( 'OUT', '_REAL', 1, 1, SIZE( BAND ), INDF,
     :                STATUS )

*  Map the required arrays.
      CALL NDF_MAP( INDF, 'DATA,VAR', '_REAL', 'WRITE', IPDV, EL,
     :              STATUS )

      CALL NDF_AMAP( INDF, 'CEN,VAR', 1, '_REAL', 'WRITE', IPCV, EL,
     :               STATUS )

*  Call another routine to store the array values.
      CALL MRESA0( SIZE( BAND ), NRESP( 1, BAND ), PRESP( BAND ),
     :             WAVEL0( BAND ), DWAVEL( BAND ), VERR, LERR,
     :             %VAL( IPDV( 1 ) ), %VAL( IPDV( 2 ) ),
     :             %VAL( IPCV( 1 ) ), %VAL( IPCV( 2 ) ), STATUS )

*  Set up AXIS array scalar values.
      CALL NDF_ACPUT( 'Wavelength', INDF, 'LABEL', 1, STATUS )
      CALL NDF_ACPUT( 'microns', INDF, 'UNITS', 1, STATUS )

*  Set up the NDF scalar values.
      WRITE( BUF( : 3 ), '(I3)' ) I90__WAVEL( BAND )
      BUF( 4 : ) = 'um spectral response'
      CALL CHR_LDBLK( BUF )
      CALL NDF_CPUT( BUF, INDF, 'TITLE', STATUS )
      CALL NDF_CPUT( 'Gain', INDF, 'LABEL', STATUS )

*  Add a history record.
      CALL IRM_HIST( 'HISTORY', INDF, 'MAKERESP', 1, BUF, STATUS )

*  If an error has occurred, delete the output NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MAKERESP_ERR1',
     :   'MAKERESP: Unable to create IRAS spectral response NDFs.',
     :   STATUS )
      END IF

      END
