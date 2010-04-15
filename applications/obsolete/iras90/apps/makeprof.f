      SUBROUTINE MAKEPROF( STATUS )
*+
*  Name:
*     MAKEPROF

*  Purpose:
*     Create an NDF holding standard in-scan detector response profiles.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKEPROF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates an NDF holding typical in-scan detector
*     point source profiles. The data is taken from the IRAS Catalogs
*     and Atlases Explanatory Supplement (second edition), page V-14.
*
*     The NDF contains four rows, each holding a typical point source
*     profile for one of the four survey wavebands. The NDF contains
*     AXIS structures which identify the wavebands and the in-scan
*     offset from the source centre.

*  Usage:
*     MAKEPROF OUT

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90", for more
*        information on history.               [current history setting]
*     OUT = NDF (Write)
*        The name of the output NDF.

*  Examples:
*     MAKEPROF PROFILES
*        Create an NDF called PROFILES containing typical in-scan
*        detector responses.

*  Allowed Formats for Profile NDFs:
*     If users wish to create their own in-scan detector profiles for
*     use with TRACECRDD, etc, then they must ensure that the NDFs
*     holding the profiles conform to the following requirements:
*
*     - Each row of the NDF must contain a single in-scan profile.
*
*     - Each profile should be normalised to a peak value of unity.
*
*     - The NDF must contain either 1 or 4 rows of data; no other
*     values are allowed. If the NDF holds 4 rows, then rows 1 to 4
*     should contain the profiles for the 12, 25, 60 and 100 um bands
*     respectively. If the NDF holds one row then the single profile
*     will be used by TRACECRDD (etc) for all wave-bands.
*
*     - If an NDF contains 4 profiles, each one must be sampled at
*     the same in-scan positions. Any gaps left at the ends of shorter
*     profiles should be filled with zeros.
*
*     - The second dimension (i.e. the in-scan axis) should have an
*     associated AXIS structure containing a CENTRE array and a UNITS
*     component. The UNITS component must start with the string
*     "ARC-MIN" (case is ignored), and the CENTRE array must contain the
*     in-scan offset from the detector centre to each point in the
*     profile, in arc-minutes. These values should be in the same sense
*     as the focal plane Y axis.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1992 (DSB):
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
      INTEGER SIZE               ! No. of points per wave band.
      PARAMETER ( SIZE = 41 )

*  Local Variables:
      INTEGER EL                 ! No. of elements mapped.
      INTEGER INDF               ! NDF identifier for output.
      INTEGER IPC1               ! Pointer to axis 1 CENTRE array.
      INTEGER IPC2               ! Pointer to axis 2 CENTRE array.
      INTEGER IPDATA             ! Pointer to DATA array.
      INTEGER LBND( 2 )          ! Lower bounds of the NDF.
      INTEGER UBND( 2 )          ! Upper bounds of the NDF.

      REAL AXIS1( SIZE )         ! In-scan offsets from source peak.
      REAL RDATA( SIZE, I90__BANDS )! Point source profile data.

*  Local Data:
      DATA RDATA/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     :           0.14, 0.75, 1, 0.75, 0.2, 4.55E-02, 0, 0, 0, 0, 0, 0,
     :           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

     :           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     :           0.11, 0.68, 1, 0.8, 0.2, 4.55E-02, 0, 0, 0, 0, 0, 0,
     :           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

     :           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2.27E-02,
     :           6.82E-02, 0.34, 0.73, 0.95, 1, 0.95, 0.77, 0.32, 0.16,
     :           6.82E-02, 4.55E-02, 2.27E-02, 0, 0, 0, 0, 0, 0, 0, 0,
     :           0, 0, 0, 0, 0,

     :           0, 0, 0, 0, 0, 0, 0, 0, 2.27E-02, 3.18E-02, 4.55E-02,
     :           5.91E-02, 6.82E-02, 0.18, 0.34, 0.55, 0.7, 0.82, 0.93,
     :           0.97, 1, 0.98, 0.95, 0.84, 0.73, 0.52, 0.32, 0.24,
     :           0.16, 0.14, 0.11, 9.09E-02, 6.82E-02, 6.25E-02,
     :           5.68E-02, 5.11E-02, 4.55E-02, 3.98E-02, 3.41E-02,
     :           2.84E-02, 2.27E-02/

      DATA AXIS1/-6, -5.7, -5.4, -5.1, -4.8, -4.5, -4.2, -3.9, -3.6,
     :           -3.3, -3, -2.7, -2.4, -2.1, -1.8, -1.5, -1.2, -0.9,
     :           -0.6, -0.3, 0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4,
     :            2.7, 3, 3.3, 3.6, 3.9, 4.2, 4.5, 4.8, 5.1, 5.4, 5.7,
     :            6.0 /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Create the output NDF.
      LBND( 1 ) = 1
      LBND( 2 ) = 1
      UBND( 1 ) = SIZE
      UBND( 2 ) = I90__BANDS
      CALL NDF_CREAT( 'OUT', '_REAL', 2, LBND, UBND, INDF, STATUS )

*  Map the DATA array.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPDATA, EL, STATUS )

*  Map the CENTRE array of the first axis.
      CALL NDF_AMAP( INDF, 'CEN', 1, '_REAL', 'WRITE', IPC1, EL,
     :               STATUS )

*  Map the CENTRE array of the second axis.
      CALL NDF_AMAP( INDF, 'CEN', 2, '_REAL', 'WRITE', IPC2, EL,
     :               STATUS )

*  Call another routine to store the values.
      CALL MPROA0( SIZE, I90__BANDS, RDATA, AXIS1, I90__WAVEL,
     :             %VAL( IPDATA ), %VAL( IPC1 ), %VAL( IPC2 ), STATUS )

*  Set up first AXIS array scalar values.
      CALL NDF_ACPUT( 'In-scan position', INDF, 'LABEL', 1, STATUS )
      CALL NDF_ACPUT( 'Arc-minutes', INDF, 'UNITS', 1, STATUS )

*  Set up second AXIS array scalar values.
      CALL NDF_ACPUT( 'Wavelength', INDF, 'LABEL', 2, STATUS )
      CALL NDF_ACPUT( 'microns', INDF, 'UNITS', 2, STATUS )

*  Set up the NDF scalar values.
      CALL NDF_CPUT( 'Typical in-scan point source profiles', INDF,
     :               'TITLE', STATUS )
      CALL NDF_CPUT( 'Normalised amplitude', INDF, 'LABEL', STATUS )

*  Add a history record.
      CALL IRM_HIST( 'HISTORY', INDF, 'MAKEPROF', 1,
     :               'In-scan point source profiles', STATUS )

*  If an error has occurred, delete the output NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'MAKEPROF_ERR1',
     :   'MAKEPROF: Unable to create NDF holding in-scan detector '//
     :   'response profiles.', STATUS )
      END IF

      END
