      SUBROUTINE COF_DOSCL( FUNITH, FUNITD, FMTCNV, TYPE, STATUS )
*+
*  Name:
*     COF_DOSCL

*  Purpose:
*     Set scaling requirements for FITS2NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_DOSCL( FUNITH, FUNITD, FMTCNV, TYPE, STATUS )

*  Description:
*     This routine converts a FITS file into an NDF.  It can process an
*     arbitrary FITS file to produce an NDF, using NDF extensions to
*     store information conveyed in table and image components of the
*     FITS file.  While no information is lost, in many common cases
*     this would prove inconvenient especially as no meaning is attached
*     to the NDF extension components.  Therefore, this routine
*     recognises certain data products (currently IUE, ISO, and 2dF),
*     and provides tailored conversions that map the FITS data better
*     on to the NDF.  For instance, an image extension storing data
*     errors will have its data array transferred to the NDF's VARIANCE
*     (after being squared).  In addition, FITS2NDF can restore NDFs
*     converted to FITS by the sister task NDF2FITS.

*     Details of the supported formats and rules for processing them,
*     and the general-case processing rules are described below.

*  Arguments:
*     FUNITH = INTEGER (Given)
*        The FITS unit number associated with the header
*        This may differ from that associated with the data if a merged
*        header has been constructed.
*     FUNITD = INTEGER (Given)
*        The FITS unit number associated with the data.
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS keywords BSCALE
*        and BZERO to the FITS data to generate the "true" data values.
*        This applies to IMAGE extensions, as well as the primary data
*        array.  If BSCALE and BZERO are not given in the FITS header,
*        they are taken to be 1.0 and 0.0 respectively.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format on tape (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The choice of
*        floating-point data type depends on the number of significant
*        digits in the BSCALE and BZERO keywords.
*     TYPE = CHARACTER*(NDF__SZTYP) (Given and returned)
*        Given as the type of the NDF component - returned = ' ' if BSCALE
*        and BZERO are to be used in scaling.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:

*  Implementation Deficiencies:
*     [routine_deficiencies]...

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-DEC-2000 (AJC):
*        Original version extracted from COF_F2NDF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FUNITH             ! FITS unit for the (merged?) FITS header
      INTEGER FUNITD             ! FITS unit for the FITS file
      LOGICAL FMTCNV             ! If format conversion required
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF array's data type

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Global Variables:

*  Local Variables:
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      CHARACTER * ( 48 ) COMENT  ! FITS header comment
      INTEGER FSTAT              ! FITSIO error status
      LOGICAL SCAPRE             ! If scaling keyword present
      DOUBLE PRECISION SCALE     ! Value of BSCALE keyword
      DOUBLE PRECISION OFFSET    ! Value of BZERO keyword
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will apply the
*  block floating-point scaling as prescribed by the BSCALE and BZERO
*  keywords.
      IF ( FMTCNV ) THEN

*  Scaling is to be applied.  Find the data type required for the
*  output array based upon the number of significant digits in the
*  BSCALE and BZERO keywords.  If these have values of 1.0D0 and 0.0D0
*  respectively either explicitly, or because one or both are absent,
*  then the data type can be set to the null string.  This instructs
*  later routines like COF_STYPC to use the data type specified by the
*  FITSIO data-type code (based on BITPIX).
         CALL COF_DSTYP( FUNITH, 'BSCALE', 'BZERO', TYPE,
     :                            STATUS )

*  If the header and data FUNITs are different, set the scaling factors
*  explicitly for the data unit according to the header unit just in case
*  it inherited them from the primary.
*  Get BSCALE, BZERO from the merged header.
         IF ( FUNITH .NE. FUNITD ) THEN
            CALL COF_GKEYD(
     :        FUNITH, 'BSCALE', SCAPRE, SCALE, COMENT,
     :                    STATUS )
            IF ( .NOT. SCAPRE ) SCALE = 1.0D0

            CALL COF_GKEYD(
     :                    FUNITH, 'BZERO', SCAPRE, OFFSET, COMENT,
     :                    STATUS )
            IF ( .NOT. SCAPRE ) OFFSET = 0.0D0
*  And set scaling of the original HDU
            CALL FTPSCL( FUNITD, SCALE, OFFSET, FSTAT )
         END IF

*  To prevent scaling, the scale and offset must be set to
*  one and zero respectively.  Note that this does not affect the
*  keywords in the header of the input FITS file.  Note that the values
*  are double precision.
      ELSE
         CALL FTPSCL( FUNITD, 1.0D0, 0.0D0, FSTAT )

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
         TYPE = ' '

      END IF  ! FMTCNV

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         BUFFER = 'Error defaulting the scale and offset '
         CALL COF_FIOER( FSTAT, 'COF_F2NDF_SCOF', 'FTPSCL', BUFFER,
     :         STATUS )
      END IF

      END
