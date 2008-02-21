      SUBROUTINE CON_WCSPX( INDF, IMAP, OBSLON, OBSLAT, VAR, STATUS )
*+
*  Name:
*     CON_WCSPX

*  Purpose:
*     Add a WCS component to an NDF created by SPECX2NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_WCSPX( INDF, IMAP, OBSLON, OBSLAT, VAR, STATUS )

*  Description:
*     This routine adds a WCS component to the output NDF holdiong a
*     FrameSet in which the current Frame is a 3D Frame with RA on axis 1,
*     DEC on axis 2, and frequency on axis 3. The parameters defining the
*     axes are read from the SPECX extensions in the supplied SPECX map,
*     except for the observatory location which is provided by the caller.
*
*     Also calculates and returns a constant variance value based on the
*     Tsys value in the SPECX extension.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier for the NDF created by SPECX2NDF.
*     IMAP = INTEGER (Given)
*        The NDF identifier for the SPECX map file.
*     OBSLON = DOUBLE PRECISION (Given)
*        The geodetic longitude of the observatory. Radians, positive east.
*     OBSLAT = DOUBLE PRECISION (Given)
*        The geodetic latitude of the observatory. Radians, positive north.
*     VAR = REAL (Returned)
*        A constant variance valued derived from the values in the SPECX
*        extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Various assumptions are made about the meaning of several items
*     in the SPECX extensions. These are described in the code comments.
*     - Double Sideband is always assumed

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-JAN-2003 (DSB):
*        Original version.
*      6-AUG-2004 (TIMJ):
*        Convert to use DSBSpecFrame
*      14-AUG-2005 (TIMJ):
*        Minor tweak to multiplication statement to make it standards compliant.
*      10-JAN-2008 (DSB):
*        - For increased precision, store the Epoch directly in the SkyFrame 
*        using AST_SETD rather than indirectly via the MJD-OBS FITS header. 
*        - Use AST attributes ObsLon/Lat instead of GeoLon/Lat
*        - Convert observation date from UT1 (sed by SPECX) to TDB (used
*        by AST) before storing it in the output WCS.
*        - Asume that the SPECX axis is in the rest frame given by the
*        bottom 4 bits of LSRFLG rather than in the source's rest frame.
*      11-JAN-2008 (DSB):
*        - The specx user guide says that JFINC is a topocentric value
*        and JFCEN is a source frame value. So convert JFINC from topo to
*        source before using it. This involves backing out of yesterdays 
*        change that assumed the SPECX axis (i.e. both JFCEN and JFINC) was 
*        in the rest frame given by the bottom 4 bits of LSRFLG.
*        - Use the correct definition of the source velocity (radio,
*        optical or relativistic) as read from bits 5 and 6 of LSRFLG.
*      8-FEB-2008 (DSB):
*        Added argument VAR.
*      21-FEB-2008 (DSB):
*        The bulk of the working has been moved out to ATL_WCSPX.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER INDF
      INTEGER IMAP
      DOUBLE PRECISION OBSLON
      DOUBLE PRECISION OBSLAT

*  Arguments Returned:
      REAL VAR

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      INTEGER DIM( 3 )       ! Dimensions of output NDF
      INTEGER INTT           ! Integration time in ms
      INTEGER IWCS           ! Pointer to NDF's WCS FrameSet
      INTEGER IWCS0          ! Pointer to SPECX's WCS FrameSet
      INTEGER JFINC          ! Pixel size (Hz) on spectral axis 
      INTEGER NDIM           ! No. of pixel axes in output NDF
      REAL CT                ! Product of channel spacing and integ time
      REAL TSYS              ! Tsys 
*.

*  Initialise returned values.
      VAR = VAL__BADR

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the dimensions of the output NDF, in pixels.
      CALL NDF_DIM( INDF, 3, DIM, NDIM, STATUS )

*  Create the FrameSet describing the SPECX WCS information.
      CALL ATL_WCSPX( IMAP, DIM, OBSLON, OBSLAT, IWCS0, STATUS )

*  Get a pointer to the FrameSet which forms the default WCS component for 
*  the output NDF. This just contains GRID, PIXEL and AXIS Frames. The 
*  GRID Frame will be the Base Frame.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Add in the current Frame from the new FrameSet.
      CALL AST_ADDFRAME( IWCS, AST__BASE, 
     :                   AST_GETMAPPING( IWCS0, AST__BASE, AST__CURRENT,
     :                                   STATUS ),
     :                   AST_GETFRAME( IWCS0, AST__CURRENT, STATUS ),
     :                   STATUS )       

*  Store the FrameSet back in the NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  Get the frequency increment per pixel, in Hz.
      JFINC = 0
      CALL NDF_XGT0I( IMAP, 'SPECX', 'JFINC(1)', JFINC, STATUS)

*  Calculate and return the constant variance value implied by the
*  Tsys, channel spacing, and integration time in the SPECX header.
      INTT = 0
      CALL NDF_XGT0I( IMAP, 'SPECX', 'INTT', INTT, STATUS )

      TSYS = 0.0
      CALL NDF_XGT0R( IMAP, 'SPECX', 'TSYS(1)', TSYS, STATUS )

      CT = REAL( JFINC )*REAL( INTT )
      IF( CT .GT. 0.0 .AND. TSYS .GT. 0.0 ) THEN
         VAR = 4000.0*TSYS*TSYS/ CT
      ELSE
         VAR = VAL__BADR
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
