      SUBROUTINE MOMENTS( STATUS )
*+
*  Name:
*     MOMENTS

*  Purpose:
*     Calculate moments of spectra in a cube.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MOMENTS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates the moments for each spectrum in a cube. For
*     most of the calculated moments each spectrum y(x) is regarded as a
*     probability distibution of x-values. That is to say that y(x)-b is
*     proportional to the probability of the value x. The results for each
*     spectrum are stored in the result structure of the Specdre Extension
*     of the input NDF. The component stored is of type `moments'. The
*     numbers calculated are:
*     -  minimum pos.: The smallest x found.
*     -  maximum pos.: The largest x found.
*     -  data minimum: The smallest data minus bias found.
*     -  data maximum: The largest data minus bias found.
*     -  sum of data: The sum of all values of data minus bias. This
*        value is bad if any addend or its variance is bad.
*     -  pos. of minimum: The x value where the minimum data value was
*        found.
*     -  pos. of maximum: The x value where the maximum data value was
*        found.
*     -  median: The median is currently not calculated. The stored
*        value is the bad value.
*     -  centroid: The mean x value. Contrary to the sum of data, this
*        is calculated using only from data points where data and
*        variance are not bad.
*     -  variance: The variance of the x values. This is calculated in a
*        second pass after the centroid is known. An approximate
*        rounding error correction is made according to Press et
*        al. 1992, p. 607.
*     -  mean abs. dev.: The mean absolute deviation of the x values
*        from the centroid. This is calculated in a second pass after
*        the centroid is known.
*     -  skewness: The skewness gives a measure of the asymmetry of the
*        profile of data minus bias versus x. It is positive when the
*        profile has a tail towards large x, negative when the profile
*        has a wing at small x.
*     -  kurtosis: The kurtosis gives a measure of the ``peakedness'' of
*        the profile. It is zero for a Gaussian profile, positive if the
*        profile peak is more pronounced, negative if the profile is
*        flatter at the centre.
*     -  momentum: If x is radial velocity and data minus bias is a
*        measure of mass, then this is a measure of the radial momentum
*        (inertia). This value is bad if any addend or its variance is
*        bad.
*     -  energy: If x is radial velocity and data minus bias is a
*        measure of mass, then this is a measure of the kinetic
*        energy. This value is bad if any addend or its variance is bad.
*
*     Note that the higher moments (variance, skewness, kurtosis) are
*     rather unreliable unless the spectral features are very strong. For
*     further discussion see Press et al. 1992, chapter 14.1.

*  Usage:
*     moments in comp

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, this routine will issue only error messages and no
*        informational message. [YES]
*     VARUSE = _LOGICAL (Read)
*        If false, input variances are ignored. [YES]
*     IN = NDF (Read)
*        The input NDF. Update access is necessary to store the results
*        in the Specdre Extension. The NDF can be a section, as in
*        myndf(5:9,). The spectroscopic axis must be the first
*        non-degenerate axis.
*     COMP = _INTEGER (Read and Write)
*        The component number to be used to store the results. This
*        should be either an existing component of type 'moments' or
*        zero. If it is zero, or if the component specified does not
*        exist, or if it is not of type 'moments', then a new component
*        will be created in the results structure. In any case, if INFO
*        is true this routine will report which component number has
*        actually been used. [0]
*     BIAS = _REAL (Read)
*        y(x) is not itself used as the probability of x, but y(x)-bias.
*        Thus for a spectrum that was normalised to the continuum level,
*        give BIAS=1.0. A bias of zero is suitable for baseline-corrected
*        spectra. [0]

*  Examples:
*     moments in(-25.:+25.,,) 5
*        The NDF is probably three-dimensional. Analysis is restricted
*        to pixels between the pixels nearest to x=-25 and +25,
*        according to the AXIS(1) information. If there are five or more
*        components in the results structure and if the fifth is of type
*        'moments' then it will be used to store the results. Otherwise
*        a new component will be created for storage.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*
*     This routine works in situ and modifies the input file.

*  References:
*     Press W.H., Teukolsky S.A., Vetterling W.T., Flannery B.P., 1992,
*     Numerical recipes, Second edition, Cambridge University Press

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Mar 1994 (hme):
*        Original version.
*     20 Nov 1995 (hme):
*        Update prologue in accordance with documentation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      LOGICAL LVARUS
      INTEGER NDF
      INTEGER COMP
      REAL BIAS
      INTEGER IVARUS             ! Integer version of VARUSE

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Get the parameters.
      CALL PAR_GET0L( 'INFO',   INFO,    STATUS )
      CALL PAR_GET0L( 'VARUSE', LVARUS, STATUS )
      CALL NDF_ASSOC( 'IN', 'UPDATE', NDF, STATUS )
      CALL PAR_GET0I( 'COMP', COMP, STATUS )
      CALL PAR_GET0R( 'BIAS', BIAS, STATUS )

*  Leave the work to a linear procedure routine.
      IF ( LVARUS ) THEN
         IVARUS = 1
      ELSE
         IVARUS = 0
      END IF
      CALL SPD_CZCA( IVARUS, NDF, BIAS, COMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Issue message and put parameter, regarding the actual component
*  number.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'MOMENTS_T01', COMP )
         CALL MSG_OUT(  'MOMENTS_M01', 'Used component # ' //
     :      '^MOMENTS_T01 to store results.', STATUS )
      END IF
      CALL PAR_PUT0I( 'COMP', COMP, STATUS )

*  Tidy up.
 500  CONTINUE
      CALL NDF_ANNUL( NDF, STATUS )
      CALL NDF_END( STATUS )

      END
