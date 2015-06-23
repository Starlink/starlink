      SUBROUTINE BBODY( STATUS )
*+
*  Name:
*     BBODY

*  Purpose:
*     Calculate a black body spectrum.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BBODY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     bbody temp in=? xstart=? xstep=? xend=? xlabel=? xunit=? out=?

*  Description:
*     This routine calculates for a given (vacuum) wavelength or
*     frequency axis the intensity of a black body at given temperature.
*     The intensity is the energy per unit time, per unit area, per unit
*     solid angle, and per unit frequency (and for all polarisations):
*
*             2 h nu^3          1
*     B_nu = ---------- ------------------
*                c^2     exp(h nu/kT) - 1
*
*     where c is the velocity of light, and h and k are the
*     Planck and Boltzmann constants.

*  ADAM Parameters:
*     LOGAR = LOGICAL (Read)
*        True if the common logarithm of intensity is to be written
*        rather than the intensity itself. [NO]
*     TEMP = REAL (Read)
*        The black body temperature in Kelvin.
*     ERRTEMP = REAL (Read)
*        The error in the black body temperature in Kelvin.
*     IN = NDF (Read)
*        The file holding axis data to be used. Enter the null value (!)
*        to read axis data parameters from keyboard.
*     XSTART = REAL (Read)
*        The spectroscopic value (pixel centre) for the first output
*        pixel.
*     XSTEP = REAL (Read)
*        The spectroscopic step (pixel distance) for the output pixels.
*     XEND = REAL (Read)
*        The spectroscopic value (pixel centre) for the last output
*        pixel.
*     XLABEL = CHARACTER (Read)
*        The label for the spectroscopic axis. Allowed values are
*        "wavelength" and "frequency". [wavelength]
*     XUNIT = CHARACTER (Read)
*        The unit for the spectroscopic axis.
*        If the label is "wavelength" then the unit can basically be "m"
*        for metre, "um" or "micron" for micrometre, or "Angstrom" for
*        Angstroem. If the label is "frequency" then the unit must be
*        basically "Hz" for Hertz.
*        Any of these units may be preceded by a power of ten, so it
*        could be "10**1*Angstrom" if you want to use nanometre as unit,
*        or "10**-9*m" to the same effect. The power must be an
*        integer.
*        You can achieve a logarithmic axis by specifying something like
*        "log10(10**-3*micron)". In this example the axis values will be
*        the common logarithms of the wavelength in nanometres.
*     OUT = NDF (Read)
*        The output file.

*  Examples:
*     bbody 5500 in=in out=out
*        This calculates the black-body spectrum for 5500 K. The
*        spectrum is written to file OUT. The routine tries to find all
*        necessary information for the 1st (and only) axis in OUT from
*        the spectroscopic axis of the file IN. Since LOGAR is left at
*        its default value of FALSE, the data are intensity in Jy/sr.
*     bbody 2.7 logar=true in=! xstart=0 xstep=0.05 xend=6
*     xlabel=wavelength xunit=log(micron) out=out
*        This calculates the black-body spectrum for 2.7 K. The spectrum
*        is written to OUT. No input file is specified. The axis
*        contains the logarithms of wavelengths in micron, which run
*        from 0 (1 micron) to 6 (1 metre). Since LOGAR=TRUE, the data
*        are the logarithms of intensity in Jy/sr.
*     bbody 1e6 logar=true in=! xstart=-1 xstep=0.05 xend=2
*     xlabel=frequency xunit=log10(10**15*Hz) out=out
*        This calculates the black-body spectrum for 1 million K. This
*        time the axis is logarithms of frequency, the units used are
*        10^15 Hz. The frequency range covered is from 10^14 Hz to
*        10^17 Hz.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  References:
*     Lang, K.R., 1980, Astrophysical Formulae, Springer, Heidelberg,
*     Berlin, New York, p. 21

*  Authors:
*     hme: Horst Meyerdierks (UoE)
*     vgg: Vito G. Graffagnino (RAL)
*     acd: Clive Davenhall (UoE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28 Mar 1991 (hme):
*        Original version.
*     11 Apr 1991 (hme):
*        Changed some parameter names. Use "Angstrom" instead of "Ang".
*     28 Jun 1991 (hme):
*        Error reporting.
*        DSA_RESHAPE_AXIS doesn't copy the axis data at call-time, but
*        probably when DSA_CLOSE is called. So we have to get the axis
*        data from the input.
*     28 Oct 1991 (hme):
*        Make ERR_REP use blank parameter and fixed message. Increase
*        length of label/units to 64. Increase range for XPOWER to -99.
*     27 Nov 1991 (hme):
*        Use of null value for IN. Make IN and OUT global. Change
*        reporting.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     18 Jun 1992 (hme):
*        Port to NDF and Unix. AXIS parameter obsolete.
*     09 Sep 1992 (hme):
*        Don't set TITLE.
*     25 Nov 1994 (hme):
*        Use new libraries.
*     05 Jul 2001 (vgg, acd):
*        Added error propagation
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! Standard PAR constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXDIM             ! Maximum supported dimension
      PARAMETER ( MAXDIM = 1 )

*  Local Variables:
*  NDF(1): IN or IN.MORE.SPECDRE.SPECVALS
*  NDF(2): OUT
      LOGICAL LOGAR
      REAL TEMP                  ! Black body temperature (Kelvin).
      LOGICAL ERRFLG             ! Flag; include error on temperature?
      REAL ERRTMP                ! Error on temperature (Kelvin).
      REAL MXTEMP                ! Maximum error on temperature (Kelvin).
      REAL MNTEMP                ! Minimum error on temperature (Kelvin).
      REAL XSTART, XEND
      INTEGER I, J               ! Temporary integers
      INTEGER LBCODE             ! Label code
      INTEGER LGCOD1             ! Code for logarithmic unit/data
      INTEGER LGCOD2             ! Code for logarithmic unit/data
      INTEGER NDF( 2 )           ! NDF identifiers
      INTEGER NDIM               ! NDF dimensionality
      INTEGER NELM               ! NDF size
      INTEGER LBND( MAXDIM )     ! NDF dimensions
      INTEGER UBND( MAXDIM )     ! NDF dimensions
      INTEGER XPTR1              ! Pointer to IN spectroscopic values
      INTEGER XPTR, DPTR, EPTR   ! Pointers to OUT centres, data, vars
      INTEGER EMXPTR, EMNPTR     ! Pointers to max, min variances.
      REAL UNFACT                ! Logarithm of unit conversion factor
      CHARACTER * ( 64 ) LABEL   ! Axis label
      CHARACTER * ( 64 ) UNITS   ! Axis unit

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Get modal parameters.
*  Depending on LOGAR parameter, we choose for the intensities linear
*  (0) or common logarithmic (2). (1 would be natural logarithmic.)
      CALL PAR_GET0L( 'LOGAR', LOGAR, STATUS )
      LGCOD2 = 0
      IF ( LOGAR ) LGCOD2 = 2

*  Get black body temperature.
      CALL PAR_GET0R( 'TEMP', TEMP, STATUS )
*  Get an error on the black body temperature if required.
      CALL PAR_GET0R( 'ERRTEMP', ERRTMP, STATUS )

*  Check to see if an error on the blackbody temperature is required
      IF (STATUS .EQ. PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
         ERRFLG = .FALSE.
      ELSE
         ERRFLG = .TRUE.
      END IF

*  Get the input spectroscopic values. This will either be a mapped
*  array from an input NDF, or a triplet XSTART, XSTEP, XEND read from
*  the parameter system. The actual case is signalled by the NDF
*  identifier being valid or not. We will also get the label and unit
*  either from that NDF, or from the keyboard.
*  The NDF may be the one from which an axis is mapped, or the SPECVALS
*  NDF. We don't really care about that.
*  The shape returned is that of the mapped array (or of the vector to
*  be created from XSTART, XSTEP, XEND). This may not be the shape of
*  NDF(1). Rather it can be used to create a minimum-dimension output
*  NDF.
      CALL SPD_CZND( 1, '_REAL', LBND, UBND, NDIM, NDF(1), XPTR1,
     :   LABEL, UNITS, XSTART, XEND, STATUS )

*  Check that output will be one-dimensional.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BBODY_INVDIM', 'BBODY: Error: ' //
     :      'Can create only one-dimensional output.', STATUS )
         GO TO 500
      END IF

*  Calculate size of output.
      NELM = UBND(1) - LBND(1) + 1

*  Evaluate the label and unit.
      CALL SPD_UAAN( LABEL, LBCODE, STATUS )
      CALL SPD_UAAP( LBCODE, UNITS, LGCOD1, UNFACT, STATUS )

*  Check that it is valid for the spectroscopic axis.
      IF ( LBCODE .LT. 201 .OR. LBCODE .GT. 299 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BBODY_INVLAB', 'BBODY: Error: ' //
     :      'The label is not valid for spectroscopic values.', STATUS )
         GO TO 500
      END IF

*  Check that we can handle the kind of data. 201 is frequency, 203 is
*  wavelength.
      IF ( LBCODE .NE. 201 .AND. LBCODE .NE. 203 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BBODY_INVLAB', 'BBODY: Error: ' //
     :      'Can handle only frequency or wavelength.', STATUS )
         GO TO 500
      END IF

*  Get output file.
      CALL NDF_CREAT( 'OUT', '_REAL', NDIM, LBND, UBND, NDF(2), STATUS )

*  Map the data and if required map the variance array.
      CALL NDF_MAP( NDF(2), 'DATA',     '_REAL', 'WRITE',
     :   DPTR, NELM, STATUS )
      IF (ERRFLG) THEN
         CALL NDF_MAP( NDF(2), 'VARIANCE', '_REAL', 'WRITE',
     :   EPTR, NELM, STATUS )
      END IF

*  Map the output spectroscopic values.
      CALL NDF_AMAP( NDF(2), 'CENTRE', 1, '_REAL', 'WRITE',
     :   XPTR, NELM, STATUS )

*  Set the axis data.
      IF ( NDF(1) .EQ. NDF__NOID ) THEN

*     Fill according to parameters.
         CALL SPD_UAAJR( XSTART, XEND, NELM, %VAL( CNF_PVAL(XPTR) ),
     :                   STATUS )
      ELSE

*     Copy input axis data.
         CALL VEC_RTOR( .FALSE., NELM, %VAL( CNF_PVAL(XPTR1) ),
     :                  %VAL( CNF_PVAL(XPTR) ), I, J, STATUS )
      END IF

*  Set axis and data info.
      CALL NDF_ACPUT( LABEL, NDF(2), 'LABEL', 1, STATUS )
      CALL NDF_ACPUT( UNITS, NDF(2), 'UNITS', 1, STATUS )
      CALL NDF_CPUT( 'intensity', NDF(2), 'LABEL', STATUS )
      IF ( LOGAR ) THEN
         CALL NDF_CPUT( 'log10(Jy/sr)', NDF(2), 'UNITS', STATUS )
      ELSE
         CALL NDF_CPUT( 'Jy/sr', NDF(2), 'UNITS', STATUS )
      END IF

*  Work out the intensity values.
      CALL SPD_WZNA( NELM, TEMP, LBCODE, UNFACT, LGCOD1, LGCOD2,
     :               %VAL( CNF_PVAL(XPTR) ),
     :               %VAL( CNF_PVAL(DPTR) ), STATUS )

* If a temperature error is given, work out the maximum and minimum
* intensity values and then calculate the average errors on these
* intensities and convert to variance.
      IF (ERRFLG) THEN

*    Create temporary arrays for the maximum and minimum variances.
        CALL PSX_CALLOC( NELM, '_REAL', EMXPTR, STATUS)
        CALL PSX_CALLOC( NELM, '_REAL', EMNPTR, STATUS)

*    Compute black-body spectra at the maximum and minimum temperatures
*    of the error range.
        MXTEMP = TEMP + ERRTMP
        CALL SPD_WZNA( NELM, MXTEMP, LBCODE, UNFACT, LGCOD1, LGCOD2,
     :                 %VAL( CNF_PVAL(XPTR) ),
     :                 %VAL( CNF_PVAL(EMXPTR) ), STATUS )

        MNTEMP = TEMP - ERRTMP
        CALL SPD_WZNA( NELM, MNTEMP, LBCODE, UNFACT, LGCOD1, LGCOD2,
     :                 %VAL( CNF_PVAL(XPTR) ),
     :                 %VAL( CNF_PVAL(EMNPTR) ), STATUS )

*    Calculate the variance from the maximum and minimum spectra.
        CALL SPD_ERTMP( NELM, %VAL(CNF_PVAL(DPTR)),
     :                  %VAL(CNF_PVAL(EMXPTR)), %VAL(CNF_PVAL(EMNPTR)),
     :                  %VAL(CNF_PVAL(EPTR)), STATUS )

*    Delete the temporary maximum and minimum variance arrays.
        CALL PSX_FREE( EMXPTR, STATUS )
        CALL PSX_FREE( EMNPTR, STATUS )
      END IF

*  Close down.

 500  CONTINUE
      CALL NDF_END( STATUS )
      END
