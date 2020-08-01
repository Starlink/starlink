      SUBROUTINE FOURIER( STATUS )
*+
*  Name:
*     FOURIER

*  Purpose:
*     Performs forward and inverse Fourier transforms of 1- or
*     2-dimensional NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FOURIER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application performs forward or reverse Fast Fourier
*     Transforms (FFTs) of 1- or 2-dimensional NDFs.  The output in the
*     forward transformation (from the space domain to the Fourier) can
*     be produced in Hermitian form in a single NDF, or as two NDFs
*     giving the real and imaginary parts of the complex transform, or
*     as two NDFs giving the power and phase of the complex transform.
*     Any combination of these may also be produced.  The inverse
*     procedure accepts any of these NDFs and produces a purely real
*     output NDF.
*
*     Any bad pixels in the input NDF may be replaced by a constant
*     value.  Input NDFs need neither be square, nor be a power of 2 in
*     size in either dimension; their shape is arbitrary.
*
*     The Hermitian transform is a single image in which each quadrant
*     consists of a linear combination of the real and imaginary
*     parts of the transform.  This form is useful if you just want to
*     multiply the Fourier transform by some known purely real mask and
*     then invert it to get a filtered image.  However, if you want to
*     multiply the Fourier transform by a complex mask (e.g. the
*     Fourier transform of another NDF), or do any other operation
*     involving combining complex values, then the Hermitian NDF must
*     be untangled into separate real and imaginary parts.
*
*     There is an option to swap the quadrants of the input NDF around
*     before performing a forward FFT.  This is useful if you want to
*     perform convolutions with the FFTs, since the point-spread
*     function (PSF) image can be created with the PSF centre at the
*     array centre, rather than at pixel (1,1) as is usually required.

*  Usage:
*     fourier in hermout

*  ADAM Parameters:
*     FILLVAL = LITERAL (Read)
*        A value to replace bad pixels before performing the transform.
*        The input image is also padded with this value if necessary to
*        form an image of acceptable size.  A value of "Mean" will cause
*        the mean value in the array to be used. [0.0]
*     HERMIN = NDF (Read)
*        Hermitian frequency-domain input NDF containing the complex
*        transform.  If null is entered no Hermitian NDF is read and
*        then the application should be supplied either separate real
*        and imaginary NDFs, or the power and phase NDFs. Prompting
*        will not occur if one of the other (inverse) input NDFs has
*        been given on the command line, but not HERMIN as well.  This
*        parameter is only relevant for an inverse transformation.
*     HERMOUT = NDF (Write)
*        Hermitian output NDF from a forward transform.  If a null value
*        is given then this NDF is not produced.
*     HM_TITLE = LITERAL (Read)
*        Title for the Hermitian Fourier-transform output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier - Hermitian"]
*     IM_TITLE = LITERAL (Read)
*        Title for the frequency-domain imaginary output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier - Imaginary"]
*     IMAGIN = NDF (Read)
*        Input frequency-domain NDF containing the real part of the
*        complex transform.  If a null is given then an image of zeros
*        is assumed unless a null is also given for REALIN, in which
*        case the input is requested in power and phase form.  This
*        parameter is only available if HERMIN is not used.  One way to
*        achieve that is to supply IMAGIN, but not HERMIN, on the
*        command line.  This parameter is only relevant for an inverse
*        transformation.
*     IMAGOUT = NDF (Write)
*        Frequency-domain output NDF containing the imaginary part of
*        the complex Fourier transform.  If a null value is given then
*        this NDF is not produced. [!]
*     IN = NDF (Read)
*        Real (space-domain) input NDF for a forward transformation.
*        There are no restrictions on the size or shape of the input
*        NDF, although the it may have to be padded or trimmed before
*        being transformed. This parameter is only used if a forward
*        transformation was requested.
*     INVERSE = _LOGICAL (Read)
*        If TRUE, then the inverse transform---frequency domain to
*        space domain---is required, otherwise a transform from the
*        space to the frequency domain is undertaken. [FALSE]
*     OUT = NDF (Write)
*        Real space-domain output NDF.  This parameter is only used if
*        an inverse transformation is requested.
*     PH_TITLE = LITERAL (Read)
*        Title for the frequency-domain phase output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier - Phase"]
*     PHASEIN = NDF (Read)
*        Input frequency-domain NDF containing the phase of the complex
*        transform.  If a null is given then an image of zeros is
*        assumed unless a null is also given for PHASEIN, in which
*        case the application quits.  This parameter is only available
*        if HERMIN, REALIN, and IMAGIN are all not used.   One way to
*        achieve that is to supply PHASEIN, but none of the
*        aforementioned parameters, on the command line.  This
*        parameter is only relevant for an inverse transformation.
*     PHASEOUT = NDF (Write)
*        Frequency-domain output NDF containing the phase of the
*        complex Fourier transform.  If a null value is given then this
*        NDF is not produced. [!]
*     POWERIN = NDF (Read)
*        Input frequency-domain NDF containing the modulus of the
*        complex transform.  Note, this should be the square root of the
*        power rather than the power itself.  If a null is given then an
*        image of zeros is assumed unless a null is also given for
*        PHASEIN, in which case the application quits.  This parameter
*        is only available if HERMIN, REALIN, and IMAGIN are all not
*        used.  One way to achieve that is to supply POWERIN, but none
*        of the aforementioned parameters, on the command line.  This
*        parameter is only relevant for an inverse transformation.
*     POWEROUT = NDF (Write)
*        Frequency-domain output NDF containing the modulus of the
*        complex Fourier transform.  Note, this is the square root of
*        the power rather than the power itself.  If a null value is
*        given then this NDF is not produced. [!]
*     PW_TITLE = LITERAL (Read)
*        Title for the frequency-domain power output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier - Power"]
*     REALIN = NDF (Read)
*        Input frequency-domain NDF containing the real part of the
*        complex transform.  If a null is given then an image of zeros is
*        assumed unless a null is also given for IMAGIN, in which case
*        the input is requested in power and phase form.  This parameter
*        is only available if HERMIN is not used.  One way to achieve
*        that is to supply REALIN, but not HERMIN, on the command
*        line.  This parameter is only relevant for an inverse
*        transformation.
*     REALOUT = NDF (Write)
*        Frequency-domain output NDF containing the real part of the
*        complex Fourier transform.  If a null value is given then this
*        NDF is not produced. [!]
*     RL_TITLE = LITERAL (Read)
*        Title for the frequency-domain real output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier - Real"]
*     SHIFT = _LOGICAL (Read)
*        If TRUE, the transform origin is to be located at the array's
*        centre.  This is implemented by swapping bottom-left and
*        top-right, and bottom-right and top-left array quadrants,
*        before doing the transform.  This results in the transformation
*        effectively being done about pixel x = INT(NAXIS1/2)+1 and
*        y = INT(NAXIS2/2)+1, where NAXISn are the padded or trimmed
*        dimensions of the NDF. [FALSE]
*     TRIM = LOGICAL (Read)
*        If TRUE, when the input array dimension cannot be processed by
*        the transform, the output arrays will be trimmed rather than
*        padded with the fill value. [FALSE]
*     TITLE = LITERAL (Read)
*        Title for the real space-domain output NDF.
*        A null (!) value means using the title of the input NDF.
*        ["KAPPA - Fourier"]

*  Examples:
*     fourier galaxy ft_gal
*        Makes an Hermitian Fourier transform stored in an NDF called
*        ft_gal from the 2-d NDF called galaxy.
*     fourier hermin=ft_gal out=galaxy inverse
*        Takes an Hermitian Fourier transform stored in an NDF called
*        ft_gal and performs the inverse transformation to yield a
*        normal (spatial domain) image in NDF galaxy.
*     fourier in=galaxy powerout=galpow hermout=ft_gal fillval=mean
*        Makes an Hermitian Fourier transform stored in an NDF called
*        ft_gal from the 2-d NDF called galaxy.  Any bad values in
*        galaxy are replaced by the mean data value of galaxy.  In
*        addition the power of the transform is written to an NDF
*        called galpow.
*     fourier realin=real_gal out=galaxy inverse
*        Takes the real component of a Fourier transform stored in an
*        NDF called real_gal and performs the inverse transformation to
*        yield a normal image in NDF galaxy.

*  Notes:
*     - See the NAG documentation, Chapter C06, and/or KAPPA routine
*     KPG1_HMLTX.GEN for more details of Hermitian Fourier transforms.

*  Algorithm:
*     The application is based on a NAG Hermitian FFT routine.  It
*     utilises the symmetry present in the FFT of a purely real array
*     to reduce the size of the problem by a factor of 4 (2 in each
*     dimension).  The resulting transform is initially returned as a
*     single array in which each quadrant consists of a linear
*     combination of the real and imaginary parts of the transform.
*
*     - Get the input parameters.
*     - Start the NDF context.
*     - The application has two parts: forward and reverse
*       transformation.  First the forward:
*       - Get the space-domain NDF, obtain its bounds and check that the
*         dimensionality is 2.
*       - Calculate the size of the Fourier transform. Create an NDF
*         section of the padded dimensions.
*       - Create the required output NDFs, propagated from the NDF
*         section, but not copying AXIS, QUALITY, and VARIANCE.
*       - Map the NDFs and required work space.
*       - Pad or trim the array, fill in bad pixels, and swap quadrants
*         if required.
*       - Perform the forward transformation, copy the data to the
*         output NDFs, calculating the power and phase where necessary,
*         and get the titles for the output NDFs.
*       - Unmap and tidy all the NDFs and work space.
*     - Now the reverse transformation:
*       - Get the frequency-domain NDFs, first trying the Hermitian,
*         then the real and imaginary, then the power and phase until
*         at least one input NDF is given.  Check the parameter states
*         to prevent unnecessary prompting. Obtain its/their bounds and
*         check that the dimensionality is 2, and that pairs of NDFs
*         have matching dimensions.
*       - Calculate the size of the Fourier transform. Create an NDF
*         section of the padded dimensions.
*       - Map the input NDF(s) and required work space.
*       - Pad or trim the arrays, fill in bad pixels, and swap quadrants
*         if required, or fill a missing pair member with zeros (e.g.
*         fill the phase if only the power NDF given).  If phase and/or
*         power given then convert to real and imaginary.  Produce a
*         Hermitian if the input was not a Hermitian NDF.
*       - Perform the reverse transformation.
*       - Create the required output space-domain NDF, propagated from
*         the NDF section, but not copying AXIS, QUALITY, and VARIANCE,
*         and map it.
*       - Copy the data to the output NDF, swapping quadrants if
*         required.  Get the title for the output NDF.
*       - Unmap and tidy all the NDFs and work space.
*     - End the NDF context.

*  Related Applications:
*     KAPPA: CONVOLVE, LUCY, MEM2D, WIENER; Figaro: BFFT, CMPLX*,
*     COSBELL, FFT, *2CMPLX.

*  Implementation Status:
*     - AXIS, VARIANCE and QUALITY are not propagated from the input to
*       output NDFs, but the LABEL, TITLE, HISTORY components and all
*       extensions are.  Arithmetic is performed using single- or
*       double-precision floating point, as appropriate for the type of
*       the data array.

*  Copyright:
*     Copyright (C) 1988, 1990-1992 Science & Engineering Research
*     Council. Copyright (C) 1995, 1998, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2005 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1988 Jun 6 (DSB):
*        Original version.
*     1990 Mar 19 (MJC):
*        Converted to KAPPA/ADAM style.
*     1990 Mar 29 (MJC):
*        Added mean option for fill values and the trim option.
*     20-MAY-1991 (DSB):
*        Bug fixed which caused incorrect power and phase images to be
*        used when performing an inverse transformation.
*     1991 May 20 (MJC):
*        Added error reports when no input or output NDFs are specified.
*        There is less prompting for input NDFs during an inverse
*        transformation when the values of one or more input NDFs are
*        supplied on the command line.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 21 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     6-JAN-1995 (DSB):
*        SUBPAR constants replaced by PAR constants.  AIF VM routines
*        replaced by PSX routines.  Re-format to edstar-style
*        commenting.  Add final context error report.  Changed to use
*        double precision arithmetic.  Use of STATV replaced by
*        KPG1_MEAND.
*     1995 March 29 (MJC):
*        Changed the Usage and Examples to lowercase, added Related
*        Applications section, removed redundant variables previously
*        needed by STATV, used a modern-style variable declaration,
*        made message reporting conditional, revised the commentary for
*        the 1-d case and no NAG usage and used headings to break up
*        the code, and various minor stylistic changes.
*     1995 March 30 (MJC):
*        Made to work on single precision too.
*     27-FEB-1998 (DSB):
*        Mistaken calls to KPS1_FOPPx to perform filling and padding
*        corrected so that they call KPS1_FOPRx.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     2005 September 17 (TIMJ):
*        Prefer VEC_xTOx over COPAx
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'NDF_ERR'          ! NDF error constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXINDF             ! Maximum number of simultaneous input
                                 ! NDFs (inverse)
      PARAMETER ( MXINDF = 2 )

      INTEGER NDIM               ! Maximum dimensionality of the NDFs
      PARAMETER ( NDIM = 2 )

*  Local Variables:
      INTEGER ACTHER             ! The HERMIN parameter state
      INTEGER ACTIMA             ! The IMAGIN parameter state
      INTEGER ACTPHA             ! The PHASEIN parameter state
      INTEGER ACTPOW             ! The POWERIN parameter state
      INTEGER ACTREA             ! The REALIN parameter state
      LOGICAL AVFILL             ! Average in the array is the fill
                                 ! value?
      CHARACTER * ( 20 ) CFILL   ! Fill value for invalid pixels and
                                 ! padding, but may contain special
                                 ! options
      DOUBLE PRECISION DFILL     ! Fill value for invalid pixels and
                                 ! padding
      INTEGER DIM( NDIM )        ! Dimensions of output data array, i.e.
                                 ! input dimensions plus any padding
      DOUBLE PRECISION DMXDEV    ! The maximum difference between
                                 ! estimates of the same Fourier
                                 ! component
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Final data type
      INTEGER EL                 ! Total no. of pixels in output data
                                 ! arrays returned by mapping calls
      REAL FILL                  ! Fill value for invalid pixels and
                                 ! padding
      INTEGER FTWKSZ             ! Size of work array needed for FFTs
      LOGICAL HERMI              ! Hermitian NDF given?
      LOGICAL HERMO              ! Hermitian NDF created?
      INTEGER IERR               ! Error position from VEC_xTOx
      INTEGER IDIM( NDIM )       ! Dimensions of input imaginary data
                                 ! arrays
      LOGICAL IMAGI              ! Imaginary NDF given?
      LOGICAL IMAGO              ! Imaginary NDF created?
      LOGICAL INVERS             ! An inverse transformation required?
      INTEGER INPNTS             ! Total no. of pixels in input data
                                 ! arrays
*
                                 ! Pointer to:
      INTEGER IP                 ! Real version of forward trans input
                                 ! after pre-processing
      INTEGER IPH                ! Hermitian input from power/phase or
                                 ! real/imaginary
      INTEGER IPHO               ! Hermitian output of forward trans
      INTEGER IPI                ! Imaginary input work space
      INTEGER IPIN               ! Input array for forward trans
      INTEGER IPINH              ! Hermitian input array
      INTEGER IPIO               ! Imaginary output
      INTEGER IPO                ! Output from inverse transform
      INTEGER IPPO               ! Power output image
      INTEGER IPR                ! Real (component) input work space
      INTEGER IPRO               ! Real (component) output
      INTEGER IPW1               ! Work array
      INTEGER IPW2               ! Work array
      INTEGER IPW3               ! Work array
      INTEGER IPINI              ! Imaginary input
      INTEGER IPINR              ! Real (component) input
      INTEGER IPZO               ! Phase output array
                                 ! End of pointers
*
      CHARACTER * ( NDF__SZFTP ) ITYPE ! Implementation data type
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of the imaginary NDFs
      INTEGER LBNDR( NDF__MXDIM ) ! Lower bounds of the real NDFs
      INTEGER LDIM               ! Dimensionality of input imaginary NDF
      REAL MAXDEV                ! The maximum difference between
                                 ! estimates of the same Fourier
                                 ! component
      INTEGER MDIM               ! Dimensionality of input NDF
*
                                 ! NDF identifiers to:
      INTEGER NERR               ! Error count from VEC_xTOx
      INTEGER NDFHI              ! Hermitian input
      INTEGER NDFHO              ! Hermitian output
      INTEGER NDFI               ! Generic imaginary array for reverse
                                 ! transform
      INTEGER NDFIC              ! Imaginary input section
      INTEGER NDFII              ! Imaginary input
      INTEGER NDFIO              ! Imaginary output
      INTEGER NDFLIS( MXINDF )   ! Input data arrays (inverse)
      INTEGER NDFPI              ! Power input
      INTEGER NDFPO              ! Power output
      INTEGER NDFR               ! Generic real for reverse transform
      INTEGER NDFRC              ! Real input section
      INTEGER NDFRI              ! Real (component) input
      INTEGER NDFRO              ! Real (component) output
      INTEGER NDFRS              ! Input/output real array
      INTEGER NDFRSC             ! Input section
      INTEGER NDFS               ! Padded section (real input)
      INTEGER NDFSI              ! Padded section (imaginary input)
      INTEGER NDFZI              ! Phase input
      INTEGER NDFZO              ! Phase output
                                 ! End of NDF indentifiers
*
      INTEGER NN                 ! Number NDFs for which to match types
      INTEGER NPNTS              ! Total no. of pixels in output data
                                 ! arrays
      LOGICAL PHASEI             ! Phase NDF given?
      LOGICAL PHASEO             ! Phase NDF created?
      LOGICAL PORP               ! Power and/or phase inputs given?
      LOGICAL POWERI             ! Power NDF given?
      LOGICAL POWERO             ! Power NDF created?
      INTEGER QUART              ! Quadrants are to be swapped
                                 ! (effectively a copy of SQUAD) though
                                 ! sign indicates direction of swap
      INTEGER RDIM( NDIM )       ! Dimensions of input real data arrays
      LOGICAL REALI              ! Real NDF given?
      LOGICAL REALO              ! Real NDF created?
      LOGICAL RORI               ! Real and/or imaginary inputs given?
      INTEGER SDIMI( NDF__MXDIM )! Significant dimensions of imaginary
                                 ! NDF
      INTEGER SDIMR( NDF__MXDIM )! Significant dimensions of real NDF
      LOGICAL SQUAD              ! Quadrants are to swapped?
      LOGICAL TRIM               ! Input arrays to be trimmed rather
                                 ! than padded?
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of the imaginary NDFs
      INTEGER UBNDR( NDF__MXDIM ) ! Upper bounds of the real NDFs
      LOGICAL UNTANG             ! Real or imaginary outputs required
                                 ! during forward transformation?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain some parameter values.
*  =============================

*  See if forward or reverse transformation is required.
      CALL PAR_GTD0L( 'INVERSE', .TRUE., .TRUE., INVERS, STATUS )

*  See if the quadrants of real array are to be swapped.
      CALL PAR_GTD0L( 'SHIFT', .FALSE., .TRUE., SQUAD, STATUS )
      IF ( SQUAD ) THEN
         QUART = 1
      ELSE
         QUART = 0
      END IF

*  See if trimming or padding is required.
      CALL PAR_GTD0L( 'TRIM', .FALSE., .TRUE., TRIM, STATUS )

*  Get value with which to replace invalid pixels.
      CALL PAR_MIX0D( 'FILLVAL', '0.0', VAL__MIND, VAL__MAXD, 'Mean',
     :                .FALSE., CFILL, STATUS )

*  Look for the special option to use the mean value in the array.
      IF ( CFILL .EQ. 'MEAN' ) THEN
         AVFILL = .TRUE.
      ELSE
         AVFILL = .FALSE.

*  Convert the output numeric string to its numeric value.  At this
*  stage we do not know whether the implementation type is real or
*  double precision.
         CALL CHR_CTOD( CFILL, DFILL, STATUS )
         FILL = REAL( DFILL )
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  The separate transformations are between the dotted lines.

*  ----------------------------------------------------------------------
*  If a forward transformation required then...
      IF ( .NOT. INVERS ) THEN

*  Obtain the input NDF and its significant dimensions.
*  ====================================================

*  Get the (purely real) input image.
         CALL LPG_ASSOC( 'IN', 'READ', NDFRS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find whether or not there are no more than two significant
*  dimensions and which ones they are.
         CALL KPG1_SDIMP( NDFRS, NDIM, SDIMR, STATUS )

*  Obtain the bounds of the image.
         CALL NDF_BOUND( NDFRS, NDF__MXDIM, LBNDR, UBNDR, MDIM, STATUS )

*  Set upper insignificant bounds to one.  We have to make a section so
*  that trailing insignificant bounds may be shifted when the user has
*  specified the whole NDF.  This cannot be done for the base NDF.
         CALL NDF_SECT( NDFRS, MDIM, LBNDR, UBNDR, NDFRSC, STATUS )
         CALL KPG1_SECSH( NDFRSC, SDIMR( NDIM ), STATUS )

*  Must have a 2-d.
         IF ( STATUS .EQ. NDF__XSDIM ) THEN
            CALL NDF_MSG( 'NDF', NDFRS )
            CALL ERR_REP( 'FOURIER_BADDIM',
     :        'FOURIER: Purely real NDF ^NDF has dimensionality that '/
     :        /'cannot be processed.', STATUS )
            GOTO 999
         END IF

*  Protect against access violations when there is an error obtaining
*  the significant dimensions.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the significant dimensions from the lower and upper
*  bounds.  NDF_DIM is not used because a section may be required.
         RDIM( 1 ) = UBNDR( SDIMR( 1 ) ) - LBNDR( SDIMR( 1 ) ) + 1
         RDIM( 2 ) = UBNDR( SDIMR( 2 ) ) - LBNDR( SDIMR( 2 ) ) + 1

*  Calculate and report the size of the output NDFs.
*  =================================================

*  Calculate the size of output NDFs which can be processed by the FFT
*  routine. (For FFTPACK these return the supplied value.)
         IF ( TRIM ) THEN
            CALL FTSIZT( RDIM( 1 ), DIM( 1 ), STATUS )
            CALL FTSIZT( RDIM( 2 ), DIM( 2 ), STATUS )
         ELSE
            CALL FTSIZE( RDIM( 1 ), DIM( 1 ), STATUS )
            CALL FTSIZE( RDIM( 2 ), DIM( 2 ), STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Let the user know what is happening.
            IF ( RDIM( 1 ) .NE. DIM( 1 ) .OR.
     :           RDIM( 2 ) .NE. DIM( 2 ) ) THEN

               CALL MSG_SETI( 'DIM1', DIM( 1 ) )
               CALL MSG_SETI( 'DIM2', DIM( 2 ) )

               IF ( TRIM ) THEN
                  CALL MSG_OUTIF( MSG__NORM, 'PADDING', 'The array '/
     :              /'will be trimmed to ^DIM1 by ^DIM2.', STATUS )

               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'PADDING', 'The array '/
     :              /'will be padded to ^DIM1 by ^DIM2.', STATUS )
               END IF

*  Apply the padding to the upper bounds.
               UBNDR( SDIMR( 1 ) ) = LBNDR( SDIMR( 1 ) ) + DIM( 1 ) - 1
               UBNDR( SDIMR( 2 ) ) = LBNDR( SDIMR( 2 ) ) + DIM( 2 ) - 1

            ELSE

*  Just copy the original dimensions to the output arrays since these
*  have not been padded.
               DIM( 1 ) = RDIM( 1 )
               DIM( 2 ) = RDIM( 2 )

            END IF

*  Create a padded section.
*  ========================

*  Do the padding by creating a superset.
            CALL NDF_SECT( NDFRSC, SDIMR( NDIM ), LBNDR, UBNDR, NDFS,
     :                     STATUS )

*  Create the output NDFs and determine if untangling is needed.
*  =============================================================

*  Start a new error context.
            CALL ERR_MARK

*  See which output NDFs the user requires.  A null response means the
*  NDF is not required.  Set flags for each output NDF for future
*  reference.  Note axis and quality information cannot be propagated
*  to Fourier space.
*
*  1st: Hermitian output
            HERMO = .FALSE.
            CALL LPG_PROP( NDFS, ' ', 'HERMOUT', NDFHO, STATUS )

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               HERMO = .TRUE.
            END IF

*  2nd: real part of complex output.
            REALO = .FALSE.
            CALL LPG_PROP( NDFS, ' ', 'REALOUT', NDFRO, STATUS )

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               REALO = .TRUE.
            END IF

*  3nd: imaginary part of complex output.
            IMAGO = .FALSE.
            CALL LPG_PROP( NDFS, ' ', 'IMAGOUT', NDFIO, STATUS )

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               IMAGO = .TRUE.
            END IF

*  4th: power.
            POWERO = .FALSE.
            CALL LPG_PROP( NDFS, ' ', 'POWEROUT', NDFPO, STATUS )

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               POWERO = .TRUE.
            END IF

*  5th: phase.
            PHASEO = .FALSE.
            CALL LPG_PROP( NDFS, ' ', 'PHASEOUT', NDFZO, STATUS )

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               PHASEO = .TRUE.
            END IF

*  Release the error context.
            CALL ERR_RLSE

*  If none of the output NDFs arrays are required then quit.
            IF ( HERMO .OR. REALO .OR. IMAGO .OR. POWERO .OR.
     :           PHASEO ) THEN

*  See if the Hermitian version will have to be untangled into its
*  component complex parts.
               IF ( REALO .OR. IMAGO .OR. POWERO .OR. PHASEO ) THEN
                  UNTANG = .TRUE.
               ELSE
                  UNTANG = .FALSE.
               END IF

*  Map the data and work arrays.
*  =============================

*  Obtain the implementation type for the processing.  There is no need to check for
*  error NDF__TYPNI because we have double precision in the list of
*  options.
               CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFRS, NDFRS, 'Data',
     :                         ITYPE, DTYPE, STATUS )

*  Map the input and output data arrays.
               CALL KPG1_MAP( NDFRS, 'Data', ITYPE, 'READ', IPIN, EL,
     :                       STATUS )
               IF ( HERMO ) CALL KPG1_MAP( NDFHO, 'Data', ITYPE,
     :                                    'WRITE', IPHO, EL, STATUS )
               IF ( REALO ) CALL KPG1_MAP( NDFRO, 'Data', ITYPE,
     :                                    'WRITE', IPRO, EL, STATUS )
               IF ( IMAGO ) CALL KPG1_MAP( NDFIO, 'Data', ITYPE,
     :                                    'WRITE', IPIO, EL, STATUS )
               IF ( POWERO ) CALL KPG1_MAP( NDFPO, 'Data', ITYPE,
     :                                    'WRITE', IPPO, EL, STATUS )
               IF ( PHASEO ) CALL KPG1_MAP( NDFZO, 'Data', ITYPE,
     :                                    'WRITE', IPZO, EL, STATUS )

*  Get the required work space.
               NPNTS = DIM( 1 ) * DIM( 2 )
               FTWKSZ = 3 * MAX( DIM( 1 ), DIM( 2 ) ) + 15
               CALL PSX_CALLOC( NPNTS, ITYPE, IP, STATUS )
               CALL PSX_CALLOC( NPNTS, ITYPE, IPW1, STATUS )
               CALL PSX_CALLOC( NPNTS, ITYPE, IPW2, STATUS )
               CALL PSX_CALLOC( FTWKSZ, ITYPE, IPW3, STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the mean value.
*  ====================

*  If required, find the mean value of the input array and use it as
*  the filling value.  Call the appropriate routine for the
*  implementation type.
                  IF ( AVFILL ) THEN
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL KPG1_MEANR( RDIM( 1 ) * RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   FILL, STATUS )
                     ELSE
                        CALL KPG1_MEAND( RDIM( 1 ) * RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   DFILL, STATUS )
                     END IF
                  END IF

*  Swap the quadrants.
*  ===================
*  Copy the input data array into real work array, replacing bad pixels
*  by the fill value.  The image is also padded out to the calculated
*  size with pixels containing the fill value.  Call the routines
*  of the appropriate implementation type.
                  IF ( ITYPE .EQ. '_REAL' ) THEN
                     IF ( SQUAD ) THEN

*  If required swap the quadrants of the input array around to produce
*  a shift in the origin of the transform from pixel ( 1, 1 ) to pixel
*  ( INT( ( M+1 ) / 2 ), INT( ( N+1 ) / 2 ), where M is the number of
*  columns, and N is the number of lines in the array.
                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .FALSE.,
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   STATUS )

                        CALL KPS1_FOQUR( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   RDIM( 1 ),
     :                                   RDIM( 2 ), QUART,
     :                                   %VAL( CNF_PVAL( IP ) ),
     :                                   STATUS )
                     ELSE

                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .FALSE.,
     :                                   %VAL( CNF_PVAL( IP ) ),
     :                                   STATUS )
                     END IF

*  As above but for double precision.
                  ELSE
                     IF ( SQUAD ) THEN

*  If required swap the quadrants of the input array around to produce
*  a shift in the origin of the transform from pixel ( 1, 1 ) to pixel
*  ( INT( ( M+1 ) / 2 ), INT( ( N+1 ) / 2 ), where M is the number of
*  columns, and N is the number of lines in the array.
                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .FALSE.,
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   STATUS )

                        CALL KPS1_FOQUD( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   RDIM( 1 ),
     :                                   RDIM( 2 ), QUART,
     :                                   %VAL( CNF_PVAL( IP ) ),
     :                                   STATUS )
                     ELSE

                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPIN ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .FALSE.,
     :                                   %VAL( CNF_PVAL( IP ) ),
     :                                   STATUS )
                     END IF
                  END IF

*  Do the forward Fourier transformation.
*  ======================================

*  Call the appropriate routine for the implementation type.
                  IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_FOFOR( DIM( 1 ), DIM( 2 ), UNTANG,
     :                                %VAL( CNF_PVAL( IP ) ),
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                %VAL( CNF_PVAL( IPW3 ) ),
     :                                %VAL( CNF_PVAL( IPW2 ) ),
     :                                STATUS )
                  ELSE
                     CALL KPS1_FOFOD( DIM( 1 ), DIM( 2 ), UNTANG,
     :                                %VAL( CNF_PVAL( IP ) ),
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                %VAL( CNF_PVAL( IPW3 ) ),
     :                                %VAL( CNF_PVAL( IPW2 ) ),
     :                                STATUS )
                  END IF

*  Make the output arrays.
*  =======================

*  Produce the required output data arrays by copying from the work
*  arrays.
*
*  1st: Hermitian output
                  IF ( HERMO ) THEN
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL VEC_RTOR( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IP ) ),
     :                              %VAL( CNF_PVAL( IPHO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     ELSE
                        CALL VEC_DTOD( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IP ) ),
     :                              %VAL( CNF_PVAL( IPHO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     END IF

*  Unmap the data array.
                     CALL NDF_UNMAP( NDFHO, 'Data', STATUS )

*  Get the title.
                     CALL NDF_CINP( 'HM_TITLE', NDFHO, 'Title',
     :                              STATUS )
                 END IF


*  2nd: real part of complex output.
                  IF ( REALO ) THEN
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL VEC_RTOR( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW2 ) ),
     :                              %VAL( CNF_PVAL( IPRO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     ELSE
                        CALL VEC_DTOD( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW2 ) ),
     :                              %VAL( CNF_PVAL( IPRO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     END IF

*  Unmap the data array.
                     CALL NDF_UNMAP( NDFRO, 'Data', STATUS )

*  Get the title.
                     CALL NDF_CINP( 'RL_TITLE', NDFRO, 'Title',
     :                              STATUS )
                  END IF


*  3rd: imaginary part of complex output.
                  IF ( IMAGO ) THEN
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL VEC_RTOR( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              %VAL( CNF_PVAL( IPIO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     ELSE
                        CALL VEC_DTOD( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              %VAL( CNF_PVAL( IPIO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     END IF

*  Unmap the data array.
                     CALL NDF_UNMAP( NDFIO, 'Data', STATUS )

*  Get the title.
                     CALL NDF_CINP( 'IM_TITLE', NDFIO, 'Title',
     :                              STATUS )
                  END IF

*  4th: power and phase outputs.
                  IF ( POWERO .OR. PHASEO ) THEN

*  The power and phase must be calculated first.
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL KPS1_FOPPR( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW2 ) ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   STATUS )
                     ELSE
                        CALL KPS1_FOPPD( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW2 ) ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   STATUS )
                     END IF

*  Create the power data array.
                     IF ( POWERO ) THEN
                        IF ( ITYPE .EQ. '_REAL' ) THEN
                           CALL VEC_RTOR( .FALSE., NPNTS,
     :                                 %VAL( CNF_PVAL( IPW2 ) ),
     :                                 %VAL( CNF_PVAL( IPPO ) ),
     :                                 IERR, NERR,
     :                                 STATUS )
                        ELSE
                           CALL VEC_DTOD( .FALSE., NPNTS,
     :                                 %VAL( CNF_PVAL( IPW2 ) ),
     :                                 %VAL( CNF_PVAL( IPPO ) ),
     :                                 IERR, NERR,
     :                                 STATUS )
                        END IF

*  Unmap the data array.
                        CALL NDF_UNMAP( NDFPO, 'Data', STATUS )

*  Get the title.
                        CALL NDF_CINP( 'PW_TITLE', NDFPO, 'Title',
     :                                 STATUS )
                     END IF

*  Create the phase data array.
                     IF ( PHASEO ) THEN
                        IF ( ITYPE .EQ. '_REAL' ) THEN
                           CALL VEC_RTOR( .FALSE., NPNTS,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 %VAL( CNF_PVAL( IPZO ) ),
     :                                 IERR, NERR,
     :                                 STATUS )
                        ELSE
                           CALL VEC_DTOD( .FALSE., NPNTS,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 %VAL( CNF_PVAL( IPZO ) ),
     :                                 IERR, NERR,
     :                                 STATUS )
                        END IF

*  Unmap the data array.
                        CALL NDF_UNMAP( NDFZO, 'Data', STATUS )

*  Get the title.
                        CALL NDF_CINP( 'PH_TITLE', NDFZO, 'Title',
     :                                 STATUS )
                     END IF

                  END IF

*  End of section where workspace was required.
               END IF

*  Tidy the work space.
               CALL PSX_FREE( IP, STATUS )
               CALL PSX_FREE( IPW1, STATUS )
               CALL PSX_FREE( IPW2, STATUS )
               CALL PSX_FREE( IPW3, STATUS )

*  Unmap the input real data array.
               CALL NDF_UNMAP( NDFRS, 'Data', STATUS )

            ELSE

*  Report the error and abort.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FOURIER_NODATAO',
     :           'FOURIER: No output NDF supplied.', STATUS )

*  End of check for an output NDF.
            END IF

*  End of check for an input NDF with suitable dimensions.
         END IF


* -------------------------------------------------------------------
*  The inverse transformation is required ...
      ELSE

*  Find which input NDFs are supplied.
*  ===================================
*
*  Find which combination is specified on the command line.  We do not
*  want to prompt unnecessarily.  The priority list is 1) HERMIN, 2)
*  REALIN and/or IMAGIN, and 3) POWERIN and/or PHASEIN.
         CALL LPG_STATE( 'HERMIN', ACTHER, STATUS )
         CALL LPG_STATE( 'REALIN', ACTREA, STATUS )
         CALL LPG_STATE( 'IMAGIN', ACTIMA, STATUS )
         CALL LPG_STATE( 'POWERIN', ACTPOW, STATUS )
         CALL LPG_STATE( 'PHASEIN', ACTPHA, STATUS )

*  Access the Hermitian NDF, if needed.
*  ====================================

*  First see whether the Hermitian may be required.  It is if it
*  specified on the command line or none of the other input NDFs are
*  given there.
         HERMI = ACTHER .EQ. PAR__ACTIVE .OR. .NOT.
     :           ( ( ACTREA .EQ. PAR__ACTIVE .OR.
     :               ACTIMA .EQ. PAR__ACTIVE ) .OR.
     :             ( ACTPOW .EQ. PAR__ACTIVE .OR.
     :               ACTPHA .EQ. PAR__ACTIVE ) )

*  Start a new error context.
         CALL ERR_MARK

         IF ( HERMI ) THEN

*  Get the `Hermitian' input NDF.
            CALL LPG_ASSOC( 'HERMIN', 'READ', NDFHI, STATUS )

*  Check whether or not an NDF was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               HERMI = .FALSE.
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE

*  Copy the identifier to the generic identifier.
               NDFR = NDFHI
            END IF

         END IF

*  Access the other input NDFs.
*  ============================

*  Initialise the flags that indicate which NDFs have been supplied.
         REALI = .FALSE.
         IMAGI = .FALSE.
         POWERI = .FALSE.
         PHASEI = .FALSE.

*  If no Hermitian input NDF was given an alternative input source NDF
*  or NDF pair needs to be associated.  See whether the `power' and/or
*  `phase' NDFs have been supplied on the command line.  If either has
*  we do not want to prompt for the `real' and `imaginary' NDFs.  If
*  neither has, or either of the `real' and `imaginary' NDFs have been
*  given on the command line, the real and imaginary take precedence.
         RORI = .NOT. HERMI .AND.
     :            ( ( ACTREA .EQ. PAR__ACTIVE .OR.
     :                ACTIMA .EQ. PAR__ACTIVE ) .OR. .NOT.
     :              ( ACTPOW .EQ. PAR__ACTIVE .OR.
     :                ACTPHA .EQ. PAR__ACTIVE ) )

         IF ( RORI .AND. ACTREA .EQ. PAR__ACTIVE ) THEN

*  Associate the real image.
            CALL LPG_ASSOC( 'REALIN', 'READ', NDFRI, STATUS )

*  Check whether or not a NDF was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               REALI = .TRUE.

*  Copy the identifier to the generic identifier.
               NDFR = NDFRI
            END IF

         END IF

         IF ( RORI .AND. ACTIMA .EQ. PAR__ACTIVE ) THEN

*  Obtain the imaginary NDF.
            CALL LPG_ASSOC( 'IMAGIN', 'READ', NDFII, STATUS )

*  Check whether or not a NDF was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               IMAGI = .TRUE.

*  Copy the identifier to the generic identifier.
               NDFI = NDFII
            END IF

         END IF

*  If no Hermitian, real or imaginary input NDF was given try to
*  associate power and/or phase input NDFs.
         PORP = .NOT. HERMI .AND. .NOT. REALI .AND. .NOT. IMAGI

*  See whether the power is specified on the command line.
         IF ( PORP .AND. ACTPOW .EQ. PAR__ACTIVE ) THEN

*  Associate the input power.
            CALL LPG_ASSOC( 'POWERIN', 'READ', NDFPI, STATUS )

*  Check whether or not an NDF was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               POWERI = .TRUE.

*  Copy the identifier to the generic identifier.
               NDFR = NDFPI
            END IF

         END IF

*  See whether the phase is specified on the command line.
         IF ( PORP .AND. ACTPHA .EQ. PAR__ACTIVE ) THEN

*  Next get the phase NDF.
            CALL LPG_ASSOC( 'PHASEIN', 'READ', NDFZI, STATUS )

*  Check whether or not an NDF was given.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_RLSE
               GO TO 999
            ELSE
               PHASEI = .TRUE.

*  Copy the identifier to the generic identifier.
               NDFI = NDFZI
            END IF

         END IF

*  Release the error context.
         CALL ERR_RLSE

*  Only proceed if an input NDF has been given.  Otherwise report the
*  error and abort.
         IF ( .NOT. ( HERMI .OR. REALI .OR. IMAGI .OR.
     :        POWERI .OR. PHASEI ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FOURIER_NODATA',
     :        'FOURIER: No input data supplied.', STATUS )
            GOTO 999
         END IF

*  Define & match bounds of the input NDFs and create padded sections.
*  ===================================================================

*  The application can only process NDIM-dimensional arrays, no more,
*  no less.  (For a vector, one of these dimensions is 1.)  So get the
*  bounds and dimensionality of each input NDF.
         MDIM = NDIM
         LDIM = NDIM

*  The NDFs can be obtained via their generic identifiers for the real
*  and imaginary components.  Deal with the `real' components first.
         IF ( HERMI .OR. REALI .OR. POWERI ) THEN

*  Find whether or not there are one or two significant dimensions and
*  which ones they are.
            CALL KPG1_SDIMP( NDFR, NDIM, SDIMR, STATUS )

*  Obtain the bounds of the image.
            CALL NDF_BOUND( NDFR, NDF__MXDIM, LBNDR, UBNDR, MDIM,
     :                      STATUS )

*  Set upper insignificant bounds to one.  We have to make a section so
*  that trailing insignificant bounds may be shifted when the user has
*  specified the whole NDF.  This cannot be done for the base NDF.
            CALL NDF_SECT( NDFR, MDIM, LBNDR, UBNDR, NDFRC, STATUS )
            CALL KPG1_SECSH( NDFRC, SDIMR( NDIM ), STATUS )

*  Must have a 2-d.
            IF ( STATUS .EQ. NDF__XSDIM ) THEN
               CALL NDF_MSG( 'NDF', NDFRS )
               CALL ERR_REP( 'FOURIER_BADDIM',
     :           'FOURIER: Real NDF ^NDF has dimensionality that '/
     :           /'cannot be processed.', STATUS )
               GOTO 999
            END IF

*  Protect against access violations when there is an error obtaining
*  the significant dimensions.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the significant dimensions from the lower and upper
*  bounds.  NDF_DIM is not used because a section may be required.
            RDIM( 1 ) = UBNDR( SDIMR( 1 ) ) - LBNDR( SDIMR( 1 ) ) + 1
            RDIM( 2 ) = UBNDR( SDIMR( 2 ) ) - LBNDR( SDIMR( 2 ) ) + 1

         END IF

*  Repeat the bounds and dimension checking but this time for the
*  `imaginary' NDFs.
         IF ( IMAGI .OR. PHASEI ) THEN

*  Find whether or not there are no more than two significant
*  dimensions and which ones they are.
            CALL KPG1_SDIMP( NDFI, NDIM, SDIMI, STATUS )

*  Obtain the bounds of the image.
            CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDI, UBNDI, LDIM,
     :                      STATUS )

*  Set upper insignificant bounds to one.  We have to make a section so
*  that trailing insignificant bounds may be shifted when the user has
*  specified the whole NDF.  This cannot be done for the base NDF.
            CALL NDF_SECT( NDFI, LDIM, LBNDI, UBNDI, NDFIC, STATUS )
            CALL KPG1_SECSH( NDFIC, SDIMI( NDIM ), STATUS )

*  Must have a 2-d.
            IF ( STATUS .EQ. NDF__XSDIM ) THEN
               CALL NDF_MSG( 'NDF', NDFRS )
               CALL ERR_REP( 'FOURIER_BADDIM',
     :           'FOURIER: Imaginary NDF ^NDF has dimensionality that '/
     :           /'cannot be processed.', STATUS )
               GOTO 999
            END IF

*  Protect against access violations when there is an error obtaining
*  the significant dimensions.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the significant dimensions from the lower and upper
*  bounds.  NDF_DIM is not used because a section may be required.
            IDIM( 1 ) = UBNDI( SDIMI( 1 ) ) - LBNDI( SDIMI( 1 ) ) + 1
            IDIM( 2 ) = UBNDI( SDIMI( 2 ) ) - LBNDI( SDIMI( 2 ) ) + 1

         END IF

*  If a pair of input NDFs have been given, check that the dimensions
*  match.
         IF ( ( REALI .AND. IMAGI ) .OR. ( POWERI .AND. PHASEI ) ) THEN
            IF ( IDIM( 1 ) .NE. RDIM( 1 ) .OR.
     :           IDIM( 2 ) .NE. RDIM( 2 ) ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FOURIER_DIMERR',
     :           'Dimensions of the two input NDFs are not the same.',
     :           STATUS )
               GO TO 999
            END IF
         END IF

         IF ( IMAGI .OR. PHASEI ) THEN

*  Copy the dimensions of the imaginary array to the reference
*  dimension.
            RDIM( 1 ) = IDIM( 1 )
            RDIM( 2 ) = IDIM( 2 )

         END IF

*  Find and report the size of the output NDFs, and apply padding.
*  ===============================================================
*
*  Calculate the size of output NDFs which can be processed by the FFT
*  routine. (For FFTPACK, these return the input values as FFTPACK canb
*  accept arbitrarily shaped arrays.)
         IF ( TRIM ) THEN
            CALL FTSIZT( RDIM( 1 ), DIM( 1 ), STATUS )
            CALL FTSIZT( RDIM( 2 ), DIM( 2 ), STATUS )
         ELSE
            CALL FTSIZE( RDIM( 1 ), DIM( 1 ), STATUS )
            CALL FTSIZE( RDIM( 2 ), DIM( 2 ), STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Let the user know what is happening.
            IF ( RDIM( 1 ) .NE. DIM( 1 ) .OR.
     :           RDIM( 2 ) .NE. DIM( 2 ) ) THEN

               CALL MSG_SETI( 'DIM1', DIM( 1 ) )
               CALL MSG_SETI( 'DIM2', DIM( 2 ) )

               IF ( TRIM ) THEN
                  CALL MSG_OUTIF( MSG__NORM, 'PADDING', 'The array '/
     :              /'will be trimmed to ^DIM1 by ^DIM2.', STATUS )

               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'PADDING', 'The array '/
     :              /'will be padded to ^DIM1 by ^DIM2.', STATUS )
               END IF

*  Apply the padding to the (significant) upper bounds.
               IF ( HERMI .OR. REALI .OR. POWERI ) THEN
                  UBNDR( SDIMR( 1 ) ) = LBNDR( SDIMR( 1 ) ) + DIM( 1 )
     :                                  - 1
                  UBNDR( SDIMR( 2 ) ) = LBNDR( SDIMR( 2 ) ) + DIM( 2 )
     :                                  - 1
               ELSE
                  UBNDI( SDIMI( 1 ) ) = LBNDI( SDIMI( 1 ) ) + DIM( 1 )
     :                                  - 1
                  UBNDI( SDIMI( 2 ) ) = LBNDI( SDIMI( 2 ) ) + DIM( 2 )
     :                                  - 1
               END IF

            ELSE

*  Just copy the original dimensions to the output array since it has
*  not been padded.
               DIM( 1 ) = RDIM( 1 )
               DIM( 2 ) = RDIM( 2 )

            END IF

*  Do the padding by creating a superset.
            IF ( HERMI .OR. REALI .OR. POWERI )
     :         CALL NDF_SECT( NDFRC, MDIM, LBNDR, UBNDR, NDFS, STATUS )

            IF ( IMAGI .OR. PHASEI )
     :        CALL NDF_SECT( NDFIC, LDIM, LBNDI, UBNDI, NDFSI, STATUS )

*  Map the input data and work arrays.
*  ===================================

*  Store and count the valid input-NDF identifiers.
            NN = 0
            IF ( HERMI ) THEN
               NN = NN + 1
               NDFLIS( NN ) = NDFR
            END IF

            IF ( REALI ) THEN
               NN = NN + 1
               NDFLIS( NN ) = NDFR
            END IF

            IF ( IMAGI ) THEN
               NN = NN + 1
               NDFLIS( NN ) = NDFI
            END IF

            IF ( POWERI ) THEN
               NN = NN + 1
               NDFLIS( NN ) = NDFR
            END IF

            IF ( PHASEI ) THEN
               NN = NN + 1
               NDFLIS( NN ) = NDFI
            END IF

*  Determine the implementation type.  There is no need to check for
*  error NDF__TYPNI because we have double precision in the list of
*  options.
            CALL NDF_MTYPN( '_REAL,_DOUBLE', NN, NDFLIS, 'Data', ITYPE,
     :                      DTYPE, STATUS )

*  Map the NDFs using the implementation data type.
            IF ( HERMI ) CALL KPG1_MAP( NDFR, 'Data', ITYPE,
     :                                 'READ', IPINH, EL, STATUS )
            IF ( REALI ) CALL KPG1_MAP( NDFR, 'Data', ITYPE,
     :                                 'READ', IPINR, EL, STATUS )
            IF ( IMAGI ) CALL KPG1_MAP( NDFI, 'Data', ITYPE,
     :                                 'READ', IPINI, EL, STATUS )
            IF ( POWERI ) CALL KPG1_MAP( NDFR, 'Data', ITYPE,
     :                                  'READ', IPINR, EL, STATUS )
            IF ( PHASEI ) CALL KPG1_MAP( NDFI, 'Data', ITYPE,
     :                                  'READ', IPINI, EL, STATUS )

*  Get the required work space.
            NPNTS = DIM( 1 ) * DIM( 2 )
            FTWKSZ = 3 * MAX( DIM( 1 ), DIM( 2 ) ) + 15

            CALL PSX_CALLOC( NPNTS, ITYPE, IPH, STATUS )
            CALL PSX_CALLOC( NPNTS, ITYPE, IPW1, STATUS )
            CALL PSX_CALLOC( FTWKSZ, ITYPE, IPW2, STATUS )

*  Extra space is required for tangling the real and imaginary
*  components into Hermitian form.
            IF ( .NOT. HERMI ) THEN
               CALL PSX_CALLOC( NPNTS, ITYPE, IPR, STATUS )
               CALL PSX_CALLOC( NPNTS, ITYPE, IPI, STATUS )
            END IF

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Prepare the input arrays.
*  =========================
               INPNTS = RDIM( 1 ) * RDIM( 2 )

*  Process using the appropriate data type.
               IF ( ITYPE .EQ. '_REAL' ) THEN

*  Prepare the input arrays.  Copy the input data array(s) into the
*  work space, replacing bad pixels by the fill value.  The array is
*  also padded out to the calculated size with pixels containing the
*  fill value.  If either array in a pair were not given, the
*  corresponding real array is filled with zeros.
                  IF ( HERMI ) THEN

*  If required, find the mean value of the input array and use it as
*  the filling value.
                     IF ( AVFILL ) CALL KPG1_MEANR( INPNTS,
     :
     :   %VAL( CNF_PVAL( IPINH ) ),
     :                                              FILL, STATUS )

                     CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                %VAL( CNF_PVAL( IPINH ) ),
     :                                DIM( 1 ), DIM( 2 ),
     :                                FILL, .FALSE.,
     :                                %VAL( CNF_PVAL( IPH ) ),
     :                                STATUS )
                  ELSE

                     IF ( .NOT. PORP ) THEN

                        IF ( AVFILL .AND. REALI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEANR( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINR ) ),
     :                                      FILL,
     :                                      STATUS )
                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINR ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .NOT. REALI,
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   STATUS )

                        IF ( AVFILL .AND. IMAGI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEANR( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINI ) ),
     :                                      FILL,
     :                                      STATUS )
                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINI ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .NOT. IMAGI,
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

*  If the power and phase inputs were given convert them to real and
*  imaginary arrays.
                     ELSE

                        IF ( AVFILL .AND. POWERI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEANR( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINR ) ),
     :                                      FILL,
     :                                      STATUS )

                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINR ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .NOT. POWERI,
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   STATUS )

                        IF ( AVFILL .AND. PHASEI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEANR( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINI ) ),
     :                                      FILL,
     :                                      STATUS )

                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRR( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINI ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), FILL, .NOT. PHASEI,
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

*  Finally in this section, convert the phase and power arrays to real
*  and imaginary.
                        CALL KPS1_FORIR( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

                     END IF

*  Produce an Hermitian array from the real and imaginary arrays.
                     CALL KPS1_FOHER( DIM( 1 ), DIM( 2 ),
     :                                %VAL( CNF_PVAL( IPR ) ),
     :                                %VAL( CNF_PVAL( IPI ) ),
     :                                %VAL( CNF_PVAL( IPH ) ),
     :                                MAXDEV, STATUS )

*  Let the user know about the deviation to judge whether or not it is
*  a robust transformation.
                     CALL MSG_SETR( 'MXD', MAXDEV )
                     CALL MSG_OUTIF( MSG__NORM, 'MAXDEV',
     :                 'Maximum difference between estimates of the '/
     :                 /'same Fourier component is ^MXD.', STATUS )

                  END IF

*  Repeat for double precision.
               ELSE

*  Prepare the input arrays.  Copy the input data array(s) into the
*  work space, replacing bad pixels by the fill value.  The array is
*  also padded out to the calculated size with pixels containing the
*  fill value.  If either array in a pair were not given, the
*  corresponding real array is filled with zeros.
                  IF ( HERMI ) THEN

*  If required, find the mean value of the input array and use it as
*  the filling value.
                     IF ( AVFILL ) CALL KPG1_MEAND( INPNTS,
     :
     :   %VAL( CNF_PVAL( IPINH ) ),
     :                                              DFILL, STATUS )

                     CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                %VAL( CNF_PVAL( IPINH ) ),
     :                                DIM( 1 ), DIM( 2 ),
     :                                DFILL, .FALSE.,
     :                                %VAL( CNF_PVAL( IPH ) ),
     :                                STATUS )
                  ELSE

                     IF ( .NOT. PORP ) THEN

                        IF ( AVFILL .AND. REALI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEAND( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINR ) ),
     :                                      DFILL, STATUS )
                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINR ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .NOT. REALI,
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   STATUS )

                        IF ( AVFILL .AND. IMAGI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEAND( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINI ) ),
     :                                      DFILL, STATUS )
                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINI ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .NOT. IMAGI,
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

*  If the power and phase inputs were given convert them to real and
*  imaginary arrays.
                     ELSE

                        IF ( AVFILL .AND. POWERI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEAND( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINR ) ),
     :                                      DFILL, STATUS )

                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINR ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .NOT. POWERI,
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   STATUS )

                        IF ( AVFILL .AND. PHASEI ) THEN

*  Find the mean value of the input array and use it as the filling
*  value.
                           CALL KPG1_MEAND( INPNTS,
     :                                      %VAL( CNF_PVAL( IPINI ) ),
     :                                      DFILL, STATUS )

                        END IF

*  Now do the filling and padding or set to zeros.
                        CALL KPS1_FOPRD( RDIM( 1 ), RDIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPINI ) ),
     :                                   DIM( 1 ),
     :                                   DIM( 2 ), DFILL, .NOT. PHASEI,
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

*  Finally in this section, convert the phase and power arrays to real
*  and imaginary.
                        CALL KPS1_FORID( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPR ) ),
     :                                   %VAL( CNF_PVAL( IPI ) ),
     :                                   STATUS )

                     END IF

*  Produce an Hermitian array from the real and imaginary arrays.
                     CALL KPS1_FOHED( DIM( 1 ), DIM( 2 ),
     :                                %VAL( CNF_PVAL( IPR ) ),
     :                                %VAL( CNF_PVAL( IPI ) ),
     :                                %VAL( CNF_PVAL( IPH ) ),
     :                                DMXDEV, STATUS )

*  Let the user know about the deviation to judge whether or not it is
*  a robust transformation.
                     CALL MSG_SETR( 'MXD', REAL( DMXDEV ) )
                     CALL MSG_OUTIF( MSG__NORM, 'MAXDEV',
     :                 'Maximum difference between estimates of the '/
     :                 /'same Fourier component is ^MXD.', STATUS )

                  END IF

*  End of type check.
               END IF

*  Unmap the input data array(s).
               IF ( HERMI .OR. REALI .OR. POWERI )
     :           CALL NDF_UNMAP( NDFR, 'Data', STATUS )

               IF ( IMAGI .OR. PHASEI )
     :           CALL NDF_UNMAP( NDFI, 'Data', STATUS )

*  The preparations are complete.

*  Do the inverse Fourier transform.
*  =================================

*  Process using the appropriate data type.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPS1_FORER( DIM( 1 ), DIM( 2 ),
     :                             %VAL( CNF_PVAL( IPH ) ),
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )
               ELSE
                  CALL KPS1_FORED( DIM( 1 ), DIM( 2 ),
     :                             %VAL( CNF_PVAL( IPH ) ),
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )
               END IF
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Produce an output space-domain NDF.  The processing rules for AXIS,
*  QUALITY and VARIANCE cannot be defined and so will not be
*  propagated.
                  IF ( HERMI .OR. REALI .OR. POWERI ) THEN
                     CALL LPG_PROP( NDFS, ' ', 'OUT', NDFRS, STATUS )

                  ELSE
                     CALL LPG_PROP( NDFSI, ' ', 'OUT', NDFRS, STATUS )

                  END IF

*  There is no point in continuing if there was an error creating the
*  output NDF, but there is work space mapped that must be unmapped
*  before exiting.
                  IF ( STATUS .NE. SAI__OK ) GO TO 980

*  Map the output space-domain data array.
                  CALL KPG1_MAP( NDFRS, 'Data', ITYPE, 'WRITE', IPO,
     :                          EL, STATUS )

*  Swap the quadrants.
*  ===================

*  Process using the appropriate data type.
                  IF ( ITYPE .EQ. '_REAL' ) THEN

*  If required, swap the quadrants around.
                     IF ( SQUAD ) THEN

                        CALL KPS1_FOQUR( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   RDIM( 1 ),
     :                                   RDIM( 2 ), -QUART,
     :                                   %VAL( CNF_PVAL( IPH ) ),
     :                                   STATUS )

*  Copy the reconfigured array to the output data array.
                        CALL VEC_RTOR( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPH ) ),
     :                              %VAL( CNF_PVAL( IPO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     ELSE

*  Copy the work array to the output data array.
                        CALL VEC_RTOR( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              %VAL( CNF_PVAL( IPO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     END IF

*  Repeat for double precision.
                  ELSE

*  If required, swap the quadrants around.
                     IF ( SQUAD ) THEN

                        CALL KPS1_FOQUD( DIM( 1 ), DIM( 2 ),
     :                                   %VAL( CNF_PVAL( IPW1 ) ),
     :                                   RDIM( 1 ),
     :                                   RDIM( 2 ), -QUART,
     :                                   %VAL( CNF_PVAL( IPH ) ),
     :                                   STATUS )

*  Copy the reconfigured array to the output data array.
                        CALL VEC_DTOD( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPH ) ),
     :                              %VAL( CNF_PVAL( IPO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     ELSE

*  Copy the work array to the output data array.
                        CALL VEC_DTOD( .FALSE., NPNTS,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              %VAL( CNF_PVAL( IPO ) ),
     :                              IERR, NERR,
     :                              STATUS )
                     END IF
                  END IF

*  Get the title for the output NDF.
                  CALL NDF_CINP( 'TITLE', NDFRS, 'Title', STATUS )

*  Unmap the output real data array.
                  CALL NDF_UNMAP( NDFRS, 'Data', STATUS )

*  End of check that the reverse transform was completed successfully.
               END IF

*  End of section where work space was required.
            END IF

  980       CONTINUE

*  Tidy the work space.
            CALL PSX_FREE( IPH, STATUS )
            CALL PSX_FREE( IPW1, STATUS )
            CALL PSX_FREE( IPW2, STATUS )

            IF ( .NOT. HERMI ) THEN
               CALL PSX_FREE( IPR, STATUS )
               CALL PSX_FREE( IPI, STATUS )
            END IF

*  End of check that the dimensions of the array are suitable.
         END IF

*  End of check for a forward or reverse transformation.
      END IF

  999 CONTINUE

*  Write an error report if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FOURIER_ERR', 'FOURIER: Unable to take a '/
     :                 /'Fourier transform of an NDF.', STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
