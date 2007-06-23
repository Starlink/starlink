/* Defining the FPTRAP macro will cause floating point exceptions to
   occur whenever a NaN, inf or overflow is generated. This can make it
   easier to debug the cause of these values. */
#if defined(FPTRAP)
#define _GNU_SOURCE
#include <fenv.h>
#endif

/*
*+
*  Name:
*     MAKECUBE

*  Purpose:
*     Regrid ACSIS spectra into a data cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_makecube( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine converts one or more raw data cubes, spanned by
*     (frequency, detector number, time) axes, into a single output cube
*     spanned by (celestial longitude, celestial latitude, frequency) axes.
*
*     The output can be either a regularly gridded tangent plane
*     projection of the sky, or a sparse array (see parameter SPARSE).
*     If a tangent plane projection is selected, the parameters of the 
*     projection from sky to pixel grid coordinates can be specified using 
*     parameters CROTA, PIXSIZE, REFLAT, REFLON. Alternatively, parameter 
*     AUTOGRID can be set true, in which case projection parameters are 
*     determined automatically in a manner that favours projections that 
*     place samples centrally within pixels.
*
*     Variance values in the output can be calculated wither on the basis
*     of the spread of input dat avalues contributing to each output pixel,
*     or on the basis of the system noise temperature values supplied in the 
*     input NDFs (see parameter GENVAR).

*  ADAM Parameters:
*     AUTOGRID = _LOGICAL (Read)
*          Determines how the dynamic default values should be determined 
*          for the projection parameters CROTA, PIXSIZE, REFLAT and REFLON. 
*          If TRUE, then default projection parameters are determined by 
*          adjusting the grid until as many data samples as possible fall 
*          close to the centre of pixels in the output cube. If FALSE, 
*          REFLON/REFLAT are set to the first pointing BASE position, CROTA 
*          is set to zero, and PIXSIZE is are set to 6 arc-seconds. In 
*          addition, if AUTOGRID is TRUE the precise placement of the tangent 
*          point is adjusted by up to 1 pixel along each spatial pixel axis 
*          in order to optimise the grid. [FALSE]
*     BADMASK = LITERAL (Read)
*          A string determining the way in which bad pixels are propagated
*          from input to output. "AND" should usually be 
*          used since this scheme uses all input data, thus reducing the 
*          noise in the output, and also minimises the number of bad pixels in
*          the output. However, for large data sets, the memory requirements 
*          of the "AND" scheme can be excessive. For this reason, two
*          other schemes, "FIRST" and "OR", are provided which greatly reduce 
*          the memory requirements, at the expense either of introducing more 
*          bad pixels into the output ("OR") or producing higher output noise 
*          levels ("FIRST"). This parameter is only requested if SPREAD is
*          set to "Nearest":
*
*          - "FIRST" -- The bad pixel mask in each output spectrum is 
*          inherited from the first input spectrum that contributes to the 
*          output spectrum. Any subsequent input spectra that contribute
*          to the same output spectrum but which have a different bad pixel 
*          mask are ignored. So an output pixel will be bad if and only if
*          the corresponding pixel in the first input NDF that contributes 
*          to it is bad. Since this scheme ignores entire input spectra
*          if they do not conform to the expected bad pixel mask, the noise 
*          in the output can be higher than using the other schemes. However, 
*          this scheme has the benefit of using much less memory than the
*          "AND" scheme, and will in general produce fewer bad pixels in
*          the output than the "OR" scheme.
*
*          - "OR" -- The bad pixel mask in each output spectrum is the union 
*          (logical OR) of the bad pixel masks for all input spectra that 
*          contribute to the output spectrum. So an output pixel will be
*          bad if any of the input pixels that contribute to it are bad.
*          This scheme will in general produce more bad output pixels than 
*          the "FIRST" scheme, but the non-bad output pixels will have a
*          lower noise because, unlike "FIRST", all the contributing input 
*          data is coadded to produce the good output pixels. Like "FIRST", 
*          this scheme uses much less memory than "AND".
*
*          "AND" -- The bad pixel mask for each output spectrum is the 
*          intersection (logical AND) of the bad pixel masks for all input 
*          spectra that contribute to the output spectrum. So an output 
*          pixel will be bad only if all the input pixels that contribute to 
*          it are bad. This scheme will produce fewer bad output pixels
*          and will also give lower output noise levels than "FIRST" or "OR", 
*          but at the expense of much greater memory requirements.
*
*          ["OR"]
*     CATFRAME = LITERAL (Read)
*          A string determining the co-ordinate Frame in which positions are 
*          to be stored in the output catalogue associated with parameter
*          OUTCAT. The string supplied for CATFRAME can be one of the 
*          following:
*	   
*          - A Domain name such as SKY, AXIS, PIXEL, etc. 
*	   
*          - An integer value giving the index of the required Frame.
*	   
*          - An IRAS90 Sky Co-ordinate System (SCS) values such as 
*          EQUAT(J2000) (see SUN/163).
*	   
*          If a null (!) value is supplied, the positions will be stored 
*          in the current Frame of the output NDF. [!]
*     CATEPOCH = DOUBLE PRECISION (Read)
*          The epoch at which the sky positions stored in the output
*          catalogue were determined. It will only be accessed if an epoch
*          value is needed to qualify the co-ordinate Frame specified by 
*          COLFRAME. If required, it should be given as a decimal years 
*          value, with or without decimal places ("1996.8" for example). 
*          Such values are interpreted as a Besselian epoch if less than 
*          1984.0 and as a Julian epoch otherwise. 
*     CROTA = REAL (Read)
*          The angle, in degrees, from north through east (in the
*          coordinate system specified by the SYSTEM parameter) to the second
*          pixel axis in the output cube. The dynamic default value is
*          determined by the AUTOGRID parameter. []
*     DETECTORS = LITERAL (Read)
*          A group of detector names. Only data from the named detectors
*          will be included in the output cube and catalogue. If a null (!) 
*          value is supplied, data from all detectors will be used. [!]
*     FBL( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the bottom left corner of the output cube
*          (the corner with the smallest PIXEL dimension for axis 1 and the smallest
*          pixel dimension for axis 2). No check is made that the pixel corresponds
*          valid data. Note that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not be reliable.
*     FBR( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the bottom right corner of the output cube
*          (the corner with the largest PIXEL dimension for axis 1 and the smallest
*          pixel dimension for axis 2). No check is made that the pixel corresponds
*          valid data. Note that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not be reliable.
*     FLBND( ) = _DOUBLE (Write)
*          The lower bounds of the bounding box enclosing the output cube in the
*          selected output WCS Frame. The values are calculated even if no output
*          cube is created. Celestial axis values will be in units of radians,
*          spectral axis units will be in the same units of the input frameset
*          (matching those used in the SPECBOUNDS parameter). The parameter
*          is named to be consistent with KAPPA NDFTRACE output.
*     FUBND( ) = _DOUBLE (Write)
*          The upper bounds of the bounding box enclosing the output cube in the
*          selected output WCS Frame. The values are calculated even if no output
*          cube is created. Celestial axis values will be in units of radians,
*          spectral axis units will be in the same units of the input frameset
*          (matching those used in the SPECBOUNDS parameter). The parameter
*          is named to be consistent with KAPPA NDFTRACE output.
*     FTL( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the top left corner of the output cube
*          (the corner with the smallest PIXEL dimension for axis 1 and the largest
*          pixel dimension for axis 2). No check is made that the pixel corresponds
*          valid data. Note that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not be reliable.
*     FTR( ) = _DOUBLE (Write)
*          Sky coordinates (radians) of the top right corner of the output cube
*          (the corner with the largest PIXEL dimension for axis 1 and the largest
*          pixel dimension for axis 2). No check is made that the pixel corresponds
*          valid data. Note that the position is reported for the centre of the pixel.
*          If SPARSE mode is enabled the positions reported will not be reliable.
*     GENVAR = LITERAL (Read)
*          Indicates how the Variance values in the output NDF are to be
*          calculated. It can take any of the following values:
*
*          - "Spread" -- the output Variance values are based on the spread 
*          of input data values contributing to each output pixel. This option 
*          is not available if parameter SPARSE is set TRUE. 
*
*          - "Tsys" -- the output Variance values are based on the system 
*          noise temperature values supplied in the input NDFs. 
*
*          - "None" -- no output Variance values are created.
*
*          ["Tsys"]
*
*     IN = NDF (Read)
*          Input file(s)
*     INWEIGHT = _LOGICAL (Read)
*          Indicates if the input spectra should be weighted when combining 
*          two or more input spectra together to form an output spectrum.
*          If TRUE, the weights used are the reciprocal of the variances
*          associated with the input spectra, as determined from the Tsys 
*          values in the input. [TRUE]
*     LBOUND( 3 ) = _INTEGER (Write)
*          The lower pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied 
*          for parameter OUT.
*     OUT = NDF (Write)
*          Output file. If a null (!) value is supplied, the application
*          will terminate early without creating an output cube, but
*          without reporting an error. Note, the pixel bounds which the
*          output cube would have had will still be written to output 
*          parameters LBOUND and UBOUND, even if a null value is supplied
*          for OUT.
*     OUTCAT = FILENAME (Write)
*          An output catalogue in which to store all the spatial detector 
*          positions used to make the output cube (i.e. those selected using 
*          the DETECTORS parameter). By default, the stored positions are in 
*          the same sky coordinate system as the current Frame in the output 
*          NDF (but see parameter CATFRAME). The label associated with each 
*          row in the catalogue is the detector name. The detector positions 
*          in the catalogue are ordered as follows: all the positions for
*          the first input NDF come first, followed by those for the second 
*          input NDF, etc. Within the group of positions associated with a 
*          single input NDF, the positions for the first time slice come
*          first, followed by the positions for the second time slice, etc.
*          If a null value (!) is supplied, no output catalogue is produced. 
*          See also parameter CATFRAME. [!]
*     PARAMS( 2 ) = _DOUBLE (Read)
*          An optional array which consists of additional parameters
*          required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*          SombCos, and Gauss spreading methods (see parameter SPREAD).
*	   
*          PARAMS( 1 ) is required by all the above schemes. It is used to 
*          specify how many pixels on either side of the output position
*          (that is, the output position corresponding to the centre of the 
*          input pixel) are to receive contributions from the input pixel.
*          Typically, a value of 2 is appropriate and the minimum allowed 
*          value is 1 (i.e. one pixel on each side). A value of zero or 
*          fewer indicates that a suitable number of pixels should be 
*          calculated automatically. [0]
*	   
*          PARAMS( 2 ) is required only by the SombCos, Gauss, SincSinc, 
*          SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*          SincCos schemes, it specifies the number of pixels at which the
*          envelope of the function goes to zero.  The minimum value is
*          1.0, and the run-time default value is 2.0.  For the Gauss and
*          SincGauss scheme, it specifies the full-width at half-maximum
*          (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*          the run-time default is 1.0.  On astronomical images and 
*          spectra, good results are often obtained by approximately 
*          matching the FWHM of the envelope function, given by PARAMS(2),
*          to the point-spread function of the input data.  []
*     PIXSIZE( 2 ) = REAL (Read)
*          Pixel dimensions in the output image, in arcsec. If only one value 
*          is supplied, the same value will be used for both axes. The 
*          dynamic default value is determined by the AUTOGRID parameter. []
*     REFLAT = LITERAL (Read)
*          The formatted celestial latitude value at the tangent point of 
*          the spatial projection in the output cube. This should be provided 
*          in the system specified by parameter SYSTEM. The dynamic default 
*          value is determined by the AUTOGRID parameter. []
*     REFLON = LITERAL (Read)
*          The formatted celestial longitude value at the tangent point of 
*          the spatial projection in the output cube. This should be provided 
*          in the system specified by parameter SYSTEM. The dynamic default 
*          value is determined by the AUTOGRID parameter. []
*     SPARSE = _LOGICAL (Read)
*          Indicates if the spectra in the output cube should be stored
*          as a sparse array, or as a regularly gridded array. If FALSE,
*          pixel axes 1 and 2 of the output cube represent a regularly
*          gridded tangent plane projection of the sky, with parameters
*          determined by CROTA, PIXSIZE, REFLON and REFLAT. Each input
*          spectrum is placed at the appropropriate pixel position in this 
*          3D projection, as given by the celestial coordinates associated
*          with the spectrum. If SPARSE is TRUE, then each input spectrum
*          is given an associated index, starting from 1, and the spectrum
*          with index "I" is stored at pixel position (I,1) in the output 
*          cube (pixel axis 2 will always have the value 1 - that is, axis 
*          2 is a degenerate axis that spans only a single pixel).
*
*          In both cases, the third pixel axis in the output cube
*          corresponds to spectral position (frequency, velocity, etc).
*
*          Whatever the setting of SPARSE, the output NDF's WCS component 
*          can be used to transform pixel position into the corresponding 
*          (celestial longitude, celestial latitude, spectral position) 
*          values. However, if SPARSE is TRUE, then the inverse transformation
*          (i.e. from (long,lat,spec) to pixel coordinates) will not be 
*          defined. This means, for instance, that if a sparse array is
*          displayed as a 2D image, then it will not be possible to
*          annotated the axes with WCS values. Also, whilst KAPPA:WCSMOSAIC 
*          will succesfully align the data in a sparse array with a
*          regularly gridded cube, KAPPA:WCSALIGN will not, since WCSALIGN 
*          needs the inverse transformation to be defined.
*
*          The dynamic default value for SPARSE depends on the value
*          supplied for parameter AUTOGRID. If AUTOGRID is set FALSE,
*          then SPARSE defaults to FALSE. If AUTOGRID is set TRUE, then
*          the default for SPARSE will be TRUE if the algorithm described 
*          under the AUTOGRID parameter fails to find useful default grid 
*          parameters. If the AUTOGRID algorithm succeeds, the default
*          for SPARSE will be FALSE. []
*     SPECBOUNDS = LITERAL (Read)
*          The bounds of the output cube on the spectral axis. Input data
*          that falls outside the supplied range will not be included in
*          the output cube. The supplied parameter value should be a
*          string containing a pair of axis values separated by white space 
*          or commas. The first should be the spectral value corresponding to 
*          lower pixel bound in the output cube, and the second should be 
*          the spectral value corresponding to upper pixel bounds in the 
*          output cube. The supplied values should refer to the spectral
*          system described by the WCS FrameSet of the first input NDF. To
*          see what this is, supply a single colon (":") for the parameter 
*          value. This will display a description of the required spectral 
*          coordinate system, and then re-prompt for a new parameter value.
*          The dynamic defaultis the entire spectral range covered by the
*          input data. []
*     SPREAD = LITERAL (Read)
*          The method to use when spreading each input pixel value out
*          between a group of neighbouring output pixels. If SPARSE is set 
*          TRUE, then SPREAD is not accessed and a value of "Nearest" is
*          always assumed. SPREAD can take the following values:
*	   
*          - "Linear" -- The input pixel value is divided bi-linearly between 
*          the four nearest output pixels.  Produces smoother output NDFs than 
*          the nearest-neighbour scheme.
*	   
*          - "Nearest" -- The input pixel value is assigned completely to the
*          single nearest output pixel. This scheme is much faster than any
*          of the others. 
*	   
*          - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*          offset from the interpolation point (resampling) or transformed
*          input pixel centre (rebinning), and sinc(z)=sin(z)/z.  Use of 
*          this scheme is not recommended.
*	   
*          - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*          valuable general-purpose scheme, intermediate in its visual
*          effect on NDFs between the bi-linear and nearest-neighbour
*          schemes. 
*	   
*          - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*          similar results to the "Sincsinc" scheme.
*	   
*          - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good 
*          results can be obtained by matching the FWHM of the
*          envelope function to the point-spread function of the
*          input data (see parameter PARAMS).
*	   
*          - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*          offset from the transformed input pixel centre, and 
*          somb(z)=2*J1(z)/z (J1 is the first-order Bessel function of the 
*          first kind.  This scheme is similar to the "Sinc" scheme.
*	   
*          - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*          scheme is similar to the "SincCos" scheme.
*	   
*          - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the Gaussian 
*          is given by parameter PARAMS(2), and the point at which to truncate 
*          the Gaussian to zero is given by parameter PARAMS(1).
*	   
*          For further details of these schemes, see the descriptions of 
*          routine AST_REBINx in SUN/211. ["Nearest"]
*     SYSTEM = LITERAL (Read)
*          The celestial coordinate system for the output cube. One of
*          ICRS, GAPPT, FK5, FK4, FK4-NO-E, AZEL, GALACTIC, ECLIPTIC. It
*          can also be given the value "TRACKING", in which case the
*          system used will be which ever system was used as the tracking
*          system during in the observation. The value supplied for the
*          CROTA parameter should refer to the coordinate system specified 
*          by this parameter.
*
*          The choice of system also determines if the telescope is 
*          considered to be tracking a moving object such as a planet or 
*          asteroid. If system is GAPPT or AZEL, then each time slice in
*          the input data will be shifted in order to put the base
*          telescope position (given by TCS_AZ_BC1/2 in the JCMTSTATE
*          extension of the input NDF) at the same pixel position that it
*          had for the first time slice. For any other system, no such 
*          shifts are applied, even if the base telescope position is
*          changing through the observation. [TRACKING]
*     TRIM = _LOGICAL (Read)
*          If TRUE, then the output cube will be trimmed to exclude any
*          borders filled with bad data caused by one or more detectors 
*          having been excluded (see parameter DETECTORS). If FALSE, then 
*          the pixel bounds of the output cube will be such as to include 
*          data from all detectors, whether or not they have been selected 
*          for inclusion using the DETECTORS parameter. [TRUE]
*     UBOUND( 3 ) = _INTEGER (Write)
*          The upper pixel bounds of the output NDF. Note, values will be
*          written to this output parameter even if a null value is supplied 
*          for parameter OUT.
*     USEDETPOS = _LOGICAL (Read)
*          If a true value is supplied, then the detector positions are
*          read from the detector position arrays in each input NDF.
*          Otherwise, the detector positions are calculated on the basis
*          of the FPLANEX/Y arrays. Both methods should (in the absence 
*          of bugs) result in identical cubes. [TRUE]
*     WEIGHTS = _LOGICAL (Read)
*          If TRUE, then the weights associated with the array of output
*          pixels is stored in an extension named ACSISRED, within the output 
*          NDF. If FALSE the weights are discarded once they have been
*          used. These weights record thre relative weight of the input
*          data associated with each output pixel. If SPARSE is set TRUE,
*          then WEIGHTS is not accessed and a FALSE value is assumed. [FALSE]

*  Notes:
*     - For regularly gridded data, the spectral range of the output cube
*     is determined by the intersection (rather than the union) of the
*     input spectral ranges. This is done in order to allow a more memory
*     efficient algorithm to be used.
*     - A FITS extension is added to the output NDF containing any keywords 
*     that are common to all input NDFs. To be included in the output
*     FITS extension, a FITS keyword must be present in the NDF extension
*     of every input NDF, and it must have the same value in all input
*     NDFs.
*     - The output NDF will contain an extension named "SMURF" containing
*     three NDFs named "EXP_TIME", "EFF_TIME" and "TSYS". Each of these NDFs 
*     is 2-dimensional, with the same pixel bounds as the spatial axes of the 
*     main output NDF, so that a pixel in one of these NDFs corresponds
*     to a spectrum in the main output NDF. EXP_TIME holds the sum of the 
*     total exposure times (Ton + Toff) for the input spectra that 
*     contributed to each output spectrum. EFF_TIME holds the sum of the
*     effective integration times (Teff) for the input spectra that contributed
*     to each output spectrum, scaled up by a factor of 4 in order to normalise
*     it to the reported exposure times in EXP_TIME. TSYS holds the effective 
*     system temperature for each output spectrum. The TSYS array is not 
*     created if GENVAR is "None".
*     - FITS keywords EXP_TIME, EFF_TIME and MEDTSYS are added to the output 
*     FITS extension. The EXP_TIME and EFF_TIME keywords holds the median 
*     values of the EXP_TIME and EFF_TIME arrays (stored in the SMURF extension
*     of the output NDF). The MEDTSYS keyword holds the median value of the 
*     TSYS array (also stored in the SMURF extension of the output NDF). If 
*     any of these values cannot be calculated for any reason, the 
*     corresponding FITS keyword is assigned a blank value.
*     - FITS keywords OBSCNT and OBSnnnnn are added to the output FITS
*     extension. These allow for tracking of OBSID FITS headers from the
*     input files. If the OBSID FITS header exists in input files, unique
*     ones from the set of input files are stored in the OBSnnnnn headers,
*     where nnnnn is a zero-padded integer starting at 1. OBSCNT contains
*     the count of unique input OBSID headers. If none of the input files
*     contain an OBSID header, then OBSCNT will exist in the output file
*     with a value of 0, and no OBSnnnnn headers will exist.
*     - FITS keywords PRVCNT and PRVnnnnn are added to the output FITS
*     header. These are simply the names of all the input files included
*     in the output map. PRVCNT indicates how many files are present.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2006 (TIMJ):
*        Clone from smurf_makemap
*     18-SEP-2006 (DSB):
*        MAKECUBE code added.
*     6-NOV-2006 (DSB):
*        Added parameter DETECTORS.
*     10-NOV-2006 (DSB):
*        Added HISTORY component to output NDF.
*     14-NOV-2006 (DSB):
*        Added AUTOGRID parameter.
*     20-NOV-2006 (DSB):
*        - Make the DETECTORS parameter case insensitive.
*        - Document label column in OUTCAT.
*     21-NOV-2006 (DSB):
*        AUTOGRID now supplies the dynamic defaults for the projection
*        parametersm which are now acquired after AUTOGRID.
*     23-NOV-2006 (DSB):
*        - SYSTEM can now accept any AST celestial System name.
*        - Fix incorrect indices for "pixsize" array when checking pixel
*        sizes.
*     24-NOV-2006 (DSB):
*        Added PARAMS and SPREAD parameters.
*     28-NOV-2006 (DSB):
*        - Propagate Label and Unit components from first input NDF to the
*        output NDF.
*        - Added WEIGHTS parameter.
*        - Added GENVAR parameter.
*     29-NOV-2006 (DSB):
*        We do not need a double size weights array if GENVAR is FALSE.
*     30-NOV-2006 (DSB):
*        Added parameter SPARSE.
*     6-DEC-2006 (DSB):
*        Add detgrp to the smf_cubegrid argument list.
*     13-DEC-2006 (DSB):
*        Allow output variances to be caclulated on the basis of the system 
*        noise temperature values in the input NDFs.
*     21-DEC-2006 (DSB):
*        Set the spatial output FrameSet to represent offsets from first
*        base pointing position if the target is moving.
*     11-JAN-2007 (DSB):
*        Aded parameters LBOUND and UBOUND, and allowed a null value to
*        be supplied for OUT.
*     11-JAN-2007 (TIMJ):
*        Added FLBND and FUBND. Add FTL, FTR, FBL, FBR parameters.
*     12-JAN-2007 (DSB):
*        Add reporting of axis labels.
*     16-JAN-2007 (DSB):
*        Use 2D variance and weights arrays where possible.
*     22-JAN-2007 (DSB):
*        Modified to accomodate changes to argument lists for smf_cubegrid, 
*        smf_cubebounds and smf_rebincube, which provide better handling
*        of moving sources.
*     25-JAN-2007 (DSB):
*        Remove duplicated code for getting parameter "SPARSE".
*     7-FEB-2007 (DSB):
*        Store median exposure time int he output NDF FITS extension.
*     8-FEB-2007 (DSB):
*        - Create a SMURF extension in the output holding arrays EXP_TIME,
*        ON_TIME and TSYS.
*        - Store the median output TSYS value in the output FITS extension.
*        - Find FITS headers that are present and have the same value in all 
*        input NDFs, and add them to the output NDF's FITS extension.
*     12-FEB-2007 (DSB):
*        Added parameter INWEIGHT.
*     21-FEB-2007 (DSB):
*        - Changed ON_TIME to EFF_TIME.
*        - Added EFF_TIME FITS header to output.
*     7-MAR-2007 (BC):
*        - Added input OBSID FITS header tracking through PROVCNT and
*        OBSnnnnn output FITS headers.
*     16-MAR-2007 (DSB):
*        Extend use of INWEIGHT to all spreading schemes.
*     20-MAR-2007 (TIMJ):
*        Factor out output FITS header code.
*     28-MAR-2007 (DSB):
*        - Expand documentation for INWEIGHT, and warn user if the supplied
*        INWEIGHT value cannot be used.
*        - Set the pixel origin of the weights NDF to be the same as the
*        pixel origin of the main output NDF.
*        - Erase the output NDF variance array if less than 10% of the
*        good output data values have good output variances.
*     14-APR-2007 (DSB):
*        Warn user about rejected input spectra.
*     24-APR-2007 (DSB):
*        Add parameter BADMASK.
*     2-MAY-2007 (DSB):
*        Modify the message about rejected spectra to indicate how many
*        input spectra there were in total.
*     8-MAY-2007 (DSB):
*        Change default BADMASK to "OR".
*     2007-06-22 (TIMJ):
*        Rework to handle PRV* as well as OBS*

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006 University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/atl.h"
#include "star/kaplibs.h"


/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smurf_makecube"
#define TASK_NAME "MAKECUBE"
#define LEN__METHOD 20

void smurf_makecube( int *status ) {

/* Local Variables */
   AstFitsChan *fchan = NULL; /* FitsChan holding output NDF FITS extension */
   AstFrame *ospecfrm = NULL; /* SpecFrame from the output WCS Frameset */
   AstFrame *tfrm = NULL;     /* Current Frame from output WCS */
   AstFrameSet *wcsout = NULL;/* WCS Frameset for output cube */
   AstFrameSet *wcsout2d = NULL;/* WCS Frameset describing 2D spatial axes */
   AstKeyMap *prvkeymap = NULL;  /* KeyMap to hold unique PRVnnnn headers */
   AstKeyMap *obskeymap = NULL;  /* KeyMap to hold unique OBSnnnn headers */
   AstMapping *oskymap = NULL;/* GRID->SkyFrame Mapping from output WCS */
   AstMapping *ospecmap = NULL;/* GRID->SpecFrame Mapping from output WCS */
   AstMapping *tmap = NULL;   /* Base->current Mapping from output WCS */
   AstSkyFrame *abskyfrm = NULL;/* Output SkyFrame (always absolute) */
   AstSkyFrame *oskyfrm = NULL;/* SkyFrame from the output WCS Frameset */
   Grp *detgrp = NULL;        /* Group of detector names */
   Grp *igrp = NULL;          /* Group of input files */
   Grp *ogrp = NULL;          /* Group containing output file */
   HDSLoc *smurf_xloc = NULL; /* HDS locator for output SMURF extension */
   HDSLoc *weightsloc = NULL; /* HDS locator of weights array */
   char *pname = NULL;        /* Name of currently opened data file */
   char pabuf[ 10 ];          /* Text buffer for parameter value */
   char system[ 10 ];         /* Celestial coord system for output cube */
   char tmpstr[10];           /* temporary unit string */
   double corner[2];          /* WCS of a corner (SKY) */
   double fcon;               /* Tsys factor for file */
   double glbnd_out[ 3 ];     /* double prec Lower GRID bounds for output map */
   double gubnd_out[ 3 ];     /* double prec Upper GRID bounds for output map */
   double gx_in[ 4 ];         /* X Grid coordinates of four corners */
   double gx_out[ 4 ];        /* X WCS coordinates of four corners */
   double gy_in[ 4 ];         /* Y Grid coordinates of four corners */
   double gy_out[ 4 ];        /* Y WCS coordinates of four corners */
   double par[ 7 ];           /* Projection parameter */
   double params[ 4 ];        /* astRebinSeq parameters */
   double wcslbnd_out[3];     /* Array of lower bounds of output cube */
   double wcsubnd_out[3];     /* Array of upper bounds of output cube */
   float *eff_array = NULL;   /* Pointer to array of eff times  */
   float *exp_array = NULL;   /* Pointer to array of exp times */
   float *ipd = NULL;         /* Pointer to the next output data value */
   float *ipt = NULL;         /* Pointer to the next Tsys value */
   float *ipw = NULL;         /* Pointer to the next work value */
   float *ipv = NULL;         /* Pointer to the next output variance value */
   float *tsys_array = NULL;  /* Pointer to array of tsys values */
   float *var_array = NULL;   /* Pointer to temporary variance array */
   float *var_out = NULL;     /* Pointer to the output variance array */
   float *work2_array = NULL; /* Pointer to temporary work array */
   float medeff;              /* Median effective integration time in output NDF. */
   float medexp;              /* Median exposure time in output NDF. */
   float medtsys;             /* Median system temperature in output NDF. */
   float teff;                /* Effective integration time */
   float var;                 /* Variance value */
   int autogrid;              /* Determine projection parameters automatically? */
   int axes[ 2 ];             /* Indices of selected axes */
   int blank;                 /* Was a blank line just output? */
   int el0;                   /* Index of 2D array element */
   int el;                    /* Index of 3D array element */
   int flag;                  /* Is group expression to be continued? */
   int genvar;                /* How to create output Variances */
   int hastsys;               /* Have some good Tsys values been found? */
   int hasoffexp;             /* Any ACS_OFFEXPOSURE values found in the i/p? */
   int i;                     /* Loop index */
   int ifile;                 /* Input file index */
   int badmask;               /* How is the output bad pixel mask chosen? */
   int is2d;                  /* Is the weights array 2-dimensional? */
   int ispec;                 /* Index of next spectrum within output NDF */
   int lbnd_out[ 3 ];         /* Lower pixel bounds for output map */
   int lbnd_wgt[ 4 ];         /* Lower pixel bounds for wight array */
   int moving;                /* Is the telescope base position changing? */
   int nbad;                  /* No. of o/p pixels with good data but bad variance */
   int ndet;                  /* Number of detectors supplied for "DETECTORS" */
   int nel;                   /* Number of elements in 3D array */
   int neluse;                /* Number of elements used */
   int ngood;                 /* No. of o/p pixels with good data */
   int nparam = 0;            /* No. of parameters required for spreading scheme */
   int npos;                  /* Number of samples included in output NDF */
   int nwgtdim;               /* No. of axes in the weights array */
   int naccept;               /* Number of accepted input spectra */
   int nreject;               /* Number of rejected input spectra */
   int nused;                 /* No. of input samples pasted into output cube */
   int nxy;                   /* Number of elements in 2D array */
   int ondf;                  /* output NDF identifier */
   int outax[ 2 ];            /* Indices of corresponding output axes */
   int outsize;               /* Number of files in output group */
   int savewgt;               /* Should weights be saved in the output NDF? */
   int size;                  /* Number of files in input group */
   int smfflags;              /* Flags for smfData */
   int sparse;                /* Create a sparse output array? */
   int spread = 0;            /* Pixel spreading method */
   int trim;                  /* Trim the output cube to exclude bad pixels? */
   int ubnd_out[ 3 ];         /* Upper pixel bounds for output map */
   int ubnd_wgt[ 4 ];         /* Upper pixel bounds for wight array */
   int use_wgt;               /* Use input variance to weight input data? */
   int usedetpos;             /* Should the detpos array be used? */
   int wgtsize;               /* No. of elements in the weights array */
   smfData *data = NULL;      /* Pointer to data struct */
   smfData *effdata = NULL;   /* Pointer to o/p struct holding eff time array */
   smfData *expdata = NULL;   /* Pointer to o/p struct holding exp time array */
   smfData *odata = NULL;     /* Pointer to o/p struct holding data array */
   smfData *tsysdata = NULL;  /* Pointer to o/p struct holding tsys array */
   smfData *wdata = NULL;     /* Pointer to o/p struct holding weights array */
   smfFile *file = NULL;      /* Pointer to data file struct */
   void *data_array = NULL;   /* Pointer to the rebinned map data */
   void *wgt_array = NULL;    /* Pointer to the weights map */

#if defined(FPTRAP)
   feenableexcept(FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW);
#endif

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* We have not yet displayed a blank line on stdout. */
   blank = 0;

/* Begin an NDF context (we do not begin an AST context since this is
   done within the calling monolith routine). */
   ndfBegin();

/* Get a group of input files */ 
   ndgAssoc( "IN", 1, &igrp, &size, &flag, status );

/* Get the celestial coordinate system for the output cube. */
   parChoic( "SYSTEM", "TRACKING", "TRACKING,FK5,ICRS,AZEL,GALACTIC,"
             "GAPPT,FK4,FK4-NO-E,ECLIPTIC", 1, system, 10, status );

/* See of the detector positions are to be read from the RECEPPOS array. 
   Otherwise, they are calculated on the basis of the FPLANEX/Y arrays. */
   parGet0l( "USEDETPOS", &usedetpos, status );

/* Get the detectors to use. If a null value is supplied, annul the
   error. Otherwise, make the group case insensitive. */
   detgrp = NULL;
   if( *status == SAI__OK ) {
      kpg1Gtgrp( "DETECTORS", &detgrp, &ndet, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
	 if (detgrp) {
	   grpDelet( &detgrp, status );
	 }
      } else {
         grpSetcs( detgrp, 0, status );
      }
   }

/* Indicate we have no projection parameters as yet. */
   par[ 0 ] = AST__BAD;
   par[ 1 ] = AST__BAD;
   par[ 2 ] = AST__BAD;
   par[ 3 ] = AST__BAD;
   par[ 4 ] = AST__BAD;
   par[ 5 ] = AST__BAD;
   par[ 6 ] = AST__BAD;

/* See if any unspecified projection parameters are to be determined using
   an optimal fitting process. */
   parGet0l( "AUTOGRID", &autogrid, status );

/* See if the output grp should be trimmed to exclude missing data (e.g.
   caused by detectors not being selected for inclusion via parameter
   DETECTORS). */
   parGet0l( "TRIM", &trim, status );

/* Calculate the default grid parameters. */
   smf_cubegrid( igrp,  size, system, usedetpos, autogrid, detgrp, 
                 par, &moving, &oskyfrm, &sparse, &hastsys, status );

/* If we are producing an output cube with the XY plane being a spatial
   projection... */
   if( !sparse && *status == SAI__OK ) {

/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. */
      smf_cubebounds( igrp, size, oskyfrm, autogrid, usedetpos, par, 
                      ( trim ? detgrp : NULL ), moving, lbnd_out, ubnd_out,
                       &wcsout, &npos, &hasoffexp, status );

/* See if the input data should be weighted according to the reciprocal
   of the input variances. This required ACS_OFFEXPOSURE values in the
   input JCMTSTATE, so warn the user if this cannot be done and continue
   without using weights. */
      parGet0l( "INWEIGHT", &use_wgt, status );
      if( use_wgt && ( !hasoffexp || !hastsys ) ) {
         if( !blank) msgBlank( status );
         if( !hasoffexp ) {
            msgOutif( MSG__NORM, "INW_MSG1A", "   ACS_OFFEXPOSURE not found "
                      "in JCMTSTATE extension.", status );
         } else {
            msgOutif( MSG__NORM, "INW_MSG1B", "   No good TSYS values found "
                      "in ACSIS extension.", status );
         }
         msgOutif( MSG__NORM, "INW_MSG1C", "   Weights cannot be determined "
                   "for the input spectra.", status );
         msgOutif( MSG__NORM, "INW_MSG2", "   Each output spectrum will be "
                   "an unweighted sum of the input spectra.", status );
         msgBlank( status );
         blank = 1;
         use_wgt = 0;
      }

/* See how the output Variances are to be created. */
      parChoic( "GENVAR", "TSYS", "SPREAD,TSYS,NONE", 1, pabuf, 10, status );

      if( !strcmp( pabuf, "SPREAD" ) ) {
         genvar = 1;
   
      } else if( !strcmp( pabuf, "TSYS" ) ) {
         genvar = 2;
   
      } else {
         genvar = 0;

      }

/* Now deal with sparse output cubes. */
   } else {

/* Validate the input files, create the WCS FrameSet to store in the
   output cube, and get the pixel index bounds of the output cube. */
      smf_sparsebounds( igrp, size, oskyfrm, usedetpos, detgrp, lbnd_out, 
                        ubnd_out, &wcsout, &hasoffexp, status );

/* See how the output Variances are to be created (the "Spread" option is
   not available in sparse mode). */
      parChoic( "GENVAR", "TSYS", "TSYS,NONE", 1, pabuf, 10, status );

      if( !strcmp( pabuf, "TSYS" ) ) {
         genvar = 2;
   
      } else {
         genvar = 0;

      }

   }

/* We need some good TSYS valaues and some ACS_OFFEXPOSURE values to
   create Tsys output variances. Report an error if these are not
   available. */
   if( genvar == 2 && ( !hasoffexp || !hastsys) ) {
      msgOutif( MSG__NORM, "GNV_MSG1", "   Variances cannot be determined "
                "for the input spectra.", status );
      msgOutif( MSG__NORM, "GNV_MSG2", "   The output file will not contain "
                "a Variance array.", status );
      msgBlank( status );
      blank = 1;
      genvar = 0;
   }

/* Get the pixel spreading scheme to use. */
   if( !sparse ) {
      parChoic( "SPREAD", "NEAREST", "NEAREST,LINEAR,SINC,"
                "SINCSINC,SINCCOS,SINCGAUSS,SOMB,SOMBCOS,GAUSS", 
                1, pabuf, 10, status );

      if( !strcmp( pabuf, "NEAREST" ) ) {
         spread = AST__NEAREST;
         nparam = 0;
   
      } else if( !strcmp( pabuf, "LINEAR" ) ) {
         spread = AST__LINEAR;
         nparam = 0;
   
      } else if( !strcmp( pabuf, "SINC" ) ) {      
         spread = AST__SINC;
         nparam = 1;
   
      } else if( !strcmp( pabuf, "SINCSINC" ) ) {      
         spread = AST__SINCSINC;
         nparam = 2;
   
      } else if( !strcmp( pabuf, "SINCCOS" ) ) {      
         spread = AST__SINCCOS;
         nparam = 2;
   
      } else if( !strcmp( pabuf, "SINCGAUSS" ) ) {      
         spread = AST__SINCGAUSS;
         nparam = 2;
   
      } else if( !strcmp( pabuf, "SOMB" ) ) {      
         spread = AST__SOMB;
         nparam = 1;
   
      } else if( !strcmp( pabuf, "SOMBCOS" ) ) {      
         spread = AST__SOMBCOS;
         nparam = 2;
   
      } else if( !strcmp( pabuf, "GAUSS" ) ) {      
         spread = AST__GAUSS;
         nparam = 2;
   
      } else if( *status == SAI__OK ) {
         nparam = 0;
         *status = SAI__ERROR;
         msgSetc( "V", pabuf );
         errRep( "", "Support not available for SPREAD = ^V (programming "
                 "error)", status );
      }

   } else {
      spread = AST__NEAREST;
      nparam = 0;
   }

/* Get an additional parameter vector if required. */
   if( nparam > 0 ) parExacd( "PARAMS", nparam, params, status );

/* See how the bad pixel mask in each output spectrum is to be determined. 
   Also choose whether to use the 2D or the 3D weighting system. The 2D  
   system assumes that all pixels in a given output spectrum have the same 
   weight and variance, and requires much less memory than the 3D system.
   Do not bother asking if we are using a 3d spread function by definition.
*/
   if (spread == AST__NEAREST) {
     parChoic( "BADMASK", "OR", "AND,OR,FIRST", 1, pabuf, 10, status );

     if( !strcmp( pabuf, "AND" ) ) {
       badmask = 2;
       is2d = 0;

     } else if( !strcmp( pabuf, "OR" ) ) {
       badmask = 1;
       is2d = 1;

     } else {
       badmask = 0;
       is2d = 1;

     }
   } else {
     badmask = 2;
     is2d = 0;
   }

/* BADMASK = OR and FIRST can only be used with SPREAD = Nearest. Report an 
   error for any other combination. */
   if( badmask != 2 && spread != AST__NEAREST && *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "", "Incompatible values supplied for parameters BADMASK "
              "and SPREAD.", status );
      errRep( "", "BADMASK values of 'OR' and 'FIRST' can only be used if "
              "SPREAD is 'Nearest'.", status );
   }

/* Output the pixel bounds. */
   parPut1i( "LBOUND", 3, lbnd_out, status );
   parPut1i( "UBOUND", 3, ubnd_out, status );

/* Store the number of pixels per spatial plane in the output cube. */
   nxy = ( ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1 )*
         ( ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1 );

/* Get the base->current Mapping from the output WCS FrameSet, and split it 
   into two Mappings; one (oskymap) that maps the first 2 GRID axes into 
   celestial sky coordinates, and one (ospecmap) that maps the third GRID
   axis into a spectral coordinate. Also extract the SpecFrame and
   SkyFrame from the current Frame. */
   tmap = astGetMapping( wcsout, AST__BASE, AST__CURRENT );
   tfrm = astGetFrame( wcsout, AST__CURRENT );

   axes[ 0 ] = 1;
   axes[ 1 ] = 2;
   astMapSplit( tmap, 2, axes, outax, &oskymap );
   oskyfrm = astPickAxes( tfrm, 2, outax, NULL );

   axes[ 0 ] = 3;
   astMapSplit( tmap, 1, axes, outax, &ospecmap );
   ospecfrm = astPickAxes( tfrm, 1, outax, NULL );

/* Create a FrameSet describing 2D GRID to spatial sky coords. This wil
   be used in the extra 2D images stored in the output SMURF extension. */
   wcsout2d = astFrameSet( astFrame( 2, "Domain=GRID" ), "" );
   astAddFrame( wcsout2d, AST__BASE, oskymap, oskyfrm );

/* Invert the spectral Mapping (for the convenience of smf_rebincube), so that
   it goes from current Frame to output grid axis. */
   astInvert( ospecmap );

/* Calculate and output the WCS bounds (matching NDFTRACE output). The bounds 
   are normalised. Celestial coordinates will use radians. */
   for( i = 0; i < 3; i++ ) {
     glbnd_out[ i ] = 0.5;
     gubnd_out[ i ] = ubnd_out[ i ] - lbnd_out[i] + 1.5;
   }

   for( i = 0; i < 3; i++ ) {
     astMapBox( tmap, glbnd_out, gubnd_out, 1, i+1, &(wcslbnd_out[ i ]), 
                &(wcsubnd_out[ i ]), NULL, NULL );
   }

   astNorm( wcsout, wcslbnd_out );
   astNorm( wcsout, wcsubnd_out );

   parPut1d( "FLBND", 3,  wcslbnd_out, status );
   parPut1d( "FUBND", 3,  wcsubnd_out, status );

   msgOutif( MSG__NORM, "WCS_WBND1",
	     "   Output cube WCS bounds:", status );

   for( i = 0; i < 3 && *status == SAI__OK; i++ ) {
     msgSetc( "L", astFormat( wcsout, i+1, wcslbnd_out[i]));
     msgSetc( "U", astFormat( wcsout, i+1, wcsubnd_out[i]));

     if( i == 2 ) {
       sprintf( tmpstr, "unit(%d)", i+1 );
       msgSetc( "UNT", astGetC( wcsout, tmpstr ));
     } else {
       msgSetc( "UNT", "" );
     }

     sprintf( tmpstr, "label(%d)", i + 1 );
     msgSetc( "LAB", astGetC( wcsout, tmpstr ) );

     msgOutif( MSG__NORM, "WCS_WBND2",
	       "        ^LAB: ^L -> ^U ^UNT", status );
   }
   msgBlank( status );
   blank = 1;

/* Now also calculate the spatial coordinates of the four corners (required
   for CADC science archive. First, calculate input GRID coordinates for 4 
   corners: TR, TL, BR, BL. Use pixel centres for reporting. This is 
   important for cases where the pixels are very large and we want to make 
   sure that we are conservative with the database reporting. */

   gx_in[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1.0; /* Right */
   gx_in[ 1 ] = 1.0;                                 /* Left */
   gx_in[ 2 ] = gx_in[ 0 ];                          /* Right */
   gx_in[ 3 ] = gx_in[ 1 ];                          /* Left */
   gy_in[ 0 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1.0; /* Top */
   gy_in[ 1 ] = gy_in[ 0 ];                          /* Top */
   gy_in[ 2 ] = 1.0;                                 /* Bottom */
   gy_in[ 3 ] = gy_in[ 2 ];                          /* Bottom */

   astTran2( oskymap, 4, gx_in, gy_in, 1, gx_out, gy_out );
   
/* Horrible code duplication */
   corner[ 0 ] = gx_out[ 0 ];
   corner[ 1 ] = gy_out[ 0 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FTR", 2, corner, status );

   corner[ 0 ] = gx_out[ 1 ];
   corner[ 1 ] = gy_out[ 1 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FTL", 2, corner, status );

   corner[ 0 ] = gx_out[ 2 ];
   corner[ 1 ] = gy_out[ 2 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FBR", 2, corner, status );

   corner[ 0 ] = gx_out[ 3 ];
   corner[ 1 ] = gy_out[ 3 ];
   astNorm( oskyfrm, corner );
   parPut1d( "FBL", 2, corner, status );

/* Create the output NDF. Abort without error if a null value is supplied. */
   ondf = NDF__NOID;
   if( *status == SAI__OK ) {
      ndgCreat ( "OUT", NULL, &ogrp, &outsize, &flag, status );

      if( *status == PAR__NULL ) {
         errAnnul( status );
         goto L998;
      }
   }

   smfflags = 0;
   if( genvar && !is2d ) smfflags |= SMF__MAP_VAR;
   smf_open_newfile( ogrp, 1, SMF__FLOAT, 3, lbnd_out, ubnd_out, smfflags, 
                     &odata, status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Save some useful pointers. */
   file = odata->file;
   ondf = file->ndfid;

/* Create a history component in the output NDF. */
   ndfHcre( ondf, status );

/* Copy the Label and Unit strings from the first input NDF, and check
   that all input NDFs have the same Label and Unit strings. */
   smf_labelunit( igrp, size, odata, status );

/* Get a pointer to the mapped output data array. */
   data_array = (odata->pntr)[ 0 ];

/* If a 3D weights array is being used, the variance will be evaluated for 
   each individual pixel in the output cube. In this case we will have 
   mapped the Variance component in the output cube, so store a pointer to 
   it. */
   if( !is2d ) {
      var_array = (odata->pntr)[ 1 ];

/* Otherwise, the variance is assumed to be the same in every spatial
   slice, so we only need memory to hold one spatial slice (this slice is
   later copied to all slices in the output cube Variance component).
   Also allocate some work arrays of the same size. */
   } else if( genvar ) {
      var_array = (float *) astMalloc( nxy*sizeof( float ) );
   }

/* If we are producing a regularly gridded output NDF, we need to
   allocate a work array. */
   if( !sparse ) {   

/* Assume for the moment that the weights array is 2-dimensional (i.e. a
   single spatial plane of the output). Store its bounds and calculate its 
   total size in pixels. */
      nwgtdim = 2;
      lbnd_wgt[ 0 ] = lbnd_out[ 0 ];
      lbnd_wgt[ 1 ] = lbnd_out[ 1 ];
      ubnd_wgt[ 0 ] = ubnd_out[ 0 ];
      ubnd_wgt[ 1 ] = ubnd_out[ 1 ];
      wgtsize = ubnd_wgt[ 0 ] - lbnd_wgt[ 0 ] + 1;
      wgtsize *= ubnd_wgt[ 1 ] - lbnd_wgt[ 1 ] + 1;

/* If the weights array is in fact 3D, increase its total size and
   increment the number of axes in the weights array. */
      if( !is2d ) {
         nwgtdim = 3;
         lbnd_wgt[ 2 ] = lbnd_out[ 2 ];
         ubnd_wgt[ 2 ] = ubnd_out[ 2 ];
         wgtsize *= ubnd_wgt[ 2 ] - lbnd_wgt[ 2 ] + 1;
      }

/* If output variances are being created from the spread of input values,
   the weights array needs to be twice the size determined above.
   Implement this as an extra trailing axis with bounds [1:2]. */
      if( genvar == 1 ) {
         lbnd_wgt[ nwgtdim ] = 1;
         ubnd_wgt[ nwgtdim ] = 2;
         nwgtdim++;
         wgtsize *= 2;
      }

/* See if weights are to be saved in the output NDF. */
      parGet0l( "WEIGHTS", &savewgt, status );

/* Create the NDF extension, or allocate the work space, as required. */
      if( savewgt ) {
         weightsloc = smf_get_xloc ( odata, "ACSISRED", "WT_ARR", "WRITE", 
                                     0, 0, status );
         smf_open_ndfname ( weightsloc, "WRITE", NULL, "WEIGHTS", "NEW", 
                            "_DOUBLE", nwgtdim, (int *) lbnd_wgt, 
                            (int *) ubnd_wgt, &wdata, status );
         if( wdata ) wgt_array = (wdata->pntr)[ 0 ];
   
      } else {
         wgt_array = astMalloc( sizeof( double )*(size_t)wgtsize );
      }
   }

/* Create a SMURF extension in the output NDF and create three 2D NDFs in 
   the extension; one for the total exposure time ("on+off"), one for the 
   "on" time, and one for the Tsys values. Each of these 2D NDFs inherits 
   the spatial bounds of the main output NDF. Note, the Tsys array also 
   needs variances to be calculated. Include spatial WCS in each NDF. */
   smurf_xloc = smf_get_xloc ( odata, "SMURF", "SMURF", "WRITE", 
                               0, 0, status );

   smf_open_ndfname ( smurf_xloc, "WRITE", NULL, "EXP_TIME", "NEW", 
                      "_REAL", 2, (int *) lbnd_out, 
                      (int *) ubnd_out, &expdata, status );
   if( expdata ) {
      exp_array = (expdata->pntr)[ 0 ];
      ndfPtwcs( wcsout2d, expdata->file->ndfid, status );
   }

   smf_open_ndfname ( smurf_xloc, "WRITE", NULL, "EFF_TIME", "NEW", 
                      "_REAL", 2, (int *) lbnd_out, 
                      (int *) ubnd_out, &effdata, status );
   if( effdata ) {
      eff_array = (effdata->pntr)[ 0 ];
      ndfPtwcs( wcsout2d, effdata->file->ndfid, status );
   }

   if( genvar ) {
      smf_open_ndfname ( smurf_xloc, "WRITE", NULL, "TSYS", "NEW", 
                         "_REAL", 2, (int *) lbnd_out, 
                         (int *) ubnd_out, &tsysdata, status );
      if( tsysdata ) {
         tsys_array = (tsysdata->pntr)[ 0 ];
         ndfPtwcs( wcsout2d, tsysdata->file->ndfid, status );
      }

   }

/* Invert the output sky mapping so that it goes from sky to pixel
   coords. */
   astInvert( oskymap );

/* Create a copy of "oskyfrm" representing absolute coords rather than 
   offsets. */
   abskyfrm = astCopy( oskyfrm );
   astClear( abskyfrm, "SkyRefIs" );

/* Create provenance keymap */
   prvkeymap = astKeyMap( "" );

/* Loop round all the input files, pasting each one into the output NDF. */
   naccept = 0;
   nreject = 0;
   nused = 0;
   ispec = 0;
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( igrp, ifile, "READ", 1, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         errRep( FUNC_NAME, "Could not open input data file.", status );
         break;

      } else {
         if( data->file == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfFile associated with smfData.", 
                    status );
            break;

         } else if( data->hdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfHead associated with smfData.", 
                    status );
            break;

         } 
      }

/* Report the name of the input file. */
      pname =  data->file->name;
      msgSetc( "FILE", pname );
      msgSeti( "THISFILE", ifile );
      msgSeti( "NUMFILES", size );
      msgOutif( MSG__VERB, " ", 
                "SMURF_MAKECUBE: Processing ^THISFILE/^NUMFILES ^FILE",
                status );

/* Store the filename in the keymap for later - the GRP would be fine
   as is but we use a keymap in order to reuse smf_fits_add_prov */
      astMapPut0I( prvkeymap, pname, 1, NULL );

/* Check that the input data type is single precision. */
      if( data->dtype != SMF__FLOAT ) {
         if( *status == SAI__OK ) {
            msgSetc( "FILE", pname );
            msgSetc( "DTYPE", smf_dtype_string( data, status ) );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "^FILE has ^DTYPE data type, should "
                    "be REAL.",  status );
         }
         break;
      }     

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than detpos, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && data->hdr->detpos ) {
         smf_free( (double *) data->hdr->detpos, status );      
         data->hdr->detpos = NULL;
      }

/* Handle output FITS header creation/manipulation */
      smf_fits_outhdr( data->hdr->fitshdr, &fchan, &obskeymap, status );

/* Rebin the data into the output grid. */
      if( !sparse ) {
         smf_rebincube( data, ifile, size, badmask, is2d, abskyfrm, oskymap, 
                        ospecfrm, ospecmap, detgrp, moving, use_wgt, lbnd_out, 
                        ubnd_out, spread, params, genvar, data_array, 
                        var_array, wgt_array, exp_array, eff_array, &fcon, 
                        &nused, &nreject, &naccept, status );

      } else {
         smf_rebinsparse( data, ifile, ospecfrm, ospecmap, abskyfrm, detgrp, 
                          lbnd_out, ubnd_out, genvar, data_array, var_array, 
                          &ispec, exp_array, eff_array, &fcon,
                          status );
      }
   
      blank = 0;

/* Close the input data file. */
      if( data != NULL ) {
	smf_close_file( &data, status );
	data = NULL;
      }
   }

/* Tell the user how many input spectra were rejected. */
   if( nreject > 0 ) {
      if( !blank ) msgBlank( status );
      msgSeti( "N", nreject );
      msgSeti( "T", naccept + nreject );
      msgOutif( MSG__NORM, " ", "WARNING: ^N out of the ^T input spectra "
                "were ignored because they included unexpected bad pixel "
                "values.", status );
      msgBlank( status );
      blank = 1;
   }

/* Arrive here if an error occurs. */
L999:;

/* Close the input data file that remains open due to an early exit from
   the above loop. */
   if( data != NULL ) {
      smf_close_file( &data, status );
      data = NULL;
   }

/* Store the WCS FrameSet in the output NDF (if any). */
   if( wcsout ) ndfPtwcs( wcsout, ondf, status );

/* If we are creating an output Variance component... */
   if( genvar && *status == SAI__OK) {

/* Count the number of pixel which have a good data value but a bad
   variance value, and count the number which have a good data value. 
   If the weights array is 2-dimensional, cycle through the 2D variance 
   array as we move through the entire 3D output data array. */
      ngood = 0;
      nbad = 0;
      ipd = (float *) data_array;
      ipv = (float *) var_array;
      nel = nxy*( ubnd_out[ 2 ] - lbnd_out[ 2 ] + 1 );

      if( !is2d ) {

         for( el = 0; el < nel; el++, ipd++,ipv++ ) {
            if( *ipd != VAL__BADR ) {
               ngood++;
               if( *ipv == VAL__BADR ) nbad++;
            }
         }

      } else {

         for( el = 0; el < nel; el++, ipd++,ipv++ ) {
            if( el % nxy == 0 ) ipv = (float *) var_array;
            if( *ipd != VAL__BADR ) {
               ngood++;
               if( *ipv == VAL__BADR ) nbad++;
            }
         }
      }

/* If more than 50% of the good data values have bad variance values,
   we will erase the variance component. */
      if( nbad > 0.5*ngood ) {
         if( !blank ) msgBlank( status );
         msgOutif( MSG__NORM, " ", "WARNING: More than 50% of the good "
                   "output data values have bad variances. The output "
                   "NDF will not contain a Variance array.", status );
         msgBlank( status );
         blank = 1;

         if( !is2d ) {
            ndfUnmap( ondf, "Variance", status );
            ndfReset( ondf, "Variance", status );
            (odata->pntr)[ 1 ] = NULL;
            var_array = NULL;

         } else {
            var_array = (float *) astFree( var_array );
         }

         genvar = 0;
  
/* Otherwise, if the output variances are the same for every spatial slice, the
   "var_array" used above will be a 2D array holding a single slice of the 3D
   Variance array. In this case we now copy this slice to the output cube. */
      } else if( is2d ) {
         ndfMap( ondf, "Variance", "_REAL", "WRITE", (void **) &var_out, &nel, 
                 status );
         if( var_out && *status == SAI__OK ) {
            ipd = (float *) data_array;
            ipv = (float *) var_out;
            el0 = 0;
            for( el = 0; el < nel; el++, el0++, ipd++, ipv++ ) {
               if( el0 == nxy ) el0 = 0;
               if( *ipd != VAL__BADR ) {
                  *ipv = var_array[ el0 ];
               } else {
                  *ipv = VAL__BADR;
               }
            }
         }

/* If all the input files had the same backend degradation factor and
   channel width, calculate a 2D array of Tsys values for the output
   cube. */
         if( fcon != VAL__BADD ) {
            for( el0 = 0; el0 < nxy; el0++ ) {
               teff = eff_array[ el0 ];
               var = var_array[ el0 ];
               if( teff != VAL__BADR && teff > 0.0 && 
                   var != VAL__BADR && var > 0.0 ) {
                  tsys_array[ el0 ] = sqrt( 0.25*var*teff/fcon );
               } else {
                  tsys_array[ el0 ] = VAL__BADR;
               }
            }
   
         } else {
            for( el0 = 0; el0 < nxy; el0++ ) {
               tsys_array[ el0 ] = VAL__BADR;
            }
         }

/* Free the memory used to store the 2D variance information. */
         var_array = astFree( var_array );

/* For 3D variances, the output Tsys values are based on the mean
   variance in every output spectrum. */
      } else if( fcon != VAL__BADD ) {

         work2_array = astMalloc( nxy*sizeof( float ) );
         if( work2_array ) {
            ipw = work2_array;
            ipt = tsys_array;
            for( el = 0; el < nxy; el++ ) {
               *(ipw++) = 0.0;
               *(ipt++) = 0.0;
            }
   
            ipv = var_array;
            ipt = tsys_array;
            ipw = work2_array;
   
            for( el = 0; el < nel; el++, ipv++, ipt++, ipw++ ) {
               if( el % nxy == 0 ) {
                  ipt = tsys_array;
                  ipw = work2_array;
               }
   
               if( *ipv != VAL__BADR ) {
                  *(ipw) += 1.0;
                  *(ipt) += *ipv;
               }
            }
   
            for( el0 = 0; el0 < nxy; el0++ ) {
               teff = eff_array[ el0 ];
               var = tsys_array[ el0 ];
               if( teff != VAL__BADR && teff > 0.0 &&
                   work2_array[ el0 ] > 0.0 && var > 0.0 ) {
                  var /= work2_array[ el0 ];
                  tsys_array[ el0 ] = sqrt( 0.25*var*teff/fcon );
               } else {
                  tsys_array[ el0 ] = VAL__BADR;
               }
            }
         }

/* For 3D weights and no Variance->Tsys conversion factor, fill the Tsys
   array with bad values. */
      } else {

         if( !blank ) msgBlank( status );
         msgOutif( MSG__NORM, " ", "WARNING: Cannot create output Tsys "
                   "values.", status );
         msgBlank( status );
         blank = 1;

         for( el0 = 0; el0 < nxy; el0++ ) {
            tsys_array[ el0 ] = VAL__BADR;
         }

      }
   }

/* If we created an output Variance component, store the median system 
   temperature as keyword TSYS in the FitsChan. */
   if( genvar ) {
      work2_array = astStore( work2_array, tsys_array, nxy*sizeof( float ) );
      kpg1Medur( 1, nxy, work2_array, &medtsys, &neluse, status );
      atlPtftr( fchan, "MEDTSYS", medtsys, 
                "[K] Median MAKECUBE system temperature", status );

   } else {
      if( !blank ) msgBlank( status );
      msgOutif( MSG__NORM, " ", "WARNING: Cannot create output Tsys "
                "values since no output variances have been created.",
                status );
      msgBlank( status );
      blank = 1;
   }

/* Store the median exposure time as keyword EXP_TIME in the FitsChan.
   Since kpg1Medur partially sorts the array, we need to take a copy of it
   first. */
   work2_array = astStore( work2_array, exp_array, nxy*sizeof( float ) );
   kpg1Medur( 1, nxy, work2_array, &medexp, &neluse, status );
   atlPtftr( fchan, "EXP_TIME", medexp, 
             "[s] Median MAKECUBE exposure time", status );

/* Store the median effective integration time as keyword EFF_TIME in the 
   FitsChan. Since kpg1Medur partially sorts the array, we need to take a 
   copy of it first. */
   work2_array = astStore( work2_array, eff_array, nxy*sizeof( float ) );
   kpg1Medur( 1, nxy, work2_array, &medeff, &neluse, status );
   atlPtftr( fchan, "EFF_TIME", medeff, 
             "[s] Median MAKECUBE effective integration time", status );

/* Retrieve the unique OBSID keys from the KeyMap and populate the OBSnnnnn
   and OBSCNT headers from this information. */
   smf_fits_add_prov( fchan, "OBS", obskeymap, status ); 
   smf_fits_add_prov( fchan, "PRV", prvkeymap, status ); 

/* Free the second work array. */
   work2_array = astFree( work2_array );

/* If the FitsChan is not empty, store it in the FITS extension of the
   output NDF (any existing FITS extension is deleted). */
   if( fchan &&  astGetI( fchan, "NCard" ) > 0 ) kpgPtfts( ondf, fchan, status );
  
/* Close the output data files. */
   if( expdata ) smf_close_file( &expdata, status );
   if( effdata ) smf_close_file( &effdata, status );
   if( tsysdata ) smf_close_file( &tsysdata, status );
   if( wdata ) smf_close_file( &wdata, status );
   if( odata ) smf_close_file( &odata, status );

/* Arrive here if no output NDF is being created. */
L998:;

/* Free resources. */  
   if (obskeymap ) astAnnul( obskeymap );
   if (prvkeymap ) astAnnul( prvkeymap );
   if( detgrp != NULL) grpDelet( &detgrp, status);
   if( igrp != NULL) grpDelet( &igrp, status);
   if( ogrp != NULL) grpDelet( &ogrp, status);
   if( wgt_array && !savewgt ) wgt_array = astFree( wgt_array );

/* End the NDF context. */
   ndfEnd( status );

/* Issue a status indication.*/  
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, cube written.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
