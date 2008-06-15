      SUBROUTINE SURF_REBIN (TSKNAME, STATUS)
*+
*  Name:
*     (BOL|INT)REBIN

*  Purpose:
*     Rebin demodulated SCUBA data onto output map

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_REBIN (TSKNAME, STATUS)

*  Arguments:
*     TSKNAME = CHARACTER * () (Given)
*        Name of task so that I can distinguish REBIN, BOLREBIN and INTREBIN,
*        DESPIKE and EXTRACT_DATA
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine rebins the demodulated data from SCUBA MAP observations
*     onto a rectangular mesh by a variety of methods.
*     Currently convolution by weighting functions and splines are 
*     supported.
*     - Weighting functions:
*       Currently linear and bessel weighting functions are supported.
*       The width of the Bessel function is such that it should preserve all
*       spatial information obtained by the telescope at the wavelength of
*       observation, but suppress higher spatial frequencies. To minimise edge
*       effects the Bessel function is truncated at a radius of 10 half-widths
*       from the centre, and apodized over its outer third by a cosine 
*       function. Viewed in frequency space the method consists of Fourier 
*       transforming the input dataset(s), multiplying the transform by a 
*       cylindrical top-hat (the F.T. of the Bessel function), 
*       then transforming back into image space.
*       A linear weighting function is also available which works out
*       to one half-width - this has the advantage that it is much faster to
*       process and is much less susceptible to edge effects. 
*
*     - Splines:
*       Additionally, spline interpolation and smoothing routines are also
*       available. Note that the spline routines work on each integration
*       in turn, whereas the weighting function routines work on all the input
*       data in one go. At present the spline routines are experimental and
*       comments are welcomed.

*     If this task is invoked as BOLREBIN then a separate map will be made
*     of each bolometer. The output file will contain an NDF for each 
*     bolometer.

*     If this task is invoked as INTREBIN then a separate map will be 
*     made of each integration. The output file will contain an NDF for each 
*     bolometer.


*  Usage:
*     rebin

*  ADAM parameters:
*     DEFOUT = LOGICAL (Read) - Used by DESPIKE only
*        Use default output names (if TRUE). Default is to ask for
*        output file names.
*     GUARD = LOGICAL (Read)
*        Determines whether a guard ring of bolometers should be used
*        during the weight function regridding. A guard ring simulates
*        a ring of bolometers at the edge of the supplied data containing
*        a value of zero volts. Default is to use a guard ring. It should be
*        turned off if the edge of the observed dataset does not have zero
*        flux. Currently this should also be turned off if the regridding
*        parameters are modified (eg if WTFNRAD or SCALE are changed from
*        their default values).
*     IN = CHAR (Read)
*        The name of the input file to be rebinned. This parameter is requested
*        repeatedly until a NULL value (!) is supplied. LOOP must be TRUE.
*        IN can include a SCUBA section.
*        Like the REF parameter this parameter accepts a text file.
*     LAT_OUT = CHAR (Read)
*        The latitude of the output map centre. The supplied default value
*        is that of the map centre of the first map.
*     LONG_OUT = CHAR (Read)
*        The longitude of the output map centre. The supplied default value 
*        is that of the map centre of the first map.
*     LOOP = LOGICAL (Read)
*        Task will ask for multiple input files if true. Only REF is read
*        if noloop.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Write)
*        For REBIN this is the name of the NDF that will contain the rebinned 
*        map. For BOLREBIN or INTREBIN this is the name of the HDS container 
*        file. In REBIN, if a null response is given, the task will 
*        immediately exit with good status -- this can be used to
*        determine the size of the output map without actually regridding it.
*     OUT_COORDS = CHAR (Read)
*        The coordinate system of the output map. Available coordinate
*        systems are:
*        - AZ:  Azimuth/elevation offsets 
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*
*        For RD current epoch is taken from the first input file.
*     OUT_OBJECT = CHAR (Read)
*        The name of the object (ie the NDF title).
*     PIXSIZE_OUT = REAL (Read)
*        Size of pixels in the output map. Units are arcsec.
*     REBIN_METHOD = CHAR (Read)
*        The rebin method to be used. A number of regridding methods are
*        available:
*        - LINEAR:  Linear weighting function
*        - BESSEL:  Bessel weighting function
*        - GAUSSIAN:Gaussian weighting function
*        - SPLINE1: Interpolating spline (PDA_IDBVIP)
*        - SPLINE2: Smoothing spline (PDA_SURFIT)
*        - SPLINE3: Interpolating spline (PDA_IDSFFT)
*
*        Please refer to the PDA documentation (SUN/194) for more information
*        on the spline fitting algorithms.
*     REF = CHAR (Read)
*        The name of the first NDF to be rebinned. The name may also be the
*        name of an ASCII text file containing NDF and parameter values.
*        See the notes. REF can include a SCUBA section.
*     REFPIX( 2 ) = INTEGER (Read)
*        The coordinate of the reference pixel in the output data
*        array. This corresponds to the pixel associated with the
*        specified RA/Dec centre. Default is to use the middle pixel
*        if a size is specified or the optimal pixel if the default
*        size is used (see the SIZE parameter).
*     SCALE = REAL (Read)
*        Radius of one scale size in arcsec. This effectively governs the
*        size of the weighting function. For LINEAR one scale size corresponds
*        to the zero of the cone, for BESSEL it is the first zero of the
*        Bessel function (PI) and for Gaussian it is the half-width
*        half maximum (HWHM).
*     SFACTOR = REAL (Read)
*        This is the smoothing factor to use for a SPLINE2 regrid.
*        See the PDA_SURFIT documentation for more information.
*     SHIFT_DX = REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line. This is a shift in the output coordinte frame.
*     SHIFT_DY = REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line. This is a shift in the output coordinate frame.
*     SIZE( 2 ) = INTEGER (Read)
*        This parameter sets the size of the output grid in pixels. The default
*        values are the minimum dimensions required to display the entirety
*        of the mapped area.
*     TRIM = REAL (Read)
*        This parameter determines the amount of good data that should
*        be trimmed from the final image to correct for edge effects.
*        The supplied value should be in arcseconds. All pixels closer
*        to a bad pixel than this distance will be set to bad in the
*        output image. Default is 0.0.
*     TIMES = LOGICAL (Read)
*        Store an extra NDF in the output map containing the 2-D histogram
*        of the data. This can be used to make an estimate of the actual
*        number of samples responsible for each point in the output grid.
*        Note that, in general, the number of pixels in the output grid
*        exceeds the number of independent beams in the image.
*        The data can be accessed as OUT.more.reds.times. Default is FALSE.
*     WEIGHT = REAL (Read)
*        The relative weight that should be assigned to each dataset.
*     WEIGHTS = LOGICAL (Read)
*        Governs whether the convolution weights array will be stored.
*        The default is false (ie do not store the weights array)
*     WTFNRAD = INTEGER (Read)
*        Size of the weighting function in scale sizes. This parameter
*        is irrelevant for LINEAR regridding. For Gaussian the default
*        is 3 (i.e. a diameter of 3 FWHM for the footprint), and for
*        Bessel it is 10. The smaller the weighting function is (a
*        combination of WTFNRAD and SCALE) the faster the regridding goes.


*  Examples:
*     rebin REBIN_METHOD=LINEAR OUT_COORDS=RJ
*        Rebin the maps with LINEAR weighting function in J2000 RA/Dec 
*        coordinates. You will be asked for input datasets until a null
*        value is given.
*     bolrebin REBIN_METHOD=BESSEL OUT=map
*        Rebin the maps with Bessel weighting function. Each bolometer is 
*        rebinned separately and placed in an NDF in the output container file
*        map.sdf. Bolometer H7 can be accessed by displaying map.h7.
*     intrebin noloop IN=test.bat
*        Rebin the files specified in test.bat but storing each integration
*        in a separate NDF (named I1, I2 etc).

*  ASCII input files:
*     The REF and IN parameters accept ASCII text files as input. These
*     text files may contain comments (signified by a #), NDF names,
*     values for the parameters WEIGHT, SHIFT_DX and SHIFT_DY,
*     and names of other ASCII files. There is one data file per line.
*     An example file is:
*    
*         file1{b5}   1.0   0.5   0.0  # Read bolometer 5 from file1.sdf
*         file2                        # Read file 2 but you will still be
*                                      # prompted fro WEIGHT, and shifts.
*         file3{i3}-  1.0   0.0   0.0  # Use everything except int 3
*         test.bat                     # Read in another text file

*
*     Note that the parameters are position dependent and are not necessary.
*     Missing parameters are requested. This means it is not possible to
*     specify SHIFT_DX (position 3) without specifying the WEIGHT.
*     If the file has the .txt extension the NDF system will attempt to
*     convert it to NDF format before processing -- this is probably not
*     what you want.

*  Notes: 
*     For each file name that is entered, values for the parameters
*     WEIGHT, SHIFT_DX and SHIFT_DY are requested.
*     - The application can read in up to 100 separate input datasets. 
*     - The output map will be large enough to include all data points.
*     - Spline regridding may have problems with SCAN/MAP (since integrations
*     contain lots of overlapping data points).
*     - SCUBA sections can be given along with any input NDF
*     - The relative weights associated with each point in the output map
*     are stored in a WEIGHTS NDF in the REDS extension of the output 
*     data. For spline rebinning each point is equivalent to the number
*     of integrations added into the final data point. For weight function
*     regridding the situation is more complicated. The weights array is
*     not stored if the WEIGHTS parameter is false.

*  Implementation Status:
*     This code deals with REBIN, INTREBIN, BOLREBIN, EXTRACT_DATA and
*     DESPIKE.

*  Related Applications:
*     SURF: SCUQUICK, EXTRACT_DATA


*  Authors:
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1995-2006 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     14-JUN-2008 (TIMJ):
*        Calculate merged output fits header from inputs
*     $Id$
*     16-JUL-1995: Original version.
*     $Log$
*     Revision 1.84  2006/10/27 07:47:07  timj
*     fix OUT_COORDS initialisation broken in v1.78
*
*     Revision 1.83  2006/10/26 22:06:21  timj
*     improve OUT_COORDS error message
*
*     Revision 1.82  2005/03/23 19:45:44  timj
*     initialise ABOL_*_PTR first time through for intrebin
*
*     Revision 1.81  2005/03/23 09:20:11  timj
*     Register the INTREBIN offsetting pointers with CNF before attempting to use them with CNF_PVAL
*
*     Revision 1.80  2005/03/23 08:04:08  timj
*     + DIAMETER is no longer a constant, it is derived from the TELESCOP
*     + Calculate Nyquist value once and use it everywhere
*
*     Revision 1.79  2005/03/23 03:49:28  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     the effective radius of influence for SPLINE and SCALE for weight function.
*
*     Revision 1.78  2005/03/18 06:27:53  timj
*     + initialise some variables
*     + add some status check protection
*
*     Revision 1.77  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.76  2003/04/01 01:10:35  timj
*     Add a comment linking MAX_DIM to SCUBA__MAX_FITS
*
*     Revision 1.75  2002/09/18 22:04:01  timj
*     BOLREBIN had an uninitialised pointer.
*
*     Revision 1.74  2000/05/11 19:57:45  timj
*     Add SFACTOR to documentation header
*
*     Revision 1.73  1999/08/19 21:17:54  timj
*     Remove debug print statements.
*
*     Revision 1.72  1999/08/03 20:01:36  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.71  1999/07/26 20:35:40  timj
*     Check for planet coordinate frame for model.
*
*     Revision 1.70  1999/07/17 02:56:39  timj
*     Further refinement of the sky removal using model.
*
*     Revision 1.69  1999/07/15 20:27:38  timj
*     First stab at improving external model input
*
*     Revision 1.68  1999/07/14 20:13:29  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.67  1999/07/14 19:21:04  timj
*     Pass TELESCOPE and INSTRUMENT down to WRITE_MAP_INFO
*
*     Revision 1.66  1999/07/14 04:50:18  timj
*     Use surf_request_output_coords
*
*     Revision 1.65  1999/06/25 05:59:36  timj
*     Fix REFPIX bounds problem.
*
*     Revision 1.64  1999/06/18 03:20:24  timj
*     Add missing parameters to docs
*
*     Revision 1.63  1999/06/16 21:08:41  timj
*     Add REFPIX and allow OUT=! to be okay
*
*     Revision 1.62  1999/05/15 01:50:38  timj
*     Add TRIM.
*     Change reference pixel.
*     Propogate UNITS.
*
*     Revision 1.61  1999/02/27 04:39:58  timj
*     Add polarimeter support (ANG_MEAS, ANG_INT).
*     Make sure that INTREBIN writes passes WPLATE and ANGROT to header.
*     Close INTREBIN output file.
*
*     Revision 1.60  1998/12/10 19:58:07  timj
*     Rename parameter USEGRD to GUARD
*
*     Revision 1.59  1998/12/08 21:34:39  timj
*     Add USEGRD parameter and make the default value be TRUE
*
*     Revision 1.58  1998/10/02 18:55:31  timj
*     Change MAX_FILE to 256 (was 100)
*
*     Revision 1.57  1998/06/09 21:59:21  timj
*     Ask for smoothing size in calcsky. Calculate smoothing size in pixels
*     and pass to CALCSKY routine.
*
*     Revision 1.56  1998/06/09 20:29:45  timj
*     Add configurable weight function rebin
*
*     Revision 1.55  1998/06/03 23:39:04  timj
*     DESPIKE can now be told to use default output filenames automatically.
*     (Note that MEDIAN rebinning was added in the previous version but
*     left out of the comment)
*
*     Revision 1.54  1998/06/03 22:05:31  timj
*     Add CALCSKY
*
*     Revision 1.53  1998/05/06 18:30:11  timj
*     Increase size of 'IN' parameter.
*
*     Revision 1.52  1998/05/05 20:01:45  timj
*     Use SCUBA__MAX_FITS to specify max size of FITS array when
*     acessing the FITS array.
*
*     Revision 1.51  1998/04/28 20:41:18  timj
*     Deal with FITS information for each input file.
*     Default filename is related to input when only one file is
*     used.
*
*     Revision 1.50  1998/04/28 00:32:07  timj
*     Add Gaussian convolution function
*
*     Revision 1.49  1998/03/24 01:23:31  timj
*     Initialise pointer variables.
*     Fix memory leak for OUT_WEIGHT_PTR.
*
*     Revision 1.48  1998/03/24 00:26:02  timj
*     Add support for BOLWT
*
*     Revision 1.47  1998/03/04 03:14:27  timj
*     Make sure that OUT_WEIGHT_PTR is initialised with zeroes
*
*     Revision 1.46  1998/02/03 01:14:24  timj
*     Make upper limit to map size larger.
*
*     Revision 1.45  1998/01/23 02:03:20  timj
*     Optionally store the WEIGHTS array (use WEIGHTS parameter).
*     Add the TIMES array (optional).
*     Use MALLOC for the output grid instead of NDF_MAP.
*
*     Revision 1.44  1998/01/22 02:06:28  timj
*     Allow the size of the output grid to be modified
*
*     Revision 1.43  1997/11/19 18:53:15  timj
*     Fix some logic problems with errors being annulled by CHR_STATUS
*
*     Revision 1.42  1997/11/06 23:27:22  timj
*     Add the verbose suffix option.
*
*     Revision 1.41  1997/10/28 19:08:38  timj
*     Add support for despiking.
*
*     Revision 1.40  1997/09/03 23:33:37  timj
*     Use SCULIB_GET_FILENAME instead of SUBPAR.
*     Supply a default for the 'OUT' parameter based on object name.
*
*     Revision 1.39  1997/07/31 19:40:24  timj
*     Fix bug with propogating of user specified output title OBJECT vs. SOBJECT
*
*     Revision 1.38  1997/06/28 00:55:10  timj
*     Put the ASCII text file part into its own section of the header.
*
*     Revision 1.37  1997/06/27 23:31:05  timj
*     Change to support '-' negation of sections.
*     Support passing of WAVELENGTH to WRITE_MAP_INFO.
*     Fix bug causing maximum of 15 characters for HDS names.
*
*     Revision 1.36  1997/06/12 23:53:28  timj
*     Change name to SURF (and calls to SURF_)
*     Update documentation
*     Fix I=1,MAX_FILES freeing bug
*
*     Revision 1.35  1997/06/05 22:56:33  timj
*     Reset pointers if good status.
*
*     Revision 1.34  1997/06/05 22:50:41  timj
*     Initialise pointers
*
*     Revision 1.33  1997/05/22 01:12:36  timj
*     Merge with REDS_REBIN.
*     Support weights in EXTRACT_DATA.
*
*     Revision 1.32  1997/05/22 00:20:50  timj
*     Move NDF reading routine into subroutines REDS_RECURSE_READ and
*     REDS_READ_REBIN_NDF.
*
*     Revision 1.31  1997/05/13 01:25:46  timj
*     Add EXTRACT_DATA.
*     Add PL output coords
*     Add SHIFTS to NA/AZ/PL
*
*     Revision 1.30  1997/04/09 20:45:13  timj
*     Add INTREBIN
*     Add extra security to new call to NDF_OPEN (include DUMMY parameter)
*
*     Revision 1.29  1997/04/08 22:06:13  timj
*     Use SCUBA sections instead of specifying INTS with parameters.
*     Remove printing of integration count.
*
*     Revision 1.28  1997/04/07 22:59:55  timj
*     Use SCULIB_WTFN_REGRID
*     Start experimenting with SCULIB_SPLINE_REGRID
*
*     Revision 1.27  1997/04/04 00:46:40  timj
*     Replace REDS_WTFN_REBIN with ^TASK token.
*
*     Revision 1.26  1997/04/01 23:38:08  timj
*     Lots of efficiency gains: SCULIB_GET_* and SCULIB_PROCESS_BOLS
*     Minor things: Start to use PKG and TSKNAME
*
*     Revision 1.25  1997/03/20 21:56:36  jfl
*     modified to handle aborted observations correctly
*
c Revision 1.24  1997/03/06  20:07:55  timj
c Improve documentation
c
c Revision 1.23  1997/01/11  01:43:05  timj
c Merge with BOLREBIN.
c Fix RASTER map problems (DEC_END)
c
c Revision 1.22  1997/01/10  19:09:12  timj
c Improve header documentation.
c Set XMAX and XMIn etc before checking for max and min of data.
c
c Revision 1.21  1996/12/17  21:09:49  timj
c Write SCUPROJ to output file. (for SCUOVER)
c
c Revision 1.20  1996/12/13  02:38:51  timj
c Replace DAT_FIND with CMP_.
c Report number of good integrations. (not just total).
c Change PRINT to MSG_OUT for timing data.
c
c Revision 1.19  1996/12/12  01:18:02  timj
c Remove CALC_AZNA_OFFSET (merged with CALC_BOL_COORDS)
c
c Revision 1.18  1996/11/14  02:52:54  timj
c Add support for AZ and NA regrids.
c
c Revision 1.17  1996/11/07  00:20:17  timj
c Change MAX_FILE to 100
c
c Revision 1.16  1996/11/05  02:08:04  timj
c Set BAD_PIXEL flag for DATA and VARIANCE
c
c Revision 1.15  1996/11/02  01:22:19  timj
c Tweak 'arameters' in header
c
c Revision 1.14  1996/11/02  00:52:10  timj
c Use NDF_ASSOC and REF parameter.
c Tweak the header
c
c Revision 1.13  1996/11/01  00:19:49  timj
c Add default for OUT_OBJECT.
c Add RD to IRAS90 output and pass current EPOCH to FITS header.
c
c Revision 1.12  1996/10/30  00:30:41  timj
c Added GLOBAL default for IN parameter.
c Fixed bug (segmentation fault) when error occurs before NDF_STATE.
c Replace calls to SCULIB_COPY? with VEC_?TO?
c
*     {note_history_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'AST_PAR'                ! AST__ constants
      INCLUDE 'DAT_PAR'                ! DAT__ constants
      INCLUDE 'MSG_PAR'                ! MSG__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'PAR_ERR'                ! for PAR__ constants
      INCLUDE 'PAR_PAR'                ! for PAR__ constants
      INCLUDE 'SURF_PAR'               ! REDS definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions
      INCLUDE 'CNF_PAR'                ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * (*)  TSKNAME

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                  ! CHR used-string-length function
      BYTE    SCULIB_BITON             ! Turn on bit

* Local Constants:
      INTEGER WTFNRES                  ! number of values per scale length
      PARAMETER (WTFNRES = 64)
      INTEGER WTFN_MAXRAD              ! Max radius of weighting function
      PARAMETER (WTFN_MAXRAD = 10)     ! in scale-lengths
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      BYTE BADBIT                      ! Bad bit mask
      PARAMETER (BADBIT = 3)
*  Note that if MAX_FILE is increased you should also increase the
*  SCUBA__MAX_FITS constant in SURF_PAR such that there is enough
*  space for all the potential extra FITS headers. This is because
*  we create a FITS header per input file.      
      INTEGER     MAX_FILE             ! max number of input files
      PARAMETER (MAX_FILE = 256)

      
*  Local variables:
      INTEGER          ABOL_DATA_END(MAX_FILE)
                                       ! Pointer to bolometer data end
      INTEGER          ABOL_DATA_PTR(MAX_FILE)
                                       ! Pointer to bolometer data
      INTEGER          ABOL_DEC_END(MAX_FILE)
                                       ! Pointer to bolometer dec end
      INTEGER          ABOL_DEC_PTR(MAX_FILE)
                                       ! Pointer to bolometer dec
      INTEGER          ABOL_RA_END(MAX_FILE)
                                       ! Pointer to bolometer RA end
      INTEGER          ABOL_RA_PTR(MAX_FILE)
                                       ! Pointer to bolometer RA
      INTEGER          ABOL_VAR_END(MAX_FILE)
                                       ! Pointer to bolometer var end
      INTEGER          ABOL_VAR_PTR(MAX_FILE)
                                       ! Pointer to bolometer variance
      REAL             ANG_INT(MAX_FILE,SCUBA__MAX_INT, 2) 
                                       ! Angles (wplate,angrot) for each
                                       ! integration (polarimetry) 
      REAL             ANG_MEAS(MAX_FILE,SCUBA__MAX_MEAS, 2)
                                       ! Angles (wplate,angrot) for each
                                       ! measurement (polarimetry) 
      REAL             ANGROT          ! POLPACK ANGROT (degrees)
      BYTE             BADB            ! Bad bit mask for despiking
      INTEGER          BITNUM          ! Bit set by DESPIKE
      LOGICAL          BOLREBIN        ! Am I rebinning bols separately?
      REAL             BOLWT (SCUBA__NUM_CHAN*SCUBA__NUM_ADC, MAX_FILE)
                                       ! Bolometer weights
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      INTEGER          BOL_DEC_END (MAX_FILE)
                                       ! pointer to end of BOL_DEC_PTR scratch
                                       ! space
      INTEGER          BOL_DEC_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! apparent Dec / y offset positions of
                                       ! measured points in input file (radians)
      INTEGER          BOL_RA_END (MAX_FILE)
                                       ! pointer to end of BOL_RA_PTR scratch
                                       ! space
      INTEGER          BOL_RA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! apparent RA / x offset positions of
                                       ! measured points in input file (radians)
      REAL             BOXSZ           ! Box size in seconds
      INTEGER          BOX_SIZE(MAX_FILE) ! Size of smoothing box (pixels)
      LOGICAL          CALCSKY         ! Are we calc skying?
      CHARACTER*5      CHOP_CRD        ! Chop coordinate frame
      REAL             CHOP_PA         ! Chop PA in arcsec
      REAL             CHOP_THROW      ! Chop throw in arcsec
      INTEGER          CHR_STATUS      ! Status for call to CHR_FIWE
      INTEGER          COUNT           ! Number of ints in INTREBIN
      INTEGER          CURR_FILE ! Current file in INTREBIN loop
      INTEGER          CURR_INT        ! Current int in INTREBIN loop
      LOGICAL          DEFOUT          ! Take default output files for DESPIKE
      LOGICAL          DESPIKE         ! Is this the despike task
      REAL             DIAMETER        ! diameter of primary mirror
      INTEGER          DOFFSET         ! Byte offset into DOUBLE data array
      LOGICAL          DOLOOP          ! Loop for input data
      LOGICAL          DOREBIN         ! True if we are rebinning the data
      DOUBLE PRECISION DTEMP           ! Scratch double
      INTEGER          EACHBOL         ! Bolometer loop counter
      REAL             EFF_RADIUS      ! EFfective radius of influence for spline
      INTEGER          FD              ! File descriptor
      INTEGER          FILE            ! number of input files read
      CHARACTER*128    FILENAME (MAX_FILE)
                                       ! names of input files read
      INTEGER          FITSCHAN        ! Merged AST FITS channel
      INTEGER          FITSTEMP        ! Temporary FITS channel
      INTEGER          FITSTEMPM       ! Temporary Merged FITS channel
      CHARACTER*80     FITS(SCUBA__MAX_FITS, MAX_FILE)
                                       ! FITS entries for each file
      LOGICAL          HAVE_MODEL      ! .TRUE. if CALCSKY is using a 
                                       ! source model
      CHARACTER*(DAT__SZNAM) HDSNAME   ! Name of the container (not the fname)
      LOGICAL          HOURS           ! .TRUE. if the angle being read in is
                                       ! in units of hours rather than degrees
      INTEGER          I               ! DO loop index
      INTEGER          ICARD           ! Counter for FITS cards
      INTEGER          IERR            ! For VEC copies
      INTEGER          IJ_PTR_END      ! End of scratch pointer
      INTEGER          IJ_PTR          ! Scratch pointer
      INTEGER          IMNDF           ! NDF id for CALCSKY source model
      CHARACTER*256    IN              ! input filename and data-spec
      CHARACTER*20     INSTRUMENT      ! Name of instrument that took the data
      LOGICAL          INTREBIN        ! Am I rebinning ints separately?
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
                                ! Pointer to integration posns
      INTEGER          INT_NEXT        ! Position of start of next int
      INTEGER          INT_START       ! Position of start of int
      INTEGER          IN_DATA_END (MAX_FILE)
                                       ! pointer to end of scratch space 
                                       ! holding data from input files
      INTEGER          IN_DATA_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data from input files
      DOUBLE PRECISION IN_DEC_CEN(MAX_FILE)!apparent Dec of input file map centre
                                       ! (radians)
      INTEGER          IN_QUALITY_END (MAX_FILE)
                                       ! Pointer to end of scratch space
                                       ! holding quality from input files
      INTEGER          IN_QUALITY_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data variance from input files
      DOUBLE PRECISION IN_RA_CEN(MAX_FILE)!apparent RA of input file map centre
                                       ! (radians)
      DOUBLE PRECISION IN_UT1(MAX_FILE)! UT1 at start of an input observation,
                                       ! expressed as modified Julian day
      INTEGER          IN_VARIANCE_END (MAX_FILE)
                                       ! pointer to end of scratch space
                                       ! holding variance from input files
      INTEGER          IN_VARIANCE_PTR (MAX_FILE)
                                       ! pointer to scratch space holding
                                       ! data variance from input files
      INTEGER          IPOSN           ! Position in string
      LOGICAL          ISNEWPD         ! Is this a new data pointer?
      LOGICAL          ISNEWPV         ! Is this a new variance pointer?
      LOGICAL          ISNEWPR         ! Is this a new ra pointer?
      LOGICAL          ISNEWPC         ! Is this a new dec pointer?
      INTEGER          ITEMP           ! scratch integer
      INTEGER          IX              ! Temporary X integer
      INTEGER          IY              ! Temporary Y integer
      INTEGER          I_CENTRE        ! I index of central pixel in output
                                       ! map
      LOGICAL          JIGGLE          ! Are we rebinning a JIGGLE map
      INTEGER          J_CENTRE        ! J index of central pixel in output
                                       ! map
      DOUBLE PRECISION LAT_OBS         ! Latitude of observatory
      INTEGER          LBND (MAX_DIM)  ! pixel indices of bottom left 
                                       ! corner of output image
      CHARACTER * (5)  MAPNAME         ! Name of each bolometer
      INTEGER          MAP_SIZE(2)     ! Number of pixels in map (x,y)
      INTEGER          MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS + 1)
                                       ! pointers to start of each measurement 
      CHARACTER* (15)  METHOD          ! rebinning method
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs
                                       ! of all
                                       ! measured positions are calculated
      DOUBLE PRECISION MODEL_DEC_CEN   ! centre DEC of CALCSKY model 
                                       ! (radians)
      CHARACTER*80     MODEL_FITS(SCUBA__MAX_FITS) 
                                       ! FITS entreis in CALCSKY model
      DOUBLE PRECISION MODEL_RA_CEN    ! centre RA of CALCSKY model
                                       ! (radians)
      CHARACTER*(DAT__SZLOC) M_FITSX_LOC ! locator to model FITS array
      INTEGER          NDF_PTR         ! Pointer to mapped NDF array
      INTEGER          NERR            ! For VEC copies
      REAL             NEWPA           ! New chop pa
      REAL             NEWTHROW        ! New chop throw
      INTEGER          NFILES          ! Number of files in INTREBIN/REBIN
      INTEGER          NPARS           ! Number of dummy pars
      INTEGER          NSPIKES(MAX_FILE) ! Number of spikes per file
      DOUBLE PRECISION NYQUIST         ! Nyquist sampling Lambda/2D
      REAL             NYQWAV          ! Wavelength used for Nyquist calc
      INTEGER          N_BOL (MAX_FILE)! number of bolometers measured in input
                                       ! files
      INTEGER          N_FILE_LOOPS    ! Number of loops for INTREBIN/REBIN
      INTEGER          N_FITS(MAX_FILE)! Number of FITS entries per file
      INTEGER          N_INTS(MAX_FILE)! Number of integrations per input file
      INTEGER          NINTS(MAX_FILE) ! N_INTS for INTREBIN support
      INTEGER          N_MEAS(MAX_FILE)! Number of measurements per input file
      INTEGER          N_M_FITS        ! Number of FITS entries in the model
      INTEGER          N_PIXELS        ! Number of pixels in output map
      INTEGER          N_POS (MAX_FILE)! number of positions measured in input
                                       ! files
      INTEGER          N_PTS (MAX_FILE)! Number of bols * positions
      CHARACTER*40     OBJECT(MAX_FILE)! name of object
      INTEGER          OFFSET          ! Offset into array
      LOGICAL          OKAY            ! Okay to rebin?
      CHARACTER*(132)  OUT             ! Output file name
      CHARACTER*40     OUT_COORDS      ! coordinate system of output map
      INTEGER          OUT_DATA_END    ! End of output data
      INTEGER          OUT_DATA_PTR    ! pointer to output map data array
      DOUBLE PRECISION OUT_DEC_CEN     ! apparent Dec of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_LAT         ! longitude of output map centre 
                                       ! (radians)
      CHARACTER*(DAT__SZLOC) OUT_LOC   ! locator of HDS item in output file
      DOUBLE PRECISION OUT_LONG        ! longitude of output map centre
                                       ! (radians)
      INTEGER          OUT_NDF         ! NDF index of output file
      REAL             OUT_PIXEL       ! size of pixels in output map (radians)
      INTEGER          OUT_QUALITY_END ! End of quality array
      INTEGER          OUT_QUALITY_PTR ! pointer to output map quality array
      DOUBLE PRECISION OUT_RA_CEN      ! apparent RA of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_ROTATION    ! angle between apparent N and N of
                                       ! output coord system (radians)
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! locator to REDS extension in output
      CHARACTER * (132)OUT_TITLE       ! Title of output NDF
      CHARACTER * (20) OUT_UNITS       ! Units of output NDF
      INTEGER          OUT_VARIANCE_END! End of pointer to output map variance
      INTEGER          OUT_VARIANCE_PTR! pointer to output map variance array
      INTEGER          OUT_WEIGHT_NDF  ! NDF identifier for weight array
      INTEGER          OUT_WEIGHT_END  ! Pointer to end weights array
      INTEGER          OUT_WEIGHT_PTR  ! Pointer to weights array
      REAL             PARS(3)         ! Dummy array of parameter values
      CHARACTER * (PAR__SZNAM) PARAM   ! Name of input parameter
      INTEGER          PLACE           ! Place holder for output NDF
      LOGICAL          READING         ! .TRUE. while reading input files
      BYTE             QBITS(MAX_FILE) ! BADBITS for each input file
      LOGICAL          QMF             ! .false. = return quality array
                                       ! .true.= return masked data
                                       ! This is used in NDF_SQMF. Rebinning
                                       ! data should use .true.
      INTEGER          QPTR            ! Pointer to mapped output quality
      LOGICAL          RASTER          ! Are we regridding a RASTER obs.
      LOGICAL          RAS_CHOP_LO     ! Is raster chopping in LO
      LOGICAL          RAS_CHOP_SC     ! Is raster chopping in SC
      CHARACTER * (10) REB_SUFFIX_STR(SCUBA__N_SUFFIX) ! Suffix for OUT REBIN
      INTEGER          RLEV     ! Recursion depth for reading
      INTEGER          ROFFSET         ! Byte offset into REAL data array
      REAL             RTEMP           ! Scratch real
      CHARACTER*10     SAMPLE_MODE     ! Sample mode for maps
      REAL             SCALE           ! Size of 1 scale length for WT rebin
      INTEGER          SECNDF          ! NDF id to section
      REAL             SFACTOR         ! Smoothing factor for spline PDA_SURFIT
      REAL             SHIFT_DX (MAX_FILE)
                                       ! x shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL             SHIFT_DY (MAX_FILE)
                                       ! y shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      INTEGER          SKYNDF          ! NDF identifier to SKY
      INTEGER          SKY_PTR(MAX_FILE) ! Pointer to SKY data
      INTEGER          SKY_END(MAX_FILE) ! Pointer to end of SKY data
      INTEGER          SKY_VPTR(MAX_FILE) ! Pointer to SKY error
      INTEGER          SKY_VEND(MAX_FILE) ! Pointer to end of SKY error
      CHARACTER*40     SOBJECT         ! name of first object
      CHARACTER*10     SPMETHOD        ! Method of spline interpolation
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      CHARACTER*15     SUTDATE         ! date of first observation
      CHARACTER*15     SUTSTART        ! UT of start of first observation
      CHARACTER*20     TELESCOPE       ! Name of telescope that took the data
      LOGICAL          THERE           ! Is a component there?
      LOGICAL          TIMES           ! Store the TIMES array
      INTEGER          TOT(MAX_FILE)   ! Number of integrations per input file
      INTEGER          TOTAL_BOLS      ! Number of bolometers
      REAL             TRIM            ! Trim radius (arcsec)
      INTEGER          UBND (MAX_DIM)  ! pixel indices of top right corner
                                       ! of output image
      LOGICAL          GUARD           ! Use guard ring in wt regrid?
      CHARACTER*15     UTDATE(MAX_FILE)! date of first observation
      CHARACTER*15     UTSTART(MAX_FILE)! UT of start of first observation
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
      REAL             WEIGHT (MAX_FILE)
                                       ! weights assigned to each input file
      LOGICAL          WEIGHTS         ! Store the weights array
      INTEGER          WEIGHTSIZE      ! Radius of weighting function
      REAL             WPLATE          ! Waveplate angle (for POLPACK)
      REAL             WTFN (WTFN_MAXRAD * WTFN_MAXRAD * 
     :     WTFNRES * WTFNRES + 1)
                                       ! Weighting function

*  Local data:
      DATA SUFFIX_STRINGS /'!_dsp','d','_dsp'/ ! Used for DESPIKE
      DATA REB_SUFFIX_STR /'!_reb','b','_reb'/ ! Used for REBIN

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialize
      INTREBIN = .FALSE.
      BOLREBIN = .FALSE.
      DESPIKE  = .FALSE.
      CALCSKY  = .FALSE.
      QMF = .TRUE.
      DOREBIN  = .TRUE.
      HAVE_MODEL = .FALSE.
      SOBJECT = ' '
      OUT_COORDS = ' '
      OUT_REDSX_LOC = DAT__NOLOC

      ISNEWPD = .FALSE.
      ISNEWPV = .FALSE.
      ISNEWPR = .FALSE.
      ISNEWPC = .FALSE.

      CALL AST_BEGIN( STATUS )

* Setup taskname (can never have BOLREBIN and INTREBIN)

      IF (TSKNAME .EQ. 'BOLREBIN') THEN
         BOLREBIN = .TRUE.
      ELSE IF (TSKNAME .EQ. 'INTREBIN') THEN
         INTREBIN = .TRUE.
      ELSE IF (TSKNAME .EQ. 'DESPIKE') THEN
         DESPIKE = .TRUE.
         QMF = .FALSE.
      ELSE IF (TSKNAME .EQ. 'CALCSKY') THEN
         CALCSKY = .TRUE.
         QMF = .FALSE.
      END IF

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* Read in the rebin method if necessary

      IF (TSKNAME .EQ. 'EXTRACT_DATA') THEN

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', '^PKG: Extracting data and'//
     :        ' position information', STATUS)

      ELSE IF (TSKNAME .EQ. 'DESPIKE') THEN

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__VERB, ' ', '^PKG: Despiking data',
     :        STATUS)

      ELSE IF (TSKNAME .EQ. 'CALCSKY') THEN

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__VERB, ' ', '^PKG: Calculating sky',
     :        STATUS)

*  check if a source model is to be used. If it is then use its
*  centre RA and Dec.

         HAVE_MODEL = .FALSE.
         IF (STATUS .EQ. SAI__OK) THEN
            CALL NDF_ASSOC ('MODEL', 'READ', IMNDF, STATUS)

*     Check for NULL response (ie no model)
            IF (STATUS .EQ. SAI__OK) THEN
               HAVE_MODEL = .TRUE.             

*  read FITS header
               CALL NDF_XLOC (IMNDF, 'FITS', 'READ', M_FITSX_LOC,
     :              STATUS)
               CALL DAT_SIZE (M_FITSX_LOC, ITEMP, STATUS)
               IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC ('TASK', TSKNAME)
                     CALL ERR_REP (' ', '^TASK: model file '//
     :                    'contains too many FITS items', STATUS)
                  END IF
               END IF
               
               CALL DAT_GET1C (M_FITSX_LOC, SCUBA__MAX_FITS, MODEL_FITS,
     :              N_M_FITS, STATUS)
               CALL DAT_ANNUL (M_FITSX_LOC, STATUS)

*     read centre coords, long and lat centre of model map
*     Eventually we should be able to read full WCS info from header
*     and convert to JCMT coordinate frames -- would put it in a
*     subroutine of course...

               CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_M_FITS,
     :              MODEL_FITS, 'SCUPROJ', OUT_COORDS, STATUS)
*               print *, 'cproj ', out_coords

*     For PLanet we do not need a LONG/LAT
               IF (OUT_COORDS .NE. 'PL' .AND. OUT_COORDS .NE. 'AZ'
     :              .AND. OUT_COORDS .NE. 'NA') THEN
                  CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_M_FITS,
     :                 MODEL_FITS, 'LONG', MODEL_RA_CEN, STATUS)
                  CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_M_FITS,
     :                 MODEL_FITS, 'LAT', MODEL_DEC_CEN, STATUS)
               ELSE
                  MODEL_RA_CEN = 0.0
                  MODEL_DEC_CEN = 0.0
               END IF

*               print *, model_ra_cen, model_dec_cen 

*     NOTE: The model file is kept open until after the despiking.
            
            ELSE IF (STATUS .EQ. PAR__NULL) THEN

*     A null response indicates no model but we have to reset the status
               CALL ERR_ANNUL( STATUS )

            END IF
            
         END IF

      ELSE
*     Regridding
         CALL PAR_CHOIC('REBIN_METHOD', 'Linear', 
     :        'Linear,Bessel,Gaussian,Spline1,Spline2,Spline3,Median', 
     :        .TRUE.,METHOD, STATUS)
         
      END IF

*     get the output coordinate system and set the default centre of the
*     output map
*     This needs to be done in advance of reading files as some systems
*     do not need to convert coordinate systems to apparent RA,Dec (eg NA).
*     It does not need to be done if we've already read the coords from
*     a source model in CALCSKY.

      IF (.NOT. CALCSKY .OR. .NOT. HAVE_MODEL) THEN 
         CALL PAR_CHOIC('OUT_COORDS','RJ','AZ,NA,RB,RJ,GA,RD,PL',
     :     .TRUE., OUT_COORDS, STATUS)
      END IF

      HOURS = .TRUE.
      IF (OUT_COORDS .EQ. 'RB') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK4 B1950.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'RJ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are FK5 J2000.0', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'GA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :     '^PKG: output coordinates are galactic', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'RD') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are apparent RA,Dec '//
     :        '(no date as yet)', STATUS)
      ELSE IF (OUT_COORDS .EQ. 'NA') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are nasmyth', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are Az/El offsets', STATUS)
         HOURS = .FALSE.
      ELSE IF (OUT_COORDS .EQ. 'PL') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: output coordinates are offsets from moving centre',
     :        STATUS)
         HOURS = .FALSE.
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('C', OUT_COORDS )
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: invalid output '//
     :        'coordinate system (^C)', STATUS)
         END IF
      END IF

*     Ask if I am looping over infinite input files or whether
*     people only want to be asked once

      CALL PAR_GET0L('LOOP', DOLOOP, STATUS)

*  start up the NDF system and read in the input demodulated files

      CALL NDF_BEGIN

      READING = .TRUE.
      FILE = 1
      IF (STATUS .NE. SAI__OK) READING = .FALSE.
      
      DO WHILE (READING)

*  read the name of the file to be read

*     Read in the GLOBAL value first
         IF (FILE .EQ. 1) THEN
            PARAM = 'REF'
         ELSE
            PARAM = 'IN'
*     Make sure the parameter is cancelled
*     IF (FILE.GT.2) CALL PAR_CANCL(PARAM, STATUS)
         END IF
         
*     Set the default to GLOBAL first (have to do it this way since
*     the GLOBAL value MUST be associated with an NDF parameter
         IF (FILE .EQ. 1) THEN
            CALL SCULIB_GET_FILENAME('DUMMY', STEMP, STATUS)
            CALL PAR_DEF0C('REF', STEMP, STATUS)
         END IF

*     Read in the file and, if required, a section
         CALL PAR_GET0C (PARAM, IN, STATUS)
         
         IF (STATUS .EQ. PAR__NULL) THEN
            READING = .FALSE.
            CALL ERR_ANNUL(STATUS)

         ELSE IF (STATUS .EQ. SAI__OK) THEN

            RLEV = 1 ! Set recursion level
            NPARS = 0 ! No parameters set before entry

            CALL SURF_RECURSE_READ( RLEV, IN, MAX_FILE,
     :           OUT_COORDS, FILE, N_BOL, N_POS, 
     :           N_INTS, N_MEAS, IN_UT1, IN_RA_CEN, 
     :           IN_DEC_CEN, FITS, N_FITS, WAVELENGTH, 
     :           SUB_INSTRUMENT, OBJECT, UTDATE, 
     :           UTSTART, FILENAME, BOL_ADC, BOL_CHAN,
     :           BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR, 
     :           BOL_DEC_END, IN_DATA_PTR, IN_DATA_END, 
     :           IN_VARIANCE_PTR, IN_VARIANCE_END,
     :           QMF, IN_QUALITY_PTR, IN_QUALITY_END, QBITS,
     :           ANG_INT, ANG_MEAS,
     :           INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX, 
     :           SHIFT_DY, NPARS, PARS,
     :           STATUS)


*     Check error return

            IF (STATUS .EQ. PAR__ABORT) THEN
*     If the status is ABORT then I should not annul the status
*     but should exit loop gracefully

               READING = .FALSE.

            ELSE IF (STATUS .NE. SAI__OK) THEN
               IF (FILE .GT. MAX_FILE) READING = .FALSE.
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP(' ','^TASK: Error occured whilst '//
     :              'reading NDF or text file', STATUS)
               CALL ERR_FLUSH(STATUS)
            END IF

         ELSE

            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP(' ','^TASK: Error reading parameter', STATUS)
            READING = .FALSE.

         END IF

*     Cancel the parameter
         CALL PAR_CANCL(PARAM, STATUS)

*     Finish looping if DOLOOP is false

         IF (.NOT.DOLOOP) READING = .FALSE.
         
      END DO

*     FILE is always one larger than the number of successful
*     reads
      FILE = FILE - 1


*----------------------------------------------------------------------
* End of loop, now have all the data


*  OK, all the data required should have been read in by now, check that
*  there is some input data

      IF (STATUS .EQ. SAI__OK) THEN
         IF (FILE .LE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: there are no '//
     :           'input data', STATUS)

         ELSE

*     Inform the user about the files they have just read in


            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM,' ','^PKG Input data: '//
     :           '(name, weight, dx, dy)', STATUS)

            DO I = 1, FILE

               CALL MSG_SETC('FILE', FILENAME(I))
               CALL MSG_SETR('SDX', SHIFT_DX(I) * REAL(R2AS))
               CALL MSG_SETR('SDY', SHIFT_DY(I) * REAL(R2AS))
               CALL MSG_SETR('WT', WEIGHT(I))
               CALL MSG_SETI('I',I)

               CALL MSG_OUTIF(MSG__NORM, ' ','   -- ^I: '//
     :              '^FILE (^WT, ^SDX, ^SDY)', STATUS)
            END DO
            CALL MSG_BLANK(STATUS)

*     Store the reference values (ie from the first data set

            SOBJECT = OBJECT(1)
            SUTDATE = UTDATE(1)
            SUTSTART = UTSTART(1)
            MJD_STANDARD = IN_UT1(1)
            OUT_RA_CEN = IN_RA_CEN(1)
            OUT_DEC_CEN = IN_DEC_CEN(1)

         END IF
      END IF

*     Since this code was written the ATL and AST libraries make header
*     merging trivial. In the past we were forced to make up an output
*     FITS header and throw away most of the input information. Now we
*     go through each fits header and merge it into a single output fitschan

      DO I = 1, FILE
*     Create a temporary fits chan from this header
*     (obviously we should replace all SURF use of FITS() and N_FITS
*     with a FITSCHAN but be reasonable)
*     First time through use the main fitschan, otherwise create new one
         
         FITSTEMP = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS)

*     Skip headers that are SUB_N FILT_N etc
         DO ICARD = 1, N_FITS(I)
            IF (.NOT. AST_CHRSUB(FITS(ICARD,I),
     :           '_\d\s*=', STEMP, STATUS ) .AND.
     :           FITS(ICARD,I)(1:8) .NE. 'NDFUNITS' .AND.
     :           FITS(ICARD,I) .NE. 'END'
     :           ) THEN
               CALL AST_PUTFITS( FITSTEMP, FITS( ICARD, I ), .FALSE., 
     :              STATUS )
            END IF
         END DO

*     Only merge after first file
         IF (I .GT. 1) THEN
            CALL ATL_MGFTS( 3, FITSTEMP, FITSCHAN, FITSTEMPM,
     :           STATUS)
            CALL AST_ANNUL( FITSCHAN, STATUS )
            CALL AST_ANNUL( FITSTEMP, STATUS )
            FITSCHAN = FITSTEMPM
         ELSE
            FITSCHAN = FITSTEMP
         END IF
      END DO


*     Check that the CHOP_PA matches if we are using SCAN/MAP
*     with LO chopping

      CHOP_THROW = VAL__BADR
      CHOP_PA  = VAL__BADR
      CHOP_CRD = ' '
      OKAY = .FALSE.
      SAMPLE_MODE = ' '

*     Nothing to check if we only have one input file
      IF (FILE .GT. 1) THEN

*     First retrieve the SAMPLE_MODE
         RASTER = .FALSE.
         JIGGLE = .FALSE.
         RAS_CHOP_LO = .FALSE.
         RAS_CHOP_SC = .FALSE.

         DO I = 1, FILE

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS(I), 
     :           FITS(1,I), 'SAM_MODE', SAMPLE_MODE, STATUS)
            CALL CHR_UCASE (SAMPLE_MODE)

            IF (SAMPLE_MODE .EQ. 'RASTER') THEN
               RASTER = .TRUE.

*     Find CHOP_COORDS
               CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS(I), 
     :              FITS(1,I), 'CHOP_CRD', CHOP_CRD, STATUS)

               IF (CHOP_CRD .EQ. 'LO') THEN
                  RAS_CHOP_LO = .TRUE.
               ELSE IF (CHOP_CRD .EQ. 'SC') THEN
                  RAS_CHOP_SC = .TRUE.
               END IF


            ELSE IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
               JIGGLE = .TRUE.
            END IF

         END DO

*     Now compare out flags
         IF (RASTER .AND. JIGGLE) THEN

            IF (RAS_CHOP_LO) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__QUIET, ' ','^TASK: WARNING! '//
     :              'You are combining jiggle and dual beam '//
     :              'scan data.', STATUS)

            END IF
         
         ELSE IF (RASTER) THEN

            IF (RAS_CHOP_LO .AND. RAS_CHOP_SC) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__QUIET, ' ','^TASK: WARNING! '//
     :              'You are combining SCAN/MAP data with different '//
     :              'chopping techniques (SC and LO).', STATUS)
               
            ELSE IF (RAS_CHOP_LO) THEN
*     If we are using RASTER with only LO chopping we need to check that
*     the chop configurations are acceptable

               CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(1), 
     :              FITS(1,1), 'CHOP_THR', CHOP_THROW, STATUS)
               CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(1), 
     :              FITS(1,1), 'CHOP_PA', CHOP_PA, STATUS)
               OKAY = .TRUE.
               
*     Loop over files comparing chop config
               DO I = 2, FILE
                  CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(I), 
     :                 FITS(1,I), 'CHOP_THR', NEWTHROW, STATUS)
                  CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(I), 
     :                 FITS(1,I), 'CHOP_PA', NEWPA, STATUS)

                  IF (NEWPA .NE. CHOP_PA .OR. 
     :                 NEWTHROW .NE. CHOP_THROW) THEN
                     OKAY = .FALSE.
                  END IF
               END DO

               IF (.NOT.OKAY) THEN
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL MSG_OUTIF(MSG__QUIET, ' ','^TASK: WARNING! '//
     :                 'You are combining different chop '//
     :                 'configurations.', STATUS)
               ELSE
                  CHOP_CRD = 'RJ' ! Always chop in RJ
               END IF

            END IF

         END IF

      ELSE
*     Retrieve chop information from first scan only
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS(1), 
     :        FITS(1,1), 'SAM_MODE', SAMPLE_MODE, STATUS)
         CALL CHR_UCASE (SAMPLE_MODE)

         IF (SAMPLE_MODE .EQ. 'RASTER') THEN

            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS(1), 
     :           FITS(1,1), 'CHOP_CRD', CHOP_CRD, STATUS)
            
            IF (CHOP_CRD .EQ. 'LO') THEN

               CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(1), 
     :              FITS(1,1), 'CHOP_THR', CHOP_THROW, STATUS)
               CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(1), 
     :              FITS(1,1), 'CHOP_PA', CHOP_PA, STATUS)

               CHOP_CRD = 'RJ' ! Always chop in RJ
               OKAY = .TRUE.

            END IF

         END IF
      END IF

*     Nasmyth rebin doesn't need a coordinate frame
*     Should only get through here if I am trying to rebin NA or AZ
*     with SCAN data

      IF ((OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ' 
     :     .AND. OUT_COORDS.NE.'PL')) THEN

*     Inform the 'RD' regridder the date of regrid

         IF (OUT_COORDS .EQ. 'RD') THEN
            CALL MSG_SETC ('UTDATE', SUTDATE)
            CALL MSG_SETC ('UTSTART', SUTSTART)
            CALL MSG_SETC('PKG', PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Using output coordinates of '//
     :           'apparent RA,Dec at ^UTSTART on ^UTDATE', STATUS)
         END IF

*     Read the latitude of the observatory
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS(1), FITS(1,1),
     :     'LAT-OBS', LAT_OBS, STATUS)
      IF (STATUS .EQ. SAI__OK) LAT_OBS = LAT_OBS * PI / 180.0D0

*     If we are just despiking or calcsky-ing we don't need to ask about
*     the coordinates

         IF (.NOT.DESPIKE .AND. .NOT.CALCSKY) THEN

*     Request the new apparent ra/dec centre
            CALL SURF_REQUEST_OUTPUT_COORDS( TSKNAME, 'LONG_OUT',
     :           'LAT_OUT', OUT_COORDS, LAT_OBS, OUT_RA_CEN,OUT_DEC_CEN,
     :           MJD_STANDARD, HOURS, OUT_RA_CEN, OUT_DEC_CEN, 
     :           OUT_ROTATION, OUT_LONG, OUT_LAT, STATUS)

         ELSE

*     We already have the apparent ra/dec of the centre - we need
*     the rotation angle though. Either we calculate it ourselves
*     or we convert from apparent to long and lat and back again!!!
*     Probably want a separate rotation sub

*     Convert the input coordinates to the output coordinates
*     and use them as the default. If we have a CALCSKY model
*     we already know the OUT_LONG and OUT_LAT

            IF (.NOT. CALCSKY .OR. .NOT. HAVE_MODEL) THEN
               CALL SCULIB_CALC_OUTPUT_COORDS (OUT_RA_CEN, OUT_DEC_CEN, 
     :              MJD_STANDARD, OUT_COORDS, OUT_LONG, OUT_LAT, STATUS)
            ELSE
               OUT_LONG = MODEL_RA_CEN
               OUT_LAT  = MODEL_DEC_CEN
            END IF
*            print *,'OUTLONG OUTLAT', OUT_LONG, OUT_LAT

*     calculate the apparent RA,Dec of the selected output centre
*     (Hopefully this will provide the same values for OUT_RA_CEN
*     and OUT_DEC_CEN) as were supplied to SCULIB_CALC_OUTPUT_COORDS)

            CALL SCULIB_CALC_APPARENT (LAT_OBS, OUT_LONG, OUT_LAT,0.0D0,
     :           0.0D0, 0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD_STANDARD, 
     :           0.0D0, 0.0D0, OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, 
     :           STATUS)

         END IF            

*  convert the RA,Decs of the observed points to tangent plane offsets
*  from the chosen output centre

         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               CALL SCULIB_APPARENT_2_TP (N_BOL(I) * N_POS(I), 
     :              %VAL(CNF_PVAL(BOL_RA_PTR(I))), 
     :              %VAL(CNF_PVAL(BOL_DEC_PTR(I))),
     :              OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION,
     :              DBLE(SHIFT_DX(I)), DBLE(SHIFT_DY(I)), STATUS)
            END DO
         END IF


*  Deal with NA coords in Jiggle mode
      ELSE

         OUT_RA_CEN = 0.0
         OUT_DEC_CEN = 0.0

*     Add a shift to the output coordinates
         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE

               CALL SCULIB_ADDCAD(N_BOL(I) * N_POS(I),
     :              %VAL(CNF_PVAL(BOL_RA_PTR(I))), DBLE(SHIFT_DX(I)),
     :              %VAL(CNF_PVAL(BOL_RA_PTR(I))))
               CALL SCULIB_ADDCAD(N_BOL(I) * N_POS(I),
     :              %VAL(CNF_PVAL(BOL_DEC_PTR(I))), DBLE(SHIFT_DY(I)),
     :              %VAL(CNF_PVAL(BOL_DEC_PTR(I))))
               
            END DO
         END IF

      END IF

*     Extract the telescope name from the first fits array
      CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS(1),
     :     FITS(1,1), 'TELESCOP', TELESCOPE, STATUS)

*     The diameter of the primary mirror should really be a FITS
*     header but now must be derived from the telescope name
      DIAMETER = 1
      IF ( TELESCOPE .EQ. 'JCMT' ) THEN
         DIAMETER = 15.0
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PKG', PACKAGE )
            CALL MSG_SETC( 'TEL', TELESCOPE )
            CALL ERR_REP( ' ', '^PKG: Unable to determine primary '//
     :           'mirror diameter for telescope "^TEL"', STATUS )
         END IF
      END IF

*     Calculate the Nyquist sampling value (in radians)
*     The WAVELENGTH value can not always be trusted: THUMP
*     THUMPER is has feedhorns that behave like the 850 micron SCUBA
*     pixels
      NYQWAV = WAVELENGTH
      IF ( SUB_INSTRUMENT .EQ. 'THUMP' ) NYQWAV = 850.0

      NYQUIST = (DBLE(NYQWAV) * 1.0D-6 ) / 
     :     ( 2.0D0 * DBLE(DIAMETER))

*     Now if I want to I can simply write the data out to a file
*     I can do this for task EXTRACT_DATA

      IF (TSKNAME .EQ. 'EXTRACT_DATA') THEN

*     Open an output file (I really want to check for STDOUT)
         CALL FIO_ASSOC('FILE', 'WRITE', 'LIST', 0, FD, STATUS)

*     Write out a small header

         CALL FIO_WRITE(FD,
     :        '       Delta X (Radians)      Delta Y(Radians) '//
     :        '        Data         Variance',
     :        STATUS)

*     Write out the good numbers
         DO I = 1, FILE

*     Multiply all the data by the weight and variance by weight squared
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_MULCAR(N_POS(I) * N_BOL(I),
     :              %VAL(CNF_PVAL(IN_DATA_PTR(I))), WEIGHT(I),
     :              %VAL(CNF_PVAL(IN_DATA_PTR(I))))

               CALL SCULIB_MULCAR(N_POS(I) * N_BOL(I),
     :              %VAL(CNF_PVAL(IN_VARIANCE_PTR(I))), WEIGHT(I)**2,
     :              %VAL(CNF_PVAL(IN_VARIANCE_PTR(I))))
            END IF

*     Write out the data
            CALL SURF_WRITE_DATA( FD, N_POS(I) * N_BOL(I), 
     :           %VAL(CNF_PVAL(IN_DATA_PTR(I))), 
     :           %VAL(CNF_PVAL(IN_VARIANCE_PTR(I))),
     :           %VAL(CNF_PVAL(BOL_RA_PTR(I))), 
     :           %VAL(CNF_PVAL(BOL_DEC_PTR(I))),
     :           STATUS)
         END DO

*     Close the file
         CALL FIO_CANCL('FILE', STATUS)

*     Time for despiking
      ELSE IF (DESPIKE) THEN

*     Call the despiking routines here


*     Construct a N_PTS array containing the total number of data
*     points for each map. This is calculated in a different place
*     for REBIN and BOLREBIN but there doesn't seem to be much gain
*     in doing this in the shared code.
            
         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               N_PTS(I) = N_BOL(I) * N_POS(I)
            END DO
         END IF

*     First we have to put the data into bins related to position
*     Returns the 2-D array filled with pointers to the individual
*     data values in a given bin

*     Select the bit that is used for despiking

         BITNUM = 4

*     Initialise the arrays
         DO I = 1, MAX_FILE
            NSPIKES(I) = 0
         END DO

*     Bin and despike the data
         CALL SURF_GRID_DESPIKE(FILE, N_PTS, N_POS, N_BOL, BITNUM,
     :        NYQUIST, BOL_RA_PTR, BOL_DEC_PTR, 
     :        IN_DATA_PTR, IN_QUALITY_PTR, 
     :        MAP_SIZE(1), MAP_SIZE(2), I_CENTRE, J_CENTRE, NSPIKES,
     :        QBITS, STATUS)

*     Look through NSPIKES and see whether any spikes were detected
*     Report the number to the user. Do it here as I want to get the
*     list before I start asking for output files

         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               IF (NSPIKES(I) .GT. 0) THEN
               
                  CALL MSG_SETC('FILE', FILENAME(I))
                  CALL MSG_SETI('NS', NSPIKES(I))
                  CALL MSG_SETC('PKG', PACKAGE)
                  CALL MSG_OUTIF(MSG__NORM, ' ', 
     :                 '^PKG: ^NS spikes detected in file ^FILE',
     :                 STATUS)
               END IF
            END DO
         END IF

*     Now we need to write the new quality data to an output file
*     Do this by propogating everything from each input file
*     and then copying in the new quality
*     Currently the system does not support a change in the actual
*     data values (because the data is changed when using scuba sections)
*     This means that the user is asked for an output file for
*     each input file. Also means that we now have to generate
*     an output filename from the input

*     Ask whether the user simply wants to take the
*     default output filename
*     This is a kludge -- what we really want to do is create
*     a GRouP containing all the input filename and let the
*     user specify the output files as a modification on the input.
*     This can be dangerous if the input files have the same
*     root....

         CALL PAR_GET0L('DEFOUT',DEFOUT,STATUS)

         DO I = 1, FILE

*     No point writing an output file if there were none.
            IF (NSPIKES(I) .GT. 0) THEN

*     Generate a default name for the output file
               CALL SCULIB_CONSTRUCT_OUT(FILENAME(I), SUFFIX_ENV, 
     :              SCUBA__N_SUFFIX,
     :              SUFFIX_OPTIONS, SUFFIX_STRINGS, OUT, STATUS)

*     set the default
               CALL PAR_DEF0C('OUT', OUT, STATUS)

*     Get an NDF identifier to the input file
*     (could simply do a SYSTEM copy!)

               CALL NDF_FIND (DAT__ROOT, FILENAME(I), ITEMP, STATUS) 

*     OK, propagate the input ndf to the output
*     If we are using the default output name we
*     have to propogate the file ourself. Otherwise we can
*     use the parameter system.

               IF (DEFOUT) THEN

                  CALL NDF_PLACE(DAT__ROOT, OUT, PLACE, STATUS)
                  CALL NDF_SCOPY(ITEMP,
     :                 'Units,Axis,DATA,VARIANCE,Quality',
     :                 PLACE, OUT_NDF, STATUS)

                  CALL MSG_SETC('OUT',OUT)
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL MSG_OUTIF(MSG__NORM, ' ',
     :                 '^TASK: Storing despiked data in ^OUT', STATUS)

               ELSE
 
                  CALL NDF_PROP (ITEMP, 
     :                 'Units,Axis,DATA,VARIANCE,Quality',
     :                 'OUT', OUT_NDF, STATUS)

               END IF
               
*     Check status for NULL (ie didn't want to write a file)

               IF (STATUS .EQ. PAR__NULL) THEN
                  
                  CALL ERR_ANNUL(STATUS)
                  CALL NDF_ANNUL(ITEMP, STATUS)
                     
               ELSE

*     Annul the input
                  CALL NDF_ANNUL(ITEMP, STATUS)

*     Map the quality array

                  CALL NDF_MAP(OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :                 QPTR, ITEMP, STATUS)
                  
*     Copy the new quality array

                  CALL VEC_UBTOUB(.FALSE., N_POS(I) * N_BOL(I),
     :                 %VAL(CNF_PVAL(IN_QUALITY_PTR(I))), 
     :                 %VAL(CNF_PVAL(QPTR)), IERR,
     :                 NERR, STATUS)

*     Get and set the bad bits mask
                  CALL NDF_BB(OUT_NDF, BADB, STATUS)
                  BADB = SCULIB_BITON(BADB, 4)
                  CALL NDF_SBB(BADB, OUT_NDF, STATUS)
                  
*     Close output
               
                  CALL NDF_UNMAP(OUT_NDF, 'QUALITY', STATUS)
                  CALL NDF_ANNUL(OUT_NDF, STATUS)

               END IF

*     Dont cancel the parameter if this is the last time round the loop
*     This means that the HISTORY will contain some mention of this param
               IF (I .NE. FILE) CALL PAR_CANCL('OUT', STATUS)

            END IF

         END DO

*     Calculate sky contribution
      ELSE IF (CALCSKY) THEN

*     Construct a N_PTS array containing the total number of data
*     points for each map. This is calculated in a different place
*     for REBIN and BOLREBIN but there doesn't seem to be much gain
*     in doing this in the shared code. Also used in DESPIKE!!!
            
         IF (STATUS .EQ. SAI__OK) THEN
            DO I = 1, FILE
               N_PTS(I) = N_BOL(I) * N_POS(I)
            END DO
         END IF


*     Need to allocate memory for the sky calculation
*     Probably shouldnt try to open all the input NDF at once
*     since I may well run out of NDF identifiers

*     In that case allocate memory using MALLOC
         DO I = 1, FILE
            SKY_PTR(I) = 0
            SKY_END(I) = 0
            SKY_VPTR(I) = 0
            SKY_VEND(I) = 0
            CALL SCULIB_MALLOC(N_POS(I)*VAL__NBR, SKY_PTR(I), 
     :           SKY_END(I), STATUS)
            CALL SCULIB_MALLOC(N_POS(I)*VAL__NBR, SKY_VPTR(I), 
     :           SKY_VEND(I), STATUS)

         END DO

*     Need to calculate the size of the smoothing box in pixels
*     for each input file
*     Default is 2.0 seconds
         CALL PAR_DEF0R('BOXSZ', 2.0, STATUS)
         CALL PAR_GET0R('BOXSZ', BOXSZ, STATUS)
         IF (BOXSZ .LT. 0.0) BOXSZ = 0.0

*     Loop over files and convert box size in seconds to box size
*     in pixels
         DO I = 1, FILE

            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS(1), 
     :           FITS(1,I), 'EXP_TIME', RTEMP, STATUS)

*     Width of bin must be an integer (and can be 0)
            BOX_SIZE(I) = INT (BOXSZ / RTEMP)

         END DO

*     Calculate the sky contribution
         CALL SURF_GRID_CALCSKY(TSKNAME, FILE, N_PTS, N_POS, N_BOL,
     :        NYQUIST, IMNDF, N_M_FITS, MODEL_FITS,  
     :        CHOP_THROW, CHOP_PA, BOX_SIZE, BOL_RA_PTR, 
     :        BOL_DEC_PTR,  IN_DATA_PTR, IN_QUALITY_PTR, 
     :        SKY_PTR, SKY_VPTR, QBITS, STATUS)

*  close the model file
         IF (IMNDF .NE. NDF__NOID) CALL NDF_ANNUL (IMNDF, STATUS)

*     Open the input NDFs and create a new SKY NDF in the
*     REDS extension of each

         DO I = 1, FILE
*     Open for read/write access
            CALL NDF_OPEN (DAT__ROOT, FILENAME(I), 'UPDATE',
     :           'OLD', OUT_NDF, ITEMP, STATUS) 

*     Return locator to REDS extension
            IF (STATUS .EQ. SAI__OK) THEN
               CALL NDF_XLOC (OUT_NDF, 'REDS', 'UPDATE', OUT_REDSX_LOC, 
     :              STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL (STATUS)
                  CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION',
     :                 0, 0, OUT_REDSX_LOC, STATUS)
               END IF
            END IF

*     Create a 'SKY' extension (unless one already exists)
            STEMP = 'SKY'
            THERE = .FALSE.
            CALL DAT_THERE(OUT_REDSX_LOC, STEMP, THERE, STATUS)

*     Delete if it is there
            IF (THERE) CALL DAT_ERASE(OUT_REDSX_LOC, STEMP, STATUS)

*     Create a new one
            CALL NDF_PLACE (OUT_REDSX_LOC, STEMP, PLACE, 
     :           STATUS) 
            CALL NDF_NEW('_REAL', 1, 1, N_POS(I), PLACE, SKYNDF,
     :           STATUS)

*     Map the data array
            CALL NDF_MAP(SKYNDF, 'DATA', '_REAL', 'WRITE',
     :           QPTR, ITEMP, STATUS)

*     Copy the sky data in
            IF (STATUS .EQ. SAI__OK) THEN
               CALL VEC_RTOR(.FALSE., N_POS(I),
     :              %VAL(CNF_PVAL(SKY_PTR(I))),
     :              %VAL(CNF_PVAL(QPTR)), IERR, NERR, STATUS)
            END IF

*     Unmap DATA
            CALL NDF_UNMAP(SKYNDF, 'DATA', STATUS)

*     Free the sky data
            CALL SCULIB_FREE('SKY', SKY_PTR(I), SKY_END(I), STATUS)

*     Map the error array
            CALL NDF_MAP(SKYNDF, 'ERROR', '_REAL', 'WRITE',
     :           QPTR, ITEMP, STATUS)

*     Copy in error
            IF (STATUS .EQ. SAI__OK) THEN
               CALL VEC_RTOR(.FALSE., N_POS(I), 
     :              %VAL(CNF_PVAL(SKY_VPTR(I))),
     :              %VAL(CNF_PVAL(QPTR)), IERR, NERR, STATUS)
            END IF

*     Unmap variance
            CALL NDF_UNMAP(SKYNDF, '*', STATUS)

*     Free the sky error
            CALL SCULIB_FREE('SKY', SKY_VPTR(I), SKY_VEND(I), STATUS)


*     Unmap the data array and close the NDF

            CALL NDF_ANNUL(SKYNDF, STATUS)
            CALL DAT_ANNUL(OUT_REDSX_LOC, STATUS)
            CALL NDF_ANNUL(OUT_NDF, STATUS)

         END DO

      ELSE

*  get a title for the output map

      CALL PAR_DEF0C ('OUT_OBJECT', SOBJECT, STATUS)
      CALL PAR_GET0C ('OUT_OBJECT', SOBJECT, STATUS)

*     Clean up SOBJECT by removing leading blanks and non-printable chars

      CALL CHR_CLEAN(SOBJECT)
      CALL CHR_LDBLK(SOBJECT)
      
*  get the pixel spacing of the output map

      OUT_PIXEL = 3.0
      CALL PAR_DEF0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      CALL PAR_GET0R ('PIXSIZE_OUT', OUT_PIXEL, STATUS)
      OUT_PIXEL = OUT_PIXEL / REAL(R2AS)


*     Now that we have the pixel spacing we can ask the user
*     the size of the weighting function that they have requested
*     (If they are using a weighting function)

      IF (METHOD .EQ. 'BESSEL' .OR. METHOD.EQ.'LINEAR' .OR.
     :     METHOD .EQ. 'GAUSSIAN') THEN

         IF (STATUS .EQ. SAI__OK) THEN
*     Calculate my default scale length - Nyquist
*     Also maybe should include factor for the width of the filter
*     Need to convert to arcsec before changing back again
            DTEMP = R2AS * NYQUIST

*     Make it a bit smaller if we have a Gaussian regrid
*     Claire Chandler tells me that she had best results
*     with HWHM = 2.5 (850), 1.3 (450)
*     This is almost approximated by LAMBDA/(4D)
            IF (METHOD .EQ. 'GAUSSIAN') DTEMP = DTEMP / 2.5

            CALL PAR_DEF0R('SCALE', REAL(DTEMP), STATUS)

         END IF

*     Change the prompt depending on the relationship
         STEMP = ' '
         IF (METHOD .EQ. 'BESSEL') THEN
            STEMP = 'Position of first null (arcsec)'
         ELSE IF (METHOD .EQ. 'LINEAR') THEN
            STEMP = 'Radius of cone (arcsec)'
         ELSE IF (METHOD .EQ. 'GAUSSIAN') THEN
            STEMP = 'Half width half maximum of Gaussian (arcsec)'
         END IF

         CALL PAR_PROMT('SCALE',STEMP, STATUS)

*     Read the parameter
         CALL PAR_GET0R('SCALE', SCALE, STATUS)

*     Convert back to radians
         IF (STATUS .EQ. SAI__OK) SCALE = SCALE / REAL(R2AS)

*     Now need to ask for the number of scale lengths
         WEIGHTSIZE = 1

*     Linear should always have weightsize of 1
         IF (METHOD.NE.'LINEAR') THEN
            IF (METHOD .EQ.'BESSEL') THEN
               ITEMP = 10
            ELSE IF (METHOD .EQ. 'GAUSSIAN') THEN
               ITEMP = 3
            END IF
            
            CALL PAR_GDR0I('WTFNRAD',ITEMP, 1, WTFN_MAXRAD, .TRUE.,
     :           WEIGHTSIZE, STATUS)

         ENDIF

*     Now initialise the weight functions

         IF (METHOD.EQ.'BESSEL') THEN
*     Bessel
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Initialising BESSEL weighting functions',
     :           STATUS)
            CALL SCULIB_BESSEL_WTINIT(WTFN, WEIGHTSIZE, WTFNRES, STATUS)
            GUARD = .TRUE.

         ELSE IF (METHOD.EQ.'LINEAR') THEN
*     Linear
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Initialising LINEAR weighting functions',
     :           STATUS)
            CALL SCULIB_LINEAR_WTINIT(WTFN, WTFNRES, STATUS)
            GUARD = .TRUE.

         ELSE IF (METHOD.EQ.'GAUSSIAN') THEN
*     Gaussian
            CALL MSG_SETC('PKG',PACKAGE)
            CALL MSG_OUTIF(MSG__NORM, ' ', 
     :           '^PKG: Initialising GAUSSIAN weighting functions',
     :           STATUS)
            CALL SCULIB_GAUSS_WTINIT(WTFN, WEIGHTSIZE, WTFNRES, STATUS)
            GUARD = .TRUE.
         ENDIF

*     Allow my decision for the Guard ring to be overridden with a
*     parameter GUARD

         CALL PAR_DEF0L('GUARD', GUARD, STATUS)
         CALL PAR_GET0L('GUARD', GUARD, STATUS)

      END IF



*     Now I can set a default output filename for parameter 'OUT'
*     For now the default is just the first word of the object
*     name if there is more than 1 file. Else, derive from input
*     name and propogate from input NDF.

      IF (STATUS .EQ. SAI__OK) THEN

*     Derive from input name
         IF (FILE .EQ. 1) THEN

*     Generate a default name for the output file
            CALL SCULIB_CONSTRUCT_OUT(FILENAME(1), SUFFIX_ENV, 
     :           SCUBA__N_SUFFIX,
     :           SUFFIX_OPTIONS, REB_SUFFIX_STR, STEMP, STATUS)

*     Else derive from object name
         ELSE
            IPOSN = 1
            CHR_STATUS = SAI__OK
            CALL CHR_FIWE(SOBJECT, IPOSN, CHR_STATUS)

*     Good status means two words were found (at least)
*     Bad status means that the end of string was hit before finding
*     the second word

            IF (CHR_STATUS .EQ. SAI__OK) THEN
               STEMP = SOBJECT(1:IPOSN)
            ELSE
               STEMP = SOBJECT
               CALL ERR_ANNUL(CHR_STATUS)
            END IF

*     Need to make sure that this default is not just a number
*     since HDS doesnt create a file in that case

            CALL CHR_CTOR(STEMP, RTEMP, CHR_STATUS)

*     Good status implies the wrong answer :-)

            IF (CHR_STATUS .EQ. SAI__OK) THEN

*     Prepend a character if the object name is a number
               CALL CHR_PREFX('i', STEMP, ITEMP)

            ELSE
               CALL ERR_ANNUL(CHR_STATUS)
            END IF

         END IF

      END IF

*     Set the default

      CALL PAR_DEF0C('OUT', STEMP, STATUS)

      OUT_LOC = DAT__NOLOC ! Assign to null value

      IF (BOLREBIN .OR. INTREBIN) THEN
*  create the output file that will contain the reduced data in NDFs
*     The 'type' depends on BOL or INT rebin
 
         CALL PAR_GET0C ('OUT', OUT, STATUS)

         IF (BOLREBIN) THEN
            STEMP = 'SURF_BOLMAPS'
         ELSE
            STEMP = 'SURF_INTMAPS'
         END IF

         HDSNAME = OUT(1:DAT__SZNAM)
         CALL HDS_NEW (OUT, HDSNAME, STEMP, 0, 0, OUT_LOC, 
     :        STATUS)

      END IF

*  Now want to have full REBIN and looped BOLREBIN in same code

      IF (BOLREBIN) THEN
         TOTAL_BOLS = N_BOL(1)
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_SETI('NB', TOTAL_BOLS)
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        '^PKG: Processing ^NB bolometers', STATUS)
      ELSE
         TOTAL_BOLS = 1
      END IF



*     Start looping on each bolometer
      IF (STATUS .EQ. SAI__OK) THEN
         DO EACHBOL = 1, TOTAL_BOLS

            IF (BOLREBIN) THEN
*     Find bolometer name
               CALL SCULIB_BOLNAME(BOL_ADC(EACHBOL), BOL_CHAN(EACHBOL), 
     :              MAPNAME, STATUS)

               CALL MSG_SETC('BOL', MAPNAME)
               CALL MSG_OUTIF(MSG__NORM, ' ',
     :              ' Processing bolometer: ^BOL', STATUS)

               DO I = 1, FILE

                  N_PTS(I) = N_POS(I)
*     Initialise pointers
                  IF (STATUS .EQ. SAI__OK) THEN
                     ABOL_DATA_PTR(I) = 0
                     ABOL_DATA_END(I) = 0
                     ABOL_VAR_PTR(I) = 0
                     ABOL_VAR_END(I) = 0
                     ABOL_RA_PTR(I) = 0
                     ABOL_RA_END(I) = 0
                     ABOL_DEC_PTR(I) = 0
                     ABOL_DEC_END(I) = 0
                  END IF

*     Extract EACHBOL from each file
*     Need IN_DATA_PTR, BOL_RA_PTR and BOL_DEC_PTR
*     Get the memory
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, 
     :                 ABOL_DATA_PTR(I), ABOL_DATA_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, 
     :                 ABOL_VAR_PTR(I), ABOL_VAR_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBD, 
     :                 ABOL_RA_PTR(I), ABOL_RA_END(I), STATUS)
                  CALL SCULIB_MALLOC(N_POS(I) * VAL__NBD, 
     :                 ABOL_DEC_PTR(I), ABOL_DEC_END(I), STATUS)

*     Extract a bolometer
                  CALL SCULIB_EXTRACT_2DIM_R(EACHBOL, N_BOL(I),N_POS(I),
     :                 %VAL(CNF_PVAL(IN_DATA_PTR(I))), 
     :                 %VAL(CNF_PVAL(ABOL_DATA_PTR(I))),
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_R(EACHBOL, N_BOL(I),N_POS(I),
     :                 %VAL(CNF_PVAL(IN_VARIANCE_PTR(I))), 
     :                 %VAL(CNF_PVAL(ABOL_VAR_PTR(I))),
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_D(EACHBOL, N_BOL(I),N_POS(I),
     :                 %VAL(CNF_PVAL(BOL_RA_PTR(I))), 
     :                 %VAL(CNF_PVAL(ABOL_RA_PTR(I))),
     :                 STATUS)
                  CALL SCULIB_EXTRACT_2DIM_D(EACHBOL, N_BOL(I),N_POS(I),
     :                 %VAL(CNF_PVAL(BOL_DEC_PTR(I))), 
     :                 %VAL(CNF_PVAL(ABOL_DEC_PTR(I))),
     :                 STATUS)
               END DO
            ELSE
               DO I = 1, FILE
                  N_PTS(I) = N_POS(I) * N_BOL(I)
*     Just need to copy pointer in this case
                  ABOL_DATA_PTR(I)= IN_DATA_PTR(I)
                  ABOL_VAR_PTR(I) = IN_VARIANCE_PTR(I)
                  ABOL_DEC_PTR(I) = BOL_DEC_PTR(I)
                  ABOL_RA_PTR(I)  = BOL_RA_PTR(I)
               END DO
            END IF


*     find the extent of the input data

            CALL SURFLIB_CALC_OUTPUT_GRID(FILE, N_PTS, OUT_PIXEL,
     :           ABOL_RA_PTR, ABOL_DEC_PTR, MAP_SIZE(1), MAP_SIZE(2),
     :           I_CENTRE, J_CENTRE, STATUS)

*     Ask for confirmation of the map size

            IX = MAP_SIZE(1)
            IY = MAP_SIZE(2)

            CALL PAR_GDR1I('SIZE', 2, MAP_SIZE, 5, 10000, .TRUE.,
     :           MAP_SIZE, STATUS)

*     Need to redefine I_CENTRE and J_CENTRE 
*     Put the reference pixels into the centre of the new grid
*     if the size has changed

            IF (MAP_SIZE(1) .NE. IX) THEN
               I_CENTRE = INT(MAP_SIZE(1) / 2)
            END IF

            IF (MAP_SIZE(2) .NE. IY) THEN
               J_CENTRE = INT(MAP_SIZE(2) / 2)
            END IF

            N_PIXELS = MAP_SIZE(1) * MAP_SIZE(2)

*     Allow for the reference pixel to be redefined - this is required
*     since sometimes the required size is equal to the actual size
*     but we *want* the ref pixel to be in the middle of the map 
*     (eg for scan map reduction) - provide the actual value as the
*     default modified to the centre if the size was different

*     Subvert UBND for now
            UBND(1) = I_CENTRE
            UBND(2) = J_CENTRE
            LBND(1) = 1
            LBND(2) = 1

*     Set the default value and ask for the range
*     Note that we do not constrain the allowed range since the
*     reference pixel does not have to lie on the regridded image.
            CALL PAR_DEF1I('REFPIX', 2, UBND, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               CALL PAR_EXACI('REFPIX', 2, UBND, STATUS)

*     Accept the defaults if PAR__NULL is returned
               IF (STATUS .EQ. PAR__NULL) THEN
                  UBND(1) = I_CENTRE
                  UBND(2) = J_CENTRE
                  CALL ERR_ANNUL(STATUS)
               END IF

            END IF

*     Copy the result back to I, J
            I_CENTRE = UBND(1)
            J_CENTRE = UBND(2)

*     Now that the size of the map is determined we have to work out whether
*     we are rebinning all integrations into one image (REBIN/BOLREBIN) or 
*     into separate images (INTREBIN).

            IF (INTREBIN) THEN
*     INTREBIN needs to loop through each integraion
               N_FILE_LOOPS = FILE
               DO I = 1, FILE
                  TOT(I) = N_INTS(I)
               END DO

               NFILES = 1       ! All lower subroutines assume one input file

               NINTS(1) = 1     ! All lower subs assume one integration
               
            ELSE
*     Normal rebin just needs to loop once
               N_FILE_LOOPS = 1
               DO I = 1, FILE
                  TOT(I) = 1
               END DO

               NFILES = FILE    ! Lower subroutines see all FILES

               DO I = 1, FILE   ! Lower subs see all integrations
                  NINTS(I)  = N_INTS(I)
               END DO
            END IF

*     Start looping over integrations if necessary
            COUNT = 0

            DO CURR_FILE = 1, N_FILE_LOOPS

*     Report some info for INTREBIN
               IF (INTREBIN) THEN
                  CALL MSG_SETC('PKG', PACKAGE)
                  CALL MSG_SETC('FILE', FILENAME(CURR_FILE))
                  CALL MSG_OUTIF(MSG__NORM, ' ','^PKG: Processing '//
     :                 'file ^FILE', STATUS)
               END IF

               DO CURR_INT = 1, TOT(CURR_FILE)

                  IF (INTREBIN) THEN

                     COUNT = COUNT + 1 ! How many ints from all files
*     Need to loop through INT_LIST retrieving section
                     INT_START = INT_LIST(CURR_FILE, CURR_INT)
                     INT_NEXT  = INT_LIST(CURR_FILE, CURR_INT + 1)
                     N_PTS(1)  = (INT_NEXT - INT_START) * 
     :                    N_BOL(CURR_FILE)

*     Modify INT_LIST so that the first integration in INT_LIST
*     actually points to the current integration. This is needed
*     for SPLINE fitting which calculates an integration at a time
*     and then coadds. NINTS has already been set to 1

                     INT_LIST(CURR_FILE,1) = 1
                     INT_LIST(CURR_FILE,2) = INT_NEXT - INT_START + 1

*     Force the weight to be 1 in INTREBIN regardless of what
*     was specified
                     WEIGHT(1) = 1.0

*     Print some information concerning the regrid
                     CALL MSG_SETC('PKG', PACKAGE)
                     CALL MSG_SETI('INT', COUNT)
                     CALL MSG_OUTIF(MSG__NORM, ' ',' --^PKG: '//
     :                    'Processing integration ^INT', STATUS)

*     Now set up the pointers for the start of each integration
*     We need to be careful because these pointers will not initially
*     be registered with CNF
                     OFFSET = (INT_START-1) * N_BOL(CURR_FILE)
                     ROFFSET = OFFSET * VAL__NBR
                     DOFFSET = OFFSET * VAL__NBD

                     IF (STATUS .EQ. SAI__OK) THEN
                        IF (OFFSET .GT. 0) THEN

                           ABOL_DATA_PTR(1) = CNF_PREG( 
     :                          CNF_PVAL(IN_DATA_PTR(CURR_FILE)) + 
     :                          ROFFSET, ISNEWPD)
                           ABOL_VAR_PTR(1) = CNF_PREG(
     :                          CNF_PVAL(IN_VARIANCE_PTR(CURR_FILE)) +
     :                          ROFFSET, ISNEWPV)
                           ABOL_RA_PTR(1) = CNF_PREG(
     :                          CNF_PVAL(BOL_RA_PTR(CURR_FILE)) +
     :                          DOFFSET, ISNEWPR)
                           ABOL_DEC_PTR(1) = CNF_PREG(
     :                          CNF_PVAL(BOL_DEc_PTR(CURR_FILE)) +
     :                          DOFFSET, ISNEWPC)

                           IF (ABOL_DATA_PTR(1) .EQ. 0 .OR.
     :                          ABOL_VAR_PTR(1) .EQ. 0 .OR.
     :                          ABOL_RA_PTR(1) .EQ. 0 .OR.
     :                          ABOL_DEC_PTR(1) .EQ. 0 ) THEN

                              STATUS = SAI__ERROR
                              CALL MSG_SETC( 'PKG', PACKAGE )
                              CALL ERR_REP( ' ','^PKG: Error '//
     :                             'registering pointers with '//
     :                             'CNF (internal error)', 
     :                             STATUS)

                           END IF

                        ELSE
*     first time through. Make sure the pointer is correct (it should be)
                           ABOL_DATA_PTR(1) = IN_DATA_PTR(CURR_FILE) 
                           ABOL_VAR_PTR(1) = IN_VARIANCE_PTR(CURR_FILE)
                           ABOL_RA_PTR(1) = BOL_RA_PTR(CURR_FILE)
                           ABOL_DEC_PTR(1) = BOL_DEC_PTR(CURR_FILE)

                           ISNEWPD = .FALSE.
                           ISNEWPV = .FALSE.
                           ISNEWPR = .FALSE.
                           ISNEWPC = .FALSE.

                        END IF
                     END IF

                     
*     Work out mapname
                     MAPNAME = 'I'
                     CALL CHR_ITOC(COUNT, STEMP, ITEMP)
                     CALL CHR_APPND(STEMP, MAPNAME, CHR_LEN(MAPNAME))

                  END IF


*     OK, create the output file, map the arrays
*     Make sure that the bounds are such that the 0,0 pixel
*     is the reference pixel

                  LBND (1) = 1 - I_CENTRE
                  LBND (2) = 1 - J_CENTRE
                  UBND (1) = MAP_SIZE(1) - I_CENTRE
                  UBND (2) = MAP_SIZE(2) - J_CENTRE

                  IF (BOLREBIN.OR.INTREBIN) THEN
                     CALL NDF_PLACE(OUT_LOC, MAPNAME, PLACE, STATUS)
                     CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, 
     :                    OUT_NDF, STATUS)
                  ELSE

*     If we have a single input file and a single output file
*     (ie not coadding multiple observations and not generating
*     images a la BOLREBIN/INTREBIN) then we should propogate from
*     the input file so that the HISTORY is retained

                     IF (FILE .EQ. 1) THEN
*     Open the input file and get a section the right size
                        CALL NDF_FIND (DAT__ROOT, FILENAME(1), ITEMP, 
     :                       STATUS) 
                        CALL NDF_SECT(ITEMP, 2, LBND, UBND,
     :                       SECNDF, STATUS)
*     Ask for the new file name - NULL response indicates 
*     that we should only calculate the map size
                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL NDF_PROP(SECNDF,
     :                       'NOEXTENSION(SCUCD,FIGARO,SCUBA,REDS,FITS)'
     :                        ,'OUT',OUT_NDF, STATUS)
                           IF (STATUS .EQ. PAR__NULL) DOREBIN = .FALSE.
                        END IF
                        CALL NDF_ANNUL(SECNDF, STATUS)
                        CALL NDF_ANNUL(ITEMP, STATUS)

                     ELSE
*     Multiple input images so just create a file via the parameter
*     system

                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL NDF_CREAT ('OUT', '_REAL', 2, LBND,UBND,
     :                          OUT_NDF, STATUS)
                           IF (STATUS .EQ. PAR__NULL) DOREBIN = .FALSE.
                        END IF

                     END IF
                  END IF

*     There will be bad pixels in the output map.

                  CALL NDF_SBAD (.TRUE., OUT_NDF, 'Data,Variance', 
     :                 STATUS)
                  CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

*     Create HISTORY
                  CALL NDF_HCRE(OUT_NDF, STATUS)


*     Get memory for the output grid
                  OUT_DATA_PTR = 0
                  OUT_DATA_END = 0
                  OUT_VARIANCE_PTR = 0
                  OUT_VARIANCE_END = 0
                  OUT_QUALITY_PTR = 0
                  OUT_QUALITY_END = 0
                  CALL SCULIB_MALLOC(N_PIXELS * VAL__NBR,
     :                 OUT_DATA_PTR, OUT_DATA_END, STATUS)
                  CALL SCULIB_MALLOC(N_PIXELS * VAL__NBR,
     :                 OUT_VARIANCE_PTR, OUT_VARIANCE_END, STATUS)
                  CALL SCULIB_MALLOC(N_PIXELS * VAL__NBUB,
     :                 OUT_QUALITY_PTR, OUT_QUALITY_END, STATUS)

*     Fill arrays with bad data and zeroes

                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_CFILLB (N_PIXELS, BADBIT,
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)))
                     CALL SCULIB_CFILLR(N_PIXELS, 0.0, 
     :                    %VAL(CNF_PVAL(OUT_DATA_PTR)))
                     CALL SCULIB_CFILLR(N_PIXELS, 0.0, 
     :                    %VAL(CNF_PVAL(OUT_VARIANCE_PTR)))
                  END IF


*     Get some memory for the weights array and fill with zeroes
                  OUT_WEIGHT_PTR = 0
                  OUT_WEIGHT_END = 0
                  CALL SCULIB_MALLOC(N_PIXELS * VAL__NBR,
     :                 OUT_WEIGHT_PTR, OUT_WEIGHT_END, STATUS)
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_CFILLR (N_PIXELS, 0.0, 
     :                    %VAL(CNF_PVAL(OUT_WEIGHT_PTR)))
                  END IF


*     Now time for regrid
                  IF ((METHOD .EQ. 'BESSEL') .OR. 
     :                 (METHOD .EQ. 'LINEAR') .OR.
     :                 (METHOD .EQ. 'GAUSSIAN')) THEN

*     Rebin the data using weighting function
                     CALL SCULIB_WTFN_REGRID( GUARD, NFILES, N_PTS,
     :                    WTFN_MAXRAD, WTFNRES, WEIGHTSIZE, SCALE, 
     :                    OUT_PIXEL, MAP_SIZE(1), 
     :                    MAP_SIZE(2), I_CENTRE, J_CENTRE, WTFN, WEIGHT,
     :                    BOLWT, N_BOL, SCUBA__NUM_ADC*SCUBA__NUM_CHAN,
     :                    ABOL_DATA_PTR, ABOL_VAR_PTR, 
     :                    ABOL_RA_PTR, ABOL_DEC_PTR, 
     :                    %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :                    %VAL(CNF_PVAL(OUT_VARIANCE_PTR)), 
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)),
     :                    %VAL(CNF_PVAL(OUT_WEIGHT_PTR)), STATUS )

                  ELSE IF (METHOD .EQ. 'MEDIAN') THEN

                     CALL SURFLIB_MEDIAN_REGRID(NFILES, N_PTS,
     :                    OUT_PIXEL, MAP_SIZE(1), MAP_SIZE(2),
     :                    I_CENTRE, J_CENTRE, 
     :                    ABOL_RA_PTR, ABOL_DEC_PTR,
     :                    ABOL_DATA_PTR,
     :                    %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :                    %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)), STATUS)

                  ELSE IF (METHOD(1:6) .EQ. 'SPLINE') THEN

                     IF (METHOD .EQ. 'SPLINE1') THEN
                        SPMETHOD = 'IDBVIP'
                     ELSE IF (METHOD .EQ. 'SPLINE2') THEN
                        SPMETHOD = 'SURFIT'
                        CALL PAR_DEF0R('SFACTOR',
     :                       N_PIXELS / 2.0,
     :                       STATUS)
                        CALL PAR_GET0R('SFACTOR', SFACTOR, STATUS)
                     ELSE 
                        SPMETHOD = 'IDSFFT'
                     END IF

*     Regrid with SPLINE interpolation

*     Note that for INTREBIN we need to supply a different
*     N_INTS and INT_LIST

*     The effective radius of influence that each data point
*     contributes to the final map. This controls how filled the final
*     spline filled image will be for sparse data sets
*     Assume nyquist or half a pixel, whichever is larger
                     EFF_RADIUS = REAL(NYQUIST)
                     IF (2*EFF_RADIUS .LT. OUT_PIXEL) THEN
                        EFF_RADIUS = OUT_PIXEL / 2.0
                     END IF

                     CALL SCULIB_SPLINE_REGRID(SPMETHOD, SFACTOR, 
     :                    MAX_FILE, NFILES, N_BOL, SCUBA__MAX_INT, 
     :                    NINTS, EFF_RADIUS, OUT_PIXEL,
     :                    MAP_SIZE(1), MAP_SIZE(2),
     :                    I_CENTRE, J_CENTRE, WEIGHT, INT_LIST, 
     :                    ABOL_DATA_PTR, ABOL_VAR_PTR, ABOL_RA_PTR, 
     :                    ABOL_DEC_PTR, %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :                    %VAL(CNF_PVAL(OUT_VARIANCE_PTR)), 
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)),
     :                    %VAL(CNF_PVAL(OUT_WEIGHT_PTR)), STATUS )

                  ELSE

*     This shouldnt happen
                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC('TASK', TSKNAME)
                        CALL ERR_REP(' ','^TASK: Unknown regrid method',
     :                       STATUS)

                     END IF
                  END IF

*     Copy the data to the output NDF

                  CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE', 
     :                 NDF_PTR, ITEMP, STATUS)
                  CALL VEC_RTOR(.FALSE., N_PIXELS,
     :                 %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :                 %VAL(CNF_PVAL(NDF_PTR)), IERR,
     :                 NERR, STATUS)
                  CALL NDF_UNMAP(OUT_NDF, 'DATA', STATUS)

                  CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 
     :                 'WRITE', NDF_PTR, ITEMP, STATUS)
                  CALL VEC_RTOR(.FALSE., N_PIXELS,
     :                 %VAL(CNF_PVAL(OUT_VARIANCE_PTR)), 
     :                 %VAL(CNF_PVAL(NDF_PTR)), IERR,
     :                 NERR, STATUS)
                  CALL NDF_UNMAP(OUT_NDF, 'VARIANCE', STATUS)

*     Trim the edges if required - the edge trimming routine
*     simply needs the quality array since that will contain a 1
*     wherever there is a bad pixel. Just set bit 2 for nearby pixels

                  CALL PAR_GET0R('TRIM', TRIM, STATUS)

                  CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :                 NDF_PTR, ITEMP, STATUS)

*     Convert to pixels
                  TRIM = TRIM / (OUT_PIXEL * REAL(R2AS))

*     No point trimming if we have less than 0.5 a pixel
                  IF ( TRIM .GT. 0.5) THEN

*     Trim the image - set the second bit
                     CALL SURFLIB_TRIM_IMAGE( TRIM, MAP_SIZE(1),
     :                    MAP_SIZE(2), 1, 
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)),
     :                    %VAL(CNF_PVAL(NDF_PTR)), STATUS)

                  ELSE

                     CALL VEC_UBTOUB(.FALSE., N_PIXELS,
     :                    %VAL(CNF_PVAL(OUT_QUALITY_PTR)), 
     :                    %VAL(CNF_PVAL(NDF_PTR)), IERR,
     :                    NERR, STATUS)

                  END IF


                  CALL NDF_UNMAP(OUT_NDF, 'QUALITY', STATUS)

*     Free the memory for the output grid
                  CALL SCULIB_FREE('OUT_DATA', OUT_DATA_PTR,
     :                 OUT_DATA_END, STATUS)
                  CALL SCULIB_FREE('OUT_VARIANCE', OUT_VARIANCE_PTR,
     :                 OUT_VARIANCE_END, STATUS)
                  CALL SCULIB_FREE('OUT_QUALITY', OUT_QUALITY_PTR,
     :                 OUT_QUALITY_END, STATUS)

*     and a title

                  OUT_TITLE = SOBJECT
                  IF (BOLREBIN .OR. INTREBIN) THEN
                     IPOSN = CHR_LEN(OUT_TITLE)
                     CALL CHR_APPND('_', OUT_TITLE, IPOSN)
                     CALL CHR_APPND(MAPNAME, OUT_TITLE, IPOSN)
                  END IF

*     Extract FILT_1 from the first FITS header so that we can write
*     the FILTER name to the output FITS

                  CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS(1), 
     :                 FITS(1,1), 'FILT_1', STEMP, STATUS)

*     Extract units from first fits array
                  CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS(1),
     :                 FITS(1,1), 'NDFUNITS', OUT_UNITS, STATUS)

*     Extract the instrument name from the first fits array
                  CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS(1),
     :                 FITS(1,1), 'INSTRUME', INSTRUMENT, STATUS)

*     Retrieve the waveplate and rotation angles
*     Can always do this since they will be bad if there was no
*     polarimetry.
                  WPLATE = ANG_INT(CURR_FILE, CURR_INT,1)
                  ANGROT = ANG_INT(CURR_FILE, CURR_INT,2)
                  
*     Now write the axis and FITS header

                  CALL SURF_WRITE_MAP_INFO (OUT_NDF, OUT_COORDS, 
     :                 OUT_TITLE,OUT_UNITS,MJD_STANDARD, FILE, FILENAME,
     :                 OUT_LONG, OUT_LAT, OUT_PIXEL, I_CENTRE, J_CENTRE,
     :                 MAP_SIZE(1), MAP_SIZE(2), WAVELENGTH, STEMP,
     :                 OKAY, CHOP_CRD, CHOP_PA, CHOP_THROW, 
     :                 WPLATE, ANGROT, TELESCOPE, INSTRUMENT, FITSCHAN,
     :                 STATUS )
                  

*     Create a REDS extension
                  CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION',
     :                 0, 0, OUT_REDSX_LOC, STATUS)

                  CALL PAR_GET0L('WEIGHTS', WEIGHTS, STATUS)

*     Also create and map associated NDF of weights for each pixel
*     as an NDF in the REDS extension if requested.

                  IF (WEIGHTS) THEN

                     CALL NDF_PLACE (OUT_REDSX_LOC, 'WEIGHTS', PLACE, 
     :                    STATUS) 
                     CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, 
     :                    OUT_WEIGHT_NDF, STATUS)
                     
                     CALL NDF_MAP (OUT_WEIGHT_NDF, 'DATA', '_REAL', 
     :                    'WRITE/ZERO', NDF_PTR, ITEMP, STATUS)

*     Copy the weights into the output NDF

                     CALL VEC_RTOR(.FALSE., N_PIXELS,
     :                    %VAL(CNF_PVAL(OUT_WEIGHT_PTR)), 
     :                    %VAL(CNF_PVAL(NDF_PTR)), IERR,
     :                    NERR, STATUS)

*     Unmap the weights NDF
                     CALL NDF_UNMAP(OUT_WEIGHT_NDF, '*', STATUS)
                     CALL NDF_ANNUL(OUT_WEIGHT_NDF, STATUS)

                  END IF

*     Free the weights array (this has to happen regardless
*     of whether we stored the weights or not.
                     CALL SCULIB_FREE('WEIGHTS', OUT_WEIGHT_PTR,
     :                    OUT_WEIGHT_END, STATUS)

*     Now store the TIMEs array if required
                  CALL PAR_GET0L('TIMES', TIMES, STATUS)

                  IF (TIMES) THEN
*     Already have a REDS extension so...

                     CALL NDF_PLACE (OUT_REDSX_LOC, 'TIMES', PLACE, 
     :                    STATUS) 
                     CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, 
     :                    OUT_WEIGHT_NDF, STATUS)

*     Map the data
                     CALL NDF_MAP(OUT_WEIGHT_NDF, 'DATA', '_INTEGER', 
     :                 'WRITE/ZERO', NDF_PTR, ITEMP, STATUS)

                     
                     OUT_QUALITY_PTR = 0
                     OUT_QUALITY_END = 0
                     CALL SCULIB_MALLOC((N_PIXELS * VAL__NBUB),
     :                    OUT_QUALITY_PTR, OUT_QUALITY_END,
     :                    STATUS)

*     Fill the quality array with zeroes
                     BADB = 0
                     IF (STATUS .EQ. SAI__OK) THEN
                        CALL SCULIB_CFILLB (N_PIXELS, BADB,
     :                       %VAL(CNF_PVAL(OUT_QUALITY_PTR)))
                     END IF

                     DO I = 1, FILE
                        IJ_PTR = 0
                        IJ_PTR_END = 0
                        CALL SCULIB_MALLOC(( 2 * N_PTS(I) * VAL__NBI),
     :                       IJ_PTR, IJ_PTR_END, STATUS)

                        CALL SURFLIB_CALC_IJPOS(N_PTS(I), 
     :                       DBLE(OUT_PIXEL), I_CENTRE, J_CENTRE,
     :                       %VAL(CNF_PVAL(ABOL_RA_PTR(I))),
     :                       %VAL(CNF_PVAL(ABOL_DEC_PTR(I))),
     :                       %VAL(CNF_PVAL(IJ_PTR)), STATUS)

                        CALL SURFLIB_HISTOGRAM_GRID(N_PTS(I),
     :                       MAP_SIZE(1), MAP_SIZE(2), .TRUE.,
     :                       %VAL(CNF_PVAL(ABOL_DATA_PTR(I))),
     :                       %VAL(CNF_PVAL(OUT_QUALITY_PTR)), BADB,
     :                       %VAL(CNF_PVAL(IJ_PTR)), 
     :                       %VAL(CNF_PVAL(NDF_PTR)), ITEMP,
     :                       ITEMP, ITEMP, STATUS)

                        CALL SCULIB_FREE('IJs',IJ_PTR, IJ_PTR_END,
     :                       STATUS)
                     END DO

*     Free everything
                     CALL SCULIB_FREE('DummyQual', OUT_QUALITY_PTR,
     :                    OUT_QUALITY_END, STATUS)
                     CALL NDF_UNMAP(OUT_WEIGHT_NDF, 'DATA', STATUS)
                     CALL NDF_ANNUL(OUT_WEIGHT_NDF, STATUS)

                  END IF

*     Annul the output NDF and locator
                  CALL DAT_ANNUL(OUT_REDSX_LOC, STATUS)
                  CALL NDF_ANNUL(OUT_NDF, STATUS)

*     Tidy up each loop

                  IF (BOLREBIN) THEN
                     DO I = 1, FILE
                        CALL SCULIB_FREE('BOL_DATA', ABOL_DATA_PTR(I),
     :                       ABOL_DATA_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_VAR', ABOL_VAR_PTR(I),
     :                       ABOL_VAR_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_RA', ABOL_RA_PTR(I),
     :                       ABOL_RA_END(I), STATUS)
                        CALL SCULIB_FREE('BOL_DEC', ABOL_DEC_PTR(I),
     :                       ABOL_DEC_END(I), STATUS)
                     END DO
                  END IF

*     unregister left over pointers
                  IF (INTREBIN) THEN
                     IF (ISNEWPD) CALL CNF_UNREGP( ABOL_DATA_PTR(1) )
                     IF (ISNEWPV) CALL CNF_UNREGP( ABOL_VAR_PTR(1) )
                     IF (ISNEWPR) CALL CNF_UNREGP( ABOL_RA_PTR(1) )
                     IF (ISNEWPC) CALL CNF_UNREGP( ABOL_DEC_PTR(1) )
                  END IF

               END DO
            END DO
            
         END DO
      END IF

*     Close the HDS container file if one was opened
      IF (OUT_LOC .NE. DAT__NOLOC) CALL DAT_ANNUL(OUT_LOC, STATUS)

*     This is the end of the BOLOMETER looping

      END IF   ! This is the end of the EXTRACT_DATA bypass

*     If we are running rebin and finish with DOREBIN=FALSE
*     and STATUS=PAR__ANNUL, then this indicates that we are simply
*     running REBIN to determine the map size and not to regrid

      IF (TSKNAME .EQ. 'REBIN' .AND. .NOT.DOREBIN .AND. 
     :     STATUS .EQ. PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
         CALL MSG_SETC('TSK',TSKNAME)
         CALL MSG_OUTIF(MSG__NORM, ' ','^TSK: Null response to'//
     :        ' parameter OUT - not regridding', STATUS)
      END IF

*  now finish off

      DO I = 1, FILE
         CALL SCULIB_FREE ('IN_DATA', IN_DATA_PTR(I),
     :        IN_DATA_END(I), STATUS)
         CALL SCULIB_FREE ('IN_VARIANCE', IN_VARIANCE_PTR(I),
     :        IN_VARIANCE_END(I), STATUS)
         CALL SCULIB_FREE ('BOL_RA', BOL_RA_PTR(I),
     :        BOL_RA_END(I), STATUS)
         CALL SCULIB_FREE ('BOL_DEC', BOL_DEC_PTR(I),
     :        BOL_DEC_END(I), STATUS)

         IF (.NOT.QMF) THEN
            CALL SCULIB_FREE ('IN_QUALITY', IN_QUALITY_PTR(I),
     :           IN_QUALITY_END(I), STATUS)
         END IF

      END DO

      CALL NDF_END (STATUS)

      CALL AST_END(STATUS)

      END
