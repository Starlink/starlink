      SUBROUTINE KAPPA_MON( STATUS )
*+
*  Name:
*     KAPPA_MON

*  Purpose:
*     Top-level KAPPA subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAPPA_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the KAPPA
*     suite of A-tasks.  Each KAPPA command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 June 19 (MJC):
*        Original based upon the KAPPA_PM monolith.
*     1995 August 7 (MJC):
*        Set the application name for the NDF HISTORY.
*     1995 August 19 (MJC):
*        Renamed from KAPPA_PM as it is now a proper monolith.
*        Removed applications now in separate monoliths.
*     1997 May 31 (MJC):
*        Added KSTEST and SUBSTITUTE for V0.10.
*     7-OCT-1998 (DSB):
*        Added COPYBAD PROFILE LISTSHOW LISTMAKE WCSALIGN WCSTRAN for V0.13
*     13-MAY-1999 (DSB):
*        Changed history application name to incorporate the current version
*        of KAPPA.
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'          ! SSE global definitions

*  Status:
      INTEGER  STATUS

*  Local Variables:
      CHARACTER NAME * ( 15 )     ! Task name from the command

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from the environment.  This returns uppercase
*  names.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Identify and execute the task.
*  ==============================
*
*  Define the current application name for history. The package version
*  number gets substituted in here when the KAPPA release source tar file 
*  is contructed.
      CALL NDF_HAPPN( NAME // ' (KAPPA "PKG_VERS")', STATUS )

*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

*  Adds two NDF data structures.
      IF ( NAME .EQ. 'ADD' ) THEN
         CALL ADD ( STATUS )

*  Derives statistics of pixels within a specified circle of a 2-d
*  data array.
      ELSE IF ( NAME .EQ. 'APERADD' ) THEN
         CALL APERADD ( STATUS )

*  Creates a text file describing selected regions of an image.
      ELSE IF ( NAME .EQ. 'ARDGEN' ) THEN
         CALL ARDGEN ( STATUS )

*  Uses an ARD file to set some pixels of an NDF to be bad.
      ELSE IF ( NAME .EQ. 'ARDMASK' ) THEN
         CALL ARDMASK ( STATUS )

*  Smooths a 1- or 2-dimensional image using a square or rectangular
*  box filter.
      ELSE IF ( NAME .EQ. 'BLOCK' ) THEN
         CALL BLOCK ( STATUS )

*  Adds a scalar to an NDF data structure.
      ELSE IF ( NAME .EQ. 'CADD' ) THEN
         CALL CADD ( STATUS )

*  Evaluates a mathematical expression.
      ELSE IF ( NAME .EQ. 'CALC' ) THEN
         CALL CALC ( STATUS )

*  Calculates polarisation parameters.
      ELSE IF ( NAME .EQ. 'CALPOL' ) THEN
         CALL CALPOL ( STATUS )

*  Divides an NDF by a scalar.
      ELSE IF ( NAME .EQ. 'CDIV' ) THEN
         CALL CDIV ( STATUS )

*  Finds the centroids of star-like features in an NDF.
      ELSE IF ( NAME .EQ. 'CENTROID' ) THEN
         CALL CENTROID ( STATUS )

*  Replaces the values of selected pixels in an NDF.
      ELSE IF ( NAME .EQ. 'CHPIX' ) THEN
         CALL CHPIX ( STATUS )

*  Multiplies an NDF by a scalar.
      ELSE IF ( NAME .EQ. 'CMULT' ) THEN
         CALL CMULT ( STATUS )

*  Reduces the size of an NDF by adding values in rectangular boxes.
      ELSE IF ( NAME .EQ. 'COMPADD' ) THEN
         CALL COMPADD ( STATUS )

*  Reduces the size of an NDF by averaging values in rectangular boxes.
      ELSE IF ( NAME .EQ. 'COMPAVE' ) THEN
         CALL COMPAVE ( STATUS )

*  Reduces the size of an NDF by picking equally spaced pixels.
      ELSE IF ( NAME .EQ. 'COMPICK' ) THEN
         CALL COMPICK ( STATUS )

*  Convolves a pair of 1- or 2-dimensional NDFs together.
      ELSE IF ( NAME .EQ. 'CONVOLVE' ) THEN
         CALL CONVOLVE ( STATUS )

*  Copies bad pixels from one NDF to another.
      ELSE IF ( NAME .EQ. 'COPYBAD' ) THEN
         CALL COPYBAD ( STATUS )

*  Generate a test 2-d data array from a selection of several types.
      ELSE IF ( NAME .EQ. 'CREFRAME' ) THEN
         CALL CREFRAME ( STATUS )

*  Subtracts a scalar from an NDF data structure.
      ELSE IF ( NAME .EQ. 'CSUB' ) THEN
         CALL CSUB ( STATUS )

*  Divides one NDF data structure by another.
      ELSE IF ( NAME .EQ. 'DIV' ) THEN
         CALL KAP_DIV( STATUS )

*  Removes pixels with large errors from an NDF.
      ELSE IF ( NAME .EQ. 'ERRCLIP' ) THEN
         CALL ERRCLIP ( STATUS )

*  Takes the base-10 exponential of each pixel of a data array.
      ELSE IF ( NAME .EQ. 'EXP10' ) THEN
         CALL EXP10 ( STATUS )

*  Takes the exponential of each pixel of a data array (base e).
      ELSE IF ( NAME .EQ. 'EXPE' ) THEN
         CALL EXPE ( STATUS )

*  Takes the exponential of each pixel of a data array (specified base).
      ELSE IF ( NAME .EQ. 'EXPON' ) THEN
         CALL EXPON ( STATUS )

*  Removes defects from a substantially flat 2-dimensional NDF.
      ELSE IF ( NAME .EQ. 'FFCLEAN' ) THEN
         CALL FFCLEAN ( STATUS )

*  Removes regions of bad values from a 2-dimensional NDF.
      ELSE IF ( NAME .EQ. 'FILLBAD' ) THEN
         CALL FILLBAD ( STATUS )

*  Reverses an NDF's pixels along a specified dimension.
      ELSE IF ( NAME .EQ. 'FITSURFACE' ) THEN
         CALL FITSURFACE ( STATUS )

*  Reverses an NDF's pixels along a specified dimension.
      ELSE IF ( NAME .EQ. 'FLIP' ) THEN
         CALL FLIP ( STATUS )

*  Performs forward and inverse Fourier transforms of 1- or
*  2-dimensional NDFs.
      ELSE IF ( NAME .EQ. 'FOURIER' ) THEN
         CALL FOURIER ( STATUS )

*  Smooths a 1- or 2-dimensional image using a Gaussian filter.
      ELSE IF ( NAME .EQ. 'GAUSMOOTH' ) THEN
         CALL GAUSMOOTH ( STATUS )

*  Replaces bad pixels in a 2-d data array with the local median.
      ELSE IF ( NAME .EQ. 'GLITCH' ) THEN
         CALL GLITCH ( STATUS )

*  Displays the values of the KAPPA global parameters.
      ELSE IF ( NAME .EQ. 'GLOBALS' ) THEN
         CALL GLOBALS ( STATUS )

*  Computes ordered statistics for an NDF's pixels using a histogram.
      ELSE IF ( NAME .EQ. 'HISTAT' ) THEN
         CALL HISTAT ( STATUS )

*  Performs an histogram equalisation on an NDF.
      ELSE IF ( NAME .EQ. 'HISTEQ' ) THEN
         CALL HISTEQ ( STATUS )

*  Computes an histogram of an NDF's values.
      ELSE IF ( NAME .EQ. 'HISTOGRAM' ) THEN
         CALL HISTOGRAM ( STATUS )

*  Gives help about KAPPA.
      ELSE IF ( NAME .EQ. 'KAPHELP' ) THEN
         CALL KAPHELP ( STATUS )

*  Compares data sets using the Kolmogorov-Smirnov test
      ELSE IF ( NAME .EQ. 'KSTEST' ) THEN
         CALL KSTEST ( STATUS )

*  Performs a Laplacian convolution as an edge detector in a 2-d data
*  array.
      ELSE IF ( NAME .EQ. 'LAPLACE' ) THEN
         CALL LAPLACE ( STATUS )

*  Create a positions list.
      ELSE IF ( NAME .EQ. 'LISTMAKE' ) THEN
         CALL LISTMAKE ( STATUS )

*  Display a positions list.
      ELSE IF ( NAME .EQ. 'LISTSHOW' ) THEN
         CALL LISTSHOW ( STATUS )

*  Takes the base-10 logarithm of each pixel of a data array.
      ELSE IF ( NAME .EQ. 'LOG10' ) THEN
         CALL KAP_LOG10 ( STATUS )

*  Takes the logarithm of each pixel of a data array (specified base).
      ELSE IF ( NAME .EQ. 'LOGAR' ) THEN
         CALL LOGAR ( STATUS )

* Takes the natural logarithm of each pixel of a data array.
      ELSE IF ( NAME .EQ. 'LOGE' ) THEN
         CALL LOGE ( STATUS )

*  Outputs the values of a sub-array of a 2-d data array to the
*  screen or an ASCII file.
      ELSE IF ( NAME .EQ. 'LOOK' ) THEN
         CALL LOOK ( STATUS )

*  Performs a Richardson-Lucy deconvolution of a 1- or 2-dimensional
*  array.
      ELSE IF ( NAME .EQ. 'LUCY' ) THEN
         CALL LUCY ( STATUS )

*   Creates a 2-dimensional NDF from the coefficients of a polynomial
*   surface.
      ELSE IF ( NAME .EQ. 'MAKESURFACE' ) THEN
         CALL MAKESURFACE ( STATUS )

*  Converts all or part of a data array from one dimensionality to
*  another.
      ELSE IF ( NAME .EQ. 'MANIC' ) THEN
         CALL MANIC ( STATUS )

*  Evaluates mathematical expressions applied to NDF data structures.
      ELSE IF ( NAME .EQ. 'MATHS' ) THEN
         CALL MATHS ( STATUS )

*  Smooths a 2-dimensional data array using a weighted median filter.
      ELSE IF ( NAME .EQ. 'MEDIAN' ) THEN
         CALL MEDIAN ( STATUS )

*  Merges several non-congruent 2-d data arrays into one output data
*  array.
      ELSE IF ( NAME .EQ. 'MOSAIC' ) THEN
         CALL MOSAIC ( STATUS )

*  Does cumulative statistics on a 2-d sub-array over a sequence of
*  2-d data arrays.
      ELSE IF ( NAME .EQ. 'MSTATS' ) THEN
         CALL MSTATS ( STATUS )

*  Multiplies two NDF data structures.
      ELSE IF ( NAME .EQ. 'MULT' ) THEN
         CALL MULT ( STATUS )

*  Resets the KAPPA global parameters.
      ELSE IF ( NAME .EQ. 'NOGLOBALS' ) THEN
         CALL NOGLOBALS ( STATUS )

*  Replaces all occurrences of magic-value pixels in a data array by a
*  user-defined value.
      ELSE IF ( NAME .EQ. 'NOMAGIC' ) THEN
         CALL NOMAGIC ( STATUS )

*  Normalizes one NDF to a similar NDF by calculating a scale
*  and zero difference.
      ELSE IF ( NAME .EQ. 'NORMALIZE' ) THEN
         CALL NORMALIZE ( STATUS )

*  Counts the number of elements of a data array with values
*  greater than a specified value.
      ELSE IF ( NAME .EQ. 'NUMB' ) THEN
         CALL NUMB ( STATUS )

*  Sets pixels outside a specified circle in a 2-d data array
*  to a specified value.
      ELSE IF ( NAME .EQ. 'OUTSET' ) THEN
         CALL OUTSET ( STATUS )

*  Obtains the value or values of an application parameter.
      ELSE IF ( NAME .EQ. 'PARGET' ) THEN
         CALL PARGET ( STATUS )

*  Pastes a series of NDFs upon each other.
      ELSE IF ( NAME .EQ. 'PASTE' ) THEN
         CALL PASTE ( STATUS )

*  Expands an NDF by pixel duplication.
      ELSE IF ( NAME .EQ. 'PIXDUPE' ) THEN
         CALL PIXDUPE ( STATUS )

*  Takes the specified power of each pixel of a data array.
      ELSE IF ( NAME .EQ. 'POW' ) THEN
         CALL KAP_POW ( STATUS )

*  Create a 1-d profile through an N-d NDF.
      ELSE IF ( NAME .EQ. 'PROFILE' ) THEN
         CALL PROFILE ( STATUS )

*  Determines the parameters of a model star profile by fitting
*  star images in a 2-dimensional NDF.
      ELSE IF ( NAME .EQ. 'PSF' ) THEN
         CALL PSF ( STATUS )

*  Generates a mosaic from equally sized 2-d data arrays, optionally
*  specified from an ASCII file.
      ELSE IF ( NAME .EQ. 'QUILT' ) THEN
         CALL QUILT ( STATUS )

*  Adds a scalar to a section of a an NDF data structure to correct
*  rift-valley defects.
      ELSE IF ( NAME .EQ. 'RIFT' ) THEN
         CALL RIFT ( STATUS )

*  Rotates a 2-d data array through any angle.
      ELSE IF ( NAME .EQ. 'ROTATE' ) THEN
         CALL ROTATE ( STATUS )

*  Copies polygonal segments from one 2-d data array to another.
      ELSE IF ( NAME .EQ. 'SEGMENT' ) THEN
         CALL SEGMENT ( STATUS )

*  Replaces all occurrences of a given value in an NDF array with the
*  bad value.
      ELSE IF ( NAME .EQ. 'SETMAGIC' ) THEN
         CALL SETMAGIC ( STATUS )

*  Enhances edges in a 2-dimensional NDF using a shadow effect.
      ELSE IF ( NAME .EQ. 'SHADOW' ) THEN
         CALL SHADOW ( STATUS )

*  Realigns a 2-d data array via an x-y shift.
      ELSE IF ( NAME .EQ. 'SLIDE' ) THEN
         CALL SLIDE ( STATUS )

*  Squashes or stretches a 2-d data array in either or both axes.
      ELSE IF ( NAME .EQ. 'SQORST' ) THEN
         CALL SQORST ( STATUS )

*  Computes simple statistics for an NDF's pixels.
      ELSE IF ( NAME .EQ. 'STATS' ) THEN
         CALL STATS ( STATUS )

*  Subtracts one NDF data structure from another.
      ELSE IF ( NAME .EQ. 'SUB' ) THEN
         CALL SUB ( STATUS )

*  Replaces all occurrences of a given value in an NDF array with
*  another value.
      ELSE IF ( NAME .EQ. 'SUBSTITUTE' ) THEN
         CALL SUBSTITUTE ( STATUS )

*  Fits a polynomial or spline surface to 2-d data array.
      ELSE IF ( NAME .EQ. 'SURFIT' ) THEN
         CALL SURFIT ( STATUS )

*  Edits an NDF such that array values below and above two thresholds
*  take constant values.
      ELSE IF ( NAME .EQ. 'THRESH' ) THEN
         CALL THRESH ( STATUS )

*  Converts free-format data into an NDF.
      ELSE IF ( NAME .EQ. 'TRANDAT' ) THEN
         CALL TRANDAT ( STATUS )

*  Inverts a transformation.
      ELSE IF ( NAME .EQ. 'TRANINVERT' ) THEN
         CALL TRANINVERT ( STATUS )

*  Joins two transformations.
      ELSE IF ( NAME .EQ. 'TRANJOIN' ) THEN
         CALL TRANJOIN ( STATUS )

*  Makes a transformation structure given its co-ordinate mappings.
      ELSE IF ( NAME .EQ. 'TRANMAKE' ) THEN
         CALL TRANMAKE ( STATUS )

*  Applies a transformation to an NDF.
      ELSE IF ( NAME .EQ. 'TRANSFORMER' ) THEN
         CALL TRANSFORMER ( STATUS )

*  Lists the contents of a transformation structure.
      ELSE IF ( NAME .EQ. 'TRANTRACE' ) THEN
         CALL TRANTRACE ( STATUS )

*  Performs a trigonometric transformation on a data array.
      ELSE IF ( NAME .EQ. 'TRIG' ) THEN
         CALL TRIG ( STATUS )

*  Align a group of NDFs using the WCS components.
      ELSE IF ( NAME .EQ. 'WCSALIGN' ) THEN
         CALL WCSALIGN ( STATUS )

*  Transforms positions between NDF WCS Frames.
      ELSE IF ( NAME .EQ. 'WCSTRAN' ) THEN
         CALL WCSTRAN ( STATUS )

*  Applies a Wiener filter to a 1- or 2-dimensional array.
      ELSE IF ( NAME .EQ. 'WIENER' ) THEN
         CALL WIENER ( STATUS )

*  Replaces regions in a 2-d data array by bad values or by linear
*  interpolation.
      ELSE IF ( NAME .EQ. 'ZAPLIN' ) THEN
         CALL ZAPLIN ( STATUS )

      ELSE

*  No such option exists.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CMD', NAME )
         CALL ERR_REP( 'KAPPA_MON_NOCOM',
     :     'KAPPA: No such option ^CMD.', STATUS )

      END IF

*  End and return.

      END

