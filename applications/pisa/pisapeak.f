      SUBROUTINE PISAPEAK( STATUS )
*+
*  Name:
*     PISAPEAK

*  Purpose:
*     PISAPEAK transforms the PISAFIND parameterisations so that the
*     variables are intensity invariant.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAPEAK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     PISAPEAK reads in the object parameterisations file produced by
*     PISAFIND. Using the PISA profiling function it then transforms
*     certain of the variables into values which are intensity
*     invariant for a radial stellar profile. Two of the output
*     variables may have values of around 1.0 for objects which are
*     well represented by the given PISA profiling parameters. The
*     variables produced are the ratio of the semi-major axis of the
*     object fit to the radius of an object whose peak intensity is the
*     same, but whose size is determined by the PISA profiling function
*     (a peakedness measure). The ratio of the integrated intensity to
*     the peak intensity and the model object ratio. The ellipticity
*     (unmodified) and the absolute value of the intensity weighted
*     cross moment (ABS(SXY)).
*
*     The output from PISAPEAK can be used in star-galaxy separation,
*     either by applying direct cuts in variable values or by
*     discrimination analysis routines such as PISAKNN.

*  Usage:
*     PISAPEAK IN FINDDATA RESULTS GSIGM CROSS COMIX BACKGROUND THRESH

*  ADAM Parameters:
*     BACKGROUND = _REAL (Read)
*        The frame background value (sky) as used by PISAFIND. [Global]
*     COMIX = _REAL (Read)
*        The mixture coefficient, as a fraction of the Gaussian peak,
*        of a Lorentzian function used to model the wings of the stellar
*        profile. At each point in the analytical profile the Lorentzian
*        function is added to the Gaussian/exponential core scaled by
*        the mixture coefficient. [Global]
*     CROSS = _REAL (Read)
*        The crossover point, as a percentage of the Gaussian peak,
*        where an exponential fall-off in the analytical stellar profile
*        takes over from the Gaussian core. The exponential function is
*        joined on smoothly to the Gaussian. [Global]
*     FINDDATA = FILENAME (Read)
*        Name of the file containing the PISAFIND parameterisations.
*        [PISAFIND.DAT]
*     GSIGM = _REAL (Read)
*        The standard deviation, in pixels, of the Gaussian core of the
*        analytical stellar profile. [Global]
*     IN = NDF (Read)
*        The NDF containing the objects which have been parameterised by
*        PISAFIND.
*     RESULTS = FILENAME (Write)
*        Name of a file to contain the PISAPEAK results. [PISAPEAK.DAT]
*     THRESH = _REAL (Read)
*        The detection threshold as used by PISAFIND. [Global]

*  Examples:
*     PISAPEAK IN=FRAME FINDDATA=PISAFIND.DAT RESULTS=PISAPEAK.DAT
*        BACKGROUND=255 THRESH=12
*        This example shows PISAPEAK using the objects found in NDF
*        FRAME with parameters stored in file PISAFIND.DAT. The results
*        are stored in file PISAPEAK.DAT. Note that the model parameters
*        GSIGM CROSS and COMIX are defaulted to those which have been
*        stored in GLOBAL. These values were probably written by the
*        routine PISAFIT which uses a list of stars to fit the PISA
*        profiling function. The background value and threshold are
*        those used by PISAFIND when detecting and parameterising the
*        objects.

*  Notes:
*     -  The model profile is used to scale the object values to those
*     around unity. This only works well if the PISA profiling function
*     is a good fit to the stars on your frame. If it is not a good fit
*     or you cannot determine the model parameters then inaccurate
*     values can be used, the only criterion being that the GSIGM value
*     is about the FWHM seeing of your data. The results file will now
*     contain values 'normalised' to this 'imaginary' object. The stars
*     will still form a group of objects with similar values, although
*     they will not be as tightly clustered as with a good fit.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-MAR-1991 (PDRAPER):
*        Original version.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PVAL. Look at that previous date!
*        Boy never thought I'd still be in this job!
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IND                ! NDF identifier
      INTEGER IPIN               ! pointer to NDF data array
      INTEGER EL                 ! dummy
      INTEGER LBND( 2 )          ! lower bounds of the NDF
      INTEGER UBND( 2 )          ! upper bounds of the NDF
      INTEGER NDIM               ! dummy
      INTEGER IDIM( 2 )          ! size of the sides of NDF data array
      REAL GSIGM                 ! sigma of model function
      REAL CROSS                 ! cross over value (percentage)
      REAL COMIX                 ! ratio of function parts
      REAL BACK                  ! background value
      REAL THRESH                ! detection threshold
      INTEGER IFS                ! FIO file descriptor (input file)
      INTEGER IFO                ! FIO file descriptor (output file)
      LOGICAL OPNF1,
     :        OPNF2              ! set if files are open
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      OPNF1 = .FALSE.
      OPNF2 = .FALSE.

*  Get the input NDF.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', IND, STATUS )

*  Map in it's data array component.
      CALL NDF_MAP( IND, 'DATA', '_REAL', 'READ', IPIN, EL, STATUS )
      CALL NDF_BOUND( IND, 2, LBND, UBND, NDIM, STATUS )

*  Work out the array size.
      IDIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      IDIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Get the model parameters.
      CALL PAR_GET0R( 'GSIGM', GSIGM, STATUS )
      CALL PAR_GET0R( 'CROSS', CROSS, STATUS )
      CALL PAR_GET0R( 'COMIX', COMIX, STATUS )

*  Get the background ( sky value ).
      BACK = 0.0
      CALL PAR_GET0R( 'BACKGROUND', BACK, STATUS )

*  Get the threshold value.
      CALL PAR_GET0R( 'THRESH', THRESH, STATUS )

*  Open the pisafind data file of the objects
      CALL PSA1_ASFIO( 'FINDDATA', 'READ', 'LIST', 0, IFS, OPNF1,
     :                 STATUS )

*  Open a file to contain the results.
      CALL PSA1_ASFIO( 'RESULTS', 'WRITE', 'LIST', 0, IFO, OPNF2,
     :                 STATUS )

*  Do the work, derive model radius, model intensity peak ratios and
*  transform the input data.
      CALL PEAKN( %VAL( CNF_PVAL( IPIN ) ), IDIM( 1 ), IDIM( 2 ),
     :            LBND( 1 ), LBND( 2 ), GSIGM, CROSS, COMIX, BACK,
     :            THRESH, IFS, IFO, STATUS )

999   CONTINUE

*  Release the input NDF.
      CALL NDF_END( STATUS )

*  Close the files, if opened
      IF( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS, STATUS )
         CALL PAR_CANCL( 'FINDDATA', STATUS )
      END IF
      IF( OPNF1 ) THEN
         CALL FIO_CLOSE( IFO, STATUS )
         CALL PAR_CANCL( 'RESULTS', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAPEAK_ERR',
     :   'PISAPEAK: Error producing peakedness measure.',
     :   STATUS )
      END IF

      END
* $Id$
