      SUBROUTINE CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP, OXBASE,
     :                       OYBASE, OXDIM, OYDIM, ILO, IHI, OLO, OHI, 
     :                       OBAD, INTSCH, INTPAR, OUT, STATUS )
*+
*  Name:
*     CCD1_MKIMG

*  Purpose:
*     Processes an array for display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP, OXBASE, OYBASE,
*                      OXDIM, OYDIM, ILO, IHI, OLO, OHI, OBAD, INTSCH,
*                      INTPAR, OUT, STATUS )

*  Description:
*     This routine resamples and rescales an array of data into a
*     integer array with a given data range.  The data is resampled 
*     using a given AST map and then rescaled.  The output array is
*     then an image suitable for displaying using PGPLOT.

*  Arguments:
*     IPIN = INTEGER (Given)
*        Pointer to the array of data which is to be resampled and
*        scaled prior to display.  The array is of shape ( IXDIM, IYDIM )
*        and its first pixel is centred at coordinates (1, 1).
*     ITYPE = CHARACTER * ( * ) (Given)
*        The numeric HDS type of the input array. One of _BYTE, _UBYTE,
*        _WORD, _UWORD, _INTEGER, _REAL, _DOUBLE.
*     IXDIM = INTEGER (Given)
*        The first dimension of the input array.
*     IYDIM = INTEGER (Given)
*        The second dimension of the input array.
*     IMAP = INTEGER (Given)
*        AST pointer to the mapping to use for resampling from the input
*        array to the output array.
*     OXBASE = INTEGER (Given)
*        The first coordinate of the first pixel in the output array.
*     OYBASE = INTEGER (Given)
*        The second coordinate of the first pixel in the output array.
*     OXDIM = INTEGER (Given)
*        The first dimension of the output array.
*     OYDIM = INTEGER (Given)
*        The second dimension of the output array.
*     ILO = DOUBLE PRECISION (Given)
*        The data value that specifies the lower image-scaling limit in
*        the input array.
*     IHI = DOUBLE PRECISION (Given)
*        The data value that specifies the upper image-scaling limit in
*        the input array.
*     OLO = INTEGER (Given)
*        The lowest value to appear in the scaled array for good input
*        data.
*     OHI = INTEGER (Given)
*        The highest value to appear in the scaled array for good input
*        data.
*     OBAD = INTEGER (Given)
*        The value to be assigned to bad pixels in the scaled array.
*     INTSCH = INTEGER (Given)
*        The INTERP parameter to be passed to the AST_RESAMPLE routine.
*        This determines the subpixel resampling scheme.  If set to 
*        AST__UINTERP then the resampling will be done using the 
*        CCG1_IBLK<X> routines, which average over a cube in the input
*        array.
*     INTPAR( * ) = DOUBLE PRECISION (Given)
*        The PARAMS vector to be passed to the AST_RESAMPLE routine.
*        For certain values of INTSCH, it gives additional information
*        about how the resampling is to be done.
*     OUT( OXDIM, OYDIM ) = INTEGER (Returned)
*        The output array. This should be integer and will contain on
*        exit data in the range OLO to OHI.  The data will be
*        resampled using the mapping given by the IMAP argument.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-AUG-2000 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER IPIN
      CHARACTER * ( * ) ITYPE
      INTEGER IXDIM
      INTEGER IYDIM
      INTEGER IMAP
      INTEGER OXBASE
      INTEGER OYBASE
      INTEGER OXDIM
      INTEGER OYDIM
      DOUBLE PRECISION ILO
      DOUBLE PRECISION IHI
      INTEGER OLO
      INTEGER OHI
      INTEGER OBAD
      INTEGER INTSCH
      DOUBLE PRECISION INTPAR( * )

*  Arguments Returned:
      INTEGER OUT( OXDIM, OYDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      INTEGER ILBND( 2 )         ! Lower bounds of input array
      INTEGER IPIV               ! Pointer to dummy input variance array
      INTEGER IPOV               ! Pointer to dummy output variance array
      INTEGER IPTEMP             ! Pointer to temporary array space
      INTEGER IUBND( 2 )         ! Upper bounds of input array
      INTEGER MAXPIX             ! Initial adaptive scale size for AST_RESAMPLE
      INTEGER NBAD               ! Number of bad pixels in resampled array
      INTEGER OLBND( 2 )         ! Lower bounds of output array
      INTEGER OUBND( 2 )         ! Upper bounds of output array
      INTEGER RFLAGS             ! Flags to pass to AST_RESAMPLE
      DOUBLE PRECISION TOL       ! Pixel tolerance for resampling
      LOGICAL BADTMP             ! Are there bad values in the resampled array?

*  Internal References:
      EXTERNAL CCG1_IBLKB        ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKUB       ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKW        ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKUW       ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKI        ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKR        ! Block averaging interpolation routine
      EXTERNAL CCG1_IBLKD        ! Block averaging interpolation routine

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set common arguments to pass to resampling routine.
      ILBND( 1 ) = 1
      ILBND( 2 ) = 1
      IUBND( 1 ) = IXDIM
      IUBND( 2 ) = IYDIM
      OLBND( 1 ) = OXBASE
      OLBND( 2 ) = OYBASE
      OUBND( 1 ) = OXBASE + OXDIM - 1
      OUBND( 2 ) = OYBASE + OYDIM - 1
      RFLAGS = AST__USEBAD
      TOL = 0.5D0 * ( FLOAT( OXDIM ) / FLOAT( IXDIM ) + 
     :                FLOAT( OYDIM ) / FLOAT( IYDIM ) )
      MAXPIX = IXDIM + IYDIM

*  Get dummy workspace for unused input and output variance arrays.  This
*  is probably not necessary, but an over-zealous runtime system might
*  not like passing invalid memory references.
      CALL CCD1_MALL( 1, ITYPE, IPIV, STATUS )
      CALL CCD1_MALL( 1, ITYPE, IPOV, STATUS )

*  Get workspace for intermediate image (of type ITYPE).
      CALL CCD1_MALL( OXDIM * OYDIM, ITYPE, IPTEMP, STATUS )

*  Now resample the data array into the output array.  Note that some of
*  the arguments in the following calls change between types, as well
*  as the names of the routines.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         NBAD = AST_RESAMPLEB( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         %VAL( IPIV ), INTSCH, CCG1_IBLKB, 
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADB,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         NBAD = AST_RESAMPLEUB( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                          %VAL( IPIV ), INTSCH, CCG1_IBLKUB, 
     :                          INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADUB,
     :                          2, OLBND, OUBND, OLBND, OUBND,
     :                          %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         NBAD = AST_RESAMPLEW( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         %VAL( IPIV ), INTSCH, CCG1_IBLKW, 
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADW,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         NBAD = AST_RESAMPLEUW( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                          %VAL( IPIV ), INTSCH, CCG1_IBLKUW, 
     :                          INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADUW,
     :                          2, OLBND, OUBND, OLBND, OUBND,
     :                          %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         NBAD = AST_RESAMPLEI( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         %VAL( IPIV ), INTSCH, CCG1_IBLKI, 
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADI,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         NBAD = AST_RESAMPLER( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         %VAL( IPIV ), INTSCH, CCG1_IBLKR, 
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADR,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         NBAD = AST_RESAMPLED( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         %VAL( IPIV ), INTSCH, CCG1_IBLKD, 
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADD,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), %VAL( IPOV ), STATUS )

      END IF

*  And finally rescale it.
      BADTMP = NBAD .GT. 0
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_ISCLB( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                    .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_ISCLUB( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                     .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_ISCLW( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                    .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_ISCLUW( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                     .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_ISCLI( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                    .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_ISCLR( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                    .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_ISCLD( BADTMP, OXDIM, OYDIM, %VAL( IPTEMP ),
     :                    .FALSE., ILO, IHI, OLO, OHI, OBAD, OUT,
     :                    STATUS )

      END IF

*  Release the temporary workspace.
      CALL CCD1_MFREE( IPTEMP, STATUS )
      CALL CCD1_MFREE( IPIV, STATUS )
      CALL CCD1_MFREE( IPOV, STATUS )

      END
* $Id$
