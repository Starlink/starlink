      SUBROUTINE CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP, OXBASE,
     :                       OYBASE, OXDIM, OYDIM, ILO, IHI, OLO, OHI, 
     :                       OBAD, OUT, STATUS )
*+
*  Name:
*     CCD1_MKIMG

*  Purpose:
*     Processes an array for display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP, OXBASE, OYBASE,
*                      OXDIM, OYDIM, ILO, IHI, OLO, OHI, OBAD, OUT, 
*                      STATUS )

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

*  Arguments Returned:
      INTEGER OUT( OXDIM, OYDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      INTEGER ILBND( 2 )         ! Lower bounds of input array
      INTEGER IPTEMP             ! Pointer to temporary array space
      INTEGER IUBND( 2 )         ! Upper bounds of input array
      INTEGER MAXPIX             ! Initial adaptive scale size for AST_RESAMPLE
      INTEGER NBAD               ! Number of bad pixels in resampled array
      INTEGER OLBND( 2 )         ! Lower bounds of output array
      INTEGER OUBND( 2 )         ! Upper bounds of output array
      INTEGER RFLAGS             ! Flags to pass to AST_RESAMPLE
      INTEGER * 1 DUMIB          ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 1 DUMOB          ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 1 DUMIUB         ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 1 DUMOUB         ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 2 DUMIW          ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 2 DUMOW          ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 2 DUMIUW         ! Dummy array to pass to AST_RESAMPLE
      INTEGER * 2 DUMOUW         ! Dummy array to pass to AST_RESAMPLE
      INTEGER DUMII              ! Dummy array to pass to AST_RESAMPLE
      INTEGER DUMOI              ! Dummy array to pass to AST_RESAMPLE
      REAL DUMIR                 ! Dummy array to pass to AST_RESAMPLE
      REAL DUMOR                 ! Dummy array to pass to AST_RESAMPLE
      DOUBLE PRECISION DUMID     ! Dummy array to pass to AST_RESAMPLE
      DOUBLE PRECISION DUMOD     ! Dummy array to pass to AST_RESAMPLE
      DOUBLE PRECISION DUMPAR    ! Dummy array to pass to AST_RESAMPLE
      DOUBLE PRECISION TOL       ! Pixel tolerance for resampling
      LOGICAL BADTMP             ! Are there bad values in the resampled array?

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

*  Get workspace for intermediate image (of type ITYPE).
      CALL CCD1_MALL( OXDIM * OYDIM, ITYPE, IPTEMP, STATUS )

*  Now resample the data array into the output array.  Note that some of
*  the arguments in the following calls change between types, as well
*  as the names of the routines.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         NBAD = AST_RESAMPLEB( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         DUMIB, AST__NEAREST, AST_NULL, DUMPAR,
     :                         RFLAGS, TOL, MAXPIX, VAL__BADB, 2, 
     :                         OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), DUMOB, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         NBAD = AST_RESAMPLEUB( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                          DUMIUB, AST__NEAREST, AST_NULL, DUMPAR,
     :                          RFLAGS, TOL, MAXPIX, VAL__BADUB, 2, 
     :                          OLBND, OUBND, OLBND, OUBND, 
     :                          %VAL( IPTEMP ), DUMOUB, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         NBAD = AST_RESAMPLEW( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         DUMIW, AST__NEAREST, AST_NULL, DUMPAR,
     :                         RFLAGS, TOL, MAXPIX, VAL__BADW, 2,
     :                         OLBND, OUBND, OLBND, OUBND, 
     :                         %VAL( IPTEMP ), DUMOW, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         NBAD = AST_RESAMPLEUW( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                          DUMIUW, AST__NEAREST, AST_NULL, DUMPAR,
     :                          RFLAGS, TOL, MAXPIX, VAL__BADUW, 2,
     :                          OLBND, OUBND, OLBND, OUBND, 
     :                          %VAL( IPTEMP ), DUMOUW, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         NBAD = AST_RESAMPLEI( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         DUMII, AST__NEAREST, AST_NULL, DUMPAR,
     :                         RFLAGS, TOL, MAXPIX, VAL__BADI, 2,
     :                         OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), DUMOI, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         NBAD = AST_RESAMPLER( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         DUMIR, AST__NEAREST, AST_NULL, DUMPAR,
     :                         RFLAGS, TOL, MAXPIX, VAL__BADR, 2,
     :                         OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), DUMOR, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         NBAD = AST_RESAMPLED( IMAP, 2, ILBND, IUBND, %VAL( IPIN ),
     :                         DUMID, AST__NEAREST, AST_NULL, DUMPAR,
     :                         RFLAGS, TOL, MAXPIX, VAL__BADD, 2,
     :                         OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( IPTEMP ), DUMOD, STATUS )

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

      END
* $Id$
