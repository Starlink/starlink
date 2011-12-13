      SUBROUTINE CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP,
     :                       OXDIM, OYDIM, ILO, IHI, OLO, OHI,
     :                       OBAD, INTSCH, INTPAR, OUT, STATUS )
*+
*  Name:
*     CCD1_MKIMG

*  Purpose:
*     Processes an array for display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKIMG( IPIN, ITYPE, IXDIM, IYDIM, IMAP,
*                      OXDIM, OYDIM, ILO, IHI, OLO, OHI, OBAD, INTSCH,
*                      INTPAR, OUT, STATUS )

*  Description:
*     This routine resamples and rescales an array of data into a
*     integer array with a given data range.  The data is resampled
*     using a given AST map, rescaled to fall within a given range,
*     and then the good pixels are written into an output integer
*     array suitable for displaying using PGPLOT.  The output array
*     should therefore be initialised to a background colour before
*     use.

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
*        This determines the subpixel resampling scheme.
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

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IPIN
      CHARACTER * ( * ) ITYPE
      INTEGER IXDIM
      INTEGER IYDIM
      INTEGER IMAP
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
      INTEGER I                  ! Loop variable
      INTEGER ILBND( 2 )         ! Lower bounds of input array
      INTEGER IPIV               ! Pointer to dummy input variance array
      INTEGER IPOV               ! Pointer to dummy output variance array
      INTEGER IPTEMP             ! Pointer to temporary array space
      INTEGER IUBND( 2 )         ! Upper bounds of input array
      INTEGER MAXPIX             ! Initial adaptive scale size for AST_RESAMPLE
      INTEGER NBAD               ! Number of bad pixels in resampled array
      INTEGER OLBND( 2 )         ! Lower bounds of output array
      INTEGER OUBND( 2 )         ! Upper bounds of output array
      INTEGER OXB                ! X base of intermediate array
      INTEGER OXD                ! X dimension of intermediate array
      INTEGER OYB                ! Y base of intermediate array
      INTEGER OYD                ! Y dimension of intermediate array
      INTEGER RFLAGS             ! Flags to pass to AST_RESAMPLE
      DOUBLE PRECISION LPOS( 2 ) ! Dummy
      DOUBLE PRECISION DOLBND    ! Output lower bound
      DOUBLE PRECISION DOUBND    ! Output upper bound
      DOUBLE PRECISION DILBND( 2 ) ! Lower bounds as a double
      DOUBLE PRECISION DIUBND( 2 ) ! Upper bounds as a double
      DOUBLE PRECISION TOL       ! Pixel tolerance for resampling
      DOUBLE PRECISION UPOS( 2 ) ! Dummy

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set common arguments to pass to resampling routine.
      ILBND( 1 ) = 1
      ILBND( 2 ) = 1
      IUBND( 1 ) = IXDIM
      IUBND( 2 ) = IYDIM
      RFLAGS = AST__USEBAD
      TOL = 0.5D0 * MIN( FLOAT( OXDIM ) / FLOAT( IXDIM ),
     :                   FLOAT( OYDIM ) / FLOAT( IYDIM ) )
      MAXPIX = IXDIM + IYDIM

*  Get dummy workspace for unused input and output variance arrays.  This
*  is probably not necessary, but an over-zealous runtime system might
*  not like passing invalid memory references.
      CALL CCD1_MALL( 1, ITYPE, IPIV, STATUS )
      CALL CCD1_MALL( 1, ITYPE, IPOV, STATUS )
      IPIV = 0
      IPOV = 0

*  Work out how big an array we need to do the initial resampling.
      DILBND( 1 ) = 1D0
      DILBND( 2 ) = 1D0
      DIUBND( 1 ) = DBLE( IXDIM )
      DIUBND( 2 ) = DBLE( IYDIM )
      CALL AST_MAPBOX( IMAP, DILBND, DIUBND, .TRUE., 1, DOLBND, DOUBND,
     :                 LPOS, UPOS, STATUS )
      OLBND( 1 ) = NINT( DOLBND ) - 1
      OUBND( 1 ) = NINT( DOUBND ) + 1
      OXB = OLBND( 1 )
      OXD = OUBND( 1 ) - OLBND( 1 ) + 1
      CALL AST_MAPBOX( IMAP, DILBND, DIUBND, .TRUE., 2, DOLBND, DOUBND,
     :                 LPOS, UPOS, STATUS )
      OLBND( 2 ) = NINT( DOLBND ) - 1
      OUBND( 2 ) = NINT( DOUBND ) + 1
      OYB = OLBND( 2 )
      OYD = OUBND( 2 ) - OLBND( 2 ) + 1

*  Get workspace for intermediate image (of type ITYPE).
      CALL CCD1_MALL( OXD * OYD, ITYPE, IPTEMP, STATUS )

*  Now resample the data array into the output array.  Note that some of
*  the arguments in the following calls change between types, as well
*  as the names of the routines.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         NBAD = AST_RESAMPLEB( IMAP, 2, ILBND, IUBND,
     :                         %VAL( CNF_PVAL( IPIN ) ),
     :                         %VAL( CNF_PVAL( IPIV ) ),
     :                         INTSCH, AST_NULL,
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADB,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         NBAD = AST_RESAMPLEUB( IMAP, 2, ILBND, IUBND,
     :                          %VAL( CNF_PVAL( IPIN ) ),
     :                          %VAL( CNF_PVAL( IPIV ) ),
     :                          INTSCH, AST_NULL,
     :                          INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADUB,
     :                          2, OLBND, OUBND, OLBND, OUBND,
     :                          %VAL( CNF_PVAL( IPTEMP ) ),
     :                          %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         NBAD = AST_RESAMPLEW( IMAP, 2, ILBND, IUBND,
     :                         %VAL( CNF_PVAL( IPIN ) ),
     :                         %VAL( CNF_PVAL( IPIV ) ),
     :                         INTSCH, AST_NULL,
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADW,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         NBAD = AST_RESAMPLEUW( IMAP, 2, ILBND, IUBND,
     :                          %VAL( CNF_PVAL( IPIN ) ),
     :                          %VAL( CNF_PVAL( IPIV ) ),
     :                          INTSCH, AST_NULL,
     :                          INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADUW,
     :                          2, OLBND, OUBND, OLBND, OUBND,
     :                          %VAL( CNF_PVAL( IPTEMP ) ),
     :                          %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         NBAD = AST_RESAMPLEI( IMAP, 2, ILBND, IUBND,
     :                         %VAL( CNF_PVAL( IPIN ) ),
     :                         %VAL( CNF_PVAL( IPIV ) ),
     :                         INTSCH, AST_NULL,
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADI,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         NBAD = AST_RESAMPLER( IMAP, 2, ILBND, IUBND,
     :                         %VAL( CNF_PVAL( IPIN ) ),
     :                         %VAL( CNF_PVAL( IPIV ) ),
     :                         INTSCH, AST_NULL,
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADR,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         %VAL( CNF_PVAL( IPOV ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         NBAD = AST_RESAMPLED( IMAP, 2, ILBND, IUBND,
     :                         %VAL( CNF_PVAL( IPIN ) ),
     :                         %VAL( CNF_PVAL( IPIV ) ),
     :                         INTSCH, AST_NULL,
     :                         INTPAR, RFLAGS, TOL, MAXPIX, VAL__BADD,
     :                         2, OLBND, OUBND, OLBND, OUBND,
     :                         %VAL( CNF_PVAL( IPTEMP ) ),
     :                         %VAL( CNF_PVAL( IPOV ) ), STATUS )

      END IF

*  And finally rescale it.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_ISCLB( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    ILO, IHI, OLO, OHI,
     :                    OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                    OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_ISCLUB( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                     ILO, IHI, OLO, OHI,
     :                     OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                     OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_ISCLW( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    ILO, IHI, OLO, OHI,
     :                    OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                    OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_ISCLUW( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                     ILO, IHI, OLO, OHI,
     :                     OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                     OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_ISCLI( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    ILO, IHI, OLO, OHI,
     :                    OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                    OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_ISCLR( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    ILO, IHI, OLO, OHI,
     :                    OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                    OLBND( 2 ) - 1, OUT, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_ISCLD( OXD, OYD, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    ILO, IHI, OLO, OHI,
     :                    OBAD, OXDIM, OYDIM, OLBND( 1 ) - 1,
     :                    OLBND( 2 ) - 1, OUT, STATUS )

      END IF

*  Release the temporary workspace.
      CALL CCD1_MFREE( IPIV, STATUS )
      CALL CCD1_MFREE( IPOV, STATUS )
      CALL CCD1_MFREE( IPTEMP, STATUS )

      END
* $Id$
