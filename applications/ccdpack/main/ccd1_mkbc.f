      SUBROUTINE CCD1_MKBC( TYPE, GENVAR, IPSTK, NPIX, NLINES, VARS,
     :                      IMETH, MINPIX, NITER, NSIGMA, ALPHA, RMIN,
     :                      RMAX, IPRES, IPVAR, WRK1, WRK2, PP, COVEC,
     :                      NMAT, NCON, POINT, USED, STATUS )
*+
*  Name:
*     CCD1_MKBC

*  Purpose:
*     To combine a stack of images of a given, using the methods
*     specfic to MAKEBIAS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKBC( TYPE, GENVAR, IPSTK, NPIX, NLINES, VARS,
*                     IMETH, MINPIX, NITER, NSIGMA, ALPHA, RMIN,
*                     RMAX, IPRES, IPVAR, WRK1, WRK2, PP, COVEC,
*                     NMAT, NCON, POINT, USED, STATUS )

*  Description:
*     This routine controls the combination of a stack of image data
*     lines for the routine MAKEBIAS. The data is combined using the
*     given technique, using the appropriate data type. If variances
*     are to be generated then these are, using the readout noise
*     estimates in VARS -- note one per line of data. If not then a
*     different set of combination routines are called which do not have
*     the overhead of processing variances.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the data to be processed. One of
*        _DOUBLE,_REAL,_INTEGER,_INT64,_WORD,_UWORD,_BYTE,_UBYTE
*     GENVAR = LOGICAL (Given)
*        Whether or not variances are to be generated for the output
*        data.
*     IPSTK = INTEGER (Given)
*        Pointer to the array of lines which are to be combined into a
*        single line.
*     NPIX = INTEGER (Given)
*        The number of pixels in a line of data.
*     NLINES = INTEGER (Given)
*        The number of lines of data in the stack.
*     VARS( NLINES ) = DOUBLE PRECISION (Given)
*        The variance to to used for each line of data.
*     IMETH = INTEGER (Given)
*        The method to use in combining the lines. Has a code of 2 to 9
*        which represent.
*        2 = MEAN
*        3 = MEDIAN
*        4 = TRIMMED MEAN
*        5 = MODE
*        6 = SIGMA CLIPPED MEAN
*        7 = THRESHOLD EXCLUSION MEAN
*        8 = MINMAX MEAN
*        9 = BROADENED MEDIAN
*        10 = CLIPPED MEDIAN
*        11 = FAST MEDIAN
*     MINPIX = INTEGER (Given)
*        The minimum number of pixels required to contribute to an
*        output pixel.
*     NITER = INTEGER (Given)
*        The maximum number of iterations ( IMETH = 5 and 6 ), NITER
*        should have been set to one for IMETH = 6.
*     NSIGMA = REAL (Given)
*        The number of sigmas to clip the data at (IMETH = 5 and 6 ).
*     ALPHA = REAL (Given)
*        The fraction of data values to remove from data (IMETH = 4 ).
*     RMIN = REAL (Given)
*        The minimum allowed data value ( IMETH = 7 )
*     RMAX = REAL (Given)
*        The maximum allowed data value ( IMETH = 7 )
*     IPRES = Integer (Given and Returned)
*        Pointer to space for the output line of data.
*     IPVAR = Integer (Given and Returned)
*        Pointer to the variance for each output pixel.
*     WRK1( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations.
*     WRK2( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations. Double precision as always need at
*        least real for weights ( inverse variances ).
*     PP( NLINES ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for order statistics calculations.
*     COVEC( NMAT, NLINES ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for storing ordered statistics variance - covariance
*        packed matrix.
*     NCON ( NLINES ) = DOUBLE PRECISION (Returned)
*        The actual number of contributing pixels.
*     POINT( NLINES ) = INTEGER (Given and Returned)
*        Workspace to hold pointers to the original positions of the
*        data before extraction and conversion in to the WRK1 array.
*     USED( NLINES ) =LOGICAL (Given and Returned)
*        Workspace used to indicate which values have been used in
*        estimating a resultant value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      LOGICAL GENVAR
      INTEGER IPSTK
      INTEGER NPIX
      INTEGER NLINES
      DOUBLE PRECISION VARS( NLINES )
      INTEGER IMETH
      INTEGER MINPIX
      INTEGER NITER
      REAL NSIGMA
      REAL ALPHA
      REAL RMIN
      REAL RMAX

*  Arguments Given and Returned:
      INTEGER IPRES
      INTEGER IPVAR
      DOUBLE PRECISION WRK1( NLINES )
      DOUBLE PRECISION WRK2( NLINES )
      DOUBLE PRECISION PP( NLINES )
      INTEGER NMAT
      DOUBLE PRECISION COVEC( NMAT, NLINES )
      INTEGER POINT( NLINES )
      LOGICAL USED( NLINES)

*  Arguments Returned:
      DOUBLE PRECISION NCON( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*.
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Combine the data using the appropriate type routine, generating
*  variances if required.

      IF ( GENVAR ) THEN
*  Process at real versions.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_CM2RB(%VAL( CNF_PVAL( IPSTK ) ),
     :                      NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                      %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )

          ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_CM2RUB( %VAL( CNF_PVAL( IPSTK ) ),
     :                        NPIX, NLINES, VARS, IMETH,
     :                        MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                        %VAL( CNF_PVAL( IPRES ) ),
     :                        %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                        PP, COVEC, NMAT, NCON, POINT, USED,
     :                        STATUS )
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL CCG1_CM2RW( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_CM2RUW( %VAL( CNF_PVAL( IPSTK ) ),
     :                        NPIX, NLINES, VARS, IMETH,
     :                        MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                        %VAL( CNF_PVAL( IPRES ) ),
     :                        %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                        PP, COVEC, NMAT, NCON, POINT,
     :                        USED, STATUS )
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL CCG1_CM2RR( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )

*  All double precision version.
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_CM2DI( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_CM2DD( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL CCG1_CM2DK( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       %VAL( CNF_PVAL( IPVAR ) ), WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED,
     :                       STATUS )
         END IF
      ELSE

*  Else do not generate variances... save effort.
*  Process at real versions.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_CM3RB(%VAL( CNF_PVAL( IPSTK ) ),
     :                      NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                      WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )

          ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_CM3RUB( %VAL( CNF_PVAL( IPSTK ) ),
     :                        NPIX, NLINES, VARS, IMETH,
     :                        MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                        %VAL( CNF_PVAL( IPRES ) ),
     :                        WRK1, WRK2, NCON, POINT,
     :                        USED, STATUS )
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL CCG1_CM3RW( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_CM3RUW( %VAL( CNF_PVAL( IPSTK ) ),
     :                        NPIX, NLINES, VARS, IMETH,
     :                        MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                        %VAL( CNF_PVAL( IPRES ) ),
     :                        WRK1, WRK2, NCON, POINT,
     :                        USED, STATUS )
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL CCG1_CM3RR( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )

*  All double precision version.
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_CM3DI( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_CM3DD( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            CALL CCG1_CM3DK( %VAL( CNF_PVAL( IPSTK ) ),
     :                       NPIX, NLINES, VARS, IMETH,
     :                       MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX,
     :                       %VAL( CNF_PVAL( IPRES ) ),
     :                       WRK1, WRK2, NCON, POINT,
     :                       USED, STATUS )
         END IF
      END IF

      END
* $Id$
