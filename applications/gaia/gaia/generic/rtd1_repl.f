      SUBROUTINE RTD1_REPL( NFIT, MASK, DIM1, DIM2, TYPE, IPIMG,
     :                      HAVVAR, IPVAR, HVQUAL, IPQUAL, BADBIT,
     :                      IPC, IPTX, NX, IPTY, NY, ER, NERROR,
     :                      STATUS )
*+
*  Name:
*     RTD1_REPL

*  Purpose:
*     Replaces masked part of an image with a surface fit.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*      CALL RTD1_REPL( NFIT, MASK, DIM1, DIM2, TYPE, IPIMG, HAVVAR,
*                      IPVAR, HVQUAL, IPQUAL, BADBIT, IPC, IPTX, NX,
*                      IPTY, NY, ER, NERROR, STATUS )

*  Description:
*     This routine replaces the part of the given image that is valid
*     by data value and by being represented as positive in the MASK
*     (probably the output from ARD). The replacement values are those
*     of a spline fit made by the routine PDA_SURFACE.

*  Arguments:
*     NFIT = INTEGER (Given)
*        The number of degrees of freedom used in the FIT.
*     MASK( DIM1, DIM2 ) = INTEGER (Given)
*        A mask for the input IMAGE that contains positive values
*        for any valid pixels.
*     DIM1 = INTEGER (Given)
*        The first dimension of MASK and IMAGE.
*     DIM2 = INTEGER (Given)
*        The second dimension of MASK and IMAGE.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type of the image and variance.
*     IPIMG = INTEGER (Given)
*        Pointer to the image data to modify. The elements of
*        these that are valid and correspond to positive values
*        in the MASK are replaced by the surface plus a noise
*        contribution.
*     HAVVAR = LOGICAL (Given)
*        Whether variance points to data or not. If variances are
*        present then they are replaced by the standard deviation
*        squared of the noise distribution added to the surface.
*     IPVAR = INTEGER (Given)
*        Pointer to the variances if given.
*     HVQUAL = LOGICAL (Given)
*        Whether or not a quality array has been given.
*     IPQUAL = INTEGER (Given)
*        Pointer to the quality data.
*     BADBIT = BYTE (Given)
*        BADBITS mask of quality data.
*     IPC = INTEGER (Given)
*        Pointer to the coefficients of the spline.
*     IPTX = INTEGER (Given)
*        Pointer to the spline X knots.
*     NX = INTEGER (Given)
*        The number of X knots.
*     IPTY = INTEGER (Given)
*        Pointer to the spline Y knots.
*     NY = INTEGER (Given)
*        The number of Y knots.
*     ER( NERROR ) = REAL (Given)
*        Array of gaussian errors. Used to add noise to fit.
*     NERROR = INTEGER (Given)
*        The number of values in errors array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996-2004 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     14-MAR-1996 (PWD):
*        Original version.
*     17-NOV-1997 (PWD):
*        Added changes to support quality reset.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      INTEGER NFIT
      INTEGER DIM1
      INTEGER DIM2
      INTEGER MASK( DIM1, DIM2 )
      CHARACTER * ( * ) TYPE
      INTEGER IPIMG
      LOGICAL HAVVAR
      INTEGER IPVAR
      LOGICAL HVQUAL
      INTEGER IPQUAL
      BYTE BADBIT
      INTEGER IPC
      INTEGER IPTX
      INTEGER NX
      INTEGER IPTY
      INTEGER NY
      INTEGER NERROR
      REAL ER( NERROR )

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants:
      INTEGER MAXORD            ! Maximum order of fit.
      PARAMETER ( MAXORD = 5 )

*  Local Variables:
      INTEGER IERROR            ! Error code
      INTEGER IPWRK1            ! Pointer to workspace
      INTEGER IPWRK2            ! Pointer to workspace
      INTEGER IPX               ! Pointer to X positions
      INTEGER IPY               ! Pointer to X positions
      INTEGER IPZ               ! Pointer to new values
      INTEGER KX                ! Order of fit in X
      INTEGER KY                ! Order of fit in Y
      INTEGER LWRK1             ! Length of workspace
      INTEGER LWRK2             ! Length of workspace
      INTEGER MX                ! Number of points in X
      INTEGER MY                ! Number of points in Y
      INTEGER NVALID            ! Number of valid pixels to replace
      INTEGER XSIZE             ! Size of new region
      INTEGER YSIZE             ! Size of new region
      REAL XMAX                 ! Maximum X value of region
      REAL XMIN                 ! Minimum X value of region
      REAL YMAX                 ! Maximum Y value of region
      REAL YMIN                 ! Minimum Y value of region

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate useful parameters about the masked region.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_MSTB( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_MSTUB( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                    .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                    STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_MSTW( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_MSTUW( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                    .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                    STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL RTD1_MSTK( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_MSTI( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_MSTR( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_MSTD( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .FALSE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE

*  Incorrect data type. Complain and give up.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'RTD1_REPLNOTYPE',
     :   'RTD1_REPL: Unknown data type ^TYPE', STATUS )
         GO TO 99
      END IF

*  Work out span of region to calculate the surface over.
      XSIZE = XMAX - XMIN + 1
      YSIZE = YMAX - YMIN + 1
      IF ( XSIZE .LE. 0 .OR. YSIZE .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RTD1_REPLNONE',
     :   'There is no valid region to replace',  STATUS )
         GO TO 99
      END IF

*  Now create the workspace for image data.
      CALL PSX_CALLOC( XSIZE, '_REAL', IPX, STATUS )
      CALL PSX_CALLOC( YSIZE, '_REAL', IPY, STATUS )

*  And set these to all numbers in between these limits.
      CALL RTD1_SETAR( XMIN, 1.0, XSIZE, %VAL( CNF_PVAL( IPX ) ),
     :                 STATUS )
      CALL RTD1_SETAR( YMIN, 1.0, YSIZE, %VAL( CNF_PVAL( IPY ) ),
     :                 STATUS )

*  And space for square of this size for function evaluation.
      CALL PSX_CALLOC( XSIZE * YSIZE, '_REAL', IPZ, STATUS )

*  Set up variables and workspace for evaluation.
      KX = MIN( NFIT, MAXORD )
      KY = MIN( NFIT, MAXORD )
      MX = XSIZE
      MY = YSIZE
      LWRK1 = MX * ( KX + 1 ) + MY * ( KY + 1 )
      CALL PSX_CALLOC( LWRK1, '_REAL', IPWRK1, STATUS )
      LWRK2 = MX + MY
      CALL PSX_CALLOC( LWRK2, '_INTEGER', IPWRK2, STATUS )

*  Now generate the values we require (overwrite the previous IPZ).
      CALL PDA_BISPEV( %VAL( CNF_PVAL( IPTX ) ), NX,
     :                 %VAL( CNF_PVAL( IPTY ) ), NY,
     :                 %VAL( CNF_PVAL( IPC ) ), KX, KY,
     :                 %VAL( CNF_PVAL( IPX ) ), MX,
     :                 %VAL( CNF_PVAL( IPY ) ), MY,
     :                 %VAL( CNF_PVAL( IPZ ) ),
     :                 %VAL( CNF_PVAL( IPWRK1 ) ), LWRK1,
     :                 %VAL( CNF_PVAL( IPWRK2 ) ), LWRK2, IERROR,
     :                 STATUS )

*  And fill in image with these values.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_DOFILB( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_DOFILUB( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                      %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                      ER, NERROR, HAVVAR, HVQUAL,
     :                      %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                      %VAL( CNF_PVAL( IPIMG ) ),
     :                      %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_DOFILW( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_DOFILUW( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                      %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                      ER, NERROR, HAVVAR, HVQUAL,
     :                      %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                      %VAL( CNF_PVAL( IPIMG ) ),
     :                      %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL RTD1_DOFILK( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_DOFILI( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_DOFILR( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_DOFILD( INT( XMIN ), INT( YMIN ), XSIZE, YSIZE,
     :                     %VAL( CNF_PVAL( IPZ ) ), DIM1, DIM2, MASK,
     :                     ER, NERROR, HAVVAR, HVQUAL,
     :                     %VAL( CNF_PVAL( IPQUAL ) ), BADBIT,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ), STATUS )
      END IF

*  And free memory.
      CALL PSX_FREE( IPX, STATUS )
      CALL PSX_FREE( IPY, STATUS )
      CALL PSX_FREE( IPZ, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
 99   CONTINUE
      END
