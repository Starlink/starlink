      SUBROUTINE RTD1_DOFIT( NFIT, MASK, DIM1, DIM2, TYPE,
     :                       HAVVAR, IPVAR, IPIMG, IPC, IPTX, NX,
     :                       IPTY, NY, FP, STATUS )
*+
*  Name:
*     RTD1_DOFIT

*  Purpose:
*     Fits a surface to the masked parts of an array.

*  Language:
*     Starlink Fortran-77.

*  Invocation:
*     CALL RTD1_DOFIT( NFIT, MASK, DIM1, DIM2, TYPE, HAVVAR, IPVAR,
*                      IPIMG, IPC, IPTX, NX, IPTY, NY, FP, STATUS )

*  Description:
*     This routine fits a surface to the valid parts of IMAGE. Valid
*     parts are those with a non-bad value and that have a value of
*     greater than zero in the INTEGER array MASK (this is usually an
*     ARD generated array). The fit is a polynomial of degree
*     NFIT in X and Y. Variances may be used to weight the fit
*     if available.
*
*     The fit is described by the coefficients IPC and the knots
*     IPTX and IPTY, which should be passed to PDA_BISPEV for
*     evaluation.

*  Arguments:
*     NFIT = INTEGER (Given)
*        The number of degrees of freedom in the FIT.
*     MASK( DIM1, DIM2 ) = INTEGER (Given)
*        A mask for the input IMAGE that contains positive values
*        for any valid pixels.
*     DIM1 = INTEGER (Given)
*        The first dimension of MASK and IMAGE.
*     DIM2 = INTEGER (Given)
*        The second dimension of MASK and IMAGE.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type of the data pointed to by IPIMG. Any
*        variances are also assumed to be in this type.
*     HAVVAR = LOGICAL (Given)
*        Whether or not any variances are available for the image
*        data. These are used as weights in the surface fit.
*     IPVAR = INTEGER (Given)
*        Pointer to variance data if it exists.
*     IPIMG = INTEGER (Given)
*        Pointer to the Image data to be fit.
*     IPC = INTEGER (Returned)
*        Pointer to dynamic memory that contains the coefficients
*        of the spline.
*     IPTX = INTEGER (Returned)
*        Pointer to the spline X knots.
*     NX = INTEGER (Returned)
*        The number of X knots.
*     IPTY = INTEGER (Returned)
*        Pointer to the spline Y knots.
*     NY = INTEGER (Returned)
*        The number of X knots.
*     FP = REAL (Returned)
*        The standard deviation of the residuals to the fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*      The maximum order of the fits is 5. Never actually got a decent
*      spline fit, fortunately for our case polynomials seem good
*      enough. Seems to be very dependent on value of S, if
*      selecting own knots.

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
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Arguments Given:
      INTEGER NFIT
      INTEGER DIM1
      INTEGER DIM2
      INTEGER MASK( DIM1, DIM2 )
      CHARACTER * ( * ) TYPE
      LOGICAL HAVVAR
      INTEGER IPVAR
      INTEGER IPIMG

*  Arguments Returned:
      INTEGER IPC
      INTEGER IPTX
      INTEGER NX
      INTEGER IPTY
      INTEGER NY
      REAL FP

*  Status:
      INTEGER STATUS            ! Global status

*  Local parameters:
      INTEGER MAXORD            ! Maximum order of fit.
      PARAMETER ( MAXORD = 5 )

*  Local Variables:
      INTEGER B1                ! Workspace factor
      INTEGER B2                ! Workspace factor
      INTEGER BX                ! Workspace factor
      INTEGER BY                ! Workspace factor
      INTEGER CSIZE             ! Size of coefficients array
      INTEGER IERROR            ! Error code
      INTEGER IPW               ! Pointer to valid weights
      INTEGER IPWRK1            ! Workspace array pointer
      INTEGER IPWRK2            ! Workspace array pointer
      INTEGER IPWRK3            ! Workspace array pointer
      INTEGER IPX               ! Pointer to valid X values
      INTEGER IPY               ! Pointer to valid Y values
      INTEGER IPZ               ! Pointer to valid data values
      INTEGER NMAX              ! Maximum number of knots
      INTEGER KM                ! Workspace factor
      INTEGER KX                ! Number of X degrees of spline
      INTEGER KY                ! Number of X degrees of spline
      INTEGER LWRK1             ! Length of workspace
      INTEGER LWRK2             ! Length of workspace
      INTEGER LWRK3             ! Length of workspace
      INTEGER NE                ! Workspace factor
      INTEGER NVALID            ! Number of valid values
      INTEGER NXEST             ! Number of knots in X
      INTEGER NYEST             ! Number of knots in Y
      INTEGER U                 ! Workspace factor
      INTEGER V                 ! Workspace factor
      REAL XMAX                 ! Maximum valid X value
      REAL XMIN                 ! Minimum valid X value
      REAL YMAX                 ! Maximum valid Y value
      REAL YMIN                 ! Minimum valid Y value
      REAL S                    ! Closeness of fit parameter
      REAL EPSR                 ! 10**(real significant figures)
      REAL SIGSUM               ! Sum of weights
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Estimate the size of the workspace that we will require for
*  containing the lists of X, Y and data values.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_MSTB( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_MSTUB( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                    .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                    STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_MSTW( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_MSTUW( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                    .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                    STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_MSTI( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL RTD1_MSTK( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_MSTR( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_MSTD( MASK, DIM1, DIM2, %VAL( CNF_PVAL( IPIMG ) ),
     :                   .TRUE., XMIN, XMAX, YMIN, YMAX, NVALID,
     :                   STATUS )
      ELSE

*  Incorrect data type. Complain and give up.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'RTD1_DOFIT',
     :   'RTD1_DOFIT: Unknown data type ^TYPE', STATUS )
         GO TO 99
      END IF

*  If no valid pixels then stop now.
      IF ( NVALID .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RTD1_DOFITNOPIX',
     : 'Fit region contains no pixels.', STATUS )
         GO TO 99
      END IF

*  Now create the workspace for image data.
      CALL PSX_CALLOC( NVALID, '_REAL', IPX, STATUS )
      CALL PSX_CALLOC( NVALID, '_REAL', IPY, STATUS )
      CALL PSX_CALLOC( NVALID, '_REAL', IPZ, STATUS )
      CALL PSX_CALLOC( NVALID, '_REAL', IPW, STATUS )

*  And copy the data values into these.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_SUFITB( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_SUFITUB( MASK,
     :                      %VAL( CNF_PVAL( IPIMG ) ),
     :                      %VAL( CNF_PVAL( IPVAR ) ),
     :                      DIM1, DIM2, HAVVAR,
     :                      %VAL( CNF_PVAL( IPX ) ),
     :                      %VAL( CNF_PVAL( IPY ) ),
     :                      %VAL( CNF_PVAL( IPZ  ) ),
     :                      %VAL( CNF_PVAL( IPW  ) ),
     :                      NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_SUFITW( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_SUFITUW( MASK,
     :                      %VAL( CNF_PVAL( IPIMG ) ),
     :                      %VAL( CNF_PVAL( IPVAR ) ),
     :                      DIM1, DIM2, HAVVAR,
     :                      %VAL( CNF_PVAL( IPX ) ),
     :                      %VAL( CNF_PVAL( IPY ) ),
     :                      %VAL( CNF_PVAL( IPZ  ) ),
     :                      %VAL( CNF_PVAL( IPW  ) ),
     :                      NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_SUFITI( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL RTD1_SUFITK( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_SUFITR( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_SUFITD( MASK,
     :                     %VAL( CNF_PVAL( IPIMG ) ),
     :                     %VAL( CNF_PVAL( IPVAR ) ),
     :                     DIM1, DIM2, HAVVAR,
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     %VAL( CNF_PVAL( IPZ  ) ),
     :                     %VAL( CNF_PVAL( IPW  ) ),
     :                     NVALID, SIGSUM, STATUS )
      END IF

*  Set up values for surface fit. Set S and NXEST and NYEST for
*  polynomial behaviour.
      KX = MIN( NFIT, MAXORD )
      KY = MIN( NFIT, MAXORD )
      S = VAL__MAXR
      NXEST = 2 * ( KX + 1 )
      NYEST = 2 * ( KY + 1 )

*  Create workspace for surface fitting routine (this is derived from
*  notes in PDA_SURFACE).
      U = NXEST - KX - 1
      V = NYEST - KY - 1
      KM = MAX( KX, KY ) + 1
      NE = MAX( NXEST, NYEST )
      BX = KX * V + KY + 1
      BY = KY * U + KX + 1
      IF ( BX .LE. BY ) THEN
         B1 = BX
         B2 = B1 + V - KY
      ELSE
         B1 = BY
         B2 = B1 + U - KX
      END IF

      CSIZE = ( NXEST - KX - 1 ) * ( NYEST - KY - 1 )
      CALL PSX_CALLOC( CSIZE, '_REAL', IPC, STATUS )

      NMAX = MAX( NXEST, NYEST )
      CALL PSX_CALLOC( NMAX, '_REAL', IPTX, STATUS )
      CALL PSX_CALLOC( NMAX, '_REAL', IPTY, STATUS )

      LWRK1 = U * V * ( 2 + B1 + B2 ) +
     :        2 * ( U + V + KM * ( NVALID + NE ) +NE - KX - KY ) +
     :        B2 + 1
      CALL PSX_CALLOC( LWRK1, '_REAL', IPWRK1, STATUS )

      LWRK2 = U * V * ( B2 + 1 ) + B2
      CALL PSX_CALLOC( LWRK2, '_INTEGER', IPWRK2, STATUS )

      LWRK3 = NVALID + ( NXEST - 2 * KX - 1 ) * ( NYEST - 2 * KY - 1 )
      CALL PSX_CALLOC( LWRK3, '_INTEGER', IPWRK3, STATUS )

*  And fit the surface.
      IF ( STATUS .EQ. SAI__OK ) THEN
         EPSR = VAL__EPSR
         CALL PDA_SURFIT( 0, NVALID,
     :                    %VAL( CNF_PVAL( IPX ) ),
     :                    %VAL( CNF_PVAL( IPY ) ),
     :                    %VAL( CNF_PVAL( IPZ ) ),
     :                    %VAL( CNF_PVAL( IPW ) ),
     :                    XMIN, XMAX, YMIN, YMAX, KX, KY, S, NXEST,
     :                    NYEST, NMAX, EPSR, NX,
     :                    %VAL( CNF_PVAL( IPTX ) ), NY,
     :                    %VAL( CNF_PVAL( IPTY ) ),
     :                    %VAL( CNF_PVAL( IPC ) ), FP,
     :                    %VAL( CNF_PVAL( IPWRK1 ) ), LWRK1,
     :                    %VAL( CNF_PVAL( IPWRK2 ) ), LWRK2,
     :                    %VAL( CNF_PVAL( IPWRK3 ) ),
     :                    LWRK3, IERROR )
         IF ( IERROR .LT. -2 .OR. IERROR .GT. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IERROR', IERROR )
            CALL ERR_REP( 'RTD1_NOFIT', 'Unable to fit surface to '//
     :'data (^IERROR).', STATUS )
         END IF
      END IF

*  FP to standard deviation. Weights are already SDs if using variances.
      IF ( NVALID .GT. 0 ) THEN
         IF ( HAVVAR ) THEN
            FP = FP / SIGSUM
         ELSE
            FP = SQRT( FP / REAL( NVALID ) )
         END IF
      END IF

*  Free all dynamic memory.
      CALL PSX_FREE( IPX, STATUS )
      CALL PSX_FREE( IPY, STATUS )
      CALL PSX_FREE( IPZ, STATUS )
      CALL PSX_FREE( IPW, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPWRK3, STATUS )

 99   CONTINUE
      END
