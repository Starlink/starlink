      SUBROUTINE CCD1_DEBIA( ITYPE, BAD, GENVAR, USEEXT, ID, EL,
     :                       IPIN, IDIM1, IDIM2, XORIG, YORIG, IPOUT,
     :                       GOTBIA, ZEROED, ZEROCK, IPBIAS, HAVBV,
     :                       IPBVAR, HAVIV, IPOVAR, IPWRK, PRESER,
     :                       DTYPE, IDBIAS, ADC, STATUS )
*+
*  Name:
*     CCD1_DEBIA

*  Purpose:
*     Get parameters associated with subtracting a bias component from
*     a CCD frame, and calls the debiasing routines

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DEBIA( ITYPE, BAD, GENVAR, USEEXT, ID, EL, IPIN, IDIM1,
*                      IDIM2, XORIG, YORIG, IPOUT, GOTBIA, ZEROED,
*                      ZEROCK, IPBIAS, HAVBV, IPBVAR, HAVIV, IPOVAR,
*                      IPWRK, PRESER, DTYPE, IDBIAS, ADC, STATUS )

*  Description:
*     This routine is mostly a dummy to the main routine CCD1_DEBI<T>,
*     it calls the appropriate DEBI on the basis of the ITYPE. The
*     arrays required for DEBI are passed to this routine. All NDF
*     handling is therefore achieved in the level above this,
*     parameters concerned with the debiasing are accessed here.
*
*     The variance handling is performed in the routine. If an input
*     variance has been given then it is copied into the output
*     variance. An estimate of the possionian noise plus the readout
*     noise is then added to this if GENVAR is true. If GENVAR is false
*     then no variance generation occurs. These values are then the
*     estimates of the variances of the bias subtracted CCD data.
*
*     If USEEXT is TRUE then the ADC and BIAS bounds related values
*     etc. are extracted from the NDF extensions. If these do not exist
*     then the parameter system values are used instead.
*
*     If ZEROCK is TRUE then checks will be made to see if the
*     processing of any bias master is sensible given whether it has
*     been zeroed or not.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The data type of the given arrays,
*        any numeric type is permissable.
*     BAD = LOGICAL (Given and Returned)
*        Whether there are BAD pixels present or not.
*     GENVAR = LOGICAL (Given)
*        If true then variances for the counts in the data array are
*        generated.
*     USEEXT = LOGICAL (Given)
*        If true then the CCD related parameters will be extracted from
*        the NDF extension if they exist.
*     ID = INTEGER (Given)
*        The input NDF identifier.
*     EL = INTEGER (Given)
*        The size of the data arrays.
*     IPIN = INTEGER (Given)
*        Pointer to array containing the data to be debiased.
*     IDIM1 = INTEGER (Given)
*        First dimension of input array.
*     IDIM2 = INTEGER (Given)
*        Second dimension of input array.
*     XORIG = INTEGER (Given)
*        X origin of input arrays (pixel indices).
*     YORIG = INTEGER (Given)
*        Y origin of input arrays (pixel indices).
*     IPOUT = INTEGER (Given)
*        Pointer to output data array.
*     GOTBIA = LOGICAL (Given)
*        Set to true if have a bias frame to subtract.
*     ZEROED = LOGICAL (Given)
*        Only used if ZEROCK is TRUE. This then indicates whether the
*        master bias has been zeroed during its creation or not. If it
*        has been zeroed then an attempt to subtract it without
*        offsetting will result in a warning message being issued,
*        likewise if the bias has not been zeroed and an attempt to
*        directly subtract it is made.
*     ZEROCK = LOGICAL (Given)
*        Whether or not to check that the ZEROED condition is sensible
*        for the chosen debiassing method.
*     IPBIAS
*        Pointer to bias data array.
*     HAVBV = LOGICAL (Given)
*        If true then bias array has an associated variance component.
*     IPBVAR = INTEGER (Given)
*        Pointer bias variance component.
*     HAVIV = LOGICAL (Given)
*        Set true if input data had some associated variances. These
*        should have been propagated to the output array and be pointed
*        to by IPOVAR.
*     IPOVAR = INTEGER (Given)
*        Pointer to the output variance component.
*     IPWRK = INTEGER (Given)
*        Pointer to workspace array of size at least IDIM1*IDIM2.
*        (ITYPE)
*     PRESER = LOGICAL (Given)
*        Whether the input data precision will be preserved on output.
*        (used in logfile)
*     DTYPE = CHRACTER * ( * ) (Given)
*        The full type of the input data (not used) this version.
*     IDBIAS = INTEGER (Given)
*        Identifier for the bias NDF.
*     ADC = DOUBLE PRECISION (Returned)
*        ADC conversion factor
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 1995, 1998 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     24-APR-1991 (PDRAPER):
*        Original version.
*     28-OCT-1991 (PDRAPER):
*        Changed to use one less full sized workspace array.
*     10-DEC-1991 (PDRAPER):
*        Changed to fully generic
*     29-SEP-1993 (PDRAPER):
*        Added USEEXT parameter and all its consequences (automated
*        processing developments).
*     1-FEB-1994 (PDRAPER):
*        Added ZEROED and ZEROCK.
*     13-JUN-1995 (PDRAPER):
*        Sigma clipping now corrected to work about mean, was using +/-
*        about zero.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     03-FEB-1998 (PDRAPER):
*        Stopped modification of global zero level (was set to UMEAN).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      CHARACTER * ( * ) DTYPE
      INTEGER EL
      INTEGER ID
      INTEGER IDIM1
      INTEGER IDIM2
      INTEGER XORIG
      INTEGER YORIG
      INTEGER IPBIAS
      INTEGER IPIN
      INTEGER IPWRK
      INTEGER IPBVAR
      INTEGER IDBIAS
      LOGICAL GOTBIA
      LOGICAL ZEROED
      LOGICAL ZEROCK
      LOGICAL OFFSET
      LOGICAL HAVBV
      LOGICAL BAD
      LOGICAL PRESER
      LOGICAL GENVAR
      LOGICAL HAVIV
      LOGICAL USEEXT

*  Arguments Returned:
      INTEGER IPOUT
      INTEGER IPOVAR
      DOUBLE PRECISION ADC

*  Local variables:
      CHARACTER * ( 20 ) CMODE   ! Clean up mode
      CHARACTER * ( 20 ) FMODE   ! Fit mode
      CHARACTER * ( 20 ) SMODE   ! Sense of fit
      CHARACTER * ( 20 ) WMODE   ! Weighting mode
      DOUBLE PRECISION MEAN      ! Mean value
      DOUBLE PRECISION RNOISE    ! Readout noise
      DOUBLE PRECISION UMEAN     ! Mean value in zeros (unweighted)
      INTEGER BOUNDS( 4 )        ! Bias strips bounds
      INTEGER DIRECT             ! Readout direction
      INTEGER IDD                ! Workspace identifier
      INTEGER IDI                ! Workspace identifier
      INTEGER IDR                ! Workspace identifier
      INTEGER IPIDD              ! Pointer to weights workspace
      INTEGER IPIDI              ! Pointer to block filter workspace
      INTEGER IPIDR              ! Pointer to block filter workspace
      INTEGER NBOUND             ! number of bounds (in pairs .lt. 4 )
      INTEGER NBOX               ! Number of box sizes returned
      INTEGER NDIR               ! Number of elements along readout direction
      INTEGER NERR               ! Number of numeric errors
      INTEGER SIZE( 2 )          ! Boxfilter sizes
      LOGICAL EXTADC             ! ADC factor from extension
      LOGICAL EXTBDS             ! Bias strip bounds from extension
      LOGICAL EXTBIA             ! Bias value from extension
      LOGICAL EXTDIR             ! Readout direction from extension
      LOGICAL EXTNOI             ! Readout noise from extension
      LOGICAL USECON             ! Whether to subtract a constant or not.
      REAL NSIGMA                ! Number of sigma for rejection.

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisations.
      OFFSET = .FALSE.
      CMODE = ' '
      SMODE = ' '
      FMODE = ' '
      WMODE = ' '
      IPIDD = 0
      IPIDI = 0
      IPIDR = 0
      RNOISE = 0.0D0

      IF ( GOTBIA ) THEN

*  Does the user require the bias frame offset to the mean value of the
*  bias strip, or not ? If not the bias will just be subtracted.
*  Offsetting is NOT allowed for unsigned data types. If ZEROCK is TRUE
*  then check this decision against the ZEROED value.
         IF ( ITYPE( 1 : 2 ) .NE. '_U' ) THEN
            CALL PAR_GET0L( 'OFFSET', OFFSET, STATUS )
         END IF
         IF ( ZEROCK ) THEN
            IF ( ZEROED .AND. .NOT. OFFSET ) THEN

*  Should be offset if the master is zeroed.
               CALL CCD1_MSG( ' ',
     :' Warning - should offset a zeroed master bias', STATUS )
            ELSE IF ( .NOT. ZEROED .AND. OFFSET ) THEN

*  Shouldn't offset if the master isn't zeroed.
               CALL CCD1_MSG( ' ',
     :' Warning - should not offset an unzeroed master bias', STATUS )
            END IF
         END IF
      END IF

*  Set nbound to 1 to stop dimension errors, when not using bounds.
      NBOUND = 1

*  Set NDIR to 0. Use this as convenient flag for weights W/S usage.
      NDIR = 0

*  Initialise values so that it appears that they have not been obtained
*  from the NDF extension (default).
      EXTBIA = .FALSE.
      EXTDIR = .FALSE.
      EXTBDS = .FALSE.
      EXTNOI = .FALSE.
      EXTADC = .FALSE.

*  Do we require bias strips or not ? If so then need the readout
*  direction and the bias strip bounds, however the first question is do
*  we have a bias if not do we just want to use a constant for the bias
*  level ?
      USECON = .FALSE.
      IF ( OFFSET .OR. .NOT. GOTBIA ) THEN
         IF ( .NOT. GOTBIA ) THEN
            CALL PAR_GET0L( 'USECON', USECON, STATUS )
         END IF
         IF ( USECON ) THEN

*  Just use a constant as the estimate of the bias level. Can we get
*  this value from the NDF extension?
            IF ( USEEXT ) THEN
               CALL CCG1_FCH0D( ID, 'ZERO', MEAN, EXTBIA, STATUS )
            END IF
            IF ( .NOT. EXTBIA ) THEN
               CALL PAR_GET0D( 'ZERO', MEAN, STATUS )
            END IF

*  Set the fmode to indicate that a global constant is to be subtracted.
            FMODE = 'GLOBAL'
         ELSE

*  Get the 'Readout' Direction. The section extents run perpendicular to
*  this.
            CALL CCD1_GTDIR( USEEXT, ID, DIRECT, EXTDIR, STATUS )

*  Get the pixel indices of the extent of the zero regions, must be
*  supplied in pairs. Correct these bounds for origin offset. The
*  resultant numbers will be array offsets, not pixel indices.
            IF ( DIRECT .EQ. 1 ) THEN
               CALL CCD1_GTBDS( USEEXT, ID, IDIM1, XORIG, 4, BOUNDS,
     :                          NBOUND, EXTBDS, STATUS )
               NDIR = IDIM1
            ELSE
               CALL CCD1_GTBDS( USEEXT, ID, IDIM2, YORIG, 4, BOUNDS,
     :                          NBOUND, EXTBDS, STATUS )
               NDIR = IDIM2
            END IF

*  Is the interpolation by a plane or by lines, or by just subtracting
*  a constant.
            IF ( .NOT. GOTBIA ) THEN

*  If nbound is equal to 2 then cannot allow linear interpolation.
*  Get the interpolation sense.
               IF ( NBOUND .EQ. 2 ) THEN
                  CALL PAR_CHOIC( 'SMODE', ' ', 'CONSTANT', .FALSE.,
     :                            SMODE, STATUS )
               ELSE
                  CALL PAR_CHOIC( 'SMODE', ' ', 'CONSTANT,LINEAR',
     :                            .FALSE., SMODE, STATUS )
               END IF

*  Do we want to subtract this using a plane or line by line ?
               CALL PAR_CHOIC( 'FMODE', ' ', 'PLANE,LINE', .FALSE.,
     :                         FMODE, STATUS )
            END IF

*  What about the clean up operation - do we want box filtering, sigma
*  clipping or weighting from the edges?
            CALL PAR_CHOIC( 'CMODE', ' ', 'BOX,SIGMA,WEIGHT', .FALSE.,
     :                      CMODE, STATUS )

*  Get the box size or sigma clipping level , if required .
            IF ( CMODE .EQ. 'BOX' ) THEN
               CALL PAR_GET1I( 'BOXSIZE', 2, SIZE, NBOX, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 99
               IF ( NBOX.EQ. 1 ) THEN
                  SIZE( 2 ) = SIZE( 1 )
               END IF
               SIZE( 1 ) = MAX( SIZE( 1 ), 1 ) / 2
               SIZE( 2 ) = MAX( SIZE( 2 ), 1 ) / 2
            ELSE IF ( CMODE .EQ. 'SIGMA' ) THEN

*  Get the number of sigma to clip at.
               CALL PAR_GET0R( 'NSIGMA', NSIGMA, STATUS )
            END IF

*  weighting is always chosen, what sort, none, linear or exponential.
            CALL PAR_CHOIC( 'WMODE', ' ', 'LINEAR,EXP,NONE', .FALSE.,
     :                      WMODE, STATUS )

*  Get workspace for linear weights along readout line.
            CALL CCD1_MKTMP( NDIR, '_DOUBLE', IDD, STATUS )

*  Map the line in and set the data values to VAL__BADD
            CALL CCD1_MPTMP( IDD, 'WRITE', IPIDD, STATUS )
            CALL CCD1_STVB( '_DOUBLE', NDIR, IPIDD, STATUS )
         END IF
      END IF

*  If we have some bounds then can estimate the noise levels.
      IF ( FMODE .NE. 'GLOBAL' .AND. NBOUND .GT. 1 ) THEN

*  Can estimate the readout noise and zero level, and inform the user
          CALL CCD1_ESTM( ITYPE, DIRECT, IPIN, IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, IPWRK, UMEAN, RNOISE, STATUS )

*  Reset the work space to BAD.
         CALL CCD1_STVB( ITYPE, EL, IPWRK, STATUS )

*  Write out the readout noise value.
         CALL MSG_SETR( 'RNOISE_VAL', REAL( RNOISE ) )
         CALL MSG_OUT( 'RNOISE_MESS',
     :    '  Estimated readout noise: ^RNOISE_VAL', STATUS )
         CALL MSG_SETR( 'MEAN_VAL', REAL( UMEAN ) )
         CALL MSG_OUT( 'MEAN_MESS',
     :    '  Estimated zero level: ^MEAN_VAL', STATUS )

*  Set the dynamic default.
         CALL PAR_DEF0D( 'RNOISE', RNOISE, STATUS )
      END IF

*  Get the readout noise level. Only get this if generating variances.
*  May get a value directly from the NDF for this value.
      IF ( GENVAR ) THEN
         IF ( USEEXT ) THEN

*  Try to get a value from the NDF.
            CALL CCG1_FCH0D( ID, 'RNOISE', RNOISE, EXTNOI, STATUS )
          END IF

*  If failed to get value from NDF or we're not using NDF extensions
*  then prompt the user to get a value.
          IF ( .NOT. EXTNOI ) THEN
             CALL PAR_GET0D( 'RNOISE', RNOISE, STATUS )
          END IF

*  Get the ADC factor. Look in NDF extension if required. If this fails
*  or otherwise get a value from the user.
         IF ( USEEXT ) THEN
            CALL CCG1_FCH0D( ID, 'ADC', ADC, EXTADC, STATUS )
         END IF
         IF ( .NOT. EXTADC ) THEN
            CALL PAR_GET0D( 'ADC', ADC, STATUS )
         END IF
      END IF

*  Do the actual processing.
      IF ( CMODE .EQ. 'BOX' ) THEN

*  Get some extra workspace for block filtering
         CALL CCD1_MKTMP( IDIM1, '_INTEGER', IDI, STATUS )
         CALL CCD1_MPTMP( IDI, 'WRITE', IPIDI, STATUS )
         CALL CCD1_MKTMP( IDIM1, '_DOUBLE', IDR, STATUS )
         CALL CCD1_MPTMP( IDR, 'WRITE', IPIDR, STATUS )
      ELSE

*  Set level to use for +/- sigma clip.
         IF ( .NOT. USECON ) THEN
            MEAN = UMEAN
         END IF
      END IF

*  Call the appropriate version of DEBI
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_DEBIB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_DEBIUB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                     IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_DEBIW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_DEBIUW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                     IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_DEBII( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_DEBIR( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_DEBID( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )
      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL CCG1_DEBIK( BAD, EL, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2,
     :                    %VAL( CNF_PVAL( IPOUT ) ), GOTBIA,
     :                    %VAL( CNF_PVAL( IPBIAS ) ),
     :                    OFFSET, FMODE, SMODE, CMODE, WMODE, SIZE,
     :                    NSIGMA, MEAN, RNOISE, DIRECT, BOUNDS, NBOUND,
     :                    %VAL( CNF_PVAL( IPIDD ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    %VAL( CNF_PVAL( IPIDI ) ),
     :                    %VAL( CNF_PVAL( IPIDR ) ), STATUS )

      ELSE
*  Else unsupported type - stop.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ITYPE', ITYPE )
         CALL ERR_REP( 'CCD1_DEBIA1',
     :   '  CCD1_DEBIA: does not support the processing of data of'//
     :   ' type ^ITYPE', STATUS )
         GO TO 99
      END IF

*  Release the workspace.
      IF ( CMODE .EQ. 'BOX' ) THEN
         CALL CCD1_FRTMP( IDI, STATUS )
         CALL CCD1_FRTMP( IDR, STATUS )
      END IF
      IF ( NDIR .NE. 0 ) THEN
         CALL CCD1_FRTMP( IDD, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Perform the variance manipulations.
*  If have bias variance then do nothing unless also have input
*  variances (previously propagated to the output variance array)
*  or are to generate some.
      IF ( GOTBIA  .AND.  HAVBV ) THEN
         IF ( GENVAR ) THEN

*  If generating variances just overwrite any previous value with the
*  bias array component.
            CALL CCD1_COPY( ITYPE, EL, IPBVAR, IPOVAR, STATUS )
         ELSE IF ( HAVIV ) THEN

*  Not generating variances, but have an bias variance and an input
*  variance - just add these.
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL CCG1_ADDB( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL CCG1_ADDUB( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                          %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL CCG1_ADDW( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL CCG1_ADDUW( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                          %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL CCG1_ADDI( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL CCG1_ADDR( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL CCG1_ADDD( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               CALL CCG1_ADDK( BAD, EL, %VAL( CNF_PVAL( IPOVAR )),
     :                         %VAL( CNF_PVAL( IPBVAR ) ),
     :                         NERR, STATUS )
            END IF

*  Check for numeric overflows etc.
            IF ( NERR .NE. 0 ) THEN
               BAD = .TRUE.

*  Report number of errors and continue.
               CALL MSG_SETI( 'NERR', NERR )
               CALL CCD1_MSG( 'CCD1_DEBIA1',
     :         '  ^NERR numeric errors HAVe occurred when '//
     :         'processing in variance contributions', STATUS )
            END IF
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Form the poisson stats (plus the readout estimates) and add to any
*  variances already present.
      IF ( GENVAR ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_GVARB( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_GVARUB( %VAL( CNF_PVAL( IPOUT ) ),
     :                        %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL CCG1_GVARW( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_GVARUW( %VAL( CNF_PVAL( IPOUT ) ),
     :                        %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_GVARI( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL CCG1_GVARR( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_GVARD( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL CCG1_GVARK( %VAL( CNF_PVAL( IPOUT ) ),
     :                       %VAL( CNF_PVAL( IPOVAR ) ), EL, RNOISE,
     :                       ADC, NERR, STATUS )
         END IF
*  Check for numeric overflows etc.
         IF ( NERR .NE. 0 ) THEN

*  Report number of errors and continue.
            CALL MSG_SETI( 'NERR', NERR )
            CALL CCD1_MSG( 'CCG1_DEBI1',
     :      '  ^NERR numeric errors have occurred when '//
     :      'generating variance estimates', STATUS )
         END IF
      END IF

*  Report DEBIAS parameters
      CALL CCD1_RDBI( CMODE, FMODE, SMODE, WMODE, ITYPE, DTYPE,
     :                MEAN, EXTBIA, UMEAN, GENVAR, RNOISE, EXTNOI,
     :                ADC, EXTADC, BOUNDS, NBOUND, EXTBDS,
     :                XORIG, YORIG, DIRECT, EXTDIR, IDBIAS, SIZE,
     :                GOTBIA, OFFSET, PRESER, NSIGMA, STATUS )

99    CONTINUE

      END
* $Id$

