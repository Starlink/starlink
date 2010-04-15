      SUBROUTINE POL1_SRTIM( RANGE, MININ, IGRP1, NNDF, NBIN,
     :                       ORIGIN, BIN, ANGRT, WORK, NOUT, PHI, NDIMO,
     :                       LBND, UBND, STATUS )
*+
*  Name:
*     POL1_SRTIM

*  Purpose:
*     Identify the analysis angle bin to which each supplied NDF belongs,
*     and return lists of the input NDFs to include in each output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SRTIM( RANGE, MININ, IGRP1, NNDF, NBIN, ORIGIN,
*                      BIN, ANGRT, WORK, NOUT, PHI, NDIMO, LBND, UBND,
*                      STATUS )

*  Description:
*     This routine identifies the analysis angle bin to which each
*     supplied NDF belongs, and returns lists of the input NDFs to
*     include in each output NDF.

*  Arguments:
*     RANGE = REAL (Given)
*        The binning range. Should be either 180.0 or 360.0.
*     MININ = INTEGER (Given)
*        The minimum number of input NDFs required to produce an output
*        NDF.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names.
*        These should be aligned pixel-for-pixel.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the supplied group.
*     NBIN = INTEGER (Given)
*        The number of analysis angle bins.
*     ORIGIN = REAL (Given)
*        The analysis angle at the start of the first bin, in degrees.
*     BIN = REAL (Given)
*        The size of each analysis angle bin, in degrees.
*     ANGRT = REAL (Returned)
*        The ACW angle from the +ve X axis in each output image to the
*        output reference direction, in degrees.
*     WORK( NBIN, -6 : NNDF ) = INTEGER (Returned)
*        An array containing a column for each analysis angle bin.
*        The row 0 contains the number of input NDFs in the bin. If
*        this value is N, then rows 1 to N contain a list of the N
*        input NDFs in the bin. Each NDF is identified by its index
*        within the supplied group. Rows -1 to -6 contains the pixel
*        bounds which span just the corresponding input NDFs, in the order
*        LBND1, LBND2, UBND1, UBND2, (LBND3, UBND3).
*     NOUT = INTEGER (Returned)
*        The number of analysis angle bins containing MININ or more input NDFs.
*     PHI( NNDF ) = INTEGER (Returned)
*        The acw angle from output ref. direction (see ANGRT) to the
*        effective analyser position in each input NDF, in degrees.
*     NDIMO = INTEGER (Returned)
*        The number of axes in the output stack; 3 if the inputs are 2d, and 4
*        if the inputs are 3d.
*     LBND( 4 ) = INTEGER (Returned)
*        Lower pixel index bounds which span all the input images. The
*        last (NDIMO'th) axis is set to 1.
*     UBND( 4 ) = INTEGER (Returned)
*        Upper pixel index bounds which span all the input images. The
*        last (NDIMO'th) axis is set to NOUT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
*        Original version.
*     26-MAY-1999 (DSB):
*        Added RANGE argument.
*     1-JUL-1999 (DSB):
*        Added ORIGIN and ILEVEL arguments.
*     19-FEB-2001 (DSB):
*        Modified to support 3D data.
*     31-JUL-2009 (TIMJ):
*        Remove ILEVEL. Use MSG filtering.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      REAL RANGE
      INTEGER MININ
      INTEGER IGRP1
      INTEGER NNDF
      INTEGER NBIN
      REAL ORIGIN
      REAL BIN

*  Arguments Returned:
      REAL ANGRT
      INTEGER WORK( NBIN, -6 : NNDF )
      INTEGER NOUT
      REAL PHI( NNDF )
      INTEGER NDIMO
      INTEGER LBND( 4 )
      INTEGER UBND( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NDFNAM*(GRP__SZFNM)! NDF name from supplied group
      CHARACTER RAY*1            ! Dual beam ray identification
      CHARACTER XLOC*(DAT__SZLOC)! Locator to POLPACK extension
      INTEGER I                  ! Index of current input NDF
      INTEGER IBIN               ! Current bin index
      INTEGER IGNORE             ! No. of input NDFs not in any bin
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER IWCS               ! AST pointer to a WCS FrameSet
      INTEGER LBNDI( 3 )         ! Lower bounds of input NDF
      INTEGER NAX                ! Required no. of axes
      INTEGER NDIM               ! No. of axes in input NDF
      INTEGER NIN                ! No. of input NDFs in the current bin
      INTEGER UBNDI( 3 )         ! Upper bounds of input NDF
      LOGICAL THERE              ! Does item exists?
      REAL ALPHA                 ! Angle from analyser to PRD
      REAL ANGROT                ! Angle from first image axis to the SRD
      REAL H                     ! Angle from ref.direction to half-wave plate
*.

*  initialise
      NOUT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of NDFs in each bin to zero, and set the
*  pixel bounds for all bins.
      DO I = 1, NBIN
         WORK( I, 0 ) = 0

         WORK( I, -1 ) = VAL__MAXI
         WORK( I, -2 ) = VAL__MAXI
         WORK( I, -3 ) = VAL__MINI
         WORK( I, -4 ) = VAL__MINI
         WORK( I, -5 ) = VAL__MAXI
         WORK( I, -6 ) = VAL__MINI

      END DO

*  Initialise the returned global pixel bounds.
      LBND( 1 ) = VAL__MAXI
      LBND( 2 ) = VAL__MAXI
      LBND( 3 ) = VAL__MAXI
      LBND( 4 ) = VAL__MAXI
      UBND( 1 ) = VAL__MINI
      UBND( 2 ) = VAL__MINI
      UBND( 3 ) = VAL__MINI
      UBND( 4 ) = VAL__MINI

*  Display a blank line if details of the input NDFs are to be displayed.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )

*  Initialise the number of NDFs which were not included in a bin.
      IGNORE = 0

*  Loop round each NDF.
      DO I = 1, NNDF

*  Get the current NDF identifier.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF, STATUS )

*  Get the NDF bounds and check it is 2 or 3-dimensional.
         CALL NDF_BOUND( INDF, 3, LBNDI, UBNDI, NDIM, STATUS )

*  If this is not the first NDF, check it has the same number of axes.
         IF( I .EQ. 1 ) THEN
	    NAX = NDIM
	    NDIMO = NDIM + 1
	 END IF

	 IF( NDIM .NE. NAX .AND. STATUS .EQ. SAI__OK ) THEN
	    STATUS = SAI__ERROR
	    CALL MSG_SETI( 'N', NDIM )
	    CALL MSG_SETI( 'M', NAX )
	    CALL NDF_MSG( 'NDF', INDF )
	    CALL ERR_REP( 'POL1_SRTIM_ERR1', 'NDF ''^NDF'' is ^N '//
     :                    'dimensional, but the first NDF was ^M '//
     :                    'dimensional.', STATUS )
            GO TO 999
	 END IF

*  See if the NDF has a POLPACK extension. If not, report an error.
         CALL NDF_XSTAT( INDF, 'POLPACK', THERE, STATUS )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'POL1_SRTIM_ERR1', 'Input image ''^NDF'' '//
     :                    'does not contain a POLPACK extension.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', XLOC, STATUS )

*  Get the WCS FrameSet from the NDF.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the ANGROT value which defines the reference direction. Use 0.0 if it
*  is missing. This is the ACW angle from +X to the zero analyser position.
         ANGROT = 0.0
         CALL POL1_GTANG( INDF, 0, IWCS, ANGROT, STATUS )

*  If this is the first input NDF, use the reference direction from this
*  input NDF for all output NDFs.
         IF( I .EQ. 1 ) ANGRT = ANGROT

*  Get the half-wave plate position in degrees (if it exists).
         CALL DAT_THERE( XLOC, 'WPLATE', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'WPLATE', H, STATUS )

*  Store the effective analyser angle for this NDF. This is the ACW angle
*  between the o/p ref. direction and a pretend analyser (with no
*  half-wave plate), which would have the same effect as the fixed
*  analyser/have-wave plate combination.
            PHI( I ) = 2*H + ANGROT - ANGRT

*  If there is no half-wave plate position in the POLPACK extension, look
*  for an analyser position. Report an error if neither is present.
         ELSE
            CALL DAT_THERE( XLOC, 'ANLANG', THERE, STATUS )
            IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'NDF', INDF )
               CALL ERR_REP( 'POL1_SRTIM_ERR3', 'The POLPACK '//
     :                       'extension in the input image ''^NDF'' '//
     :                       'does not contain a WPLATE or ANLANG '//
     :                       'value.', STATUS )
               GO TO 999
            END IF

*  Get the analyser angle, in degrees.
            CALL CMP_GET0R( XLOC, 'ANLANG', ALPHA, STATUS )

*  Store the effective analyser angle for this NDF (ACW angle from the
*  o/p ref. direction to the analyser).
            PHI( I ) = ALPHA + ANGROT - ANGRT

         END IF

*  If this NDF contains E-ray dual-beam data, add 90 degrees onto the
*  effective analyser angle.
         CALL DAT_THERE( XLOC, 'RAY', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0C( XLOC, 'RAY', RAY, STATUS )
            IF( RAY .EQ. 'E' ) PHI( I ) = PHI( I ) + 90.0
         END IF

*  Map the analysis angle into the range ORIGIN to RANGE+ORIGIN.
         PHI( I ) = PHI( I ) - RANGE*INT( ( PHI( I ) - ORIGIN )/RANGE )
         IF( PHI( I ) .LT. ORIGIN ) PHI( I ) = PHI( I ) + RANGE

*  Find the index of the bin which encompasses this analysis angle.
         IBIN = 1 + INT( ( PHI( I ) - ORIGIN ) / BIN )

*  Ignore illegal bin numbers.
         IF( IBIN .GT. 0 .AND. IBIN .LE. NBIN .AND.
     :       STATUS .EQ. SAI__OK ) THEN

*  Display details of this input NDF if required.
            CALL GRP_GET( IGRP1, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDF', NDFNAM )
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETR( 'PHI', PHI( I ) )
            CALL MSG_SETI( 'BIN', IBIN )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :                      '  Image ^I:  angle=^PHI  bin=^BIN '//
     :                      ' (^NDF)', STATUS )

*  Increment the number of input images in this bin.
            NIN = WORK( IBIN, 0 ) + 1
            WORK( IBIN, 0 ) = NIN

*  Store the index of this input NDF.
            WORK( IBIN, NIN ) = I

*  Update the pixel bounds required to span all the input NDFs in this bin.
            WORK( IBIN, -1 ) = MIN( LBNDI( 1 ), WORK( IBIN, -1 ) )
            WORK( IBIN, -2 ) = MIN( LBNDI( 2 ), WORK( IBIN, -2 ) )
            WORK( IBIN, -3 ) = MAX( UBNDI( 1 ), WORK( IBIN, -3 ) )
            WORK( IBIN, -4 ) = MAX( UBNDI( 2 ), WORK( IBIN, -4 ) )

            LBND( 1 ) = MIN( LBNDI( 1 ), LBND( 1 ) )
            LBND( 2 ) = MIN( LBNDI( 2 ), LBND( 2 ) )
            UBND( 1 ) = MAX( UBNDI( 1 ), UBND( 1 ) )
            UBND( 2 ) = MAX( UBNDI( 2 ), UBND( 2 ) )

            IF( NDIM .EQ. 3 ) THEN
               WORK( IBIN, -5 ) = MIN( LBNDI( 3 ), WORK( IBIN, -5 ) )
               WORK( IBIN, -6 ) = MAX( UBNDI( 3 ), WORK( IBIN, -6 ) )
               LBND( 3 ) = MIN( LBNDI( 3 ), LBND( 3 ) )
               UBND( 3 ) = MAX( UBNDI( 3 ), UBND( 3 ) )
	    END IF

*  Increment the number of illegal bin numbers.
         ELSE
            IGNORE = IGNORE + 1

*  Display details of this input NDF if required.
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETR( 'PHI', PHI( I ) )
            CALL MSG_SETI( 'BIN', IBIN )
            CALL MSG_OUT( MSG__VERB,' ',
     :                    '  Image ^I:  angle=^PHI  <not '//
     :                    'included>  (^NDF)', STATUS )

         END IF

*  Annul the FrameSet pointer.
         CALL AST_ANNUL( IWCS, STATUS )

*  Annul the locator to the POLPACK extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Annul the current NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

      CALL MSG_BLANKIF( MSG__VERB, STATUS )

*  See how many bins contain sufficient input images.
      DO I = 1, NBIN
         IF( WORK( I, 0 ) .GE. MININ ) THEN
            NOUT = NOUT + 1
         ELSE
            IGNORE = IGNORE + WORK( I, 0 )
         END IF
      END DO

*  Set the bounds of the last axis of the output stack.
      LBND( NDIMO ) = 1
      UBND( NDIMO ) = NOUT

*  Warn the user of any NDFs were not include din a bin.
      IF( IGNORE .EQ. 1  ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '  WARNING: One input image was '//
     :                 'not included in any output image.', STATUS )
         CALL MSG_BLANK( STATUS )

      ELSE IF( IGNORE .GT. 1  ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETI( 'IG', IGNORE )
         CALL MSG_OUT( ' ', '  WARNING: ^IG input images were '//
     :                 'not included in any output image.', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

 999  CONTINUE

      END
