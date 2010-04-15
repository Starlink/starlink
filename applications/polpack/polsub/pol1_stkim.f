      SUBROUTINE POL1_STKIM( MININ, ANGRT, IGRP1, IGRP2, IBIN, INDFT,
     :                       NBIN, NNDF, WORK, PHI, BL, BH,
     :                       NAX, IOUT, SAXIS, STATUS )
*+
*  Name:
*     POL1_STKIM

*  Purpose:
*     Create an output image by stacking the corresponding input images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STKIM( MININ, ANGRT, IGRP1, IGRP2, IBIN, INDFT, NBIN, NNDF,
*                      WORK, PHI, BL, BH, NAX, IOUT, SAXIS, STATUS )

*  Description:
*     This routine creates an output intensity image by stacking the
*     corresponding input intensity images.

*  Arguments:
*     MININ = INTEGER (Given)
*        The minimum number of input images required to form an output image.
*     ANGRT = REAL (Given)
*        The ACW angle from the +ve X axis of each output image, to the
*        output reference direction, in degrees.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for the group containing the output NDF names.
*     IBIN = INTEGER (Given)
*        The IBIN of the output NDF to be created.
*     INDFT = INTEGER (Given)
*        An identifier for an output 3/4D stack to hold the output images.
*        Supplied equal to NDF__NOID if no stack is required.
*     NBIN = INTEGER (Given)
*        The number of analysis angle bins.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the supplied group.
*     WORK( NBIN, -6 : NNDF ) = INTEGER (Given)
*        An array containing a column for each analysis angle bin.
*        The row 0 contains the number of input NDFs in the bin. If
*        this value is N, then rows 1 to N contain a list of the N
*        input NDFs in the bin. Each NDF is identified by its IBIN
*        within the supplied group. Rows -1 to -6 contains the pixel
*        bounds which span all the corresponding input NDFs, in the order
*        LBND1, LBND2, UBND1, UBND2, (LBND3, UBND3).
*     PHI( NNDF ) = INTEGER (Given)
*        The acw angle from +ve X axis to the effective analyser position
*        in each input NDF, in degrees.
*     BL = REAL (Given)
*        The lower analysis angle bound of the bin, in degrees.
*     BH = REAL (Given)
*        The upper analysis angle bound of the bin, in degrees.
*     NAX = INTEGER (Given)
*        The number of axes in each input NDF.
*     IOUT = INTEGER (Given and Returned)
*        The number of output NDFs created so far. Incremented by 1 on
*        exit.
*     SAXIS( * )= REAL (Given and Returned)
*        The axis centre array for the last axis of the output stack.
*        Ignored if INDFT is NDF__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
*        Original version.
*     21-JUN-1999 (DSB):
*        Move assignment to SAXIS so that it is conditional on INDFT
*        being supplied.
*     1-JUL-1999 (DSB):
*        Replaced argument QUIET with ILEVEL, and added extra reporting
*        options. Create output NDF by propagation from first contributing
*        input NDF (so that history is propagated).
*     19-FEB-2001 (DSB):
*        Modified to support 3D data.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
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
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER MININ
      REAL ANGRT
      INTEGER IGRP1
      INTEGER IGRP2
      INTEGER IBIN
      INTEGER INDFT
      INTEGER NBIN
      INTEGER NNDF
      INTEGER WORK( NBIN, -6 : NNDF )
      REAL PHI( NNDF )
      REAL BL
      REAL BH
      INTEGER NAX

*  Arguments Given and Returned:
      INTEGER IOUT
      REAL SAXIS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER INLIST*1024      ! List of input NDF indices
      CHARACTER LOC*(DAT__SZLOC) ! Locator to POLPACK extension
      CHARACTER NDFNAM*(GRP__SZNAM)! Name of the NDF being processed
      INTEGER EL                 ! No. of mapped elements
      INTEGER I                  ! Index of current input NDF
      INTEGER IAT                ! Used length of string
      INTEGER IERR               ! Index of first numerical error
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER INDFO              ! Identifier for output NDF
      INTEGER INDFS              ! NDF identifier for input section
      INTEGER IPCNT              ! Pointer to array holding pixel counts
      INTEGER IPDIN              ! Pointer to input DATA array
      INTEGER IPDOUT             ! Pointer to output DATA array
      INTEGER IPDP               ! Pointer to a plane of the DATA stack
      INTEGER IPVOUT             ! Pointer to output VARIANCE array
      INTEGER IPVP               ! Pointer to a plane of the VARIANCE stack
      INTEGER IWCS               ! AST pointer to a WCS FrameSet
      INTEGER LBND( 4 )          ! Lower pixel bounds of output NDF
      INTEGER NAMLEN             ! Length of name string
      INTEGER NERR               ! No. of numerical errors
      INTEGER NIN                ! No. of input NDFs in the current bin
      INTEGER UBND( 4 )          ! Upper pixel bounds of output NDF
      REAL ANGROT                ! Angle from first image axis to the ref. dirn
      REAL ANLANG                ! Angle from ref. dirn to effective analyser
      REAL ASUM                  ! Sum of input analysis angles
      REAL ME                    ! Mean error in output NDF
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of input NDFs in the output NDFs analysis angle bin.
      NIN = WORK( IBIN, 0 )

*  Only proceed if the bin contains sufficient input images.
      IF( NIN .GE. MININ ) THEN

*  Increment the number of output images which will have been created on
*  exit.
         IOUT = IOUT + 1

*  Get the name of the output NDF now, while we know that no error has
*  occurred.
         CALL GRP_GET( IGRP2, IOUT, 1, NDFNAM, STATUS )

*  Write out the name of this NDF.
         CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
         CALL MSG_OUT( ' ', '   Creating ''^CURRENT_NDF''',
     :                 STATUS )


*  Get an identifier for the first input NDF contributing to this output
*  NDF.
         CALL NDG_NDFAS( IGRP1, WORK( IBIN, 1 ), 'READ', INDF, STATUS )

*  Create the output NDF by propagation from this input NDF, so that we
*  can retain the history information. We will correct the shape later.
         CALL NDG_NDFPR( INDF, 'NOEXT(IRAS90,FITS,POLPACK)', IGRP2,
     :                   IOUT, INDFO, STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Get the pixel bounds for the output image.
         LBND( 1 ) = WORK( IBIN, -1 )
         LBND( 2 ) = WORK( IBIN, -2 )
         UBND( 1 ) = WORK( IBIN, -3 )
         UBND( 2 ) = WORK( IBIN, -4 )
	 IF( NAX .EQ. 3 ) THEN
	    LBND( 3 ) = WORK( IBIN, -5 )
	    UBND( 3 ) = WORK( IBIN, -6 )
	 END IF

*  Set the shape of the output NDF.
         CALL NDF_SBND( NAX, LBND, UBND, INDFO, STATUS )

*  Map the DATA and VARIANCE arrays.
         CALL NDF_MAP( INDFO, 'DATA', '_REAL', 'WRITE/ZERO', IPDOUT, EL,
     :                 STATUS )
         CALL NDF_MAP( INDFO, 'VARIANCE', '_REAL', 'WRITE/ZERO', IPVOUT,
     :                 EL, STATUS )

*  Allocate an work array to hold the number of good input pixels
*  at each output pixel.
         CALL PSX_CALLOC( EL, '_REAL', IPCNT, STATUS )

*  Initialise it to zero at every pixel.
         CALL POL1_FILLR( 0.0, EL, %VAL( CNF_PVAL( IPCNT ) ), STATUS )

*  Initialise the sum of the analysis angles.
         ASUM = 0.0

*  Initialise a list of the NDF indices which contribute to this output
*  image.
         INLIST = ' '
         IAT = 0

*  Loop round each input NDF which contributes to this output NDF.
         DO I = 1, NIN

*  Check all is well.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context
            CALL NDF_BEGIN

*  Get the input NDF identifier.
            CALL NDG_NDFAS( IGRP1, WORK( IBIN, I ), 'READ', INDF,
     :                      STATUS )

*  Get a section which matches the output NDF.
            CALL NDF_SECT( INDF, NAX, LBND, UBND, INDFS, STATUS )

*  If this is the first input NDF...
            IF( I .EQ. 1 ) THEN

*  Get the WCS informaton in the input.
               CALL KPG1_GTWCS( INDFS, IWCS, STATUS )

*  Store the reference direction as a POLANAL Frame in the WCS FrameSet.
               CALL POL1_PTANG( ANGRT, IWCS, STATUS )

*  Store the WCS informaton in the output.
               CALL NDF_PTWCS( IWCS, INDFO, STATUS )

*  Annul the FrameSet.
               CALL AST_ANNUL( IWCS, STATUS )
            END IF

*  Map the DATA array.
            CALL NDF_MAP( INDFS, 'DATA', '_REAL', 'READ', IPDIN, EL,
     :                    STATUS )

*  Add this array into the running sum arrays.
            CALL POL1_STKSM( EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                       %VAL( CNF_PVAL( IPDOUT ) ),
     :                       %VAL( CNF_PVAL( IPVOUT ) ),
     :                       %VAL( CNF_PVAL( IPCNT ) ), STATUS )

*  Increment the mean analysis angle.
            ASUM = ASUM + PHI( WORK( IBIN, I ) )

*  Append the input NDF index to the list of indices, followed by a space.
            CALL CHR_PUTI( WORK( IBIN, I ), INLIST, IAT )
            IAT = IAT + 1

*  End the current NDF context.
            CALL NDF_END( STATUS )

         END DO

*  Normalize the returned data and variance values.
         CALL POL1_STKNM( EL, %VAL( CNF_PVAL( IPCNT ) ),
     :                    %VAL( CNF_PVAL( IPDOUT ) ),
     :                    %VAL( CNF_PVAL( IPVOUT ) ), ME, STATUS )

*  Find the effective analysis angle for the output image. This is the
*  ACW angle from the reference direction given by ANGRT, to the
*  effective analyser, in degrees.
         ANLANG = ASUM/NIN

*  Create the POLPACK extension in the output NDF.
         CALL NDF_XNEW( INDFO, 'POLPACK', 'POLPACK', 0, 0, LOC, STATUS )

*  Save the effective analysis angle.
         CALL NDF_XPT0R( ANLANG, INDFO, 'POLPACK', 'ANLANG', STATUS )

*  Annul the extension locator.
         CALL DAT_ANNUL( LOC, STATUS )

*  If required, describe the output NDF.
*  Note that we output slightly different formatting in verbose mode.
         IF( MSG_FLEVOK( MSG__VERB, STATUS ) ) THEN

            CALL MSG_SETR( 'BL', BL )
            CALL MSG_SETR( 'BH', BH )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :                    '      Analysis angle range     '//
     :                    ': ^BL to ^BH degrees', STATUS )

            CALL MSG_SETC( 'L', INLIST( : IAT ) )
            IF( NIN .GT. 1 ) THEN
               CALL MSG_OUTIF( MSG__VERB, ' ',
     :                    '      Contributing input '//
     :                    'images: ^L', STATUS )
            ELSE
               CALL MSG_OUTIF( MSG__VERB, ' ',
     :                    '      Contributing input '//
     :                    'image : ^L', STATUS )
            END IF

            CALL MSG_SETR( 'A', ANLANG )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :                    '      Effective analysis angle '//
     :                    ': ^A degrees', STATUS )

            IF( ME .NE. VAL__BADR ) THEN
               CALL MSG_SETR( 'ME', ME )
            ELSE
              CALL MSG_SETC( 'ME', '<undefined>' )
            END IF

            CALL MSG_OUTIF( MSG__VERB, ' ',
     :                    '      Mean error               '//
     :                    ': ^ME', STATUS )

            CALL MSG_BLANKIF( MSG__VERB, STATUS )

         ELSE IF( MSG_FLEVOK( MSG__NORM, STATUS ) ) THEN

            CALL MSG_SETR( 'BL', BL )
            CALL MSG_SETR( 'BH', BH )
            CALL MSG_OUT( ' ', '      Analysis angle range      '//
     :                    '      : ^BL to ^BH degrees', STATUS )

            CALL MSG_SETI( 'N', NIN )
            CALL MSG_OUT( ' ', '      No. of contributing input '//
     :                    'images: ^N', STATUS )

            CALL MSG_SETR( 'A', ANLANG )
            CALL MSG_OUT( ' ', '      Effective analysis angle  '//
     :                    '      : ^A degrees', STATUS )

            IF( ME .NE. VAL__BADR ) THEN
               CALL MSG_SETR( 'ME', ME )
            ELSE
              CALL MSG_SETC( 'ME', '<undefined>' )
            END IF

            CALL MSG_OUT( ' ', '      Mean error                '//
     :                    '      : ^ME', STATUS )

            CALL MSG_BLANK( STATUS )

         END IF

*  If a stack is being produced, copy the current output NDF into
*  one plane of the stack.
         IF( INDFT .NE. NDF__NOID ) THEN

*  Get a section of the output stack which covers the output image.
            LBND( NAX + 1 ) = IOUT
            UBND( NAX + 1 ) = IOUT
            CALL NDF_SECT( INDFT, NAX + 1, LBND, UBND, INDFS, STATUS )

*  Map the DATA and VARIANCE arrays of this section of the stack.
            CALL NDF_MAP( INDFS, 'DATA', '_REAL', 'WRITE', IPDP, EL,
     :                    STATUS )
            CALL NDF_MAP( INDFS, 'VARIANCE', '_REAL', 'WRITE', IPVP,
     :                    EL, STATUS )

*  Copy the data and variance from output to stack.
            CALL VEC_RTOR( .TRUE., EL, %VAL( CNF_PVAL( IPDOUT ) ),
     :                     %VAL( CNF_PVAL( IPDP ) ),
     :                     IERR, NERR, STATUS )
            CALL VEC_RTOR( .TRUE., EL, %VAL( CNF_PVAL( IPVOUT ) ),
     :                     %VAL( CNF_PVAL( IPVP ) ),
     :                     IERR, NERR, STATUS )

*  Store the effective analyser position for the last axis.
            SAXIS( IOUT ) = ANLANG

*  Annul the section identifier.
            CALL NDF_ANNUL( INDFS, STATUS )

         END IF

*  Annul the output NDF identifier.
         CALL NDF_ANNUL( INDFO, STATUS )

*  Free the work space.
         CALL PSX_FREE( IPCNT, STATUS )

      END IF

 999  CONTINUE

      END
