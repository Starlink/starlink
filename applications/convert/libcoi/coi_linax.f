      SUBROUTINE COI_LINAX( IMDESC, NDF, USEDEF, BUFFER, STATUS )
*+
*  Name:
*     COI_LINAX

*  Purpose:
*     Creates axis centres within an NDF from FITS-axis information in
*     an IRAF header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_LINAX( IMDESC, NDF, USEDEF, BUFFER, STATUS )

*  Description:
*     The routine searches the IRAF headers for the keywords that
*     describe a linear axis structure.  At least one complete set of
*     keywords defining the axis must be present.  These keywords are
*     CRVALn, the reference value; CRPIXn, the reference pixel to which
*     the reference value corresponds; and CDELTn or CDn_n, the step
*     size between axis values.  If they all exist for axis n, then an
*     axis component is created and filled with the appropriate values.
*     If any of CRVALn, or CRPIXn, or either CDELTn and CDn_n are
*     absent, and USEDEF=.FALSE., the missing keywords are assigned
*     1.0D0.  This option allows conversion from the Multispec-system,
*     which omits these keywords.  When only some of the axes have
*     defined centres, the remaining axes are assigned pixel
*     co-ordinates.  If CTYPEn is in the header it is used to assign a
*     value to the nth axis's label component.  If CUNITn is present,
*     its value is stored in the nth axis's units component.
*
*     The precision of the output axis-centre array depends on the
*     absolute relative size of the offset to the scale.  Single
*     precision is used if this ratio is greater than one hundred times
*     the floating-point precision.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     USEDEF = LOGICAL (Given)
*        Whether or not to use default values of CRVALn, CRPIXn, and
*        CDELTn/CDn_n if any are absent.  This should currently only be
*        set to .TRUE. when the MWCS system is Equispec.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated value from multi-line headers
*        WATd_nnn.  In this case it is used to obtain the axis labels
*        and units.  It should be at least 68 characters long (for a
*        single line), and to be on the safe side, tens of times this.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The axis structure is deleted if not all the axes have
*     defined axis centres.
*
*  Deficiencies:
*     Does not handle log-linear case by checking DC-FLAG keyword.
*
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 24 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER IMDESC
      INTEGER NDF
      LOGICAL USEDEF

*  Arguments Returned:
      CHARACTER * ( * ) BUFFER 

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER IMOK               ! IMFORT OK status
      PARAMETER( IMOK = 0 )

      DOUBLE PRECISION PRECF     ! The factor times the machine
                                 ! precision above which the ratio of
                                 ! the scale factor to offset must lie
                                 ! for single-precision axis centres
                                 ! to be created
      PARAMETER ( PRECF = 100.0D0 )

*  Local Variables:
      INTEGER BUFLEN             ! Character length of the multi-line
                                 ! buffer for WATd_nnn headers
      CHARACTER * ( 1 ) CNDIM    ! The axis number
      DOUBLE PRECISION DELT      ! The co-ordinate increment between
                                 ! pixels
      LOGICAL DFAXIS             ! At least one axis-centre array
                                 ! defined?
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the NDF
      INTEGER DTYPE              ! Data code (0=linear,1=log-linear)
      INTEGER EL                 ! Dimension of the current axis
      INTEGER ERR                ! IMFORT status
      INTEGER I                  ! Loop counter
      CHARACTER * ( DAT__SZTYP ) ITYPE ! Data type of the axis centres
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      CHARACTER * ( 70 ) LABEL   ! Axis label
      INTEGER NC                 ! Number of characters in keyword
      INTEGER NCL                ! Number of characters in label
      INTEGER NCU                ! Number of characters in units
      INTEGER NDIM               ! Number of dimensions of the NDF
      INTEGER NSDIG              ! Number of significant digits
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component
      DOUBLE PRECISION REFP      ! Pixel position of the reference pixel
      DOUBLE PRECISION REFV      ! Co-ordinate at the reference pixel
      LOGICAL THERE              ! The nominated FITS keyword is present
                                 ! in the header?
      CHARACTER * ( 70 ) UNITS   ! Axis units
      CHARACTER * ( 70 ) VALUE   ! Keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
      CALL NDF_BEGIN
      
*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Create a default axis system in case only some of the FITS axes are
*  defined in the headers.
      CALL NDF_ACRE( NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Initialise flag to indicate whether any axis has been written.
      DFAXIS = .FALSE.

*  Obtain the DC-FLAG which indicates if the data are linear, log-linear
*  or non-linear.
      CALL IMGKWI( IMDESC, 'DC-FLAG', DTYPE, ERR )
      IF ( ERR .NE. IMOK ) DTYPE = 0

*  Loop for each dimension.
      DO I = 1, NDIM

*  Read the header for the four axis parameters.
*  =============================================
*
*  Note rotation cannot be mapped on to an axis structure and requires
*  a more-sophisticated astrometric system.

*  Create the extension to the keyword name for the current dimension.
         CALL CHR_ITOC( I, CNDIM, NC )

*  Obtain the value of the physical co-ordinate at the reference pixel.
         KEYWRD = ' '
         KEYWRD = 'CRVAL'//CNDIM( :NC )
         CALL IMGKWD( IMDESC, KEYWRD, REFV, ERR )
         THERE = ERR .EQ. IMOK
         IF ( .NOT. THERE .AND. USEDEF ) REFV = 1.0D0

*  The co-ordinate must be present to make any sense of an axis
*  structure.
         IF ( THERE ) THEN

*  Obtain the value of the element number of the reference pixel.
            KEYWRD = 'CRPIX'//CNDIM( :NC )
            CALL IMGKWD( IMDESC, KEYWRD, REFP, ERR )
            THERE = ERR .EQ. IMOK
            IF ( .NOT. THERE .AND. USEDEF ) REFP = 1.0D0

*  The reference pixel must be present to make any sense of an axis
*  structure.
            IF ( THERE ) THEN

*  Obtain the increment of physical co-ordinate between adjacent
*  pixels.  The co-ordinate increment per pixel must be present to make
*  any sense of an axis structure.
               KEYWRD = 'CDELT'//CNDIM( :NC )
               CALL IMGKWD( IMDESC, KEYWRD, DELT, ERR )
               THERE = ERR .EQ. IMOK

*  If absent, look for the synonym CDn_n first.
               IF ( .NOT. THERE ) THEN
                  KEYWRD = 'CD'//CNDIM//'_'//CNDIM

*  Obtain the value for the reference pixel.
                  CALL IMGKWD( IMDESC, KEYWRD, DELT, ERR )
                  THERE = ERR .EQ. IMOK

                  IF ( .NOT. THERE .AND. USEDEF ) DELT = 1.0D0
               END IF
            END IF
         END IF
      
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Decide the data type of the axis centres.
*  =========================================
         IF ( THERE .OR. USEDEF ) THEN

*  Set the flag to indicate that at least one axis-centre array has been
*  defined.
            DFAXIS = .TRUE.

*  Find the offset for a scale-and-offset calculation of the axis
*  information.
            OFFSET = REFV - ( REFP - 1.0D0 ) * DELT

*  Determine the data type of the axis centres when there is only a
*  constant value.
            IF ( ABS( DELT ) .LT. VAL__SMLD ) THEN

*  Obtain the value as a string.  There is no need to test for an error
*  because it would have been detected earlier.
               KEYWRD = 'CRVAL'//CNDIM( :NC )
               CALL IMGKWC( IMDESC, KEYWRD, VALUE, ERR )

*  Find the number of significant digits in the numerical value.
               CALL CON_SGDIG( VALUE, NSDIG, STATUS )

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
               IF ( NSDIG .LE. -INT( LOG10( VAL__EPSR ) ) ) THEN
                  ITYPE = '_REAL'
               ELSE
                  ITYPE = '_DOUBLE'
               END IF

*  Define the criterion for deciding whether or not single-precision
*  axis centres are adequate.  It is important that the increments
*  between elements are greater than the floating-point precision.  To
*  reduce possible errors the fractional difference is scaled by a
*  constant.  However, the values may conspire to give a zero offset
*  (e.g. US-style co-ordinates of pixels).  A zero ratio is tested
*  also, since the type required for a constant depends on the number of
*  significant figures.
            ELSE
               IF ( ABS( OFFSET ) .LT. VAL__EPSD ) THEN
                  ITYPE = '_REAL'
               ELSE
                  
                  IF ( ABS( DELT / OFFSET ) .GT. DBLE( VAL__EPSR ) *
     :                 PRECF ) THEN
                     ITYPE = '_REAL'
                  ELSE
                     ITYPE = '_DOUBLE'
                  END IF
               END IF
            END IF

*  Fill the axis centres.
*  ======================
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'WRITE', PNTR, 
     :                        EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CON_SSAZR( EL, DELT, OFFSET, %VAL( PNTR( 1 ) ),
     :                            STATUS )

*  Exponentiate a log-linear axis.
                  IF ( DTYPE .EQ. 1 )
     :              CALL COI_ALOGR( EL, %VAL( PNTR( 1 ) ), STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
               END IF

*  Double precision is required.
            ELSE

*  Ensure that the type is double precision.
               CALL NDF_ASTYP( '_DOUBLE', NDF, 'Centre', I, STATUS )

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CON_SSAZD( EL, DELT, OFFSET, %VAL( PNTR( 1 ) ),
     :                            STATUS )

*  Exponentiate a log-linear axis.
                  IF ( DTYPE .EQ. 1 )
     :              CALL COI_ALOGD( EL, %VAL( PNTR( 1 ) ), STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
               END IF
            END IF

*  Obtain the axis label and units from `standard' headers.
*  ========================================================

*  Obtain the label of the axis.  The label is not mandatory, so the
*  presence flag will only determine whether or not the axis label
*  component is written.
            KEYWRD = 'CTYPE'//CNDIM( :NC )
            CALL IMGKWC( IMDESC, KEYWRD, LABEL, ERR )

            IF ( ERR .EQ. IMOK .AND. LABEL .NE. ' ' ) THEN

*  Find the length of the label.
               NCL = CHR_LEN( LABEL )

*  Write the label to the axis structure, using only the length actually
*  required.
               CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', I, STATUS )
            END IF

*  Obtain the units of the axis.  Axis units is not part of the FITS
*  standard yet, but the CUNITn keywords are widely used and they are
*  in the WCS proposal.  The presence flag will only determine whether
*  or not the axis units component is written.
            KEYWRD = 'CUNIT'//CNDIM( :NC )
            CALL IMGKWC( IMDESC, KEYWRD, UNITS, ERR )

            IF ( ERR .EQ. IMOK .AND. UNITS .NE. ' ' ) THEN

*  Find the length of the units.
               NCU = CHR_LEN( UNITS )

*  Write the units to the axis structure.
               CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', I, STATUS )
            END IF

*  Obtain the axis label and units from IRAF MWCS headers.
*  =======================================================

*  Concatenate the WAT<dim>_nnn header, which may contain the axis label
*  and units.
            CALL ERR_MARK
            CALL COI_WCCAT( IMDESC, I, BUFFER, BUFLEN, STATUS )

*  Extract the label.
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract the label.
               CALL COI_WCWRD( BUFFER( :BUFLEN ), 'label', LABEL,
     :                         THERE, STATUS )

*  If a label is present, write it to the NDF AXIS strucutre for the
*  current axis.  Pass only the used length not to waste space in the
*  NDF.
               IF ( THERE ) THEN
                  NCL = CHR_LEN( LABEL )
                  CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', I,
     :                            STATUS )
               END IF

*  Extract the units.
               CALL COI_WCWRD( BUFFER( :BUFLEN ), 'units', UNITS, THERE,
     :                         STATUS )

*  If units value is present, write it to the NDF AXIS strucutre for
*  the current axis.  Pass only the used length not to waste space in
*  the NDF.
               IF ( THERE ) THEN
                  NCU = CHR_LEN( UNITS )
                  CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', I,
     :                            STATUS )
               END IF

*  It is not serious if we cannot obtain the header card, as the
*  character components of the axis may have been provided
*  through`standard' FITS keywords.
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE

*  End of a check for a physical co-ordinate.
         END IF

*  End of the loop for each dimension.
      END DO

*  Delete the axis structure if it is just the default created earlier.
      IF ( .NOT. DFAXIS ) CALL NDF_RESET( NDF, 'Axis', STATUS )

  999 CONTINUE

*  Close the new NDF context.
      CALL NDF_END( STATUS )
      
      END
