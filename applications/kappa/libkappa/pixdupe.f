      SUBROUTINE PIXDUPE ( STATUS )
*+
*  Name:
*     PIXDUPE

*  Purpose:
*     Expands an NDF by pixel duplication.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PIXDUPE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine expands the size of an NDF structure by duplicating
*     each input pixel a specified number of times along each
*     dimension, to create a new NDF structure.

*  Usage:
*     pixdupe in out expand

*  ADAM Parameters:
*     EXPAND() = _INTEGER (Read)
*        Linear expansion factors to be used to create the new data
*        array.  The number of factors should equal the number of
*        dimensions in the input NDF.  If fewer are supplied the last
*        value in the list of expansion factors is given to the
*        remaining dimensions.  Thus if a uniform expansion is required
*        in all dimensions, just one value need be entered.  If the net
*        expansion is one, an error results.  The suggested default is
*        the current value.
*     IN  = NDF (Read)
*        Input NDF structure to be expanded.
*     OUT = NDF (Write)
*        Output NDF structure.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     pixdupe aa bb 2
*        This expands the NDF called aa duplicating pixels along each
*        dimension, and stores the enlarged data in the NDF called bb.
*        Thus if aa is 2-dimensional, this command would result in a
*        four-fold increase in the array components.
*     pixdupe cosmos galaxy [2,1]
*        This expands the NDF called cosmos by duplicating along the
*        first axis, and stores the enlarged data in the NDF called
*        galaxy.
*     pixdupe cube1 cube2 [3,1,2]  title="Reconfigured cube"
*        This expands the NDF called cube1 by having three pixels for
*        each pixel along the first axis and duplicating along the
*        third axis, and stores the enlarged data in the NDF called
*        cube2.  The title of cube2 is {\tt "Reconfigured cube"}.

*  Related Applications:
*     KAPPA: COMPADD, COMPAVE, COMPICK.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, and HISTORY, components of an NDF
*     data structure and propagates all extensions.  Origin information
*     becomes undefined by the duplication and so is not propagated.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 28 (MJC):
*        Original NDF version.
*     11-JUN-1998 (DSB):
*        Added propagation of the NDF WCS component.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SAE definitions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM )! Matrix component of linear mapping
      DOUBLE PRECISION OFFSET( NDF__MXDIM )   ! Translation component of linear mapping
      INTEGER ACTVAL             ! Actual number of compression factors
      INTEGER AEXPND( NDF__MXDIM ) ! Axis expansion factors
      INTEGER AIDIMS( NDF__MXDIM ) ! Axis expansion dimensions of input
      INTEGER AODIMS( NDF__MXDIM ) ! Axis expansion dimensions of output
      LOGICAL AVAR               ! Axis variance is present?
      LOGICAL AXIS               ! Axis structure is present?
      CHARACTER * ( 80 ) DIMSTR  ! List of the output dimensions
      INTEGER EXPAND( NDF__MXDIM ) ! Expansion factors
      INTEGER EXPMAX( NDF__MXDIM ) ! Maximum expansion factors
      INTEGER EXPMIN( NDF__MXDIM ) ! Minimum expansion factors
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER I                  ! Loop counter for the dimensions
      INTEGER IAXIS              ! Loop counter for the axis-array
                                 ! components
      INTEGER IDIMS( NDF__MXDIM )! Dimensions of input NDF
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Numeric type for processing
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER NCD                ! No. of characters in dimension list
      INTEGER NDFI               ! Identifier to the input NDF
      INTEGER NDFO               ! Identifier to the output NDF
      INTEGER NDFS               ! Identifier to the section of the input
                                 ! NDF
      INTEGER NDIM               ! Dimensionality of the NDF
      INTEGER ODIMS( NDF__MXDIM )! Dimensions of output array
      INTEGER PNTRI( 2 )         ! Pointer to input array component(s)
      INTEGER PNTRO( 2 )         ! Pointer to output array component(s)
      LOGICAL QUAL               ! Quality is present?
      INTEGER TOTEXP             ! Total expansion factor
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM )! Upper bounds of output NDF
      LOGICAL VAR                ! Variance is present?
      LOGICAL WIDTH              ! Axis width is present?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Inquire the dimensions of the NDF.
      CALL NDF_DIM( NDFI, NDF__MXDIM, IDIMS, NDIM, STATUS )

*  Obtain the expansion factors.
*  =============================
*
*  Set the acceptable range of values from no expansion to expansion
*  of a single element in a dimension.  Initialise values in case of an
*  error to prevent a possible divide-by-zero catastrophe.
      DO I = 1, NDIM
         EXPMIN( I ) = 1
         EXPMAX( I ) = VAL__MAXI / IDIMS( I )
         EXPAND( I ) = 1
      END DO

*  Get the expansion factors.
      CALL PAR_GRMVI( 'EXPAND', NDIM, EXPMIN, EXPMAX, EXPAND, ACTVAL,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Should less values be entered than is required copy the last value to
*  higher dimensions.
      IF ( ACTVAL .LT. NDIM ) THEN
         DO I = ACTVAL + 1, NDIM
            EXPAND( I ) = EXPAND( ACTVAL )
         END DO
      END IF

*  Pad out the expansion factors and dimensions to the maximum number
*  of dimensions.  The subroutine uses them all to avoid a complex
*  piece of coding.
      IF ( NDIM .LT. NDF__MXDIM ) THEN
         DO I = NDIM + 1, NDF__MXDIM
            EXPAND( I ) = 1
            IDIMS( I ) = 1
            ODIMS( I ) = 1
         END DO
      END IF

*  Check there is going to be a expansion.
*  =======================================

*  Find total expansion.
      TOTEXP = 1
      DO I = 1, NDIM
         TOTEXP = TOTEXP * EXPAND( I )
      END DO

*  Report and abort if there is no expansion.
      IF ( TOTEXP .EQ. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_PIXDUPE_NOEXPR',
     :     'PIXDUPE: There is no expansion to be made.', STATUS )
         GOTO 999
      END IF

*  Compute the output NDF's dimensions.
*  ====================================

*  Work out the size of the output array from the input array
*  dimensions and the expansion factor.  Also derive bounds for the
*  output array.  These are somewhat arbitrary.
      DO I = 1, NDIM
         ODIMS( I ) = IDIMS( I ) * EXPAND( I )
         LBNDO( I ) = 1
         UBNDO( I ) = ODIMS( I )
      END DO

*  Report the new dimensions.
      CALL DIMLST( NDIM, ODIMS, NCD, DIMSTR, STATUS )
      CALL MSG_OUTIF( MSG__VERB, 'OUTPUT_DIMS', ' The output NDF will '/
     :  /'have ' // DIMSTR( :NCD ) // ' pixels.', STATUS )

*  Create the output NDF.
*  ======================
*
*  Take a shortcut to propagate ancillary data from the input NDF.
*  Create a section from the input NDF of the size of the required NDF.
      CALL NDF_SECT( NDFI, NDIM, LBNDO, UBNDO, NDFS, STATUS )

*  Create the output NDF based on the sub-section.  The array
*  components and axes will be processed individually, but this enables
*  the LABEL, HISTORY, AXIS character components, and extensions to be
*  propagated.  The axis arrays will be changed later.
      CALL NDF_PROP( NDFS, 'Axis,Units', 'OUT', NDFO, STATUS )

*  Obtain a title and assign it to the output NDF.  A null results in
*  the output title being the same as the input title.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Expand the data array.
*  ======================

*  As the values and quality are merely duplicated, there is no need to
*  test for bad values.  Hence we can switch off automatic quality
*  masking too.
      CALL NDF_SQMF( .FALSE., NDFI, STATUS )

*  Find the data type of the data array.
      CALL NDF_TYPE( NDFI, 'Data', ITYPE, STATUS )

*  Map the full input, and output data arrays.
      CALL NDF_MAP( NDFI, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL NDF_MAP( NDFO, 'Data', ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Duplicate the data array, using the routine approriate for the data
*  type.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_PXDPR( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_PXDPB( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_PXDPD( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_PXDPI( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_PXDPUB( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                     %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_PXDPUW( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                     %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_PXDPW( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                    %VAL( PNTRO( 1 ) ), STATUS )
      END IF

*  Tidy the data arrays.
      CALL NDF_UNMAP( NDFI, 'Data', STATUS )
      CALL NDF_UNMAP( NDFO, 'Data', STATUS )

*  Expand the variance array.
*  ==========================

*  Determine if a variance component is present.  Normally it would be
*  more efficient to map the variance at the same time as the data
*  array as quality masking need only be applied once.  However,
*  masking has been switched off, and in this case a lack of memory is
*  more important, as the output arrays could be large, so we process
*  each of the array components in turn.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )
      IF ( VAR ) THEN

*  Find the data type of the variance array.
         CALL NDF_TYPE( NDFI, 'Variance', ITYPE, STATUS )

*  Map the full input, and output variance arrays.
         CALL NDF_MAP( NDFI, 'Variance', ITYPE, 'READ', PNTRI, EL,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'Variance', ITYPE, 'WRITE', PNTRO, EL,
     :                 STATUS )

*  Duplicate the variance array, using the routine approriate for the
*  data type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_PXDPR( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                       %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_PXDPB( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                       %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_PXDPD( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                       %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_PXDPI( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                       %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_PXDPUB( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                        %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_PXDPUW( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                        %VAL( PNTRO( 1 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_PXDPW( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                       %VAL( PNTRO( 1 ) ), STATUS )
         END IF

*  Tidy the variance arrays.
         CALL NDF_UNMAP( NDFI, 'Variance', STATUS )
         CALL NDF_UNMAP( NDFO, 'Variance', STATUS )

      END IF

*  Expand the quality array.
*  =========================

*  Determine if a quality component is present.
      CALL NDF_STATE( NDFI, 'Quality', QUAL, STATUS )
      IF ( QUAL ) THEN

*  Map the full input, and output Quality arrays.  The data type must be
*  unsigned byte.
         CALL NDF_MAP( NDFI, 'Quality', '_UBYTE', 'READ', PNTRI, EL,
     :                 STATUS )
         CALL NDF_MAP( NDFO, 'Quality', '_UBYTE', 'WRITE', PNTRO, EL,
     :                 STATUS )

*  Expand the quality array.
         CALL KPG1_PXDPUB( IDIMS, %VAL( PNTRI( 1 ) ), EXPAND, ODIMS,
     :                     %VAL( PNTRO( 1 ) ), STATUS )

*  Tidy the quality arrays.
         CALL NDF_UNMAP( NDFI, 'Quality', STATUS )
         CALL NDF_UNMAP( NDFO, 'Quality', STATUS )
      END IF

*  Expand the axis-centres array.
*  ==============================

*  See whether or not there is an axis system.
      CALL NDF_STATE( NDFI, 'Axis', AXIS, STATUS )

      IF ( AXIS ) THEN

*  Set the expansion factors.  Pad out the expansion factors to the
*  maximum number of dimensions.  Also set the dummy input and output
*  dimensions.
         DO I = 2, NDF__MXDIM
            AEXPND( I ) = 1
            AIDIMS( I ) = 1
            AODIMS( I ) = 1
         END DO

*  Process each dimension separately.
         DO IAXIS = 1, NDIM

*  Find the data type of the centre array.
            CALL NDF_ATYPE( NDFI, 'Centre', IAXIS, ITYPE, STATUS )

*  Map the full input, and output variance arrays.
            CALL NDF_AMAP( NDFI, 'Centre', IAXIS, ITYPE, 'READ', PNTRI,
     :                     EL, STATUS )
            CALL NDF_AMAP( NDFO, 'Centre', IAXIS, ITYPE, 'WRITE', PNTRO,
     :                     EL, STATUS )

*  Set the axis expansion factor, and the dimensions.
            AEXPND( 1 ) = EXPAND( IAXIS )
            AIDIMS( 1 ) = IDIMS( IAXIS )
            AODIMS( 1 ) = ODIMS( IAXIS )

*  Duplicate the axis centres, calling the appropriate routine for the
*  data type.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_PXDPR( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                          AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_PXDPB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                          AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                CALL KPG1_PXDPD( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                           AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_PXDPI( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                          AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_PXDPUB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                           AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_PXDPUW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                           AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_PXDPW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                          AODIMS, %VAL( PNTRO( 1 ) ), STATUS )
            END IF

*  Tidy the centre arrays.
            CALL NDF_AUNMP( NDFI, 'Centre', IAXIS, STATUS )
            CALL NDF_AUNMP( NDFO, 'Centre', IAXIS, STATUS )
         END DO

*  Expand the axis-variance array.
*  ==============================

*  Process each dimension separately.
         DO IAXIS = 1, NDIM

*  See whether or not the axis variance exists for this axis.
            CALL NDF_ASTAT( NDFI, 'Variance', IAXIS, AVAR, STATUS )

            IF ( AVAR ) THEN

*  Find the data type of the centre array.
               CALL NDF_ATYPE( NDFI, 'Variance', IAXIS, ITYPE, STATUS )

*  Map the full input, and output variance arrays.
               CALL NDF_AMAP( NDFI, 'Variance', IAXIS, ITYPE, 'READ',
     :                        PNTRI, EL, STATUS )
               CALL NDF_AMAP( NDFO, 'Variance', IAXIS, ITYPE, 'WRITE',
     :                        PNTRO, EL, STATUS )

*  Set the axis expansion factor, and the dimensions.
               AEXPND( 1 ) = EXPAND( IAXIS )
               AIDIMS( 1 ) = IDIMS( IAXIS )
               AODIMS( 1 ) = ODIMS( IAXIS )

*  Duplicate the axis variance, calling the appropriate routine for the
*  data type.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_PXDPR( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
                   CALL KPG1_PXDPB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                   CALL KPG1_PXDPD( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_PXDPI( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_PXDPUB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_PXDPUW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_PXDPW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )
               END IF

*  Tidy the centre arrays.
               CALL NDF_AUNMP( NDFI, 'Variance', IAXIS, STATUS )
               CALL NDF_AUNMP( NDFO, 'Variance', IAXIS, STATUS )
            END IF
         END DO

*  Expand the axis-width array.
*  ============================

*  Process each dimension separately.
         DO IAXIS = 1, NDIM

*  See whether or not the axis variance exists for this axis.
            CALL NDF_ASTAT( NDFI, 'Variance', IAXIS, WIDTH, STATUS )

            IF ( WIDTH ) THEN

*  Find the data type of the centre array.
               CALL NDF_ATYPE( NDFI, 'Width', IAXIS, ITYPE, STATUS )

*  Map the full input, and output width arrays.
               CALL NDF_AMAP( NDFI, 'Width', IAXIS, ITYPE, 'READ',
     :                        PNTRI, EL, STATUS )
               CALL NDF_AMAP( NDFO, 'Width', IAXIS, ITYPE, 'WRITE',
     :                        PNTRO, EL, STATUS )

*  Set the axis expansion factor, and the dimensions.
               AEXPND( 1 ) = EXPAND( IAXIS )
               AIDIMS( 1 ) = IDIMS( IAXIS )
               AODIMS( 1 ) = ODIMS( IAXIS )

*  Duplicate the axis width, calling the appropriate routine for the
*  data type.
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_PXDPR( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
                  CALL KPG1_PXDPB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                   CALL KPG1_PXDPD( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                  CALL KPG1_PXDPI( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL KPG1_PXDPUB( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                  CALL KPG1_PXDPUW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                              AODIMS, %VAL( PNTRO( 1 ) ), STATUS )

               ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                  CALL KPG1_PXDPW( AIDIMS, %VAL( PNTRI( 1 ) ), AEXPND,
     :                             AODIMS, %VAL( PNTRO( 1 ) ), STATUS )
               END IF

*  Tidy the width arrays.
               CALL NDF_AUNMP( NDFI, 'Width', IAXIS, STATUS )
               CALL NDF_AUNMP( NDFO, 'Width', IAXIS, STATUS )
            END IF
         END DO
      END IF

*  Propagate the WCS component, incorporating a linear mapping between
*  pixel coordinates. This mapping is described by a matrix and an offset
*  vector. Set these up. 
      DO I = 1, NDIM*NDIM
         MATRIX( I ) = 0.0
      END DO

      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

      DO I = 1, NDIM
         OFFSET( I ) = DBLE( LBNDO( I ) - 1 ) - EXPAND( I )*
     :                 DBLE( LBNDI( I ) - 1 )
         MATRIX( NDIM*( I - 1 ) + I ) = DBLE( EXPAND( I ) )
      END DO

*  Propagate the WCS component.
      CALL KPG1_ASPRP( NDIM, NDFI, NDFO, MATRIX, OFFSET, STATUS )

*  Come here if something has gone wrong.
  999 CONTINUE

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PIXDUPE_ERR',
     :     'PIXDUPE: Unable to duplicate pixels an NDF.', STATUS )
      END IF

      END

