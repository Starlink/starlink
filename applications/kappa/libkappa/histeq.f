      SUBROUTINE HISTEQ( STATUS )
*+
*  Name:
*     HISTEQ

*  Purpose:
*     Performs an histogram equalisation on an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISTEQ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application transforms an NDF via histogram equalisation.
*     Histogram equalisation is an image-processing technique in which
*     the distribution (between limits) of data values in the input
*     array is adjusted so that in the output array there are
*     approximately equal numbers of elements in each histogram bin.
*     To achieve this the histogram bin size is no longer a constant.
*     This technique is commonly known as histogram equalisation.  It
*     is useful for displaying features across a wide dynamic range,
*     sometimes called a maximum information picture.  The transformed
*     array is output to a new NDF.

*  Usage:
*     histeq in out [numbin]

*  ADAM Parameters:
*     IN  = NDF (Read)
*        The NDF structure to be transformed.
*     NUMBIN = INTEGER (Read)
*        The number of histogram bins to be used.  This should be a
*        large number, say 2000, to reduce quantisation errors.  It
*        must be in the range 100 to 10000. [2048]
*     OUT = NDF (Write)
*        The NDF structure to contain the transformed data array.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     histeq halley maxinf
*        The data array in the NDF called halley is remapped via
*        histogram equalisation to form the new NDF called maxinf.
*     histeq halley maxinf 10000 title="Maximum information of Halley"
*        The data array in the NDF called halley is remapped via
*        histogram equalisation to form the new NDF called maxinf.
*        Ten thousand bins in the histogram are required rather than
*        the default of 2048.  The title of NDF maxinf is
*        "Maximum information of Halley".

*  Notes:
*     If there are a few outliers in the data and most of the points
*     concentrated about a value it may be wise to truncate the
*     data array via THRESH, or have a large number of histogram bins.

*  Algorithm:
*     -  Get the number of bins.
*     -  Associate the input NDF. Inquire whether it may contain bad
*     pixels.  Create the ouptput NDF by propagation, excluding UNITS
*     and VARIANCE.  Obtain the processing type. Map the arrays.
*     -  Obtain workspace for the histogram and key.
*     -  Call a subroutine to perform the histogram equalisation using
*     the appropriate data type.

*  Related Applications:
*     KAPPA: LAPLACE, LUTABLE, SHADOW, THRESH; Figaro: HOPT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, WCS and HISTORY components of an NDF data structure and
*     propagates all extensions.  UNITS and VARIANCE become undefined
*     by the transformation, and so are not propagated.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  Any number of NDF dimensions is supported.
*  
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 November 8 (MJC):
*        Original NDF version.
*     1995 January 12 (MJC):
*        Replaced AIF calls with PAR and PSX.  Lowercase examples and
*        usage.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global environment definitions
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXBIN             ! Maximum numbr of histogram bins
      PARAMETER ( MAXBIN = 10000 )

*  Local Variables:
      LOGICAL BAD                ! Input NDF contains bad values
      INTEGER EL                 ! Number of elements in an array
      INTEGER HPNTR              ! Pointer to the histogram
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Processing type of the image
      INTEGER MPNTR              ! Pointer to the key to histogram
                                 ! transformation
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER PNTRI( 1 )         ! Pointer to input data-array component
      INTEGER PNTRO( 1 )         ! Pointer to output data-array component
      INTEGER NUMBIN             ! Number of bins in histogram

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of histogram bins to be used, within a sensible
*  range.
      CALL PAR_GDR0I( 'NUMBIN', 2048, 100, MAXBIN, .TRUE., NUMBIN,
     :                STATUS )

*  Associate the input NDF. 
*  ========================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the NDF to be displayed.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Inquire whether or not the NDF may have bad values.
      CALL NDF_BAD( NDFI, 'Data', .FALSE., BAD, STATUS )

*  Create the output NDF.
*  ======================

*  Propagate the QUALITY, LABEL, HISTORY, WCS and AXIS components from the
*  input NDF to the output.  Note that UNITS and VARIANCE become
*  undefined following the transformation and so are not propagated.
      CALL LPG_PROP( NDFI, 'WCS,Quality,Axis', 'OUT', NDFO, STATUS )

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
      CALL NDF_TYPE( NDFI, 'Data', ITYPE, STATUS )

*  Obtain a title and assign it to the output NDF.
*  ===============================================
*  A null results in the output title being the same as the input
*  title.
      CALL KPG1_CCPRO( 'TITLE', 'TITLE', NDFI, NDFO, STATUS )

*  Map the data arrays.
*  ====================
      CALL KPG1_MAP( NDFI, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )
      CALL KPG1_MAP( NDFO, 'Data', ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Obtain workspace for the histogram and mapping.
*  ===============================================
      CALL PSX_CALLOC( NUMBIN, '_INTEGER', HPNTR, STATUS )
      CALL PSX_CALLOC( NUMBIN, '_INTEGER', MPNTR, STATUS )

*  Process the array using the appropriate implementation data type.
*  =================================================================
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_HSTQR( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                    %VAL( HPNTR ), %VAL( MPNTR ),
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_HSTQB( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                    %VAL( HPNTR ), %VAL( MPNTR ),
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_HSTQD( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                    %VAL( HPNTR ), %VAL( MPNTR ),
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_HSTQI( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                    %VAL( HPNTR ), %VAL( MPNTR ),
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_HSTQUB( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                     %VAL( HPNTR ), %VAL( MPNTR ),
     :                     %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_HSTQUW( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                     %VAL( HPNTR ), %VAL( MPNTR ),
     :                     %VAL( PNTRO( 1 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_HSTQW( BAD, EL, %VAL( PNTRI( 1 ) ), NUMBIN,
     :                    %VAL( HPNTR ), %VAL( MPNTR ),
     :                    %VAL( PNTRO( 1 ) ), STATUS )

      END IF

*  Tidy the data system.
*  =====================

*  Free the workspace.
      CALL PSX_FREE( HPNTR, STATUS )
      CALL PSX_FREE( MPNTR, STATUS )

*  Tidy the NDF system.
      CALL NDF_END( STATUS )

*  If an error occurred, report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTEQ_ERR',
     :     'HISTEQ: Error performing the histogram equalisation of '/
     :     /'an NDF.', STATUS )
      END IF

      END
