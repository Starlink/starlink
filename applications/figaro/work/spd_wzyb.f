      SUBROUTINE SPD_WZYB( DCENTR, USEVAL, WIDTH, VARUSE,
     :   SPAXIS, NELM, DIM, FILENO, FORM, BADVAL,
     :   CEN1, CEN2, CEN3, CEN4, CEN5, CEN6, CEN7,
     :   WID1, WID2, WID3, WID4, WID5, WID6, WID7,
     :   SCEN, SWID, DAT, VAR, DBL1, DBL2, STATUS )
*+
*  Name:
*     SPD_WZYB

*  Purpose:
*     Write a number of columns to an ASCII table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZYB( DCENTR, USEVAL, WIDTH, VARUSE,
*        SPAXIS, NELM, DIM, FILENO, FORM, BADVAL,
*        CEN1, CEN2, CEN3, CEN4, CEN5, CEN6, CEN7,
*        WID1, WID2, WID3, WID4, WID5, WID6, WID7,
*        SCEN, SWID, DAT, VAR, DBL1, DBL2, STATUS )

*  Description:
*     This routine writes the contents of a 7-D data array to an ASCII
*     table. The table has columns for the centres (and widths if
*     desired) of all non-degenerate axes (i.e. all axes longer than one
*     pixel), for the data, and optionally for the errors. Bad data and
*     bad or negative variances are converted to a specified number
*     before being written to the table.
*
*     One of the axes has special meaning as what the Specdre package
*     calls the spectroscopic axis. The centre and width for this axis
*     can be ordinary vectors as long as the axis or they can be arrays
*     of the same shape as the data array. This corresponds to the
*     SPECVALS and SPECWIDS components in the Specdre Extension.
*
*     The centre values of the spectroscopic axis can be passed as
*     double precision vector or array.
*
*     NOTE that all ordinary centre (and possibly width) vectors must be
*     given. See description of the arguments.

*  Arguments:
*     DCENTR = LOGICAL (Given)
*        True if the pixel centres along the spectroscopic axis are to
*        be written with higher precision.
*     USEVAL = LOGICAL (Given)
*        True if the pixel centres and widths along the spectroscopic
*        axis are given in an array of length NELM rather than
*        DIM(SPAXIS). Typically such arrays are the Specdre Extension's
*        SPECVALS and SPECWIDS.
*     WIDTH = LOGICAL (Given)
*        True if widths are to be written in addition to centres.
*     VARUSE = LOGICAL (Given)
*        True if errors are to be written in addition to data.
*     SPAXIS = INTEGER (Given)
*        The number of the spectroscopic axis.
*     NELM = INTEGER (Given)
*        The length of the data and variance arrays.
*     DIM( NDF__MXDIM ) = INTEGER (Given)
*        The lengths of the axis centre vectors.
*     FILENO = INTEGER (Given)
*        The Fortran unit number to write the ASCII table to.
*     FORM = CHARACTER * ( * ) (Given)
*        The Fortran format to be used to write the ASCII table. Such a
*        format is calculated by the routine ASCWRH.
*     BADVAL = REAL (Given)
*        The value to be written instead of the bad values VAL__BADR.
*     CENi( DIM(i) ) = REAL ( i = 1 ... 7 ) (Given)
*        The vector of pixel centres along the i-th axis. Each of these
*        arrays must be given and must have sufficient length. If the
*        axis is degenerate (i.e. of length 1), then that one element
*        must be given, albeit an incorrect value. The value is not
*        used, but the vector element is referred to in an assignment
*        statement. This would fail if CENi was located at an arbitrary
*        address in memory. Similarly, if DCENTR is true, the values
*        from the spectroscopic axis' CEN-vector are not used, but
*        referred to. In this case it is advised to pass the WID-vector,
*        which has the same type and length, or to pass the DAT-array,
*        which has the same type and at least the same length.
*     WIDi( DIM(i) ) = REAL ( i = 1 ... 7 ) (Given)
*        The vector of pixel widths along the i-th axis. If WIDTH is
*        is true, then each of these arrays must be given and must have
*        sufficient length. If the axis is degenerate (i.e. of length
*        1), then that one element must be given, albeit an incorrect
*        value. The value is not used, but the vector element is
*        referred to in an assignment statement. This would fail if WIDi
*        was located at an arbitrary address in memory.
*     SCEN( NELM ) = REAL (Given)
*        The array of spectroscopic values. This is an array of the same
*        shape as the data array, but of the same meaning as the
*        spectroscopic axis' centre vector. Typically this array is
*        SPECVALS from the Specdre Extension. The array is referred to
*        if and only if ( USEVAL .AND. .NOT.DCENTR ).
*     SWID( NELM ) = REAL (Given)
*        The array of spectroscopic values. This is an array of the same
*        shape as the data array, but of the same meaning as the
*        spectroscopic axis' centre vector. Typically this array is
*        SPECWIDS from the Specdre Extension. The array is referred to
*        if and only if ( USEVAL .AND. WIDTH ).
*     DAT( NELM ) = REAL (Given)
*        The data array.
*     VAR( NELM ) = REAL (Given)
*        The variance array. This array is referred to if and only if
*        VARUSE is true.
*     DBL1( DIM(SPAXIS) ) = DOUBLE PRECISION (Given)
*        The normal length double precision vector of pixel centres
*        along the spectroscopic axis. This is referred to if and only
*        if ( DCENTR .AND. .NOT.USEVAL ).
*     DBL2( NELM ) = DOUBLE PRECISION (Given)
*        The increased length double precision array of pixel centres
*        along the spectroscopic axis. Typically this array is SPECVALS
*        from the Specdre Extension. The array is referred to if and
*        only if ( DCENTR .AND. USEVAL ).
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set to SAI__ERROR
*        -  if more than MAXCOL (=8) columns would have to be written,
*        -  if the data array is declared longer than the product of all
*           axis lengths.

*  Algorithm:
*     This routine gets a lot of arrays, usually only known as pointers
*     to the calling routine. The routine is fixed to a data set
*     dimensionality of NDF__MXDIM or 7, assuming that these are equal.
*
*     To keep this routine relatively simple all CEN vectors must be
*     given, even if their expected length is only 1. Also if WIDTH is
*     true, all WID vectors must be given with sufficient length. All
*     CEN vectors must be given even if one of them is the spectroscopic
*     axis and if it should be written from SCEN, DBL1 or DBL2.
*
*     All CEN (and possibly WID) vectors must be given to avoid an
*     access violation when elements of these vectors are copied into
*     local variables. What values are contained in these vectors is
*     important only for non-degenerate axes that are not written from
*     SCEN, DBL1 or DBL2.
*
*     In other words all CEN (WID?) vectors must be given, but they have
*     to contain reasonable numbers only if these are supposed to make
*     it into the ASCII table output.
*
*     The heart of this routine consists of two DO loops (with labels 1
*     and 2). The first loops through the NDF__MXDIM axes and
*     established which axis is actually interesting and to which table
*     column its centre and width values go.
*
*     This information is kept in the vector of integers K(I).
*     K(1) to K(NDF_MXDIM) tell into which column K(i) the i-th axis
*     centre goes.
*     K(1+NDF__MXDIM) to K(NDF__MXDIM+NDF__MXDIM) tell into which column
*     K(i+NDF__MXDIM) the i-th axis width goes.
*     So far the speciality of the spectroscopic axis is still
*     irrelevant. Any degenerate axis centre and width is directed into
*     the MAXCOL+1st column, which exists only in memory, but is not
*     written to the ASCII table.
*
*     When all axes have been allocated their columns, the columns for
*     data and errors are allocated. NCOL is the number of allocated
*     columns. At this point the routine checks that only MAXCOL or less
*     columns are used.
*
*     With the K-vector complete the table can actually be written, line
*     by line from 1 to NELM. This is the second loop. For each line the
*     correct elements from the given column arrays have to be picked
*     and put into the correct element of the line array PVEC. In
*     addition to PVEC there is the scalar PDB, a double precision
*     number that might be printed into column no. K(SPAXIS).
*
*     The loop starts with a 0th-order picking or centres and widths.
*     The COUNT(j)-th element from each CENj is picked into its
*     allocated column. If the j-th axis is degenerate, then K(j) will
*     be MAXCOL+1 and the number is picked into the phantom column that
*     will not be printed. Numbers from several CEN vectors may be
*     picket into that element PVEC(MAXCOL+1), overwriting previous
*     pickings; it's a phantom column after all.
*
*     Then comes the 1st-order picking of spectroscopic value and width.
*     This will be the COUNT(SPAXIS)-th element or the I-th element
*     depending on USEVAL. It will be picked from DBLi or SCEN and into
*     PDB or PVEC(K(SPAXIS)) depending on DCENTR.
*
*     Next the datum and variance are picked, the error calculated, and
*     bad values replaced.
*
*     Now all picking is done and PVEC(1) to PVEC(NCOL) and PDB contain
*     the numbers to be printed. The format was given by the calling
*     routine, which probably got it from ASCWRH. There are of course
*     two different WRITE statements. One will print PDB in place of
*     PVEC(K(SPAXIS)).
*
*     The big loop ends with incrementing the axis counters COUNT(1) to
*     COUNT(NDF__MXDIM). These had all been initialised to 1 before the
*     big loop started. This will first of all increment the counter
*     along the first axis. If this then exceeds the dimension of that
*     axis (if we would move beyond the right edge of the image), then
*     it resets that counter to 1 and increments the next axis' counter
*     (it goes to the start of the next row in the image). If that
*     counter exceeds the dimension (if we would move beyond the top
*     edge), it resets that counter to 1 and increments the next axis'
*     counter (it goes to the start of the next image plane in a cube).
*     And so on. The NDF_MXDIM-th counter cannot overflow because the
*     whole loop ends after NELM = DIM(1) * ... * DIM(NDF__MXDIM)
*     passes. Still there is a check.
*
*     With all COUNT elements updated the big loop attends to the next
*     line of the ASCII table.

*  Notes:
*     This routine assumes in three instances hat NDF__MXDIM = 7.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Arguments Given:
      LOGICAL DCENTR
      LOGICAL USEVAL
      LOGICAL WIDTH
      LOGICAL VARUSE
      INTEGER SPAXIS
      INTEGER NELM
      INTEGER DIM( NDF__MXDIM )
      INTEGER FILENO
      CHARACTER * ( * ) FORM
      REAL BADVAL
      REAL CEN1( 1 )             ! Actually DIM(1)
      REAL CEN2( 1 )             ! Actually DIM(2)
      REAL CEN3( 1 )             ! Actually DIM(3)
      REAL CEN4( 1 )             ! Actually DIM(4)
      REAL CEN5( 1 )             ! Actually DIM(5)
      REAL CEN6( 1 )             ! Actually DIM(6)
      REAL CEN7( 1 )             ! Actually DIM(7)
      REAL WID1( 1 )             ! Actually DIM(1)
      REAL WID2( 1 )             ! Actually DIM(2)
      REAL WID3( 1 )             ! Actually DIM(3)
      REAL WID4( 1 )             ! Actually DIM(4)
      REAL WID5( 1 )             ! Actually DIM(5)
      REAL WID6( 1 )             ! Actually DIM(6)
      REAL WID7( 1 )             ! Actually DIM(7)
      REAL SCEN( NELM )
      REAL SWID( NELM )
      REAL DAT(  NELM )
      REAL VAR(  NELM )
      DOUBLE PRECISION DBL1( 1 ) ! Actually DIM(SPAXIS)
      DOUBLE PRECISION DBL2( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXCOL             ! Maximum number of columns printed
      PARAMETER ( MAXCOL = 8 )

*  Local Variables:
      INTEGER I, J               ! Loop indices
      INTEGER K( 2+2*NDF__MXDIM ) ! Table column numbers for all vectors
      INTEGER COUNT( NDF__MXDIM ) ! Axis pixel counters
      INTEGER NCOL               ! Number of printed columns
      REAL PVEC( MAXCOL + 1 )    ! Print row vector + phantom
      DOUBLE PRECISION PDB       ! Double to be printed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise all axis counters.
*  And work out the destination indicator K(I).
*  I ist the number of the axis.
*  J is the counter for table columns.
*  The column number for the I-th centre vector is stored as K(I). The
*  column number for the I-th width vector is stored as K(I+NDF__MXDIM).
*  MAXCOL+1 is a phantom column which is not printed. It can absorb
*  information for degenerate axes that should not end up in the ASCII
*  output.
      J = 0
      DO 1 I = 1, NDF__MXDIM
         COUNT(I) = 1

*     If this is a non-degenerate axis, allocate column(s) for centre
*     (and width).
         IF ( DIM(I) .GT. 1 ) THEN
            J = J + 1
            K(I) = J
            K(I+NDF__MXDIM) = MAXCOL + 1
            IF ( WIDTH ) THEN
               J = J + 1
               K(I+NDF__MXDIM) = J
            END IF

*     Else (axis is degenerate), allocate the phantom column.
         ELSE
            K(I) = MAXCOL + 1
            K(I+NDF__MXDIM) = MAXCOL + 1
         END IF
 1    CONTINUE

*  Allocate columns for data and variances.
      J = J + 1
      K(1+2*NDF__MXDIM) = J
      IF ( VARUSE ) THEN
         J = J + 1
         K(2+2*NDF__MXDIM) = J
      ELSE
         K(2+2*NDF__MXDIM) = MAXCOL + 1
      END IF

*  Set and check number of columns.
      NCOL = J
      IF ( NCOL .GT. MAXCOL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPABW_TOOMC',
     :      'SPD_WZYB: Error: Too many columns requested.', STATUS )
         GO TO 500
      END IF

*  Loop through data array.
*  Within this loop we use COUNT(I) to count pixels along each axis.
*  The biggest problem is that we are given one vector for each column,
*  but we need a vector for each line. The loop starts with copying one
*  element from each column into the proper element of the current line.
      DO 2 I = 1, NELM

*     Check if the loop through the data array has proceeded beyond what
*     is covered by the combined lengths of all seven axes.
*     (At the end of the loop, the counters are incremented, taking care
*     of the lengths of dimensions of the data array. We cannot do this
*     check there, because after the last data pixel COUNT(7) will
*     always overflow - just at the end of the loop. Normally that is
*     the end of the loop. If not, the loop will return here, and an
*     error must be reported.)
         IF ( COUNT(7) .GT. DIM(7) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPABW_INCSHP',
     :         'SPD_WZYB: Error: Data array too long.',
     :         STATUS )
            GO TO 500
         END IF

*     Copy the axis centres into the print vector. This is not
*     complicated since all centre vectors exist and have DIM(I)
*     elements even if they are degenerate. For the spectroscopic axis
*     there might even be NELM elements. Or it might be double and seem
*     to have 2*DIM(SPAXIS) or 2*NELM elements.
*     There is one PVEC element which is not printed, but absorbs all
*     the rubbish copied here.
*     This goes till 7, assuming NDF_MXDIM = 7.
         PVEC(K(1)) = CEN1(COUNT(1))
         PVEC(K(2)) = CEN2(COUNT(2))
         PVEC(K(3)) = CEN3(COUNT(3))
         PVEC(K(4)) = CEN4(COUNT(4))
         PVEC(K(5)) = CEN5(COUNT(5))
         PVEC(K(6)) = CEN6(COUNT(6))
         PVEC(K(7)) = CEN7(COUNT(7))

*     Copy the axis widths into the print vector.
*     This goes till 7, assuming NDF_MXDIM = 7.
         IF ( WIDTH ) THEN
            PVEC(K(1+NDF__MXDIM)) = WID1(COUNT(1))
            PVEC(K(2+NDF__MXDIM)) = WID2(COUNT(2))
            PVEC(K(3+NDF__MXDIM)) = WID3(COUNT(3))
            PVEC(K(4+NDF__MXDIM)) = WID4(COUNT(4))
            PVEC(K(5+NDF__MXDIM)) = WID5(COUNT(5))
            PVEC(K(6+NDF__MXDIM)) = WID6(COUNT(6))
            PVEC(K(7+NDF__MXDIM)) = WID7(COUNT(7))
         END IF

*     We may have got the wrong centre and width for the spectroscopic
*     axis. That may have to be double presicion or may come from a
*     longer than DIM(SPAXIS) vector, or both.
         IF ( DCENTR .AND. USEVAL ) THEN
            PDB = DBL2(I)
            IF ( WIDTH ) PVEC(K(SPAXIS+NDF__MXDIM)) = SWID(I)
         ELSE IF ( DCENTR ) THEN
            PDB = DBL1(COUNT(SPAXIS))
         ELSE IF ( USEVAL ) THEN
            PVEC(K(SPAXIS)) = SCEN(I)
            IF ( WIDTH ) PVEC(K(SPAXIS+NDF__MXDIM)) = SWID(I)
         END IF

*     Copy the datum and error.
         PVEC(K(1+2*NDF__MXDIM)) = DAT(I)
         IF ( PVEC(K(1+2*NDF__MXDIM)) .EQ. VAL__BADR )
     :      PVEC(K(1+2*NDF__MXDIM)) = BADVAL
         IF ( VARUSE ) THEN
            PVEC(K(2+2*NDF__MXDIM)) = VAR(I)
            IF ( PVEC(K(2+2*NDF__MXDIM)) .EQ. VAL__BADR .OR.
     :           PVEC(K(2+2*NDF__MXDIM)) .LT. 0. ) THEN
               PVEC(K(2+2*NDF__MXDIM)) = BADVAL
            ELSE
               PVEC(K(2+2*NDF__MXDIM)) = SQRT( PVEC(K(2+2*NDF__MXDIM)) )
            END IF
         END IF

*     Write the line to the ASCII table.
*     K(SPAXIS) is the column for spectroscopic values. These might be
*     double precision (PDB) or not (PVEC(K(SPAXIS))).
         IF ( DCENTR ) THEN
            WRITE( FILENO, FORM ) ( PVEC(J), J = 1, K(SPAXIS) - 1 ),
     :         PDB, ( PVEC(J), J = K(SPAXIS) + 1, NCOL )
         ELSE
            WRITE( FILENO, FORM ) ( PVEC(J), J = 1, NCOL )
         END IF

*     Increment axis counters.
*     Increment any axis counter. If it overflows, reset it and
*     increment the next counter. If it overflows, ...
*     This goes till 7, assuming NDF_MXDIM = 7.
         COUNT(1) = COUNT(1) + 1
         IF ( COUNT(1) .GT. DIM(1) ) THEN
            COUNT(1) = 1
            COUNT(2) = COUNT(2) + 1
            IF ( COUNT(2) .GT. DIM(2) ) THEN
               COUNT(2) = 1
               COUNT(3) = COUNT(3) + 1
               IF ( COUNT(3) .GT. DIM(3) ) THEN
                  COUNT(3) = 1
                  COUNT(4) = COUNT(4) + 1
                  IF ( COUNT(4) .GT. DIM(4) ) THEN
                     COUNT(4) = 1
                     COUNT(5) = COUNT(5) + 1
                     IF ( COUNT(5) .GT. DIM(5) ) THEN
                        COUNT(5) = 1
                        COUNT(6) = COUNT(6) + 1
                        IF ( COUNT(6) .GT. DIM(6) ) THEN
                           COUNT(6) = 1
                           COUNT(7) = COUNT(7) + 1
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
 2    CONTINUE

*  Return.
 500  CONTINUE
      END
