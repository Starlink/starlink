      SUBROUTINE KPG1_HSDSR( NUMBIN, HIST, HRMIN, HRMAX, STATUS )
*+
*  Name:
*     KPG1_HSDSx
 
*  Purpose:
*     Tabulates an histogram.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_HSDSx( NUMBIN, HIST, HRMIN, HRMAX, STATUS )
 
*  Description:
*     This routine reports an histogram to the user.
 
*  Arguments:
*     NUMBIN = INTEGER (Given)
*        The number of bins in the histogram.
*     HIST( NUMBIN ) = INTEGER (Given)
*        The array holding the histogram.
*     HRMIN = ? (Given)
*        The minimum data value that could be included within the
*        histogram.
*     HRMAX = ? (Given)
*        The maximum data value that could be included within the
*        histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for the following numeric data types:
*     replace "x" in the routine name by D or R as appropriate.  The
*     extreme values of the histogram must have the data type
*     specified.
 
*  Algorithm:
*     -  If there are insufficient bins report an error and exit.
*     -  Calculate the binsize.
*     -  Report the title.  For each histogram bin report the range and
*     number in the bin.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1988 July 7 (MJC):
*        Original version called HSTDSP.
*     1992 March 9 (MJC):
*        Made generic, and reordered the NUMBIN argument.  Used SST
*        prologue.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE            ! No default typing allowed
 
*    Global constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
 
*  Arguments Given:
      INTEGER NUMBIN
      INTEGER HIST( NUMBIN )
      REAL HRMIN
      REAL HRMAX
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      REAL NBINWD             ! Width of the report bins
      REAL RANGE              ! The difference between the maximum
                                 ! and the minimum values
 
      INTEGER K                  ! Loop counter
 
      CHARACTER * ( 50 ) TEXT    ! Summary of the histogram
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions
 
*.
 
*  If the status is bad, then return
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Check that the number of bins is valid.
      IF ( NUMBIN .LT. 1 ) THEN
 
*  Report error and set a bad status.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUMBIN', NUMBIN )
         CALL ERR_REP( 'KPG1_HSDSx_ISFBIN',
     :     'KPG1_HSDSx: Insufficient bins (^NUMBIN) in the histogram.',
     :     STATUS )
 
      ELSE
 
*  Calculate the size of the bins for the report.
         RANGE = HRMAX - HRMIN
         NBINWD = RANGE / NUM_ITOR( NUMBIN )
 
*  Report the title of the histogram. Leave a blank line to delineate
*  the histogram and to separate the title from the body of the
*  histogram report.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'TITLE', '                Histogram', STATUS )
         CALL MSG_BLANK( STATUS )
 
*  Report the histogram itself.  Use an internal write to place the
*  values in neat columns. I8 should be adequate for ~30000 pixels
*  square images.
         DO  K = 1, NUMBIN, 1
            WRITE ( TEXT, 10 ) HRMIN + NUM_ITOR( K-1 ) * NBINWD,
     :              HRMIN + NUM_ITOR( K ) * NBINWD, HIST( K )
  10        FORMAT( ' ', 1PG15.7, ' to', 1PG15.7, ' ', I8, ' pixels' )
            CALL MSG_OUT( 'OUTLINE', TEXT, STATUS )
 
         END DO
 
*   Leave a blank line to delineate the histogram.
         CALL MSG_BLANK( STATUS )
      END IF
 
      END
