      SUBROUTINE KPG1_HSTQB( BAD, EL, INARR, NUMBIN, OLDHST, MAP,
     :                         OUTARR, STATUS )
*+
*  Name:
*     KPG1_HSTQx
 
*  Purpose:
*     Equalises the histogram of an array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_HSTQx( BAD, EL, INARR, NUMBIN, OLDHST, MAP, OUTARR,
*                      STATUS )
 
*  Description:
*     This routine equalises (linearises) the histogram of an array.
*     Thus approximately equal numbers of different values appear in
*     output equalised version of the input array.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        If true there may be bad values within the input array.  If
*        false there are no bad values.  It is used as an efficiency
*        mechanism.
*     EL = INTEGER (Given)
*        The dimension of the arrays.
*     INARR( EL ) = ? (Given)
*        The array to be equalised.
*     NUMBIN = INTEGER (Given)
*        Number of bins used in histogram.  A moderately large number of
*        bins is recommended so that there is little artifactual
*        quantisation is introduced, say a few thousand except for
*        byte data.
*     OLDHST( NUMBIN ) = INTEGER (Returned)
*        The histogram of the input array, i.e. before equalisation.
*     MAP( NUMBIN ) = INTEGER (Returned)
*        Key to transform the input histogram's bin number to the
*        equalised bin number after histogram mapping.
*     OUTARR( EL ) = ? (Returned)
*        The array containing the equalised values.
*     STATUS = INTEGER (Given)
*        Global status value.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     arrays supplied to and returned from this routine must have the
*     data type specified.
 
*  Algorithm:
*     -  Get maximum and minimum values in array.  Generate the
*     histogram of the input array.  Initialise cumulative probability
*     variable
*     -  For all bins in the histogram: calculate current bin
*     probability and the cumulative probability; work out the
*     corresponding equalised histogram bin number; and set the key
*     element accordingly to record the mapped bin number.
*     -  Remap the input array according to the key.
 
*  References:
*     Gonzalez, R.C. and Wintz, P., 1977, "Digital Image Processing",
*     Addison-Wesley, pp. 118--126.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 November 8 (MJC):
*        Original version based on HSTQSB.
*     1995 February 21 (MJC):
*        Used modern subroutine prefixes, standardised comment
*        alignment, and sorted the variables.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_new_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
 
*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      BYTE INARR( EL )
      INTEGER NUMBIN
 
*  Arguments Returned:
      INTEGER OLDHST( NUMBIN )
      INTEGER MAP( NUMBIN )
      BYTE OUTARR( EL )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      LOGICAL BAD2               ! There are bad values in the array
      REAL CUMPRB                ! Cumulative probability function
      INTEGER J                  ! Loop counter
      BYTE MAXMUM             ! Maximum value in array
      INTEGER MAXPOS             ! Position of maximum found in array
      BYTE MINMUM             ! Minimum value in array
      INTEGER MINPOS             ! Position of maximum found in array
      INTEGER NEWBIN             ! Bin counter for new histogram
      INTEGER NINVAL             ! Number of bad values in array
      REAL NUMPIX                ! Number of valid values in array
      REAL PROB                  ! Probability function for old bins
 
*.
 
*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.  Derive the number of good pixels.
      CALL KPG1_MXMNB( BAD, EL, INARR, NINVAL, MAXMUM, MINMUM,
     :                   MAXPOS, MINPOS, STATUS )
      NUMPIX = REAL( EL - NINVAL )
 
*  The number of bad pixels has been counted so it might be possible to
*  save future processing.
      BAD2 = BAD .OR. ( NINVAL .EQ. 0 )
 
*  Generate the histogram between those bounds.
      CALL KPG1_GHSTB( BAD2, EL, INARR, NUMBIN, MAXMUM, MINMUM,
     :                   OLDHST, STATUS )
 
*  Initialise the cumulative-probability variable.
      CUMPRB = 0.0
 
*  Loop round all the histogram bins in order to equalise the
*  histogram.
      DO  J = 1, NUMBIN
 
*  Calculate the probability for the jth bin, and the cumulative
*  probability so far.
         PROB = REAL( OLDHST( J ) ) / NUMPIX
         CUMPRB = CUMPRB + PROB
 
*  Work out the corresponding bin in the equalised histogram from the
*  cumulative probability and the number of bins.
         NEWBIN = NINT( CUMPRB * REAL( NUMBIN ) )
         NEWBIN = MIN( NUMBIN, MAX( 1, NEWBIN ) )
 
*  Keep track of which bin in the equalised histogram the pixels in the
*  jth bin of the old histogram belong.
         MAP( J ) = NEWBIN
 
*  End of the loop for all histogram bins to derive equalised
*  histogram.
      END DO
 
*  Apply the transformation via the key contained in the MAP array to
*  create an equalised output array.
      CALL KPG1_RMAPB( EL, INARR, MAXMUM, MINMUM, NUMBIN, MAP, OUTARR,
     :                   STATUS )
 
      END
