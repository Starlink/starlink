      SUBROUTINE SETVAR( STATUS )
*+
*  Name:
*     SETVAR

*  Purpose:
*     Set new values for the variance component of an NDF data
*     structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETVAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine sets new values for the variance component of an NDF
*     data structure. The data structure is accessed in `update' mode,
*     and new variance values are generated from the NDF's data array
*     by means of a Fortran-like arithmetic expression.  Any previous
*     variance information is over-written with the new values.
*     Alternatively, if a `null' value (!) is given for the variance,
*     then any pre-existing variance information is erased.

*  Usage:
*     ndf variance

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF data structure whose variance values are to be
*        modified.
*     VARIANCE = LITERAL (Read)
*        A Fortran-like arithmetic expression giving the variance value
*        to be assigned to each pixel in terms of the variable DATA,
*        which represents the value of the corresponding data array
*        pixel.  For example, VARIANCE="DATA" implies normal `root N'
*        error estimates, whereas VARIANCE="DATA + 50.7" might be used
*        if a sky background of 50.7 units had previously been
*        subtracted.
*
*        If a `null' value (!) is given for this parameter, then no new
*        variance component will be created and any pre-existing
*        variance values will be erased.

*  Examples:
*     setvar ngc4709 data
*        This sets the variance component within the NDF structure
*        ngc4709 to equal its corresponding data-array component.
*     setvar ndf=arcspec "data - 0.31"
*        This sets the variance component within the NDF structure
*        arcspec to be its corresponding data-array component less a
*        constant 0.31.
*     setvar cube4 variance=!
*        This erases the values of the variance component within
*        the NDF structure cube4, if it exists.

*  Notes:
*     -  All of the standard Fortran 77 intrinsic functions are
*     available for use in the variance expression, plus a few others
*     (see SUN/61 for details and an up-to-date list).
*     -  Calculations are performed using real arithmetic (or double
*     precision if appropriate) and are constrained to be non-negative.
*     -  The data type of the variance component is set to match that of
*     the data component.

*  Related Applications:
*     KAPPA: ERRCLIP; Figaro: GOODVAR.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-DEC-1989 (RFWS):
*        Original version.
*     9-APR-1990 (RFWS):
*        Changed name.
*     1992 January 22 (MJC):
*        Added Usage and Examples items.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     23-JUN-1998 (DSB):
*        Used KPG1_MAP instead of NDF_MAP, so that NaN and Inf values
*        are converted to Starlink BAD values before being used.
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZEXP              ! Length of an expression
      PARAMETER ( SZEXP = 132 )

*  Local Variables:
      CHARACTER * ( 4 ) INV( 1 ) ! Inverse transformation
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Transformation locator
      CHARACTER * ( NDF__SZFRM ) FORM ! Form of the NDF variance
      CHARACTER * ( NDF__SZTYP ) TYPE ! DATA/VARIANCE numeric type
      CHARACTER * ( SZEXP ) EXPRS ! Variance expression
      CHARACTER * ( SZEXP + 21 ) FOR( 1 ) ! Forward transformation
      INTEGER DPNTR( 1 )         ! Pointer to mapped data component
      INTEGER EL                 ! Number of mapped values
      INTEGER IMAP               ! Compiled mapping identifier
      INTEGER INDF               ! NDF identifier
      INTEGER LEXP               ! Length of variance expression
      INTEGER VPNTR( 1 )         ! Pointer to mapped variance component
      LOGICAL BAD                ! Bad pixel flag

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Begin an error context, to allow annulling of errors which may occur.
      CALL ERR_MARK

*  Obtain the NDF structure to be modified and determine the numeric
*  type of its data component.
      CALL NDF_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )
      CALL NDF_TYPE( INDF, 'Data', TYPE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the expression relating variance values to data values.
         EXPRS = 'DATA'
         CALL PAR_GET0C( 'VARIANCE', EXPRS, STATUS )

*  If a null expression was given, then no variance component is
*  required, so annul the error and reset the variance component.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL NDF_RESET( INDF, 'Variance', STATUS )

*  Otherwise, set up the required transformation and create a temporary
*  transformation structure.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            LEXP = MAX( 1, CHR_LEN( EXPRS ) )
            FOR( 1 ) = 'VARIANCE = MAX( 0, ' // EXPRS( : LEXP ) // ' )'
            INV( 1 ) = 'DATA'
            CALL TRN_NEW( 1, 1, FOR, INV, '_REAL:',
     :                    'DATA --> VARIANCE', ' ', ' ', LOCTR, STATUS )

*  Compile the transformation to give a mapping identifier.  Then delete
*  the temporary transformation structure.
            CALL TRN_COMP( LOCTR, .TRUE., IMAP, STATUS )
            CALL TRN1_ANTMP( LOCTR, STATUS )

*  Add context information at this point if an error occurs.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'SETVAR_COMP',
     :         'Error in %VARIANCE expression.', STATUS )

*  Reset any pre-existing variance component and set its data type to
*  match the NDF's data component.
            ELSE
               CALL NDF_RESET( INDF, 'Variance', STATUS )
               CALL NDF_STYPE( TYPE, INDF, 'Variance', STATUS )

*  Determine the numerical precision required to calculate the new
*  variance values.
               IF ( TYPE .NE. '_DOUBLE' ) THEN
                  TYPE =  '_REAL'
               END IF

*  Disable automatic quality masking and map the data array and
*  variance array for reading and writing, respectively.
               CALL NDF_SQMF( .FALSE., INDF, STATUS )
               CALL KPG1_MAP( INDF, 'Data', TYPE, 'READ', DPNTR, EL,
     :                       STATUS )
               CALL KPG1_MAP( INDF, 'Variance', TYPE, 'WRITE', VPNTR, 
     :                       EL, STATUS )

*  See if bad pixels need to be checked for during the calculations.
               CALL NDF_BAD( INDF, 'Data', .FALSE., BAD, STATUS )

*  Calculate the new variance values, using the appropriate precision.

*  Real...
               IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL TRN_TR1R( BAD, EL, %VAL( DPNTR( 1 ) ), IMAP,
     :                           %VAL( VPNTR( 1 ) ), STATUS )

*  Double precision...
               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL TRN_TR1D( BAD, EL, %VAL( DPNTR( 1 ) ), IMAP,
     :                           %VAL( VPNTR( 1 ) ), STATUS )
               END IF

*  Annul the compiled mapping.
               CALL TRN_ANNUL( IMAP, STATUS )

*  Set the variance component's bad pixel flag to .TRUE., since bad
*  pixels may now be present unless the output NDF is primitive.
               CALL NDF_FORM( INDF, 'Variance', FORM, STATUS )

               IF ( FORM .NE. 'PRIMITIVE' ) THEN
                  CALL NDF_SBAD( .TRUE., INDF, 'Variance', STATUS )
               END IF
            END IF
         END IF
      END IF

*  End the error context.
      CALL ERR_RLSE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Add context information if an error occurred.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETVAR_ERR',
     :     'SETVAR: Error setting new variance values for an NDF ' //
     :     'data structure.', STATUS )
      END IF

      END
