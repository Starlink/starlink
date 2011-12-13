      SUBROUTINE SETVAR( STATUS )
*+
*  Name:
*     SETVAR

*  Purpose:
*     Set new values for the VARIANCE component of an NDF data
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
*     This routine sets new values for the VARIANCE component of an NDF
*     data structure.  The new values can be copied from a specified
*     component of a second NDF or can be generated from the supplied
*     NDF's data array by means of a Fortran-like arithmetic expression.
*     Any previous variance information is over-written with the new
*     values.  Alternatively, if a `null' value (!) is given for the
*     variance, then any pre-existing variance information is erased.

*  Usage:
*     ndf variance

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of an NDF array component within the NDF specified by
*        parameter FROM.  The values in this array component are used as
*        the new variance values to be stored in the VARIANCE component
*        of the NDF specified by parameter NDF.  The supplied value must
*        be one of "Data" or "Variance".  ["Data"]
*     FROM = NDF (Read)
*        An NDF data structure containing the values to be used as the
*        new variance values.  The NDF component from which to read the
*        new variance values is specified by parameter COMP.  If NDF is
*        not contained completely within FROM, then the VARIANCE
*        component of NDF will be padded with bad values.  If a null
*        (!) value is supplied, the new variance values are determined
*        by the expression given for parameter VARIANCE. [!]
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
*        VARIANCE component will be created and any pre-existing
*        variance values will be erased.

*  Examples:
*     setvar ngc4709 data
*        This sets the VARIANCE component within the NDF structure
*        ngc4709 to equal its corresponding data-array component.
*     setvar ngc4709 from=noise comp=data
*        This sets the VARIANCE component within the NDF structure
*        ngc4709 to equal the values in the Data array of the NDF
*        structure noise.
*     setvar ndf=arcspec "data - 0.31"
*        This sets the VARIANCE component within the NDF structure
*        arcspec to be its corresponding data-array component less a
*        constant 0.31.
*     setvar cube4 variance=!
*        This erases the values of the VARIANCE component within
*        the NDF structure cube4, if it exists.

*  Notes:
*     -  All of the standard Fortran 77 intrinsic functions are
*     available for use in the variance expression, plus a few others
*     (see SUN/61 for details and an up-to-date list).
*     -  Calculations are performed using real arithmetic (or double
*     precision if appropriate) and are constrained to be non-negative.
*     -  The data type of the VARIANCE component is set to match that of
*     the DATA component.

*  Related Applications:
*     KAPPA: ERRCLIP; Figaro: GOODVAR.

*  Copyright:
*     Copyright (C) 1989-1990, 1992 Science & Engineering Research
*     Council. Copyright (C) 1995, 2004 Central Laboratory of the
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     21-MAR-2004 (DSB):
*        Added parameters FROM and COMP.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZEXP              ! Length of an expression
      PARAMETER ( SZEXP = 132 )

*  Local Variables:
      CHARACTER COMP*8           ! NDF array component to read
      CHARACTER EXPRS*( SZEXP )  ! Variance expression
      CHARACTER FOR( 1 )*( SZEXP + 21 ) ! Forward transformation
      CHARACTER FORM*( NDF__SZFRM )! Form of the NDF variance
      CHARACTER INV( 1 )*4       ! Inverse transformation
      CHARACTER LOCTR*( DAT__SZLOC )! Transformation locator
      CHARACTER TYPE*( NDF__SZTYP )! DATA/VARIANCE numeric type
      INTEGER DPNTR( 1 )         ! Pointer to mapped DATA component
      INTEGER EL                 ! Number of mapped values
      INTEGER IERR               ! Index of first error element
      INTEGER IMAP               ! Compiled mapping identifier
      INTEGER INDF               ! Identifier for NDF to be modified
      INTEGER INDF2              ! Identifier for NDF to be read
      INTEGER INDF3              ! Identifier for NDF section to be read
      INTEGER IPIN               ! Pointer to input array
      INTEGER IPOUT              ! Pointer to output array
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel bounds
      INTEGER LEXP               ! Length of variance expression
      INTEGER NDIM               ! The number of NDF pixel axes
      INTEGER NERR               ! Total number of error elements
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel bounds
      INTEGER VPNTR( 1 )         ! Pointer to mapped VARIANCE component
      LOGICAL BAD                ! Bad pixel flag
      LOGICAL THERE              ! Is the component defined?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Begin an error context, to allow annulling of errors which may occur.
      CALL ERR_MARK

*  Obtain the NDF structure to be modified and determine the numeric
*  type of its DATA component.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )
      CALL NDF_TYPE( INDF, 'Data', TYPE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the NDF structure containing the new Variance values.
         CALL LPG_ASSOC( 'FROM', 'READ', INDF2, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Determine which array component is to be copied.
            CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Variance', .FALSE.,
     :                      COMP, STATUS )

*  Check that the required component exists and report an error if it
*  does not.
            CALL NDF_STATE( INDF2, COMP, THERE, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'COMP', COMP )
               CALL NDF_MSG( 'NDF', INDF2 )
               CALL ERR_REP( 'SETVAR_NOCOMP', 'The ^COMP component is'//
     :                       ' undefined in the NDF structure ^NDF',
     :                       STATUS )
            END IF

*  Get a section of the "FROM" NDF which matches the bounds of the "NDF" NDF.
            CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
            CALL NDF_SECT( INDF2, NDIM, LBND, UBND, INDF3, STATUS )

*  Determine the numerical precision required to calculate the new
*  variance values.
            IF ( TYPE .NE. '_DOUBLE' ) THEN
               TYPE =  '_REAL'
            END IF

*  Disable automatic quality masking and map the selected component of
*  the "FROM" NDF.
            CALL NDF_SQMF( .FALSE., INDF3, STATUS )
            CALL KPG1_MAP( INDF3, COMP, TYPE, 'READ', IPIN, EL,
     :                     STATUS )

*  Disable automatic quality masking and map the VARIANCE component of
*  the "NDF" NDF.
            CALL NDF_SQMF( .FALSE., INDF, STATUS )
            CALL KPG1_MAP( INDF, 'Variance', TYPE, 'WRITE', IPOUT, EL,
     :                     STATUS )

*  Copy the array values.
            IF( TYPE .EQ. '_REAL' ) THEN
               CALL VEC_RTOR( .FALSE., EL, %VAL( CNF_PVAL( IPIN ) ),
     :                        %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                        STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( IPIN ) ),
     :                        %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                        STATUS )
            END IF

*  If a null value was supplied for "FROM", annul the error and continue
*  to use the VARIANCE expression to specify the new Variance values.
         ELSE IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Obtain the expression relating variance values to data values.
            EXPRS = 'DATA'
            CALL PAR_GET0C( 'VARIANCE', EXPRS, STATUS )

*  If a null expression was given, then no VARIANCE component is
*  required, so annul the error and reset the VARIANCE component.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL NDF_RESET( INDF, 'Variance', STATUS )

*  Otherwise, set up the required transformation and create a temporary
*  transformation structure.
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               LEXP = MAX( 1, CHR_LEN( EXPRS ) )
               FOR( 1 ) = 'VARIANCE = MAX( 0, ' // EXPRS( : LEXP ) //
     :                    ' )'
               INV( 1 ) = 'DATA'
               CALL TRN_NEW( 1, 1, FOR, INV, '_REAL:',
     :                       'DATA --> VARIANCE', ' ', ' ', LOCTR,
     :                       STATUS )

*  Compile the transformation to give a mapping identifier.  Then delete
*  the temporary transformation structure.
               CALL TRN_COMP( LOCTR, .TRUE., IMAP, STATUS )
               CALL TRN1_ANTMP( LOCTR, STATUS )

*  Add context information at this point if an error occurs.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'SETVAR_COMP',
     :            'Error in %VARIANCE expression.', STATUS )

*  Reset any pre-existing VARIANCE component and set its data type to
*  match the NDF's DATA component.
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
     :                          STATUS )
                  CALL KPG1_MAP( INDF, 'Variance', TYPE, 'WRITE', VPNTR,
     :                          EL, STATUS )

*  See if bad pixels need to be checked for during the calculations.
                  CALL NDF_BAD( INDF, 'Data', .FALSE., BAD, STATUS )

*  Calculate the new variance values, using the appropriate precision.

*  Real...
                  IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL TRN_TR1R( BAD, EL,
     :                              %VAL( CNF_PVAL( DPNTR( 1 ) ) ),
     :                              IMAP,
     :                              %VAL( CNF_PVAL( VPNTR( 1 ) ) ),
     :                              STATUS )

*  Double precision...
                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL TRN_TR1D( BAD, EL,
     :                              %VAL( CNF_PVAL( DPNTR( 1 ) ) ),
     :                              IMAP,
     :                              %VAL( CNF_PVAL( VPNTR( 1 ) ) ),
     :                              STATUS )
                  END IF

*  Annul the compiled mapping.
                  CALL TRN_ANNUL( IMAP, STATUS )

*  Set the VARIANCE component's bad pixel flag to .TRUE., since bad
*  pixels may now be present unless the output NDF is primitive.
                  CALL NDF_FORM( INDF, 'Variance', FORM, STATUS )

                  IF ( FORM .NE. 'PRIMITIVE' ) THEN
                     CALL NDF_SBAD( .TRUE., INDF, 'Variance', STATUS )
                  END IF
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
