      SUBROUTINE KPS1_CPBR( NEL, VAR, DATREF, DATOUT, VAROUT, NBAD,
     :                      STATUS )
*+
*  Name:
*     KPS1_CPBR

*  Purpose:
*     Copies bad pixels from DATREF to DATOUT

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CPBR( NEL, VAR, DATREF, DATOUT, VAROUT, NBAD, STATUS )

*  Description:
*     This routine is called by copybad, and is passed two real
*     arrays, DATOUT and DATREF, which contain the data 
*     values of two NDF files. If any element in DATREF is equal to
*     VAL__BADR, and the corresponding element in DATOUT is not
*     equal to VAL__BADR, then the corresponding element in DATOUT 
*     is set to VAL__BADR. If a variance array is present in NDF OUT,
*     then the corresponding variance element is also set to VAL__BADR.

*  Arguments:
*     NEL = INTEGER (Given)
*         Number of elements in either of the arrays.
*     VAR = LOGICAL (Given)
*         Is a variance array present in NDF OUT ?
*     DATREF( NEL ) = REAL (Given)
*         Array containing the data values of NDF REF.
*     DATOUT( NEL ) = REAL (Given and Returned)
*         Array containing the data values of NDF OUT.
*     VAROUT( NEL ) = REAL (Given and Returned)
*         Array containing the variance values of NDF OUT.
*     NBAD = Integer (Returned)
*         Number of bad pixels in the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     TDCA: Tim D.C. Ash (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1998 (TDCA):
*        Original version.
*     13-OCT-1998 (DSB):
*        - Routine name changed so that different data types can have the
*          same "root" name without exceeding 5 characters ("kps1_cpb").
*        - Re-order the arguments into the standard order ( "Given", then
*          "Given and Returned", then "Returned").
*        - Declare the arrays with an explicit dimension.
*        - Argument names changed to indicate their purpose more clearly.
*        - Insert standard "start of code" comment ("*.") after all the 
*          declarations etc.
*        - Include comments and standard white space in the code.
*        - Remove check which prevented pixels which were already bad in
*          the input NDF being included in the count of bad pixels.
*        - Include the required check on the inherited status value.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER NEL                 ! No of elements
      LOGICAL VAR                 ! Variance array present in OUT ?
      REAL DATREF( NEL )          ! Pixel values from REF

*  Arguments Given and Returned:
      REAL DATOUT( NEL )          ! Pixel values from OUT
      REAL VAROUT( NEL )          ! Variance values from OUT

*  Arguments Returned:
      INTEGER NBAD                ! Number of bad pixels copied

*  Status:
      INTEGER STATUS              ! Global status

*  Internal Variables:
      INTEGER COUNT               ! Loop counter  
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of bad values copied.
      NBAD=0

*  Loop round each element of the arrays.
      DO COUNT = 1, NEL

*  If the reference NDF has a bad data value, ensure that the output DATA
*  and VARIANCE arrays are both bad.
         IF( DATREF( COUNT ) .EQ. VAL__BADR ) THEN

            DATOUT( COUNT ) = VAL__BADR
            IF( VAR ) VAROUT( COUNT ) = VAL__BADR

         END IF

*  Increment the number of bad pixels in the output NDF.
         IF( DATOUT( COUNT ) .EQ. VAL__BADR ) NBAD = NBAD + 1

      END DO

      END 
