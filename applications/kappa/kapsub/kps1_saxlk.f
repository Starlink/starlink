      SUBROUTINE KPS1_SAXLK( INDF1, INDF2, STATUS )
*+
*  Name:
*     KPS1_SAXLK
 
*  Purpose:
*     Copy an AXIS system from one NDF to another.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPS1_SAXLK( INDF1, INDF2, STATUS )
 
*  Description:
*     This routine copies the entire array of AXIS structures from INDF2
*     to INDF1. These axis arrays are extrapolated as necessary by the
*     NDF library.
 
*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the destination NDF.
*     INDF2 = INTEGER (Given)
*        Identifier for the source NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}
 
*  History:
*     16 MAY 2000 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
 
*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Constants:
      INTEGER NCCOMP             ! No. of character components
      PARAMETER ( NCCOMP = 2 )
      INTEGER NACOMP             ! No. of array components
      PARAMETER ( NACOMP = 3 )
 
*  Local Variables:
      CHARACTER ACOMPS( NACOMP )*8 ! AXIS array components
      CHARACTER CCOMPS( NCCOMP )*5 ! AXIS character components
      CHARACTER COMP*8             ! AXIS component name
      CHARACTER TYPE*( NDF__SZTYP )! Array numeric type
      CHARACTER VALUE*256          ! Character component value
      INTEGER EL                   ! No. of elements in an axis array
      INTEGER IAX                  ! Axis index
      INTEGER ICOMP                ! Component index
      INTEGER IERR                 ! Index of first numerical error
      INTEGER INDF2S               ! NDF identifier for source section
      INTEGER IP1                  ! Pointer to destination array
      INTEGER IP2                  ! Pointer to source array
      INTEGER LBND( NDF__MXDIM )   ! Lower axis bounds of destination
      INTEGER NDIM                 ! No. of axes in destination
      INTEGER NERR                 ! No. of numerical errors
      INTEGER UBND( NDF__MXDIM )   ! Upper axis bounds of destination
      INTEGER VLEN                 ! Length of character component
      LOGICAL NORM                 ! Axis normalization error
      LOGICAL THERE                ! Is the component there?

      DATA CCOMPS / 'LABEL', 'UNITS' /
      DATA ACOMPS / 'CENTRE', 'WIDTH', 'VARIANCE' /
*.
 
*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Reset the destination AXIS structures.
      CALL NDF_RESET( INDF1, 'AXIS', STATUS ) 

*  Find the pixel bounds of the destination NDF.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS ) 

*  Take a section from the source NDF which matches the bounds of the
*  destination NDF.
      CALL NDF_SECT( INDF2, NDIM, LBND, UBND, INDF2S, STATUS ) 

*  Check each axis.
      DO IAX = 1, NDIM

*  Loop round each array component.
         DO ICOMP = 1, NACOMP
            COMP = ACOMPS( ICOMP )

*  Pass on if this axis does not have the a defined AXIS component in the 
*  source section.
            CALL NDF_ASTAT( INDF2S, COMP, IAX, THERE, STATUS ) 
            IF( THERE ) THEN

*  Get the numeric type of the array.
               CALL NDF_ATYPE( INDF2S, COMP, IAX, TYPE, STATUS ) 

*  Map the source array.
               CALL NDF_AMAP( INDF2S, COMP, IAX, TYPE, 'READ', IP2, EL, 
     :                        STATUS ) 

*  Map the destination array.
               CALL NDF_AMAP( INDF1, COMP, IAX, TYPE, 'WRITE', IP1, EL, 
     :                        STATUS ) 

*  Copy the source array to the destination array.
               CALL KPG1_COPY( TYPE, EL, IP2, IP1, STATUS )

            END IF
         END DO

*  Loop round each character component.
         DO ICOMP = 1, NCCOMP
            COMP = CCOMPS( ICOMP )

*  Pass on if this axis does not have the a defined AXIS component in the 
*  source section.
            CALL NDF_ASTAT( INDF2S, COMP, IAX, THERE, STATUS ) 
            IF( THERE ) THEN

*  Get the value from the source NDF, and its length.
               CALL NDF_ACGET( INDF2S, COMP, IAX, VALUE, STATUS )
               CALL NDF_ACLEN( INDF2S, COMP, IAX, VLEN, STATUS ) 

*  Put it into destination NDF.
               CALL NDF_ACPUT( VALUE( : VLEN ), INDF1, COMP, IAX, 
     :                         STATUS ) 

            END IF
         END DO

* Get the normalization flag for this axis from the source and put it
*  into the destination.
         CALL NDF_ANORM( INDF2S, IAX, NORM, STATUS ) 
         CALL NDF_ASNRM( NORM, INDF1, IAX, STATUS )

      END DO
 
      END
