      SUBROUTINE KPG1_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )
*+
*  Name:
*     KPG1_MAP

*  Purpose:
*     Obtain mapped access to an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Description:
*     This routine is a wrapper for NDF_MAP which obtains mapped access 
*     to an array component of an NDF, returning a pointer to the mapped 
*     values and a count of the number of elements mapped. This routine
*     additionally converts and NaN or Inf values in the mapped array 
*     into corresponding VAL__BADx values. This is only done if the 
*     data type is _REAL or _DOUBLE.
*
*     In READ mode, the returned pointers are pointers to a temporary NDF
*     into which the requested array components are copied before being
*     modified. In order to ensure that this temporary NDF is annulled
*     correctly, the calling application should enclose calls to this 
*     routine within an NDF_BEGIN/NDF_END block. 
*
*     In UPDATE mode, the values stored in the supplied NDF are modified
*     in situ.
*
*     In WRITE mode, no conversions take place.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component to be mapped: 'DATA',
*        'QUALITY' or 'VARIANCE' (or 'ERROR').
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used for access (e.g. '_REAL').
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode for access to the array: 'READ', 'UPDATE' or
*        'WRITE', with an optional initialisation mode '/BAD' or
*        '/ZERO' appended.
*     PNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped values (see the Notes section).
*     EL = INTEGER (Returned)
*        Number of elements mapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR( * )
      INTEGER EL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DVAL      ! Double precision replacement value
      INTEGER I                  ! Loop count
      INTEGER INDFT              ! Temporary NDF identifier
      INTEGER IPT( 20 )          ! Pointers to NDF array components
      INTEGER NCOMP              ! No. of components being mapped
      INTEGER PLACE              ! Place holder for temporary NDF
      REAL RVAL                  ! Single precision replacement value

*.

*  Call NDF_MAP to map the array.
      CALL NDF_MAP( INDF, COMP, TYPE, MMOD, PNTR, EL, STATUS )

*  Check that the mapping was succesful. Do nothing else if we are in WRITE
*  mode, or if the data type is not floating point (i.e. _REAL or _DOUBLE).
      IF( STATUS .EQ. SAI__OK .AND.
     :    MMOD( 1 : 1 ) .NE. 'W' .AND. MMOD( 1 : 1 ) .NE. 'w' .AND.
     :    ( TYPE( 2 : 2 ) .EQ. 'D' .OR. TYPE( 2 : 2 ) .EQ. 'd' .OR.
     :      TYPE( 2 : 2 ) .EQ. 'R' .OR. TYPE( 2 : 2 ) .EQ. 'r' ) ) THEN
      
*  Count the number of commas in the component list.
         NCOMP = 1
         DO I = 1, LEN( COMP )      
            IF( COMP( I : I ) .EQ. ',' ) NCOMP = NCOMP + 1
         END DO

*  For READ mode, we need to allocate a new array in which the converted
*  data can be stored. We use a temporary NDF so that the array will 
*  automatically be freed when the final NDF_END is called.
         IF( MMOD( 1 : 1 ) .NE. 'U' .AND. 
     :       MMOD( 1 : 1 ) .NE. 'u' ) THEN
            CALL NDF_TEMP( PLACE, STATUS )
            CALL NDF_SCOPY( INDF, ' ', PLACE, INDFT, STATUS )
            CALL NDF_MAP( INDFT, COMP, TYPE, 'WRITE', IPT, EL, STATUS )

*  In UPDATE mode, we can modify the data in situ, so just copy the
*  pointers.
         ELSE
            DO I = 1, NCOMP
               IPT( I ) = PNTR( I )
            END DO

         END IF

*  Loop round each pointer.
         DO I = 1, NCOMP 

*  Convert NaN and Inf values within REAL data.
            IF ( TYPE( 2 : 2 ) .EQ. 'R' .OR. 
     :           TYPE( 2 : 2 ) .EQ. 'r' ) THEN
               RVAL = VAL__BADR
               CALL KPG1_IEEER( EL, RVAL, %VAL( PNTR( I ) ), 
     :                                    %VAL( IPT( I ) ) )

*  Convert NaN and Inf values within _DOUBLE data.
            ELSE IF ( TYPE( 2 : 2 ) .EQ. 'D' .OR. 
     :                TYPE( 2 : 2 ) .EQ. 'd' ) THEN
               DVAL = VAL__BADD
               CALL KPG1_IEEED( EL, DVAL, %VAL( PNTR( I ) ),
     :                                    %VAL( IPT( I ) ) )
 
            END IF

*  Return the pointer to the converted data.
            PNTR( I ) = IPT( I )

         END DO

*  In READ mode, unmap the converted arrays, and then re-map them in READ 
*  mode so that they cannot be accessed. Also, unmap the original NDF
*  components.
         IF( MMOD( 1 : 1 ) .NE. 'U' .AND. 
     :       MMOD( 1 : 1 ) .NE. 'u' ) THEN

            CALL NDF_UNMAP( INDFT, '*', STATUS )
            CALL NDF_MAP( INDFT, COMP, TYPE, 'READ', PNTR, EL, STATUS )
            CALL NDF_UNMAP( INDF, COMP, STATUS )

         END IF

      END IF

      END
