      INTEGER FUNCTION ARD1_GATTR(  ATT, VAL, OLDVAL, PRIM )
*+
*  Name:
*     ARD1_GATTR

*  Purpose:
*     Set/Get a ARD "graphics attribute".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_GATTR(  ATT, VAL, OLDVAL, PRIM )

*  Description:
*     This is a dummy routine to allow ARD to use the graphics
*     facilities of the AST Plot class for "drawing" curves into integer 
*     pixel arrays.

*  Arguments:
*     ATT = INTEGER (Given)
*        An integer identifying the required attribute. 
*     VAL = DOUBLE PRECISION (Given) 
*        A new value to store for the attribute. If this is AST__BAD
*        no value is stored.
*     OLDVAL = DOUBLE PRECISION (Returned) 
*       Returned holding the attribute value.
*     PRIM = INTEGER (Given)
*       The sort of graphics primitive to be drawn with the new attribute.

*  Returned Value:
*     One for success. Zero for failure.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER ATT 
      DOUBLE PRECISION VAL
      INTEGER PRIM 

*  Arguments Returned:
      DOUBLE PRECISION OLDVAL

*  Initialize the returned value to indicate success.
      ARD1_GATTR = 1

      END
