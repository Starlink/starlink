      SUBROUTINE LSFUN1( M, N, XC, FVECC )
*+
* Name:
*    LSFUN1

*  Purpose:
*     Evaluates function values for NAG minimisation routine E04FDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LSFUN1( M, N, XC, FVECC )

*  Description:
*     This function is related to application SETSKY.  It services the
*     calls to E04FDF made in subroutine KPS1_SKYFT.  The data needed
*     by this routine is passed from KPS1_SKYFT in common blocks defined
*     in include file SFT_COM.
*
*     The argument XC defines a projection (together with values
*     supplied in common).  This projection is used to transform the sky
*     co-ordinates supplied in common.  The residuals between the
*     resulting image co-ordinates and the image co-ordinates supplied in
*     common are returned in argument FVECC.  Separate residuals are
*     returned for the X and Y axes.
*
*     If an error occurs in this routine, an error is reported and the
*     status value is returned in common variable ISTAT.
      
*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals to be calculated.  M should be two
*        times the number of positions stored in common.
*     N = INTEGER (Given)
*        The number of projection parameters which are allowed to vary.
*     XC( N ) = DOUBLE PRECISION (Given)
*        The projection parameters for which the residuals are required.
*        These projection parameters are stored in the same order as the
*        "P" argument for subroutine IRA_CREAT, except that for each
*        projection parameter value which has been fixed by the user,
*        the remaining elements in XC are shuffled down to occupy the
*        location which otherwise would have been used by the fixed
*        projection parameter.  Fixed parameter values are supplied in
*        common.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        The residuals.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'IRA_PAR'          ! IRA public constants

*  Global Variables:
      INCLUDE 'SFT_COM'          ! Used for communicating with KPS1_SKYFT
*        IPWA = INTEGER (Read)
*           Pointer to work space containing sky longitude values.
*        IPWB = INTEGER (Read)
*           Pointer to work space containing sky latitude values.
*        IPWX = INTEGER (Read)
*           Pointer to work space containing supplied image X values.
*        IPWY = INTEGER (Read)
*           Pointer to work space containing supplied image Y values.
*        IPWXO = INTEGER (Read)
*           Pointer to work space for temporary image X values.
*        IPWYO = INTEGER (Read)
*           Pointer to work space for temporary image Y values.
*        ISTAT = INTEGER (Read and Write)
*           Local status value.
*        NPOSC = INTEGER (Read)
*           No. of supplied sky positions.
      
*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      
*  Arguments Returned:
      DOUBLE PRECISION FVECC( M ) 

*.
      
*  Call a lower level routine to do the work, passing the work arrays
*  using %VAL so that their contents can be accessed.
      CALL KPS1_SKYF3( M, N, NPOSC, XC, %VAL( IPWA ), %VAL( IPWB ),
     :                 %VAL( IPWX ), %VAL( IPWY ),  %VAL( IPWXO ),
     :                 %VAL( IPWYO ), FVECC, ISTAT )

      END 
