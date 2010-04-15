      SUBROUTINE KPS1_PLCPUB( EL, NEL, IN, USE, FILL, OUT, STATUS )
*+
*  Name:
*     KPS1_PLCPX

*  Purpose:
*     Copy a single element of an array from the input to the output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPUB( EL, NEL, IN, USE, FILL, OUT, STATUS )

*  Description:
*     Stores a value in a specified pixel of the ouput array, either by
*     copying the corresponding pixel value from the input array, or
*     by using a supplied constant value.

*  Arguments:
*     EL = INTEGER (Given)
*        The index of the array element to be copied.
*     NEL = INTEGER (Given)
*        The size of the input and output arrays.
*     IN( NEL ) = BYTE (Given)
*        The input array.
*     USE = LOGICAL (Given)
*        If .TRUE. then copy in the input array value to the output,
*        otherwise use the value of argument FILL.
*     FILL = BYTE (Given)
*        A constant value to store in the output if the input array
*        value is not to be used.
*     OUT( NEL ) = BYTE (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER NEL
      BYTE IN( NEL )
      LOGICAL USE
      BYTE FILL

*  Arguments Returned:
      BYTE OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input array can be used, copy the pixel value to the output
*  array.
      IF( USE ) THEN
         OUT( EL ) = IN( EL )

*  Otherwise, copy the supplied fill value to the output array.
      ELSE
         OUT( EL ) = FILL

      END IF

      END
      SUBROUTINE KPS1_PLCPD( EL, NEL, IN, USE, FILL, OUT, STATUS )
*+
*  Name:
*     KPS1_PLCPX

*  Purpose:
*     Copy a single element of an array from the input to the output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPD( EL, NEL, IN, USE, FILL, OUT, STATUS )

*  Description:
*     Stores a value in a specified pixel of the ouput array, either by
*     copying the corresponding pixel value from the input array, or
*     by using a supplied constant value.

*  Arguments:
*     EL = INTEGER (Given)
*        The index of the array element to be copied.
*     NEL = INTEGER (Given)
*        The size of the input and output arrays.
*     IN( NEL ) = DOUBLE PRECISION (Given)
*        The input array.
*     USE = LOGICAL (Given)
*        If .TRUE. then copy in the input array value to the output,
*        otherwise use the value of argument FILL.
*     FILL = DOUBLE PRECISION (Given)
*        A constant value to store in the output if the input array
*        value is not to be used.
*     OUT( NEL ) = DOUBLE PRECISION (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER NEL
      DOUBLE PRECISION IN( NEL )
      LOGICAL USE
      DOUBLE PRECISION FILL

*  Arguments Returned:
      DOUBLE PRECISION OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input array can be used, copy the pixel value to the output
*  array.
      IF( USE ) THEN
         OUT( EL ) = IN( EL )

*  Otherwise, copy the supplied fill value to the output array.
      ELSE
         OUT( EL ) = FILL

      END IF

      END
      SUBROUTINE KPS1_PLCPI( EL, NEL, IN, USE, FILL, OUT, STATUS )
*+
*  Name:
*     KPS1_PLCPX

*  Purpose:
*     Copy a single element of an array from the input to the output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPI( EL, NEL, IN, USE, FILL, OUT, STATUS )

*  Description:
*     Stores a value in a specified pixel of the ouput array, either by
*     copying the corresponding pixel value from the input array, or
*     by using a supplied constant value.

*  Arguments:
*     EL = INTEGER (Given)
*        The index of the array element to be copied.
*     NEL = INTEGER (Given)
*        The size of the input and output arrays.
*     IN( NEL ) = INTEGER (Given)
*        The input array.
*     USE = LOGICAL (Given)
*        If .TRUE. then copy in the input array value to the output,
*        otherwise use the value of argument FILL.
*     FILL = INTEGER (Given)
*        A constant value to store in the output if the input array
*        value is not to be used.
*     OUT( NEL ) = INTEGER (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER NEL
      INTEGER IN( NEL )
      LOGICAL USE
      INTEGER FILL

*  Arguments Returned:
      INTEGER OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input array can be used, copy the pixel value to the output
*  array.
      IF( USE ) THEN
         OUT( EL ) = IN( EL )

*  Otherwise, copy the supplied fill value to the output array.
      ELSE
         OUT( EL ) = FILL

      END IF

      END
      SUBROUTINE KPS1_PLCPR( EL, NEL, IN, USE, FILL, OUT, STATUS )
*+
*  Name:
*     KPS1_PLCPX

*  Purpose:
*     Copy a single element of an array from the input to the output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPR( EL, NEL, IN, USE, FILL, OUT, STATUS )

*  Description:
*     Stores a value in a specified pixel of the ouput array, either by
*     copying the corresponding pixel value from the input array, or
*     by using a supplied constant value.

*  Arguments:
*     EL = INTEGER (Given)
*        The index of the array element to be copied.
*     NEL = INTEGER (Given)
*        The size of the input and output arrays.
*     IN( NEL ) = REAL (Given)
*        The input array.
*     USE = LOGICAL (Given)
*        If .TRUE. then copy in the input array value to the output,
*        otherwise use the value of argument FILL.
*     FILL = REAL (Given)
*        A constant value to store in the output if the input array
*        value is not to be used.
*     OUT( NEL ) = REAL (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER NEL
      REAL IN( NEL )
      LOGICAL USE
      REAL FILL

*  Arguments Returned:
      REAL OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input array can be used, copy the pixel value to the output
*  array.
      IF( USE ) THEN
         OUT( EL ) = IN( EL )

*  Otherwise, copy the supplied fill value to the output array.
      ELSE
         OUT( EL ) = FILL

      END IF

      END
      SUBROUTINE KPS1_PLCPW( EL, NEL, IN, USE, FILL, OUT, STATUS )
*+
*  Name:
*     KPS1_PLCPX

*  Purpose:
*     Copy a single element of an array from the input to the output

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPW( EL, NEL, IN, USE, FILL, OUT, STATUS )

*  Description:
*     Stores a value in a specified pixel of the ouput array, either by
*     copying the corresponding pixel value from the input array, or
*     by using a supplied constant value.

*  Arguments:
*     EL = INTEGER (Given)
*        The index of the array element to be copied.
*     NEL = INTEGER (Given)
*        The size of the input and output arrays.
*     IN( NEL ) = INTEGER*2 (Given)
*        The input array.
*     USE = LOGICAL (Given)
*        If .TRUE. then copy in the input array value to the output,
*        otherwise use the value of argument FILL.
*     FILL = INTEGER*2 (Given)
*        A constant value to store in the output if the input array
*        value is not to be used.
*     OUT( NEL ) = INTEGER*2 (Returned)
*        The output array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER NEL
      INTEGER*2 IN( NEL )
      LOGICAL USE
      INTEGER*2 FILL

*  Arguments Returned:
      INTEGER*2 OUT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input array can be used, copy the pixel value to the output
*  array.
      IF( USE ) THEN
         OUT( EL ) = IN( EL )

*  Otherwise, copy the supplied fill value to the output array.
      ELSE
         OUT( EL ) = FILL

      END IF

      END
