      SUBROUTINE CREOBJ( STATUS )
*+
*  Name:
*     CREOBJ

*  Purpose:
*     Create an HDS object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CREOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates an HDS object, primitive or structure, scalar
*     or array. In theory it is possible to build up a complete NDF or
*     Figaro DST data file. This is not recommended because the risk of
*     creating an illegal hierarchy of HDS structures - i.e. one not
*     accepted by KAPPA, Figaro, etc. - is very high. This routine is
*     intended only for minor repairs to such files, or in emergencies
*     to create a very simple, minimal, data file.

*  Usage:
*     creobj type dims object

*  ADAM Parameters:
*     TYPE = _CHAR (Read)
*        The HDS type of the object to be created. Figaro users note
*        that this is something like '_REAL', '_DOUBLE', '_INTEGER',
*        '_WORD' etc. Anything which is not such a primitive HDS type
*        will cause a structure to be created. The type specified here
*        will then be used as that structure's type.
*        ['_REAL']
*     DIMS( 7 ) = _INTEGER (Read)
*        The dimensions of the object to be created, i.e. the size of
*        the array along each axis. The number of positive integers
*        specified indicates the dimensionality of the object created.
*        To create a scalar object enter zero, a single zero will do.
*        [0]
*     OBJECT = HDSOBJECT (Read)
*        The object to be created. Specify beginning with directory and
*        file name in the syntax of the operating system, followed by
*        the dot-separated structure hierarchy. Elements of structure
*        arrays are specified in ordinary brackets (). An array element
*        cannot be created.

*  Examples:
*     creobj type=NDF dims=0 object=file
*        This will create an empty HDS file. The top level structure is
*        of type "NDF", which has little consequence.
*     creobj type=ARRAY dims=0 object=file.DATA_ARRAY
*        This will create the scalar structure DATA_ARRAY in the top
*        level structure of the file "file". The structure type is
*        "ARRAY", which has special meaning in the Starlink Data Format.
*     creobj type=_REAL dims=[20,30] object=file.DATA_ARRAY.DATA
*        This will create a two-dimensional array of _REAL numbers
*        called DATA and situated in file.DATA_ARRAY. The size of the
*        new array is 20 by 30 numbers.
*     creobj type=AXIS dims=2 object=file.AXIS
*        This will create a one-dimensional array of AXIS structures
*        called AXIS and situated underneath the top level of "file".

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19-APR-1984 (KS):
*        Original version.
*     26-MAR-1991 (KS):
*        Now allows for the possibility of more than one default file
*        extension.  Support for USHORT and LOGICAL data types added.
*     01-OCT-1992 (HME):
*        Rewritten in terms of HDS.
*     06-APR-1993 (HME):
*        Removed the contextual error message.
*        Renamed from CROBJ.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NDIM
      INTEGER DIMS( DAT__MXDIM )
      CHARACTER * ( DAT__SZTYP ) TYPE
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the type to be used in creation,
      CALL PAR_GET0C( 'TYPE', TYPE, STATUS )

*  Get the dimensions and so some second guessing.
*  In the first instance the dimensionality is assumed to be the number
*  of dimensions returned. But then the given dimensions are checked for
*  being positive. Only dimensions before the first non-positive one are
*  assumed to be intended by the user.
*  Consequently, if the first given dimension is 0 or negative, the
*  dimensionality is 0 as well, which tells DAT_CREAT to create a
*  scalar.
      CALL PAR_GET1I( 'DIMS', DAT__MXDIM, DIMS, NDIM, STATUS )
      DO 1 I = 1, DAT__MXDIM
         IF ( I .LE. NDIM .AND. DIMS(I) .LE. 0 ) NDIM = I - 1
 1    CONTINUE

*  Create the object by association with an ADAM parameter.
      CALL DAT_CREAT( 'OBJECT', TYPE, NDIM, DIMS, STATUS )

      END
