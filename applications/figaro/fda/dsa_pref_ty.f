      SUBROUTINE DSA_PREFERRED_TYPE( STRUCT_TYPE, PRIM_TYPE, STATUS )
*+
*  Name:
*     DSA_PREFERRED_TYPE

*  Purpose:
*     Provide the primitive type best suited to a structured array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_PREFERRED_TYPE( STRUCT_TYPE, PRIM_TYPE, STATUS )

*  Description:
*     DSA that enquire the stored type of an array often return
*     something like 'SIMPLE/FLOAT', which is of little use. This
*     routine will extract the primitive type, in the example 'FLOAT',
*     which is much more useful.

*  Arguments:
*     STRUCT_TYPE = CHARACTER * ( * ) (Given)
*        A type as returned by one of the Figaro type enquiry routines.
*        This can be a primitive or structured type.
*     PRIM_TYPE = CHARACTER * ( * ) (Returned)
*        The primitive data type best suited to handling the data whose
*        type is given by STRUCT_TYPE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Feb 1990 (ks):
*        Original version.
*     27 Apr 1990 (ks):
*        Now uses DSA__TYPE_DETAILS and so supports some SGP38
*        structured types.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     21 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRUCT_TYPE

*  Arguments Returned:
      CHARACTER * ( * ) PRIM_TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Pointer into string

*.

      IF ( STATUS .NE. 0 ) RETURN

      I = INDEX( STRUCT_TYPE, '/' )
      IF ( I .GT. 0 ) THEN
         PRIM_TYPE = STRUCT_TYPE(I+1:)
      ELSE
         PRIM_TYPE = STRUCT_TYPE
      END IF

      END
