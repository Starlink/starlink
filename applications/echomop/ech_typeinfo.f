      SUBROUTINE ECH_TYPEINFO( TYPE, TYPE_CODE, UNIT_SIZE )
*+
*  Name:
*     ECHOMOP - ECH_TYPEINFO

*  Purpose:
*     Gets type specific information (eg size).

*  Description:
*     This routine retreives information about the Data system access codes
*     and unitary byte size of a particular data type.

*  Invocation:
*     CALL ECH_TYPEINFO( TYPE, TYPE_CODE, UNIT_SIZE )

*  Arguments:
*     TYPE = CHAR (Given)
*        Type to be checked.
*     TYPE_CODE = INTEGER (Returned)
*        Data structure type code for this type.
*     UNIT_SIZE = INTEGER (Returned)
*        Size in bytes of one object of this type.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     05-APR-1996 (MJC):
*       New prologue, tidy up.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_DTATCON.INC'

*  Arguments Given:
      CHARACTER*( * ) TYPE

*  Arguments Returned:
      INTEGER UNIT_SIZE
      INTEGER TYPE_CODE
*-

      IF ( TYPE .EQ. 'BYTE' ) THEN
         UNIT_SIZE = 1
         TYPE_CODE = TYP_DSBYTE

      ELSE IF ( TYPE .EQ. 'SHORT' ) THEN
         UNIT_SIZE = 2
         TYPE_CODE = TYP_DSSHORT

      ELSE IF ( TYPE .EQ. 'INT' ) THEN
         UNIT_SIZE = 4
         TYPE_CODE = TYP_DSINT

      ELSE IF ( TYPE .EQ. 'IMAGE' ) THEN
         UNIT_SIZE = 4

      ELSE IF ( TYPE .EQ. 'LONG' ) THEN
         UNIT_SIZE = 4
         TYPE_CODE = TYP_DSLONG

      ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN
         UNIT_SIZE = 4
         TYPE_CODE = TYP_DSFLOAT

      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
         UNIT_SIZE = 8
         TYPE_CODE = TYP_DSDOUBLE

      ELSE IF ( TYPE .EQ. 'CHAR' ) THEN
         UNIT_SIZE = 1
         TYPE_CODE = TYP_DSCHAR
      END IF

      END
