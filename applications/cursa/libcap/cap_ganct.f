      SUBROUTINE CAP_GANCT (CIOUT, COMM, STATUS)
*+
*  Name:
*     CAP_GANCT
*  Purpose:
*     Annotate a StarGaze output catalogue with comments.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GANCT (CIOUT, COMM; STATUS)
*  Description:
*     Annotate a catalogue saved from the StarGaze current selection
*     with comments.  These comments include: the name of the base
*     catalogue, the selection criterion and an optional comment string.
*  Arguments:
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     COMM  =  CHARACTER*(*) (Given)
*        Optional comments.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Output the name of the base catalogue.
*     Output the selection criterion.
*     If the optional comments are not blank then
*       Output the optional comments.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     30/10/94 (ACD): Original version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  CIOUT
      CHARACTER
     :  COMM*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  CRIT*(CAT__SZEXP)  ! Selection criterion for current selection.
      INTEGER
     :  LCNAME,  ! Length of CNAME__SGZ (excl. trail. blanks).
     :  LCRIT,   !   "    "  CRIT       ( "  .   "  .   "   ).
     :  LCOMM    !   "    "  COMM       ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Output the name of the base catalogue.

         CALL CAT_PUTXT (CIOUT, 'COMMENT', 'This catalogue is a '/
     :     /'selection from the following catalogue:', STATUS)

         IF (CNAME__SGZ .NE. ' ') THEN
            LCNAME = CHR_LEN(CNAME__SGZ)
         ELSE
            LCNAME = 1
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', CNAME__SGZ(1 : LCNAME),
     :     STATUS)

*
*       Output the selection criterion.

         CALL CAT_PUTXT (CIOUT, 'COMMENT', 'It is defined by the '/
     :     /'following criterion:', STATUS)

         CRIT = CRIT__SGZ(CSEL__SGZ)

         IF (CRIT .NE. ' ') THEN
            LCRIT = CHR_LEN(CRIT)
         ELSE
            LCRIT = 1
         END IF

         CALL CAT_PUTXT (CIOUT, 'COMMENT', CRIT(1 : LCRIT), STATUS)

*
*       Output the comments if they are not blank.

         IF (COMM .NE. ' ') THEN
            CALL CAT_PUTXT (CIOUT, 'COMMENT', ' ', STATUS)

            LCOMM = CHR_LEN(COMM)

            CALL CAT_PUTXT (CIOUT, 'COMMENT', COMM(1 : LCOMM), STATUS)
         END IF

      END IF

      END
