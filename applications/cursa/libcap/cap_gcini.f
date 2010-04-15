      SUBROUTINE CAP_GCINI (CNAME, CI, STATUS)
*+
*  Name:
*     CAP_GCINI
*  Purpose:
*     Initialise StarGaze for a new catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCINI (CNAME, CI; STATUS)
*  Description:
*     Initialise StarGaze for a new catalogue.
*  Arguments:
*     CNAME  =  CHARACTER*(*) (Given)
*        Name of the catalogue.
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the catalogue name and identifier.
*     Set the number of selections to one.
*     Set the first selection to be the entire catalogue.
*     Set the current selection to be the first selection.
*     Set the current row to 1.
*     Set a sequence number to be listed and set its width.
*     Assemble the default list of components to be listed.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94 (ACD): Original version.
*     2/6/94  (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     9/3/95  (ACD): Modified to reflect the changes which isolated the
*        computation of the lines of columns to be displayed to the
*        routines doing the displaying.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  CNAME*(*)
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS              ! Global status
*  Local Variables:
      INTEGER
     :  ROWS                      ! No. of rows in the catalogue.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the catalogue name and identifier.

         CNAME__SGZ  = CNAME
         CI__SGZ = CI
         COPEN__SGZ = .TRUE.

*
*       Set the number of selections to one.

         SELS__SGZ = 1

*
*       Set the first selection to be the entire catalogue.

         CRIT__SGZ(1) = 'Entire catalogue.'
         SELBS__SGZ(1) = 0
         SELID__SGZ(1) = CI

         CALL CAT_TROWS (CI, ROWS, STATUS)
         SELRW__SGZ(1) = ROWS

*
*       Set the current selection to be the first selection.

         CSEL__SGZ = 1

*
*       Set the current row to be the first row in the current
*       selection.

         CROW__SGZ = 1

*
*       Set a sequence number to be listed and set its details.

         RUN__SGZ = INT(LOG10(REAL(ROWS) ) ) + 2
         RUN__SGZ = MAX(RUN__SGZ, 3)

*
*       Assemble the default list of columns to be listed.

         CALL CAP_GCPID (CI, SGZ__MXCMP, CMPS__SGZ, CMPID__SGZ,
     :     CMPNM__SGZ, STATUS)

      END IF

      END
