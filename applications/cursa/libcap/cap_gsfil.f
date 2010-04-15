      SUBROUTINE CAP_GSFIL (FPRINT, FPGSZE, FWID, FSUMM, FCOL, FPAR,
     :  FTXT, FTABL, STATUS)
*+
*  Name:
*     CAP_GSFIL
*  Purpose:
*     Set the configuration details for the output file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSFIL (FPRINT, FPGSZE, FWID, FSUMM, FCOL, FPAR, FTXT,
*       FTABL; STATUS)
*  Description:
*     Set the configuration details for the output file.
*  Arguments:
*     FPRINT  =  LOGICAL (Given)
*        Flag; is the file a print file or a data file?  Coded as follows:
*        .TRUE.  -  print file,
*        .FALSE. -  data file.
*     FPGSZE  =  INTEGER (Given)
*        The number of lines in a single page of the output file.
*     FWID  =  INTEGER (Given)
*        The maximum permitted number of characters in a single line
*        of the output file.
*     FSUMM  =  CHARACTER*(*) (Given)
*        Flag indicating whether a summary of the catalogue is to be
*        included in the output file.  It is coded as follows:
*        'A'  -  the summary should be absent,
*        'F'  -  a summary should be included.
*     FCOL  =  CHARACTER*(*) (Given)
*        Flag indicating whether details of the columns are to be
*        included in the output file.  It is coded as follows:
*        'A'  -  details of the columns should be absent,
*        'S'  -  include only a summary of the columns,
*        'F'  -  include full details of the columns.
*     FPAR  =  CHARACTER*(*) (Given)
*        Flag indicating whether details of the parameters are to be
*        included in the output file.  It is coded as follows:
*        'A'  -  details of the parameters should be absent,
*        'S'  -  include only a summary of the parameters,
*        'F'  -  include full details of the parameters.
*     FTXT  =  CHARACTER*(*) (Given)
*        Flag indicating whether header text is to be included in the
*        output file.  It is coded as follows:
*        'A'  -  the text should be absent,
*        'F'  -  include the text.
*     FTABL  =  CHARACTER*(*) (Given)
*        Flag indicating whether the data table for the catalogue is to be
*        included in the output file.  It is coded as follows:
*        'A'  -  the table should be absent,
*        'S'  -  include the table, but WITHOUT COLUMN HEADINGS,
*        'F'  -  include the table, with column headings.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each item:-
*       If the file is a print file then
*         If the value for the item is valid then
*           Copy it to the common block.
*         else
*           Report a warning.
*           Copy a default value to the common block.
*         end if
*       else
*         Set appropriate value to the common block.
*       end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/9/94  (ACD): Original version.
*     22/10/94 (ACD): First stable version.
*     21/6/99  (ACD): Added handling of print and data files.
*     1/7/99   (ACD): Replaced explicit values for line lengths with
*       parameteric constants.
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
      LOGICAL
     :  FPRINT
      INTEGER
     :  FPGSZE,
     :  FWID
      CHARACTER
     :  FSUMM*(*),
     :  FCOL*(*),
     :  FPAR*(*),
     :  FTXT*(*),
     :  FTABL*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check whether a print or data file is required.

         FPRNT__SGZ = FPRINT

         IF (FPRINT) THEN

*
*          Check the page size.

            IF (FPGSZE .GT. 0) THEN
               FPGSZ__SGZ = FPGSZE

            ELSE
               CALL MSG_SETI ('FPGSZE', FPGSZE)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid page size: '/
     :           /'^FPGSZE', STATUS)

               FPGSZ__SGZ = 55

            END IF

*
*          Check the page width.

            IF (FWID .GT. 0  .AND.  FWID .LE. SGZ__SZOPR) THEN
               FWID__SGZ = FWID

            ELSE
               CALL MSG_SETI ('FWID', FWID)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid page width: '/
     :           /'^FWID', STATUS)

               FWID__SGZ = 75

            END IF

*
*          Check the summary flag.

            IF (FSUMM(1 : 1) .EQ. 'A') THEN
               FSUMM__SGZ = SGZ__FABNT

            ELSE IF (FSUMM(1 : 1) .EQ. 'F') THEN
               FSUMM__SGZ = SGZ__FFULL

            ELSE
               CALL MSG_SETC ('FSUMM', FSUMM)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid level of detail '/
     :           /'specified for summary: ^FSUMM', STATUS)

               FSUMM__SGZ = SGZ__FFULL

            END IF

*
*          Check the columns flag.

            IF (FCOL(1 : 1) .EQ. 'A') THEN
               FCOL__SGZ = SGZ__FABNT

            ELSE IF (FCOL(1 : 1) .EQ. 'S') THEN
               FCOL__SGZ = SGZ__FSUMM

            ELSE IF (FCOL(1 : 1) .EQ. 'F') THEN
               FCOL__SGZ = SGZ__FFULL

            ELSE
               CALL MSG_SETC ('FCOL', FCOL)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid level of detail '/
     :           /'specified for columns: ^FCOL', STATUS)

               FCOL__SGZ = SGZ__FSUMM

            END IF

*
*          Check the parameters flag.

            IF (FPAR(1 : 1) .EQ. 'A') THEN
               FPAR__SGZ = SGZ__FABNT

            ELSE IF (FPAR(1 : 1) .EQ. 'S') THEN
               FPAR__SGZ = SGZ__FSUMM

            ELSE IF (FPAR(1 : 1) .EQ. 'F') THEN
               FPAR__SGZ = SGZ__FFULL

            ELSE
               CALL MSG_SETC ('FPAR', FPAR)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid level of detail '/
     :           /'specified for parameters: ^FPAR', STATUS)

               FPAR__SGZ = SGZ__FSUMM

            END IF

*
*          Check the text flag.

            IF (FTXT(1 : 1) .EQ. 'A') THEN
               FTXT__SGZ = SGZ__FABNT

            ELSE IF (FTXT(1 : 1) .EQ. 'F') THEN
               FTXT__SGZ = SGZ__FFULL

            ELSE
               CALL MSG_SETC ('FTXT', FTXT)
               CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid level of detail '/
     :           /'specified for text: ^FTXT', STATUS)

               FTXT__SGZ = SGZ__FSUMM

            END IF

         ELSE
            FSUMM__SGZ = SGZ__FABNT
            FCOL__SGZ = SGZ__FABNT
            FPAR__SGZ = SGZ__FABNT
            FTXT__SGZ = SGZ__FABNT

         END IF

*
*       Check the table flag.

         IF (FTABL(1 : 1) .EQ. 'A') THEN
            FTABL__SGZ = SGZ__FABNT

         ELSE IF (FTABL(1 : 1) .EQ. 'S') THEN
            FTABL__SGZ = SGZ__FSUMM

         ELSE IF (FTABL(1 : 1) .EQ. 'F') THEN
            FTABL__SGZ = SGZ__FFULL

         ELSE
            CALL MSG_SETC ('FTABL', FTABL)
            CALL CAP_WARN (GUI__SGZ, ' ', 'Invalid level of detail '/
     :        /'specified for table: ^FTABL', STATUS)

            FTABL__SGZ = SGZ__FSUMM

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_GSFIL_ERR', 'Error setting the'/
     :        /'configuration of the text file.', STATUS)
         END IF

      END IF

      END
