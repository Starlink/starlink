      SUBROUTINE CAP_GSRNG (STATUS)
*+
*  Name:
*     CAP_GSRNG
*  Purpose:
*     List all the columns available for a range selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSRNG (STATUS)
*  Description:
*     List all the names of all the columns available for a range
*     selection.
*
*     Currently only ordered (sorted) columns are listed; ultimately
*     indexed columns will also be included.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Do while there are more columns to be output
*         Attempt to get a new column identifier.
*         If the status is ok and the identifier is not null then
*           Get the order of the column.
*           If the column is in ascending or descending order then
*             Increment the number of ordered columns.
*             Get the name of the column.
*             Output the name.
*           end if
*         else
*           Set the termination flag.
*         end if
*         If an error status was raised then
*           Set the termination flag.
*         end if
*       end do
*       If no columns were found then
*         Report a warning.
*       end if
*     else
*       Report warning: catalogue not open.
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     Currently only ordered (sorted) columns are chosen; ultimately
*     indexed columns will also be included.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/10/94  (ACD): Original version.
*     20/10/94 (ACD): First stable version.
*     11/4/95  (ACD): Changed the name of the null identifier.
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
*  Status:
      INTEGER STATUS              ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,          ! Catalogue identifier.
     :  COUNT,       ! Number of the current column.
     :  ORDCNT,      ! Number of ordered (sorted) columns.
     :  FI,          ! Column identifier.
     :  ORDER,       ! Order of the current column.
     :  LENGTH       ! Length of string (excl. trail. blanks).
      LOGICAL
     :  MORE         ! Flag; continue listing columns?
      CHARACTER
     :  NAME*(CAT__SZCMP)   ! Column: name.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Output names of the columns.

            CI = CI__SGZ

            COUNT = 0
            ORDCNT = 0

            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to get a new column identifier; proceed if all
*             is ok and the identifier is not null.

               COUNT = COUNT + 1

               CALL CAT_TNDNT (CI, CAT__FITYP, COUNT, FI, STATUS)

               IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*                Get the order of the column and proceed if the order
*                is ascending or descending.

                  CALL CAT_TIQAI (FI, 'ORDER', ORDER, STATUS)

                  IF (ORDER .EQ. CAT__ASCND  .OR.
     :                ORDER .EQ. CAT__DSCND) THEN
                     ORDCNT = ORDCNT + 1

*
*                   Get the name of the column.

                     CALL CAT_TIQAC (FI, 'NAME', NAME, STATUS)

*
*                   Output the name.

                     IF (NAME .NE. ' ') THEN
                        LENGTH = CHR_LEN(NAME)
                     ELSE
                        LENGTH = 1
                     END IF

                     CALL CAP_OUT (GUI__SGZ, ' ', NAME(1 : LENGTH),
     :                 STATUS)
                  END IF

               ELSE

*
*                Either a bad status was raised or the null identifier
*                was returned; set the termination flag.

                  MORE = .FALSE.
               END IF

*
*             Check if any error status has been raised and if so then
*             set the termination flag.

               IF (STATUS .NE. SAI__OK) THEN
                  MORE = .FALSE.
               END IF
            END DO

*
*          If no ordered columns were found then output a warning.

            IF (ORDCNT .EQ. 0) THEN
               CALL CAP_WARN (GUI__SGZ, ' ', 'This catalogue '/
     :           /'has no columns suitable for a range selection.',
     :           STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('COUNT', COUNT)
            CALL ERR_REP ('CAP_GLCOL_ERR', 'Error getting name '/
     :        /'of column number ^COUNT.', STATUS)
         END IF

      END IF

      END
