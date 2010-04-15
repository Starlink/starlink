      SUBROUTINE CAP_GFOUT (FUNIT, TEXT, STATUS)
*+
*  Name:
*     CAP_GFOUT
*  Purpose:
*     Output a single line to the text file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GFOUT (FUNIT, TEXT; STATUS)
*  Description:
*     Output a single line to the text file.
*  Arguments:
*     FUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the file.
*     TEXT  =  CHARACTER*(*) (Given)
*        Line of text to be written to the file.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If titles are being generated and the line count is zero then
*       Increment the page number.
*       Output the title.
*       Reset the line number to the number of lines in the title.
*     end if
*     Attempt to output the line.
*     Increment the line number.
*     If titles are being generated and the line number is greater than
*     the number of lines per page then
*       Reset the line number.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     1/6/94  (ACD): Original version.
*     30/9/94 (ACD): First stable version.
*     6/11/97 (ACD): Prevented long titles corrupting the page count.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! Stargaze constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  FUNIT
      CHARACTER
     :  TEXT*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LOOP,     ! Loop index.
     :  LSTAT,    ! Local Fortran I/O status.
     :  PSTART,   ! Starting position for the page number.
     :  LENGTH    ! Length of character string (excl. trail. blanks).
      CHARACTER
     :  FORMFD*1, ! Form feed character.
     :  PAGE*8,   ! Buffer to hold the page number.
     :  BUFFER*(SGZ__SZOLN)  ! Buffer for first line of the title.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       If titles are being generated and the line count is zero then
*       throw a page: Increment the page number, output the title and reset
*       the line number to the number of lines in the title.
*
*       Note that ASCII character 12 is a form feed.  embedding a form
*       feed in a file causes a page throw when the file is printed.

         IF (FTABL__SGZ  .EQ.  SGZ__FFULL  .AND.  FLNCT__SGZ .EQ. 0)
     :     THEN
            FPGCT__SGZ = FPGCT__SGZ + 1

            FORMFD = CHAR(12)

            PAGE = ' '
            WRITE(PAGE(1 : 8), 2000, IOSTAT=LSTAT) FPGCT__SGZ
 2000       FORMAT('Page', I4)
            CALL FIO_SERR (LSTAT, STATUS)

            BUFFER = FTITL__SGZ(1)

            PSTART = FWID__SGZ - 10
            PSTART = MAX(PSTART, 1)

            BUFFER(PSTART : SGZ__SZOLN) = ' '

            CALL CHR_PUTC (PAGE, BUFFER, PSTART)

            LENGTH = CHR_LEN(BUFFER)
            WRITE(FUNIT, 2001, IOSTAT=LSTAT) FORMFD, BUFFER(1 : LENGTH)
 2001       FORMAT(A1, 1X, A / )
            CALL FIO_SERR (LSTAT, STATUS)

            DO LOOP = 2, FNTTL__SGZ
               IF (FTITL__SGZ(LOOP) .NE. ' ') THEN
                  LENGTH = CHR_LEN(FTITL__SGZ(LOOP) )
               ELSE
                  LENGTH = 1
               END IF

               LENGTH = MIN(LENGTH, FWID__SGZ)

               WRITE(FUNIT, 2002, IOSTAT=LSTAT)
     :           FTITL__SGZ(LOOP)(1 : LENGTH)
 2002          FORMAT(1X, A)
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL FIO_SERR (LSTAT, STATUS)
               END IF
            END DO

            FLNCT__SGZ = FNTTL__SGZ + 1
         END IF

*
*       Attempt to output the line.

         IF (TEXT .NE. ' ') THEN
            LENGTH = CHR_LEN(TEXT)
         ELSE
            LENGTH = 1
         END IF

         LENGTH = MIN(LENGTH, FWID__SGZ)

         WRITE(FUNIT, 2003, IOSTAT=LSTAT) TEXT(1 : LENGTH)
 2003    FORMAT(1X, A)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL FIO_SERR (LSTAT, STATUS)
         END IF

*
*       Increment the line number.

         FLNCT__SGZ = FLNCT__SGZ + 1

*
*       If titles are being generated and more than a pageful of lines
*       have been output then reset the line number.

         IF (FTABL__SGZ  .EQ.  SGZ__FFULL  .AND.
     :     FLNCT__SGZ .GE. FPGSZ__SGZ) THEN
            FLNCT__SGZ = 0
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_GFOUT_ERR', 'CAP_GFOUT: error writing '/
     :        /'to text file.', STATUS)
         END IF

      END IF

      END
