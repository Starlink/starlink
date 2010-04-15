**==outfil.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996
C
C
C

************************************************************************

      SUBROUTINE OUTFIL(IFILE,FILE,ISTAT)

*+
*  Name :
*     OUTFIL
*
*  Purpose :
*     Open a sequential disk data file for writing.
*
*  Invocation :
*     CALL OUTFIL( IFILE, FILE, ISTAT )
*
*  Description :
*     UNIX FORTRAN-specific subroutine to open a sequential disk data
*     file for writing.
*
*  Arguments :
*     IFILE = INTEGER (Given)
*        The logical unit number to be used
*     FILE = CHARACTER*(30) (Given)
*        The filename
*     ISTAT = INTEGER (Returned)
*        The Daophot error flag.  A zero value is returned if there
*        is no error.
*
*  Deficiencies :
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*      6-DEC-1991 (NE):
*        Original version derived from DAOPHOT Classic routine.
*     19-FEB-1992 (NE):
*        Corrected argument list by adding status argument.
*        Unix does not allow multiple versions of a file.
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER IFILE
      CHARACTER*(30) FILE

*  Status :
      INTEGER ISTAT

*  Local Variables :
      LOGICAL EXIST

      CHARACTER*30 ANSWER
*.

*   See if the file already exists
 100  CONTINUE
      INQUIRE (FILE=FILE,EXIST=EXIST)

*   If the file exists ask for another name ( default OVERWRITE )
      IF ( EXIST ) THEN
         CALL STUPID('This file already exists: ' // FILE)
         ANSWER = 'OVERWRITE'
         CALL GETNAM('New output file name:',ANSWER)

*   Delete the existing file if OVERWRITE is chosen
         IF ( ANSWER.EQ.'OVERWRITE' ) THEN
            OPEN (IFILE,FILE=FILE,STATUS='OLD')
            CLOSE (IFILE,STATUS='DELETE')

*   Otherwise use the one given
         ELSE
            FILE = ANSWER
            GO TO 100
         END IF
      END IF

*   Open the file
      OPEN (IFILE,FILE=FILE,STATUS='NEW',IOSTAT=ISTAT)

      END
