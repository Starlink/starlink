*+  SCULIB_PUT_FITS_D - write a DOUBLE to a FITS item specified
      SUBROUTINE SCULIB_PUT_FITS_D (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, COMMENT, STATUS)
*    Description :
*     This routine  writes a FITS double precision item into a character array
*     ready to be written out to the .MORE.FITS structure in an NDF file.
*     An error will be reported and bad status returned if the name of the
*     FITS item is blank or more than 8 characters long. An error will also
*     occur if the FITS character array is full.
*    Invocation :
*     CALL SCULIB_PUT_FITS_D (MAX_FITS, N_FITS, FITS, NAME, VALUE,
*    :  COMMENT, STATUS)
*    Parameters :
*     MAX_FITS                       = INTEGER (Given)
*           the size of the FITS array
*     N_FITS                         = INTEGER (Given and returned)
*           the number of FITS items currently in the FITS array
*     FITS (MAX_FITS)                = CHARACTER*80 (Given and returned)
*           array containing the FITS items
*     NAME                           = CHARACTER*(*) (Given)
*           the name of the FITS item to be written
*     VALUE                          = DOUBLE PRECISION (Given)
*           the value to be assigned to the item
*     COMMENT                        = CHARACTER*(*) (Given)
*           a comment applying to the FITS item
*     STATUS                         = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)
*    History :
*     $Id$
*     15-AUG-1995: original version
*     18-DEC-1998 (TIMJ)
*       Convert n.nnDee to n.nnEee (eg 1.5D-6 to 1.5E-6)
*       This is because the AST FITS reader can not understand
*       numbers containing D)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER MAX_FITS
      CHARACTER*(*) NAME
      DOUBLE PRECISION VALUE
      CHARACTER*(*) COMMENT
*    Import-Export :
      INTEGER N_FITS
      CHARACTER*(80) FITS (MAX_FITS)
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN                      ! CHR used string length function
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER CPTR                         ! index of start of comment
      INTEGER NCHAR                        ! length of encoded number
      INTEGER IPOSN                        ! Position in string
      CHARACTER*32 STEMP                   ! scratch string
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      N_FITS = N_FITS + 1

      IF (N_FITS .LT. 1) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_D: bad input value of '//
     :     'N_FITS', STATUS)
      ELSE IF (N_FITS .GT. MAX_FITS) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_D: size of FITS array '//
     :     'has been exceeded', STATUS)
      ELSE IF (CHR_LEN(NAME) .GT. 8) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('NAME', NAME)
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_D: invalid name for '//
     :     'FITS item - ^NAME', STATUS)
      ELSE IF (CHR_LEN(NAME) .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_D: blank name given '//
     :     'for FITS item', STATUS)
      ELSE

         FITS (N_FITS) = NAME
         FITS (N_FITS)(9:9) = '='
         CALL CHR_DTOC (VALUE, STEMP, NCHAR)

*     Check for a 'D' in STEMP (since some FITS readers (eg AST)
*     do not understand 1.5D-6 and want 1.5E-6)
         IPOSN = NCHAR
         CALL CHR_FIND(STEMP, 'D', .FALSE., IPOSN)
         IF (IPOSN .NE. 0) STEMP(IPOSN:IPOSN) = 'E'

*     Now construct the FITS card

         IF (NCHAR .LE. 20) THEN
            FITS (N_FITS)(31-NCHAR:30) = STEMP
            CPTR = 32
         ELSE
            FITS (N_FITS)(11:10+NCHAR) = STEMP
            CPTR = 12 + NCHAR
         END IF

         FITS (N_FITS)(CPTR:CPTR) = '/'
         FITS (N_FITS)(CPTR+2:) = COMMENT
      END IF

      END
