      SUBROUTINE SCULIB_REWRITE_FITS_C (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, STATUS)
*+
*  Name:
*     SCULIB_REWRITE_FITS_C

*  Purpose:
*     rewrite the value of specified FITS character keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_REWRITE_FITS_C (MAX_FITS, N_FITS, FITS, NAME,
*    :  VALUE, STATUS)

*  Description:
*     This routine will overwrite the value of a specified FITS character
*     keyword held in the FITS extension of an NDF file. The FITS extension
*     must have been read into the input array FITS before this routine is
*     called and be written out again afterwards for the change to take effect.
*
*        The routine assumes that each line in the FITS array will contain
*     a string with format:-
*
*      "KEYWORD='VALUE'           / this is a comment"
*
*     It will search the input array for a line containing the required
*     keyword and replace VALUE. If the keyword is not found an error will
*     be reported and bad status returned. If the keyword is found but the
*     line does not conform to the above format an error will be reported
*     and bad status returned.

*  Arguments:
*     MAX_FITS             = INTEGER (Given)
*           the maximum number of items in the FITS array
*     N_FITS               = INTEGER (Given)
*           the actual number of items in the FITS array
*     FITS (MAX_FITS)      = CHARACTER*(*) (Given and returned)
*           array containing the FITS items
*     NAME                 = CHARACTER*(*) (Given)
*           the name of the FITS keyword whose value is to be changed
*     VALUE                = CHARACTER*(*) (Given)
*           the value that the FITS keyword is to have
*     STATUS               = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T. Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     26-JUL-1995 (jfl):
*       original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MAX_FITS
      INTEGER N_FITS
      CHARACTER*(*) NAME
      CHARACTER*(*) VALUE

*  Arguments Given & Returned:
      CHARACTER*(*) FITS (MAX_FITS)

*    Status :
      INTEGER STATUS

*    External references:
      INTEGER      CHR_LEN             ! CHR used-string length function

*    Local variables:
      LOGICAL      FOUND               ! .TRUE. if keywrod found
      INTEGER      I                   ! DO loop index
      INTEGER      IPOS                ! position of = in string
      INTEGER      JPOS                ! position of / in string
      CHARACTER*80 KEYNAME             ! name of FITS keyword in string
      LOGICAL      LOOPING             ! .TRUE. while looping
      CHARACTER*80 STEMP               ! scratch string
      CHARACTER*80 UNAME               ! upper case version of NAME

*.

      IF (STATUS .NE. SAI__OK) RETURN

      UNAME = NAME
      CALL CHR_UCASE (UNAME)
      FOUND = .FALSE.

*  loop through the FITS array

      IF (N_FITS .GT. 0) THEN
         I = 0
         LOOPING = .TRUE.

         DO WHILE (LOOPING)
            I = I + 1

*  the FITS keyword name should be in first part of the string, immediately
*  in front of the =

            IPOS = INDEX (FITS(I),'=')
            IF (IPOS .GT. 1) THEN
               KEYNAME = FITS(I) (1:IPOS-1)
               CALL CHR_UCASE (KEYNAME)
               CALL CHR_RMBLK (KEYNAME)

               IF (CHR_LEN(KEYNAME) .EQ. 0) THEN
                  STATUS = SAI__ERROR
               ELSE
                  IF (KEYNAME(:CHR_LEN(KEYNAME)) .EQ.
     :                UNAME(:CHR_LEN(UNAME)))    THEN

*  OK, we've found the FITS keyword, the value should be between the = and
*  a /
                     FOUND = .TRUE.
                     JPOS = INDEX (FITS(I),'/')

                     IF ((JPOS .EQ. 0)       .OR.
     :                   (JPOS .EQ. IPOS+1)) THEN
                        STATUS = SAI__ERROR
                     ELSE

*  reset the value, enclosing the string in quotes, truncating it or padding
*  it out with blanks as necessary

                        IF (CHR_LEN(VALUE) .LE. JPOS-IPOS-4) THEN
                           IF (CHR_LEN(VALUE) .NE. 0) THEN
                              STEMP = ''''//VALUE(:CHR_LEN(VALUE))//''''
                           ELSE
                              STEMP = ''' '''
                           END IF
                           CALL CHR_TERM (JPOS-IPOS-1, STEMP)
                        ELSE
                           STEMP = ''''//VALUE(:JPOS-IPOS-4)//''''
                        END IF
                        FITS (I)(IPOS+2:JPOS-1) = STEMP (:JPOS-IPOS-2)
                     END IF
                  END IF
               END IF
            END IF

*  break out of loop if we've found the keyword, reached the end of the FITS
*  array, or if an error has occured

            IF (FOUND                 .OR.
     :          (STATUS .NE. SAI__OK) .OR.
     :          (I .EQ. N_FITS))      THEN
               LOOPING = .FALSE.
            END IF

         END DO

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ', 'SCULIB_REWRITE_FITS_C: error '//
     :        'rewriting -', STATUS)
            CALL MSG_SETC ('LINE', FITS(I))
            CALL ERR_REP (' ', '^LINE', STATUS)
         END IF
      END IF

*  check that the keyword was found

      IF (.NOT. FOUND) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('NAME', NAME)
            CALL ERR_REP (' ', 'SCULIB_REWRITE_FITS_C: failed to '//
     :        'find FITS item ^NAME', STATUS)
         END IF
      END IF

      END
