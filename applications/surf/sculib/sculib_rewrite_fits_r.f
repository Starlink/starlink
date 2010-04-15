      SUBROUTINE SCULIB_REWRITE_FITS_R (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, STATUS)
*+
*  Name:
*     SCULIB_REWRITE_FITS_R

*  Purpose:
*     rewrite the value of specified FITS real keyword

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_REWRITE_FITS_R (MAX_FITS, N_FITS, FITS, NAME,
*    :  VALUE, STATUS)


*  Description:
*     This routine will overwrite the value of a specified FITS real
*     keyword held in the FITS extension of an NDF file. The FITS extension
*     must have been read into the input array FITS before this routine is
*     called and be written out again afterwards for the change to take effect.
*
*        The routine assumes that each line in the FITS array will contain
*     a string with format:-
*
*      "KEYWORD=VALUE           / this is a comment"
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
*           the name of the FITS keyword whose value is to be rewritten
*     VALUE                = REAL (Given)
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
*     $Log$
*     Revision 1.7  1999/08/19 03:37:23  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.6  1999/08/06 02:24:48  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.5  1999/08/03 19:35:26  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1997/11/19 02:32:05  timj
*     Fix bug in length of string for CHR_TERM
*
*     Revision 1.3  1997/11/19 02:26:45  timj
*     Make sure there is a space between the equals and the number.
*
*     Revision 1.2  1997/11/19 01:23:41  timj
*     Update header
*
*     26 Jul 1995 (JFL)
*        original version.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MAX_FITS
      INTEGER N_FITS
      CHARACTER*(*) NAME
      REAL    VALUE

*  Arguments Given & Returned:
      CHARACTER*(*) FITS (MAX_FITS)

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER      CHR_LEN             ! CHR used-string length function

*  Global variables:

*  Local Constants:

*  Local variables :
      LOGICAL      FOUND               ! .TRUE. if keywrod found
      INTEGER      I                   ! DO loop index
      INTEGER      IPOS                ! position of = in string
      INTEGER      ITEMP               ! scratch integer
      INTEGER      JPOS                ! position of / in string
      CHARACTER*80 KEYNAME             ! name of FITS keyword in string
      LOGICAL      LOOPING             ! .TRUE. while looping
      CHARACTER*80 STEMP               ! scratch string
      CHARACTER*80 UNAME               ! upper case version of NAME

*  Internal References:

*  Local data:

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

*  reset the value, padding out the string with blanks

                        CALL CHR_RTOC (VALUE, STEMP, ITEMP)
                        CALL CHR_TERM (JPOS-IPOS-1, STEMP)
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
            CALL ERR_REP (' ', 'SCULIB_REWRITE_FITS_R: error '//
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
            CALL ERR_REP (' ', 'SCULIB_REWRITE_FITS_R: failed to '//
     :        'find FITS item ^NAME', STATUS)
         END IF
      END IF

      END
