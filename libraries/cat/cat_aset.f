      SUBROUTINE CAT_ASET (CI, ASTFRM, STATUS)
*+
*  Name:
*     CAT_ASET
*  Purpose:
*     Set details of an AST frame-set.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_ASET (CI, ASTFRM; STATUS)
*  Description:
*     Set details of the AST frame-set which is subsequently to be
*     generated for a catalogue.
*
*     The specification of the frame-set (argument ASTFRM) is of the
*     form:
*
*       domain(column 1, column2) attribute-list
*
*     The attribute-list is optional; the other items are mandatory.
*     If present, the attribute-list comprises a list of attribute
*     specifications separated by commas.  Each attribute specification
*     has the form 'name=value'.
*
*     Examples:
*
*       SKY(RA,DEC)
*       SKY(RA,DEC) EPOCH=J1998.5, EQUINOX=J2000
*
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ASTFRM  =  CHARACTER*(*) (Given)
*        Specification for the AST frame-set.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get the catalogue array element.
*     If ok then
*       If the specification is not blank then
*         Attempt to find the separator between the mandatory and optional
*         parts.
*         If found then
*           Extract the mandatory part.
*           Extract the optional part (ie. the attributes list).
*           Replace brackets and commas in the mandatory part with spaces.
*           Split the mandatory part into words.
*           If the number of words is three then
*             Set the domain and column names.
*             Use the domain name to set the sky frame flag.
*           else
*             Set the status.
*             Report an error.
*           end if
*           If ok then
*             If the attributes list is not blank then
*               Copy the list to the common block.
*               Replace commas with blanks.
*               Split the list into words.
*               For every word
*                 Replace any equals sign with a space.
*                 Split into words.
*                 If the first word is 'equinox' then
*                   Set the equinox.
*                 end if
*                 If the first word is 'epoch' then
*                   Set the epoch.
*                 end if
*                 If the first word is 'system' then
*                   Set the system.
*                 end if
*               end for
*             end if
*           end if
*         else (the separator at the end of the mandatory part is missing)
*           Set the status.
*           Report error.
*         end if
*       else (empty specification)
*         Set the status.
*         Report error.
*       end if
*     else
*       Report error; bad catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     5/11/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_AST_CMN'      ! CAT - AST common block.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  ASTFRM*(*)
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER  CHR_LEN
*  Local Constants:
      INTEGER MXWRD           ! Maximum number of words.
      PARAMETER (MXWRD = 20)
*
      INTEGER MXATT           ! Maximum number of words in attribure-value
      PARAMETER (MXATT = 3)   ! pair.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Array element corresponding to the catalogue.
     :  ENDMAN,  ! Position of bracket closing the mandatory specification.
     :  LASTFR,  ! Length of ASTFRM (excl. trail. blanks).
     :  LOOP,    ! Loop index.
     :  START(MXWRD),  ! Start position of words.
     :  STOP(MXWRD),   ! Stop     "     "    "  .
     :  LSTAT,   ! Local status.
     :  NUMWRD,  ! Number of words.
     :  LATTLS,  ! Length if ATTLST (excl. trail. blanks).
     :  CURATT   ! Current attribute.
      INTEGER
     :  EQPOS,   ! Position of any equals sign in the current attribute.
     :  ASTART(MXATT),  ! Start position of words in attribute.
     :  ASTOP(MXATT),   ! Stop     "     "    "   "      "    .
     :  AWRD     ! Number of words in attribute.
      CHARACTER
     :  MANPRT*((AST__SZSTR * 3) + 5),   ! Mandatory part of the spec.
     :  ATTLST*(AST__SZATR),             ! Attribute list.
     :  WORDS(MXWRD)*((AST__SZSTR * 2) + 1),    ! List of words.
     :  CURWRD*((AST__SZSTR * 2) + 1),          ! Current word.
     :  ATTWRD(MXATT)*((AST__SZSTR * 2) + 1),   ! List of attribute words.
     :  ATTNAM*(AST__SZSTR),    ! Name  of the current attribute.
     :  ATTVAL*(AST__SZSTR)     ! Value "   "     "        "    .
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to get the catalogue array element and proceed if ok.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check that the frame specification given is not completely
*          blank.

            IF (ASTFRM .NE. ' ') THEN

*
*             Attempt to find the separator (a closing bracket) between the
*             mandatory and optional parts and proceed if it is found.

               ENDMAN = INDEX(ASTFRM, ')')


               IF (ENDMAN .GT. 0) THEN

*
*                Extract the mandatory part and, if it is not blank,
*                the optional list of attributes.

                  MANPRT = ' '
                  MANPRT = ASTFRM(1 : ENDMAN)

                  ATTLST = ' '

                  LASTFR = CHR_LEN(ASTFRM)

                  IF (LASTFR .GT. ENDMAN) THEN
                     ATTLST = ASTFRM(ENDMAN+1 : LASTFR)
                  END IF

*
*                Replace brackets and commas in the mandatory part with
*                spaces.

                  DO LOOP = 1, ENDMAN
                     IF (MANPRT(LOOP : LOOP) .EQ. '('  .OR.
     :                   MANPRT(LOOP : LOOP) .EQ. ')'  .OR.
     :                   MANPRT(LOOP : LOOP) .EQ. ',') THEN
                        MANPRT(LOOP : LOOP) = ' '
                     END IF
                  END DO

*
*                Split the mandatory part into words.

                  CALL CHR_DCWRD (MANPRT, MXWRD, NUMWRD, START, STOP,
     :              WORDS, LSTAT)

*
*                If the number of words is three then extract the domain
*                and column names and use the domain name to set the sky
*                frame flag.  Otherwise set the status and report an
*                error.

                  IF (NUMWRD .EQ. 3) THEN
                     CALL CHR_UCASE (WORDS(1))

                     DOMAN__AST(CIELM) = WORDS(1)

                     COLS__AST(CIELM, 1) = WORDS(2)
                     COLS__AST(CIELM, 2) = WORDS(3)

                     IF (DOMAN__AST(CIELM) .EQ. 'SKY') THEN
                        SKYFR__AST(CIELM) = .TRUE.
                     ELSE
                        SKYFR__AST(CIELM) = .FALSE.
                     END IF

                  ELSE
                     STATUS = CAT__INVAP

                     CALL MSG_SETC ('ASTFRM', ASTFRM)
                     CALL ERR_REP ('CAT_ASET_WRN', 'Invalid AST '/
     :                 /'frame-set specification: ^ASTFRM', STATUS)

                  END IF

*
*                Proceed if all is ok.

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Check whether the optional list of attributes is blank.

                     IF (ATTLST .NE. ' ') THEN

*
*                      Copy the list to the common block.

                        ATTRB__AST(CIELM) = ATTLST

*
*                      Replace commas with blanks.

                        LATTLS = CHR_LEN(ATTLST)

                        DO LOOP = 1, LATTLS
                           IF (ATTLST(LOOP : LOOP) .EQ. ',') THEN
                              ATTLST(LOOP : LOOP) = ' '
                           END IF
                        END DO

*
*                      Split the list into words.

                        CALL CHR_DCWRD (ATTLST, MXWRD, NUMWRD, START,
     :                    STOP, WORDS, LSTAT)

*
*                      Process each word in the list and decompose it
*                      into an 'attribute name, value' pair.

                        DO CURATT = 1, NUMWRD

*
*                         Convert to upper case.

                           CURWRD = WORDS(CURATT)
                           CALL CHR_UCASE (CURWRD)

*
*                         Replace any equals sign with a space.

                           EQPOS = INDEX(CURWRD, '=')

                           IF (EQPOS .GT. 0) THEN
                              CURWRD(EQPOS : EQPOS) = ' '
                           END IF

*
*                         Split the attribute into attribute name and
*                         value.

                           CALL CHR_DCWRD (CURWRD, MXATT, AWRD,
     :                       ASTART, ASTOP, ATTWRD, LSTAT)

                           IF (AWRD .GE. 2) THEN
                              ATTNAM = ATTWRD(1)
                              ATTVAL = ATTWRD(2)

*
*                            Check for the equinox, epoch and system.

                              IF (ATTNAM .EQ. 'EQUINOX') THEN
                                 EQUIN__AST(CIELM) = ATTVAL
                              ELSE IF (ATTNAM .EQ. 'EPOCH') THEN
                                 EPOCH__AST(CIELM) = ATTVAL
                              ELSE IF (ATTNAM .EQ. 'SYSTEM') THEN
                                 SYS__AST(CIELM) = ATTVAL
                              END IF
                           END IF
                        END DO

C                       print4000, EQUIN__AST(CIELM),
C    :                    EPOCH__AST(CIELM), SYS__AST(CIELM)
C4000                   format(1x, 'equin, epoch, sys : ', a15,
C    :                    a15, a15)

                     END IF
                  END IF

               ELSE

*
*                The separator at the end of the mandatory part is missing.
*                Set the status and report an error.

                  STATUS = CAT__INVAP

                  CALL MSG_SETC ('ASTFRM', ASTFRM)
                  CALL ERR_REP ('CAT_ASET_SEP', 'Invalid AST '/
     :              /'frame-set specification: ^ASTFRM', STATUS)

               END IF

            ELSE

*
*             The frame set specified was completely blank.
*             Set the status and report an error.

               STATUS = CAT__INVAP

               CALL ERR_REP ('CAT_ASET_BLK', 'The AST frame-set '/
     :           /'specified was completely empty.', STATUS)

            END IF

         ELSE

*
*          Unable to get an array element for the catalogue.

            CALL ERR_REP ('CAT_ASET_CAT', 'Attempt to set an AST '/
     :        /'frame-set for an invalid catalogue.', STATUS)

         END IF

      END IF

      END
