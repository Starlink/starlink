*+  CAR_LXFMT - Determine the width of an external format.
      SUBROUTINE CAR_LXFMT (EXFMT, LXFMT, STATUS)
*    Description :
*     CALL CAR_LXFMT (EXFMT; LXFMT; STATUS)
*    Invocation :
*     Determine the width (that is the number of characters required by)
*     of a StarBase external format.
*
*     Note that this version on handles 'Fortran like' specifiers, not 
*     the angular ones.
*    Parameters :
*     EXFMT  =  CHARACTER*(*) (ENTRY)
*           A StarBase external format specifier.
*     LXFMT  =  INTEGER (EXIT)
*           The width (that is, the number of spaces required by) the
*           StarBase external format specifier.
*     STATUS =  INTEGER (UPDATE)
*           Running status
*    Method :
*     If the specifier is not blank then
*       Take a local copy of the specifier
*       Check whether the specifier contains a full stop.
*       If so then
*         Replace the full stop and all following characters with 
*         spaces.
*       end if
*       Replace any alphabetic characters with spaces.
*       Remove any leading and embedded blanks.
*       Attempt to extract an integer number from the string.
*       If this extraction failed then
*         set the length to one.
*       end if
*     else (the specifier is blank)
*       set the length to one.
*     end if
*    Deficiencies
*     Note that this version on handles 'Fortran like' specifiers, not 
*     the angular ones.
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall        (LEI::ACD)
*    History :
*     24/9/93:  Original version.                             (LEI::ACD)
*     17/10/93: First stable version.                         (LEI::ACD)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
*    Import :
      CHARACTER
     :  EXFMT*(*)
*    Import-Export :
*    Export :
      INTEGER
     :  LXFMT
*    Status :
      INTEGER
     :  STATUS         ! Running status.
*    External references :
      INTEGER CHR_LEN
      LOGICAL CHR_ISDIG
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER
     :  CXFMT*(CAT__SZEXF) ! Copy for external format specifier.
      INTEGER
     :  LCXFMT,  ! Length of CXFMT (excl. trail. blanks).
     :  LOOP,    ! Loop index.
     :  DOTPOS,  ! Position of first full stop in CXFMT.
     :  ISTAT    ! Local status.
      LOGICAL
     :  FOUND,   ! Flag: full stopp found in CXFMT?
     :  DIGIT    ! Flag: is the current character a digit?
*-

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check that the string is not blank.

         IF (EXFMT .NE. ' ') THEN

C           print3000, exfmt
C3000       format(1x, 'LXFMT on entry, exfmt: ', a / )

*
*          Take a local copy of the string (which will be modified) and
*          determine its length.

            CXFMT = EXFMT
            LCXFMT = CHR_LEN(CXFMT)

*
*          Check if this string contains a full stop.

            FOUND = .FALSE.

            DO LOOP = 1, LCXFMT
               IF (.NOT. FOUND) THEN
                  IF (CXFMT(LOOP : LOOP) .EQ. '.') THEN
                     FOUND = .TRUE.
                     DOTPOS = LOOP
                  END IF
               END IF
            END DO

*
*          If a full stop was found then replace it and all the 
*          characters which follow it with spaces.

            IF (FOUND) THEN
               DO LOOP = DOTPOS, LCXFMT
                  CXFMT(LOOP : LOOP) = ' '
               END DO
            END IF

*
*          Replace any non-numeric characters with spaces.

            DO LOOP = 1, LCXFMT
               DIGIT = CHR_ISDIG(CXFMT(LOOP : LOOP) )

               IF (.NOT. DIGIT) THEN
                  CXFMT(LOOP : LOOP) = ' '
               END IF
            END DO

*
*          Remove all the leading and embedded blanks.

            CALL CHR_RMBLK (CXFMT)

*
*          Attempt to extract an integer number from the string.

            ISTAT = SAI__OK
            CALL CHR_CTOI (CXFMT, LXFMT, ISTAT)

*
*          Set the length to one if this extraction failed.

            IF (ISTAT .NE. SAI__OK) THEN
               LXFMT = 1
            END IF

         ELSE

*
*          The external format is blank.  Set the length to 1.

            LXFMT = 1
         END IF

C           print3001, exfmt, lxfmt
C3001       format(1x, 'LXFMT on exit, exfmt, lxfmt: ', a, 2x, i4/ )

      END IF

      END
