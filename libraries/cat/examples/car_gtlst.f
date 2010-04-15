*+  CAR_GTLST - List values of named columns for objects in catalogue.
      SUBROUTINE CAR_GTLST (CI, HEADER, NUMBER, NUMLEN, BUFLEN,
     :  NUMFLD, NAMEA, FIA, UNITS, MODE, FLUNIT, CWIDTH, SPACE,
     :  STATUS)
*    Description :
*     Lists the values of named columns for all the objects in
*     a StarBase catalogue. The output is sent either to a user-defined
*     formatted file or to the environment, or both.
*    Invocation :
*     CALL CAR_GTLST (CI, HEADER, NUMBER, NUMLEN, BUFLEN, NUMFLD,
*             NAMEA, FIA, UNITS, MODE, FILOUT, FLUNIT, CWIDTH,
*             SPACE, STATUS)
*    Parameters :
*     CI               =  INTEGER (READ)
*           input catalogue identifier
*     HEADER           =  LOGICAL (READ)
*           Header for output?
*     NUMBER           =  LOGICAL (READ)
*           Number output records?
*     NUMLEN           =  INTEGER (READ)
*           Length of string containing number
*     BUFLEN           =  INTEGER (READ)
*           Length of output buffer
*     NUMFLD           =  INTEGER (READ)
*           Number of columns requested
*     NAMEA (NUMFLD)    =  CHARACTER *(*) (READ)
*           Field names
*     FIA (NUMFLD)      =  INTEGER (READ)
*           Column identifiers
*     UNITS (NUMFLD)    CHARACTER (READ)
*           Field units
*     MODE             =  CHARACTER *(*) (READ)
*           Output on SCREEN, FILE, or BOTH
*     FLUNIT           =  INTEGER (READ)
*           Unit number for output
*     CWIDTH (NUMFLD)  =  INTEGER (READ)
*           Column width for output
*     SPACE            =  INTEGER (READ)
*           Column spacing
*     STATUS           =  INTEGER (UPDATE)
*           Running status
*    Method :
*     If (status ok) then
*       if (output involves screen display or requires header) then
*         if (output is to both screen and file .and. a header is
*          not wanted) then
*           reset output mode to screen only
*         end if
*         obtain the name of the catalogue
*         assemble the title
*         output the title to file and/or environment
*         assemble list of field names
*         output the list to file and/or environment
*         assemble list of units
*         output the list to file and/or environment
*       end if
*       Do while (status ok)
*         attempt to read record
*         if (ok) then
*           initialise buffer
*           if (number) then
*             update number
*             add number to buffer
*           end if
*           for all fields
*             attempt to obtain value
*             add value to output buffer
*           end for
*           output the buffer to file and/or environment
*         end if
*       end do
*     end if
*    Deficiencies
*    Bugs :
*     None known.
*    Authors :
*     S K Leggett          (ROE::SKL)
*     A C Davenhall        (LEI::ACD)
*    History :
*     30/3/87:  Original version.                        (ROE::SKL)
*     24/5/89:  1.2 Mbyte of local data used so reduce!  (STADAT::JHF)
*     23/9/93:  Converted to StarBase.                   (LEI::ACD)
*     10/2/94:  First stable StarBase version.           (LEI::ACD)
*     16/3/94:  Fixed bug in assembling centred title    (LEI::ACD)
*               (which was only apparent on Unix).
*     16/12/96: Removed non-standard features revealed   (ROE::ACD)
*               by the port to Linux (based on
*               modifications by BKM).
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
      INCLUDE 'CAT_ERR'
*    Import :
      INTEGER
     :  NUMFLD,             ! Number of fields
     :  CI,                 ! Identifier for input StarBase catalogue.
     :  NUMLEN,             ! Length of string containing number
     :  BUFLEN,             ! Length of output buffer
     :  FIA(NUMFLD),        ! Identifiers for required columns
     :  FLUNIT,             ! Unit number for output file
     :  CWIDTH(NUMFLD),     ! Column width
     :  SPACE               ! Column spacing
      CHARACTER
     :  NAMEA(NUMFLD)*(*),  ! Names of the required fields
     :  UNITS(NUMFLD)*(*),  ! Units of the required fields
     :  MODE*(*)            ! Screen, file or both?
      LOGICAL
     :  HEADER,             ! Header required?
     :  NUMBER              ! Numbering required?
*    Import-Export :
*    Export :
*    Status :
      INTEGER
     :  STATUS         ! Running status.
*    External references :
      INTEGER
     :  CHR_LEN        ! Length of character string
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER
     :  CATIN*(CAT__SZCNM),      ! Name of input catalogue.
     :  OMODE*(10),              ! Output mode for header.
     :  TITLE*(132),             ! String containing title.
     :  CTITLE*(132),            ! String for centred title.
     :  BUFFER*(132),            ! String containing list of values.
     :  VALUE*80,                ! Character value of field.
     :  RTFMT*80                 ! Run-time format string.
      LOGICAL
     :  NULFLG         ! Flag; null value encountered?
      INTEGER
     :  POS,           ! Position in character string
     :  PAD,           ! Spacing for centring title
     :  NAMLEN,        ! Length of catalogue name
     :  LCTITL,        ! Length of CTITLE (excl. trail. blanks)
     :  CURFLD,        ! Current field number
     :  LENNAM,        ! Length of field name character string
     :  LENUNT,        ! Length of units character string
     :  LENVAL,        ! Length of value character string
     :  NUMREC         ! Record number
*    Internal References :
*    Local data :
*-

      IF (STATUS .EQ. SAI__OK) THEN

C        print4000, numfld
C4000    format(1x, 'numfld: ', i10 / )

C        do curfld = 1, numfld
C           print4001, curfld, namea(curfld)(1 : 10), fia(curfld),
C    :        units(curfld)(1 : 10), cwidth(curfld)
C4001       format(1x, 'curfld, namea, fia, units, cwidth: ',
C    :        i3, 2x, a10, 2x, i5, 2x, a10, 2x, i4)
C        end do


*       A header is required if output is going to the environment,
*       or if the header flag has been set
          IF (MODE .EQ. 'SCREEN' .OR. MODE .EQ. 'BOTH'
     :       .OR. HEADER) THEN

*           If the mode is both, but a header is only required for
*           the environment, reset the mode for header output
              IF (MODE .EQ. 'BOTH' .AND. .NOT. HEADER) THEN
                  OMODE = 'SCREEN'
              ELSE
                  OMODE = MODE
              END IF

*           obtain the name of the catalogue
              CALL CAT_TIQAC (CI, 'NAME', CATIN, STATUS)

              IF (CATIN .NE. ' ') THEN
*               assemble the title
                  TITLE = ' '
                  POS   = 0
                  CALL CHR_PUTC('List of named fields '/
     :              /'for catalogue ', TITLE, POS)
                  NAMLEN = CHR_LEN (CATIN)
                  CALL CHR_PUTC (CATIN(1:NAMLEN), TITLE,
     :            POS)
                  CALL CHR_PUTC ('.', TITLE, POS)
*               centre title
                  PAD = (BUFLEN - POS)/2
                  CTITLE = ' '
                  LCTITL = PAD
                  CALL CHR_PUTC (TITLE(1 : POS), CTITLE, LCTITL)

*               output the title to file and/or environment
                  CALL CAR_PTLST (CTITLE(1 : LCTITL), OMODE,
     :              FLUNIT, STATUS)
              END IF

*           output blank line between title and field names
              BUFFER = ' '
              CALL CAR_PTLST (BUFFER, OMODE, FLUNIT, STATUS)

*           assemble list of field names, right hand justify
              POS = -1 * SPACE
              IF (NUMBER) THEN
                  POS = NUMLEN - 4
                  CALL CHR_PUTC ('NUMBER', BUFFER, POS)
              END IF

              DO CURFLD = 1, NUMFLD
                  LENNAM = CHR_LEN(NAMEA(CURFLD))
                  POS = POS + CWIDTH(CURFLD) + SPACE - LENNAM
                  CALL CHR_PUTC (NAMEA(CURFLD)(1:LENNAM), BUFFER, POS)
              END DO

*           output the list to file and/or environment
              CALL CAR_PTLST (BUFFER(1:POS), OMODE, FLUNIT,
     :          STATUS)

*           assemble list of units, right hand justify
              BUFFER = ' '
              POS = -1 * SPACE
              IF (NUMBER) THEN
                  POS = NUMLEN + 2
              END IF
              DO CURFLD = 1, NUMFLD
                  IF (UNITS(CURFLD) .NE. ' ') THEN
                      LENUNT = CHR_LEN(UNITS(CURFLD))
                  ELSE
                      LENUNT = 0
                  END IF
                  POS = POS + CWIDTH(CURFLD) + SPACE - LENUNT
                  IF (UNITS(CURFLD) .NE. ' ') THEN
                      CALL CHR_PUTC (UNITS(CURFLD)(1:LENUNT), BUFFER,
     :                  POS)
                  END IF
              END DO

*           output the list to file and/or environment
              CALL CAR_PTLST (BUFFER(1:POS), OMODE, FLUNIT,
     :          STATUS)

          END IF


          NUMREC = 0
          DO WHILE (STATUS .EQ. SAI__OK)
*           Attempt to read the next record
              NUMREC = NUMREC + 1
              CALL CAT_RGET (CI, NUMREC, STATUS)

C             print4002, ci, numrec, status
C4002         format(1x, 'rget - ci, numrec, status:', i10, i10, i10)

              IF (STATUS .EQ. SAI__OK) THEN
                  BUFFER = ' '
                  POS = -1 * SPACE

                  IF (NUMBER) THEN
                      WRITE (RTFMT, '(''(I'', I4, '',1X,A1)'')' )
     :                  NUMLEN
                      WRITE (BUFFER, RTFMT) NUMREC, ':'

                      POS = NUMLEN + 2
                  END IF

                  DO CURFLD = 1, NUMFLD
*                   Obtain the value for the field.
                      VALUE = ' '
                      CALL CAT_EGT0F (FIA(CURFLD),
     :                  VALUE(1 : CWIDTH(CURFLD) ), NULFLG, STATUS)

C                     print4003, curfld, fia(curfld), value(1 : 10),
C    :                  status
C4003                 format(1x, 'curfld, fia, value, status: ',
C    :                  i10, i10, 2x, a10, 2x, i10)

                      IF (VALUE .NE. ' ') THEN
                          LENVAL = CHR_LEN(VALUE)
                      ELSE
                          LENVAL = 1
                      END IF

*                   add value to output buffer
                      POS = POS + CWIDTH(CURFLD) + SPACE - LENVAL
                      CALL CHR_PUTC (VALUE(1:LENVAL), BUFFER,
     :                  POS)

                  END DO
*               output the buffer to file and/or environment
                  CALL CAR_PTLST (BUFFER(1:POS), MODE, FLUNIT,
     :              STATUS)

              END IF
          END DO

          IF (STATUS .EQ. CAT__EOF  .OR.  STATUS .EQ. CAT__INVRW)
     :       THEN
              STATUS = SAI__OK
          END IF

      END IF

      END

