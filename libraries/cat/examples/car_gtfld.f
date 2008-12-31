*+  CAR_GTFLD - Get names and details for the columns to be listed.
      SUBROUTINE CAR_GTFLD (CI, MXFLD, NUMFLD, NAMEA, FIA, UNITS,
     :  CWIDTH, STATUS)
*    Description :
*     Get the names and various other details for the columns which are
*     to be listed.
*    Invocation : 
*     CAR_GTFLD (CI, MXFLD; NUMFLD, NAMEA, FIA, UNITS, CWIDTH; STATUS)
*    Parameters :
*     CI  =  INTEGER (ENTRY)
*           Catalogue identifer.
*     MXFLD  =  INTEGER (ENTRY)
*           Maximum permitted number of columns(the size of the returned
*           arrays).
*     NUMFLD  =  INTEGER (EXIT)
*           The number of columns chosen.
*     NAMEA(MXFLD)  =  CHARACTER*(*) (EXIT)
*           The names of the chosen columns.
*     FIA(MXFLD)  =  INTEGER (EXIT)
*           Identifiers for the chosen columns.
*     UNITS(MXFLD)  =  CHARACTER*(*) (EXIT)
*           The units of the chosen columns.
*     CWIDTH(MXFLD)  =  INTEGER (EXIT)
*           The width (number of characters) to be used to display
*           each chosen column.  It is derived from the external format
*           for the column.
*     STATUS =  INTEGER (UPDATE)
*           Running status
*    Method :
*     Obtain the names and comments of all the columns in the catalogue.
*     Append the 'termination' entry to this list of columns.
*     Do while (more columns are to be chosen)
*       Prompt for a new reply.
*       If the reply is equal to 'EXIT' then
*         Set the termination flag.
*       else if the reply is equal to 'HELP' then
*         List the columns available.
*       else
*         If the reply is non-blank  (the reply is a column name) then
*           Attempt to get an identifier for the column.
*           If ok then
*             Increment the number of columns.
*             Save the column name.
*             Save the column identifier.
*             Get the units for the column.
*             Get the external format for the column.
*             Derive the column width from the external format.
*           else
*             Display message - invalid column name.
*             Reset the status.
*           end if
*         end if
*       end if
*       If the maximum number of columns have been selected then
*         Set the termination flag.
*       end if
*       If the status is not ok then
*         Set the termination flag.
*       end if
*     end do
*    Deficiencies
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall        (LEI::ACD)
*    History :
*     24/9/93:  Original version.                             (LEI::ACD)
*     17/10/93: First working version.                        (LEI::ACD)
*     9/2/94:   Re-written to handle vector columns.          (LEI::ACD)
*     21/2/94:  Removed unused variable.                      (LEI::ACD)
*     19/4/95:  Changed parametric constants to correspond to (LEI::ACD)
*               changes to CAT_PAR.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
*    Import :
      INTEGER
     :  CI,
     :  MXFLD
*    Import-Export :
*    Export :
      INTEGER
     :  NUMFLD,
     :  FIA(MXFLD),
     :  CWIDTH(MXFLD)
      CHARACTER
     :  NAMEA(MXFLD)*(*),
     :  UNITS(MXFLD)*(*)
*    Status :
      INTEGER
     :  STATUS         ! Running status.
*    External references :
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR
*    Global variables :
*    Local Constants :
*    Local variables :

*
*    Note that the '+1' in the size of the following arrays is to allow
*    for the extra entry in the list of columns which corresponds to
*    choosing to terminate the selection.

      CHARACTER
     :  REPLY*(CAT__SZCMP),   ! Current reply.
     :  FNAME*(CAT__SZCMP+7), ! Name of current column.
     :  FNAMES(CAT__MXCOL+1)*(CAT__SZCMP+7), ! Names of colums in cat.
     :  FCOMMS(CAT__MXCOL+1)*50,             ! Comments for columns.
     :  EXFMT*(CAT__SZEXF)    ! External format for current column.
      INTEGER
     :  SIZE,   ! Number of elements in current (vector) column.
     :  LFNAME, ! Length of FNAME (excl. trail. blanks).
     :  FI,     ! Identifier for current column.
     :  FIB,    ! Identifier for base element of vector column.
     :  IDTYPE, ! Type of identifier.
     :  FCOUNT, ! Running count for current column.
     :  LNAME,  ! Length of current column name   (excl. trail. blanks).
     :  LUNITS, !   "    "     "      "    units  ( "  .   "  .   "   ).
     :  LEXFMT  !   "    "     "      "    format ( "  .   "  .   "   ).
      LOGICAL
     :  MORCOL, ! Flag: more columns to be obtaine from the cat?
     :  VECTR,  ! Flag: are there any vector columns in the list?
     :  MORCHO  ! Flag: continue choosing columns?
*-

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain names and comments for all the columns in the catalogue.

         VECTR = .FALSE.
         FCOUNT = 0
         MORCOL = .TRUE.

         DO WHILE (MORCOL)
            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CI, CAT__GPHYS, FCOUNT, FI, STATUS)

            IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN
               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               CALL CAT_TIQAI (FI, 'SIZE', SIZE, STATUS)

               IF (SIZE .GT. 1) THEN
                  VECTR = .TRUE.

                  LFNAME = CHR_LEN (FNAME)

                  CALL CHR_PUTC ('[1-', FNAME, LFNAME)
                  CALL CHR_PUTI (SIZE, FNAME, LFNAME)
                  CALL CHR_PUTC (']', FNAME, LFNAME)
               END IF

               FNAMES(FCOUNT) = FNAME

               CALL CAT_TIQAC (FI, 'COMM', FCOMMS(FCOUNT), STATUS)
            END IF

            IF (FI .EQ. CAT__NOID) THEN
               FCOUNT = FCOUNT - 1
               MORCOL = .FALSE.
            END IF

            IF (FCOUNT .EQ. CAT__MXCOL) THEN
               MORCOL = .FALSE.
            END IF

            IF (STATUS .NE. SAI__OK) THEN
               MORCOL = .FALSE.
            END IF
         END DO

*
*       Add the termination entry to the list of columns.

         FCOUNT = FCOUNT + 1
         FNAMES(FCOUNT) = 'EXIT'
         FCOMMS(FCOUNT) = 'Finish choosing columns and exit'

*
*       Proceed while the user wishes to obtain more columns.

         MORCHO = .TRUE.
         NUMFLD = 0

         DO WHILE (MORCHO)

*
*          Prompt for a new reply.

            CALL PAR_GET0C ('REPLY', REPLY, STATUS)
            CALL PAR_CANCL ('REPLY', STATUS)

*
*          Check if the reply is requesting termination.

            IF (CHR_SIMLR(REPLY, 'EXIT') ) THEN
               MORCHO = .FALSE.

*
*          Check if the reply is requesting help on the columns in the
*          catalogue.

            ELSE IF (CHR_SIMLR(REPLY, 'HELP') ) THEN
               CALL CAR_CHOI1 (FCOUNT, FCOMMS, FNAMES, .FALSE., STATUS)

               CALL MSG_OUT (' ', '***NOTE*** column names are '/
     :           /'cAsE sEnSiTiVe. ***', STATUS)

               IF (VECTR) THEN
                  CALL MSG_OUT (' ', '***NOTE*** elements of vector '/
     :              /'columns must be specified individually.', STATUS)
               END IF

*
*          Otherwise, assume that the reply is a column name.  Attempt
*          to obtain an identifier for it and proceed to get the other
*          details if ok.

            ELSE
               IF (REPLY .NE. ' ') THEN
                  CALL ERR_MARK
                  CALL CAT_TIDNT (CI, REPLY, FI, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     NUMFLD = NUMFLD + 1

                     NAMEA(NUMFLD) = REPLY
                     LNAME = CHR_LEN(NAMEA(NUMFLD) )
                     FIA(NUMFLD) = FI

*
*                   If the identifier is an array element then get the
*                   identifier for the base of the array; otherwise
*                   copy the identifier.

                     CALL CAT_TIDTP (FI, IDTYPE, STATUS)

                     IF (IDTYPE .EQ. CAT__FETYP) THEN
                        CALL CAT_TIQAI (FI, 'BASEID', FIB, STATUS)
                     ELSE
                        FIB = FI
                     END IF

                     CALL CAT_TIQAC (FIB, 'UNITS', UNITS(NUMFLD),
     :                 STATUS)
                     IF (UNITS(NUMFLD) .NE. ' ') THEN
                        LUNITS = CHR_LEN(UNITS(NUMFLD) )
                     ELSE
                        LUNITS = 1
                     END IF

                     CALL CAT_TIQAC (FIB, 'EXFMT', EXFMT, STATUS)
                     CALL CAR_LXFMT(EXFMT, LEXFMT, STATUS)

                     CWIDTH(NUMFLD) = MAX(LNAME, LUNITS, LEXFMT)

C                    print2000, numfld, namea(numfld)(1:10),
C    :                 fia(numfld),
C    :                 units(numfld)(1:10), exfmt(1:10), cwidth(numfld)
C2000                format(1x, 'numfld, namea, fia, units, exfmt, ',
C    :                 'cwidth: ',
C    :                 I3, 2X, A10, 2X, I5, 2X, A10, 2X, A10, 2X, I3 )

                  ELSE
                     CALL ERR_ANNUL (STATUS)
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', '** Invalid column name ** '/
     :                 /'(type HELP for a list of columns).', STATUS)
                     CALL ERR_FLUSH (STATUS)

                  END IF
                  CALL ERR_RLSE

               END IF
            END IF
 
*
*          Set the termination flag if the maximum number of columns
*          have been entered.

            IF (NUMFLD .GE. MXFLD) THEN
               MORCHO = .FALSE.

               CALL MSG_OUT (' ', 'Maximum number of columns chosen',
     :           STATUS)
            END IF

*
*          Set the termination flag if the status is not ok.

            IF (STATUS .NE. SAI__OK) THEN
               MORCHO = .FALSE.
            END IF
         END DO

      END IF 

      END
