      SUBROUTINE CAT1_TCNVT (ITYPE, INUB, INB, INUW, INW, INI, INK,
     :  INR, IND, INL, INC, OTYPE, OUTUB, OUTB, OUTUW, OUTW, OUTI,
     :  OUTK, OUTR, OUTD, OUTL, OUTC, CONVOK, STATUS)
*+
*  Name:
*     CAT1_TCNVT
*  Purpose:
*     Convert a value from one data type to another.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_TCNVT (ITYPE, INUB, INB, INUW, INW, INI, INK, INR, IND,
*       INL, INC, OTYPE; OUTUB, OUTB, OUTUW, OUTW, OUTI, OUTK, OUTR,
*       OUTD, OUTL, OUTC, CONVOK; STATUS)
*  Description:
*     Convert a value from one data type to another.
*
*     A failure during a type conversion is flagged using CONVOK, not
*     by setting the running status argument.
*  Arguments:
*     ITYPE  =  INTEGER (Given)
*        Numeric code defining the data type of the input value.
*     INUB  =  unsigned BYTE (Given)
*        Input value if the input data type is unsigned byte.
*     INB  =  BYTE (Given)
*        Input value if the input data type is byte.
*     INUW  =  unsigned WORD (Given)
*        Input value if the input data type is unsigned word.
*     INW  =  WORD (Given)
*        Input value if the input data type is word.
*     INI  =  INTEGER (Given)
*        Input value if the input data type is integer.
*     INK  =  INTEGER*8 (Given)
*        Input value if the input data type is integer*8.
*     INR  =  REAL (Given)
*        Input value if the input data type is real.
*     IND  =  DOUBLE PRECISION (Given)
*        Input value if the input data type is double precision.
*     INL  =  LOGICAL (Given)
*        Input value if the input data type is logical.
*     INC  =  CHARACTER*(*) (Given)
*        Input value if the input data type is character.
*     OTYPE  =  INTEGER (Given)
*        Numeric code defining the data type of the output value.
*     OUTUB  =  unsigned BYTE (Returned)
*        Output value if the output data type is unsigned byte.
*     OUTB  =  BYTE (Returned)
*        Output value if the output data type is byte.
*     OUTUW  =  unsigned WORD (Returned)
*        Output value if the output data type is unsigned word.
*     OUTW  =  WORD (Returned)
*        Output value if the output data type is word.
*     OUTI  =  INTEGER (Returned)
*        Output value if the output data type is integer.
*     OUTK  =  INTEGER*8 (Returned)
*        Output value if the output data type is integer*8.
*     OUTR  =  REAL (Returned)
*        Output value if the output data type is real.
*     OUTD  =  DOUBLE PRECISION (Returned)
*        Output value if the output data type is double precision.
*     OUTL  =  LOGICAL (Returned)
*        Output value if the output data type is logical.
*     OUTC  =  CHARACTER*(*) (Returned)
*        Output value if the output data type is character.
*     CONVOK  =  LOGICAL (Returned)
*        Flag indicating whether the conversion was successful,
*        coded as follows:
*        .TRUE.  - the conversion succeeded,
*        .FALSE. - the conversion failed.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*
*  Implementation Deficiencies:
*     The behaviour prescribed for this routine is fully described in
*     Section 4.3 of StarBase/ACD/3.3.  This implementation falls short
*     of that prescription in some respects, particularly in terms of
*     trapping failures during the type conversions.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     7/7/93  (ACD): Original version.
*     23/1/94 (ACD): Modified error reporting.
*     8/2/94  (ACD): Changed parameters for data type codes.
*     2012-05-11 (TIMJ):
*        Add K.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'   ! External CAT constants.
      INCLUDE 'CAT1_PAR'  ! Internal CAT constants.
      INCLUDE 'CAT_ERR'   ! CAT error codes.
      INCLUDE 'PRM_PAR'   ! Max. and min. values for various data types.
*  Arguments Given:
      INTEGER ITYPE
      BYTE INUB
      BYTE INB
      INTEGER*2 INUW
      INTEGER*2 INW
      INTEGER INI
      INTEGER*8 INK
      REAL INR
      DOUBLE PRECISION IND
      LOGICAL INL
      CHARACTER*(*) INC
      INTEGER OTYPE
*  Arguments Returned:
      BYTE OUTUB
      BYTE OUTB
      INTEGER*2 OUTUW
      INTEGER*2 OUTW
      INTEGER OUTI
      INTEGER*8 OUTK
      REAL OUTR
      DOUBLE PRECISION OUTD
      LOGICAL OUTL
      CHARACTER*(*) OUTC
      LOGICAL CONVOK
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  REM,    ! Remaineder after deviding by two.
     :  POSN,   ! Current position in character string.
     :  IVAL,   ! INTEGER copy of the current value.
     :  LSTAT   ! Local status.
      INTEGER*8
     :  KVAL    ! INTEGER*8 copy of the current value
*
      BYTE
     :  BMIN,   ! Minimum value as a byte number.
     :  BMAX    ! Maximum   "   "  "  "     "   .
      INTEGER*2
     :  WMIN,   ! Minimum value as a word number.
     :  WMAX    ! Maximum   "   "  "  "     "   .
      INTEGER
     :  IMIN,   ! Minimum value as an integer number.
     :  IMAX    ! Maximum   "   "  "     "      "   .
      INTEGER*8
     :  KMIN,   ! Minimum value as an integer*8 number.
     :  KMAX    ! Maximum   "   "  "     "      "   .
      REAL
     :  RMIN,   ! Minimum value as a real number.
     :  RMAX    ! Maximum   "   "  "  "     "   .
      DOUBLE PRECISION
     :  DMIN,   ! Minimum value as a double precision number.
     :  DMAX    ! Maximum   "   "  "   "        "       "   .
*.

      IF (STATUS .EQ. CAT__OK) THEN

         OUTUB = 0
         OUTB = 0
         OUTUW = 0
         OUTW = 0
         OUTI = 0
         OUTK = 0
         OUTR = 0.0E0
         OUTD = 00.D0
         OUTL = .FALSE.
         OUTC = ' '


         CONVOK = .TRUE.

*
*       Case where the input value is unsigned byte.

         IF (ITYPE .EQ. CAT__TYPEUB) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               OUTUB = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               OUTB = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               OUTUW = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               OUTW = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               OUTI = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               OUTK = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INUB

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INUB
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               IVAL = INUB

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTI (IVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is byte.

         ELSE IF (ITYPE .EQ. CAT__TYPEB) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
c               BMIN = VAL__MINUB
c               BMAX = VAL__MAXUB
               BMIN = VAL__MINUB
               BMAX = VAL__MAXB

               IF (INB .GE. BMIN .AND.  INB .LE. BMAX) THEN
                  OUTUB = INB
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               OUTB = INB

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               OUTUW = INB
               IF (INB .LT. 0) CONVOK = .FALSE.

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               OUTW = INB

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               OUTI = INB

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               OUTK = INB

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INB

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INB

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INB
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               IVAL = INB

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTI (IVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is unsigned word.

         ELSE IF (ITYPE .EQ. CAT__TYPEUW) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
c               WMIN = VAL__MINUB
c               WMAX = VAL__MAXUB
               WMIN = VAL__MINUB
               WMAX = VAL__MAXB

               IF (INUW .GE. WMIN  .AND.  INUW .LE. WMAX) THEN
                  OUTUB = INUW
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               WMIN = VAL__MINB
               WMAX = VAL__MAXB

               IF (INUW .GE. WMIN  .AND.  INUW .LE. WMAX) THEN
                  OUTB = INUW
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               OUTUW = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               OUTW = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               OUTI = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               OUTK = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INUW

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INUW
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               IVAL = INUW

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTI (IVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is word.

         ELSE IF (ITYPE .EQ. CAT__TYPEW) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
c               WMIN = VAL__MINUB
c               WMAX = VAL__MAXUB
               WMIN = VAL__MINUB
               WMAX = VAL__MAXB

               IF (INW .GE. WMIN  .AND.  INW .LE. WMAX) THEN
                  OUTUB = INW
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               WMIN = VAL__MINB
               WMAX = VAL__MAXB

               IF (INW .GE. WMIN  .AND.  INW .LE. WMAX) THEN
                  OUTB = INW
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
c               WMIN = VAL__MINUW
c               WMAX = VAL__MAXUW
               WMIN = VAL__MINUW
               WMAX = VAL__MAXW

               IF (INW .GE. WMIN  .AND.  INW .LE. WMAX) THEN
                  OUTUW = INW
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               OUTW = INW

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               OUTI = INW

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               OUTK = INW

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INW

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INW

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INW
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               IVAL = INW

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTI (IVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is integer.

         ELSE IF (ITYPE .EQ. CAT__TYPEI) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               IMIN = VAL__MINUB
               IMAX = VAL__MAXUB

               IF (INI .GE. IMIN  .AND.  INI .LE. IMAX) THEN
                  OUTUB = INI
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               IMIN = VAL__MINB
               IMAX = VAL__MAXB

               IF (INI .GE. IMIN  .AND.  INI .LE. IMAX) THEN
                  OUTB = INI
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
c               IMIN = VAL__MINUW
c               IMAX = VAL__MAXUW
               IMIN = VAL__MINUW
               IMAX = VAL__MAXW

c               write(17, 1000) imin, imax, ini
c 1000          format(1x, 'imin, imax, ini: ', i10, i10, i10 )

               IF (INI .GE. IMIN  .AND.  INI .LE. IMAX) THEN
                  OUTUW = INI
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               IMIN = VAL__MINW
               IMAX = VAL__MAXW

               IF (INI .GE. IMIN  .AND.  INI .LE. IMAX) THEN
                  OUTW = INI
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               OUTI = INI

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INI

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INI

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               REM = MOD(INI, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               IVAL = INI

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTI (IVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is integer*8.

         ELSE IF (ITYPE .EQ. CAT__TYPEK) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               KMIN = VAL__MINUB
               KMAX = VAL__MAXUB

               IF (INK .GE. KMIN  .AND.  INK .LE. KMAX) THEN
                  OUTUB = INK
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               KMIN = VAL__MINB
               KMAX = VAL__MAXB

               IF (INK .GE. KMIN  .AND.  INK .LE. KMAX) THEN
                  OUTB = INK
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
c               KMIN = VAL__MINUW
c               KMAX = VAL__MAXUW
               KMIN = VAL__MINUW
               KMAX = VAL__MAXW

c               write(17, 1000) kmin, kmax, ink
c 1000          format(1x, 'kmin, kmax, ink: ', i10, i10, i10 )

               IF (INK .GE. KMIN  .AND.  INK .LE. KMAX) THEN
                  OUTUW = INK
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               KMIN = VAL__MINW
               KMAX = VAL__MAXW

               IF (INK .GE. KMIN  .AND.  INK .LE. KMAX) THEN
                  OUTW = INK
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               KMIN = VAL__MINI
               KMAX = VAL__MAXI

               IF (INK .GE. KMIN  .AND.  INK .LE. KMAX) THEN
                  OUTI = INK
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               OUTK = INK

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INK

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INK

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               REM = MOD(INK, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               KVAL = INK

               POSN = 0
               OUTC = ' '

               CALL CHR_PUTK (KVAL, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is real.

         ELSE IF (ITYPE .EQ. CAT__TYPER) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               RMIN = VAL__MINUB
               RMAX = VAL__MAXUB

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTUB = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               RMIN = VAL__MINB
               RMAX = VAL__MAXB

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTB = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               RMIN = VAL__MINUW
               RMAX = VAL__MAXUW

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTUW = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               RMIN = VAL__MINW
               RMAX = VAL__MAXW

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTW = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               RMIN = VAL__MINI
               RMAX = VAL__MAXI

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTI = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               RMIN = VAL__MINK
               RMAX = VAL__MAXK

               IF (INR .GE. RMIN  .AND.  INR .LE. RMAX) THEN
                  OUTK = INR
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               OUTR = INR

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = INR

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INT(INR)
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               POSN = 0
               OUTC = ' '

               CALL CHR_PUTR (INR, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is double precision.

         ELSE IF (ITYPE .EQ. CAT__TYPED) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               DMIN = VAL__MINUB
               DMAX = VAL__MAXUB

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTUB = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               DMIN = VAL__MINB
               DMAX = VAL__MAXB

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTB = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               DMIN = VAL__MINUW
               DMAX = VAL__MAXUW

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTUW = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               DMIN = VAL__MINW
               DMAX = VAL__MAXW

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTW = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               DMIN = VAL__MINI
               DMAX = VAL__MAXI

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTI = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               DMIN = VAL__MINK
               DMAX = VAL__MAXK

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTK = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               DMIN = VAL__MINR
               DMAX = VAL__MAXR

               IF (IND .GE. DMIN  .AND.  IND .LE. DMAX) THEN
                  OUTR = IND
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               OUTD = IND

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IVAL = INT(IND)
               REM = MOD(IVAL, 2)
               IF (REM .EQ. 0) THEN
                  OUTL = .FALSE.
               ELSE
                  OUTL = .TRUE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               POSN = 0
               OUTC = ' '

               CALL CHR_PUTD (IND, OUTC, POSN)

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is logical.

         ELSE IF (ITYPE .EQ. CAT__TYPEL) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               IF (INL) THEN
                  OUTUB = 1
               ELSE
                  OUTUB = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               IF (INL) THEN
                  OUTB = 1
               ELSE
                  OUTB = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               IF (INL) THEN
                  OUTUW = 1
               ELSE
                  OUTUW = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               IF (INL) THEN
                  OUTW = 1
               ELSE
                  OUTW = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               IF (INL) THEN
                  OUTI = 1
               ELSE
                  OUTI = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               IF (INL) THEN
                  OUTK = 1
               ELSE
                  OUTK = 0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               IF (INL) THEN
                  OUTR = 1.0E0
               ELSE
                  OUTR = 0.0E0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               IF (INL) THEN
                  OUTD = 1.0D0
               ELSE
                  OUTD = 0.0D0
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               OUTL = INL

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               POSN = 0
               OUTC = ' '

               IF (INL) THEN
                  CALL CHR_PUTC ('TRUE', OUTC, POSN)
               ELSE
                  CALL CHR_PUTC ('FALSE', OUTC, POSN)
               END IF

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

*
*       Case where the input value is character.

         ELSE IF (ITYPE .EQ. CAT__TYPEC) THEN
            IF (OTYPE .EQ. CAT__TYPEUB) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (INC, IVAL, LSTAT)

               IF (LSTAT .NE. CAT__OK) THEN
                  IMIN = VAL__MINUB
                  IMAX = VAL__MAXB

                  IF (IVAL .GE. IMIN  .AND.  IVAL .LE. IMAX) THEN
                     OUTUB = IVAL
                  ELSE
                     CONVOK = .FALSE.
                  END IF
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEB) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (INC, IVAL, LSTAT)

               IF (LSTAT .NE. CAT__OK) THEN
                  IMIN = VAL__MINB
                  IMAX = VAL__MAXB

                  IF (IVAL .GE. IMIN  .AND.  IVAL .LE. IMAX) THEN
                     OUTB = IVAL
                  ELSE
                     CONVOK = .FALSE.
                  END IF
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEUW) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (INC, IVAL, LSTAT)

               IF (LSTAT .NE. CAT__OK) THEN
                  IMIN = VAL__MINUW
                  IMAX = VAL__MAXW

                  IF (IVAL .GE. IMIN  .AND.  IVAL .LE. IMAX) THEN
                     OUTUW = IVAL
                  ELSE
                     CONVOK = .FALSE.
                  END IF
               ELSE
                  CONVOK = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEW) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (INC, IVAL, LSTAT)

               IF (LSTAT .NE. CAT__OK) THEN
                  IMIN = VAL__MINW
                  IMAX = VAL__MAXW

                  IF (IVAL .GE. IMIN  .AND.  IVAL .LE. IMAX) THEN
                     OUTW = IVAL
                  ELSE
                     CONVOK = .FALSE.
                  END IF
               ELSE
                  CONVOK = .FALSE.
               END IF


            ELSE IF (OTYPE .EQ. CAT__TYPEI) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOI (INC, OUTI, LSTAT)
               IF (LSTAT .NE. CAT__OK) CONVOK = .FALSE.

            ELSE IF (OTYPE .EQ. CAT__TYPEK) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOK (INC, OUTK, LSTAT)
               IF (LSTAT .NE. CAT__OK) CONVOK = .FALSE.

            ELSE IF (OTYPE .EQ. CAT__TYPER) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOR (INC, OUTR, LSTAT)
               IF (LSTAT .NE. CAT__OK) CONVOK = .FALSE.

            ELSE IF (OTYPE .EQ. CAT__TYPED) THEN
               LSTAT = CAT__OK
               CALL CHR_CTOD (INC, OUTD, LSTAT)
               IF (LSTAT .NE. CAT__OK) CONVOK = .FALSE.

            ELSE IF (OTYPE .EQ. CAT__TYPEL) THEN
               IF (INC(1 : 1) .EQ. 'T'  .OR.  INC(1 : 1) .EQ. 't') THEN
                  OUTL = .TRUE.
               ELSE
                  OUTL = .FALSE.
               END IF

            ELSE IF (OTYPE .EQ. CAT__TYPEC) THEN
               CALL CHR_COPY (INC, .TRUE., OUTC, LSTAT)
               IF (LSTAT .NE. 0) CONVOK = .FALSE.

            ELSE
               STATUS = CAT__INVDT
               CONVOK = .FALSE.

               CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :           /'type conversion.', STATUS)

            END IF

         ELSE
            STATUS = CAT__INVDT
            CONVOK = .FALSE.

            CALL CAT1_ERREP ('CAT1_TCNVT_IDT', 'Error during data '/
     :        /'type conversion.', STATUS)

         END IF

      END IF

      END
