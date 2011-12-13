
*+
*  Name:
*    FIO1_SERR_PC_LINUX.F

*  Purpose:
*    Generic error code handling.

*  Description:
*    The various bodies for the routine fio1_serr.f_pc_linux. The default
*    case is for g77 as we can detect g95 using it's __G95__ macro (which
*    is set to the major version so we #ifdef for it, not #if) and
*    if __GNUC__ is defined to be 4 or more, then if this is not g95
*    it must be gfortran (g77 died at the end of life for GCC3).

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*    PWD: Peter Draper (Durham)

*  History:
*    2005-SEP-12 (PWD):
*       First version.

*-

#if HAVE_CONFIG_H
#   include "config.h"
#endif

*
*

#if defined(__G95__)

*  Tuned for g95.

      INTEGER MXENO             ! Values <= MXENO are C ERRNOs
      PARAMETER( MXENO = 199 )  !
      INTEGER MXFER             ! Maximum value for FORTRAN IO errors
      PARAMETER( MXFER = 216 )  ! but may change, still under development

#elif defined(__GNUC__) && __GNUC__ >= 4

*  Must be gfortran.
      INTEGER MXENO             ! Values <= MXENO are C ERRNOs
      PARAMETER( MXENO = 5000 ) !
      INTEGER MXFER             ! Maximum value for FORTRAN IO errors
      PARAMETER( MXFER = 5017 ) ! but may change, still under development

#else

*  Tuned for g77.

*  Local Constants:
      INTEGER MXENO             ! Values <MXENO are C ERRNOs
      PARAMETER( MXENO = 99 )   ! but note that ERRNO can be > 99
      INTEGER MXFER             ! Maximum value for FORTRAN IO errors
      PARAMETER( MXFER = 129 )

#endif

*  Local Variables:
      INTEGER FIOERN( MXFER - MXENO ) ! Table of FIO error codes

#if defined(__G95__)
*  Local Data:
      DATA FIOERN/ FIO__INVRG, FIO__INVRG, FIO__INVKY, FIO__ALOPN,
     :             FIO__UNTNO, FIO__ERFMT, FIO__IVACM, FIO__ENDFL,
     :             FIO__INPCN, FIO__INPCN, FIO__INSVR, FIO__ERROR,
     :             FIO__ERROR, FIO__ILARG, FIO__ERROR, FIO__OFEOR,
     :             FIO__OUTOV /

* G95 errors:
* INVRG 200  ERROR_OPTION_CONFLICT: "Conflicting statement options";
* INVRG 201  ERROR_BAD_OPTION: "Bad statement option";
* INVKY 202  ERROR_MISSING_OPTION: "Missing statement option";
* ALOPN 203  ERROR_ALREADY_OPEN: "File already opened in another unit";
* UNTNO 204  ERROR_BADUNIT: "Unattached unit";
* ERFMT 205  ERROR_FORMAT: "FORMAT error";
* IVACM 206  ERROR_BAD_ACTION: "Incorrect ACTION specified";
* ENDFL 207  ERROR_ENDFILE: "Read past ENDFILE record";
* INPCN 208  ERROR_READ_VALUE: "Bad value during read";
* INPCN 209  ERROR_READ_OVERFLOW: "Numeric overflow on read";
* INSVR 210  ERROR_NOMEM: "Out of memory";
* ERROR 211  ERROR_ALREADY_ALLOC: "Array already allocated";
* ERROR 212  ERROR_BADFREE: "Deallocated a bad pointer";
* ILARG 213  ERROR_CORRUPT_DIRECT: "Bad record number in direct-access file";
* ERROR 214  ERROR_CORRUPT_SEQUENTIAL: "Corrupt record in unformatted sequential-access file";
* OFEOR 215  ERROR_READ_RECL: "Reading more data than the record size (RECL)";
* OUTOV 216  ERROR_WRITE_RECL: "Writing more data than the record size (RECL)";

#elif defined(__GNUC__) && __GNUC__ >= 4

      DATA FIOERN/ FIO__INVRG, FIO__INVRG, FIO__INVKY, FIO__ALOPN,
     :             FIO__UNTNO, FIO__ERFMT, FIO__IVACM, FIO__ENDFL,
     :             FIO__RDER,  FIO__RDER,  FIO__RDER,  FIO__INVRG,
     :             FIO__ILOPU, FIO__INSVR, FIO__OUTOV, FIO__OFEOR,
     :             FIO__ERROR /

* Gfortran errors:
* ERROR_OPTION_CONFLICT 5001  "Conflicting statement options"
* ERROR_BAD_OPTION      5002  "Bad statement option"
* ERROR_MISSING_OPTION  5003  "Missing statement option"
* ERROR_ALREADY_OPEN    5004  "File already opened in another unit"
* ERROR_BAD_UNIT        5005  "Unattached unit"
* ERROR_FORMAT          5006  "FORMAT error"
* ERROR_BAD_ACTION      5007  "Incorrect ACTION specified"
* ERROR_ENDFILE         5008  "Read past ENDFILE record"
* ERROR_BAD_US          5009  "Corrupt unformatted sequential file"
* ERROR_READ_VALUE      5010  "Bad value during read"
* ERROR_READ_OVERFLOW   5011  "Numeric overflow on read"
* ERROR_INTERNAL        5012  "Internal error in run-time library"
* ERROR_INTERNAL_UNIT   5013  "Internal unit I/O error"
* ERROR_ALLOCATION      5014  "Error in allocation"
* ERROR_DIRECT_EOR      5015  "Write exceeds length of DIRECT access record"
* ERROR_SHORT_RECORD    5016  "I/O past end of record on unformatted file"
* ERROR_CORRUPT_FILE    5017  "Unformatted file structure has been corrupted"


#else

*  g77
*  Local Data:
      DATA FIOERN/
     :   FIO__ERFMT, FIO__ILLUN, FIO__FIONA, FIO__UIONA, FIO__DIONA,
     :   FIO__SIONA, FIO__CNTBF, FIO__NAMER, FIO__CNTSF, FIO__UNTNC,
     :   FIO__OFEOR, FIO__TRUNF, FIO__ILSTI, FIO__OOFSP, FIO__UNTNC,
     :   FIO__RUNCH, FIO__BLINP, FIO__FORVR, FIO__SYNAM, FIO__IINAM,
     :   FIO__UNKNO, FIO__FORVR, FIO__INVRV, FIO__INVRV, FIO__INVRV,
     :   FIO__INVRV, FIO__RDER,  FIO__WRTER, FIO__NFEXI, FIO__WRTER
     :/
#endif
*.

* Rest of code is same for g77 and g95.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If SYSERR is zero, return a status value of SAI__OK.
      IF ( SYSERR .EQ. 0 ) THEN
         STATUS = SAI__OK
         GOTO 999

*  Is the error number set to end of file?
      ELSE IF ( SYSERR .EQ. -1 ) THEN
         STATUS = FIO__EOF
         GOTO 999

*  C system errors.
      ELSE IF ( SYSERR .GT. 0 .AND. SYSERR .LE. MXENO ) THEN

*  Check for specific values we can translate
         IF ( SYSERR .EQ. ERRNO__ENOENT ) THEN  ! No such file or directory
            STATUS = FIO__FILNF
         ELSE IF ( SYSERR .EQ. ERRNO__EACCES ) THEN  ! Permission denied
            STATUS = FIO__PTAFD
         ELSE IF ( SYSERR .EQ. ERRNO__EEXIST ) THEN  ! File exists
            STATUS = FIO__NFEXI
         ELSE IF ( SYSERR .EQ. ERRNO__ENODEV ) THEN  ! No such device
            STATUS = FIO__NODEV
         ELSE IF ( SYSERR .EQ. ERRNO__EMFILE ) THEN  ! Too many open files
            STATUS = FIO__TOMNY
         ELSE IF ( SYSERR .EQ. ERRNO__EFBIG ) THEN   ! File too large
            STATUS = FIO__FTOOL
         ELSE IF ( SYSERR .EQ. ERRNO__ENOSPC ) THEN  ! No space left on device
            STATUS = FIO__NSLOD
         ELSE IF ( SYSERR .EQ. ERRNO__EROFS ) THEN   ! Read-only file system
            STATUS = FIO__REDON
         ELSE IF ( SYSERR .EQ. ERRNO__ENAMETOOLONG) THEN   ! File name too long
            STATUS = FIO__FNTL
         ELSE
            STATUS = FIO__OSERR            ! General OS error
         END IF

*   FIO errors generated by the Fortran compiler rather than via
*   the C subsystem.
      ELSE IF (SYSERR .LE. MXFER ) THEN
         STATUS = FIOERN( SYSERR - MXENO )

      ELSE
*  No conversion was done.
         STATUS = FIO__ERROR
      END IF

 999  CONTINUE
