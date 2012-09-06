#include "config.h"
      INTEGER FUNCTION GKDFOP(LUN, NAME, RECLEN)
*
* Copyright (C) SERC 1986
*
*-----------------------------------------------------------------------
*
*  Type of routine:  SYSTEM INTERFACE
*  Author:           DLT
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Find and open a data file.
*
*  MAINTENANCE LOG
*  ---------------
*     28/11/95  DLT  Original version stabilized.
*     17/06/04  TIMJ Autoconf version
*
*  ARGUMENTS
*  ---------
*     NAME    INP  The name of the data file.
*     RECLEN  INP  Record length
*     GKDFOP  OUT  Status from Fortran OPEN.
*
      CHARACTER*(*) NAME
      INTEGER LUN, RECLEN

      CHARACTER*1024 TRANS, FNAME
      INTEGER LNBLNK, IOS, LDIR, I1, I2

*  See if there is a translation for GKS_DIR
#if HAVE_INTRINSIC_GETENV || HAVE_GETENV
      CALL GETENV('GKS_DIR', TRANS)
#else
 error 'Do not know how to get environment variables'
#endif
      IF (LNBLNK(TRANS).EQ.0) THEN
         TRANS = '/star/etc'
      ENDIF

*  Try the directory pointed to by the environment variable.
      FNAME = TRANS(:LNBLNK(TRANS))//NAME
      OPEN(UNIT = LUN, IOSTAT = IOS, FILE = FNAME,
#if HAVE_FC_OPEN_ACTIONREAD
     :       ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :       READONLY,
#endif
     :       STATUS = 'OLD', ACCESS = 'DIRECT', RECL = RECLEN)

      IF (IOS.NE.0) THEN

*  Try directories relative to PATH
         CALL GETENV('PATH', TRANS)

         LDIR = LNBLNK(TRANS)
         I1 = 1
   10    CONTINUE
         I2 = INDEX(TRANS(I1:),':')
         IF (I2.EQ.0) THEN
            FNAME = TRANS(I1:LDIR) // '/../etc' // NAME
         ELSE
            FNAME = TRANS(I1:I1+I2-2) // '/../etc' // NAME
            I1 = I1 + I2
         ENDIF
         OPEN(UNIT = LUN, IOSTAT = IOS, FILE = FNAME,
#if HAVE_FC_OPEN_ACTIONREAD
     :       ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :       READONLY,
#endif
     :       STATUS = 'OLD', ACCESS = 'DIRECT', RECL = RECLEN)
         IF (IOS.EQ.0) GOTO 999

         IF (I2.NE.0 .AND. I1.LE. LDIR) GO TO 10
      END IF
  999 CONTINUE
      GKDFOP = IOS
      END
