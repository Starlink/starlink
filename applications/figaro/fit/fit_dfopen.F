#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
C                      F I T _ D F O P E N
C
C  Routine:
C     FIT_DFOPEN
C
C  Function:
C     Creates a file into which FITS data can be written.
C
C  Description:
C     This routine is used to abstract some of the operating system
C     dependent aspects of file creation. It creates a file that the
C     other FIT routines can write to. Since writing unformatted data
C     is such a system-dependent operation, this routine (and the routine
C     that does the actual writing) have to be provided in different
C     versions for different operating systems.
C
C  Call:
C     CALL FIT_DFOPEN (LU,NAME,FIXED,STATUS)
C
C  Parameters:
C     (>) LU        (Integer) The logical unit number on which the file
C                   is to be opened.
C     (>) NAME      (Character) The full name of the file.
C     (>) FIXED     (Logical) True if the file is to be open with a fixed
C                   record length. This is ignored by UNIX, but makes a
C                   difference to VMS. Under VMS it matters whether a file
C                   has fixed 2880 byte records or havs variable length
C                   records all of which happen to be 2880 bytes long. (The
C                   variable length records contain embedded record length
C                   information. Normally, this should be avoided, and
C                   FIXED should be set .TRUE.)
C     (<) STATUS    (Integer) The status of the open call. Zero implies OK,
C                   any non-zero value will be as returned by the OPEN
C                   statement.
C
C  Common variables used:
C     (>) RINDEX    (Integer) Record index for disk file.
C                   (Defined in COMF.INC)
C
C  Author(s): K. Shortridge, AAO (KS).
C-
C  Version:
C     This version is for Linux systems. Different systems differ in the
C     way they interpret the RECL specifier in an OPEN statement.
C
C  History:
C     28th Apr 1993. KS/AAO Original version.
C     19th Jul 1993. HME/UoE, Starlink.  Disuse preprocessor and
C                    compiler switches.
C     25th Jul 1996. MJCL/Starlink, UCL.  copied from sun4 version.
C     13th Jul 2004 AA/Starlink, Exeter. Modified to Fortran preprocessor
C                   and hopefully architecture independant FPP file for
C                   new autoconf build system.
C+
      SUBROUTINE FIT_DFOPEN (LU,NAME,FIXED,STATUS)
C
      IMPLICIT NONE
C
C     Parameters:
C
      INTEGER LU, STATUS
      LOGICAL FIXED
      CHARACTER*(*) NAME
C
C     Include files
C
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER RECL              ! Value to be used for RECL.

C   Use autoconf without trying to do variable substitution
      INTEGER BYTEPRU
#if FC_RECL_UNIT == 1
      PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
      PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
      PARAMETER ( BYTEPRU = 4 )
#else
 error "Impossible FC_RECL_UNIT"
#endif

C   Calculate record length in local units, not enitrely sure about
C   this. Have to check exactly what FC_RECL_UNIT returns on each OS,
C   values should be as below;
C
C   ix86_linux =  2880    alpha_OSF1 = 720    sun4_Solaris = 2880       
      RECL = 2880 / BYTEPRU           

C
C     Set RINDEX in common to the initial value - being non-zero indicated
C     that a direct access file is being used.
C
      RINDEX = 1
C
C     Now open the file.
C
      OPEN (UNIT=LU,FILE=NAME,STATUS='UNKNOWN',ACCESS='DIRECT',
     :                   RECL=RECL,FORM='UNFORMATTED',IOSTAT=STATUS)
C
      END
         
