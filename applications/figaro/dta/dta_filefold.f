*+                          D T A _ F I L E F O L D
*  Routine:
*     DTA_FILEFOLD
*
*  Function:
*     Dummy subroutine.
*
*  Description:
*     This routine was introduced on Unix systems to fold the file names
*     to lowercase. The routine is discontinued, file names are left
*     unfolded, both on Unix and VMS.
*
*  Language:
*     Fortran.
*
*  Call:
*     CALL DTA_FILEFOLD(FILENAME)
*
*  Parameters:
*     (!) FILENAME     (Character string) Ignored.
*
*  History:
*     02-SEP-1992 (HME):
*        Dummy routine.
*+
      SUBROUTINE DTA_FILEFOLD(FILENAME)

      CHARACTER*(*) FILENAME

      END
