*+                          D T A _ V E R S N A M E
*                         D T A Z _ V E R S N A M E
*  Routine:
*     DTA_VERSNAME
*    DTAZ_VERSNAME
*
*  Function:
*     Dummy subroutine.
*
*  Description:
*     This routine was introduced on Unix systems to add a version
*     number to a file name. The routine is discontinued, file names are
*     left unchanged, both on Unix and VMS.
*
*  Language:
*     Fortran.
*
*  Call:
*     CALL DTA_VERSNAME(OLD,STAT,NEW)
*    CALL DTAZ_VERSNAME(OLD,STAT,NEW)
*
*  Parameters:
*     (>) OLD          (Character string) Copied to NEW.
*     (>) STAT         (Character string) Ignored.
*     (<) NEW          (Character string) A copy of OLD.
*
*  History:
*     03-SEP-1992 (HME):
*        Dummy routine.
*+
      SUBROUTINE DTA_VERSNAME( OLD, STAT, NEW )

      CHARACTER * ( * ) OLD, STAT, NEW

      NEW = OLD

      END
