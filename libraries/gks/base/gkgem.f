C# IL>=a, OL>=0
      SUBROUTINE GKGEM (IER, AMESS )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONT END
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   The GKS ERROR MESSAGE FILE is searched for the required message. If this
*   is found it is returned, otherwise a default message is provided.
*
*  MAINTENANCE LOG
*  ---------------
*     17/12/82  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*     14/07/83  CJW  Local copy of KERROR
*     11/08/83  AS   GKOPIO to GKIOOP
*      7/03/84  CJW  New error message if invalid error number
*     22/01/87  JCS   IS conversion. Error checking
*
*  ARGUMENTS
*  ---------
*     IER     Error Number
*     OUT   AMESS  Error message
*
      INTEGER IER
      CHARACTER * (*) AMESS
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKFLS/    KEMFLU
*     Modify /GKFLS/    KEMFLS,KWDFLS
*     Read   /GKOPS/    KOPS
*     Read   /GKERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BGNMES  Start of default message  (P)
*     NOMESM  End of normal default error message  (P)
*     IERSV   Saved value of KERROR
*     NOEMFM  End of message if no error message file available  (P)
*     INVLMS  End of message if invalid error number (P)
*     LOW     Record number to first entry in current sublist
*     IHIGH   Record number to last entry in current sublist
*     MIDDLE  Record number to middle entry in current sublist
*     MAXREC  Number of records in file
*     IKEY    Message number of current message
*     LFOUND  True if we have found the required message
*
      CHARACTER * 20 BGNMES
      PARAMETER    ( BGNMES = '(No Error message - '        )

      CHARACTER * 18 NOMESM
      PARAMETER    ( NOMESM = 'Message not found)'          )

      CHARACTER * 27 NOEMFM
      PARAMETER    ( NOEMFM = 'Cannot access message file)' )

      CHARACTER * 21 INVLMS
      PARAMETER    ( INVLMS = 'Invalid Error number)' )

      INTEGER IERSV, LOW, IHIGH, MIDDLE, MAXREC, IKEY
      LOGICAL LFOUND
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     KEMFLU   disk         error messages file opened and accessed
*     KWDFLS   disk         WDT file closed
*
*  ALGORITHM
*  ---------
*  Binary search of Error message file is performed.
*
*  COMMENTS
*  --------
*   The action of the routine overwrites KERROR (part avoidable but
*   part not, so there is no point trying!) and so we take a copy of
*   KERROR into IERSV and restore it afterwards. There is also a problem
*   if GKERR is called with argument KERROR. To solve this IER must
*   be made STATIC by taking a local copy and putting it back at the
*   end.
*
*---------------------------------------------------------------------



      IERSV = KERROR
      KERROR = 0
      LFOUND = .FALSE.

      IF (IER .EQ. 0) THEN
         AMESS = BGNMES // INVLMS
      ELSE

         IF (KEMFLS .EQ. KFLCL) THEN

*           Open Error Message file

            CALL GKIOOP (KFEMES, KNIL, KEMFLU)

         ENDIF

         IF (KEMFLS .EQ. KFLOP) THEN

*           Find size of file

            READ(KEMFLU,IOSTAT=KERROR,REC=1) MAXREC

*           Search file for Message

            LOW = 2
            IHIGH = MAXREC

*           While (not found) and (low<=IHIGH) do
    1       CONTINUE
            IF (LFOUND .OR. (LOW .GT. IHIGH)) GO TO 2

               MIDDLE = (LOW + IHIGH) / 2
               READ(KEMFLU,IOSTAT=KERROR,REC=MIDDLE) IKEY, AMESS
               IF (IER .EQ. IKEY) THEN
                  LFOUND = .TRUE.
               ELSE
                  IF (IER .LT. IKEY) THEN
                     IHIGH = MIDDLE - 1
                  ELSE
                     LOW = MIDDLE + 1
                  ENDIF
               ENDIF

*           End While
            GO TO 1
    2       CONTINUE

         ENDIF

         IF (.NOT. LFOUND) THEN

*           Use Default Message

            IF (KEMFLS .NE. KFLOP) THEN
               AMESS = BGNMES // NOEMFM
            ELSE
               AMESS = BGNMES // NOMESM
            ENDIF

         ENDIF
      ENDIF

*     All of this will have messed up KERROR - set it back

      KERROR = IERSV
      END
