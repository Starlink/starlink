C# IL>=a,OL>=0
      SUBROUTINE GKLWDT ( IWKTY, MAJOR, NREALA, REALA, ITEM)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             DLT
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Find the record number of a WDT entry, opening WDT if necessary.
*
*  MAINTENANCE LOG
*  ---------------
*     29/04/86  DLT   Original version stabilized.
*     02/06/86  DCS   Removed unused local ISET.
*     01/02/90  KEVP  Set length of master directory to twice the
*                     number of available workstation types (S322).
*                     Improved commenting.
*
*  ARGUMENTS
*  ---------
*     INP   IWKTY  Workstation type index
*     INP   MAJOR  Major key to identify item (WDT entry)
*     INP   NREALA Dimension of array REALA
*     INP   REALA  Scratch real array
*     OUT   ITEM   Record number of requested item
*
      INTEGER IWKTY, MAJOR, ITEM ,NREALA
      REAL REALA(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKOPS/    KOPS
*     Read   /GKWDT/    CWDTFN
*     Read   /GKFLS/    KWDFLU, KWDTRL
*     Modify /GKYERR/   Set error status
*     Modify /GKFLS/    KWDFLS, KEMFLS
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IDIREC  Directory record number (P)
*     ITEMSZ  Length of WDT directory cache (P)
*     IWDT    Record number of WDT Directory for wk type IWKTY
*     J       Loop index
*     MASTER  Copy of WDT file master directory
*     NQIRM   Number of integers in master directory
*     NQIR    Number of integer values found
*     NQRR    Number of real values found
*     IWDTD   WDT directory cache
*     IWKTC   Workstation type of cache entry
*
      INTEGER     IDIREC
      PARAMETER ( IDIREC = 1)
      INTEGER      ITEMSZ
      PARAMETER ( ITEMSZ = 124)
*     More thought needs to be done in selecting this value.
      INTEGER  IWDT, J ,NQIR, NQRR
      INTEGER MASTER(2*KMXWKT), IWDTD(ITEMSZ), NQIRM, IWKTC
      SAVE NQIRM, MASTER, IWDTD, IWKTC
*
*  ERRORS
*  ------
*     23
*
*  COMMENTS
*  --------
*     The WDT consists of of records containing  variable sized items
*     (accessed by GKGTRC) which may span over record boundaries.
*
*     A cut item is part of an item cut from the rest by a record
*     boundary. It has the same structure as a whole item not cut
*     by a record boundary.
*
*  A record contains -
*
*     KITEM      Number of and cut items in this record
*     INEXT      Next record number
*     MORE       Record overflow flag (last item 0=whole 1=cut)
*    (cut) item  KITEM lists each consisting of -
*
*        KINT   Number of integer data in this list. (may be zero)
*        KREAL  Number of real data in this list. (may be zero)
*        KDATA  Integer data. (only if KINT > 0)
*        QDATA  Real data. (only if KREAL > 0)
*
*  Master directory  Record 1 is the directory, it contains only one
*                    item (item 1).
*                    the directory only has integer data (NQIR=0)
*                    consisting of pairs :
*                                            Workstation type
*                                            WDT directory record number
*
*  WDT directory     One for each workstation type. It contains only one
*                    item, containing pointers (record numbers) to the
*                    bundle tables for that workstation type.
*
*   The code for cacheing the WDT master index assumes that this routine
*   is the only one that opens the WDT file and that therefore the flag
*   that shows whether the file is open can be used to determine if the
*   master index has been read.
*---------------------------------------------------------------------

*     Open WDT file and get master directory


      IF (KWDFLS .EQ. KFLCL) THEN
         CALL GKIOOP(KFWDT, KNIL, KWDFLU)

*        Read in master directory

         CALL GKGTRC ( IDIREC, 1, KMXWKT*2, NREALA, NQIRM, NQRR,
     :                                MASTER, REALA )
         NQIRM = NQIRM / 2

*        Set workstation cache value invalid

         IWKTC = -999

      END IF


      IF (KERROR .EQ. 0) THEN

*        Case : List of Workstation types

         IF ((MAJOR .EQ. KWKLST) .AND. (KERROR .EQ. 0)) THEN

            ITEM = 1

         ELSE IF (KERROR .EQ. 0) THEN

*           Search for required workstation

            DO 3 J = 1, NQIRM
               IF (MASTER(1+2*(J-1)) .EQ. IWKTY) THEN
                  IWDT = MASTER(1+2*J-1)
                  GO TO 4
               END IF
    3       CONTINUE

            KERROR = 23
            GO TO 998

    4       CONTINUE

*           Input WDT directory if not already in cache

            IF (IWKTC.NE.IWKTY) THEN
               CALL GKGTRC ( IWDT, 1, ITEMSZ, NREALA, NQIR, NQRR,
     :                                 IWDTD, REALA )
               IF (KERROR .NE. 0) GO TO 998
               IWKTC = IWKTY
            END IF

*           Get Record number of required item
            IF (MAJOR .LT. KLCDR) THEN
               ITEM = IWDTD(MAJOR)
            ELSE
               ITEM = IWDTD(MAJOR-KLCDR+KLC)
            END IF

         END IF

      END IF

  998 CONTINUE

      END
