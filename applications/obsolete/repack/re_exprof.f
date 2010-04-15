*+RE_EXPROF - Produce a light curve exposure profile for ROSAT WFC data.
      PROGRAM RE_EXPROF
      IMPLICIT NONE
      include 'DAT_PAR'

* P. McGale - Aug 94.
* P. McGale - May 95 -- UNIX mods
*-
*    Local variables :
      CHARACTER*(DAT__SZLOC) IULOC 	! Locator to input uncorrected lcurve.
      CHARACTER*(DAT__SZLOC) ICLOC 	! Locator to input corrected lcurve.
      CHARACTER*(DAT__SZLOC) OLOC       ! Locator to output exposure profile.
      CHARACTER*(DAT__SZLOC) DULOC  ! Locator to input uncorrected lcurve data.
      CHARACTER*(DAT__SZLOC) DCLOC  ! Locator to input corrected lcurve data.
      CHARACTER*(DAT__SZLOC) DLOC  	! Locator to output exposure data
      CHARACTER*(DAT__SZLOC) QLOC       ! Locator to output exposure QUALITY
      CHARACTER*(DAT__SZLOC) QCLOC      ! Locator to input c.lcurve QUALITY
      CHARACTER*32 	TYPE
      CHARACTER*132 	INFC, INFU, OUF

      INTEGER		NVAL       ! Number of mapped data values.
      INTEGER           NDIM       ! Dimensionality of the data
      INTEGER           DPTR       ! Pointer to output exposure.
      INTEGER           DUPTR      ! Pointer to input uncorr lc. data.
      INTEGER           DCPTR      ! Pointer to input corr lc. data.
      INTEGER           QPTR       ! Pointer to exposure QUALITY array.
      INTEGER           QCPTR      ! Pointer to corr. lc. QUALITY array.
      INTEGER           VPTR       ! Pointer to exposure VARIANCE array.
      INTEGER           VCPTR      ! Pointer to corr. lc. VARIANCE array.
      INTEGER		IDIM
      integer		status
      LOGICAL           OK         ! Data is OK.

      data status/0/

* Initialise the HDS and PAR systems
      WRITE(*,*) '   RE_EXPROF Version 090595'

      CALL HDS_START (STATUS)
      CALL PAR_cmdl (' ', STATUS)

*   Initialize Asterix routines
      CALL AST_INIT()

*   Open the input & output files
      CALL PAR_GETlC ('INPU Uncorrected dataset', INFU, STATUS)
      CALL HDS_OPEN (INFU, 'READ', IULOC, STATUS)
      CALL DAT_TYPE (IULOC, TYPE, STATUS)

      CALL PAR_GETlC ('INPC Corrected dataset', INFC, STATUS)
      CALL HDS_OPEN (INFC, 'READ', ICLOC, STATUS)

      CALL PAR_GETlC ('OUTF Exposure profile', OUF, STATUS)
      CALL HDS_NEW (OUF, TYPE, TYPE, 0, 0, OLOC, STATUS)

      CALL HDX_COPY (ICLOC, OLOC, STATUS)
      IF (STATUS.NE.0) STOP '   Unable to create o/p file'

*   Check that the data are OK.
      CALL BDA_CHKDATA (IULOC, OK, NDIM, IDIM, STATUS)
      IF (NDIM.NE.1) STOP '   Dataset must be one dimensional'
      CALL BDA_CHKDATA (ICLOC, OK, NDIM, IDIM, STATUS)
      IF (NDIM.NE.1) STOP '   Dataset must be one dimensional'

* Map DATA_ARRAY arrays.
      CALL BDA_LOCDATA (IULOC, DULOC, STATUS)
      CALL DAT_MAPV (DULOC, '_REAL', 'READ', DUPTR, NVAL, STATUS)
      CALL BDA_LOCDATA (ICLOC, DCLOC, STATUS)
      CALL DAT_MAPV (DCLOC, '_REAL', 'READ', DCPTR, NVAL, STATUS)
      CALL BDA_LOCDATA (OLOC, DLOC, STATUS)
      CALL DAT_MAPV (DLOC, '_REAL', 'UPDATE', DPTR, NVAL, STATUS)

* Map VARIANCE arrays.
      CALL BDA_MAPVAR (ICLOC, 'READ', VCPTR, STATUS)
      CALL BDA_MAPVAR (OLOC, 'UPDATE', VPTR, STATUS)

* Map QUALITY arrays
      CALL BDA_LOCQUAL (ICLOC, QCLOC, STATUS)
      CALL DAT_MAPV (QCLOC, '_INTEGER', 'READ', QCPTR, NVAL, STATUS)
      CALL BDA_LOCQUAL (OLOC, QLOC, STATUS)
      CALL DAT_MAPV (QLOC, '_INTEGER', 'UPDATE', QPTR, NVAL, STATUS)

* Calculate the bin exposures.
      CALL BINEXPS(%VAL(DUPTR), %VAL(DCPTR), %VAL(VCPTR), %VAL(QCPTR),
     & %VAL(DPTR), %VAL(VPTR), %VAL(QPTR), IDIM, STATUS)


      CALL BDA_PUTUNITS (OLOC, 'second', STATUS)
      CALL BDA_PUTLABEL (OLOC, 'Exposure Profile', STATUS)

*   Tidy up
      CALL DAT_UNMAP (QCLOC, STATUS)
      CALL DAT_UNMAP (QLOC, STATUS)
      CALL DAT_UNMAP (DULOC, STATUS)
      CALL DAT_UNMAP (DCLOC, STATUS)
      CALL DAT_UNMAP (DLOC, STATUS)

      CALL AST_CLOSE

      END
