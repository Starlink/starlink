      subroutine read_file
*+
* Name:
*    READ_FILE

* Invocation:
*    CALL READ_FILE
*
* Purpose:
*   To read in dictionaries used by BATCH into its internal arrays, from
*   the files BATCH.DAT and NOBATCH.DAT in FIGARO_PROG_U
*
* Description:
*   To read in dictionaries used by BATCH into its internal arrays, from
*   the files BATCH.DAT and NOBATCH.DAT in FIGARO_PROG_U
*
* Returned in common:
*   /BATCH_DICT/:
*     DICT(400) = CHARACTER*150 ARRAY (Given)
*        Dictionary of commands
*     FORBID(30) = CHARACTER*20 ARRAY (Given)
*        "Forbidden commands"
*   /BATCH_DICTI/:
*     NDICT = INTEGER (Given)
*        Number of commands in dictionary
*     NFORBID = INTEGER (Given)
*        Number of "forbidded" commands
* Author:
*   T.N.Wilkins, Manchester
* History:
*   Altered to search files in FIGARO_PROG_U, FIGARO_PROG_L and
*   FIGARO_PROG_S, in that order. TNW 22/1/88
*   Introduction of routine OPNFIL 2/3/88 TNW, changed to fig_opfile 13/7/88
*- ---------------------------------------------------------------------
      implicit none
      character*150 dict(400),forbid(30)*20
      integer ndict,istat
      integer nforbid
      common/bat_dict/dict,forbid
      common/bat_dicti/ndict,nforbid

* File for allowed functions to run in batch

      call fig_opfile('batch','dat',1,istat)
      ndict=0
      do while(istat.eq.0)
        ndict=ndict+1
        read(1,'(a150)',iostat=istat) dict(ndict)
      end do
      ndict = ndict - 1
      close(unit=1,iostat=istat)

* File for functions not allowed to be run in batch

      istat=0
      call fig_opfile('nobatch','dat',1,istat)
      nforbid=0
      do while(istat.eq.0)
        nforbid=nforbid+1
        read(1,'(a20)',iostat=istat) forbid(nforbid)
      end do
      nforbid=nforbid-1
      close(unit=1,iostat=istat)
      end
