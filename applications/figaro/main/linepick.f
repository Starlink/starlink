      subroutine linepick(line_name,wavelength,old_lincnt,status)
*+
* Name:
*    LINEPICK

* Invocation:
*    CALL LINEPICK(LINE_NAME,WAVELENGTH,OLD_LINCNT,STATUS)

* Purpose:
*   Identification of a spectral lines and
*   the rest wavelength to be tagged to it.
*
*
* Description:
*     None or more tables of possible lines are read in from disc and
*     the user is asked to identify the lines in the data file, matching
*     them to the lines is this list. The list can be displayed if
*     required, and the user can force indentifcations to be of lines
*     not in the list. The work tables will be increased in size as
*     required.
*
* Global variables:
*     LINE_COUNT = INTEGER (Given)
*         the number of line identifications  required
*     LEFT(NYP) = REAL ARRAY (Given)
*         Left of tram line boundaries containing line
*     RIGHT(NYP) = REAL ARRAY (Given)
*         Right of tram line boundaries containing line
*
* Arguments:
*    OLD_LINCNT = INTEGER (Given)
*        Previous value of line_count
*    LINE_NAME(LINE_COUNT) (c* = INTEGER ARRAY (Given and returned)
*        Name of line
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given and returned)
*        wavelength of line
*    STATUS = INTEGER (Given and returned)
*        Error status
*
* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*        Durham
*
* History:
*    TNW: Altered to search in figaro_prog_l and figaro_prog_s for .ids
*         files, 21-22/1/88
*    TNW: Altered to use OPNFIL 24/3/88
*    TNW: Altered to use FIG_OPFILE 5/7/88
*    TNW: Altered to receive work tables as workspace from above 12/8/88
*    TNW: DSA_OPEN_TEXT_FILE replaced FIG_OPFILE and DSA_GET_LU, 14/7/89
*    TNW: Some tidying, change to argument list of identify_line
*         25/10/89
*    TNW: Use of QMENU 1/11/89
*    TNW: changes to file i/o etc. 17/7/90
*    TNW: PAR_QUEST not used 15/8/91
*    TNW: 28/7/93 Increase size of tables if required, rather than
*         editing out lines, etc.
*- ---------------------------------------------------------------------
      implicit none

      include 'arc_dims'
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
* integer
*
      integer old_lincnt
      integer status
      integer n_work
      integer list,in_list
      integer i,lu
      integer namesst,wavestable
      integer new_namesst,new_wavestable
      integer namlen,new_namlen
      integer worksize,new_worksize
      integer slot,slot2,slot3,slot4
*
* Initial size of work tables
*
      integer MASTER
      parameter (MASTER = 800)
*
* character
*
      character*(10) line_name(line_count)
      character*50 file
      character*72 chars
*
* menus
*
      integer NMENU
      parameter (NMENU = 9)
      character*50 dict_list(NMENU)
*
* logical
*
      logical loop, ok, openok,filend
*
* integer
*
      integer len1,len2
      integer thisfile,flag
      integer pstat
*
* real
*
      real wavelength(line_count)

      integer nnums
      real values(2)
      integer chr_len

* data statements
*
      data dict_list/
     :     'EMISSION : Emission lines',
     :     'ABSORPTION : Absorption lines',
     :     'NEON   : Neon arc',
     :     'CUAR   : Copper-argon arc',
     :     'HELIUM : Helium arc',
     :     'IRON   : Iron arc',
     :     'SKY    : Sky lines',
     :     'STORED %Tfile: User supplied list in file',
     :     'OK     : All tables read in'/
*
* default settings
*
      ok = .false.
      worksize = MASTER

* Allocate space for work tables

      namlen = worksize * 10
      call dsa_get_work_array(worksize,'float',wavestable,slot,status)
      call dsa_get_work_array(namlen,'char',namesst,slot2,status)
*
* set wavelength =-1 and name ="unknown" for all the lines.
*
      do i = old_lincnt+1, line_count
        wavelength(i) = -1.0
        line_name(i)  = 'UNKNOWN'
      end do
      n_work = 0
*
* choose the appropriate table
*
      do while( .not. ok)

        openok = .false.

        do while(.not.openok)

          call qmenu('Line List Menu',dict_list,NMENU,0,values,file,
     :         list,nnums,status)

          if(status.ne.SAI__OK) then
            return
          else if(list.eq.NMENU) then

*     Exit

            openok = .true.
            ok = .true.

          else

*       stored

            if(list .ne. 8) then
              file = dict_list(list)
              len1 = index(file,':') - 1
              file(len1:) = ' '
            end if
*
*  If we get here then the file name is ok so
* open file and read in the lines. First check default directory
* for file, if not found there then look in figaro_prog_u, then try
* figaro_prog_l.
*
            call chr_lcase(file)
            if(index(file,'.').eq.0) then
              len2 = chr_len(file)
              len1 = len2
              call chr_putc('.ids',file,len1)
              call dsa_open_text_file(file,' ','old',.false.,lu,chars,
     :             status)
              if(status.ne.SAI__OK) then
                call par_wruser('Looking for .ARC file...',pstat)
                status = SAI__OK
                len1 = len2
                call chr_putc('.arc',file,len1)
                call dsa_open_text_file(file,' ','old',.false.,lu,chars,
     :               status)
              end if
              openok = status.eq.SAI__OK
              status = SAI__OK
            else
              call dsa_open_text_file(file,' ','old',.false.,lu,chars,
     :             status)
            end if
          end if
        end do


*   We want to read from the file

        if(.not.ok) then

* Read in number of lines in list, allow for any number of lines
* preceded with "*" to be at the start of the list-these are just output
* to the terminal

          call par_wruser(' ',pstat)
*
* read the lines. If we are limiting the range to consider, then discard
* lines at this stage. Note that values less than zero are accepted,
* since these are not wavelengths anyway
*
          thisfile = n_work + 1
          loop = status.eq.SAI__OK
          flag = 0
          do while(loop)
            loop = .false.
            do while(n_work.lt.worksize)
              call rdids(lu,%VAL(CNF_PVAL(wavestable)),
     :                   %VAL(CNF_PVAL(namesst)),filend,n_work,
     :                   %VAL(CNF_CVAL(namlen)))
              if(filend) goto 1
            end do
*
*  overflow MASTER_name and  MASTER_table
*
* Allocate new space for work tables

            new_worksize = worksize * 2
            new_namlen = new_worksize * 10
            call dsa_get_work_array(new_worksize,'float',new_wavestable,
     :                              slot3,status)
            call dsa_get_work_array(new_namlen,'char',new_namesst,slot4,
     :                              status)

* Copy lists to new workspace

            call copr2r(n_work,%VAL(CNF_PVAL(wavestable)),
     :                  %VAL(CNF_PVAL(new_wavestable)))
            call chr_move(%VAL(CNF_PVAL(namesst)),
     :                    %VAL(CNF_PVAL(new_namesst)),
     :                    %VAL(CNF_CVAL(namlen)),
     :                    %VAL(CNF_CVAL(new_namlen)))

*  and free old workspace

            call dsa_free_workspace(slot2,status)
            call dsa_free_workspace(slot,status)

*  Now switch to pointing to new arrays

            wavestable = new_wavestable
            namesst = new_namesst
            worksize = new_worksize
            slot = slot3
            slot2 = slot4

            loop = flag.le.0

* This bit allows for over-estimate because of end=1 in read

            n_work = n_work + 1
 1          continue
            n_work = n_work - 1
          end do
*
* close down the line file and release unit number
*
          call dsa_free_lu(lu,status)
          in_list = n_work - thisfile + 1
          write(chars,'(i4,a)')in_list,' lines read from '//file
          call par_wruser(chars,pstat)
*
* check that when combined with existing lines
* that tables do not overlfow. Not sure this is right
*
          if(n_work .eq. worksize) then
            ok=.true.
          end if
        end if

* ok

      end do

      if(status.eq.SAI__OK) then

*   Sort wavelengths in look_up table into ascending order

        if(n_work.gt.1) then
          call arsort2(%VAL(CNF_PVAL(wavestable)),
     :                 %VAL(CNF_PVAL(namesst)),n_work,
     :                 %VAL(CNF_CVAL(namlen)))
        end if
*
*   Identify lines
*
        call identify_line(%VAL(CNF_PVAL(namesst)),
     :                     %VAL(CNF_PVAL(wavestable)),line_name,
     :                     wavelength,n_work,%VAL(CNF_PVAL(d_xptr)),
     :                     %VAL(CNF_PVAL(d_tlptr)),
     :                     %VAL(CNF_PVAL(d_trptr)),status)

        call dsa_free_workspace(slot2,status)
        call dsa_free_workspace(slot,status)
*
*   Write out current line list
*
        call par_wruser('C U R R E N T   L I N E   L I S T',pstat)
        call par_wruser('_________________________________',pstat)
        call par_wruser('line No     identification      wavelength'
     :       ,pstat)
        do i=1,line_count
          write(chars,'(i4,11x,a10,8x,f10.4)')i,line_name(i),
     :         wavelength(i)
          call par_wruser(chars,pstat)
        end do
      end if
      end
