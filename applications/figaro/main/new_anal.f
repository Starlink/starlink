      subroutine new_anal(line_name,wavelength,control,status)
*+
* Name:
*    NEW_ANAL

* Invocation:
*    CALL NEW_ANAL(LINE_NAME,WAVELENGTH,CONTROL,STATUS)

* Purpose:
*   To set the control block for data analysis.

* Description:
*     To set the control block for data analysis.
*   This may be done in either of two ways:-
*       (1) All lines are fitted with the same model
*       (2) Separate models are used for each line

* Arguments:
*    LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Lines rest wavelengths
*    STATUS = INTEGER (Given and returned)
*        Error status
*    CONTROL(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Returned)
*        fitting control
* History:
*  Altered to use par_quest TNW 7/10/88
*
* ----------------------------------------------------------------------
      implicit none
      include 'arc_dims'
*-
      integer status
      character*10 line_name(line_count)
      real wavelength(line_count)
      integer control(ncntrl,nyp,nxp,spdim2)
      include 'status_inc'
      integer fit_status(MAX_CONTROL)
      integer i,j,k
      logical all
      integer pstat
      logical par_quest,dummy1,dummy2
      character*24 chars

* Copy default model into current

      do i = 1, MAX_DECODE_CONTROL
        deccntr(i) = default_model(i)
      end do

* define single gaussian+base for each piont

      if((line_count.gt.1).and.(.not.batch)) then
        all = par_quest('Use same fit model for all lines?',.true.)
      else
        all = .true.
      end if

      if (all) then
        call set_fit_menu(dummy1,dummy2,deccntr,wavdim,gpscal
     :        ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :        ,line_count,errpre,inherit,status)

* encode in fit_status word

        call encode_contrl(deccntr,ncntrl,fit_status)

* set the initial values of mask and fit_status for each point

        do k = 1,spdim2
          do i = 1,nxp

* Store the requested operation in CONTROL

            do j=1,line_count
              call set_control(control,j,i,k,fit_status)
            end do
          end do
        end do

      else

        do j=1,line_count
          write(chars,'(''Line number '',i3)') j
          call par_wruser(chars,pstat)
          write(chars,'(a10,4x,f9.4)') line_name(j),wavelength(j)
          call par_wruser(chars,pstat)
          call set_fit_menu(dummy1,dummy2,deccntr,wavdim,gpscal
     :         ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :         ,line_count,errpre,inherit,status)

* encode in fit_status word

          call encode_contrl(deccntr,ncntrl,fit_status)

* set the initial values of mask and fit_status for each point


          do k = 1,spdim2
            do i = 1,nxp

* Store the requested operation in CONTROL

              call set_control(control,j,i,k,fit_status)
            end do
          end do
        end do
      end if

      end
