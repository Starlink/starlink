CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFITS_DR.INC
C
C  Include file for IMFITS_DR


      logical do_info		!Print out exposure summary

      character*8 n_object, n_filter, n_ra, n_dec, n_date, 
     +            n_time,   n_exp

      character*8 m_n_object, m_n_filter, m_n_ra,  m_n_dec, 
     +            m_n_date,   m_n_time,   m_n_exp
      data        m_n_object, m_n_filter, m_n_ra,  m_n_dec, 
     +            m_n_date,   m_n_time,   m_n_exp
     +   / 'OBJECT  ', 'FILTER  ', 'RA      ', 'DEC     ',
     +     'DATE    ', 'TIME    ', 'EXPOSED ' /


      logical  imfi_dum1, imfi_dum2, imfi_dum3	!n*4 make-ups


      common /imfits_drb/  do_info, imfi_dum1, imfi_dum2, imfi_dum3

      common /imfits_drc/  n_object, n_filter, n_ra, n_dec, 
     +                     n_date,   n_time,   n_exp

