/***********************************************************************
*                                                                      *
*   file IDI_ERR.H                                                     *
*                                                                      *
*   IDI  ERRORS & CONSTANTS definition                                 *
*                                                                      *
*   Code = 134250498 + 65536 * <fac> + 8 * <mes>                       *
*   where <fac> = 275                                                  *
*                                                                      *
************************************************************************
*   V 2.0    910320                                                    *
*            920326  Added VD_ codes                                   *
*            920807  Added NOCAP                                       *
*   Author : Nick Eaton  - Durham University                           *
*   Revised: Brian McIlwrath - Starlink, RAL                           *
*            960604  Revised to match FORTRAN include file             *
***********************************************************************/

# define II_SUCCESS    0

# define DISWNDERR     152275474
# define NOAVAILDEV    152275482
# define DEVNOTOP      152275490
# define NODYNCONF     152275498
# define DEVNAMERR     152275506
# define DEVCAPTRUNC   152275514
# define MAXCONFN      152275522
# define EXTBMNOTFND   152275530

# define MEMALLERR     152275538
# define DCTFILERR     152275546
# define DYNCONFEN     152275554
# define DYNCONFNOTEN  152275562

# define ILLCONFID     152275570

# define MEMNOTFREE    152275578
# define ILLMEMID      152275586
# define MEMNOTALL     152275594
# define MAXMEM        152275602

# define TWTOOBIG      152275610
# define IMGTOOBIG     152275618

# define LUTIDERR      152275626
# define LUTLENERR     152275634
# define LUTNOTDEF     152275642

# define ITTIDERR      152275650
# define ITTLENERR     152275658

# define CURNOTDEF     152275666

# define ROINOTDEF     152275674
# define MAXROI        152275682

# define ILLCURID      152275690
# define ILLTRIGGER    152275698
# define ILLINTTYPE    152275706
# define ILLINTOBJ     152275714
# define ILLINTOPER    152275722

# define IDINOSUPP     152275730

# define DCTFILWARN    152275738

# define DEPTHERR      152275746
# define MEMTYPERR     152275754
# define MAXMEMLST     152275762

# define LUTNOTFREE    152275770

# define CURNOTFREE    152275778
# define CURNOTVIS     152275786

# define ROINOTVIS     152275794

# define INTNOTALL     152275802
# define MAXNOINT      152275810
# define INTNOEN       152275818

# define FONTNOTDEF    152275826

# define VD_MAXWIND    152275834
# define VD_KWTYPERR   152275842
# define VD_FILKWNOTCR 152275850
# define VD_WINOTOPN   152275858
# define VD_FILKWERR   152275866
# define VD_NULLDEV    152275874
# define VD_SAVNOTALL  152275882
# define VD_XCONNERR   152275890
# define VD_UNSUPVT    152275898

# define NOCAP         152275394

/*********************************************************************/
