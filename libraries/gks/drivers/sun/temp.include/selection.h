/*	@(#)selection.h 10.4 88/02/08 SMI	*/

/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

struct	selection {
	int	sel_type,
		sel_items,
		sel_itembytes,
		sel_pubflags;
	caddr_t	sel_privdata;
};

extern	struct selection	selnull;

/*
 * sel_type values
 */
#define	SELTYPE_NULL	0
#define	SELTYPE_CHAR	1

/*
 * sel_itembytes values
 */
#define	SEL_UNKNOWNITEMS	-1	/* Don't know how many items */

/*
 * sel_pubflags values
 */
#define	SEL_PRIMARY	0X01	/* Primary selection */
#define	SEL_SECONDARY	0X02	/* Secondary selection */

#ifdef	cplus
/*
 * Create the selection
 */
void	selection_set(struct selection *sel, int (*sel_write)(), (*sel_clear)(),
	    windowfd);
/*
 * Fetch the selection
 */
void	selection_get(int (*sel_read)(), windowfd);
/*
 * Clear the selection
 */
void	selection_clear(windowfd);
/*
 * Write the bits of the selection
 */
void	sel_write(struct selection *sel, FILE *file);
/*
 * Read the bits of the selection
 */
void	sel_read(struct selection *sel, FILE *file);
/*
 * As the owner of the selection you should clear your hiliting because
 * you are no longer the selection owner.
 */
void	sel_clear(struct selection *sel, int windowfd);
#endif	cplus

