# Makefile for SPECX: GEN

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#  Author:
#     Tim Jenness (JACH)

# Include directories
# Should really inherit values from parent makefile

STAR_INC = /star/include
SPX_INC  = ../inc

# Package and subsystem name

LIB = gen
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C
CFLAGS = 
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= cli_empty.f gen_alphanum.f gen_at.f gen_comach.f \
gen_cvt_type.f gen_decode.f gen_delim.f gen_dformat.f \
gen_digits.f gen_do_op.f gen_doarry.f gen_dofunc.f \
gen_eformat.f gen_encode.f gen_ermsg.f gen_eval_ae.f\
gen_eval_all.f gen_eval_str.f gen_exop.f gen_exopa.f \
gen_exopl.f gen_exopr.f gen_exops.f gen_factor.f \
gen_floating.f gen_get_luns.f gen_geti4.f gen_geti4a.f \
gen_geti4a2.f gen_getit3.f gen_getit4.f gen_getline.f \
gen_getmore.f gen_getr4.f gen_getr4a.f gen_getr4a2.f \
gen_getr8.f gen_getr8a.f gen_getr8a2.f gen_getstr.f \
gen_getstr2.f gen_getsym1.f gen_getwrd.f gen_hash.f \
gen_hashdel.f gen_hashins.f gen_hashsrch.f \
gen_hdnorm.f gen_ichtot.f gen_iendch.f gen_ilen.f \
gen_input.f gen_inqfunc.f gen_inqsymb.f gen_inqsymb1.f \
gen_integer.f gen_jline.f gen_jnloff.f gen_jnlon.f \
gen_kntwrd.f gen_lineno.f gen_make_var.f \
gen_makesym1.f gen_makesymb.f gen_match.f gen_negate.f \
gen_operator.f gen_parseexp.f gen_parseflt.f \
gen_parseint.f gen_parsename.f gen_parseop.f \
gen_pause.f gen_put_symb.f gen_putpmt.f gen_rank.f \
gen_readnum.f gen_rewind.f gen_sequal.f gen_set_cli.f \
gen_set_luns.f gen_set_symt.f gen_setsym1.f \
gen_sprint.f gen_unwind.f gen_write.f gen_yesno.f \
get_subexpr.f ins_cli_item.f refresh_cli.f \
reset_stk_pt.f stack_pointer.f string_test.f \
strip_string.f symtab_init.f usubs.f xcopy.f

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES = 

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = cli_stack.inc eval_ae4.inc gen_symbols.inc \
logical_units.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libgen.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null; \
	rm -f `echo $(PRIVATE_INCLUDES) | tr "[:lower:]" "[:upper:]"` 1>/dev/null 2>/dev/null;


# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_gen.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

CLI_STACK.INC:     cli_stack.inc;     $(LINK) $? $@
EVAL_AE4.INC:      eval_ae4.inc;      $(LINK) $? $@
GEN_SYMBOLS.INC:   gen_symbols.inc;   $(LINK) $? $@
LOGICAL_UNITS.INC: logical_units.inc; $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

cli_empty.o: CLI_STACK.INC
gen_at.o: CLI_STACK.INC
gen_do_op.o: EVAL_AE4.INC
gen_encode.o: LOGICAL_UNITS.INC
gen_eval_ae.o: EVAL_AE4.INC
gen_eval_all.o: EVAL_AE4.INC
gen_eval_str.o: GEN_SYMBOLS.INC
gen_factor.o: EVAL_AE4.INC
gen_get_luns.o: LOGICAL_UNITS.INC
gen_getline.o: CLI_STACK.INC LOGICAL_UNITS.INC
gen_getmore.o: CLI_STACK.INC
gen_ichtot.o: CLI_STACK.INC
gen_input.o: CLI_STACK.INC
gen_inqsymb.o: GEN_SYMBOLS.INC
gen_lineno.o: CLI_STACK.INC
gen_operator.o: EVAL_AE4.INC
gen_putpmt.o: LOGICAL_UNITS.INC
gen_rewind.o: CLI_STACK.INC
gen_set_cli.o: CLI_STACK.INC
gen_set_luns.o: LOGICAL_UNITS.INC
gen_set_symt.o: GEN_SYMBOLS.INC
gen_sprint.o: LOGICAL_UNITS.INC
gen_unwind.o: CLI_STACK.INC
ins_cli_item.o: CLI_STACK.INC
refresh_cli.o: CLI_STACK.INC
reset_stk_pt.o: CLI_STACK.INC
stack_pointer.o: CLI_STACK.INC

