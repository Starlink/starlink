## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libgen.la

libgen_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) $(PRIVATE_INCLUDES) \
     $(FPP_ROUTINES)

F_ROUTINES = cli_empty.f gen_alphanum.f gen_at.f gen_comach.f	      \
gen_cvt_type.f gen_decode.f gen_delim.f gen_dformat.f gen_digits.f    \
gen_do_op.f gen_doarry.f gen_dofunc.f gen_eformat.f gen_encode.f      \
gen_ermsg.f gen_eval_ae.f gen_eval_all.f gen_eval_str.f gen_exop.f    \
gen_exopa.f gen_exopl.f gen_exopr.f gen_exops.f	      \
gen_floating.f gen_get_luns.f gen_geti4.f gen_geti4a.f gen_geti4a2.f  \
gen_getit3.f gen_getit4.f gen_getline.f gen_getmore.f gen_getr4.f     \
gen_getr4a.f gen_getr4a2.f gen_getr8.f gen_getr8a.f gen_getr8a2.f     \
gen_getstr.f gen_getstr2.f gen_getsym1.f gen_getwrd.f gen_hash.f      \
gen_hdnorm.f gen_ichtot.f gen_iendch.f gen_ilen.f gen_input.f	      \
gen_inqfunc.f gen_inqsymb.f gen_inqsymb1.f gen_integer.f gen_jline.f  \
gen_jnloff.f gen_jnlon.f gen_kntwrd.f gen_lineno.f gen_make_var.f     \
gen_makesym1.f gen_makesymb.f gen_match.f gen_negate.f gen_operator.f \
gen_parseexp.f gen_parseflt.f gen_parseint.f gen_parsename.f	      \
gen_parseop.f gen_pause.f gen_put_symb.f gen_putpmt.f gen_rank.f      \
gen_readnum.f gen_rewind.f gen_sequal.f gen_set_cli.f gen_set_luns.f  \
gen_set_symt.f gen_setsym1.f gen_sprint.f gen_unwind.f gen_write.f    \
gen_yesno.f get_subexpr.f ins_cli_item.f refresh_cli.f reset_stk_pt.f \
stack_pointer.f string_test.f strip_string.f symtab_init.f usubs.f    \
xcopy.f

C_ROUTINES = gen_makesym2.c hash.c

FPP_ROUTINES = gen_factor.F

PRIVATE_INCLUDES = CLI_STACK.INC EVAL_AE4.INC GEN_SYMBOLS.INC LOGICAL_UNITS.INC
