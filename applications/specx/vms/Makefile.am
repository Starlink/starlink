## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libvms.la
AM_FCFLAGS = -I../inc

libvms_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(C_ROUTINES)

F_ROUTINES = putscreen.f sysdep.f \
vms_date.f vms_help.f qualifiers.f parseat.f

FPP_ROUTINES = openuf.F  welcome.F

