## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	for d in @itcl_subdirs@; \
        do \
           cd $$d; \
           $(MAKE) $@; \
        done
           
STAR_MANIFEST_INSTALL_COMMAND = \
        for d in @itcl_subdirs@; \
        do \
           cd $$d; \
           $(MAKE) prefix=STAGINGPREFIX exec_prefix=STAGINGPREFIX install; \
        done

