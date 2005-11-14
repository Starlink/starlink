# E.S.O. - VLT project/ESO Archive
# $Id: depend.mk,v 1.1.1.1 2002/04/04 20:11:45 brighton Exp $
#
# depend.mk - include Makefile for generating automatic dependencies
#
# who             when       what
# --------------  ---------  ---------------------------------------------
# Allan Brighton  05 Apr 96  Created
# ------------------------------------------------------------------------

# generate automatic dependencies
depend: $(DEPEND_TARGET) objlist

# makedepend sometimes gets confused by #ifdefs in system headers...
makedepend:
	-makedepend $(DEPEND_FLAGS) $(SRCS)

# gcc supports the -MM option for dependencies, 
# which works better than makedepend in some cases
dep: 
	@echo "generating depencies in $(PWD)"
	@sed -e '/# DO NOT DELETE THIS LINE/,$$d' < Makefile > Makefile.tmp
	@echo '# DO NOT DELETE THIS LINE -- make depend depends on it.' >> Makefile.tmp
	$(CXX) -MM $(CC_SWITCHES) $(SRCS) >> Makefile.tmp
	@cp Makefile Makefile.bak
	@mv Makefile.tmp Makefile

# Solaris CC supports the -xM1 option for dependencies,
# which works better than makedepend in some cases
soldep:
	@echo "generating depencies in $(PWD)"
	@sed -e '/# DO NOT DELETE THIS LINE/,$$d' < Makefile > Makefile.tmp
	@echo '# DO NOT DELETE THIS LINE -- make depend depends on it.' >> Makefile.tmp
	$(CXX) -xM1 $(CC_SWITCHES) $(SRCS) >> Makefile.tmp
	@cp Makefile Makefile.bak
	@mv Makefile.tmp Makefile

# generate a file with a list of object files (full path names) for this dir
objlist: 
	rm -f .objs
	d=`pwd`; for i in $(OBJS); do echo $$d/$$i >> .objs; done

depclean:
	rm -f .objs

