## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	for d in @itcl_subdirs@; \
        do \
           ( cd $$d; $(MAKE) $@; ) \
        done


itcl_config_dir = config/config.guess config/config.sub config/install-sh \
config/mkinstalldirs  
 
itcl_doc_dir = doc/README  
 
itcl_itcl_dir = itcl/Makefile.in itcl/aclocal.m4 itcl/configure \
itcl/configure.in itcl/license.terms itcl/pkgIndex.tcl.in
 
 
itcl_itcl_doc_dir = itcl/doc/configbody.n itcl/doc/body.n itcl/doc/class.n \
itcl/doc/code.n itcl/doc/delete.n itcl/doc/ensemble.n \
itcl/doc/find.n itcl/doc/itcl.n itcl/doc/itcl_class.n \
itcl/doc/itcl_info.n itcl/doc/itclsh.1 itcl/doc/itclvars.n \
itcl/doc/license.terms itcl/doc/local.n itcl/doc/man.macros \
itcl/doc/scope.n  
 
itcl_itcl_mac_dir = itcl/mac/MW_ItclHeader.pch itcl/mac/itclMacApplication.r itcl/mac/itclMacLibrary.r \
itcl/mac/itclMacResource.r itcl/mac/itclMacTclCode.r itcl/mac/itclStaticApplication.r \
itcl/mac/pkgIndex.tcl itcl/mac/tclMacAppInit.c

 
itcl_itcl_generic_dir = itcl/generic/itcl.decls itcl/generic/itcl.h itcl/generic/itclDecls.h \
itcl/generic/itclInt.decls itcl/generic/itclInt.h itcl/generic/itclIntDecls.h \
itcl/generic/itclStubInit.c itcl/generic/itclStubLib.c itcl/generic/itcl_bicmds.c \
itcl/generic/itcl_class.c itcl/generic/itcl_cmds.c itcl/generic/itcl_ensemble.c \
itcl/generic/itcl_linkage.c itcl/generic/itcl_methods.c itcl/generic/itcl_migrate.c \
itcl/generic/itcl_objects.c itcl/generic/itcl_obsolete.c itcl/generic/itcl_parse.c \
itcl/generic/itcl_util.c  
 
itcl_itcl_library_dir = itcl/library/itcl.tcl  
 
itcl_itcl_tests_dir = itcl/tests/defs itcl/tests/all itcl/tests/basic.test \
itcl/tests/body.test itcl/tests/chain.test itcl/tests/delete.test \
itcl/tests/ensemble.test itcl/tests/import.test itcl/tests/info.test \
itcl/tests/inherit.test itcl/tests/interp.test itcl/tests/local.test \
itcl/tests/methods.test itcl/tests/mkindex.itcl itcl/tests/mkindex.test \
itcl/tests/namespace.test itcl/tests/protection.test itcl/tests/scope.test \
itcl/tests/tclIndex  
 
itcl_itcl_tests_old_dir = itcl/tests/old/AAA.test itcl/tests/old/Bar.tcl itcl/tests/old/BarFoo.tcl \
itcl/tests/old/Baz.tcl itcl/tests/old/Foo.tcl itcl/tests/old/FooBar.tcl \
itcl/tests/old/Geek.tcl itcl/tests/old/Mongrel.tcl itcl/tests/old/VirtualErr.tcl \
itcl/tests/old/all itcl/tests/old/basic.test itcl/tests/old/inherit.test \
itcl/tests/old/tclIndex itcl/tests/old/testlib.tcl itcl/tests/old/toaster.test \
itcl/tests/old/uplevel.test itcl/tests/old/upvar.test

 
itcl_itcl_tests_old_toasters_dir = itcl/tests/old/toasters/Appliance.tcl itcl/tests/old/toasters/Hazard.tcl itcl/tests/old/toasters/Outlet.tcl \
itcl/tests/old/toasters/SmartToaster.tcl itcl/tests/old/toasters/Toaster.tcl itcl/tests/old/toasters/tclIndex \
itcl/tests/old/toasters/usualway.tcl  
 
itcl_itcl_unix_dir = itcl/unix/Makefile.in itcl/unix/configure itcl/unix/configure.in \
itcl/unix/itclConfig.sh.in itcl/unix/pkgIndex.tcl.in itcl/unix/tclAppInit.c \
itcl/unix/test.tcl  
 
itcl_itcl_win_dir = itcl/win/dllEntryPoint.c itcl/win/itcl.rc itcl/win/itclsh.rc \
itcl/win/makefile.bc itcl/win/makefile.vc itcl/win/pkgIndex.tcl \
itcl/win/tclAppInit.c  
 
itcl_itk_dir = itk/Makefile.in itk/aclocal.m4 itk/configure \
itk/configure.in itk/license.terms itk/pkgIndex.tcl.in
 
 
itcl_itk_demos_dir = itk/demos/README itk/demos/itkedit

 
itcl_itk_doc_dir = itk/doc/Archetype.n itk/doc/Toplevel.n itk/doc/Widget.n \
itk/doc/itk.n itk/doc/itkvars.n itk/doc/itkwish.1 \
itk/doc/license.terms itk/doc/man.macros itk/doc/usual.n
 
 
itcl_itk_examples_dir = itk/examples/Info.itk itk/examples/MessageInfo.itk itk/examples/README \
itk/examples/TextDisplay.itk itk/examples/TextInfo.itk itk/examples/tclIndex \
itk/examples/viewfile  
 
itcl_itk_generic_dir = itk/generic/itk.decls itk/generic/itk.h itk/generic/itkDecls.h \
itk/generic/itkStubInit.c itk/generic/itkStubLib.c itk/generic/itk_archetype.c \
itk/generic/itk_cmds.c itk/generic/itk_option.c itk/generic/itk_util.c
 
 
itcl_itk_library_dir = itk/library/Archetype.itk itk/library/Toplevel.itk itk/library/Widget.itk \
itk/library/itk.tcl itk/library/tclIndex

 
itcl_itk_mac_dir = itk/mac/MW_ItkHeader.pch itk/mac/itkMacApplication.r itk/mac/itkMacLibrary.r \
itk/mac/itkMacResource.r itk/mac/itkMacTclCode.r itk/mac/itkStaticApplication.r \
itk/mac/pkgIndex.tcl itk/mac/tclIndex itk/mac/tkMacAppInit.c
 
 
itcl_itk_tests_dir = itk/tests/defs itk/tests/all itk/tests/interp.test \
itk/tests/option.test itk/tests/privacy.test itk/tests/public.test \
itk/tests/toplevel.test itk/tests/widget.test

 
itcl_itk_unix_dir = itk/unix/Makefile.in itk/unix/configure itk/unix/configure.in \
itk/unix/itkConfig.sh itk/unix/itkConfig.sh.in itk/unix/pkgIndex.tcl.in \
itk/unix/tkAppInit.c  
 
itcl_itk_win_dir = itk/win/dllEntryPoint.c itk/win/makefile.bc itk/win/makefile.vc \
itk/win/pkgIndex.tcl itk/win/winMain.c

 
itcl_itk_win_rc_dir = itk/win/rc/cursor00.cur itk/win/rc/cursor02.cur itk/win/rc/cursor04.cur \
itk/win/rc/cursor06.cur itk/win/rc/cursor08.cur itk/win/rc/cursor0a.cur \
itk/win/rc/cursor0c.cur itk/win/rc/cursor0e.cur itk/win/rc/cursor10.cur \
itk/win/rc/cursor12.cur itk/win/rc/cursor14.cur itk/win/rc/cursor16.cur \
itk/win/rc/cursor18.cur itk/win/rc/cursor1a.cur itk/win/rc/cursor1c.cur \
itk/win/rc/cursor1e.cur itk/win/rc/cursor20.cur itk/win/rc/cursor22.cur \
itk/win/rc/cursor24.cur itk/win/rc/cursor26.cur itk/win/rc/cursor28.cur \
itk/win/rc/cursor2a.cur itk/win/rc/cursor2c.cur itk/win/rc/cursor2e.cur \
itk/win/rc/cursor30.cur itk/win/rc/cursor32.cur itk/win/rc/cursor34.cur \
itk/win/rc/cursor36.cur itk/win/rc/cursor38.cur itk/win/rc/cursor3a.cur \
itk/win/rc/cursor3c.cur itk/win/rc/cursor3e.cur itk/win/rc/cursor40.cur \
itk/win/rc/cursor42.cur itk/win/rc/cursor44.cur itk/win/rc/cursor46.cur \
itk/win/rc/cursor48.cur itk/win/rc/cursor4a.cur itk/win/rc/cursor4c.cur \
itk/win/rc/cursor4e.cur itk/win/rc/cursor50.cur itk/win/rc/cursor52.cur \
itk/win/rc/cursor54.cur itk/win/rc/cursor56.cur itk/win/rc/cursor58.cur \
itk/win/rc/cursor5a.cur itk/win/rc/cursor5c.cur itk/win/rc/cursor5e.cur \
itk/win/rc/cursor60.cur itk/win/rc/cursor62.cur itk/win/rc/cursor64.cur \
itk/win/rc/cursor66.cur itk/win/rc/cursor68.cur itk/win/rc/cursor6a.cur \
itk/win/rc/cursor6c.cur itk/win/rc/cursor6e.cur itk/win/rc/cursor70.cur \
itk/win/rc/cursor72.cur itk/win/rc/cursor74.cur itk/win/rc/cursor76.cur \
itk/win/rc/cursor78.cur itk/win/rc/cursor7a.cur itk/win/rc/cursor7c.cur \
itk/win/rc/cursor7e.cur itk/win/rc/cursor80.cur itk/win/rc/cursor82.cur \
itk/win/rc/cursor84.cur itk/win/rc/cursor86.cur itk/win/rc/cursor88.cur \
itk/win/rc/cursor8a.cur itk/win/rc/cursor8c.cur itk/win/rc/cursor8e.cur \
itk/win/rc/cursor90.cur itk/win/rc/cursor92.cur itk/win/rc/cursor94.cur \
itk/win/rc/cursor96.cur itk/win/rc/cursor98.cur itk/win/rc/itk.ico \
itk/win/rc/itk.rc itk/win/rc/itkwish.rc

 
itcl_iwidgets2.2.0_dir = iwidgets2.2.0/Makefile.in iwidgets2.2.0/CHANGES iwidgets2.2.0/aclocal.m4 \
iwidgets2.2.0/README iwidgets2.2.0/configure iwidgets2.2.0/configure.in \
iwidgets2.2.0/iwidgets.tcl.in iwidgets2.2.0/license.terms iwidgets2.2.0/pkgIndex.tcl.in
 
 
itcl_iwidgets2.2.0_demos_dir = iwidgets2.2.0/demos/buttonbox iwidgets2.2.0/demos/canvasprintdialog iwidgets2.2.0/demos/catalog \
iwidgets2.2.0/demos/combobox iwidgets2.2.0/demos/demo.html iwidgets2.2.0/demos/dialog \
iwidgets2.2.0/demos/dialogshell iwidgets2.2.0/demos/entryfield iwidgets2.2.0/demos/feedback \
iwidgets2.2.0/demos/fileselectionbox iwidgets2.2.0/demos/fileselectiondialog iwidgets2.2.0/demos/hyperhelp \
iwidgets2.2.0/demos/iwidgets.gif iwidgets2.2.0/demos/labeledwidget iwidgets2.2.0/demos/menubar \
iwidgets2.2.0/demos/messagedialog iwidgets2.2.0/demos/notebook iwidgets2.2.0/demos/optionmenu \
iwidgets2.2.0/demos/panedwindow iwidgets2.2.0/demos/promptdialog iwidgets2.2.0/demos/pushbutton \
iwidgets2.2.0/demos/radiobox iwidgets2.2.0/demos/scrolledcanvas iwidgets2.2.0/demos/scrolledframe \
iwidgets2.2.0/demos/scrolledhtml iwidgets2.2.0/demos/scrolledlistbox iwidgets2.2.0/demos/scrolledtext \
iwidgets2.2.0/demos/selectionbox iwidgets2.2.0/demos/selectiondialog iwidgets2.2.0/demos/spindate \
iwidgets2.2.0/demos/spinint iwidgets2.2.0/demos/spinner iwidgets2.2.0/demos/spintime \
iwidgets2.2.0/demos/tabnotebook iwidgets2.2.0/demos/tabset iwidgets2.2.0/demos/toolbar
 
 
itcl_iwidgets2.2.0_demos_html_dir = iwidgets2.2.0/demos/html/buttonbox.n.html iwidgets2.2.0/demos/html/canvasprintbox.n.html iwidgets2.2.0/demos/html/canvasprintdialog.n.html \
iwidgets2.2.0/demos/html/combobox.n.html iwidgets2.2.0/demos/html/dialog.n.html iwidgets2.2.0/demos/html/dialogshell.n.html \
iwidgets2.2.0/demos/html/entryfield.n.html iwidgets2.2.0/demos/html/feedback.n.html iwidgets2.2.0/demos/html/fileselectionbox.n.html \
iwidgets2.2.0/demos/html/fileselectiondialog.n.html iwidgets2.2.0/demos/html/hyperhelp.n.html iwidgets2.2.0/demos/html/iwidgets2.2.0UserCmds.html \
iwidgets2.2.0/demos/html/labeledwidget.n.html iwidgets2.2.0/demos/html/menubar.n.html iwidgets2.2.0/demos/html/messagedialog.n.html \
iwidgets2.2.0/demos/html/notebook.n.html iwidgets2.2.0/demos/html/optionmenu.n.html iwidgets2.2.0/demos/html/panedwindow.n.html \
iwidgets2.2.0/demos/html/promptdialog.n.html iwidgets2.2.0/demos/html/pushbutton.n.html iwidgets2.2.0/demos/html/radiobox.n.html \
iwidgets2.2.0/demos/html/scrolledcanvas.n.html iwidgets2.2.0/demos/html/scrolledframe.n.html iwidgets2.2.0/demos/html/scrolledhtml.n.html \
iwidgets2.2.0/demos/html/scrolledlistbox.n.html iwidgets2.2.0/demos/html/scrolledtext.n.html iwidgets2.2.0/demos/html/selectionbox.n.html \
iwidgets2.2.0/demos/html/selectiondialog.n.html iwidgets2.2.0/demos/html/shell.n.html iwidgets2.2.0/demos/html/spindate.n.html \
iwidgets2.2.0/demos/html/spinint.n.html iwidgets2.2.0/demos/html/spinner.n.html iwidgets2.2.0/demos/html/spintime.n.html \
iwidgets2.2.0/demos/html/tabnotebook.n.html iwidgets2.2.0/demos/html/tabset.n.html iwidgets2.2.0/demos/html/toolbar.n.html
 
 
itcl_iwidgets2.2.0_demos_images_dir = iwidgets2.2.0/demos/images/poly.gif iwidgets2.2.0/demos/images/mag.gif iwidgets2.2.0/demos/images/ruler.gif \
iwidgets2.2.0/demos/images/select.gif  
 
itcl_iwidgets2.2.0_doc_dir = iwidgets2.2.0/doc/buttonbox.n iwidgets2.2.0/doc/canvasprintbox.n iwidgets2.2.0/doc/canvasprintdialog.n \
iwidgets2.2.0/doc/combobox.n iwidgets2.2.0/doc/dialog.n iwidgets2.2.0/doc/dialogshell.n \
iwidgets2.2.0/doc/entryfield.n iwidgets2.2.0/doc/feedback.n iwidgets2.2.0/doc/fileselectionbox.n \
iwidgets2.2.0/doc/fileselectiondialog.n iwidgets2.2.0/doc/hyperhelp.n iwidgets2.2.0/doc/iwidgets.ps \
iwidgets2.2.0/doc/labeledwidget.n iwidgets2.2.0/doc/man.macros iwidgets2.2.0/doc/menubar.n \
iwidgets2.2.0/doc/messagedialog.n iwidgets2.2.0/doc/mkitclman iwidgets2.2.0/doc/notebook.n \
iwidgets2.2.0/doc/optionmenu.n iwidgets2.2.0/doc/panedwindow.n iwidgets2.2.0/doc/promptdialog.n \
iwidgets2.2.0/doc/pushbutton.n iwidgets2.2.0/doc/radiobox.n iwidgets2.2.0/doc/scrolledcanvas.n \
iwidgets2.2.0/doc/scrolledframe.n iwidgets2.2.0/doc/scrolledhtml.n iwidgets2.2.0/doc/scrolledlistbox.n \
iwidgets2.2.0/doc/scrolledtext.n iwidgets2.2.0/doc/selectionbox.n iwidgets2.2.0/doc/selectiondialog.n \
iwidgets2.2.0/doc/shell.n iwidgets2.2.0/doc/spindate.n iwidgets2.2.0/doc/spinint.n \
iwidgets2.2.0/doc/spinner.n iwidgets2.2.0/doc/spintime.n iwidgets2.2.0/doc/tabnotebook.n \
iwidgets2.2.0/doc/tabset.n iwidgets2.2.0/doc/tk2html iwidgets2.2.0/doc/tk2html.awk \
iwidgets2.2.0/doc/tk2html.perl iwidgets2.2.0/doc/tk2html2.awk iwidgets2.2.0/doc/toolbar.n
 
 
itcl_iwidgets2.2.0_generic_dir = iwidgets2.2.0/generic/buttonbox.itk iwidgets2.2.0/generic/canvasprintbox.itk iwidgets2.2.0/generic/canvasprintdialog.itk \
iwidgets2.2.0/generic/colors.itk iwidgets2.2.0/generic/combobox.itk iwidgets2.2.0/generic/dialog.itk \
iwidgets2.2.0/generic/dialogshell.itk iwidgets2.2.0/generic/entryfield.itk iwidgets2.2.0/generic/feedback.itk \
iwidgets2.2.0/generic/fileselectionbox.itk iwidgets2.2.0/generic/fileselectiondialog.itk iwidgets2.2.0/generic/hyperhelp.itk \
iwidgets2.2.0/generic/labeledwidget.itk iwidgets2.2.0/generic/menubar.itk iwidgets2.2.0/generic/messagedialog.itk \
iwidgets2.2.0/generic/notebook.itk iwidgets2.2.0/generic/optionmenu.itk iwidgets2.2.0/generic/pane.itk \
iwidgets2.2.0/generic/panedwindow.itk iwidgets2.2.0/generic/promptdialog.itk iwidgets2.2.0/generic/pushbutton.itk \
iwidgets2.2.0/generic/radiobox.itk iwidgets2.2.0/generic/scrolledcanvas.itk iwidgets2.2.0/generic/scrolledframe.itk \
iwidgets2.2.0/generic/scrolledhtml.itk iwidgets2.2.0/generic/scrolledlistbox.itk iwidgets2.2.0/generic/scrolledtext.itk \
iwidgets2.2.0/generic/selectionbox.itk iwidgets2.2.0/generic/selectiondialog.itk iwidgets2.2.0/generic/shell.itk \
iwidgets2.2.0/generic/spindate.itk iwidgets2.2.0/generic/spinint.itk iwidgets2.2.0/generic/spinner.itk \
iwidgets2.2.0/generic/spintime.itk iwidgets2.2.0/generic/tabnotebook.itk iwidgets2.2.0/generic/tabset.itk \
iwidgets2.2.0/generic/tclIndex iwidgets2.2.0/generic/toolbar.itk iwidgets2.2.0/generic/unknownimage.gif
 
 
itcl_iwidgets2.2.0_incoming_dir = iwidgets2.2.0/incoming/README  
 
itcl_iwidgets2.2.0_incoming_doc_dir = itcl_iwidgets2.2.0/incoming/doc/man.macros  
 
itcl_iwidgets2.2.0_incoming_tests_dir = iwidgets2.2.0/incoming/tests/defs iwidgets2.2.0/incoming/tests/all

 
itcl_iwidgets2.2.0_mac_dir = iwidgets2.2.0/mac/Build_Iwidgets.rsrc iwidgets2.2.0/mac/iwidgets.tcl iwidgets2.2.0/mac/iwidgetsResources.r \
iwidgets2.2.0/mac/pkgIndex.tcl iwidgets2.2.0/mac/tclIndex

 
itcl_iwidgets2.2.0_outgoing_dir = iwidgets2.2.0/outgoing/README  
 
itcl_iwidgets2.2.0_tests_dir = iwidgets2.2.0/tests/defs iwidgets2.2.0/tests/all iwidgets2.2.0/tests/buttonbox.test \
iwidgets2.2.0/tests/canvasprintbox.test iwidgets2.2.0/tests/canvasprintdialog.test iwidgets2.2.0/tests/combobox.test \
iwidgets2.2.0/tests/dialog.test iwidgets2.2.0/tests/dialogshell.test iwidgets2.2.0/tests/entryfield.test \
iwidgets2.2.0/tests/feedback.test iwidgets2.2.0/tests/fileselectionbox.test iwidgets2.2.0/tests/fileselectiondialog.test \
iwidgets2.2.0/tests/hyperhelp.html iwidgets2.2.0/tests/hyperhelp.test iwidgets2.2.0/tests/labeledwidget.test \
iwidgets2.2.0/tests/menubar.test iwidgets2.2.0/tests/messagedialog.test iwidgets2.2.0/tests/notebook.test \
iwidgets2.2.0/tests/optionmenu.test iwidgets2.2.0/tests/panedwindow.test iwidgets2.2.0/tests/promptdialog.test \
iwidgets2.2.0/tests/pushbutton.test iwidgets2.2.0/tests/radiobox.test iwidgets2.2.0/tests/scrolledcanvas.test \
iwidgets2.2.0/tests/scrolledframe.test iwidgets2.2.0/tests/scrolledhtml.test iwidgets2.2.0/tests/scrolledlistbox.test \
iwidgets2.2.0/tests/scrolledtext.test iwidgets2.2.0/tests/selectionbox.test iwidgets2.2.0/tests/selectiondialog.test \
iwidgets2.2.0/tests/shell.test iwidgets2.2.0/tests/spindate.test iwidgets2.2.0/tests/spinint.test \
iwidgets2.2.0/tests/spinner.test iwidgets2.2.0/tests/spintime.test iwidgets2.2.0/tests/tabnotebook.test \
iwidgets2.2.0/tests/tabset.test iwidgets2.2.0/tests/toolbar.test iwidgets2.2.0/tests/usual.test
 
 
itcl_iwidgets2.2.0_unix_dir = iwidgets2.2.0/unix/Makefile.in iwidgets2.2.0/unix/configure iwidgets2.2.0/unix/configure.in \
iwidgets2.2.0/unix/iwidgets.tcl.in iwidgets2.2.0/unix/pkgIndex.tcl.in

 
itcl_iwidgets2.2.0_win_dir = iwidgets2.2.0/win/catalog.bat iwidgets2.2.0/win/iwidgets.tcl iwidgets2.2.0/win/makefile.bc \
iwidgets2.2.0/win/makefile.vc iwidgets2.2.0/win/pkgIndex.tcl iwidgets2.2.0/win/tclIndex
 
 
itcl_iwidgets3.0.0_dir = iwidgets3.0.0/Makefile.in iwidgets3.0.0/CHANGES iwidgets3.0.0/aclocal.m4 \
iwidgets3.0.0/README iwidgets3.0.0/configure iwidgets3.0.0/configure.in \
iwidgets3.0.0/iwidgets.tcl.in iwidgets3.0.0/license.terms iwidgets3.0.0/pkgIndex.tcl.in
 
 
itcl_iwidgets3.0.0_demos_dir = iwidgets3.0.0/demos/buttonbox iwidgets3.0.0/demos/calendar iwidgets3.0.0/demos/canvasprintbox \
iwidgets3.0.0/demos/canvasprintdialog iwidgets3.0.0/demos/catalog iwidgets3.0.0/demos/checkbox \
iwidgets3.0.0/demos/combobox iwidgets3.0.0/demos/dateentry iwidgets3.0.0/demos/datefield \
iwidgets3.0.0/demos/demo.html iwidgets3.0.0/demos/dialog iwidgets3.0.0/demos/dialogshell \
iwidgets3.0.0/demos/disjointlistbox iwidgets3.0.0/demos/entryfield iwidgets3.0.0/demos/extfileselectionbox \
iwidgets3.0.0/demos/extfileselectiondialog iwidgets3.0.0/demos/feedback iwidgets3.0.0/demos/fileselectionbox \
iwidgets3.0.0/demos/fileselectiondialog iwidgets3.0.0/demos/finddialog iwidgets3.0.0/demos/hierarchy \
iwidgets3.0.0/demos/hyperhelp iwidgets3.0.0/demos/iwidgets.gif iwidgets3.0.0/demos/labeledframe \
iwidgets3.0.0/demos/labeledwidget iwidgets3.0.0/demos/mainwindow iwidgets3.0.0/demos/menubar \
iwidgets3.0.0/demos/messagebox iwidgets3.0.0/demos/messagedialog iwidgets3.0.0/demos/notebook \
iwidgets3.0.0/demos/optionmenu iwidgets3.0.0/demos/panedwindow iwidgets3.0.0/demos/promptdialog \
iwidgets3.0.0/demos/pushbutton iwidgets3.0.0/demos/radiobox iwidgets3.0.0/demos/scopedobject \
iwidgets3.0.0/demos/scrolledcanvas iwidgets3.0.0/demos/scrolledframe iwidgets3.0.0/demos/scrolledhtml \
iwidgets3.0.0/demos/scrolledlistbox iwidgets3.0.0/demos/scrolledtext iwidgets3.0.0/demos/selectionbox \
iwidgets3.0.0/demos/selectiondialog iwidgets3.0.0/demos/shell iwidgets3.0.0/demos/spindate \
iwidgets3.0.0/demos/spinint iwidgets3.0.0/demos/spinner iwidgets3.0.0/demos/spintime \
iwidgets3.0.0/demos/tabnotebook iwidgets3.0.0/demos/tabset iwidgets3.0.0/demos/timeentry \
iwidgets3.0.0/demos/timefield iwidgets3.0.0/demos/toolbar iwidgets3.0.0/demos/watch
 
 
itcl_iwidgets3.0.0_demos_html_dir = iwidgets3.0.0/demos/html/buttonbox.n.html iwidgets3.0.0/demos/html/calendar.n.html iwidgets3.0.0/demos/html/canvasprintbox.n.html \
iwidgets3.0.0/demos/html/canvasprintdialog.n.html iwidgets3.0.0/demos/html/checkbox.n.html iwidgets3.0.0/demos/html/combobox.n.html \
iwidgets3.0.0/demos/html/dateentry.n.html iwidgets3.0.0/demos/html/datefield.n.html iwidgets3.0.0/demos/html/dialog.n.html \
iwidgets3.0.0/demos/html/dialogshell.n.html iwidgets3.0.0/demos/html/disjointlistbox.n.html iwidgets3.0.0/demos/html/entryfield.n.html \
iwidgets3.0.0/demos/html/extfileselectionbox.n.html iwidgets3.0.0/demos/html/extfileselectiondialog.n.html iwidgets3.0.0/demos/html/feedback.n.html \
iwidgets3.0.0/demos/html/fileselectionbox.n.html iwidgets3.0.0/demos/html/fileselectiondialog.n.html iwidgets3.0.0/demos/html/finddialog.n.html \
iwidgets3.0.0/demos/html/hierarchy.n.html iwidgets3.0.0/demos/html/hyperhelp.n.html iwidgets3.0.0/demos/html/iwidgets3.0.0UserCmds.html \
iwidgets3.0.0/demos/html/labeledframe.n.html iwidgets3.0.0/demos/html/labeledwidget.n.html iwidgets3.0.0/demos/html/mainwindow.n.html \
iwidgets3.0.0/demos/html/menubar.n.html iwidgets3.0.0/demos/html/messagebox.n.html iwidgets3.0.0/demos/html/messagedialog.n.html \
iwidgets3.0.0/demos/html/notebook.n.html iwidgets3.0.0/demos/html/optionmenu.n.html iwidgets3.0.0/demos/html/panedwindow.n.html \
iwidgets3.0.0/demos/html/promptdialog.n.html iwidgets3.0.0/demos/html/pushbutton.n.html iwidgets3.0.0/demos/html/radiobox.n.html \
iwidgets3.0.0/demos/html/scopedobject.n.html iwidgets3.0.0/demos/html/scrolledcanvas.n.html iwidgets3.0.0/demos/html/scrolledframe.n.html \
iwidgets3.0.0/demos/html/scrolledhtml.n.html iwidgets3.0.0/demos/html/scrolledlistbox.n.html iwidgets3.0.0/demos/html/scrolledtext.n.html \
iwidgets3.0.0/demos/html/selectionbox.n.html iwidgets3.0.0/demos/html/selectiondialog.n.html iwidgets3.0.0/demos/html/shell.n.html \
iwidgets3.0.0/demos/html/spindate.n.html iwidgets3.0.0/demos/html/spinint.n.html iwidgets3.0.0/demos/html/spinner.n.html \
iwidgets3.0.0/demos/html/spintime.n.html iwidgets3.0.0/demos/html/tabnotebook.n.html iwidgets3.0.0/demos/html/tabset.n.html \
iwidgets3.0.0/demos/html/timeentry.n.html iwidgets3.0.0/demos/html/timefield.n.html iwidgets3.0.0/demos/html/toolbar.n.html \
iwidgets3.0.0/demos/html/watch.n.html  
 
itcl_iwidgets3.0.0_demos_images_dir = iwidgets3.0.0/demos/images/clear.gif iwidgets3.0.0/demos/images/box.xbm iwidgets3.0.0/demos/images/close.gif \
iwidgets3.0.0/demos/images/copy.gif iwidgets3.0.0/demos/images/cut.gif iwidgets3.0.0/demos/images/exit.gif \
iwidgets3.0.0/demos/images/find.gif iwidgets3.0.0/demos/images/help.gif iwidgets3.0.0/demos/images/line.xbm \
iwidgets3.0.0/demos/images/mag.gif iwidgets3.0.0/demos/images/new.gif iwidgets3.0.0/demos/images/open.gif \
iwidgets3.0.0/demos/images/oval.xbm iwidgets3.0.0/demos/images/paste.gif iwidgets3.0.0/demos/images/points.xbm \
iwidgets3.0.0/demos/images/poly.gif iwidgets3.0.0/demos/images/print.gif iwidgets3.0.0/demos/images/ruler.gif \
iwidgets3.0.0/demos/images/save.gif iwidgets3.0.0/demos/images/select.gif iwidgets3.0.0/demos/images/text.xbm
 
 
itcl_iwidgets3.0.0_doc_dir = iwidgets3.0.0/doc/buttonbox.n iwidgets3.0.0/doc/calendar.n iwidgets3.0.0/doc/canvasprintbox.n \
iwidgets3.0.0/doc/canvasprintdialog.n iwidgets3.0.0/doc/checkbox.n iwidgets3.0.0/doc/combobox.n \
iwidgets3.0.0/doc/dateentry.n iwidgets3.0.0/doc/datefield.n iwidgets3.0.0/doc/dialog.n \
iwidgets3.0.0/doc/dialogshell.n iwidgets3.0.0/doc/disjointlistbox.n iwidgets3.0.0/doc/entryfield.n \
iwidgets3.0.0/doc/extfileselectionbox.n iwidgets3.0.0/doc/extfileselectiondialog.n iwidgets3.0.0/doc/feedback.n \
iwidgets3.0.0/doc/fileselectionbox.n iwidgets3.0.0/doc/fileselectiondialog.n iwidgets3.0.0/doc/finddialog.n \
iwidgets3.0.0/doc/generateHtml.sh iwidgets3.0.0/doc/hierarchy.n iwidgets3.0.0/doc/hyperhelp.n \
iwidgets3.0.0/doc/iwidgets.ps iwidgets3.0.0/doc/labeledframe.n iwidgets3.0.0/doc/labeledwidget.n \
iwidgets3.0.0/doc/mainwindow.n iwidgets3.0.0/doc/man.macros iwidgets3.0.0/doc/menubar.n \
iwidgets3.0.0/doc/messagebox.n iwidgets3.0.0/doc/messagedialog.n iwidgets3.0.0/doc/mkitclman \
iwidgets3.0.0/doc/notebook.n iwidgets3.0.0/doc/optionmenu.n iwidgets3.0.0/doc/panedwindow.n \
iwidgets3.0.0/doc/promptdialog.n iwidgets3.0.0/doc/pushbutton.n iwidgets3.0.0/doc/radiobox.n \
iwidgets3.0.0/doc/scopedobject.n iwidgets3.0.0/doc/scopedobject.n.backup iwidgets3.0.0/doc/scrolledcanvas.n \
iwidgets3.0.0/doc/scrolledframe.n iwidgets3.0.0/doc/scrolledhtml.n iwidgets3.0.0/doc/scrolledlistbox.n \
iwidgets3.0.0/doc/scrolledtext.n iwidgets3.0.0/doc/selectionbox.n iwidgets3.0.0/doc/selectiondialog.n \
iwidgets3.0.0/doc/shell.n iwidgets3.0.0/doc/spindate.n iwidgets3.0.0/doc/spinint.n \
iwidgets3.0.0/doc/spinner.n iwidgets3.0.0/doc/spintime.n iwidgets3.0.0/doc/tabnotebook.n \
iwidgets3.0.0/doc/tabset.n iwidgets3.0.0/doc/timeentry.n iwidgets3.0.0/doc/timefield.n \
iwidgets3.0.0/doc/tk2html iwidgets3.0.0/doc/tk2html.awk iwidgets3.0.0/doc/tk2html.perl \
iwidgets3.0.0/doc/tk2html2.awk iwidgets3.0.0/doc/toolbar.n iwidgets3.0.0/doc/watch.n
 
 
itcl_iwidgets3.0.0_generic_dir = iwidgets3.0.0/generic/buttonbox.itk iwidgets3.0.0/generic/calendar.itk iwidgets3.0.0/generic/canvasprintbox.itk \
iwidgets3.0.0/generic/canvasprintdialog.itk iwidgets3.0.0/generic/checkbox.itk iwidgets3.0.0/generic/colors.itcl \
iwidgets3.0.0/generic/combobox.itk iwidgets3.0.0/generic/dateentry.itk iwidgets3.0.0/generic/datefield.itk \
iwidgets3.0.0/generic/dialog.itk iwidgets3.0.0/generic/dialogshell.itk iwidgets3.0.0/generic/disjointlistbox.itk \
iwidgets3.0.0/generic/entryfield.itk iwidgets3.0.0/generic/extfileselectionbox.itk iwidgets3.0.0/generic/extfileselectiondialog.itk \
iwidgets3.0.0/generic/feedback.itk iwidgets3.0.0/generic/fileselectionbox.itk iwidgets3.0.0/generic/fileselectiondialog.itk \
iwidgets3.0.0/generic/finddialog.itk iwidgets3.0.0/generic/hierarchy.itk iwidgets3.0.0/generic/hyperhelp.itk \
iwidgets3.0.0/generic/labeledframe.itk iwidgets3.0.0/generic/labeledwidget.itk iwidgets3.0.0/generic/mainwindow.itk \
iwidgets3.0.0/generic/menubar.itk iwidgets3.0.0/generic/messagebox.itk iwidgets3.0.0/generic/messagedialog.itk \
iwidgets3.0.0/generic/notebook.itk iwidgets3.0.0/generic/optionmenu.itk iwidgets3.0.0/generic/pane.itk \
iwidgets3.0.0/generic/panedwindow.itk iwidgets3.0.0/generic/promptdialog.itk iwidgets3.0.0/generic/pushbutton.itk \
iwidgets3.0.0/generic/radiobox.itk iwidgets3.0.0/generic/regexpfield.itk iwidgets3.0.0/generic/roman.itcl \
iwidgets3.0.0/generic/scopedobject.itcl iwidgets3.0.0/generic/scopedobject.tcl iwidgets3.0.0/generic/scrolledcanvas.itk \
iwidgets3.0.0/generic/scrolledframe.itk iwidgets3.0.0/generic/scrolledhtml.itk iwidgets3.0.0/generic/scrolledlistbox.itk \
iwidgets3.0.0/generic/scrolledtext.itk iwidgets3.0.0/generic/scrolledwidget.itk iwidgets3.0.0/generic/selectionbox.itk \
iwidgets3.0.0/generic/selectiondialog.itk iwidgets3.0.0/generic/shell.itk iwidgets3.0.0/generic/spindate.itk \
iwidgets3.0.0/generic/spinint.itk iwidgets3.0.0/generic/spinner.itk iwidgets3.0.0/generic/spintime.itk \
iwidgets3.0.0/generic/tabnotebook.itk iwidgets3.0.0/generic/tabset.itk iwidgets3.0.0/generic/tclIndex \
iwidgets3.0.0/generic/timeentry.itk iwidgets3.0.0/generic/timefield.itk iwidgets3.0.0/generic/toolbar.itk \
iwidgets3.0.0/generic/unknownimage.gif iwidgets3.0.0/generic/watch.itk

 
itcl_iwidgets3.0.0_incoming_dir = iwidgets3.0.0/incoming/README  
 
itcl_iwidgets3.0.0_incoming_doc_dir = iwidgets3.0.0/incoming/doc/man.macros  
 
itcl_iwidgets3.0.0_incoming_tests_dir = iwidgets3.0.0/incoming/tests/defs iwidgets3.0.0/incoming/tests/all

 
itcl_iwidgets3.0.0_outgoing_dir = iwidgets3.0.0/outgoing/README  
 
itcl_iwidgets3.0.0_tests_dir = iwidgets3.0.0/tests/defs iwidgets3.0.0/tests/all iwidgets3.0.0/tests/buttonbox.test \
iwidgets3.0.0/tests/calendar.test iwidgets3.0.0/tests/canvasprintbox.test iwidgets3.0.0/tests/canvasprintdialog.test \
iwidgets3.0.0/tests/checkbox.test iwidgets3.0.0/tests/combobox.test iwidgets3.0.0/tests/dateentry.test \
iwidgets3.0.0/tests/datefield.test iwidgets3.0.0/tests/dialog.test iwidgets3.0.0/tests/dialogshell.test \
iwidgets3.0.0/tests/disjointlistbox.test iwidgets3.0.0/tests/entryfield.test iwidgets3.0.0/tests/extfileselectionbox.test \
iwidgets3.0.0/tests/extfileselectiondialog.test iwidgets3.0.0/tests/feedback.test iwidgets3.0.0/tests/fileselectionbox.test \
iwidgets3.0.0/tests/fileselectiondialog.test iwidgets3.0.0/tests/finddialog.test iwidgets3.0.0/tests/hierarchy.test \
iwidgets3.0.0/tests/hyperhelp.html iwidgets3.0.0/tests/hyperhelp.test iwidgets3.0.0/tests/labeledframe.test \
iwidgets3.0.0/tests/labeledwidget.test iwidgets3.0.0/tests/menubar.test iwidgets3.0.0/tests/messagebox.test \
iwidgets3.0.0/tests/messagedialog.test iwidgets3.0.0/tests/notebook.test iwidgets3.0.0/tests/optionmenu.test \
iwidgets3.0.0/tests/panedwindow.test iwidgets3.0.0/tests/promptdialog.test iwidgets3.0.0/tests/pushbutton.test \
iwidgets3.0.0/tests/radiobox.test iwidgets3.0.0/tests/regexpfield.test iwidgets3.0.0/tests/scrolledcanvas.test \
iwidgets3.0.0/tests/scrolledframe.test iwidgets3.0.0/tests/scrolledhtml.test iwidgets3.0.0/tests/scrolledlistbox.test \
iwidgets3.0.0/tests/scrolledtext.test iwidgets3.0.0/tests/selectionbox.test iwidgets3.0.0/tests/selectiondialog.test \
iwidgets3.0.0/tests/shell.test iwidgets3.0.0/tests/spindate.test iwidgets3.0.0/tests/spinint.test \
iwidgets3.0.0/tests/spinner.test iwidgets3.0.0/tests/spintime.test iwidgets3.0.0/tests/tabnotebook.test \
iwidgets3.0.0/tests/tabset.test iwidgets3.0.0/tests/timeentry.test iwidgets3.0.0/tests/timefield.test \
iwidgets3.0.0/tests/toolbar.test iwidgets3.0.0/tests/usual.test iwidgets3.0.0/tests/watch.test
 
 
itcl_iwidgets3.0.0_unix_dir = iwidgets3.0.0/unix/Makefile.in iwidgets3.0.0/unix/configure iwidgets3.0.0/unix/configure.in \
iwidgets3.0.0/unix/install-sh iwidgets3.0.0/unix/iwidgets.tcl.in iwidgets3.0.0/unix/pkgIndex.tcl.in
 
 
itcl_iwidgets3.0.0_win_dir = iwidgets3.0.0/win/iwidgets.tcl iwidgets3.0.0/win/makefile.vc iwidgets3.0.0/win/pkgIndex.tcl
 
 
itcl_tools_dir = tools/genWinhelp.cmd tools/incr30.hpj tools/index.tcl \
tools/man2help.tcl tools/man2help2.tcl tools/man2tcl.c


EXTRA_DIST = tcl.m4 makefile.bc makefile.vc $(itcl_config_dir) \
             $(itcl_doc_dir) $(itcl_itcl_dir) $(itcl_itcl_doc_dir) \
             $(itcl_itcl_mac_dir) $(itcl_itcl_generic_dir) $(itcl_itcl_library_dir) \
             $(itcl_itcl_tests_old_dir) $(itcl_itcl_tests_old_toasters_dir) \
             $(itcl_itcl_unix_dir) $(itcl_itcl_win_dir) $(itcl_itk_dir) \
             $(itcl_itk_demos_dir) $(itcl_itk_doc_dir) $(itcl_itk_examples_dir) \
             $(itcl_itk_generic_dir) $(itcl_itk_library_dir) $(itcl_itk_mac_dir) \
             $(itcl_itk_tests_dir) $(itcl_itk_unix_dir) $(itcl_itk_win_dir) \
             $(itcl_itk_win_rc_dir) $(itcl_iwidgets2.2.0_dir) $(itcl_iwidgets2.2.0_demos_dir) \
             $(itcl_iwidgets2.2.0_demos_html_dir) $(itcl_iwidgets2.2.0_demos_images_dir) \
             $(itcl_iwidgets2.2.0_doc_dir) $(itcl_iwidgets2.2.0_generic_dir) \
             $(itcl_iwidgets2.2.0_incoming_dir) $(itcl_iwidgets2.2.0_incoming_doc_dir) \
             $(itcl_iwidgets2.2.0_incoming_tests_dir) $(itcl_iwidgets2.2.0_mac_dir) \
             $(itcl_iwidgets2.2.0_outgoing_dir) $(itcl_iwidgets2.2.0_tests_dir) \
             $(itcl_iwidgets2.2.0_unix_dir) $(itcl_iwidgets2.2.0_win_dir) \
             $(itcl_iwidgets3.0.0_dir) $(itcl_iwidgets3.0.0_demos_dir) \
             $(itcl_iwidgets3.0.0_demos_html_dir) $(itcl_iwidgets3.0.0_demos_images_dir) \
             $(itcl_iwidgets3.0.0_doc_dir) $(itcl_iwidgets3.0.0_generic_dir) \
             $(itcl_iwidgets3.0.0_incoming_dir) $(itcl_iwidgets3.0.0_incoming_doc_dir) \
             $(itcl_iwidgets3.0.0_incoming_tests_dir) $(itcl_iwidgets3.0.0_outgoing_dir) \
             $(itcl_iwidgets3.0.0_tests_dir) $(itcl_iwidgets3.0.0_unix_dir) \
             $(itcl_iwidgets3.0.0_win_dir) $(itcl_tools_dir)


dist_pkgdata_DATA = license.terms INCOMPATIBLE CHANGES
