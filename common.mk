# versioned installation directories

verpkglibdir = $(pkglibdir)/@VERSION@
verpkglibexecdir = $(libexecdir)/@PACKAGE@/@VERSION@
verpkgdatadir = $(pkgdatadir)/@VERSION@
docdir = $(verpkgdatadir)/doc
docchmdir = $(docdir)/chm
dochtmldir = $(docdir)/html
docsharedir = $(docdir)/share
demodir = $(verpkgdatadir)/demo
emacsdir = $(EMACSDIR)
sharedir = $(verpkgdatadir)/share
instsrcdir = $(verpkgdatadir)/src
xmaximadir = $(verpkgdatadir)/xmaxima
insttestsdir = $(verpkgdatadir)/tests

# Support for installation of DATA files in a generic directory
# with subdirectories.
# To use, set genericdir to point to the installation directory.
# Set genericdirDATA to hold the list of files to install.
# genericdirDATA may contain subdirectories. Subdirectories will
# be created if necessary.

install-data-local: install-datafiles
install-datafiles: $(genericdirDATA)
       @d=$(DESTDIR)$(genericdir); \
       test -d $$d && $(mkinstalldirs) $$d; \
       list="$^"; for p in $$list; do \
         b=$${p#$(builddir)/}; \
         s=$${p#$(srcdir)/}; \
         if test -f $(builddir)/$$b; then \
           t=`dirname $$d/$$b`; \
            test -d $$t || $(mkinstalldirs) $$t; \
           echo " $(INSTALL_DATA) BUILDDIR/$$b $$d/$$b"; \
           $(INSTALL_DATA) $(builddir)/$$b $$d/$$b; \
         elif test -f $(srcdir)/$$s; then \
           t=`dirname $$d/$$s`; \
            test -d $$t || $(mkinstalldirs) $$t; \
           echo " $(INSTALL_DATA) SRCDIR/$$s $$d/$$s"; \
           $(INSTALL_DATA) $(srcdir)/$$s $$d/$$s; \
         elif test -f $$p; then \
           t=`dirname $$d/$$p`; \
            test -d $$t || $(mkinstalldirs) $$t; \
           echo " $(INSTALL_DATA) $$p $$d/$$p"; \
           $(INSTALL_DATA) $$p $$d/$$p; \
         fi; \
	done

uninstall-local: uninstall-datafiles
uninstall-datafiles:
	@$(NORMAL_UNINSTALL)
	list='$(genericdirDATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(genericdir)/$$p; \
	done
