## Process this file with Automake to create Makefile.in

INCLUDES = @INCLUDES@ -I$(top_srcdir) -I$(top_srcdir)/lib
DEFS = @DEFS@ -DSYSCONFDIR=\"$(sysconfdir)/\"

LIBS = @LIBS@ @CURSES@ @LIBPAM@

AM_CFLAGS = $(PICFLAGS)
AM_LDFLAGS = $(PILDFLAGS)

bin_PROGRAMS = vtysh

vtysh_SOURCES = vtysh_main.c vtysh.c vtysh_user.c vtysh_config.c
nodist_vtysh_SOURCES = vtysh_cmd.c
CLEANFILES = vtysh_cmd.c
noinst_HEADERS = vtysh.h vtysh_user.h
vtysh_LDADD = ../lib/libzebra.la @LIBCAP@ @LIBREADLINE@

examplesdir = $(exampledir)
dist_examples_DATA = vtysh.conf.sample

EXTRA_DIST = extract.pl

vtysh_cmd_FILES = \
  $(top_srcdir)/bgpd/*.c \
  $(top_srcdir)/zebra/*.c

# <+<+<+<+To be added back in when command tables are fixed up. 
#  $(top_srcdir)/isisd/*.c \...
#  $(top_srcdir)/ospfd/*.c \...
#  $(top_srcdir)/ospf6d/*.c \...
#  $(top_srcdir)/ripd/*.c \...
#  $(top_srcdir)/ripngd/*.c \...
# +>+>+>+>

vtysh_cmd.c: $(vtysh_cmd_FILES)
	./$(EXTRA_DIST) $(vtysh_cmd_FILES) > vtysh_cmd.c

