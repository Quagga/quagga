## Process this file with automake to produce Makefile.in.

INCLUDES = @INCLUDES@ -I.. -I$(top_srcdir) -I$(top_srcdir)/lib @SNMP_INCLUDES@
DEFS = @DEFS@ -DSYSCONFDIR=\"$(sysconfdir)/\"

lib_LTLIBRARIES = libzebra.la
libzebra_la_LDFLAGS = -version-info 0:0:0

libzebra_la_SOURCES = \
	buffer.c  checksum.c  command.c  command_execute.c \
	command_parse.c  command_queue.c  daemon.c  distribute.c \
	elstring.c  errno_names.c  filter.c  getopt.c  getopt1.c \
	hash.c  heap.c  if.c  if_rmap.c  jhash.c  keychain.c  keystroke.c \
	linklist.c  list_util.c  log.c  md5.c  memory.c  memtypes.c  mqueue.c \
	network.c  pid_output.c  plist.c  pqueue.c  prefix.c  privs.c \
	pthread_safe.c  qfstring.c  qiovec.c  qlib_init.c  qpath.c \
	qpnexus.c  qpselect.c  qpthreads.c  qrand.c  qstring.c \
	qtime.c  qtimers.c  routemap.c  sigevent.c  smux.c \
	sockopt.c  sockunion.c  str.c  stream.c  symtab.c \
	table.c  thread.c  vector.c  vio_fifo.c  vio_lines.c \
	vty.c  vty_cli.c  vty_command.c  vty_io.c  vty_io_basic.c \
	vty_io_file.c  vty_io_shell.c  vty_io_term.c  vty_io_vsh.c \
	vty_log.c  workqueue.c  zclient.c
	
BUILT_SOURCES = memtypes.h route_types.h

libzebra_la_DEPENDENCIES = @LIB_REGEX@

libzebra_la_LIBADD = @LIB_REGEX@

pkginclude_HEADERS = \
	command.h  command_common.h  command_execute.h \
	command_local.h  command_parse.h  command_queue.h  confirm.h \
	distribute.h  elstring.h  errno_names.h  filter.h  getopt.h \
	hash.h  heap.h  if.h  if_rmap.h  jhash.h  keychain.h \
	keystroke.h  linklist.h  list_util.h  log.h  log_common.h  log_local.h \
	md5.h  memory.h  memtypes.h  misc.h  miyagi.h  mqueue.h \
	network.h  plist.h  pqueue.h  prefix.h  privs.h \
	pthread_safe.h  qafi_safi.h  qdebug_nb.h  qfstring.h  qiovec.h \
	qlib_init.h  qpath.h  qpnexus.h  qpselect.h  qpthreads.h \
	qrand.h  qstring.h  qtime.h  qtimers.h  route_types.h \
	routemap.h  sigevent.h  smux.h  sockopt.h  sockunion.h  str.h \
	stream.h  symtab.h  table.h  thread.h  tstring.h  vargs.h \
	vector.h  version.h  vio_fifo.h  vio_lines.h  vty.h  vty_cli.h \
	vty_command.h  vty_common.h  vty_io.h  vty_io_basic.h \
	vty_io_file.h  vty_io_shell.h  vty_io_term.h  vty_io_vsh.h \
	vty_local.h  vty_log.h  workqueue.h  zassert.h  zclient.h \
	zconfig.h  zebra.h
		
EXTRA_DIST = regex.c regex-gnu.h memtypes.awk route_types.awk route_types.txt

memtypes.h: $(srcdir)/memtypes.c $(srcdir)/memtypes.awk
	($(GAWK) -f $(srcdir)/memtypes.awk $(srcdir)/memtypes.c > $@)

route_types.h: $(srcdir)/route_types.txt $(srcdir)/route_types.awk
	($(GAWK) -f $(srcdir)/route_types.awk $(srcdir)/route_types.txt > $@)
