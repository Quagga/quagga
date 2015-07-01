/*
 * rt_names.c		rtnetlink names DB.
 *
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 *
 * Authors:	Alexey Kuznetsov, <kuznet@ms2.inr.ac.ru>
 *
 * Fix:		23 Apr 2005 	Calin Velea <vcalinus@gemenii.ro>
 *		
 *		bgpd-specific fixes
 *		
 *			- Modified rtnl_tab_initialize() function to free allocated entries
 *			before re-reading table; rtnl_rtrealm_initialize() to zero
 *			unused entries at first call
 *			- Modified rtnl_rtrealm_a2n() to read realm table each time; otherwise
 *			bgpd restart was necessary to be in sync with /etc/iproute2/rt_realms
 *		
 * Change:	June 2015	Kaloyan Kovachev
 *		
 *			- Leave only calls we need for realms and fix "discards 'const' qualifier" warnings
 *		
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <fcntl.h>
#include <string.h>
#include <sys/time.h>

#include "zebra.h"

static void rtnl_tab_initialize(const char *file, char **tab, int size)
{
	int i;
	char buf[512];
	FILE *fp;

	for(i = 1; i < 255; i++)
	    if(tab[i]) {
		free(tab[i]);
		tab[i] = NULL;
	    }
		

	fp = fopen(file, "r");
	if (!fp)
		return;
	while (fgets(buf, sizeof(buf), fp)) {
		char *p = buf;
		int id;
		char namebuf[512];

		while (*p == ' ' || *p == '\t')
			p++;
		if (*p == '#' || *p == '\n' || *p == 0)
			continue;
		if (sscanf(p, "0x%x %s\n", &id, namebuf) != 2 &&
		    sscanf(p, "0x%x %s #", &id, namebuf) != 2 &&
		    sscanf(p, "%d %s\n", &id, namebuf) != 2 &&
		    sscanf(p, "%d %s #", &id, namebuf) != 2) {
			fprintf(stderr, "Database %s is corrupted at %s\n",
				file, p);
			return;
		}

		if (id<0 || id>size)
			continue;
		
		tab[id] = strdup(namebuf);
	}
	fclose(fp);
}

static char * rtnl_rtrealm_tab[256];

static int rtnl_rtrealm_init = 0;

static void rtnl_rtrealm_initialize(void)
{
	int i;
	struct timeval now;
	static struct timeval last_init = {0,0};

	gettimeofday(&now, NULL);
	if ( last_init.tv_sec && last_init.tv_sec > now.tv_sec + 60 )
		return;
	gettimeofday(&last_init, NULL);

	if(!rtnl_rtrealm_init)
	    for(i = 0; i < 255; i++)
		rtnl_rtrealm_tab[i] = NULL;	
	
	rtnl_rtrealm_init = 1;
	rtnl_tab_initialize("/etc/iproute2/rt_realms", rtnl_rtrealm_tab, 256);
	if ( !rtnl_rtrealm_tab[0] )
	    rtnl_rtrealm_tab[0] = strdup("unknown");
}

const char * rtnl_rtrealm_n2a(int id, char *buf, int len)
{
	if (id<0 || id>=256) {
		snprintf(buf, len, "%d", id);
		return buf;
	}
	if (!rtnl_rtrealm_tab[id]) {
		if (!rtnl_rtrealm_init)
			rtnl_rtrealm_initialize();
	}
	if (rtnl_rtrealm_tab[id])
		return rtnl_rtrealm_tab[id];
	snprintf(buf, len, "%d", id);
	return buf;
}


int rtnl_rtrealm_a2n(u_int32_t *id, const char *arg)
{
	static char *cache = NULL;
	static unsigned long cache_res;
	unsigned long res;
	char *end;
	int i;

	if (cache && strcmp(cache, arg) == 0) {
		*id = cache_res;
		return 0;
	}
	rtnl_rtrealm_initialize();

	for (i=0; i<256; i++) {
		if (rtnl_rtrealm_tab[i] &&
		    strcmp(rtnl_rtrealm_tab[i], arg) == 0) {
			cache = rtnl_rtrealm_tab[i];
			cache_res = i;
			*id = cache_res;
			return 0;
		}
	}

	res = strtoul(arg, &end, 0);
	if (!end || end == arg || *end || res > 255)
		return -1;
	*id = res;
	return 0;
}
