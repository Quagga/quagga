/*
  PIM for Quagga
  Copyright (C) 2008  Everton da Silva Marques

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING; if not, write to the
  Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
  MA 02110-1301 USA
  
  $QuaggaId: $Format:%an, %ai, %h$ $
*/

#include "pim_ssmpingd.h"
#include "pim_time.h"
#include "pimd.h"

void pim_ssmpingd_init()
{
}

void pim_ssmpingd_destroy()
{
  if (qpim_ssmpingd_list)
    list_free(qpim_ssmpingd_list);
}

int pim_ssmpingd_start(struct in_addr source_addr)
{
  return 0;
}

int pim_ssmpingd_stop(struct in_addr source_addr)
{
  return 0;
}
