/* Symbol Table data structure -- header
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _ZEBRA_SYMTAB_H
#define _ZEBRA_SYMTAB_H

#include "vector.h"
#include <stddef.h>
#include <stdint.h>

/* Macro in case there are particular compiler issues.    */
#ifndef Inline
  #define Inline static inline
#endif

/* Maximum number of symbol table bases -- something has gone tragically wrong
 * if we hit this.  Assume can multiply this by 2 and get valid size_t result.
 */
#define SYMBOL_TABLE_BASES_MAX	(1024 * 1024 * 1024)

/* Minimum number of symbol table bases.                        */
#define SYMBOL_TABLE_BASES_MIN	         10
/* Point at which stops doubling the symbol table size (bases)  */
#define SYMBOL_TABLE_BASES_DOUBLE_MAX  2000

/* Structures defined below.    */
struct symbol_table ;
struct symbol ;
struct symbol_ref ;
struct symbol_hash ;

typedef struct symbol_table* symbol_table ;
typedef struct symbol*       symbol ;
typedef struct symbol_ref*   symbol_ref ;
typedef struct symbol_hash*  symbol_hash ;

/* Function types used.		*/
typedef void symbol_hash_function(symbol_hash hash, const void* name) ;
typedef void symbol_call_back_function(symbol sym, void* value) ;
typedef void symbol_destroy_function(symbol sym) ;

/* Symbol Table.
 *
 * Don't fiddle with this directly... see access functions below.
 */

struct symbol_table
{
  void* parent ;			/* to identify the table.          */

  symbol*       bases ;                 /* ref:array of chain bases        */
  unsigned int	base_count ;            /* number of chain bases           */

  unsigned int	entry_count ;		/* number of entries in the table  */
  unsigned int	extend_thresh ;		/* when to extend the hash table   */

  symbol_hash_function* hash_function ;	/* function to hash given "name"   */
					/* NULL => use default	           */
  symbol_call_back_function* value_call_back ;
                                        /* called when symbol value is set */
} ;

/* Symbol Table Entry.
 *
 * Don't fiddle with this directly... see access macros/functions below.
 */

struct symbol
{
  symbol_table table ;    /* so can go from symbol to enclosing table   */
                          /* NULL => orphan symbol, with NULL value.    */

  symbol     next ;       /* assume chains are short and seldom remove  */
                          /* symbols -- so single pointer will do.      */

  void*      value ;      /* see: symbol_get_value(sym) etc.            */

  symbol_ref ref_list ;   /* list of symbol_ref references              */
  unsigned   ref_count ;  /* count of simple references                 */

  uint32_t   hash ;       /* used in lookup and when extending bases.   */

  uint16_t   name_len ;	  /* see: symbol_get_name_len(sym)              */
  char       name[] ;     /* see: symbol_get_name(sym)                  */
} ;

/* Symbol Reference (or "bookmark").
 *
 * Don't fiddle with this directly...  see access macros/functions below
 */

typedef union
{
  void*          p ;
  unsigned long  u ;
    signed long  i ;
} symbol_ref_tag_t ;

struct symbol_ref
{
  symbol sym ;              /* Address of symbol referred to (if any).   */
                            /* (In "bookmark" this points to self.)      */

  symbol_ref next ;         /* fellow references to the symbol ...       */
  symbol_ref prev ;         /* ... ignore if sym is NULL.                */

  void*	 parent ;	    /* see: sym_ref_parent(sym_ref) etc.         */
  symbol_ref_tag_t  tag ;   /* see: sym_ref_tag(sym_ref) etc.            */
} ;

/* Result of a hash function for a symbol name.                       */
struct symbol_hash
{
  uint32_t      hash ;          /* the hash value !			      */
  const void*   name ;          /* symbol name as byte vector		      */
  uint16_t      name_len ;      /* length in chars for comparison purposes    */
  uint16_t      name_copy_len ; /* number of chars to copy to store name.     */
} ;

/* Symbol Walk Iterator		*/
struct symbol_walker
{
  symbol    next ;        /* next symbol to return (if any)         */
  symbol*   base ;        /* next chain base to process (if any)    */
  unsigned  base_count ;  /* number of chain bases left to process  */
} ;

/* Symbol Table Operations.	*/

extern symbol_table
symbol_table_init_new(symbol_table table,
		      void* parent,
		      unsigned bases,
		      unsigned density,
		      symbol_hash_function* hash_function,
		      symbol_call_back_function* value_call_back) ;
void symbol_table_set_parent(symbol_table table, void* parent) ;
void* symbol_table_get_parent(symbol_table table) ;
void* symbol_table_ream(symbol_table table, int free_structure) ;
#define symbol_table_ream_free(table) symbol_table_ream(table, 1)
#define symbol_table_ream_keep(table) symbol_table_ream(table, 0)
symbol_table symbol_table_reset(symbol_table table, int free_structure) ;
#define symbol_table_reset_free(table) symbol_table_reset(table, 1)
#define symbol_table_reset_keep(table) symbol_table_reset(table, 0)

extern void symbol_hash_string(struct symbol_hash* p_hash, const char* string) ;
extern void symbol_hash_bytes(struct symbol_hash* p_hash, const void* bytes,
                                                                   size_t len) ;
extern void symbol_table_set_value_call_back(symbol_table table,
				   symbol_call_back_function* value_call_back) ;

extern void symbol_table_free(symbol_table) ;

extern symbol symbol_lookup(symbol_table table, const void* name, int add) ;

#define symbol_seek(table, name) symbol_lookup(table, name, 0)
#define symbol_find(table, name) symbol_lookup(table, name, 1)

extern void* symbol_delete(symbol sym) ;

extern void* symbol_set_value(symbol sym, void* new_value) ;
#define symbol_unset_value(sym) symbol_set_value(sym, NULL)

void symbol_ref_walk_start(symbol sym, symbol_ref walk) ;
symbol_ref symbol_ref_walk_step(symbol_ref walk) ;
void symbol_ref_walk_end(symbol_ref walk) ;

void symbol_walk_start(symbol_table table, struct symbol_walker* walker) ;
symbol symbol_walk_next(struct symbol_walker* walker) ;

typedef int symbol_select_cmp(const symbol, const void*) ;
typedef int symbol_sort_cmp(const symbol*, const symbol*) ;
vector symbol_table_extract(symbol_table table,
			    symbol_select_cmp* select, const void* p_value,
			    int most, symbol_sort_cmp* sort) ;

extern symbol_sort_cmp symbol_mixed_name_cmp ;

/* Access functions -- argument is address of symbol (may be NULL).           */
Inline void*
symbol_get_value(const symbol sym)
{
  return (sym != NULL) ? sym->value : NULL ;
} ;

Inline const void*
symbol_get_name(const symbol sym)
{
  return (sym != NULL) ? sym->name : NULL ;
} ;

Inline unsigned
symbol_get_name_len(const symbol sym)
{
  return (sym != NULL) ? sym->name_len : 0 ;
} ;

Inline struct symbol_table*
symbol_get_table(const symbol sym)
{
  return (sym != NULL) ? sym->table : NULL ;
} ;

Inline symbol
symbol_inc_ref(symbol sym)
{
  ++sym->ref_count ;
  return sym ;
} ;

extern symbol symbol_zero_ref(symbol sym, int force) ;

Inline symbol
symbol_dec_ref(symbol sym)
{
  if (sym->ref_count <= 1)
    return symbol_zero_ref(sym, 0) ;

  --sym->ref_count ;
  return sym ;
} ;

extern symbol_ref
symbol_init_ref(symbol_ref ref) ;

extern symbol_ref
symbol_set_ref(symbol_ref ref, symbol sym) ;

extern symbol_ref
symbol_unset_ref(symbol_ref ref, int free_ref_structure) ;

#define symbol_unset_ref_free(ref) symbol_unset_ref(ref, 1) ;
#define symbol_unset_ref_keep(ref) symbol_unset_ref(ref, 0) ;

/* Access functions -- argument is address of symbol_ref.                     */
/* These cope if address of symbol_ref is null, or reference is undefined.    */
Inline void*
sym_ref_symbol(symbol_ref ref)
{
  return (ref != NULL) ? ref->sym : NULL ;
}
Inline void*
sym_ref_value(symbol_ref ref)
{
  return symbol_get_value(sym_ref_symbol(ref)) ;
}
Inline const void*
sym_ref_name(symbol_ref ref)
{
  return symbol_get_name(sym_ref_symbol(ref)) ;
}
Inline uint16_t
sym_ref_name_len(symbol_ref ref)
{
  return symbol_get_name_len(sym_ref_symbol(ref)) ;
}

Inline void*
sym_ref_parent(symbol_ref ref)
{
  return (ref != NULL) ? ref->parent : NULL ;
}
Inline void*
sym_ref_p_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.p  : NULL ;
}
Inline unsigned long int
sym_ref_u_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.u : 0 ;
}
Inline signed long int
sym_ref_i_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.i : 0 ;
}

/* Set properties of reference -- argument is address of symbol_ref, which is */
/* assumed to NOT be NULL.                                                    */
Inline void
sym_ref_set_parent(symbol_ref ref, void* pa)
{
  ref->parent = pa ;
}
Inline void
sym_ref_set_p_tag(symbol_ref ref, void* p_tag)
{
  ref->tag.p  = p_tag ;
}
Inline void
sym_ref_set_u_tag(symbol_ref ref, unsigned long int u_tag)
{
  ref->tag.u  = u_tag ;
}
Inline void
sym_ref_set_i_tag(symbol_ref ref, signed long int i_tag)
{
  ref->tag.i  = i_tag ;
}

#endif /* _ZEBRA_SYMTAB_H */
