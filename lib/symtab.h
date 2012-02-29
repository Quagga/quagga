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

#include "misc.h"
#include "vector.h"

/*==============================================================================
 * Symbol table definitions
 *
 * Note that count things in uint -- which is expected to be at least 32 bits.
 *
 * Expect to run out of memory before really challenge that assumption !  (At
 * 8 bytes to a pointer, 4G of pointers is already 32G.)
 */
enum
{
  /* Minimum and maximum number of symbol table bases.
   *
   * Something has gone tragically wrong if we hit the maximum !  We assume can
   * multiply the maximum by 2 and get valid uint result.
   */
  SYMBOL_TABLE_BASES_MIN = 50,
  SYMBOL_TABLE_BASES_MAX = UINT_MAX / 2,

  /* Point at which stops doubling the symbol table size (bases)
   */
  SYMBOL_TABLE_BASES_DOUBLE_MAX  = 4000,

  /* Default density
   */
  SYMBOL_TABLE_DEFAULT_DENSITY   = 200,         /* 2.00 entries/base    */

#if 0
  /* LS bit of the reference count is used as symbol is set bit.
   */
  symbol_is_set_bit    = 1,
  symbol_ref_increment = 2,     /* so we count in 2's !                 */
#endif
} ;

/*------------------------------------------------------------------------------
 * Structures defined below.
 */
struct symbol_table ;
struct symbol ;
struct symbol_nref ;

typedef uint32_t symbol_hash_t ;
typedef uint symbol_ref_count_t ;

typedef struct symbol_table*   symbol_table ;
typedef struct symbol_table    symbol_table_t ;

typedef struct symbol*         symbol ;
typedef struct symbol          symbol_t ;

typedef struct symbol_nref*    symbol_nref ;
typedef struct symbol_nref     symbol_nref_t ;

typedef struct symbol_funcs* symbol_funcs ;
typedef struct symbol_funcs  symbol_funcs_t ;
typedef const struct symbol_funcs* symbol_funcs_c ;

typedef struct symbol_walker*    symbol_walker ;
typedef struct symbol_walker     symbol_walker_t ;

/*------------------------------------------------------------------------------
 * Set of functions for a Symbol Table
 */
typedef symbol_hash_t symbol_hash_func(const void* name) ;
typedef int           symbol_cmp_func(const void* body, const void* name) ;
typedef void          symbol_free_func(void* body) ;

struct symbol_funcs
{
  symbol_hash_func* hash ;      /* function to hash given name          */
  symbol_cmp_func*  cmp ;       /* function to compare symbol and name  */
  symbol_free_func* free ;      /* called when symbol is destroyed      */
} ;

/*------------------------------------------------------------------------------
 * Symbol Table.
 *
 * Don't fiddle with this directly... see access functions below.
 */
struct symbol_table
{
  void*    parent ;             /* to identify the table.               */

  symbol*  bases ;              /* ref:array of chain bases             */
  uint	   base_count ;         /* number of chain bases                */

  uint	   entry_count ;        /* number of entries in the table       */
  uint     max_index ;          /* maximum index in the table           */
  uint	   extend_thresh ;      /* when to extend the hash table        */

  float    density ;            /* entries per chain base               */

  symbol_funcs_t func ;         /* the functions, as above              */
} ;

/*------------------------------------------------------------------------------
 * Symbol Table Entry.
 *
 * Don't fiddle with this directly... see access functions below.
 */
struct symbol
{
  symbol_table table ;    /* so can go from symbol to enclosing table
                           * NULL => orphan symbol                      */

  void*    body ;         /* see: symbol_get_body(sym) etc.             */

  symbol   next ;         /* assume chains are short and seldom remove
                           * symbols -- so single pointer will do.      */

  symbol_nref nref_list ; /* list of notify references (if any)         */
  symbol_ref_count_t ref_count ;
                          /* count of references (incl. nref)           */

  symbol_hash_t  hash ;   /* used in lookup and when extending bases.   */
} ;

/* The ref_count actually counts in 2's.  The LS bit is the "symbol_set" bit.
 * While the "symbol_set" bit is set, the reference count is not zero !
 */
enum
{
  symbol_ref_count_increment   = 2,
  symbol_ref_count_symbol_set  = 1
} ;

/*------------------------------------------------------------------------------
 * Symbol Notify Reference (or "bookmark").
 *
 * Don't fiddle with this directly...  see access functions below
 */
struct symbol_nref
{
  symbol      sym ;         /* Address of symbol referred to (if any).
                             * (In "bookmark" this points to self.)     */

  void*	      parent ;      /* In "bookmark" points to symbol           */
  uintptr_t   tag ;

  symbol_nref next ;         /* fellow references to the symbol ...     */
  symbol_nref prev ;         /* ... ignore if sym is NULL.              */
} ;

/* Symbol Walk Iterator
 */
struct symbol_walker
{
  symbol    next ;        /* next symbol to return (if any)         */
  symbol*   base ;        /* next chain base to process (if any)    */
  uint      base_count ;  /* number of chain bases left to process  */
} ;

/*==============================================================================
 * Symbol Table Operations.
 */
extern symbol_table symbol_table_new(void* parent, uint base_count,
                                           uint density, symbol_funcs_c funcs) ;

extern void  symbol_table_set_parent(symbol_table table, void* parent) ;
extern void* symbol_table_get_parent(symbol_table table) ;

extern symbol symbol_table_ream(symbol_table table, symbol sym,
                                                        free_keep_b free_body) ;
extern symbol_table symbol_table_free(symbol_table table,
                                                        free_keep_b free_body) ;
extern void symbol_table_reset(symbol_table table, uint base_count) ;

extern symbol symbol_lookup(symbol_table table, const void* name, add_b add) ;

extern void symbol_set_body(symbol sym, void* body, bool set,
                                                         free_keep_b free_old) ;
Inline void symbol_set(symbol sym) ;
Inline symbol symbol_unset(symbol sym, free_keep_b free_body) ;
extern symbol symbol_delete(symbol sym, free_keep_b free_body) ;

Inline bool symbol_is_set(const symbol sym) ;
Inline bool symbol_has_references(const symbol sym) ;

extern symbol_hash_t symbol_hash_string(const void* string) ;
extern symbol_hash_t symbol_hash_bytes(const void* bytes, size_t len) ;
Inline symbol_hash_t symbol_hash_word(symbol_hash_t h) ;

Inline void* symbol_get_body(const symbol sym) ;
Inline symbol_table symbol_get_table(const symbol sym) ;

Inline symbol symbol_inc_ref(symbol sym) ;
Inline symbol symbol_dec_ref(symbol sym) ;

Private symbol symbol_zero_ref(symbol sym, free_keep_b free_body) ;

extern symbol_nref symbol_nref_init(symbol_nref nref) ;
extern symbol_nref symbol_nref_set(symbol_nref nref, symbol sym) ;
extern symbol_nref symbol_nref_unset(symbol_nref nref, free_keep_b free_it) ;

Inline void* symbol_nref_get_symbol(symbol_nref nref) ;
Inline void* symbol_nref_get_body(symbol_nref nref) ;
Inline void* symbol_nref_get_parent(symbol_nref nref) ;
Inline uintptr_t symbol_nref_get_tag(symbol_nref nref) ;
Inline void symbol_nref_set_parent(symbol_nref nref, void* pa) ;
Inline void symbol_nref_set_tag(symbol_nref nref, uintptr_t tag) ;

extern void symbol_nref_walk_start(symbol sym, symbol_nref walk) ;
extern symbol_nref symbol_nref_walk_step(symbol_nref walk) ;
extern void symbol_nref_walk_end(symbol_nref walk) ;

extern void symbol_walk_start(symbol_table table, symbol_walker walk);
extern symbol symbol_walk_next(symbol_walker walk) ;

typedef int symbol_select_cmp(const symbol sym, const void* name) ;
typedef int symbol_sort_cmp(const symbol* a, const symbol* b) ;
extern vector symbol_table_extract(symbol_table table,
	                           symbol_select_cmp* select,
	                           const void* p_value,
			           bool most,
			           symbol_sort_cmp* sort) ;

extern int symbol_mixed_name_cmp(const char* a, const char* b) ;

/*==============================================================================
 * The Inline stuff.
 */

/*------------------------------------------------------------------------------
 * Access functions -- argument is address of symbol (may be NULL).
 */
Inline void*
symbol_get_body(const symbol sym)
{
  return (sym != NULL) ? sym->body : NULL ;
} ;

Inline symbol_table
symbol_get_table(const symbol sym)
{
  return (sym != NULL) ? sym->table : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Reference count functions.
 */
Inline symbol
symbol_inc_ref(symbol sym)
{
  sym->ref_count += symbol_ref_count_increment ;
  return sym ;
} ;

Inline symbol
symbol_dec_ref(symbol sym)
{
  if (sym->ref_count > symbol_ref_count_increment)
    {
      sym->ref_count -= symbol_ref_count_increment ;
      return NULL ;
    } ;

  qassert(sym->ref_count == symbol_ref_count_increment) ;

  return symbol_zero_ref(sym, free_it) ;        /* returns NULL */
} ;

/*------------------------------------------------------------------------------
 * Set the "symbol_set" state
 */
Inline void
symbol_set(symbol sym)
{
  sym->ref_count |= symbol_ref_count_symbol_set ;
} ;

/*------------------------------------------------------------------------------
 * Clear the "symbol_set" state (if is set)
 *
 * If result is a zero reference count, free the symbol and, if required, the
 * symbol body.
 */
Inline symbol
symbol_unset(symbol sym, free_keep_b free_body)
{
  sym->ref_count &= ~symbol_ref_count_symbol_set ;

  if (sym->ref_count != 0)
    return sym ;

  return symbol_zero_ref(sym, free_body) ;      /* returns NULL */
} ;

/*------------------------------------------------------------------------------
 * Test whether there are any references -- NULL symbol has none (!)
 *
 * Note that for this purpose, the "value_set" state does not count
 */
Inline bool
symbol_has_references(const symbol sym)
{
  return (sym != NULL) ? (sym->ref_count >= symbol_ref_count_increment)
                       : false ;
} ;

/*------------------------------------------------------------------------------
 * Test the "value_set" state -- NULL symbol is not set (!)
 */
Inline bool
symbol_is_set(const symbol sym)
{
  return (sym != NULL) ? (sym->ref_count & symbol_ref_count_symbol_set)
                       : false ;
} ;

/*------------------------------------------------------------------------------
 * Symbol Reference access functions -- argument is address of symbol_ref.
 *
 * These cope if address of symbol_ref is null, or reference is undefined.
 *
 * Note that can cast a uintptr_t to a void* if required.
 */
Inline void*
symbol_nref_get_symbol(symbol_nref nref)
{
  return (nref != NULL) ? nref->sym : NULL ;
} ;

Inline void*
symbol_nref_get_body(symbol_nref nref)
{
  return symbol_get_body(symbol_nref_get_symbol(nref)) ;
} ;

Inline void*
symbol_nref_get_parent(symbol_nref nref)
{
  return (nref != NULL) ? nref->parent : NULL ;
} ;

Inline uintptr_t
symbol_nref_get_tag(symbol_nref nref)
{
  return (nref != NULL) ? nref->tag : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set properties of reference -- argument is address of symbol_ref, which is
 * assumed to NOT be NULL.
 *
 * Note that can cast a void* to a uintptr_t if required.
 */

Inline void
symbol_nref_set_parent(symbol_nref nref, void* pa)
{
  nref->parent = pa ;
} ;

Inline void
symbol_nref_set_tag(symbol_nref nref, uintptr_t tag)
{
  nref->tag = tag ;
} ;

/*------------------------------------------------------------------------------
 * Standard symbol integer hash function.
 *
 * Simple approach -- treat as seed for random number !
 */
Inline symbol_hash_t
symbol_hash_word(symbol_hash_t h)
{
  return (h * 2650845021u) + 5 ;        /* See Knuth 3.3.4      */
} ;

#endif /* _ZEBRA_SYMTAB_H */
