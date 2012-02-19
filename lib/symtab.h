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

  /* LS bit of the reference count is used as symbol is set bit.
   */
  symbol_is_set_bit    = 1,
  symbol_ref_increment = 2,     /* so we count in 2's !                 */
} ;

/* Structures defined below.
 */
struct symbol_table ;
struct symbol ;
struct symbol_ref ;
struct symbol_default_value ;

typedef uint32_t symbol_hash_t ;
typedef uint symbol_ref_count_t ;

typedef struct symbol_table*     symbol_table ;
typedef struct symbol_table      symbol_table_t ;

typedef struct symbol*           symbol ;
typedef struct symbol            symbol_t ;

typedef struct symbol_ref*       symbol_ref ;
typedef struct symbol_ref        symbol_ref_t ;

typedef struct symbol_funcs* symbol_funcs ;
typedef struct symbol_funcs  symbol_funcs_t ;
typedef const struct symbol_funcs* symbol_funcs_c ;

typedef struct symbol_walker*    symbol_walker ;
typedef struct symbol_walker     symbol_walker_t ;

/* For symbol_tell_func -- what change is being made.                   */
typedef enum
{
  sym_unset,    /* symbol value is being unset          */
  sym_set,      /* symbol value is being set or changed */
  sym_delete    /* symbol is being deleted              */
} symbol_change_t ;

/* Function types used.		*/
typedef symbol_hash_t symbol_hash_func(const void* name) ;
typedef int           symbol_cmp_func(const void* value, const void* name) ;
typedef void          symbol_tell_func(symbol sym, symbol_change_t ch,
                                                              symbol_ref walk) ;
typedef void          symbol_free_func(void* value) ;

/* Set of functions for a Symbol Table                                  */
struct symbol_funcs
{
  symbol_hash_func* hash ;      /* function to hash given name          */
  symbol_cmp_func*  cmp ;       /* function to compare symbol and name  */
  symbol_tell_func* tell ;
                                /* called when symbol value is added,
                                 * changed or deleted.  NULL => none    */
  symbol_free_func* free ;
                                /* called when symbol is destroyed      */
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
 * Don't fiddle with this directly... see access macros/functions below.
 */
struct symbol
{
  symbol_table table ;    /* so can go from symbol to enclosing table
                           * NULL => orphan symbol                      */

  union
  {
    symbol   next ;       /* assume chains are short and seldom remove
                           * symbols -- so single pointer will do.      */

    symbol_free_func* free ;
                          /* when symbol has been orphaned, need to be
                           * able (eventually) to destroy the value.    */
  } u ;

  void*      value ;      /* see: symbol_get_value(sym) etc.            */

  symbol_ref ref_list ;   /* list of symbol_ref references              */
  symbol_ref_count_t ref_count ;
                          /* count of simple references                 */

  symbol_hash_t  hash ;   /* used in lookup and when extending bases.   */
} ;

/* Default symbol value.
 *
 * Starts with pointer to '\0' terminated string
 */
struct symbol_default_value
{
  char* name ;
  char  value[] ;
} ;

/*------------------------------------------------------------------------------
 * Symbol Reference (or "bookmark").
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
  symbol sym ;              /* Address of symbol referred to (if any).
                             * (In "bookmark" this points to self.)      */

  symbol_ref next ;         /* fellow references to the symbol ...       */
  symbol_ref prev ;         /* ... ignore if sym is NULL.                */

  void*	 parent ;	    /* see: sym_ref_parent(sym_ref) etc.         */
  symbol_ref_tag_t  tag ;   /* see: sym_ref_tag(sym_ref) etc.            */
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
extern void symbol_table_set_tell(symbol_table table,
                                           symbol_tell_func* tell) ;

extern symbol symbol_table_ream(symbol_table table, symbol sym, void* value) ;
extern symbol_table symbol_table_free(symbol_table table) ;
extern void symbol_table_reset(symbol_table table, uint base_count) ;

extern symbol symbol_lookup(symbol_table table, const void* name, add_b add) ;

extern void* symbol_set(symbol sym, void* value) ;
extern void* symbol_unset(symbol sym, void* value) ;
extern void* symbol_delete(symbol sym, void* value) ;

Inline bool symbol_is_set(const symbol sym) ;
Inline bool symbol_is_unset(const symbol sym) ;
Inline bool symbol_is_deleted(const symbol sym) ;

extern symbol_hash_t symbol_hash_string(const void* string) ;
extern symbol_hash_t symbol_hash_bytes(const void* bytes, size_t len) ;
Inline symbol_hash_t symbol_hash_word(symbol_hash_t h) ;

Inline void* symbol_get_value(const symbol sym) ;
Inline symbol_table symbol_get_table(const symbol sym) ;

Inline symbol symbol_inc_ref(symbol sym) ;
Inline symbol symbol_dec_ref(symbol sym) ;

Private symbol symbol_zero_ref(symbol sym) ;

extern symbol_ref symbol_init_ref(symbol_ref ref) ;
extern symbol_ref symbol_set_ref(symbol_ref ref, symbol sym) ;
extern symbol_ref symbol_unset_ref(symbol_ref ref,
                                               free_keep_b free_ref_structure) ;
Inline void* sym_ref_symbol(symbol_ref ref) ;
Inline void* sym_ref_value(symbol_ref ref) ;
Inline void* sym_ref_parent(symbol_ref ref) ;
Inline void* sym_ref_p_tag(symbol_ref ref) ;
Inline ulong sym_ref_u_tag(symbol_ref ref) ;
Inline long  sym_ref_i_tag(symbol_ref ref) ;
Inline void sym_ref_set_parent(symbol_ref ref, void* pa) ;
Inline void sym_ref_set_p_tag(symbol_ref ref, void* p_tag) ;
Inline void sym_ref_set_u_tag(symbol_ref ref, ulong u_tag) ;
Inline void sym_ref_set_i_tag(symbol_ref ref, long i_tag) ;

extern void symbol_ref_walk_start(symbol sym, symbol_ref walk) ;
extern symbol_ref symbol_ref_walk_step(symbol_ref walk) ;
extern void symbol_ref_walk_end(symbol_ref walk) ;

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
 * Symbol status checks -- these assume the address of the symbol is NOT NULL
 *
 * Note that a symbol which is not set may be a deleted symbol.
 */

Inline bool
symbol_has_references(const symbol sym)
{
  return (sym->ref_count > symbol_is_set_bit) || (sym->ref_list != NULL) ;
} ;

Inline bool
symbol_is_set(const symbol sym)
{
  return ((sym->ref_count & symbol_is_set_bit) != 0) ;
} ;

Inline bool
symbol_is_unset(const symbol sym)
{
  return ((sym->ref_count & symbol_is_set_bit) == 0) ;
} ;

Inline bool
symbol_is_deleted(const symbol sym)
{
  return (sym->table == NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Access functions -- argument is address of symbol (may be NULL).
 */
Inline void*
symbol_get_value(const symbol sym)
{
  return (sym != NULL) ? sym->value : NULL ;
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
  sym->ref_count += symbol_ref_increment ;
  return sym ;
} ;

Inline symbol
symbol_dec_ref(symbol sym)
{
  if (sym->ref_count <= symbol_ref_increment)
    return symbol_zero_ref(sym) ;

  sym->ref_count -= symbol_ref_increment ;
  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Symbol Reference access functions -- argument is address of symbol_ref.
 *
 * These cope if address of symbol_ref is null, or reference is undefined.
 */
Inline void*
sym_ref_symbol(symbol_ref ref)
{
  return (ref != NULL) ? ref->sym : NULL ;
} ;

Inline void*
sym_ref_value(symbol_ref ref)
{
  return symbol_get_value(sym_ref_symbol(ref)) ;
} ;

Inline void*
sym_ref_parent(symbol_ref ref)
{
  return (ref != NULL) ? ref->parent : NULL ;
} ;

Inline void*
sym_ref_p_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.p  : NULL ;
} ;

Inline unsigned long int
sym_ref_u_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.u : 0 ;
} ;

Inline signed long int
sym_ref_i_tag(symbol_ref ref)
{
  return (ref != NULL) ? ref->tag.i : 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set properties of reference -- argument is address of symbol_ref, which is
 * assumed to NOT be NULL.
 */

Inline void
sym_ref_set_parent(symbol_ref ref, void* pa)
{
  ref->parent = pa ;
} ;

Inline void
sym_ref_set_p_tag(symbol_ref ref, void* p_tag)
{
  ref->tag.p  = p_tag ;
} ;

Inline void
sym_ref_set_u_tag(symbol_ref ref, unsigned long int u_tag)
{
  ref->tag.u  = u_tag ;
} ;

Inline void
sym_ref_set_i_tag(symbol_ref ref, signed long int i_tag)
{
  ref->tag.i  = i_tag ;
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
