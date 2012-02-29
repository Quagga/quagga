/* Symbol Table data structure -- functions
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
#include "misc.h"

#include <ctype.h>

#include "symtab.h"
#include "memory.h"

/*==============================================================================
 * A symbol table maps symbol "names" to symbol "bodies" and, for each symbol,
 * has two ways of keeping track of references to the symbol.
 *
 * A symbol's "name" can be an arbitrary collection of bytes, or a string.  All
 * names in a symbol table are unique.
 *
 * The symbol body is a void* -- whose contents are no concern of this code,
 * *except* that the "body" must point to or contain the name -- so that the
 * table function symbol_cmp_func can test whether a symbol has the given name.
 *
 * Once a name has been mapped to a symbol, the symbol can be referred to using
 * its address or via a symbol reference -- see below.
 *
 * A symbol table can be walked to visit all symbols, and there is support for
 * extracting a vector of all symbols which satisfy a given criterion, and
 * sorting same.
 *
 * A symbol table comprises:
 *
 *   * symbol table structure   -- containing all "red-tape"
 *   * array of chain-bases     -- for the hash table
 *   * symbol entries           -- each containing the value of a symbol
 *
 * The symbol table structure may only be dynamically allocated.
 *
 * A symbol table may point to its "parent".  The symbol table code does not
 * use or need this -- it is for the convenience of the caller.
 *
 * The "name" of a symbol may be anything at all.  As far as the symbol table
 * is concerned the name is defined by the symbol_hash_func and
 * symbol_cmp_func -- see symbol_table_new().
 *
 * The array of chain-bases is dynamically allocated and will grow to maintain
 * an approximate given maximum number of symbols per chain base.  This density
 * is set when the symbol table is initialised.  The array does not
 * automatically reduce in size.
 *
 * The number of chain bases is always odd.  The hash function returns an
 * unsigned value, which is mapped to the chain bases modulo their number.
 * Since that is odd, it is co-prime with the hash, so contributes to the hash
 * process.
 *
 * Each symbol in the table has a dynamically allocated entry, which includes:
 *
 *   * symbol body           -- void*  (which includes the "name" in some way)
 *   * reference count       -- includes number of of reference objects
 *   * list of reference objects
 *
 * While in the symbol table a symbol may *not* have a NULL body, because
 * the symbol table requires access to the name when searching the table.
 * The body may change, and a new body may be set.  The only thing that
 * remains constant is that the symbol_cmp_func() works no matter what form
 * the body takes.
 *
 * When a symbol is created -- symbol_lookup(..., add) -- the body must be set
 * *immediately* and *before* any other symbol table operations on the table.
 * The reference count is zero.
 *
 * Wherever a pointer to the symbol is kept, either the reference count must
 * be incremented or a reference object set up (or the owner of the pointer
 * must have their own mechanism to know when the pointer is valid !).
 *
 * It is generally assumed that the body will only be accessed via the symbol.
 * Assuming that, the body may move around, provided symbol_set() is called
 * to keep the symbol up to date.
 *
 * Simple use of
 *
 *
 * When the count is reduced to zero the symbol is removed from the symbol
 * table, and the body will be destroyed -- symbol_free_func().
 *
 * If there is an "owner" of the body of a symbol, when its value is emptied or
 * unset, the owner may call symbol_unset().  This is essentially the same as
 * symbol_dec_ref() -- balancing the implicit symbol_inc_ref() in the original
 * symbol_set().  Apart from the obvious symmetry, the unset function also
 * allows the caller to replace the body by one which is (say) empty *apart*
 * from the name.  Note that it is up to the caller to ensure that different
 * forms of body can be distinguished by any code that accesses it via the
 * symbol.
 *
 * Note that the symbol table and symbols provide a number of facilities:
 *
 *   (1) access by symbol name to the symbol body.
 *
 *       The symbol table can be used this way without using any of the
 *       reference facilities.
 *
 *       When the symbol is added and the body set, the symbol comes into
 *       existence.  When the body has a value, then symbol_set() will
 *       set the "symbol_set" state -- though this is optional.
 *
 *       The symbol can then be looked up, and its body accessed, via one level
 *       of indirection.
 *
 *       When the symbol is unset -- symbol_unset() -- it will wink out of
 *       existence and the body will be freed.
 *
 *       NB: the one level of indirection means that the body can be moved
 *           around between lookups.
 *
 *   (2) access to the symbol body via saved pointer to the symbol.
 *
 *       Having looked up a symbol, the user may hold on to a pointer to it,
 *       so that subsequent access to the symbol body does not require a
 *       lookup.
 *
 *       The simple reference counting supports this use.
 *
 *       It is the reference owner's responsibility to symbol_inc_ref() and
 *       symbol_dec_ref() as required.
 *
 *       Note that if "symbol_set" is not set, then a symbol and body can
 *       come into existence simply to support named references to an empty
 *       object, and will automatically disappear again if all references
 *       are unset.
 *
 *       If or when some real value is attached to the symbol, symbol_set()
 *       sets the "symbol_set" state so that the symbol and the body are
 *       preserved even if there are no references.  If the value is later
 *       dispantled, then symbol_unset() clears the "symbol_set" state, and
 *       the symbol and body will disappear either immediately or when the
 *       references drop to zero.
 *
 *       Before doing symbol_unset(), the owner of the symbol might choose to
 *       replace the symbol body by an "empty" one -- containing only the
 *       name -- symbol_has_references() indicates whether that is required.
 *
 *       This supports the notion of a named object, which exists, even if its
 *       value is NULL, while there are references to it.  Moreover, when the
 *       value of the object changes, all "by name" references are implicitly
 *       references to the latest value.
 *
 *   (3) notification references
 *
 *       Instead of saving a pointer to the symbol, it is possible to construct
 *       a "notification reference" (symbol_nref).  This itself contains a
 *       pointer to the symbol, and so setting/unsetting an nref implicitly
 *       increments/decrements the symbol reference count.  In addition, the
 *       "nref" is chained on to the symbol, so that all such references can be
 *       walked when the owner of each reference needs to be informed about
 *       some change to the symbol or its body.
 *
 *       The nref belongs to the owner of the reference.  So an nref may be
 *       associated with different symbols at different times.
 *
 *       It is the nref owner's responsibility to symbol_nref_set() and
 *       symbol_nref_unset() as required.
 *
 * It is possible that these facilities should be unbundled.  TBA !  TODO
 *
 * Finally, a symbol may be deleted -- symbol_delete().  This is brutal, and
 * should be used only if the holders of pointers to the symbol and/or nrefs
 * are aware that the symbol will have no body and no longer be associated
 * with a symbol table once it has been deleted.  The symbol will persist
 * until its reference count drops to zero at which point it will wink out
 * of existence.
 */

/*==============================================================================
 * Symbol table operations
 */
static void symbol_table_new_bases(symbol_table table, uint new_base_count) ;
static void symbol_table_extend_bases(symbol_table table) ;

inline static uint symbol_base_index(symbol_table table, symbol_hash_t hash) ;

/*------------------------------------------------------------------------------
 * Allocate and initialise a new symbol table.
 *
 * Requires:
 *
 *   parent   -- address of some parent or other higher level data structure.
 *
 *               This is not used by the symbol table code and may be NULL if
 *               the caller has no use for it.
 *
 *   bases    -- number of list bases to start the symbol table at.
 *
 *               Symbol table grows as required, but can set initial size if
 *               have some expectations and wish to avoid growth steps.
 *
 *               A minimum of SYMBOL_TABLE_BASES_MIN will be allocated.
 *
 *   density  -- %-age of entries/bases.   0 => use default.
 *                                       150 => 1.50 entries/base (for example)
 *
 *   funcs    -- address of struct symbol_funcs, filled in:
 *
 *     hash     -- symbol_hash_func*
 *
 *                 function to calculate symbol_hash_t from a given "name".
 *                 see, for example: symbol_hash_string(), below.
 *
 *     cmp      -- symbol_cmp_func*
 *
 *                 function to compare name of a given value with a given name.
 *
 *     free     -- symbol_free_func*
 *
 *                 function to be called when the number of references to a
 *                 symbol reduce to zero and the value is unset (or deleted).
 *
 * Returns:  address of new symbol table
 */
extern symbol_table
symbol_table_new(void* parent, uint base_count, uint density,
                                                      symbol_funcs_c funcs)
{
  symbol_table table ;

  assert(base_count <= SYMBOL_TABLE_BASES_MAX) ;

  table = XCALLOC(MTYPE_SYMBOL_TABLE, sizeof (struct symbol_table)) ;

  /* The XCALLOC sets:
   *
   *   parent         -- NULL  -- set below
   *
   *   bases          -- NULL  -- set below, by symbol_table_new_bases()
   *   base_count     -- 0     -- set below, ditto
   *
   *   entry_count    -- 0     -- table is empty !
   *   max_index      -- 0     -- table is empty !
   *   extend_thresh  -- 0     -- set below, by symbol_table_new_bases()
   *
   *   density        -- 0     -- set below
   *
   *   func           -- NULLs -- set below
   */
  table->parent        = parent ;

  if (density == 0)
    density = SYMBOL_TABLE_DEFAULT_DENSITY ;

  table->density = (float)density / 100.0 ;

  symbol_table_new_bases(table, base_count) ;

  table->func = *funcs ;                /* Makes a *copy*       */

  return table ;
} ;

/*------------------------------------------------------------------------------
 * Set "parent" of symbol table.
 */
extern void
symbol_table_set_parent(symbol_table table, void* parent)
{
  table->parent = parent ;
} ;

/*------------------------------------------------------------------------------
 * Get "parent" of symbol table.
 */
extern void*
symbol_table_get_parent(symbol_table table)
{
  return table->parent ;
} ;

/*------------------------------------------------------------------------------
 * Ream out given Symbol Table -- if any.
 *
 * This is a symbol_delete() for all symbols.  Has the option to free or keep
 * each symbol's body.
 *
 * Returns the value of the next non-NULL symbol (if any).  So may be used, for
 * example:
 *
 *    symbol sym ;
 *    ...
 *    sym = NULL ;
 *    while ((sym = symbol_table_ream(table, sym, free_it)) != NULL)
 *      {
 *         body = symbol_get_body(sym) ;
 *         ... do what ever is required.  May even unset or delete the
 *             symbol, *provided* keep sym up to date -- because on next call
 *             of symbol_table_ream(), will delete the symbol.
 *
 *         ... note that need do nothing here, the symbol_table_ream() deletes
 *             the symbol !
 *      }
 *
 * Note that deleting a symbol unsets any nrefs there are to it.
 *
 * Symbols which have one or more references when they are deleted are left as
 * orphans, which will be freed when all their references are unset.
 *
 * Returns NULL when the table is empty.
 *
 * Once the table is empty it may continue to be used, using the current
 * set of bases.  To free the empty table: symbol_table_free().  To reset
 * the number of bases: symbol_table_reset().
 *
 * NB: it is wise not to do anything else with the symbol table once reaming
 *     has started and not to stop until this function returns NULL.  However,
 *     will cope if the given symbol or other symbols are deleted between
 *     calls.  Will even cope with symbols being added... but they will be
 *     collected up and reamed out !
 *
 * NB: it is the caller's responsibility to unset all references and release
 *     any that need to be released -- either before or after this operation.
 *
 *     Any symbols whose reference count does not drop to zero when the delete
 *     is done remain -- but with NULL body (and NULL table) !
 */
extern symbol
symbol_table_ream(symbol_table table, symbol sym, free_keep_b free_body)
{
  bool rescan ;
  uint index ;

  if (table == NULL)
    return NULL ;

  qassert((table->bases != NULL) && (table->base_count > 0)) ;

  /* Delete last symbol, if any.
   *
   * If not already deleted, must belong to the table in question !
   */
  if (sym != NULL)
    {
      qassert(table == sym->table) ;
      symbol_delete(sym, free_body) ;
    } ;

  /* Search for next entry.
   */
  rescan = false ;
  index  = table->max_index ;

  if (table->entry_count > 0)
    {
      while (1)
        {
          sym = table->bases[index] ;

          if (sym != NULL)
            {
              table->max_index = index ;
              return sym ;
            } ;

          if (index == 0)
            {
              /* This should not happen.
               *
               * If it does, either the table->entry_count is wrong, or the
               * table->max_index is.  In case it is the later, will rescan
               * once, starting from the end.
               */
              qassert(false) ;

              if (rescan)
                break ;

              rescan = true ;
              index = table->base_count ;
            } ;

            --index ;
        } ;
    } ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Free empty symbol table, if any.
 *
 * Reams the table first -- so any symbols with non-zero reference counts
 * (after any nref have been unset) will persist.  Has the option to
 * free or keep each symbol's body.
 *
 * Zeroises the symbol table structure, to trap dangling references ASAP.
 *
 * Returns:  NULL
 */
extern symbol_table
symbol_table_free(symbol_table table, free_keep_b free_body)
{
  if (table != NULL)
    {
      symbol sym ;

      sym = NULL ;
      while ((sym = symbol_table_ream(table, sym, free_body)) != NULL)
        ;

      XFREE(MTYPE_SYMBOL_BASES, table->bases) ;

      memset(table, 0, sizeof (struct symbol_table)) ;

      XFREE(MTYPE_SYMBOL_TABLE, table) ;        /* table = NULL */
    } ;
  return table ;
} ;

/*------------------------------------------------------------------------------
 * Reset number of bases in given table, if any.
 *
 * This is for use when a table has grown and then shrunk, and it is felt to
 * be essential to recover space by reducing the number of chain bases.
 *
 * Sets the new number of bases to the number given, or such that the threshold
 * for the next reorganisation is about 1.25 * number of entries currently
 * have, whichever is the greater.
 */
extern void
symbol_table_reset(symbol_table table, uint base_count)
{
  uint new_base_count ;

  new_base_count = ((float)table->entry_count * (float)1.25) / table->density ;

  if (new_base_count < base_count)
    new_base_count = base_count ;

  symbol_table_new_bases(table, new_base_count) ;
} ;

/*------------------------------------------------------------------------------
 * Extend the existing array of list bases.
 *
 * To be called when the number of entries exceeds the threshold.
 */
static void
symbol_table_extend_bases(symbol_table table)
{
  uint    old_base_count ;
  uint    new_base_count ;

  qassert((table->bases != NULL) && (table->base_count != 0)) ;

  /* Should be here because the number of entries in the table has exceeded
   * the threshold.
   *
   * Depending on how big the table is, we either double it or add a reasonable
   * chunk of bases.
   */
  old_base_count = table->base_count ;
  new_base_count = (old_base_count | 1) - 1 ;   /* trim enforced odd-ness */

  if (new_base_count <= SYMBOL_TABLE_BASES_DOUBLE_MAX)
    new_base_count *= 2 ;
  else
    new_base_count += SYMBOL_TABLE_BASES_DOUBLE_MAX ;

  assert(new_base_count > old_base_count) ;     /* check for overflow   */

  /* Do the hard work of rearranging the bases
   */
  symbol_table_new_bases(table, new_base_count) ;
} ;

/*------------------------------------------------------------------------------
 * Create and set new chain bases and threshold for next extension.
 *
 * Ensures that the base count used is at least the minimum and is odd.
 *
 * The minumum is the larger of the absolute SYMBOL_TABLE_BASES_MIN, or enough
 * for the number of entries to grow by 25% before the new threshold will be
 * exceeded.
 *
 * If there is an existing set of chain bases, transfers entries from old
 * to new chain bases, and frees the old bases.
 */
static void
symbol_table_new_bases(symbol_table table, uint new_base_count)
{
  uint    new_minimum ;
  symbol* old_bases ;
  uint    old_base_count ;
  uint    old_entry_count ;

  /* Extract what we need to know about the old bases and create new ones.
   */
  old_bases       = table->bases ;
  old_base_count  = table->base_count ;
  old_entry_count = table->entry_count ;

  new_minimum = ((float)table->entry_count * (float)1.25) / table->density ;

  if (new_minimum < SYMBOL_TABLE_BASES_MIN)
    new_minimum = SYMBOL_TABLE_BASES_MIN ;

  if (new_base_count < new_minimum)
    new_base_count = new_minimum ;

  new_base_count |= 1 ;         /* ENSURE is odd                */

  table->bases = XCALLOC(MTYPE_SYMBOL_BASES, new_base_count * sizeof(symbol)) ;
  table->base_count    = new_base_count ;
  table->extend_thresh = new_base_count * table->density ;

  table->max_index     = 0 ;
  table->entry_count   = 0 ;

  /* Finished if have just allocated the first set of bases.
   */
  if (old_bases == NULL)
    {
      qassert((old_base_count == 0) && (old_entry_count == 0)) ;
      return ;
    } ;

  /* Rehome everything on the new chain bases.
   */
  qassert(old_base_count != 0) ;

  while (old_base_count--)
    {
      symbol  next ;
      next = old_bases[old_base_count] ;
      while (next != NULL)
        {
          symbol  this ;
          uint    index ;
          symbol* base ;

          this = next ;
          next = this->next ;

          index = symbol_base_index(table, this->hash) ;
          base  = &table->bases[index] ;
          this->next = *base ;
          *base = this ;

          if (index > table->max_index)
            table->max_index = index ;
          ++table->entry_count ;
        } ;
    } ;

  qassert(table->entry_count == old_entry_count) ;

  /* Release the old chain bases, and we're done
   */
  XFREE(MTYPE_SYMBOL_BASES, old_bases) ;
} ;

/*------------------------------------------------------------------------------
 * Return chain base index for given hash value.
 */
inline static uint
symbol_base_index(symbol_table table, symbol_hash_t hash)
{
  return hash % table->base_count ;
} ;

/*==============================================================================
 * Symbol operations
 */

/*------------------------------------------------------------------------------
 * Look-up name in given symbol table.  Add if required.
 *
 * Returns NULL if not found and not required to add.
 *
 * If adds, the symbol returned will have a NULL body.  That body MUST be
 * set BEFORE any further symbol table operations, and that body MUST have
 * the same "name" value as given here -- symbol_set_body().
 *
 * If does not add, and symbol exists, the symbol returned will NOT have a NULL
 * body.
 *
 * NB: the name argument is passed to the symbol table's hash function.
 */
extern symbol
symbol_lookup(symbol_table table, const void* name, add_b add)
{
  struct symbol*  this ;
  struct symbol** base ;
  uint            index ;
  symbol_hash_t   hash ;

  qassert((table != NULL) && (table->bases != NULL)) ;

  hash = table->func.hash(name) ;

  index = symbol_base_index(table, hash) ;
  base  = &table->bases[index] ;
  this  = *base ;
  while (this != NULL)
    {
      qassert((this->table == table) && (this->body != NULL)) ;

      if ((this->hash == hash) && (table->func.cmp(this->body, name) == 0))
        return this ;

      this = this->next ;
    } ;

  /* Not found -- quit now if not required to add       */
  if (!add)
    return NULL ;

  /* Adding: first, carve a new, empty symbol entry
   *
   * XCALLOC sets:
   *
   *   table       = NULL -- set below
   *   next        = NULL
   *   value       = NULL
   *   ref_list    = NULL
   *   ref_count   = 0
   *   hash        = 0    -- set below
   */
  this = XCALLOC(MTYPE_SYMBOL, sizeof(struct symbol)) ;

  this->table     = table ;
  this->hash      = hash ;

  /* Second, if required, extend the array of list bases.  We extend if
   * we have a collision *and* we exceed threshold of number of entries.
   *
   * Once extended, recalculate the index and select new base.
   */
  if ((*base != NULL) && (table->entry_count > table->extend_thresh))
    {
      symbol_table_extend_bases(table) ;

      index = symbol_base_index(table, hash) ;
      base  = &table->bases[index] ;
    } ;

  /* Third, chain in the new entry
   */
  this->next = *base ;
  *base = this ;

  /* Finally, count the new entry and update the max_index.
   */
  if (index > table->max_index)
    table->max_index = index ;

  ++table->entry_count ;

  return this ;
} ;

/*==============================================================================
 * Symbol Value handling.
 */
static symbol symbol_remove(symbol sym, free_keep_b free_body) ;

/*------------------------------------------------------------------------------
 * Set the given symbol's body -- must NOT be NULL.
 *
 * If "set" is true, will symbol_set() the symbol -- after freeing any
 * existing body, if that is required (in case the free operation cares).
 *
 * If "set" is false, has *no* effect on the reference count -- see
 * symbol_unset()
 *
 * May be used after a new symbol is added to the symbol table -- in which case
 * the current body will be NULL.
 *
 * May be used to replace an existing symbol body -- in which case the current
 * body will be returned.  It is the caller's responsibility to free it.
 *
 * May set the symbol body any number of times, but each time the "name" MUST
 * be the same as the original name.
 *
 * May be used to replace the body of a symbol just before unsetting it.
 * Caller may wish to check symbol_has_references() before bothering to do
 * this.
 */
extern void
symbol_set_body(symbol sym, void* body, bool set, free_keep_b free_old)
{
  qassert((sym->table != NULL) && (body != NULL)) ;

  if ((sym->body != NULL) && free_old)
    sym->table->func.free(sym->body) ;

  sym->body = body ;

  if (set)
    symbol_set(sym) ;
} ;

/*------------------------------------------------------------------------------
 * Delete symbol -- set body and table to NULL
 *
 * Unsets any nrefs -- since nref structure does not belong to the symbol, does
 * not free the nref.  So, deleting a symbol makes all nref point to NULL.
 *
 * Does symbol_unset() -- which will remove the symbol if there are no
 * remaining references.
 *
 * If the symbol still exists after reducing the reference count: remove it
 * from the symbol table and free the current body.
 *
 * After symbol_delete(), the caller should forget about the symbol, leaving it
 * up to the reference system to sweep up, as required.
 *
 * Returns:  NULL
 *
 * NB: this operation leaves any existing references pointing at the orphan
 *     symbol.  If a new symbol is created, with the same name, those
 *     references will NOT then point at the new value.
 *
 * NB: any existing references point to a symbol with a NULL table and a NULL
 *     body.  Because the body is NULL the symbol no longer even has a name !
 *
 * NB: this will destroy any bookmarks there are on the nref_list... which the
 *     nref_list walker has to look out for !
 */
extern symbol
symbol_delete(symbol sym, free_keep_b free_body)
{
  if (sym == NULL)
    return NULL ;               /* do nothing if already deleted        */

  symbol_inc_ref(sym) ;         /* hold on to the symbol & body pro tem */

  while (sym->nref_list != NULL)
    symbol_nref_unset(sym->nref_list, keep_it) ;

  symbol_unset(sym, keep_it) ;  /* unset, as promised                   */

  sym->ref_count -= symbol_ref_count_increment ;
                                /* hand crank -- about to remove, and
                                 * may be keeping the body.             */

  return symbol_remove(sym, free_body) ;
                                /* free the body (if required) and
                                 * remove from symbol table, leaving
                                 * orphan symbol.                       */
} ;

/*------------------------------------------------------------------------------
 * Zeroise the reference count -- and remove the symbol
 */
Private symbol
symbol_zero_ref(symbol sym, free_keep_b free_body)
{
  sym->ref_count = 0 ;

  qassert(sym->nref_list == NULL) ;

  return symbol_remove(sym, free_body) ;        /* Returns NULL */
} ;

/*------------------------------------------------------------------------------
 * Remove symbol from its symbol table (if any).
 *
 * If there is a body, free it.
 *
 * If the reference count is zero, free the symbol.
 *
 * Returns:  NULL <=> symbol has been freed
 *           otherwise == the symbol
 *
 * NB: removing things from the symbol table does not change the chain bases
 *     or anything like that... so table can be reamed and walked, without
 *     worrying about that !
 */
static symbol
symbol_remove(symbol sym, free_keep_b free_body)
{
  symbol_table table ;

  table = sym->table ;
  if (table != NULL)            /* Deleted symbols have no parent table.  */
    {
      symbol* base ;
      symbol  prev ;

      assert(table->entry_count != 0) ;

      base = &table->bases[symbol_base_index(table, sym->hash)] ;
      if (*base == sym)
        *base = sym->next ;
      else
        {
          prev = *base ;
          while (1)
            {
              assert(prev != NULL) ;

              if (prev->next == sym)
                break ;

              prev = prev->next ;
            } ;
          prev->next = sym->next ;
        } ;

      --table->entry_count ;

      sym->table = NULL ;
      sym->next  = NULL ;
      sym->hash  = 0 ;
    } ;

  if (sym->body != NULL)
    {
      qassert(table != NULL) ;

      if (free_body)
        table->func.free(sym->body) ;

      sym->body = NULL ;
    } ;

  if (sym->ref_count == 0)
    {
      qassert(sym->nref_list == NULL) ;
      XFREE(MTYPE_SYMBOL, sym) ;        /* sets sym = NULL      */
    } ;

  return sym ;
} ;

/*==============================================================================
 * Simple hashing functions.
 *
 * The hash functions provided here use CRC32 as a hash.
 *
 * CRC32 is not intended as a hash function, and is not a perfect one.
 * However it is fast -- requiring a few simple operations per byte.  Taken
 * with the secondary effect of using the hash produced modulo an odd number,
 * experience suggests this is sufficient.
 */
static u_int32_t crc_table[] ;

/*------------------------------------------------------------------------------
 * Simple hash function for '\0' terminated strings.
 *
 * Can be used directly in a struct symbol_funcs.
 */
extern symbol_hash_t
symbol_hash_string(const void* string)
{
  uint32_t  h = 0x31415927 ;
  const u_int8_t* p = string ;

  while (*p != '\0')
    h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;

  return h ;
} ;

/*------------------------------------------------------------------------------
 * Simple symbol byte vector hash function.
 */
extern  symbol_hash_t
symbol_hash_bytes(const void* bytes, size_t len) {
  uint32_t  h = (uint32_t)len * 0x31415927 ;
                                /* So strings of zeros don't CRC the same ! */
  const u_int8_t*  p = bytes ;
  const u_int8_t*  e = p + len ;

  if ((len & 3) != 0)
    {
      if ((len & 2) != 0)
        {
          h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
          h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
        } ;
      if ((len & 1) != 0)
        h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
    } ;

  while (p < e)
    {
      h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
      h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
      h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
      h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;
    } ;

  return h ;
} ;

/*==============================================================================
 * Reference list handling.
 *
 * References are added at the head of the list -- which is significant when
 * adding references during a symbol reference walk.
 *
 * Implementation note: the next and prev pointers in the symbol_ref structure
 * are significant only if the sym pointer is not NULL.
 */

static inline void symbol_nref_list_add(symbol sym, symbol_nref nref) ;
static inline void symbol_nref_list_del(symbol sym, symbol_nref nref) ;
static inline bool symbol_nref_is_bookmark(symbol_nref nref) ;

/*------------------------------------------------------------------------------
 * Initialise symbol notify reference -- allocate if required.
 */
extern symbol_nref
symbol_nref_init(symbol_nref nref)
{
  if (nref == NULL)
    return XCALLOC(MTYPE_SYMBOL_REF, sizeof(struct symbol_nref)) ;
  else
    return memset(nref, 0, sizeof(struct symbol_nref)) ;
} ;

/*------------------------------------------------------------------------------
 * Set symbol notify reference -- allocate if required (nref == NULL).
 *
 * Increments reference count on new symbol (if any) and decrements count on
 * old symbol (if any).
 *
 * NB: does nothing if reference already set to the given symbol.
 *
 * NB: unsets (but does not free) nref if was not NULL (and is not same as
 *     symbol being set to) before setting new reference.
 *
 *     Note that the old symbol may suddenly disappear !
 *
 * NB: setting nref to NULL unsets any existing nref, but does NOT free the
 *     nref structure.
 *
 * NB: if nref is allocated, the parent is set NULL and the tag is set 0.
 *
 *     if nref is not allocated, the parent and tag are unchanged.
 */
extern symbol_nref
symbol_nref_set(symbol_nref nref, symbol sym)
{
  if (nref != NULL)
    {
      if (nref->sym == sym)
        return nref ;     /* Nothing more to do if already set to given value */

      if (nref->sym != NULL)
        symbol_nref_unset(nref, keep_it) ;
    }
  else
    nref = symbol_nref_init(NULL) ;

  qassert(nref->sym == NULL) ;

  if (sym != NULL)
    {
      symbol_nref_list_add(sym, nref) ;
      nref->sym = symbol_inc_ref(sym) ;
    } ;

  return nref ;
} ;

/*------------------------------------------------------------------------------
 * Unset symbol notify reference.  Free the structure if required.
 *
 * NB: does nothing if address of nref is NULL.
 *
 * NB: if nref is not freed, the parent and tag are unchanged.
 *
 * NB: removing the last reference to an symbol causes the symbol to be freed.
 *
 * NB: copes if the nref is already unset, of course.
 *
 * NB: this is called by symbol_delete(), and may be unsetting a bookmark !!
 *
 *     this is also called by symbol_nref_walk_step(), also to unset a
 *     bookmark.
 */
extern symbol_nref
symbol_nref_unset(symbol_nref nref, free_keep_b free)
{
  symbol sym ;

  if (nref == NULL)
    return nref ;

  if (symbol_nref_is_bookmark(nref))
    {
      /* Get actual symbol for bookmark, and then set parent to NULL and
       * set sym as for normal nref...  then it can be deleted along with
       * normal nrefs !
       */
      nref->sym    = (symbol)nref->parent ;
      nref->parent = NULL ;

      qassert(!free) ;          /* when symbol_delete()                 */
      free = false ;
    } ;

  sym = nref->sym ;

  if (sym != NULL)              /* NULL => reference already unset      */
    {
      symbol_nref_list_del(sym, nref) ;
      nref->sym = symbol_dec_ref(sym) ; /* sets nref->sym to NULL       */
    } ;

  if (free)
    XFREE(MTYPE_SYMBOL_REF, nref) ;     /* nref is set to NULL          */

  return nref ;
} ;

/*------------------------------------------------------------------------------
 * Insert symbol_nref at head of symbol's nref_list.
 *
 * NB: only affects sym->nref_list, nref->next and nref->prev
 */
static inline void
symbol_nref_list_add(symbol sym, symbol_nref nref)
{
  symbol_nref next ;

  next = sym->nref_list ;       /* current first on list, if any        */

  sym->nref_list = nref ;       /* new first on list                    */
  nref->next = next ;           /* point to old first on list           */
  nref->prev = (void*)sym ;     /* mark as first on list                */

  if (next != NULL)
    next->prev = nref ;         /* update old first on list             */
} ;

/*------------------------------------------------------------------------------
 * Clip symbol_ref from symbol's list of references.
 *
 * If symbol_ref has already been deleted the prev pointer is NULL, and this
 * function copes -- and does not need the symbol to be valid (sym may be NULL).
 *
 * NB: only affects sym->nref_list, nref->next and nref->prev
 */
static inline void
symbol_nref_list_del(symbol sym, symbol_nref nref)
{
  symbol_nref prev = nref->prev ;
  symbol_nref next = nref->next ;

  if (prev != NULL)
    {
      if (prev == (void*)sym)
	{
	  assert(sym->nref_list == nref) ;
	  sym->nref_list = next ;       /* remove from head of list     */
	}
      else
        prev->next = next ;             /* remove from place in list    */

      if (next != NULL)
	next->prev = prev ;             /* update next's prev ptr       */
    } ;

  nref->next = nref->prev = NULL ;
} ;

/*==============================================================================
 * Procedure for walking the notify references to a symbol:
 *
 *   symbol_nref_t walk[1] ;
 *   symbol        sym ;
 *   symbol_nref   ref ;
 *
 *   symbol_nref_walk_start(sym, walk) ;
 *   while ((ref = symbol_nref_walk_step(walk)) != NULL)
 *   	.... whatever
 *   symbol_nref_walk_end(walk) ;
 *
 *  NB: it is *essential* to call symbol_nref_walk_end() exactly once at some
 *      time after symbol_nref_walk_start.
 *
 * The symbol table walk uses a "bookmark" which is a special from of entry in
 * the symbol's nref_list.  This mechanism:
 *
 *   (a) prevents the symbol being freed while the reference walk is in
 *       progress -- that may happen during symbol_ref_walk_end.
 *
 *   (b) allows for the current and other nref to be set or unset.
 *
 *       Setting an nref inserts it upstream of the bookmark -- so it will
 *       not be visited during the walk.
 *
 *       Unsetting an nref that has yet to be visited eliminates it from
 *       the walk.
 *
 *       Note that setting an nref to refer to the symbol it already refers to
 *       has no effect at all.
 *
 *   (c) allows the symbol to be reset, unset or set during am nref walk.
 *
 *       If that triggers another symbol nref walk, then that will proceed
 *       until it hits the point reached by the previous walk, and will
 *       terminate that and proceed.
 *
 *       Suppose the earlier walk was dealing with the value changing from
 *       'A' to 'B'.  A later walk will deal with a change from 'B' to 'C'
 *       until it meets the earlier one, in which point it will be dealing with
 *       a change 'A' to 'C'.
 *
 *       If that does not suit, don't fiddle with symbol values during a
 *       symbol nref walk.
 */

/*------------------------------------------------------------------------------
 * Start walk of symbol notify references
 *
 * NB: given walk symbol_nref may NOT be NULL.
 */
extern void
symbol_nref_walk_start(symbol sym, symbol_nref walk)
{
  assert(walk != NULL) ;

  symbol_nref_init(walk) ;      /* keeping things tidy                  */
  walk->sym     = (void*)walk ; /* bookmark signature                   */
  walk->parent  = sym ;

  symbol_nref_list_add(sym, walk) ;  /* insert bookmark at head of list      */
  symbol_inc_ref(sym) ;         /* count in the bookmark                */
} ;

/*------------------------------------------------------------------------------
 * Step walk and return the next reference (if any).
 */
extern symbol_nref
symbol_nref_walk_step(symbol_nref walk)
{
  symbol_nref nref ;

  /* It is possible for a symbol_delete() to have emptied out the nref_list,
   * including bookmarks.
   *
   * It is also possible that a later walk has removed this bookmark.
   *
   * Both set nref->sym to NULL
   */
  if (walk->sym == NULL)
    return NULL ;                               /* walk terminated      */

  assert(symbol_nref_is_bookmark(walk)) ;       /* must be a bookmark ! */

  /* Deal with cases of (a) the next item is a bookmark, and (b) there are
   * no more items to deal with.
   */
  while (1)
    {
      if (walk->next == NULL)
        {
          /* Have reached the end -- so unset self to close down the walk and
           * decrement the reference count.
           */
          symbol_nref_unset(walk, keep_it) ;

          qassert(walk->sym == NULL) ;

          return NULL ;                 /* finished     */
        } ;

      if (!symbol_nref_is_bookmark(walk->next))
        break ;

      symbol_nref_unset(walk->next, keep_it) ;
    } ;

  /* Now we move the bookmark one place forwards
   */
  nref = walk->next ;

  qassert(nref->prev == walk) ;

  nref->prev = walk->prev ;
  walk->next = nref->next ;

  nref->next  = walk ;
  walk->prev  = nref ;

  /* Return the nref we just stepped past
   */
  return nref ;
} ;

/*------------------------------------------------------------------------------
 * End of symbol notify reference walk.
 *
 * Unset the bookmark, if it is still set.
 *
 * If the walk was run to completion, then there is, in fact, nothing to be
 * done here.
 */
extern void
symbol_nref_walk_end(symbol_nref walk)
{
  if (walk->sym != NULL)
    symbol_nref_unset(walk, keep_it) ;
} ;

/*------------------------------------------------------------------------------
 * Bookmarks are symbol_ref structures, distinguished from ordinary symbol_ref
 * structures by setting the sym field to point at the bookmark symbol_ref
 * itself.
 *
 * (It would be nicer to use the parent field for this... but what is put
 *  there in ordinary symbol_ref structures is not guaranteed...)
 */
static inline bool
symbol_nref_is_bookmark(symbol_nref nref)
{
  return (void*)nref->sym == (void*)nref ;
} ;

/*==============================================================================
 * Walking a symbol table
 *
 * Simple walk: visits all entries in the table, in the order they are hashed
 *              to.  Simple iterator.
 *
 * Extract:     makes vector of pointers to selected entries, and sorts that
 *              vector as required.
 */

/*------------------------------------------------------------------------------
 * Walk the given symbol table.
 *
 * Usage:
 *
 *   symbol_walker_t walker ;
 *   symbol sym ;
 *   ....
 *   symbol_walk_start(table, walker) ;
 *   while ((sym = symbol_walk_next(walker)))
 *     ....
 *
 * NB: it is possible to change the current symbol while the walk is in
 *     progress -- up to and including unsetting/deleting it.  Any other
 *     changes to the table must NOT be attempted.
 */
extern void
symbol_walk_start(symbol_table table, symbol_walker walk)
{
  assert(walk != NULL) ;

  walk->next       = NULL ;
  walk->base       = table->bases ;
  walk->base_count = table->base_count ;
} ;

/*------------------------------------------------------------------------------
 * Walk to next symbol to consider
 */
extern symbol
symbol_walk_next(symbol_walker walk)
{
  symbol this = walk->next ;

  while (this == NULL)
    {
      if (walk->base_count == 0)
        return NULL ;

      --walk->base_count ;
      this = *(walk->base++) ;
    } ;

  walk->next = this->next ;
  return this ;
} ;

/*------------------------------------------------------------------------------
 * Extract Symbols.
 *
 * Walk symbol table and select symbols to add to a new vector.  Then sort the
 * vector, if required.  Takes:
 *
 *  -- selector: NULL => select all
 *  -- p_val:    pointer is passed to the select function (if any)
 *  -- most:     if there is a select function, this flag hints that most of
 *               the symbols will be selected -- so it is worth preallocating
 *               a vector big enough for all symbols.
 *  -- sort:     NULL => no sort (!)
 *
 * NB: the vector contains pointers to the selected symbols.  It is the
 *     caller's responsibility to avoid deleting any symbol whose pointer
 *     in the vector they expect to rely on !
 */
extern vector
symbol_table_extract(symbol_table table,
		     symbol_select_cmp* selector, const void* p_val, bool most,
							  symbol_sort_cmp* sort)
{
  vector extract ;
  symbol* base ;
  unsigned int	count ;
  symbol sym ;

  extract = vector_init_new(NULL, (most || (selector == NULL))
						    ? table->entry_count : 8) ;
  base  = table->bases ;

  if (base == NULL)
    return extract ;            /* Quit if symbol table is "reset"      */

  count = table->base_count ;
  while (count--)
    {
      sym = *base++ ;
      while (sym != NULL)
	{
	  if ((selector == NULL) || selector(sym, p_val))
	    vector_push_item(extract, sym) ;
	  sym = sym->next ;
	} ;
    } ;

  if (sort != NULL)
    vector_sort(extract, (vector_sort_cmp*)sort) ;

  return extract ;
} ;

/*==============================================================================
 * Some common comparison functions for symbol table extracts.
 */

/*------------------------------------------------------------------------------
 * Comparison function to sort names which are a mixture of digits and other
 * characters.
 *
 * Assumes that symbol_get_name() returns address of '\0' terminated string.
 *
 * This comparison treats substrings of digits as numbers, so "a10" is > "a1".
 */
extern int
symbol_mixed_name_cmp(const char* a,
		      const char* b)
{
  const char* ap ;
  const char* bp ;
  int la, lb ;

  ap = a ;
  bp = b ;

  while (1) {
    if (isdigit(*ap) && isdigit(*bp))
      {
	char* aq ;	/* Required to stop the compiler whining */
	char* bq ;
	ulong na = strtoul(ap, (char** restrict)&aq, 10) ;
	ulong nb = strtoul(bp, (char** restrict)&bq, 10) ;
	if (na != nb)
	  return (na < nb) ? -1 : +1 ;
	ap = aq ;
	bp = bq ;
      }
    else
      {
	if (*ap != *bp)
	  return (*ap < *bp) ? -1 : +1 ;
	if (*ap == '\0')
	  break ;
	++ap ;
	++bp ;
      }
  } ;

  /* Looks like the names are equal.
   * But may be different lengths if have number part(s) with leading zeros,
   */

  la = strlen(a) ;
  lb = strlen(b) ;
  if (la != lb)
    return (la < lb) ? -1 : +1 ;
  return 0 ;
} ;

/*==============================================================================
 * Table for generating CRC-32 -- Standard (0x1_04C1_1DB7 0xEDB8_8320)
 */
static u_int32_t crc_table[] =
{
  0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F,
  0xE963A535, 0x9E6495A3, 0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988,
  0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91, 0x1DB71064, 0x6AB020F2,
  0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
  0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9,
  0xFA0F3D63, 0x8D080DF5, 0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172,
  0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B, 0x35B5A8FA, 0x42B2986C,
  0xDBBBC9D6, 0xACBCF940, 0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
  0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116, 0x21B4F4B5, 0x56B3C423,
  0xCFBA9599, 0xB8BDA50F, 0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924,
  0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D, 0x76DC4190, 0x01DB7106,
  0x98D220BC, 0xEFD5102A, 0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
  0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818, 0x7F6A0DBB, 0x086D3D2D,
  0x91646C97, 0xE6635C01, 0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E,
  0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457, 0x65B0D9C6, 0x12B7E950,
  0x8BBEB8EA, 0xFCB9887C, 0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
  0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2, 0x4ADFA541, 0x3DD895D7,
  0xA4D1C46D, 0xD3D6F4FB, 0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0,
  0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9, 0x5005713C, 0x270241AA,
  0xBE0B1010, 0xC90C2086, 0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
  0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4, 0x59B33D17, 0x2EB40D81,
  0xB7BD5C3B, 0xC0BA6CAD, 0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A,
  0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683, 0xE3630B12, 0x94643B84,
  0x0D6D6A3E, 0x7A6A5AA8, 0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
  0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE, 0xF762575D, 0x806567CB,
  0x196C3671, 0x6E6B06E7, 0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC,
  0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5, 0xD6D6A3E8, 0xA1D1937E,
  0x38D8C2C4, 0x4FDFF252, 0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
  0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60, 0xDF60EFC3, 0xA867DF55,
  0x316E8EEF, 0x4669BE79, 0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236,
  0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F, 0xC5BA3BBE, 0xB2BD0B28,
  0x2BB45A92, 0x5CB36A04, 0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
  0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A, 0x9C0906A9, 0xEB0E363F,
  0x72076785, 0x05005713, 0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38,
  0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21, 0x86D3D2D4, 0xF1D4E242,
  0x68DDB3F8, 0x1FDA836E, 0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
  0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C, 0x8F659EFF, 0xF862AE69,
  0x616BFFD3, 0x166CCF45, 0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2,
  0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB, 0xAED16A4A, 0xD9D65ADC,
  0x40DF0B66, 0x37D83BF0, 0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
  0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6, 0xBAD03605, 0xCDD70693,
  0x54DE5729, 0x23D967BF, 0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94,
  0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D,
} ;
