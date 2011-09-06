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
 * A symbol table maps symbol "names" to symbol values and, for each symbol,
 * has two ways of keeping track of references to the symbol.
 *
 * A symbol's "name" can be an arbitrary collection of bytes, or a string.  All
 * names in a symbol table are unique.
 *
 * The symbol value is a void* -- whose contents are no concern of this code,
 * except that the value must point to or contain the name -- so that the table
 * function symbol_cmp_func can test whether a symbol has the given name.
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
 *   * symbol value   -- void*  (which includes the "name" in some way)
 *   * count of references
 *   * list of reference objects
 *
 * While in the symbol table a symbol may *not* have a NULL value, because
 * the symbol table requires access to the name when searching the table.
 *
 * A symbol may be in one of three states:
 *
 *   * set     -- symbol is in the table and the value is set to something.
 *
 *                Even if there are no references to it, the symbol is kept in
 *                the table.
 *
 *   * unset   -- symbol is in the table but the value is unset or empty.
 *
 *                There is at least one reference to the symbol, and it is kept
 *                in the table until the references reduce to zero.  When the
 *                references do reduce to zero, the symbol is removed from the
 *                table and it and the value are freed.
 *
 *   * deleted -- symbol is not in the table, and the value is unset.
 *
 *                There is at least one reference to the symbol, and it is kept
 *                in the table until the references reduce to zero.  When the
 *                references do reduce to zero, the symbol and any remaining
 *                value are freed.
 *
 *                An deleted symbol may have no value.
 *
 * Setting or unsetting a symbol moves it between the two states.  When a
 * symbol is unset all existing references point at the empty value, if the
 * value is later set again, then the references point at the new value.
 *
 * Deleting a symbol moves it into deleted state.  The value is assumed to be
 * unset/empty, but may be set completely NULL -- in the later case, the symbol
 * no longer even has a name.  If a new value with the same name is later
 * invented, all existing references to the deleted symbol will continue to
 * point at the deleted symbol.  This is intended for when a symbol table is
 * being dismantled (so no new value will ever be set) but may have other uses.
 *
 * The value of a symbol may be changed at any time -- provided the "name"
 * remains the same.  The symbol refers to the value (and "name"), so
 * provides one level of indirection.
 *
 * See symbol_set, symbol_unset, symbol_delete and symbol_table_ream.
 *
 * Keeping track of references is important because it ensures that symbols
 * can be deleted or their value unset, without leaving dangling references.
 * There are two, parallel mechanisms for keeping track of references to a
 * symbol:
 *
 *   1. reference count -- this is for when all that is required is a pointer
 *      to the symbol, ie:
 *
 *       * ptr_to_symbol = symbol_inc_ref(sym)  -- set reference & count up
 *       * ptr_to_symbol = symbol_dec_ref(sym)  -- unset reference & count down
 *
 *      NB: when a symbol is created the reference count is set to 1.  For
 *          ordinary reference counting, the count increases/decreases by 2.
 *
 *          So, while a symbol is "set", it will not be freed.
 *
 *   2. list of references -- this is for when it is useful to visit all
 *      references to a given symbol, for example when the value of the symbol
 *      is set and all users of the symbol need to adjust to the new state.
 *
 *      A walk mechanism is provided so that all references to a given symbol
 *      may be processed sequentially.
 *
 *      A symbol reference may be statically allocated, embedded in another
 *      structure, or allocated dynamically.  In any case the symbol reference
 *      operations require the address of the symbol reference structure -- see
 *      typedef for symbol_ref.
 *
 * Symbol references of both kinds are the responsibility of of the owner of
 * the reference, who must look after setting, unsetting and (if required)
 * releasing them.
 *
 * It may seem profligate to have two mechanisms.  However, it is simpler than
 * having two types of symbol, or two types of symbol table.  It may also be
 * useful to have both simple references and references for dealing with
 * value changes.
 *
 * Deleting a symbol from the table will leave it in existence (but orphaned)
 * if there are any references to the symbol.  Once deleted, when the number
 * of references is reduced to zero, the symbol will be freed and any value
 * still associated with it is freed (using the symbol_free_func).
 *
 * A symbol_tell_func may be set (but is optional).  When the value is set or
 * unset or the symbol is deleted the tell function is called:
 *
 *   symbol_tell_func(symbol sym, symbol_change_t why, symbol_ref walk) ;
 *
 * where the "why" indicates whether the symbol has been set (or set again),
 * unset or deleted.  The "walk" is set if there is at least one symbol_ref,
 * and may be used to walk all such references.  See symbol_table_init(),
 * below.
 */

/*==============================================================================
 * Symbol table operations
 */
static uint symbol_table_new_bases(symbol_table table, uint new_base_count) ;
static void symbol_extend_bases(symbol_table table) ;

inline static uint symbol_base_index(symbol_table table, symbol_hash_t hash) ;
inline static symbol* symbol_base(symbol_table table, symbol_hash_t hash) ;

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
 *     tell     -- symbol_tell_func*
 *
 *                 function to be called when the symbol value is set or
 *                 changed or unset, or if the symbol is about to be deleted.
 *
 *                 NULL => no symbol_tell_func.
 *
 *                 The function is passed the new value, and the symbol will
 *                 be set or unset depending on its new state.  Note that if
 *                 the symbol is about to be deleted, it will be unset, but
 *                 not yet deleted.
 *
 *                 During the symbol_tell_func, references to the symbol may be
 *                 set or unset, but the value may not be changed.
 *
 *                 If there are one or more symbol_ref objects, then sets up a
 *                 symbol_ref walker and passes it to the symbol_tell_func,
 *                 which may:
 *
 *                   symbol_ref  ref ;
 *                   while ((ref = symbol_ref_walk_step(walk)) != NULL)
 *                   .... whatever
 *
 *                 to walk all the references.  The symbol_tell_func does not
 *                 need to symbol_ref_walk_start() or symbol_ref_walk_end().
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

  table->func = *funcs ;

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
 * Set the symbol_tell_func
 *
 * May be set to NULL to turn off.
 */
extern void
symbol_table_set_tell(symbol_table table, symbol_tell_func* tell)
{
  table->func.tell = tell ;
} ;

/*------------------------------------------------------------------------------
 * Ream out given Symbol Table -- if any.
 *
 * This is effectively a symbol_delete() for all symbols, followed by freeing
 * the given symbol table.
 *
 * Returns the value of the next non-NULL symbol (if any).  So may be used, for
 * example:
 *
 *    xxxx* val ;
 *    symbol sym ;
 *    ...
 *    sym = NULL ;
 *    while ((sym = symbol_table_ream(table, sym, val)) != NULL)
 *      {
 *         val = symbol_get_value(sym) ;
 *         ... do what's required to release the value       ...
 *         ... may set val == NULL, or to new, reduced value ...
 *         ... MUST loop back with sym *unchanged*           ...
 *         ... when loops back will symbol_delete(sym, val)  ...
 *      }
 *
 * Note that the symbol returned by symbol_table_ream() is deleted when it is
 * passed in to symbol_table_ream() the next time around the loop.
 *
 * Returns NULL when the table is empty, at which point the table will have
 * been freed, and any pointers to it should be forgotten.
 *
 * Symbols which have one or more references when they are deleted are left as
 * orphans, which will be freed when all their references are unset.
 *
 * NB: do NOT attempt to do anything else with the symbol table once reaming
 *     has started and do not stop until this function returns NULL.
 *
 * NB: it is the caller's responsibility to unset all references and release
 *     any that need to be released -- either before or after this operation.
 */
extern symbol
symbol_table_ream(symbol_table table, symbol sym, void* value)
{
  uint i ;

  if (table == NULL)
    return NULL ;               /* already reamed or never kissed       */

  /* Delete last symbol, or start process.                              */
  if (sym != NULL)
    {
      qassert(table == sym->table) ;

      i = symbol_base_index(table, sym->hash) ;
      symbol_delete(sym, value) ;
    }
  else
    {
      qassert(table->base_count != 0) ;
      i = table->base_count - 1 ;
    } ;

  /* There are no actual bases until they have been allocated.          */

  while (table->entry_count != 0)
    {
      sym = table->bases[i] ;

      if (sym != NULL)
        return sym ;

      qassert(i != 0) ;

      --i ;
    } ;

  while (qdebug)
    {
      qassert(table->bases[i] == NULL) ;

      if (i == 0)
        break ;

      --i ;
    } ;

  /* No entries, so now can free the symbol table.                      */

  XFREE(MTYPE_SYMBOL_BASES, table->bases);
  XFREE(MTYPE_SYMBOL_TABLE, table) ;

  return NULL ;
} ;

/*------------------------------------------------------------------------------
 * Create and set new chain bases and threshold for next extension.
 *
 * Ensures that the base count is at least the minimum and is odd,
 * and returns the value set.
 *
 * Sets new: table->bases
 *           table->base_count
 *           table->extend_thresh
 *
 * Returns: the actual new base count
 */
static unsigned int
symbol_table_new_bases(symbol_table table, uint new_base_count)
{
  if (new_base_count < SYMBOL_TABLE_BASES_MIN)
    new_base_count = SYMBOL_TABLE_BASES_MIN ;

  new_base_count |= 1 ;         /* ENSURE is odd                */

  table->bases = XCALLOC(MTYPE_SYMBOL_BASES, new_base_count * sizeof(symbol)) ;
  table->base_count    = new_base_count ;
  table->extend_thresh = new_base_count * table->density ;

  return new_base_count ;
} ;

/*------------------------------------------------------------------------------
 * Extend the array of list bases.
 */
static void
symbol_extend_bases(symbol_table table)
{
  symbol  this ;
  symbol  next ;
  symbol* old_bases ;
  symbol* new_bases ;
  symbol* base ;
  uint    new_base_count ;
  uint    old_base_count ;

  old_bases      = table->bases ;
  old_base_count = table->base_count ;

  assert((old_bases != NULL) && (old_base_count != 0)) ;

  new_base_count = (table->base_count | 1) - 1 ; /* trim enforced odd-ness */

  if (new_base_count <= SYMBOL_TABLE_BASES_DOUBLE_MAX)
    new_base_count *= 2 ;
  else
    new_base_count += SYMBOL_TABLE_BASES_DOUBLE_MAX ;

  assert(new_base_count > old_base_count) ;

  new_base_count = symbol_table_new_bases(table, new_base_count) ;

  /* Rehome everything on the new chain bases.          */
  new_bases = table->bases ;
  while (old_base_count--)
    {
       next = old_bases[old_base_count] ;
       while (next != NULL)
         {
           this = next ;
           next = this->u.next ;
           base = &new_bases[this->hash % new_base_count] ;
           this->u.next = *base ;
           *base = this ;
         } ;
    } ;

  /* Release the old chain bases, and we're done.       */
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

/*------------------------------------------------------------------------------
 * Return chain base for given hash value.
 */
inline static symbol*
symbol_base(symbol_table table, symbol_hash_t hash)
{
  return &table->bases[symbol_base_index(table, hash)] ;
} ;

/*==============================================================================
 * Symbol operations
 */

/*------------------------------------------------------------------------------
 * Look-up name in given symbol table.  Add if required.
 *
 * Returns NULL if not found and not required to add.
 *
 * If adds, the symbol returned will have a NULL value.  That value MUST be
 * set BEFORE any further symbol table operations, and that value MUST have
 * the same "name" value as given here.
 *
 * If does not add, and symbol exists, the symbol returned will NOT have a NULL
 * value.  The function symbol_is_set() will return false for an unset symbol
 * (which is still in the symbol table because there is at least one remaining
 * reference to it).
 *
 * NB: the name argument is passed to the symbol table's hash function.
 */
extern symbol
symbol_lookup(symbol_table table, const void* name, add_b add)
{
  struct symbol*  this ;
  struct symbol** base ;
  symbol_hash_t   hash ;

  qassert((table != NULL) && (table->bases != NULL)) ;

  hash = table->func.hash(name) ;

  base = symbol_base(table, hash) ;
  this = *base ;
  while (this != NULL)
    {
      qassert((this->table == table) && (this->value != NULL)) ;
      if ((this->hash == hash) && (table->func.cmp(this->value, name) == 0))
        return this ;

      this = this->u.next ;
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
   */
  if ((*base != NULL) && (table->entry_count > table->extend_thresh))
    {
      symbol_extend_bases(table) ;
      base = symbol_base(table, hash) ;
    } ;

  /* Third, chain in the new entry, count it in and return              */
  this->u.next = *base ;
  *base = this ;

  ++table->entry_count ;

  return this ;
} ;

/*==============================================================================
 * Symbol Value handling.
 */

static void symbol_do_tell(symbol sym, symbol_change_t ch) ;
static void symbol_remove(symbol sym, free_keep_b free) ;
inline static bool symbol_redundant(symbol sym) ;
inline static void symbol_remove_if_redundant(symbol sym) ;

/*------------------------------------------------------------------------------
 * Set the given symbol's value -- must NOT be NULL.
 *
 * Sets the new value, sets the symbol_is_set_bit and then invokes the
 * symbol_tell_func, if required.
 *
 * May set the symbol any number of times, but each time the "name" MUST be the
 * same as the original name.
 *
 * A set symbol may be unset or deleted.
 *
 * An unset symbol may be set again.  A deleted symbol may not.
 *
 * Returns:  the old value (NULL if setting initial value)
 */
extern void*
symbol_set(symbol sym, void* value)
{
  void* old_value ;

  qassert((sym->table != NULL) && (value != NULL)) ;

  old_value = sym->value ;

  sym->value      = value ;
  sym->ref_count |= symbol_is_set_bit ;

  if (sym->table->func.tell != NULL)
    symbol_do_tell(sym, sym_set) ;

  return old_value ;
} ;

/*------------------------------------------------------------------------------
 * Unsets the given symbol's value -- must NOT be NULL.
 *
 * Sets the new value, clears the symbol_is_set_bit and then invokes the
 * symbol_tell_func, if required.  The new value may be a "cut down" value, but
 * it MUST contain the current "name" and symbol_cmp_func must still work.  The
 * new value should be set "empty" or "undef" in some way which is obvious to
 * anyone if the symbol and its value is used later.
 *
 * After the symbol_tell_func (if any) returns:
 *
 *   * If there are no references, the symbol will be immediately removed from
 *     the symbol table, and it and the *new* value will be freed.  So the
 *     symbol may disappear immediately, taking the new value with it.
 *
 *   * If there is at least one reference, the symbol remains in the symbol
 *     table, marked unset, until:
 *
 *       * symbol_set() is used to set a value again
 *
 *       * the number of references is reduced to zero -- in which case the
 *         symbol is removed and it and the value freed.
 *
 *       * symbol_delete() is used to remove it from the symbol table.
 *
 *     While the symbol remains, the function symbol_is_set() will return false
 *     and symbol_is_unset() will return true.
 *
 *     Note that if the caller has some way of knowing that the value has not
 *     yet been freed, then the value may safely point to the symbol -- because
 *     when the symbol is freed the value is also freed.
 *
 * After symbol_unset() the caller must assume that the symbol and its new
 * value are, or will be at some time, removed from the symbol table and freed
 * (unless the caller holds a reference).
 *
 * The straightforward approach to unsetting a value is to release all
 * possible memory so that the value has the minimum footprint, subject to
 * the need for the symbol_cmp_func to work, and then forget both the
 * value and the symbol once symbol_unset() has been called.  The value will
 * be freed if/when there are no references, or may be "rediscovered" by
 * symbol_lookup().
 *
 * Returns:  the old value iff different from the new one
 *
 * NB: it is the caller's responsibility to free the old value, if required.
 */
extern void*
symbol_unset(symbol sym, void* value)
{
  void* old_value ;

  qassert((sym->table != NULL) && (value != NULL)) ;

  old_value = sym->value ;
  if (old_value == value)
    old_value = NULL ;
  else
    sym->value = value ;

  sym->ref_count &= ~(symbol_ref_count_t)symbol_is_set_bit ;

  if (sym->table->func.tell != NULL)
    symbol_do_tell(sym, sym_unset) ;

  symbol_remove_if_redundant(sym) ;

  return old_value ;
} ;

/*------------------------------------------------------------------------------
 * Delete symbol -- new value may be NULL.
 *
 * Sets the new value, clears the symbol_is_set_bit and then invokes the
 * symbol_tell_func, if required.  The new value may be a "cut down" value, as
 * in symbol_unset() above.
 *
 * The new value may be NULL -- it is up to the caller to cope with that.  (The
 * symbol table has no further interest in the value.)
 *
 * Note that the symbol_tell_func is called with the new value and in unset
 * state, but before it is removed from the symbol table -- so the symbol will
 * not show as symbol_is_deleted() (yet).
 *
 * After the symbol_tell_func (if any) returns, the symbol is removed from
 * the symbol table, and then:
 *
 *   * If there are no references, the symbol and the *new* value will be freed.
 *     So the symbol may disappear immediately, taking the new value with it.
 *
 *   * If there is at least one reference, the symbol remains, as an orphan,
 *     until the number of references is reduced to zero -- in which case the
 *     symbol is removed and it and the value freed.
 *
 * After symbol_delete(), the caller should forget about the symbol and
 * its value, leaving it up to the reference system to sweep up, if required.
 *
 * NB: this operation leaves any existing references pointing at the orphan
 *     symbol.  If a new symbol is created, with the same name, those
 *     references will NOT then point at the new value.
 *
 * NB: can use use symbol_delete() thus:
 *
 *       val = symbol_delete(sym, NULL) ;
 *
 *     which removes the symbol from the table, sets the value NULL and returns
 *     the value as was.  The symbol_free_func will not be called.
 *
 *     This works particularly well if there are no references, or the
 *     referencers can cope with a NULL value.
 *
 * Returns:  the old value iff different from the new one
 *
 * NB: it is the caller's responsibility to free the old value, if required.
 */
extern void*
symbol_delete(symbol sym, void* value)
{
  void* old_value ;

  old_value = sym->value ;
  if (old_value == value)
    old_value = NULL ;
  else
    sym->value = value ;

  sym->ref_count &= ~(symbol_ref_count_t)symbol_is_set_bit ;

  if ((sym->table != NULL) && (sym->table->func.tell != NULL))
    symbol_do_tell(sym, sym_delete) ;

  symbol_remove(sym, symbol_redundant(sym)) ;

  return old_value ;
} ;

/*------------------------------------------------------------------------------
 * Invoke the call-back -- there must be one !
 *
 * If there are one or more symbol_ref objects, then sets up a symbol_ref
 * walker and passes it to the symbol_tell_func, which may:
 *
 *   symbol_ref  ref ;
 *   while ((ref = symbol_ref_walk_step(walk)) != NULL)
 *      .... whatever
 *
 * to walk all the references.  The symbol_tell_func does not need to do the
 * symbol_ref_walk_start() or symbol_ref_walk_end() -- that is taken care of,
 * here.
 *
 * NB: the symbol_tell_func may set/unset references, but may NOT change the
 *     value.
 *
 * NB: arranges for symbol NOT to be removed, even if references drop to
 *     zero -- caller must deal with that.
 */
static void
symbol_do_tell(symbol sym, symbol_change_t ch)
{
  symbol_ref  walk ;
  struct symbol_ref walk_s ;

  qassert((sym->table != NULL) && (sym->table->func.tell != NULL)) ;

  sym->ref_count += symbol_ref_increment ;      /* hold         */

  if (sym->ref_list == NULL)
    walk = NULL ;
  else
    symbol_ref_walk_start(sym, (walk = &walk_s)) ;

  sym->table->func.tell(sym, ch, walk) ;

  if (walk != NULL)
    symbol_ref_walk_end(walk) ;

  sym->ref_count -= symbol_ref_increment ;      /* release      */
} ;

/*------------------------------------------------------------------------------
 * Remove symbol from its symbol table (if any) and if required free the symbol
 * and any value.
 *
 * If symbol is not freed, it is left deleted, to be tidied up when there are
 * no references left.
 *
 * NB: symbol MUST be unset (and may already be deleted) !
 */
static void
symbol_remove(symbol sym, free_keep_b free)
{
  symbol_table  table ;
  symbol*       base ;
  symbol        prev ;

  qassert(symbol_is_unset(sym)) ;

  table = sym->table ;
  if (table != NULL)            /* Deleted symbols have no parent table.  */
    {
      assert(table->entry_count != 0) ;

      base = symbol_base(table, sym->hash) ;
      if (*base == sym)
        *base = sym->u.next ;
      else
        {
          prev = *base ;
          while (1)
            {
              assert(prev != NULL) ;

              if (prev->u.next == sym)
                break ;

              prev = prev->u.next ;
            } ;
          prev->u.next = sym->u.next ;
        } ;

      sym->table = NULL ;       /* Symbol is now an orphan.             */
      --table->entry_count ;

      sym->u.free = table->func.free ;
                                /* Need to keep a hold of this          */
    } ;

  confirm(free_it) ;    /* true => free it      */
  if (free)
    {
      if (sym->value != NULL)
        sym->u.free(sym->value) ;

      XFREE(MTYPE_SYMBOL, sym) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Return true iff the symbol is not set and there are no references to it.
 *
 * Note that the LS bit of the ref_count is the "symbol is set bit".
 */
inline static bool
symbol_redundant(symbol sym)
{
  confirm(symbol_is_set_bit == 1) ;

  return (sym->ref_count == 0) && (sym->ref_list == NULL) ;
} ;

/*------------------------------------------------------------------------------
 * Remove symbol and free it and value if it is now redundant.
 */
inline static void
symbol_remove_if_redundant(symbol sym)
{
  if (symbol_redundant(sym))
    symbol_remove(sym, free_it) ;
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
 * Reference count handling.
 *
 *   symbol_inc_ref(sym)  -- declared Inline
 *   symbol_dec_ref(sym)  -- declared Inline
 */

/*------------------------------------------------------------------------------
 * Zeroise the reference count -- count is <= symbol_ref_increment
 */
Private symbol
symbol_zero_ref(symbol sym)
{
  qassert(sym->ref_count == symbol_ref_increment) ;

  sym->ref_count = 0 ;
  symbol_remove_if_redundant(sym) ;

  return NULL ;
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

static inline void symbol_add_ref(symbol sym, symbol_ref ref) ;
static inline void symbol_del_ref(symbol sym, symbol_ref ref) ;

/*------------------------------------------------------------------------------
 * Initialise symbol reference -- allocate if required.
 */
extern symbol_ref
symbol_init_ref(symbol_ref ref)
{
  if (ref == NULL)
    return XCALLOC(MTYPE_SYMBOL_REF, sizeof(struct symbol_ref)) ;
  else
    return memset(ref, 0, sizeof(struct symbol_ref)) ;
} ;

/*------------------------------------------------------------------------------
 * Set symbol reference -- allocate if required (ref == NULL).
 *
 * NB: does nothing if reference already set to the given symbol.
 *
 * NB: unsets (but does not free) reference if was not NULL (and is not
 *     same as symbol being set to) before setting new reference.
 *
 * NB: setting reference to NULL unsets any existing reference, but does NOT
 *     release the reference structure.
 *
 * NB: if reference is allocated, the parent is set NULL and the tag is set
 *     NULL/0.
 *
 *     if reference is not allocated, the parent and tag are unchanged.
 */
extern symbol_ref
symbol_set_ref(symbol_ref ref, struct symbol* sym)
{
  if (ref != NULL)
    {
      if (ref->sym == sym)
        return ref ;      /* Nothing more to do if already set to given value */

      if (ref->sym != NULL)
        symbol_unset_ref(ref, keep_it) ;
    }
  else
    ref = symbol_init_ref(NULL) ;

  ref->sym = sym ;
  if (sym != NULL)
    symbol_add_ref(sym, ref) ;

  return ref ;
} ;

/*------------------------------------------------------------------------------
 * Unset symbol reference.  Free the structure if required.
 *
 * NB: does nothing if address of reference is NULL.
 *
 * NB: if reference is not freed, the parent and tag are unchanged.
 *
 * NB: removing the last reference to an symbol that has been deleted causes
 *     the symbol to be freed.
 *
 * NB: copes if the reference is already unset, of course.
 */
extern symbol_ref
symbol_unset_ref(symbol_ref ref, free_keep_b free_ref_structure)
{
  symbol sym ;

  if (ref == NULL)
    return ref ;

  sym = ref->sym ;
  if (sym != NULL)         /* NULL => reference already unset     */
    {
      symbol_del_ref(sym, ref) ;
      ref->sym = NULL ;

      symbol_remove_if_redundant(sym) ;
    } ;

  confirm(free_it == true) ;
  if (free_ref_structure)
    XFREE(MTYPE_SYMBOL_REF, ref) ;      /* ref is set to NULL */

  return ref ;
} ;

/*------------------------------------------------------------------------------
 * Insert symbol_ref at head of symbol's list of references.
 */
static inline void
symbol_add_ref(symbol sym, symbol_ref ref)
{
  symbol_ref next ;

  next = sym->ref_list ;        /* current first on list, if any        */

  sym->ref_list = ref ;         /* new first on list                    */
  ref->next = next ;            /* point to old first on list           */
  ref->prev = (void*)sym ;      /* mark as first on list                */

  if (next != NULL)
    next->prev = ref ;          /* update old first on list             */
} ;

/*------------------------------------------------------------------------------
 * Clip symbol_ref from symbol's list of references.
 *
 * If symbol_ref has already been deleted the prev pointer is NULL, and this
 * function copes -- and does not need the symbol to be valid (sym may be NULL).
 */
static inline void
symbol_del_ref(symbol sym, symbol_ref ref)
{
  symbol_ref prev = ref->prev ;
  symbol_ref next = ref->next ;

  if (prev != NULL)
    {
      if (prev == (void*)sym)
	{
	  assert(sym->ref_list == ref) ;
	  sym->ref_list = next ;        /* remove from head of list     */
	}
      else
        prev->next = next ;             /* remove from place in list    */

      if (next != NULL)
	next->prev = prev ;             /* update next's prev ptr       */
    } ;

  ref->next = ref->prev = NULL ;
} ;

/*==============================================================================
 * Procedure for walking the references to a symbol:
 *
 *   symbol_ref_t walk[1] ;
 *   symbol       sym ;
 *   symbol_ref   ref ;
 *
 *   symbol_ref_walk_start(sym, walk) ;
 *   while ((ref = symbol_ref_walk_step(walk)) != NULL)
 *   	.... whatever
 *   symbol_ref_walk_end(walk) ;
 *
 *  NB: it is *essential* to call symbol_ref_walk_end() exactly once at some
 *      time after symbol_ref_walk_start.
 *
 * The symbol table walk uses a "bookmark" which is a special from of entry in
 * the symbol's reference list.  This mechanism:
 *
 *   (a) prevents the symbol being freed while the reference walk is in
 *       progress -- that may happen during symbol_ref_walk_end.
 *
 *   (b) allows for the current and other references to be set or unset.
 *
 *       Setting a reference inserts it upstream of the bookmark -- so it will
 *       not be visited during the walk.
 *
 *       Unsetting a reference that has yet to be visited eliminates it from
 *       the walk.
 *
 *       Note that setting a reference to refer to the symbol it already
 *       refers to has no effect at all.
 *
 *   (c) allows the symbol to be defined, undefined or redefined during a
 *       symbol reference walk.
 *
 *       If that triggers another symbol reference walk, then that walk will
 *       proceed until it hits the point reached by the walk it is nested
 *       inside, and then stop.
 *
 *       Suppose the outer walk was dealing with the value having changed from
 *       'A' to 'B'.  The inner walk will do from 'B' to the latest value 'C'
 *       for the references that have already seen 'A' to 'B'.  When the outer
 *       walk resumes, it will deal with the change 'A' to 'C', unaware of the
 *       intermediate step.
 *
 *       If that does not suit, don't fiddle with symbol values during a
 *       symbol reference walk.
 */
static inline bool symbol_ref_is_bookmark(symbol_ref ref) ;

/*------------------------------------------------------------------------------
 * Start walk of symbol references
 *
 * NB: given walk symbol_ref may NOT be NULL.
 */
extern void
symbol_ref_walk_start(symbol sym, symbol_ref walk)
{
  assert(walk != NULL) ;

  symbol_init_ref(walk) ;         /* keeping things tidy              */
  walk->sym     = (void*)walk ;   /* bookmark signature               */
  walk->parent  = sym ;
  symbol_add_ref(sym, walk) ;     /* insert bookmark at head of list  */
} ;

/*------------------------------------------------------------------------------
 * Step walk and return the next reference (if any).
 */
extern symbol_ref
symbol_ref_walk_step(symbol_ref walk)
{
  symbol_ref next_ref ;

  assert(symbol_ref_is_bookmark(walk)) ;	/* must be a bookmark !   */

  /* Pick up reference following the bookmark, before deleting it.        */
  next_ref = walk->next ;
  symbol_del_ref((symbol)walk->parent, walk) ;

  /* Stop immediately if bookmark was at the end of the list or the next  */
  /* item is a bookmark (for a walk that started earlier).                */
  if ((next_ref == NULL) || symbol_ref_is_bookmark(next_ref))
    return NULL ;

  /* Now we move the bookmark from where it is now to after next_ref.   */

  walk->next     = next_ref->next ;
  next_ref->next = walk ;
  walk->prev     = next_ref ;
  if (walk->next != NULL)
    walk->next->prev = walk ;

  /* Return the next real reference to be processed.  */
  return next_ref ;
} ;

/*------------------------------------------------------------------------------
 * End of symbol reference walk.
 *
 * NB: if the symbol is not set and has no references or bookmarks it will
 *     now be removed from the symbol table, and it and the value freed.
 */
extern void
symbol_ref_walk_end(symbol_ref walk)
{
  assert(symbol_ref_is_bookmark(walk)) ;  /* must be a bookmark ! */

  symbol_del_ref((symbol)(walk->parent), walk) ;  /* make sure */

  symbol_remove_if_redundant((symbol)(walk->parent)) ;
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
symbol_ref_is_bookmark(symbol_ref ref)
{
  return (void*)ref->sym == (void*)ref ;
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
 *   struct symbol_walker walker ;
 *   symbol sym ;
 *   ....
 *   symbol_walk_start(table, &walker) ;
 *   while ((sym = symbol_walk_next(&walker)))
 *     ....
 *
 * NB: it is possible to change the current symbol while the walk is in
 *     progress -- up to and including deleting it.  Any other changes to
 *     the table must NOT be attempted.
 */
extern void
symbol_walk_start(symbol_table table, symbol_walker walk)
{
  assert(walk != NULL) ;

  walk->next       = NULL ;
  walk->base       = table->bases ;
  walk->base_count = table->base_count ;
} ;

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

  walk->next = this->u.next ;
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
	  sym = sym->u.next ;
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
