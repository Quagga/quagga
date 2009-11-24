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

#include <zebra.h>
#include <stddef.h>

#include "symtab.h"
#include "memory.h"

/* A symbol table maps symbol "names" to symbol values and, for each symbol,
 * has two ways of keeping track of references to the symbol.
 *
 * The symbol name can be an arbitrary collection of bytes, or a string.  All
 * names in a symbol table are unique.
 *
 * The symbol value is a void* -- whose contents are no concern of this code.
 *
 * A symbol table comprises:
 *
 *   * symbol table structure   -- containing all "red-tape"
 *   * array of chain-bases     -- for the hash table
 *   * symbol entries           -- each containing name and value of a symbol
 *
 * The symbol table structure may be statically allocated, embedded in another
 * structure, or allocated dynamically.  In any case the symbol table operations
 * require the address of the symbol table structure -- see typedef for
 * symbol_table.
 *
 * A symbol table may point to its "parent" -- which could be an enclosing
 * structure, or any other higher level data.  The symbol table code does not
 * use or need this -- it is for the convenience of the caller.
 *
 * A symbol table structure which is zeroised is implicitly an empty symbol
 * table, using the default symbol name type -- a null terminated string.
 *
 * Each Symbol Table requires a hash function, which takes a pointer to
 * whatever and returns a 32-bit hash value and a canonical form of the
 * symbol "name".  When a new symbol is created the canonical form of the name
 * is copied to the symbol entry.
 *
 * The array of chain-bases is dynamically allocated and will grow to maintain
 * an approximate given maximum number of symbols per chain base.  This density
 * is set when the symbol table is initialised.  The array does not
 * automatically reduce in size.
 *
 * The number of chain bases is always odd.  The hash function returns a 32-bit
 * unsigned value, which is mapped to the chain bases modulo their number.
 * Since that is odd, it is co-prime with the hash, so contributes to the hash
 * process.
 *
 * Each symbol in the table has a dynamically allocated entry, which includes:
 *
 *   * symbol value   -- void*
 *   * symbol name
 *   * count of references
 *   * list of references
 *
 * Symbols may have the value NULL.  Deleting a symbol is not the same as
 * setting its value to NULL.  If the value is set NULL and later set to
 * something else, all references to the symbol will see the new value.  If the
 * symbol is deleted, all references will see its value set to NULL, but if a
 * new symbol with the same name is later created, all references to the old
 * symbol are unchanged, and continue to see the (orphan) NULL.
 *
 * Keeping track of references is important because it ensures that symbols
 * can be deleted without leaving dangling references.  When a symbol is
 * deleted, it is removed from the symbol table and its value is set to NULL.
 * If there are no references to the symbol, the symbol entry is released.
 * If there are references to the symbol, it is preserved (as an orphan) until
 * all references are unset.  The value of an orphaned symbol should not be
 * changed.
 *
 * There are two, parallel mechanisms for keeping track of references to a
 * symbol:
 *
 *   1. reference count -- this is for when all that is required is a pointer
 *      to the symbol, ie:
 *
 *       * ptr_to_symbol = symbol_inc_ref(sym)  -- set reference & count up
 *       * ptr_to_symbol = symbol_dec_ref(sym)  -- unset reference & count down
 *
 *   2. list of references -- this is for when it is useful to visit all
 *      references to a given symbol, for example when the value of the symbol
 *      is set and all users of the symbol need to adjust to the new state.
 *
 *      A symbol reference contains a pointer to the symbol, and lives on the
 *      list of references to the symbol.  When the value is set, a call-back
 *      associated with the table is called (if it is set).  That call-back may
 *      walk the list of references to the symbol.  Each reference contains a
 *      pointer and a pointer/long int value, which may be used to identify the
 *      reference.
 *
 *      A symbol reference may be statically allocated, embedded in another
 *      structure, or allocated dynamically.  In any case the symbol reference
 *      operations require the address of the symbol reference structure -- see
 *      typedef for symbol_ref.
 *
 * Symbol references are the responsibility of of the owner of the reference,
 * who must look after setting, unsetting and (if required) releasing them.
 *
 * It may seem profligate to have two mechanisms.  However, it is simpler than
 * having two types of symbol, or two types of symbol table.  It may also be
 * useful to have both simple references and references for dealing with
 * value changes.  Suppose, for instance, that the value of a symbol has a
 * pointer to the symbol -- so that the value of the symbol can refer back to
 * its name, for example.  This requires only a simple reference, even if
 * more generally a list of references is required.
 *
 * A symbol table can be walked to visit all symbols, and there is support for
 * extracting a vector of all symbols which satisfy a given criterion.
 */

static void symbol_extend_bases(symbol_table table) ;
static void symbol_free(symbol sym) ;

/* Return true iff there are no references to the given symbol  */
inline static int
symbol_no_references(symbol sym)
{
  return (sym->ref_count == 0) && (sym->ref_list == NULL) ;
} ;

/* If symbol is an orphan with no references/bookmarks, free it.
 *
 * NB: if symbol is an orphan, then it implicitly has a NULL value, because
 *     to become an orphan it must have been deleted, which unsets the value.
 */
inline static void
symbol_free_if_redundant(symbol sym)
{
  if ((sym->table == NULL) && symbol_no_references(sym))
    symbol_free(sym) ;
} ;

/* Return chain base for given hash value.      */
inline static symbol*
symbol_base(symbol_table table, u_int32_t hash)
{
  return &table->bases[hash % table->base_count] ;
} ;

/* Initialise a new symbol table -- allocate if required.
 *
 * table      -- address of table to initialise.  NULL -> allocate.
 *               NB: if allocated, it is the caller's responsibility to free.
 *
 * parent     -- address of some parent or other higher level data structure.
 *               This is not used by the symbol table code and may be NULL if
 *               the caller has no use for it.
 *
 * bases      -- number of list bases to start the symbol table at.
 *               Symbol table grows as required, but can set initial size if
 *               have some expectations and wish to avoid growth steps.
 *
 * density    -- %-age of entries/bases.  0 => use default.
 *
 * hash_function
 *            -- function to fill in symbol_hash from a given "name".
 *               see: description of struct symbol_hash and default function,
 *               symbol_hash_string, below.
 *               NULL => the names in this symbol table are null terminated
 *                       strings, so use symbol_hash_string.
 *
 * value_call_back
 *            -- function to be called when the symbol value is set or unset.
 *               (Or the symbol is about to be deleted -- which starts by
 *               unsetting it).
 *
 *               The function is passed the new state of the symbol and its
 *               previous value.
 *
 *               The value of the symbol is a pointer to some data.  If the
 *               data changes symbol_set_value() must be called if its
 *               address changes and may be called even if its address doesn't
 *               change.
 *
 *               In any event, value_call_back is called when symbol_set_value()
 *               is called -- except where the symbol is being set to NULL and
 *               the value is already NULL.
 *
 *               During the call-back the symbol may be set again or unset,
 *               which will cause the call-back to be called inside itself.
 *               Also, references may be set or unset.
 *
 *               In the call-back the reference list may be walked to signal
 *               to all holders of the reference that something has changed.
 *
 * NB: A completely zeroized symbol_table structure is a valid, empty
 *     symbol table.  The first time it is used it will be set up to default
 *     state -- in particular: symbol names are null terminated strings (a
 *     state which *cannot* then be changed).
 *
 * NB: when this is used to re-initialising an existing symbol table structure,
 *     any existing chain base array, symbols and symbol references are simply
 *     discarded -- which will leak memory and is probably a mistake.
 */
symbol_table
symbol_table_init_new(symbol_table table,
		      void* parent,
		      unsigned int base_count,
		      unsigned int density,
		      symbol_hash_function*      hash_function,
		      symbol_call_back_function* value_call_back)
{
  assert(base_count <= SYMBOL_TABLE_BASES_MAX) ;

  if (table == NULL)
    table = XCALLOC(MTYPE_SYMBOL_TABLE, sizeof (struct symbol_table)) ;

  table->parent        = parent ;

  table->bases         = NULL ;		/* Allocated when required	*/
  table->base_count    = base_count ;

  table->entry_count   = 0 ;
  table->extend_thresh = density ;	/* Fixed up when required	*/

  table->hash_function = hash_function ;
  symbol_table_set_value_call_back(table, value_call_back) ;

  return table ;
} ;

/* Set "parent" of symbol table.  */
void
symbol_table_set_parent(symbol_table table, void* parent)
{
  table->parent = parent ;
} ;

/* Get "parent" of symbol table.  */
void*
symbol_table_get_parent(symbol_table table)
{
  return table->parent ;
} ;

/* Set the value_call_back                                                */
void
symbol_table_set_value_call_back(symbol_table table,
                                 symbol_call_back_function* value_call_back)
{
  table->value_call_back = value_call_back ;
} ;

/* Create and set new chain bases and threshold for next extension.       */
/*                                                                        */
/* Ensures that the base count is at least the minimum and is odd,        */
/* and returns the value set.                                             */
static unsigned int
symbol_table_new_bases(symbol_table table,
                                     unsigned int new_base_count, float density)
{
  if (new_base_count < SYMBOL_TABLE_BASES_MIN)
    new_base_count = SYMBOL_TABLE_BASES_MIN ;
  new_base_count |= 1 ;

  table->bases = XCALLOC(MTYPE_SYMBOL_BASES, new_base_count * sizeof(symbol)) ;
  table->extend_thresh     = new_base_count * density ;
  return table->base_count = new_base_count ;
} ;

/* Setup symbol table body for use.
 *
 * Used for "lazy" allocation of chain bases and allows symbol_lookup
 * to operate on a completely zeroized symbol_table structure.
 */
static void
symbol_table_setup(symbol_table table)
{
  float density ;

  /* If density was set explicitly, extend_thresh entry is a %age.      */

  if (table->extend_thresh != 0)
    density = (float)table->extend_thresh / (float)100 ;
  else
    density = (float)2 ;        /* Default density                      */

  /* Initialise the chain bases -- enforces minimum base_count and odd-ness */
  symbol_table_new_bases(table, table->base_count, density) ;

  /* make default hash_function explicit.                               */
  if (table->hash_function == NULL)
    table->hash_function = (symbol_hash_function*)symbol_hash_string ;
} ;

/* Reset symbol table.
 *
 * Free the symbol table body, and free the symbol table structure or reset it.
 *
 * Return NULL if frees symbol table structure, otherwise the address of same.
 *
 * NB: must only be done when the table is empty -- see assertion !
 */
symbol_table
symbol_table_reset(symbol_table table, int free_structure)
{
  if (table== NULL)
    return NULL ;	/* allow for already freed table	*/

  assert(table->entry_count == 0) ;

  if (table->bases)
    XFREE(MTYPE_SYMBOL_BASES, table->bases);

  if (free_structure)
    {
      XFREE(MTYPE_VECTOR, table) ;
      return NULL ;
    }
  else
    return memset(table, 0, sizeof(struct symbol_table)) ;
} ;

/* Remove symbol from its symbol table (if any).                        */

static void
symbol_remove(symbol sym)
{
  symbol_table  table ;
  symbol*       base ;
  symbol        prev ;

  table = sym->table ;
  if (table != NULL)            /* Deleted symbols have no parent table.  */
    {
      assert(table->entry_count != 0) ;

      base = symbol_base(table, sym->hash) ;
      if (*base == sym)
        *base = sym->next ;
      else
        {
          prev = *base ;
          while (prev->next != sym)
            prev = prev->next ;
          prev->next = sym->next ;
        } ;

      sym->table = NULL ;       /* Symbol is now an orphan.               */
      --table->entry_count ;
    } ;
} ;

/* Free symbol, removing it from the symbol table.
 *
 * NB: the value and all references MUST already have been unset, because:
 *
 *      * any value may well need to be released, and have no idea how to do
 *        that here.
 *
 *      * similarly, references may need to be released and should not, in
 *        any case, be left dangling.
 */
static void
symbol_free(symbol sym)
{
  assert((sym->value == NULL) && symbol_no_references(sym)) ;

  symbol_remove(sym) ;          /* Remove from table, if any.   */

  XFREE(MTYPE_SYMBOL, sym) ;
} ;

/* Ream out symbols.
 *
 * Delete symbols -- but do not invoke the value_call_back.
 *
 * When the table is (or becomes) empty, the chain bases are freed, and the
 * structure freed or reset (depending on the free_structure argument).
 *
 * This is intended for use when the symbol table is being destroyed, and all
 * references have been, or will be unset.
 *
 * Returns the value of the next non-NULL symbol (if any).  So may be used, for
 * example:
 *
 *    xxxx* val ;
 *    ...
 *    while ((val = symbol_table_ream(table, free_structure)) != NULL)
 *      {
 *         ... do what's required to release the value ...
 *      }
 *
 * Noting that the symbol may already have been released when its value is
 * returned.  (If the symbol is required when the value is released, then the
 * value should hold a simple reference to the symbol.)
 *
 * Returns NULL when the table is empty.
 *
 * Symbols which have one or more references when they are deleted are left as
 * orphans, which will be freed when all their references are unset.
 *
 * NB: do NOT attempt to do anything else with the symbol table once reaming
 *     has started.
 *
 * NB: it is the caller's responsibility to unset all references and release
 *     any that need to be released -- either before or after this operation.
 */
void*
symbol_table_ream(symbol_table table, int free_structure)
{
  void* value ;
  symbol sym ;
  unsigned int i ;

  i = table->base_count ;

  while (i--)
    {
      while ((sym = table->bases[i]) != NULL)
        {
          assert(table->entry_count != 0) ;

          /* the following is effectively symbol_delete, but avoids the   */
          /* value_call_back and returns only if the value is not NULL.   */

          table->bases[i] = sym->next ;   /* remove from table            */
          --table->entry_count ;          /* count down                   */

          sym->table = NULL ;             /* orphan symbol                */
          value = sym->value ;            /* pick up value.               */
          sym->value = NULL ;             /* and set symbol undefined     */

          if (symbol_no_references(sym))
            symbol_free(sym) ;  /* not in table, no value, no references  */

          if (value != NULL)
            {
              table->base_count = i + 1 ; /* where we've got to           */
              return value ;  /* <<< RETURN: caller must deal with value  */
            } ;
        } ;
    } ;

  symbol_table_reset(table, free_structure) ;

  return NULL ;
} ;

/* Look-up name in given symbol table.  Add if required.
 *
 * Returns NULL if not found and not required to add.
 *
 * NB: the name argument is passed to the symbol table's hash function.  That
 *     function is required to return with a 32-bit hash
 *
 * NB: if required to add, the caller cannot distinguish between a symbol
 *     which did not previously exist, and one which did exist but had no
 *     value and no references.  Where that distinction matters, it is
 *     necessary to do an extra lookup.
 */
symbol
symbol_lookup(symbol_table table, const void* name, int add)
{
  struct symbol*  this ;
  struct symbol** base ;
  struct symbol_hash hash ;

  assert(table != NULL) ;
  if (table->bases == NULL)
    symbol_table_setup(table) ;	  /* Lazy allocation of chain bases etc.  */

  table->hash_function(&hash, name) ;

  base = symbol_base(table, hash.hash) ;
  this = *base ;
  while (this)
    {
      if ((this->hash == hash.hash)
	    && (this->name_len == hash.name_len)
	    && (memcmp(this->name, hash.name, this->name_len) == 0))
        return this ;
      this = this->next ;
    } ;

  /* Not found -- quit now if not required to add */
  if (!add) return NULL ;

  /* Adding: first, carve a new, empty symbol entry           */
  this = XCALLOC(MTYPE_SYMBOL, sizeof(struct symbol) + hash.name_copy_len) ;

  this->table     = table ;
  this->value     = NULL ;
  this->ref_list  = NULL ;
  this->ref_count = 0 ;
  this->hash      = hash.hash ;
  this->name_len  = hash.name_len ;
  memcpy(this->name, hash.name, hash.name_copy_len) ;

  /* Second, if required, extend the array of list bases.  We extend if     */
  /* we have a collision *and* we exceed threshold of number of entries.    */
  if ((*base != NULL) && (table->entry_count > table->extend_thresh))
    {
      symbol_extend_bases(table) ;
      base = symbol_base(table, hash.hash) ;
    } ;

  /* Third, chain in the new entry, count it in and return  */
  this->next = *base ;
  *base = this ;

  ++table->entry_count ;

  return this ;
} ;

/* Delete symbol.
 *
 * The first effect of this is to set the symbol value to NULL, which may
 * trigger a value_call_back etc.
 *
 * Then the symbol is removed from the table (and the symbol becomes an orphan).
 *
 * Then, if there are no (remaining) references the symbol is freed.  Otherwise
 * the symbol entry remains in existence until there are no more references
 * (at which point it will finally be destroyed).
 *
 * Returns the last value of the symbol -- which may itself need to be
 * destroyed -- noting that the symbol may already have been released.  (If the
 * symbol is required when the value is released, then the value should hold a
 * simple reference to the symbol.)
 *
 * NB: the effect of deleting a symbol is to leave all remaining references
 *     pointing at an NULL value, orphaned symbol.
 *
 *     If a new symbol is created with the same name, that will be a
 *     completely different symbol -- references to the old symbol will
 *     continue to be to the vestigial NULL value.
 *
 *     This is different from setting the symbol value to NULL and later
 *     giving it a new value.
 *
 * NB: orphan symbols can be deleted.  The effect is to free the symbol if
 *     possible.
 */
void*
symbol_delete(symbol sym)
{
  void* old_value = symbol_unset_value(sym) ;

  if (symbol_no_references(sym))
    symbol_free(sym) ;     /* free symbol now if no references              */
  else
    symbol_remove(sym) ;   /* else just remove it from the table -- will be */
                           /* freed when all references are unset.          */
  return old_value ;
} ;

/* The hash functions provided here use CRC32 as a hash.
 *
 * CRC32 is not intended as a hash function, and is not a perfect one.
 * However it is fast -- requiring a few simple operations per byte.  Taken
 * with the secondary effect of using the hash produced modulo an odd number,
 * experience suggests this is sufficient.
 */

static u_int32_t crc_table[] ;

/* Standard symbol string hash function.  */
void
symbol_hash_string(symbol_hash p_hash, const char* string) {
  u_int32_t  h = 0 ;
  const char* p = string ;

  while (*p != 0)
    h = crc_table[(h & 0xFF) ^ (u_int8_t)*p++] ^ (h >> 8) ;

  assert((p - string) < 0xFFFF) ;

  p_hash->hash          = h ;
  p_hash->name          = string ;
  p_hash->name_len      = (p - string) ;
  p_hash->name_copy_len = p_hash->name_len + 1 ;
} ;

/* Standard symbol byte vector hash function.  */
void
symbol_hash_bytes(symbol_hash p_hash, const void* bytes, size_t len) {
  assert(len < 0xFFFF) ;

  u_int32_t  h = len ;		/* So strings of zeros don't CRC the same ! */
  const u_int8_t*  p = bytes ;
  const u_int8_t*  e = p + len ;

  while (p < e)
    h = crc_table[(h & 0xFF) ^ *p++] ^ (h >> 8) ;

  p_hash->hash          = h ;
  p_hash->name          = (const void*)bytes ;
  p_hash->name_len      = len ;
  p_hash->name_copy_len = len ;
} ;

/* Extend the array of list bases. */
static void
symbol_extend_bases(symbol_table table)
{
  symbol  this ;
  symbol  next ;
  symbol* old_bases ;
  symbol* new_bases ;
  symbol* base ;
  unsigned int	new_base_count ;
  unsigned int	old_base_count ;

  old_bases      = table->bases ;
  old_base_count = table->base_count ;

  assert((old_bases != NULL) && (old_base_count != 0)) ;

  /* TODO: should look out for overflowing base_count and requiring     */
  /*       impossible amounts of memory ?!                              */

  new_base_count = (table->base_count | 1) - 1 ; /* trim enforced odd-ness */

  if (new_base_count <= SYMBOL_TABLE_BASES_DOUBLE_MAX)
    new_base_count *= 2 ;
  else
    new_base_count += SYMBOL_TABLE_BASES_DOUBLE_MAX ;

  new_base_count = symbol_table_new_bases(table, new_base_count,
                              (float)table->extend_thresh / table->base_count) ;

  /* Rehome everything on the new chain bases.          */
  new_bases = table->bases ;
  while (old_base_count--)
    {
       next = old_bases[old_base_count] ;
       while (next != NULL)
         {
           this = next ;
           next = this->next ;
           base = &new_bases[this->hash % new_base_count] ;
           this->next = *base ;
           *base = this ;
         } ;
    } ;

  /* Release the old chain bases, and we're done.       */
  XFREE(MTYPE_SYMBOL_BASES, old_bases) ;
} ;

/*==============================================================================
 * Reference count handling.
 *
 *   symbol_inc_ref(sym)  -- declared Inline
 *   symbol_dec_ref(sym)  -- declared Inline
 */

/* Zeroise the reference count.*/

void symbol_zero_ref(symbol sym, int force)
{
  assert((sym->ref_count == 1) || force) ;

  sym->ref_count = 0 ;
  symbol_free_if_redundant(sym) ;
} ;

/*==============================================================================
 * Reference list handling.
 *
 * References are added at the head of the list -- which is significant when
 * adding references during a symbol reference walk.
 */

/* Insert symbol_ref at head of symbol's list of references.  */
static inline void
symbol_add_ref(symbol sym, symbol_ref ref)
{
  symbol_ref next = sym->ref_list ;
  sym->ref_list = ref ;
  if (next)
    next->prev = ref ;
  ref->next = next ;
  ref->prev = (void*)sym ;	/* marker for first on list	*/
} ;

/* Clip symbol_ref from symbol's list of references.
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
	  sym->ref_list = next ;
	}
      else
        prev->next = next ;

      if (next != NULL)
	next->prev = prev ;
    } ;
  ref->next = ref->prev = NULL ;
} ;

/*==============================================================================
 * The value_call_back handling and symbol reference list walking.
 *
 * If there is one, the value_call_back function is called when the value of
 * a symbol is set -- except when it is set NULL and is already NULL.  Note
 * that setting the same non-NULL value *does* invoke the value_call_back.
 *
 * The value_call_back function is passed the current state of the symbol,
 * complete with new value, and the old value of the symbol.
 *
 * During the value_call_back the symbol reference list may be walked, so that
 * users of the value may be updated.
 *
 * During the value_call_back the symbol may be set, unset or deleted, and
 * references added or taken away.  This may cause nested calls of the
 * call-back.  Note that each call-back holds a reference to the symbol, so if
 * the symbol is deleted it won't be freed until the outermost call-back
 * returns.
 *
 * Procedure for walking the references to a symbol:
 *
 *   struct symbol_ref  walk ;
 *   symbol      sym ;
 *   symbol_ref  ref ;
 *   symbol_ref_walk_start(sym, &walk) ;
 *   while ((ref = symbol_ref_walk_step(&walk)) != NULL)
 *   	.... whatever
 *   symbol_ref_walk_end(&walk) ;
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

/* Bookmarks are symbol_ref structures, distinguished from ordinary symbol_ref
 * structures by setting the sym field to point at the bookmark symbol_ref
 * itself.
 *
 * (It would be nicer to use the parent field for this... but what is put
 *  there in ordinary symbol_ref structures is not guaranteed...)
 */
static inline int
symbol_ref_is_bookmark(symbol_ref ref)
{
  return (void*)ref->sym == (void*)ref ;
} ;

/* Start walk of symbol references */
void
symbol_ref_walk_start(symbol sym, symbol_ref walk)
{
  symbol_init_ref(walk) ;         /* keeping things tidy              */
  walk->sym     = (void*)walk ;   /* bookmark signature               */
  walk->parent  = sym ;
  symbol_add_ref(sym, walk) ;     /* insert bookmark at head of list  */
} ;

/* Step walk and return the next reference (if any).  */
symbol_ref
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

/* End of symbol reference walk.
 *
 * NB: if the symbol is not defined and has no references or bookmarks it
 *     will now be freed.
 */
void
symbol_ref_walk_end(symbol_ref walk)
{
  assert(symbol_ref_is_bookmark(walk)) ;  /* must be a bookmark ! */

  symbol_del_ref((symbol)(walk->parent), walk) ;  /* make sure */

  symbol_free_if_redundant((symbol)(walk->parent)) ;
} ;

/*==============================================================================
 * Symbol Value handling.
 */

/* Set symbol value.  NB: setting to NULL == symbol_unset_value.
 *                    NB: setting same value as currently looks like a change.
 *                        (except for setting NULL to NULL !)
 *
 * Invokes change call-back, if any -- except when setting to NULL and is
 * already NULL.
 *
 * It is possible for the call-back to set the value again, to unset it, to
 * change references, etc.
 *
 * Returns previous value -- which may require releasing.
 */
void*
symbol_set_value(symbol sym, void* new_value)
{
  void* old_value ;

  assert(sym->table != NULL) ;  /* may not set value for orphan symbol !   */

  old_value  = sym->value ;
  sym->value = new_value ;

  /* Invoke value_call_back (if any).                                      */
  /* Note that the value_call_back may set/unset references and/or         */
  /* define/undefine the value.                                            */
  if (((sym)->table->value_call_back != NULL)
        && ( (new_value != NULL) || (old_value != NULL) ))
    {
      symbol_inc_ref(sym) ;     /* preserve until call-back returns     */
      sym->table->value_call_back(sym, old_value) ;
      symbol_dec_ref(sym) ;     /* may now free if has been deleted     */
    } ;

  return old_value ;
} ;

/*==============================================================================
 * Symbol Reference handling.
 *
 * Implementation note: the next and prev pointers in the symbol_ref structure
 * are significant only if the sym pointer is not NULL.
 */

/* Initialise symbol reference -- allocate if required.   */
symbol_ref
symbol_init_ref(symbol_ref ref)
{
  if (ref == NULL)
    return XCALLOC(MTYPE_SYMBOL_REF, sizeof(struct symbol_ref)) ;
  else
    return memset(ref, 0, sizeof(struct symbol_ref)) ;
} ;

/* Set symbol reference -- allocate if required (ref == NULL).
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
symbol_ref
symbol_set_ref(symbol_ref ref, struct symbol* sym)
{
  if (ref != NULL)
    {
      if (ref->sym == sym)
	return ref ;      /* Nothing more to do if already set to given value */
      if (ref->sym != NULL)
	symbol_unset_ref_keep(ref) ;
    }
  else
    ref = symbol_init_ref(NULL) ;

  ref->sym = sym ;
  if (sym != NULL)
    symbol_add_ref(sym, ref) ;

  return ref ;
} ;

/* Unset symbol reference.  Free the structure if required.
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
symbol_ref
symbol_unset_ref(symbol_ref ref, int free_ref_structure)
{
  if (ref == NULL) return ref ;

  if (ref->sym != NULL)		/* NULL => reference already unset     */
    {
      symbol_del_ref(ref->sym, ref) ;
      symbol_free_if_redundant(ref->sym) ;
      ref->sym = NULL ;
    } ;

  if (free_ref_structure)
    XFREE(MTYPE_SYMBOL_REF, ref) ;	/* ref is set to NULL */

  return ref ;
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

/* Walk the given symbol table.  Usage:
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
void
symbol_walk_start(symbol_table table, struct symbol_walker* walker)
{
  walker->next       = NULL ;
  walker->base       = table->bases ;
  walker->base_count = table->base_count ;
} ;

symbol
symbol_walk_next(struct symbol_walker* walker)
{
  symbol this = walker->next ;

  while (this == NULL)
    {
      if (walker->base_count == 0)
        return NULL ;
      --walker->base_count ;
      this = *(walker->base++) ;
    } ;

  walker->next = this->next ;
  return this ;
} ;

/* Extract Symbols.
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
vector
symbol_table_extract(symbol_table table,
		     symbol_select_cmp* selector, const void* p_val, int most,
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

/* Comparison function to sort names which are a mixture of digits and other
 * characters.
 *
 * This comparison treats substrings of digits as numbers, so "a10" is > "a1".
 */
int
symbol_mixed_name_cmp(const symbol* p_a,
		      const symbol* p_b)
{
  const char* a  = symbol_get_name(*p_a) ;
  const char* b  = symbol_get_name(*p_b) ;
  int la, lb ;

  while (1) {
    if (isdigit(*a) && isdigit(*b))
      {
	char* as ;	/* Required to stop the compiler whining */
	char* bs ;
	unsigned long int na = strtoul(a, (char** restrict)&as, 10) ;
	unsigned long int nb = strtoul(b, (char** restrict)&bs, 10) ;
	if (na != nb)
	  return (na < nb) ? -1 : +1 ;
	a = as ;
	b = bs ;
      }
    else
      {
	if (*a != *b)
	  return (*a < *b) ? -1 : +1 ;
	if (*a == '\0')
	  break ;
	++a ;
	++b ;
      }
  } ;

  /* Looks like the names are equal.
   * But may be different lengths if have number part(s) with leading zeros,
   */

  la = symbol_get_name_len(*p_a) ;
  lb = symbol_get_name_len(*p_b) ;
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
