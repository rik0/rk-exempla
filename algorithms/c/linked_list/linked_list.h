/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#ifndef RK_LINKED_LIST_H
#define RK_LINKED_LIST_H

#include <stdlib.h>

struct rk_dllist_node;

struct rk_dllist_handle {
    struct rk_dllist_node* first;
    struct rk_dllist_node* last;
};

typedef struct rk_dllist_handle rk_dllist[1];
typedef struct rk_dllist_node* rk_dllist_iterator;

void rk_dllist_init(rk_dllist lst);
bool rk_dllist_is_empty(rk_dllist lst);
void rk_dllist_destroy(rk_dllist lst);


struct rk_dllist_node* rk_dllist_push_front(rk_dllist lst,
                                        void* value, size_t size);
struct rk_dllist_node* rk_dllist_push_back(rk_dllist lst,
                                        void* value, size_t size);


typedef enum {
    RK_OK = 0, RK_EEMPTY, RK_EBUFFER
} rk_dllist_error_status;

rk_dllist_error_status rk_dllist_pop_front(rk_dllist lst);
rk_dllist_error_status rk_dllist_pop_back(rk_dllist lst);

rk_dllist_error_status rk_dllist_peek_front_size(rk_dllist lst, size_t* size);
rk_dllist_error_status rk_dllist_peek_back_size(rk_dllist lst,  size_t* size);

rk_dllist_error_status rk_dllist_peek_front(rk_dllist lst, void* buff, size_t max_size);
rk_dllist_error_status rk_dllist_peek_back(rk_dllist lst, void* buff, size_t max_size);

rk_dllist_error_status rk_dllist_peek_front_notsafe(rk_dllist lst, void* buff);
rk_dllist_error_status rk_dllist_peek_back_notsafe(rk_dllist lst, void* buff);

void rk_dllist_iterator_init(rk_dllist_iterator* it, rk_dllist lst);
bool rk_dllist_iterator_has_next(rk_dllist_iterator it);
bool rk_dllist_iterator_has_prev(rk_dllist_iterator it);
rk_dllist_error_status rk_dllist_iterator_inc(rk_dllist_iterator* it);
rk_dllist_error_status rk_dllist_iterator_dec(rk_dllist_iterator* it);

rk_dllist_error_status rk_dllist_iterator_current_size(rk_dllist_iterator it, size_t* size);
rk_dllist_error_status rk_dllist_iterator_current_value(rk_dllist_iterator it, void* buffer, size_t max_size);
rk_dllist_error_status rk_dllist_iterator_current_value_notsafe(rk_dllist_iterator it, void* buffer);

struct rk_dllist_node* rk_dllist_insert_before(rk_dllist_iterator it,
                                            void* value, size_t size);
struct rk_dllist_node* rk_dllist_insert_after(rk_dllist_iterator it,
                                            void* value, size_t size);

rk_dllist_error_status rk_dllist_remove_before(rk_dllist_iterator it);
rk_dllist_error_status rk_dllist_remove_after(rk_dllist_iterator it);

#endif
