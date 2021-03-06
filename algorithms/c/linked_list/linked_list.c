/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#include "util.h"
#include "linked_list.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>


struct rk_dllist_node {
    struct rk_dllist_node* prev;
    struct rk_dllist_node* next;
    size_t size;
    unsigned char value[];
};

static struct rk_dllist_node*
rk_dllist_node_alloc(size_t sz) {
    struct rk_dllist_node* tmp = malloc(sizeof(struct rk_dllist_node) + sz);
    if(tmp != NULL) {
        tmp->size = sz;
        tmp->prev = tmp->next = NULL;
    }
    return tmp;
}

static void
rk_dllist_node_free(struct rk_dllist_node* ch) {
    free(ch);
}

static void
rk_dllist_node_set(struct rk_dllist_node* ch, void* val) {
    memcpy(ch->value, val, ch->size);
}

static void
rk_dllist_node_get(void* val, struct rk_dllist_node* ch) {
    memcpy(val, ch->value, ch->size);
}

static void*
rk_dllist_node_get2(struct rk_dllist_node* ch) {
    void* val = malloc(sizeof(ch->size));
    if(val) {
        memcpy(val, ch->value, ch->size);
    }
    return val;
}

static struct rk_dllist_node*
rk_dllist_node_new(void* val, size_t sz) {
    struct rk_dllist_node* tmp = rk_dllist_node_alloc(sz);
    if(tmp != NULL) {
        rk_dllist_node_set(tmp, val);
    }
    return tmp;
}

void
rk_dllist_node_link_next(struct rk_dllist_node* node,
                         struct rk_dllist_node* next) {
    node->next = next;
    if(next) {
        next->prev = node;
    }
}

void
rk_dllist_node_link_prev(struct rk_dllist_node* node,
                         struct rk_dllist_node* prev) {
    node->prev = prev;
    if(prev) {
        prev->next = node;
    }
}

void
rk_dllist_node_link(struct rk_dllist_node* node,
                    struct rk_dllist_node* prev,
                    struct rk_dllist_node* next) {
    rk_dllist_node_link_next(node, next);
    rk_dllist_node_link_prev(node, prev);
}

/*! Return a copy of node @ch.
 *
 * \warning prev and next field of the returned node are not set.
 */
static struct rk_dllist_node*
rk_dllist_node_copy(struct rk_dllist_node* ch) {
    return rk_dllist_node_new(ch->value, ch->size);
}


void rk_dllist_init(rk_dllist lst) {
    lst->first = lst->last = NULL;
}

bool rk_dllist_is_empty(rk_dllist lst) {
    assert(lst->last == NULL);
    return lst->first == NULL;
}

void rk_dllist_destroy(rk_dllist lst) {
    while(rk_dllist_pop_front(lst));
}

rk_dllist_iterator
rk_dllist_push_front(rk_dllist lst, void* value, size_t size) {
    struct rk_dllist_node* new_node = rk_dllist_node_new(value, size);
    if(new_node) {
        rk_dllist_node_link(new_node, NULL, lst->first);
        lst->first = new_node;
    }
    return new_node;
}

rk_dllist_iterator
rk_dllist_push_back(rk_dllist lst, void* value, size_t size) {
    struct rk_dllist_node* new_node = rk_dllist_node_new(value, size);
    if(new_node) {
        rk_dllist_node_link(new_node, lst->last, NULL);
        lst->last = new_node;
    }
    return new_node;
}

rk_dllist_error_status rk_dllist_pop_front(rk_dllist lst) {
    if(lst->first) {
        struct rk_dllist_node* delendo = lst->first;
        delendo->next->prev = NULL;
        lst->first = delendo->next;
        rk_dllist_node_free(delendo);
        return RK_OK;
    }
    return RK_EEMPTY;
}

rk_dllist_error_status rk_dllist_pop_back(rk_dllist lst) {
    if(lst->last) {
        struct rk_dllist_node* delendo = lst->last;
        delendo->prev->next = NULL;
        lst->last = delendo->prev;
        rk_dllist_node_free(delendo);
        return RK_OK;
    }
    return RK_EEMPTY;
}

rk_dllist_error_status rk_dllist_peek_front_size(rk_dllist lst, size_t* size) {
    if(rk_dllist_empty(lst)) {
        return RK_EEMPTY;
    }
    *size = lst->first->size;
    return RK_OK;
}

rk_dllist_error_status rk_dllist_peek_back_size(rk_dllist lst, size_t* size) {
    if(rk_dllist_empty(lst)) {
        return RK_EEMPTY;
    }
    *size = lst->last->size;
    return RK_OK;
}

rk_dllist_error_status
rk_dllist_peek_front_notsafe(rk_dllist lst, void* buff) {
    if(rk_dllist_empty(lst)) {
        return RK_EEMPTY;
    }
    rk_dllist_node_get(buff, lst->first);
    return RK_OK;
}

rk_dllist_error_status
rk_dllist_peek_back_notsafe(rk_dllist lst, void* buff) {
    if(rk_dllist_empty(lst)) {
        return RK_EEMPTY;
    }
    rk_dllist_node_get(buff, lst->last);
    return RK_OK;
}


rk_dllist_error_status
rk_dllist_peek_back(rk_dllist lst, void* buff, size_t max_size) {
    if(max_size < lst->last->size) {
        return RK_EBUFFER;
    }
    return rk_dllist_peek_back_notsafe(lst, buff);
}

rk_dllist_error_status
rk_dllist_peek_front(rk_dllist lst, void* buff, size_t max_size) {
    if(max_size < lst->first->size) {
        return RK_EBUFFER;
    }
    return rk_dllist_peek_first_notsafe(lst, buff);
}

void rk_dllist_iterator_init(rk_dllist_iterator* it, rk_dllist lst) {
    *it = lst->first;
}

bool rk_dllist_iterator_has_next(rk_dllist_iterator it) {
    assert(it);
    return (intptr_t)it->next;
}

bool rk_dllist_iterator_has_prev(rk_dllist_iterator it) {
    assert(it);
    return (intptr_t)it->prev;
}

rk_dllist_error_status
rk_dllist_iterator_inc(rk_dllist_iterator* it) {
    assert(it);
    assert(rk_dllist_iterator_has_next(*it));
    return (*it = (*it)->next) ? RK_OK : RK_EEMPTY;
}

rk_dllist_error_status
rk_dllist_iterator_dec(rk_dllist_iterator* it) {
    assert(it);
    assert(rk_dllist_iterator_has_prev(*it));
    return (*it = (*it)->next) ? RK_OK : RK_EEMPTY;
}

rk_dllist_error_status
rk_dllist_iterator_current_size(rk_dllist_iterator it, size_t* size) {
    assert(it);
    *size = it->size;
    return RK_OK;
}

rk_dllist_error_status
rk_dllist_iterator_current_value(rk_dllist_iterator it, void* buffer,
                                 size_t max_size) {
    assert(it);
    if(max_size < it->size) {
        return RK_EBUFFER;
    }
    return rk_dllist_iterator_current_value_notsafe(it, buffer);
}

rk_dllist_error_status
rk_dllist_iterator_current_value_notsafe(rk_dllist_iterator it,
                                         void* buffer) {
    assert(it);
    rk_dllist_node_get(buffer, it);
    return RK_OK;
}

rk_dllist_iterator
rk_dllist_insert_before(rk_dllist_iterator it, void* value, size_t size) {
    struct rk_dllist_node* new_node = rk_dllist_node_new(value, size);
    if(new_node) {
        rk_dllist_node_link(new_node, it->prev, it);
    }
    return new_node;
}

rk_dllist_iterator
rk_dllist_insert_after(rk_dllist_iterator it, void* value, size_t size) {
    struct rk_dllist_node* new_node = rk_dllist_node_new(value, size);
    if(new_node) {
        rk_dllist_node_link(new_node, it, it->next);
    }
    return new_node;
}

rk_dllist_error_status rk_dllist_remove_before(rk_dllist_iterator it) {
    if(rk_dllist_iterator_has_prev(it)) {
        struct rk_dllist_node* delendo = it->prev;
        rk_dllist_node_link_prev(it, it->prev->prev);
        rk_dllist_node_free(delendo);
        return RK_OK;
    } else {
        return RK_EEMPTY;
    }
}

rk_dllist_error_status rk_dllist_remove_after(rk_dllist_iterator it) {
    if(rk_dllist_iterator_has_prev(it)) {
        struct rk_dllist_node* delendo = it->next;
        rk_dllist_node_link_next(it, it->next->next);
        rk_dllist_node_free(delendo);
        return RK_OK;
    } else {
        return RK_EEMPTY;
    }
}
