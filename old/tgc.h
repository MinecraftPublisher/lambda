#ifndef TGC_H
#define TGC_H

#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

char is_ptr(void *data) {
    // Allocate a shared memory space for the parent and child process.
    char *glob_var
        = mmap(NULL, sizeof(char), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);

    pid_t p = fork();
    if (p < 0) { // Failed to fork. Cannot recover!!
        printf("MarkC Error: Fork failed");
        exit(SIGABRT);
    } else if (p == 0) {
        // Child process. Try to deallocate the pointer and then change the variable to true.
        *glob_var = 0;            // Set *glob_var to false.
        int *x    = (int *) data; // Cast.
        char data = *x;           // Deallocate.
        *glob_var = 1;
        // If we deallocated it, We own it and it is a pointer. If not, We don't own that pointer.

        exit(0);
    } else { /* Parent process. Wait until the child process exits and then read the status,
             Deallocate the memory and return the output. */
        wait(NULL);
        char output = *glob_var;

        munmap(glob_var, sizeof(char));

        return output;
    }
}

enum { TGC_MARK = 0x01, TGC_ROOT = 0x02, TGC_LEAF = 0x04 };

typedef struct {
    void  *ptr;
    int    flags;
    size_t size, hash;
    void   (*dtor)(void *);
} tgc_ptr_t;

typedef struct {
    void      *bottom;
    int        paused;
    uintptr_t  minptr, maxptr;
    tgc_ptr_t *items, *frees;
    double     loadfactor, sweepfactor;
    size_t     nitems, nslots, mitems, nfrees;
} tgc_t;

void tgc_start(tgc_t *gc, void *stk);
void tgc_stop(tgc_t *gc);
void tgc_pause(tgc_t *gc);
void tgc_resume(tgc_t *gc);
void tgc_run(tgc_t *gc);

void *tgc_alloc(tgc_t *gc, size_t size);
void *tgc_calloc(tgc_t *gc, size_t num, size_t size);
void *tgc_realloc(tgc_t *gc, void *ptr, size_t size);
void  tgc_free(tgc_t *gc, void *ptr);

void *tgc_alloc_opt(tgc_t *gc, size_t size, int flags, void (*dtor)(void *));
void *tgc_calloc_opt(tgc_t *gc, size_t num, size_t size, int flags, void (*dtor)(void *));

void   tgc_set_dtor(tgc_t *gc, void *ptr, void (*dtor)(void *));
void   tgc_set_flags(tgc_t *gc, void *ptr, int flags);
int    tgc_get_flags(tgc_t *gc, void *ptr);
void   (*tgc_get_dtor(tgc_t *gc, void *ptr))(void *);
size_t tgc_get_size(tgc_t *gc, void *ptr);

static size_t tgc_hash(void *ptr) {
    uintptr_t ad = (uintptr_t) ptr;
    return (size_t) ((13 * ad) ^ (ad >> 15));
}

static size_t tgc_probe(tgc_t *gc, size_t i, size_t h) {
    long v = i - (h - 1);
    if (v < 0) { v = gc->nslots + v; }
    return v;
}

static tgc_ptr_t *tgc_get_ptr(tgc_t *gc, void *ptr) {
    size_t i, j, h;
    i = tgc_hash(ptr) % gc->nslots;
    j = 0;
    while (1) {
        h = gc->items[ i ].hash;
        if (h == 0 || j > tgc_probe(gc, i, h)) { return NULL; }
        if (gc->items[ i ].ptr == ptr) { return &gc->items[ i ]; }
        i = (i + 1) % gc->nslots;
        j++;
    }
    return NULL;
}

static void tgc_add_ptr(tgc_t *gc, void *ptr, size_t size, int flags, void (*dtor)(void *)) {
    tgc_ptr_t item, tmp;
    size_t    h, p, i, j;

    i = tgc_hash(ptr) % gc->nslots;
    j = 0;

    item.ptr   = ptr;
    item.flags = flags;
    item.size  = size;
    item.hash  = i + 1;
    item.dtor  = dtor;

    while (1) {
        h = gc->items[ i ].hash;
        if (h == 0) {
            gc->items[ i ] = item;
            return;
        }
        if (gc->items[ i ].ptr == item.ptr) { return; }
        p = tgc_probe(gc, i, h);
        if (j >= p) {
            tmp            = gc->items[ i ];
            gc->items[ i ] = item;
            item           = tmp;
            j              = p;
        }
        i = (i + 1) % gc->nslots;
        j++;
    }
}

static void tgc_rem_ptr(tgc_t *gc, void *ptr) {
    size_t i, j, h, nj, nh;

    if (gc->nitems == 0) { return; }

    for (i = 0; i < gc->nfrees; i++) {
        if (gc->frees[ i ].ptr == ptr) { gc->frees[ i ].ptr = NULL; }
    }

    i = tgc_hash(ptr) % gc->nslots;
    j = 0;

    while (1) {
        h = gc->items[ i ].hash;
        if (h == 0 || j > tgc_probe(gc, i, h)) { return; }
        if (gc->items[ i ].ptr == ptr) {
            memset(&gc->items[ i ], 0, sizeof(tgc_ptr_t));
            j = i;
            while (1) {
                nj = (j + 1) % gc->nslots;
                nh = gc->items[ nj ].hash;
                if (nh != 0 && tgc_probe(gc, nj, nh) > 0) {
                    memcpy(&gc->items[ j ], &gc->items[ nj ], sizeof(tgc_ptr_t));
                    memset(&gc->items[ nj ], 0, sizeof(tgc_ptr_t));
                    j = nj;
                } else {
                    break;
                }
            }
            gc->nitems--;
            return;
        }
        i = (i + 1) % gc->nslots;
        j++;
    }
}

enum { TGC_PRIMES_COUNT = 24 };

static const size_t tgc_primes[ TGC_PRIMES_COUNT ] = {
    0,    1,    5,     11,    23,    53,     101,    197,    389,     683,     1259,    2417,
    4733, 9371, 18617, 37097, 74093, 148073, 296099, 592019, 1100009, 2200013, 4400021, 8800019
};

static size_t tgc_ideal_size(tgc_t *gc, size_t size) {
    size_t i, last;
    size = (size_t) ((double) (size + 1) / gc->loadfactor);
    for (i = 0; i < TGC_PRIMES_COUNT; i++) {
        if (tgc_primes[ i ] >= size) { return tgc_primes[ i ]; }
    }
    last = tgc_primes[ TGC_PRIMES_COUNT - 1 ];
    for (i = 0;; i++) {
        if (last * i >= size) { return last * i; }
    }
    return 0;
}

static int tgc_rehash(tgc_t *gc, size_t new_size) {
    size_t     i;
    tgc_ptr_t *old_items = gc->items;
    size_t     old_size  = gc->nslots;

    gc->nslots = new_size;
    gc->items  = calloc(gc->nslots, sizeof(tgc_ptr_t));

    if (gc->items == NULL) {
        gc->nslots = old_size;
        gc->items  = old_items;
        return 0;
    }

    for (i = 0; i < old_size; i++) {
        if (old_items[ i ].hash != 0) {
            tgc_add_ptr(
                gc,
                old_items[ i ].ptr,
                old_items[ i ].size,
                old_items[ i ].flags,
                old_items[ i ].dtor);
        }
    }

    if (is_ptr(old_items)) free(old_items);

    return 1;
}

static int tgc_resize_more(tgc_t *gc) {
    size_t new_size = tgc_ideal_size(gc, gc->nitems);
    size_t old_size = gc->nslots;
    return (new_size > old_size) ? tgc_rehash(gc, new_size) : 1;
}

static int tgc_resize_less(tgc_t *gc) {
    size_t new_size = tgc_ideal_size(gc, gc->nitems);
    size_t old_size = gc->nslots;
    return (new_size < old_size) ? tgc_rehash(gc, new_size) : 1;
}

static void tgc_mark_ptr(tgc_t *gc, void *ptr) {
    size_t i, j, h, k;

    if ((uintptr_t) ptr < gc->minptr || (uintptr_t) ptr > gc->maxptr) { return; }

    i = tgc_hash(ptr) % gc->nslots;
    j = 0;

    while (1) {
        h = gc->items[ i ].hash;
        if (h == 0 || j > tgc_probe(gc, i, h)) { return; }
        if (ptr == gc->items[ i ].ptr) {
            if (gc->items[ i ].flags & TGC_MARK) { return; }
            gc->items[ i ].flags |= TGC_MARK;
            if (gc->items[ i ].flags & TGC_LEAF) { return; }
            for (k = 0; k < gc->items[ i ].size / sizeof(void *); k++) {
                tgc_mark_ptr(gc, ((void **) gc->items[ i ].ptr)[ k ]);
            }
            return;
        }
        i = (i + 1) % gc->nslots;
        j++;
    }
}

static void tgc_mark_stack(tgc_t *gc) {
    void *stk, *bot, *top, *p;
    bot = gc->bottom;
    top = &stk;

    if (bot == top) { return; }

    if (bot < top) {
        for (p = top; p >= bot; p = ((char *) p) - sizeof(void *)) {
            tgc_mark_ptr(gc, *((void **) p));
        }
    }

    if (bot > top) {
        for (p = top; p <= bot; p = ((char *) p) + sizeof(void *)) {
            tgc_mark_ptr(gc, *((void **) p));
        }
    }
}

static void tgc_mark(tgc_t *gc) {
    size_t  i, k;
    jmp_buf env;
    void    (*volatile mark_stack)(tgc_t *) = tgc_mark_stack;

    if (gc->nitems == 0) { return; }

    for (i = 0; i < gc->nslots; i++) {
        if (gc->items[ i ].hash == 0) { continue; }
        if (gc->items[ i ].flags & TGC_MARK) { continue; }
        if (gc->items[ i ].flags & TGC_ROOT) {
            gc->items[ i ].flags |= TGC_MARK;
            if (gc->items[ i ].flags & TGC_LEAF) { continue; }
            for (k = 0; k < gc->items[ i ].size / sizeof(void *); k++) {
                tgc_mark_ptr(gc, ((void **) gc->items[ i ].ptr)[ k ]);
            }
            continue;
        }
    }

    memset(&env, 0, sizeof(jmp_buf));
    setjmp(env);
    mark_stack(gc);
}

void tgc_sweep(tgc_t *gc) {
    size_t i, j, k, nj, nh;

    if (gc->nitems == 0) { return; }

    gc->nfrees = 0;
    for (i = 0; i < gc->nslots; i++) {
        if (gc->items[ i ].hash == 0) { continue; }
        if (gc->items[ i ].flags & TGC_MARK) { continue; }
        if (gc->items[ i ].flags & TGC_ROOT) { continue; }
        gc->nfrees++;
    }

    gc->frees = realloc(gc->frees, sizeof(tgc_ptr_t) * gc->nfrees);
    if (gc->frees == NULL) { return; }

    i = 0;
    k = 0;
    while (i < gc->nslots) {
        if (gc->items[ i ].hash == 0) {
            i++;
            continue;
        }
        if (gc->items[ i ].flags & TGC_MARK) {
            i++;
            continue;
        }
        if (gc->items[ i ].flags & TGC_ROOT) {
            i++;
            continue;
        }

        gc->frees[ k ] = gc->items[ i ];
        k++;
        memset(&gc->items[ i ], 0, sizeof(tgc_ptr_t));

        j = i;
        while (1) {
            nj = (j + 1) % gc->nslots;
            nh = gc->items[ nj ].hash;
            if (nh != 0 && tgc_probe(gc, nj, nh) > 0) {
                memcpy(&gc->items[ j ], &gc->items[ nj ], sizeof(tgc_ptr_t));
                memset(&gc->items[ nj ], 0, sizeof(tgc_ptr_t));
                j = nj;
            } else {
                break;
            }
        }
        gc->nitems--;
    }

    for (i = 0; i < gc->nslots; i++) {
        if (gc->items[ i ].hash == 0) { continue; }
        if (gc->items[ i ].flags & TGC_MARK) { gc->items[ i ].flags &= ~TGC_MARK; }
    }

    tgc_resize_less(gc);

    gc->mitems = gc->nitems + (size_t) (gc->nitems * gc->sweepfactor) + 1;

    for (i = 0; i < gc->nfrees; i++) {
        if (gc->frees[ i ].ptr) {
            if (gc->frees[ i ].dtor) { gc->frees[ i ].dtor(gc->frees[ i ].ptr); }
            if (is_ptr(gc->frees[ i ].ptr)) free(gc->frees[ i ].ptr);
        }
    }

    if (is_ptr(gc->frees)) free(gc->frees);
    gc->frees  = NULL;
    gc->nfrees = 0;
}

void tgc_start(tgc_t *gc, void *stk) {
    gc->bottom      = stk;
    gc->paused      = 0;
    gc->nitems      = 0;
    gc->nslots      = 0;
    gc->mitems      = 0;
    gc->nfrees      = 0;
    gc->maxptr      = 0;
    gc->items       = NULL;
    gc->frees       = NULL;
    gc->minptr      = UINTPTR_MAX;
    gc->loadfactor  = 0.9;
    gc->sweepfactor = 0.5;
}

void tgc_stop(tgc_t *gc) {
    tgc_sweep(gc);
    if (is_ptr(gc->items)) free(gc->items);
    if (is_ptr(gc->frees)) free(gc->frees);
}

void tgc_pause(tgc_t *gc) { gc->paused = 1; }

void tgc_resume(tgc_t *gc) { gc->paused = 0; }

void tgc_run(tgc_t *gc) {
    tgc_mark(gc);
    tgc_sweep(gc);
}

static void *tgc_add(tgc_t *gc, void *ptr, size_t size, int flags, void (*dtor)(void *)) {
    gc->nitems++;
    gc->maxptr = ((uintptr_t) ptr) + size > gc->maxptr ? ((uintptr_t) ptr) + size : gc->maxptr;
    gc->minptr = ((uintptr_t) ptr) < gc->minptr ? ((uintptr_t) ptr) : gc->minptr;

    if (tgc_resize_more(gc)) {
        tgc_add_ptr(gc, ptr, size, flags, dtor);
        if (!gc->paused && gc->nitems > gc->mitems) { tgc_run(gc); }
        return ptr;
    } else {
        gc->nitems--;
        if (is_ptr(ptr)) free(ptr);
        return NULL;
    }
}

static void tgc_rem(tgc_t *gc, void *ptr) {
    tgc_rem_ptr(gc, ptr);
    tgc_resize_less(gc);
    gc->mitems = gc->nitems + gc->nitems / 2 + 1;
}

void *tgc_alloc(tgc_t *gc, size_t size) { return tgc_alloc_opt(gc, size, 0, NULL); }

void *tgc_calloc(tgc_t *gc, size_t num, size_t size) {
    return tgc_calloc_opt(gc, num, size, 0, NULL);
}

void *tgc_realloc(tgc_t *gc, void *ptr, size_t size) {
    tgc_ptr_t *p;
    void      *qtr = realloc(ptr, size);

    if (qtr == NULL) {
        tgc_rem(gc, ptr);
        return qtr;
    }

    if (ptr == NULL) {
        tgc_add(gc, qtr, size, 0, NULL);
        return qtr;
    }

    p = tgc_get_ptr(gc, ptr);

    if (p && qtr == ptr) {
        p->size = size;
        return qtr;
    }

    if (p && qtr != ptr) {
        int  flags           = p->flags;
        void (*dtor)(void *) = p->dtor;
        tgc_rem(gc, ptr);
        tgc_add(gc, qtr, size, flags, dtor);
        return qtr;
    }

    return NULL;
}

void tgc_free(tgc_t *gc, void *ptr) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) {
        if (p->dtor) { p->dtor(ptr); }
        if (is_ptr(ptr)) free(ptr);
        tgc_rem(gc, ptr);
    }
}

void *tgc_alloc_opt(tgc_t *gc, size_t size, int flags, void (*dtor)(void *)) {
    void *ptr = malloc(size);
    if (ptr != NULL) { ptr = tgc_add(gc, ptr, size, flags, dtor); }
    return ptr;
}

void *tgc_calloc_opt(tgc_t *gc, size_t num, size_t size, int flags, void (*dtor)(void *)) {
    void *ptr = calloc(num, size);
    if (ptr != NULL) { ptr = tgc_add(gc, ptr, num * size, flags, dtor); }
    return ptr;
}

void tgc_set_dtor(tgc_t *gc, void *ptr, void (*dtor)(void *)) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) { p->dtor = dtor; }
}

void tgc_set_flags(tgc_t *gc, void *ptr, int flags) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) { p->flags = flags; }
}

int tgc_get_flags(tgc_t *gc, void *ptr) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) { return p->flags; }
    return 0;
}

void (*tgc_get_dtor(tgc_t *gc, void *ptr))(void *) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) { return p->dtor; }
    return NULL;
}

size_t tgc_get_size(tgc_t *gc, void *ptr) {
    tgc_ptr_t *p = tgc_get_ptr(gc, ptr);
    if (p) { return p->size; }
    return 0;
}

#endif
