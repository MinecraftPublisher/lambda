#include "tgc.h"
//

#include <malloc/_malloc.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef char              *string;
typedef void              *var;
typedef unsigned long long ull;
#define auto __auto_type

void memoryFailure(string file, int line) {
    printf(
        "----------------------------\n"
        " Memory allocation failure!\n"
        " At file %s\n"
        " Line %i\n"
        "----------------------------\n",
        file,
        line);
    exit(SIGABRT);
}

#define format(fmts, ...)                                                                          \
    ({                                                                                             \
        string buf    = NULL;                                                                      \
        int    result = asprintf(&buf, fmts, __VA_ARGS__);                                         \
        if (result < 0) memoryFailure(__FILE__, __LINE__);                                         \
        tgc_add(&gc, buf, strlen(buf), 0, NULL);                                                   \
        buf;                                                                                       \
    })

struct context {
    string text;
    ull    size;
    ull    pointer;
};

struct lambda;

typedef char variable;

typedef struct function {
    variable      *input;
    struct lambda *body;
} function;

typedef struct application {
    struct lambda *left;
    struct lambda *right;
} application;

union lambda_values {
    variable    *variable;
    function    *function;
    application *application;
};

enum lambda_type { VARIABLE, FUNCTION, APPLICATION };

typedef struct lambda {
    enum lambda_type    type;
    union lambda_values values;
} lambda;

#define forever while (1)

void skip_whitepace(struct context *ctx) {
    while ((ctx->text[ ctx->pointer ] == ' ') || (ctx->text[ ctx->pointer ] == '\n')
           || (ctx->text[ ctx->pointer ] == '\t') || (ctx->text[ ctx->pointer ] == '#')) {
        if (ctx->text[ ctx->pointer ] == '#') { // comment handling
            while (ctx->text[ ctx->pointer ] != '\n') ctx->pointer++;
        }
        ctx->pointer++;
    }
}

tgc_t gc = {};

var __malloc(ull size) { return tgc_alloc(&gc, size); }
#define malloc(size) __malloc(size)

variable *parse_variable(struct context *ctx) {
    skip_whitepace(ctx);

    switch (ctx->text[ ctx->pointer ]) {
        case '_':
        case '$':
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '0' ... '9': {
            ull last = ctx->pointer;

            forever {
                switch (ctx->text[ ctx->pointer ]) {
                    case '_':
                    case '$':
                    case 'a' ... 'z':
                    case 'A' ... 'Z':
                    case '0' ... '9': ctx->pointer++; continue;
                }
                break;
            }

            ull new_size = ctx->pointer;

            ull length = new_size - last;

            string new  = malloc(sizeof(char) * length);
            ull counter = 0;

            for (ull i = last; i < new_size; i++) new[ counter++ ] = ctx->text[ i ];

            return new;
        }
    }

    printf("Expected name.\n");
    exit(1);
}

lambda *parse(struct context *ctx) {
    skip_whitepace(ctx);

    lambda *result = malloc(sizeof(lambda));

    switch (ctx->text[ ctx->pointer ]) {
        case '\\': {
            ctx->pointer++;

            result->type                   = FUNCTION;
            result->values.function        = malloc(sizeof(function));
            result->values.function->input = parse_variable(ctx);
            if ((ctx->text[ ctx->pointer ] != '.') && (ctx->text[ ctx->pointer ] != ':')) {
                printf("Expected `.` or `:` in function.\n");
                exit(1);
            }
            ctx->pointer++;
            result->values.function->body = parse(ctx);
            break;
        }
        case '_':
        case '$':
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '0' ... '9': {
            if (strncmp(&ctx->text[ ctx->pointer ], "lambda", 6) == 0) {
                ctx->pointer += 6;

                result->type                   = FUNCTION;
                result->values.function        = malloc(sizeof(function));
                result->values.function->input = parse_variable(ctx);
                skip_whitepace(ctx);
                if ((ctx->text[ ctx->pointer ] != '.') && (ctx->text[ ctx->pointer ] != ':')) {
                    printf("Expected `.` or `:` in function.\n");
                    exit(1);
                }
                ctx->pointer++;
                result->values.function->body = parse(ctx);
                break;
            }

            result->type            = VARIABLE;
            result->values.variable = parse_variable(ctx);
            break;
        }
        case '(': {
            ctx->pointer++;
            result->type                      = APPLICATION;
            result->values.application        = malloc(sizeof(application));
            result->values.application->left  = parse(ctx);
            result->values.application->right = parse(ctx);
            skip_whitepace(ctx);
            if (ctx->text[ ctx->pointer ] != ')') {
                printf("Expected `)` in application.\n");
                exit(1);
            }
            ctx->pointer++;
            break;
        }
        default: {
            printf("Expected variable, function or application.\n");
            exit(1);
        }
    }

    return result;
}

typedef struct linkednode {
    string             value;
    struct linkednode *next;
    struct linkednode *prev;
} linkednode;

typedef struct linkedlist {
    linkednode *head;
    linkednode *end;
    ull         size;
} linkedlist;

typedef signed long long ill;
typedef unsigned char bool;
const bool true  = 1;
const bool false = 0;

struct list_result {
    string value;
    ull    index;
};

ill exists_in_list(linkedlist *list, string name) {
    auto head  = list->head;
    ill  index = 0;

    while (head != NULL) {
        if (strcmp(head->value, name) == 0) return index;
        index++;

        head = head->next;
    }

    return -1;
}

void push_list(linkedlist *list, string value) {
    linkednode *node = malloc(sizeof(linkednode));

    node->next  = NULL;
    node->prev  = list->end;
    node->value = value;

    if (list->end != NULL) list->end->next = node;
    if (list->head == NULL) list->head = node;

    list->end = node;
}

void pop_list(linkedlist *list) {
    if (list->end != NULL) { list->end->prev->next = NULL; }
    if (list->head == list->end) list->head = NULL;
}

string __print_lambda(lambda *input, ull depth, linkedlist *names) {
    switch (input->type) {
        case VARIABLE: {
            auto res = exists_in_list(names, input->values.variable);
            if (res != -1) return format("$%lli", res);
            else { return format("%s", input->values.variable); }
        }
        case FUNCTION: {
            push_list(names, input->values.function->input);
            exists_in_list(names, input->values.function->input);
            string body = __print_lambda(input->values.function->body, depth, names);

            auto   res = exists_in_list(names, input->values.function->input);
            string fmt = format("lambda $%lli: %s", res, body);

            pop_list(names);

            return fmt;
        }
        case APPLICATION: {
            string left  = __print_lambda(input->values.application->left, depth, names);
            string right = __print_lambda(input->values.application->right, depth, names);
            string fmt   = format("(%s %s)", left, right);

            return fmt;
        }
    }
}

string print_lambda(lambda *input) {
    linkedlist *names = malloc(sizeof(linkedlist));
    names->size       = 0;
    names->head       = NULL;
    names->end        = NULL;

    return __print_lambda(input, 0, names);
}

lambda *clone_lambda(lambda *input) {
    lambda *new = malloc(sizeof(lambda));
    new->type   = input->type;

    switch (new->type) {
        case VARIABLE: new->values.variable = format("%s", input->values.variable); break;
        case FUNCTION:
            new->values.function = malloc(sizeof(function));

            new->values.function->input = format("%s", input->values.function->input);
            new->values.function->body  = clone_lambda(input->values.function->body);
            break;
        case APPLICATION:
            new->values.application = malloc(sizeof(application));

            new->values.application->left  = clone_lambda(input->values.application->left);
            new->values.application->right = clone_lambda(input->values.application->right);
            break;
    }

    return new;
}

lambda *replace_lambda(lambda *input, variable *find, lambda *replace) {
    switch (input->type) {
        case VARIABLE: {
            if (strcmp(input->values.variable, find) == 0) return clone_lambda(replace);
        }
        case APPLICATION: {
            lambda *new = clone_lambda(input);
            new->values.application->left
                = replace_lambda(new->values.application->left, find, replace);
            new->values.application->right
                = replace_lambda(new->values.application->right, find, replace);

            return new;
        }
        case FUNCTION: {
            lambda *new = clone_lambda(input);

            if (strcmp(new->values.function->input, find) == 0) { // requires alpha reduction
                new->values.function->input = format("_%s", new->values.function->input);
                lambda *new_name            = malloc(sizeof(lambda));
                new_name->type              = VARIABLE;
                new_name->values.variable   = new->values.function->input;

                new->values.function->body = replace_lambda(
                    new->values.function->body, input->values.function->input, new_name);
            }

            new->values.function->body = replace_lambda(new->values.function->body, find, replace);

            return new;
        }
    }
}

typedef struct table {
    struct item {
        string key;
        enum item_type { item_type_EXPR, item_type_FUNCTION } type;
        union item_value {
            lambda *expr;
            lambda *(*function)(lambda *input);
        } value;
    }  *items;
    ull size;
} table;

table builtin = {};

lambda *exists_in_builtin(lambda *name) {
    auto x = builtin.size;
    for (ull i = 0; i < builtin.size; i++) {
        if (builtin.items[ i ].type == item_type_EXPR) {
            if (builtin.items[ i ].key == NULL) continue;
            if (strcmp(builtin.items[ i ].key, name->values.variable) == 0) {
                return builtin.items[ i ].value.expr;
            }
        }
    }

    return name;
}

lambda *__evaluate_lambda(lambda *input) {
    printf("     STEP -> %s\n", print_lambda(input));

    switch (input->type) {
        case VARIABLE: {
            return exists_in_builtin(input);
        }
        case FUNCTION: return input;
        case APPLICATION:
            if (input->values.application->left->type == FUNCTION) {
                lambda *result = replace_lambda(
                    input->values.application->left->values.function->body,
                    input->values.application->left->values.function->input,
                    input->values.application->right);

                return result;
            }

            if (input->values.application->left->type == VARIABLE) {
                for (ull i = 0; i <= builtin.size; i++) {
                    if (builtin.items[ i ].type == item_type_FUNCTION) {
                        if (strcmp(
                                builtin.items[ i ].key,
                                input->values.application->left->values.variable)
                            == 0) {
                            auto func   = builtin.items[ i ].value.function;
                            auto result = func(input->values.application->right);

                            return result;
                        }
                    }
                }
            }

            lambda *new = clone_lambda(input);
            auto left   = __evaluate_lambda(new->values.application->left);
            auto right  = __evaluate_lambda(new->values.application->right);

            new->values.application->left  = left;
            new->values.application->right = right;

            return new;
    }
}

lambda *global_nil;

lambda *evaluate_lambda(lambda *input) {
    auto _x = input;

    do {
        input = _x;
        _x    = __evaluate_lambda(input);
        tgc_run(&gc);
        if (print_lambda(_x) == print_lambda(global_nil)) return _x;
    } while (strcmp(print_lambda(_x), print_lambda(input)) != 0);

    return _x;
}

struct context *create_ctx(string text) {
    struct context *ctx = malloc(sizeof(struct context));
    ctx->pointer        = 0;
    ctx->size           = strlen(text);
    ctx->text           = text;

    return ctx;
}

#define set_string(name, _value)                                                                   \
    ({                                                                                             \
        builtin.size++;                                                                            \
        builtin.items = realloc(builtin.items, sizeof(struct item) * (builtin.size + 1));          \
        builtin.items[ builtin.size ].key        = #name;                                          \
        builtin.items[ builtin.size ].type       = item_type_EXPR;                                 \
        builtin.items[ builtin.size ].value.expr = parse(create_ctx(_value));                      \
    })

#define set_function(name, func)                                                                   \
    ({                                                                                             \
        builtin.size++;                                                                            \
        builtin.items = realloc(builtin.items, sizeof(struct item) * (builtin.size + 1));          \
        builtin.items[ builtin.size ].key            = #name;                                      \
        builtin.items[ builtin.size ].type           = item_type_FUNCTION;                         \
        builtin.items[ builtin.size ].value.function = func;                                       \
    })

lambda *internal_print(lambda *input) {
    string stringified_input = print_lambda(evaluate_lambda(input));
    for (ull i = 0; i <= builtin.size; i++) {
        auto item = builtin.items[ i ];
        if (item.type == item_type_EXPR && item.key != NULL) {
            if (strcmp(print_lambda(item.value.expr), stringified_input) == 0) {
                printf("%s", item.key);
                return global_nil;
            }
        }
    }

    return global_nil;
}

lambda *internal_nil(lambda *input) {
    evaluate_lambda(input);
    return clone_lambda(global_nil);
}

int main(int argc, string *argv) {
    tgc_start(&gc, &argc);
    global_nil = __evaluate_lambda(parse(create_ctx("nil")));

    // ------------- builtin -------------
    builtin.size  = 0;
    builtin.items = malloc(sizeof(struct item) * (builtin.size + 1));
    set_string(0, "lambda f: lambda x: x");
    set_string(1, "lambda f: lambda x: (f x)");
    set_string(2, "lambda f: lambda x: (f(f x))");
    set_string(3, "lambda f: lambda x: (f(f(f x)))");
    set_string(4, "lambda f: lambda x: (f(f(f(f x))))");
    set_string(5, "lambda f: lambda x: (f(f(f(f(f x)))))");
    set_string(6, "lambda f: lambda x: (f(f(f(f(f(f x))))))");
    set_string(7, "lambda f: lambda x: (f(f(f(f(f(f(f x)))))))");
    set_string(8, "lambda f: lambda x: (f(f(f(f(f(f(f(f x))))))))");
    set_string(9, "lambda f: lambda x: (f(f(f(f(f(f(f(f(f x)))))))))");
    set_string(true, "lambda t: lambda f: t");
    set_string(false, "lambda t: lambda f: f");
    set_function(print, internal_print);
    set_function(nil, internal_nil);
    // ------------- builtin -------------

    const auto TEXT = "(print 1)";

    auto ctx = create_ctx(TEXT);

    auto output = evaluate_lambda(parse(ctx));
    // printf("%s\n", print_lambda(output));

    tgc_stop(&gc);
    return 0;
}
