#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/inotify.h>
#include <unistd.h>

#include <ei.h>
#include <erl_driver.h>

#include "macrology.h"

struct instance {
    ErlDrvPort port;
    int fd;
};

static ErlDrvData start(ErlDrvPort port, char *UNUSED)
{
    struct instance *self = driver_alloc(sizeof(*self));
    if (!self) return ERL_DRV_ERROR_GENERAL;
    *self = (struct instance){0};
    self->port = port;

    self->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (self->fd < 0) {
        driver_free(self);
        return ERL_DRV_ERROR_ERRNO;
    }

    int error = driver_select(
        self->port,
        (ErlDrvEvent)(intptr_t)self->fd,
        ERL_DRV_READ | ERL_DRV_USE,
        1
    );
    if (error) {
        close(self->fd);
        driver_free(self);
        return ERL_DRV_ERROR_GENERAL;
    }

    return (ErlDrvData)self;
}

static void stop_select(ErlDrvEvent event, void *UNUSED)
{
    close((intptr_t)event);
}

static void stop(ErlDrvData self_)
{
    struct instance *self = (struct instance *)self_;
    driver_select(self->port, (ErlDrvEvent)(intptr_t)self->fd, ERL_DRV_USE, 0);
    driver_free(self);
}

static ErlDrvSSizeT call(
    ErlDrvData self_,
    unsigned int UNUSED,
    char *buf, ErlDrvSizeT UNUSED,
    char **rbuf, ErlDrvSizeT rlen,
    unsigned int *UNUSED)
{
    struct instance *self = (struct instance *)self_;

    int index = 0;
    if (ei_decode_version(buf, &index, NULL) < 0) return -1;

    int type, path_len;
    if (ei_get_type(buf, &index, &type, &path_len) < 0) return -1;
    char *path = malloc(path_len + 1);
    if (!path) return -1;

    if (ei_decode_string(buf, &index, path) < 0) goto fail0;

    int error = 0;
    char *error_str = NULL;
    /* int wd = inotify_add_watch(self->fd, path, IN_ALL_EVENTS); */
    int wd = inotify_add_watch(self->fd, path, IN_MODIFY | IN_MOVED_TO);
    if (wd < 0) {
        error = errno;
        error_str = strerror(error);
    }

    // Encode the terms first with a NULL buffer, which safely increments index
    // to determine the required buffer length. On the second iteration, either
    // encode to *rbuf if it is big enough, or to a new allocation of the right
    // size.
    char *out = NULL;
    index = 0;
    do {
        if (index) {
            out = ((ErlDrvSizeT)index <= rlen) ? *rbuf : driver_alloc(index);
            if (!out) goto fail0;
            index = 0;
        }

        if (ei_encode_version(out, &index) < 0) goto fail1;
        if (ei_encode_tuple_header(out, &index, 2) < 0) goto fail1;
        if (error) {
            if (ei_encode_atom(out, &index, "error") < 0) goto fail1;
            if (ei_encode_tuple_header(out, &index, 3) < 0) goto fail1;
            if (ei_encode_string(out, &index, path) < 0) goto fail1;
            if (ei_encode_long(out, &index, error) < 0) goto fail1;
            if (ei_encode_string(out, &index, error_str) < 0) goto fail1;
        } else {
            if (ei_encode_atom(out, &index, "ok") < 0) goto fail1;
            if (ei_encode_long(out, &index, wd) < 0) goto fail1;
        }
    } while (!out);

    free(path);
    *rbuf = out;
    return index;

fail1:
    if (out && out != *rbuf) driver_free(out);
fail0:
    free(path);
    return -1;
}

ErlDrvTermData generate_event_atom(uint32_t mask)
{
    if (mask & IN_ACCESS) {
        return driver_mk_atom("access");
    }
    if (mask & IN_ATTRIB) {
        return driver_mk_atom("attribute_change");
    }
    if (mask & IN_CLOSE_WRITE ||
        mask & IN_CLOSE_NOWRITE) {
        return driver_mk_atom("close");
    }
    if (mask & IN_CREATE) {
        return driver_mk_atom("create");
    }
    if (mask & IN_DELETE ||
        mask & IN_DELETE_SELF) {
        return driver_mk_atom("delete");
    }
    if (mask & IN_MODIFY) {
        return driver_mk_atom("modify");
    }
    if (mask & IN_MOVE_SELF) {
        return driver_mk_atom("move_self");
    }
    if (mask & IN_OPEN) {
        return driver_mk_atom("open");
    }
    if (mask & IN_MOVED_FROM) {
        return driver_mk_atom("moved_from");
    }
    if (mask & IN_MOVED_TO) {
        return driver_mk_atom("moved_to");
    }
    if (mask & IN_IGNORED) {
        return driver_mk_atom("remove_watch");
    }
    if (mask & IN_Q_OVERFLOW) {
        return driver_mk_atom("queue_overflow");
    }
    return driver_mk_atom("other");
}

static void ready_input(ErlDrvData self_, ErlDrvEvent fd_)
{
    struct instance *self = (struct instance *)self_;
    int fd = (intptr_t)fd_;

    char buf[4096] __attribute((aligned(__alignof(struct inotify_event))));
    const struct inotify_event *event;
    ErlDrvTermData event_atom;
    const char *name;
    size_t name_len;
    ssize_t len;
    while ((len = read(fd, buf, sizeof(buf))) > 0) {
        for (char *ptr = buf; ptr < buf + len; ptr += sizeof(struct inotify_event) + event->len) {
            event = (const struct inotify_event *)ptr;
            name = event->name;
            name_len = strlen(event->name);

            event_atom = generate_event_atom(event->mask);

            ErlDrvTermData d[] = {
                ERL_DRV_PORT, driver_mk_port(self->port),
                ERL_DRV_ATOM, event_atom,
                ERL_DRV_STRING, name, name_len,
                ERL_DRV_INT, event->wd,
                ERL_DRV_NIL,
                ERL_DRV_LIST, 2,
                ERL_DRV_TUPLE, 4
            };
            erl_drv_output_term(driver_mk_port(self->port), d, sizeof(d) / sizeof(*d));
        }
    }

    // TODO: Check errno != EAGAIN?
}

static ErlDrvEntry driver_entry = {
    .driver_name = "filewatch",
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
    .start = start,
    .stop_select = stop_select,
    .stop = stop,
    .call = call,
    .ready_input = ready_input
};

DRIVER_INIT(filewatch) { return &driver_entry; }
