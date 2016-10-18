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
    int wd = inotify_add_watch(self->fd, path, IN_CLOSE_WRITE | IN_MOVE_SELF);
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

static void ready_input(ErlDrvData UNUSED, ErlDrvEvent UNUSED)
{
    // TODO
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
