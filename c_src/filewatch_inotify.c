#include <stdint.h>
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

#include <assert.h>

static ErlDrvSSizeT call(
    ErlDrvData self_,
    unsigned int UNUSED,
    char *buf, ErlDrvSizeT len,
    char **rbuf, ErlDrvSizeT rlen,
    unsigned int *UNUSED)
{
    // Encode the terms first with a NULL buffer, which safely increments index
    // to determine the required buffer length. On the second iteration, either
    // encode to *rbuf if it is big enough, or to a new allocation of the right
    // size.
    char *out = NULL;
    int index = 0;
    do {
        if (index) out = ((ErlDrvSizeT)index <= rlen) ? *rbuf : driver_alloc(index);
        index = 0;
        ei_encode_version(out, &index);
        ei_encode_tuple_header(out, &index, 2);
        ei_encode_atom(out, &index, "ok");
        ei_encode_long(out, &index, 1337);
    } while (!out);
    *rbuf = out;
    return index;
}

static void ready_input(ErlDrvData self_, ErlDrvEvent event)
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
