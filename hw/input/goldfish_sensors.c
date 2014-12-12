/*
 * Goldfish 'sensors' device model
 *
 * This provides a android pipe based device to report sensor data to
 * the android goldfish emulation platform. It is a very much cut-down
 * version of the original "hw-sensors.c" from the Android Emulator.
 * The main difference is it only supports the accelerometer and
 * directly hooks into the AndroidPipe mechanism without the qemud
 * machinery.
 *
 * Copyright (c) 2014 Linaro Limited
 * Copyright (c) 2009 The Android Open Source Project
 *
 * This software is licensed under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation, and
 * may be copied, distributed, and modified under those terms.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "sysemu/char.h"
#include "qemu/error-report.h"
#include "qemu/timer.h"
#include "trace.h"
#include "hw/misc/android_pipe.h"

//#define DEBUG_SENSORS

#ifdef DEBUG_SENSORS
#define DPRINTF(fmt, ...) \
    do { fprintf(stderr, "%s: " fmt , __func__, ## __VA_ARGS__); } while (0)
#else
#define DPRINTF(fmt, ...) do {} while(0)
#endif

enum {
    ANDROID_SENSOR_ACCELERATION
} SensorID_t;

/* The sensor configuration is global to whole driver */
typedef struct SensorsConfig {
    /* Timer tick stuff */
    int         delay_ms;

    /* Sensor data */
    int         enabled_mask;
    struct {
        struct {
            float   x, y, z;
        } acceleration;
    } sensors;
} SensorsConfig;

struct SensorsConfig sensor_config =
{
    800,
    0,
    {
        .acceleration = { 0.0, 0.0, 0.0 }
    }
};

/* There will be one pipe connection per open connection with the
 * guest. Generally there will be a control path used to interrogate
 * the list of sensors and a data path which will trigger the periodic
 * updating of the the data
 */
typedef struct SensorsPipe {
    /* Android Pipe structs */
    void*     hwpipe;
    /* Sensor Ticks */
    QEMUTimer   *periodic_tick;
    /* Output reply data */
    GString *send_data;
} SensorsPipe;


/* I'll just append to a GString holding our data here, which we
 * truncate as we page back out....
 */
static void sensor_send_data(SensorsPipe *sp, const uint8_t *buf, int len)
{
    g_string_append_len(sp->send_data, (gchar *) buf, len);
    android_pipe_wake(sp->hwpipe, PIPE_WAKE_READ);
}


/*
 * - when the qemu-specific sensors HAL module starts, it sends
 *   "list-sensors"
 *
 * - this code replies with a string containing an integer corresponding
 *   to a bitmap of available hardware sensors in the current AVD
 *   configuration (e.g. "1" a.k.a (1 << ANDROID_SENSOR_ACCELERATION))
 *
 * - the HAL module sends "set:<sensor>:<flag>" to enable or disable
 *   the report of a given sensor state. <sensor> must be the name of
 *   a given sensor (e.g. "accelerometer"), and <flag> must be either
 *   "1" (to enable) or "0" (to disable).
 *
 * - Once at least one sensor is "enabled", this code should periodically
 *   send information about the corresponding enabled sensors. The default
 *   period is 200ms.
 *
 * - the HAL module sends "set-delay:<delay>", where <delay> is an integer
 *   corresponding to a time delay in milli-seconds. This corresponds to
 *   a new interval between sensor events sent by this code to the HAL
 *   module.
 *
 * - the HAL module can also send a "wake" command. This code should simply
 *   send the "wake" back to the module. This is used internally to wake a
 *   blocking read that happens in a different thread. This ping-pong makes
 *   the code in the HAL module very simple.
 *
 * - each timer tick, this code sends sensor reports in the following
 *   format (each line corresponds to a different line sent to the module):
 *
 *      acceleration:<x>:<y>:<z>
 *      sync:<time_us>
 *
 *   Where each line before the sync:<time_us> is optional and will only
 *   appear if the corresponding sensor has been enabled by the HAL module.
 *
 *   Note that <time_us> is the VM time in micro-seconds when the report
 *   was "taken" by this code. This is adjusted by the HAL module to
 *   emulated system time (using the first sync: to compute an adjustment
 *   offset).
 */

/* this function is called periodically to send sensor reports
 * to the HAL module, and re-arm the timer if necessary
 */
static void
goldfish_sensor_tick( void*  opaque )
{
    SensorsPipe *sp = opaque;
    int64_t          delay = sensor_config.delay_ms;
    int64_t          now_ms;
    uint32_t         mask  = sensor_config.enabled_mask;
    char             buffer[128];

    if (sensor_config.enabled_mask & (1<<ANDROID_SENSOR_ACCELERATION)) {
        snprintf(buffer, sizeof buffer, "acceleration:%g:%g:%g",
                 sensor_config.sensors.acceleration.x,
                 sensor_config.sensors.acceleration.y,
                 sensor_config.sensors.acceleration.z);
        sensor_send_data(sp, (uint8_t*)buffer, strlen(buffer));
    }

    now_ms = qemu_clock_get_ms(QEMU_CLOCK_VIRTUAL);

    snprintf(buffer, sizeof buffer, "sync:%" PRId64, now_ms * 1000);
    sensor_send_data(sp, (uint8_t*)buffer, strlen(buffer));

    /* rearm timer, use a minimum delay of 20 ms, just to
     * be safe.
     */
    if (mask == 0)
        return;

    if (delay < 20)
        delay = 20;

    timer_mod(sp->periodic_tick, now_ms + delay);
}

/* Incoming command from the guest */
static ssize_t goldfish_sensors_have_data(SensorsPipe *sp, const uint8_t *buf,
                                          ssize_t len)
{
    /* "list-sensors" is used to get an integer bit map of
     * available emulated sensors. We compute the mask from the
     * current hardware configuration.
     */
    if (len == 12 && !memcmp(buf, "list-sensors", 12)) {
        sensor_send_data(sp, (const uint8_t*)"1", 1);
        return len;
    }

    /* "wake" is a special message that must be sent back through
     * the channel. It is used to exit a blocking read.
     */
    if (len == 4 && !memcmp(buf, "wake", 4)) {
        sensor_send_data(sp, (const uint8_t*)"wake", 4);
        return len;
    }

    /* "set-delay:<delay>" is used to set the delay in milliseconds
     * between sensor events
     */
    if (len > 10 && !memcmp(buf, "set-delay:", 10)) {
        sensor_config.delay_ms = atoi((const char*)buf+10);
        if (sensor_config.enabled_mask != 0)
            goldfish_sensor_tick(sp);
        return len;
    }

    /* "set:<name>:<state>" is used to enable/disable a given
     * sensor. <state> must be 0 or 1
     */
    if (len > 4 && !memcmp(buf, "set:", 4)) {
        gchar *name, *state;
        int mask = 0;

        name = (gchar *)&buf[4];
        state = g_strrstr_len((gchar *)buf, len, ":");
        if (g_strstr_len(name, len - 4, "acceleration")) {
            mask = (1<<ANDROID_SENSOR_ACCELERATION);
        }
        if (state && mask) {
            switch (state[1]) {
            case '0':
                sensor_config.enabled_mask &= ~mask;
                break;
            case '1':
                sensor_config.enabled_mask |= mask;
                goldfish_sensor_tick(sp);
                break;
            default:
                DPRINTF("bad set: command (%s)", buf);
                break;
            }
        }
        return len;
    }

    return len;
}

static void* sensors_pipe_init(void *hwpipe, void *opaque, const char *args)
{
    SensorsPipe *pipe;

    DPRINTF("hwpipe=%p\n", hwpipe);
    pipe = g_malloc0(sizeof(SensorsPipe));
    pipe->hwpipe = hwpipe;
    pipe->periodic_tick = timer_new_ms(QEMU_CLOCK_VIRTUAL, goldfish_sensor_tick, pipe);
    pipe->send_data = g_string_sized_new(1024);
    return pipe;
}

static void sensors_pipe_close( void* opaque )
{
    SensorsPipe*  pipe = opaque;
    DPRINTF("pipe %p, hwpipe %p\n", pipe, pipe->hwpipe);
    timer_del(pipe->periodic_tick);
    timer_free(pipe->periodic_tick);
    pipe->periodic_tick = NULL;
    g_string_free(pipe->send_data, TRUE);
    g_free(pipe);
}

static void sensors_pipe_wake(void *opaque, int flags)
{
    SensorsPipe *pipe = opaque;

    /* we have data for the guest to read */
    if (flags & PIPE_WAKE_READ && (pipe->send_data->len > 0)) {
        DPRINTF("0x%x:PIPE_WAKE_READ we have %ld bytes\n", flags, pipe->send_data->len);
        android_pipe_wake(pipe->hwpipe, PIPE_WAKE_READ);
    }

    /* we can always be written to... */
    if (flags & PIPE_WAKE_WRITE) {
        DPRINTF("0x%x:PIPE_WAKE_WRITE\n", flags);
        android_pipe_wake(pipe->hwpipe, PIPE_WAKE_WRITE);
    }
}

static int sensors_pipe_recv(void *opaque, AndroidPipeBuffer *buffers,
                             int cnt)
{
    SensorsPipe *pipe = opaque;
    int i, bytes=0;

    if (pipe->send_data->len > 0) {
        for (i=0; i<cnt && pipe->send_data->len; i++) {
            int to_copy =
                pipe->send_data->len <= buffers[i].size ?
                pipe->send_data->len : buffers[i].size;
            memcpy(buffers[i].data, pipe->send_data->str, to_copy);
            g_string_erase(pipe->send_data, 0, to_copy);
            bytes += to_copy;
        }
        DPRINTF("pipe %p, hwpipe %p, read %d bytes\n", pipe, pipe->hwpipe, bytes);
        return bytes;
    } else {
        return PIPE_ERROR_AGAIN;
    }
}

/*
 * ASSUMPTIONS: we assume any given write will be consumed in entirely
 * one buffer which is not unreasonable given the guest writes them as
 * such. If we ever see a count of > 1 we should join the buffers
 * together and drip feed the contents to goldfish_sensors_have_data.
 
 */
static int sensors_pipe_send(void *opaque, const AndroidPipeBuffer* buffers,
                             int cnt)
{
    SensorsPipe *pipe = opaque;
    int i, consumed = 0;
    DPRINTF("pipe %p, buffers %p, cnt: %d\n", pipe, buffers, cnt);

    for (i=0; i<cnt; i++) {
        consumed += goldfish_sensors_have_data(pipe,
                                               buffers[i].data, buffers[i].size);
    }
    
    return consumed;
}

static unsigned sensors_pipe_poll(void *opaque)
{
    SensorsPipe *pipe = opaque;
    unsigned flags = 0;
    
    if (pipe->send_data->len > 0) {
        flags |= PIPE_POLL_IN;
    }
    flags |= PIPE_POLL_OUT;
    DPRINTF("pipe %p, flags 0x%x", pipe, flags);
    return flags;
}

static const AndroidPipeFuncs  sensors_pipe_funcs = {
    sensors_pipe_init,
    sensors_pipe_close,
    sensors_pipe_send,
    sensors_pipe_recv,
    sensors_pipe_poll,
    sensors_pipe_wake,
};

void android_sensors_init(void)
{
    goldfish_sensors_set_rotation(0);
    android_pipe_add_type("sensors", NULL, &sensors_pipe_funcs);
}
