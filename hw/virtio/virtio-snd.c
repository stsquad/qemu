/*
 * VIRTIO Sound Device conforming to
 *
 * "Virtual I/O Device (VIRTIO) Version 1.2
 * Committee Specification Draft 01
 * 09 May 2022"
 *
 * <https://docs.oasis-open.org/virtio/virtio/v1.2/csd01/virtio-v1.2-csd01.html#x1-52900014>
 *
 * Copyright (c) 2023 Emmanouil Pitsidianakis <manos.pitsidianakis@linaro.org>
 * Copyright (C) 2019 OpenSynergy GmbH
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */

#include "qemu/osdep.h"
#include "qemu/iov.h"
#include "qemu/log.h"
#include "include/qemu/lockable.h"
#include "sysemu/runstate.h"
#include "trace.h"
#include "qapi/error.h"
#include "hw/virtio/virtio-snd.h"

#define VIRTIO_SOUND_VM_VERSION 1
#define VIRTIO_SOUND_JACK_DEFAULT 0
#define VIRTIO_SOUND_STREAM_DEFAULT 1
#define VIRTIO_SOUND_CHMAP_DEFAULT 0
#define VIRTIO_SOUND_HDA_FN_NID 0

static const char *print_code(uint32_t code)
{
    #define CASE(CODE)            \
    case VIRTIO_SND_R_##CODE:     \
        return "VIRTIO_SND_R_"#CODE

    switch (code) {
    CASE(JACK_INFO);
    CASE(JACK_REMAP);
    CASE(PCM_INFO);
    CASE(PCM_SET_PARAMS);
    CASE(PCM_PREPARE);
    CASE(PCM_RELEASE);
    CASE(PCM_START);
    CASE(PCM_STOP);
    CASE(CHMAP_INFO);
    default:
        return "invalid code";
    }

    #undef CASE
};

static const VMStateDescription vmstate_virtio_snd_device = {
    .name = TYPE_VIRTIO_SND,
    .version_id = VIRTIO_SOUND_VM_VERSION,
    .minimum_version_id = VIRTIO_SOUND_VM_VERSION,
};

static const VMStateDescription vmstate_virtio_snd = {
    .name = "virtio-sound",
    .minimum_version_id = VIRTIO_SOUND_VM_VERSION,
    .version_id = VIRTIO_SOUND_VM_VERSION,
    .fields = (VMStateField[]) {
        VMSTATE_VIRTIO_DEVICE,
        VMSTATE_END_OF_LIST()
    },
};

static Property virtio_snd_properties[] = {
    DEFINE_PROP_UINT32("jacks", VirtIOSound, snd_conf.jacks,
                       VIRTIO_SOUND_JACK_DEFAULT),
    DEFINE_PROP_UINT32("streams", VirtIOSound, snd_conf.streams,
                       VIRTIO_SOUND_STREAM_DEFAULT),
    DEFINE_PROP_UINT32("chmaps", VirtIOSound, snd_conf.chmaps,
                       VIRTIO_SOUND_CHMAP_DEFAULT),
    DEFINE_PROP_END_OF_LIST(),
};

static void
virtio_snd_get_config(VirtIODevice *vdev, uint8_t *config)
{
    VirtIOSound *s = VIRTIO_SND(vdev);
    trace_virtio_snd_get_config(vdev,
                                s->snd_conf.jacks,
                                s->snd_conf.streams,
                                s->snd_conf.chmaps);

    memcpy(config, &s->snd_conf, sizeof(s->snd_conf));
}

static void
virtio_snd_set_config(VirtIODevice *vdev, const uint8_t *config)
{
    VirtIOSound *s = VIRTIO_SND(vdev);
    const virtio_snd_config *sndconfig =
        (const virtio_snd_config *)config;


   trace_virtio_snd_set_config(vdev,
                               s->snd_conf.jacks,
                               sndconfig->jacks,
                               s->snd_conf.streams,
                               sndconfig->streams,
                               s->snd_conf.chmaps,
                               sndconfig->chmaps);

    memcpy(&s->snd_conf, sndconfig, sizeof(s->snd_conf));
}

/*
 * The actual processing done in virtio_snd_process_cmdq().
 *
 * @s: VirtIOSound device
 * @cmd: control command request
 */
static inline void
process_cmd(VirtIOSound *s, virtio_snd_ctrl_command *cmd)
{
    size_t sz = iov_to_buf(cmd->elem->out_sg,
                           cmd->elem->out_num,
                           0,
                           &cmd->ctrl,
                           sizeof(cmd->ctrl));
    if (sz != sizeof(cmd->ctrl)) {
        qemu_log_mask(LOG_GUEST_ERROR,
                "%s: virtio-snd command size incorrect %zu vs \
                %zu\n", __func__, sz, sizeof(cmd->ctrl));
        return;
    }

    trace_virtio_snd_handle_code(cmd->ctrl.code,
                                 print_code(cmd->ctrl.code));

    switch (cmd->ctrl.code) {
    case VIRTIO_SND_R_JACK_INFO:
    case VIRTIO_SND_R_JACK_REMAP:
        qemu_log_mask(LOG_UNIMP,
                     "virtio_snd: jack functionality is unimplemented.");
        cmd->resp.code = VIRTIO_SND_S_NOT_SUPP;
        break;
    case VIRTIO_SND_R_PCM_INFO:
    case VIRTIO_SND_R_PCM_SET_PARAMS:
    case VIRTIO_SND_R_PCM_PREPARE:
    case VIRTIO_SND_R_PCM_START:
    case VIRTIO_SND_R_PCM_STOP:
    case VIRTIO_SND_R_PCM_RELEASE:
        cmd->resp.code = VIRTIO_SND_S_NOT_SUPP;
        break;
    case VIRTIO_SND_R_CHMAP_INFO:
        qemu_log_mask(LOG_UNIMP,
                     "virtio_snd: chmap info functionality is unimplemented.");
        trace_virtio_snd_handle_chmap_info();
        cmd->resp.code = VIRTIO_SND_S_NOT_SUPP;
        break;
    default:
        /* error */
        error_report("virtio snd header not recognized: %"PRIu32,
                     cmd->ctrl.code);
        cmd->resp.code = VIRTIO_SND_S_BAD_MSG;
    }

    iov_from_buf(cmd->elem->in_sg,
                 cmd->elem->in_num,
                 0,
                 &cmd->resp,
                 sizeof(cmd->resp));
    virtqueue_push(cmd->vq, cmd->elem, sizeof(cmd->elem));
    virtio_notify(VIRTIO_DEVICE(s), cmd->vq);
}

/*
 * Consume all elements in command queue.
 *
 * @s: VirtIOSound device
 */
static void virtio_snd_process_cmdq(VirtIOSound *s)
{
    virtio_snd_ctrl_command *cmd;

    if (unlikely(qatomic_read(&s->processing_cmdq))) {
        return;
    }

    WITH_QEMU_LOCK_GUARD(&s->cmdq_mutex) {
        qatomic_set(&s->processing_cmdq, true);
        while (!QTAILQ_EMPTY(&s->cmdq)) {
            cmd = QTAILQ_FIRST(&s->cmdq);

            /* process command */
            process_cmd(s, cmd);

            QTAILQ_REMOVE(&s->cmdq, cmd, next);

            g_free(cmd);
        }
        qatomic_set(&s->processing_cmdq, false);
    }
}

/*
 * The control message handler. Pops an element from the control virtqueue,
 * and stores them to VirtIOSound's cmdq queue and finally calls
 * virtio_snd_process_cmdq() for processing.
 *
 * @vdev: VirtIOSound device
 * @vq: Control virtqueue
 */
static void virtio_snd_handle_ctrl(VirtIODevice *vdev, VirtQueue *vq)
{
    VirtIOSound *s = VIRTIO_SND(vdev);
    VirtQueueElement *elem;
    virtio_snd_ctrl_command *cmd;

    trace_virtio_snd_handle_ctrl(vdev, vq);

    if (!virtio_queue_ready(vq)) {
        return;
    }

    elem = virtqueue_pop(vq, sizeof(VirtQueueElement));
    while (elem) {
        cmd = g_new0(virtio_snd_ctrl_command, 1);
        cmd->elem = elem;
        cmd->vq = vq;
        cmd->resp.code = VIRTIO_SND_S_OK;
        QTAILQ_INSERT_TAIL(&s->cmdq, cmd, next);
        elem = virtqueue_pop(vq, sizeof(VirtQueueElement));
    }

    virtio_snd_process_cmdq(s);
}

/*
 * The event virtqueue handler.
 * Not implemented yet.
 *
 * @vdev: VirtIOSound device
 * @vq: event vq
 */
static void virtio_snd_handle_event(VirtIODevice *vdev, VirtQueue *vq)
{
    qemu_log_mask(LOG_UNIMP, "virtio_snd: event queue is unimplemented.");
    trace_virtio_snd_handle_event();
}

/*
 * Stub buffer virtqueue handler.
 *
 * @vdev: VirtIOSound device
 * @vq: virtqueue
 */
static void virtio_snd_handle_xfer(VirtIODevice *vdev, VirtQueue *vq) {}

static uint64_t get_features(VirtIODevice *vdev, uint64_t features,
                             Error **errp)
{
    /*
     * virtio-v1.2-csd01, 5.14.3,
     * Feature Bits
     * None currently defined.
     */
    VirtIOSound *s = VIRTIO_SND(vdev);
    features |= s->features;

    trace_virtio_snd_get_features(vdev, features);

    return features;
}

static void virtio_snd_set_pcm(VirtIOSound *snd)
{
    VirtIOSoundPCM *pcm;

    pcm = g_new0(VirtIOSoundPCM, 1);
    pcm->snd = snd;
    pcm->streams = g_new0(VirtIOSoundPCMStream *, snd->snd_conf.streams);
    pcm->pcm_params = g_new0(VirtIOSoundPCMParams, snd->snd_conf.streams);

    snd->pcm = pcm;
}

static void virtio_snd_common_realize(DeviceState *dev,
                                      VirtIOHandleOutput ctrl,
                                      VirtIOHandleOutput evt,
                                      VirtIOHandleOutput txq,
                                      VirtIOHandleOutput rxq,
                                      Error **errp)
{
    VirtIODevice *vdev = VIRTIO_DEVICE(dev);
    VirtIOSound *vsnd = VIRTIO_SND(dev);

    virtio_snd_set_pcm(vsnd);

    virtio_init(vdev, VIRTIO_ID_SOUND, sizeof(virtio_snd_config));
    virtio_add_feature(&vsnd->features, VIRTIO_F_VERSION_1);

    /* set number of jacks and streams */
    if (vsnd->snd_conf.jacks > 8) {
        error_setg(errp,
                   "Invalid number of jacks: %"PRIu32,
                   vsnd->snd_conf.jacks);
        return;
    }
    if (vsnd->snd_conf.streams < 1 || vsnd->snd_conf.streams > 10) {
        error_setg(errp,
                   "Invalid number of streams: %"PRIu32,
                    vsnd->snd_conf.streams);
        return;
    }

    if (vsnd->snd_conf.chmaps > VIRTIO_SND_CHMAP_MAX_SIZE) {
        error_setg(errp,
                   "Invalid number of channel maps: %"PRIu32,
                   vsnd->snd_conf.chmaps);
        return;
    }

    AUD_register_card("virtio-sound", &vsnd->card);

    vsnd->queues[VIRTIO_SND_VQ_CONTROL] = virtio_add_queue(vdev, 64, ctrl);
    vsnd->queues[VIRTIO_SND_VQ_EVENT] = virtio_add_queue(vdev, 64, evt);
    vsnd->queues[VIRTIO_SND_VQ_TX] = virtio_add_queue(vdev, 64, txq);
    vsnd->queues[VIRTIO_SND_VQ_RX] = virtio_add_queue(vdev, 64, rxq);
    qemu_mutex_init(&vsnd->cmdq_mutex);
    QTAILQ_INIT(&vsnd->cmdq);
}

static void
virtio_snd_vm_state_change(void *, bool running, RunState)
{
    if (running) {
        trace_virtio_snd_vm_state_running();
    } else {
        trace_virtio_snd_vm_state_stopped();
    }
}

static void virtio_snd_realize(DeviceState *dev, Error **errp)
{
    ERRP_GUARD();
    VirtIOSound *vsnd = VIRTIO_SND(dev);

    vsnd->pcm = NULL;
    vsnd->vmstate =
        qemu_add_vm_change_state_handler(virtio_snd_vm_state_change, vsnd);

    trace_virtio_snd_realize(vsnd);

    virtio_snd_common_realize(dev,
                              virtio_snd_handle_ctrl,
                              virtio_snd_handle_event,
                              virtio_snd_handle_xfer,
                              virtio_snd_handle_xfer,
                              errp);
}

/*
 * Close the stream and free its resources.
 *
 * @stream: VirtIOSoundPCMStream *stream
 */
static void virtio_snd_pcm_close(VirtIOSoundPCMStream *stream)
{
}

static void virtio_snd_unrealize(DeviceState *dev)
{
    VirtIODevice *vdev = VIRTIO_DEVICE(dev);
    VirtIOSound *vsnd = VIRTIO_SND(dev);
    VirtIOSoundPCMStream *stream;

    qemu_del_vm_change_state_handler(vsnd->vmstate);
    virtio_del_queue(vdev, 0);

    trace_virtio_snd_unrealize(vsnd);

    if (vsnd->pcm) {
        if (vsnd->pcm->streams) {
            for (uint32_t i = 0; i < vsnd->snd_conf.streams; i++) {
                stream = vsnd->pcm->streams[i];
                if (stream) {
                    virtio_snd_process_cmdq(stream->s);
                    virtio_snd_pcm_close(stream);
                    g_free(stream);
                }
            }
            g_free(vsnd->pcm->streams);
        }
        g_free(vsnd->pcm);
        vsnd->pcm = NULL;
    }
    AUD_remove_card(&vsnd->card);
    virtio_cleanup(vdev);
}


static void virtio_snd_reset(VirtIODevice *vdev)
{
    VirtIOSound *s = VIRTIO_SND(vdev);
    virtio_snd_ctrl_command *cmd;

    WITH_QEMU_LOCK_GUARD(&s->cmdq_mutex) {
        while (!QTAILQ_EMPTY(&s->cmdq)) {
            cmd = QTAILQ_FIRST(&s->cmdq);
            QTAILQ_REMOVE(&s->cmdq, cmd, next);
            g_free(cmd);
        }
    }
}

static void virtio_snd_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    VirtioDeviceClass *vdc = VIRTIO_DEVICE_CLASS(klass);


    set_bit(DEVICE_CATEGORY_SOUND, dc->categories);
    device_class_set_props(dc, virtio_snd_properties);

    dc->vmsd = &vmstate_virtio_snd;
    vdc->vmsd = &vmstate_virtio_snd_device;
    vdc->realize = virtio_snd_realize;
    vdc->unrealize = virtio_snd_unrealize;
    vdc->get_config = virtio_snd_get_config;
    vdc->set_config = virtio_snd_set_config;
    vdc->get_features = get_features;
    vdc->reset = virtio_snd_reset;
    vdc->legacy_features = 0;
}

static const TypeInfo virtio_snd_types[] = {
    {
      .name          = TYPE_VIRTIO_SND,
      .parent        = TYPE_VIRTIO_DEVICE,
      .instance_size = sizeof(VirtIOSound),
      .class_init    = virtio_snd_class_init,
    }
};

DEFINE_TYPES(virtio_snd_types)
