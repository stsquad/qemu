/*
 * VIRTIO Sound Device conforming to
 *
 * "Virtual I/O Device (VIRTIO) Version 1.2
 * Committee Specification Draft 01
 * 09 May 2022"
 *
 * Copyright (c) 2023 Emmanouil Pitsidianakis <manos.pitsidianakis@linaro.org>
 * Copyright (C) 2019 OpenSynergy GmbH
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */

#ifndef QEMU_VIRTIO_SOUND_H
#define QEMU_VIRTIO_SOUND_H

#include "hw/virtio/virtio.h"
#include "audio/audio.h"
#include "standard-headers/linux/virtio_ids.h"
#include "standard-headers/linux/virtio_snd.h"

#define TYPE_VIRTIO_SND "virtio-sound"
#define VIRTIO_SND(obj) \
        OBJECT_CHECK(VirtIOSound, (obj), TYPE_VIRTIO_SND)

/* CONFIGURATION SPACE */

typedef struct virtio_snd_config virtio_snd_config;

/* COMMON DEFINITIONS */

/* common header for request/response*/
typedef struct virtio_snd_hdr virtio_snd_hdr;

/* event notification */
typedef struct virtio_snd_event virtio_snd_event;

/* common control request to query an item information */
typedef struct virtio_snd_query_info virtio_snd_query_info;

/* JACK CONTROL MESSAGES */

typedef struct virtio_snd_jack_hdr virtio_snd_jack_hdr;

/* jack information structure */
typedef struct virtio_snd_jack_info virtio_snd_jack_info;

/* jack remapping control request */
typedef struct virtio_snd_jack_remap virtio_snd_jack_remap;

/*
 * PCM CONTROL MESSAGES
 */
typedef struct virtio_snd_pcm_hdr virtio_snd_pcm_hdr;

/* PCM stream info structure */
typedef struct virtio_snd_pcm_info virtio_snd_pcm_info;

/* set PCM stream params */
typedef struct virtio_snd_pcm_set_params virtio_snd_pcm_set_params;

/* I/O request header */
typedef struct virtio_snd_pcm_xfer virtio_snd_pcm_xfer;

/* I/O request status */
typedef struct virtio_snd_pcm_status virtio_snd_pcm_status;

/* device structs */

typedef struct VirtIOSound VirtIOSound;

typedef struct VirtIOSoundPCMStream VirtIOSoundPCMStream;

typedef struct virtio_snd_ctrl_command virtio_snd_ctrl_command;

typedef struct VirtIOSoundPCMParams VirtIOSoundPCMParams;

typedef struct VirtIOSoundPCM VirtIOSoundPCM;

/* Stream params */
struct VirtIOSoundPCMParams {
    uint32_t features;
    uint32_t buffer_bytes;          /* size of hardware buffer in bytes */
    uint32_t period_bytes;          /* size of hardware period in bytes */
    uint8_t channels;
    uint8_t format;
    uint8_t rate;
};

struct VirtIOSoundPCM {
    VirtIOSound *snd;
    VirtIOSoundPCMParams *pcm_params;
    VirtIOSoundPCMStream **streams;
};

struct VirtIOSoundPCMStream {
    VirtIOSoundPCM *pcm;
    virtio_snd_pcm_info info;
    uint32_t id;
    uint32_t buffer_bytes;
    uint32_t period_bytes;
    /* channel position values (VIRTIO_SND_CHMAP_XXX) */
    uint8_t positions[VIRTIO_SND_CHMAP_MAX_SIZE];
    VirtIOSound *s;
    uint32_t features; /* 1 << VIRTIO_SND_PCM_F_XXX */
    uint64_t formats; /* 1 << VIRTIO_SND_PCM_FMT_XXX */
    uint64_t rates; /* 1 << VIRTIO_SND_PCM_RATE_XXX */
    uint8_t direction;
    uint8_t channels_min;
    uint8_t channels_max;
    bool flushing;
    audsettings as;
    audsettings desired_as;
    union {
        SWVoiceIn *in;
        SWVoiceOut *out;
    } voice;
    QemuMutex queue_mutex;
    QSIMPLEQ_HEAD(, VirtIOSoundPCMBlock) queue;
};

struct VirtIOSound {
    VirtIODevice parent_obj;
    VirtQueue *queues[VIRTIO_SND_VQ_MAX];
    uint64_t features;
    VirtIOSoundPCM *pcm;
    QEMUSoundCard card;
    VMChangeStateEntry *vmstate;
    virtio_snd_config snd_conf;
    QemuMutex cmdq_mutex;
    QTAILQ_HEAD(, virtio_snd_ctrl_command) cmdq;
    bool processing_cmdq;
};

struct virtio_snd_ctrl_command {
    VirtQueueElement *elem;
    VirtQueue *vq;
    virtio_snd_hdr ctrl;
    virtio_snd_hdr resp;
    QTAILQ_ENTRY(virtio_snd_ctrl_command) next;
};
#endif
