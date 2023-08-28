/*
 * VIRTIO Sound Device PCI Bindings
 *
 * Copyright (c) 2023 Emmanouil Pitsidianakis <manos.pitsidianakis@linaro.org>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or
 * (at your option) any later version.  See the COPYING file in the
 * top-level directory.
 */

#include "qemu/osdep.h"
#include "qapi/error.h"
#include "hw/audio/soundhw.h"
#include "hw/virtio/virtio-pci.h"
#include "hw/virtio/virtio-snd.h"

typedef struct VirtIOSoundPCI VirtIOSoundPCI;

/*
 * virtio-snd-pci: This extends VirtioPCIProxy.
 */
#define TYPE_VIRTIO_SND_PCI "virtio-sound-pci"
DECLARE_INSTANCE_CHECKER(VirtIOSoundPCI, VIRTIO_SND_PCI,
                         TYPE_VIRTIO_SND_PCI)

struct VirtIOSoundPCI {
    VirtIOPCIProxy parent;
    VirtIOSound vdev;
};

static Property virtio_snd_pci_properties[] = {
    DEFINE_AUDIO_PROPERTIES(VirtIOSoundPCI, vdev.card),
    DEFINE_PROP_BIT("ioeventfd", VirtIOPCIProxy, flags,
                    VIRTIO_PCI_FLAG_USE_IOEVENTFD_BIT, true),
    DEFINE_PROP_UINT32("vectors", VirtIOPCIProxy, nvectors,
                       DEV_NVECTORS_UNSPECIFIED),
    DEFINE_PROP_END_OF_LIST(),
};

static void virtio_snd_pci_realize(VirtIOPCIProxy *vpci_dev, Error **errp)
{
    VirtIOSoundPCI *dev = VIRTIO_SND_PCI(vpci_dev);
    DeviceState *vdev = DEVICE(&dev->vdev);

    if (vpci_dev->nvectors == DEV_NVECTORS_UNSPECIFIED) {
        vpci_dev->nvectors = 2;
    }

    virtio_pci_force_virtio_1(vpci_dev);
    qdev_realize(vdev, BUS(&vpci_dev->bus), errp);
}

static void virtio_snd_pci_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    VirtioPCIClass *vpciklass = VIRTIO_PCI_CLASS(klass);

    device_class_set_props(dc, virtio_snd_pci_properties);
    dc->desc = "Virtio Sound";
    set_bit(DEVICE_CATEGORY_SOUND, dc->categories);

    vpciklass->realize = virtio_snd_pci_realize;
}

static void virtio_snd_pci_instance_init(Object *obj)
{
    VirtIOSoundPCI *dev = VIRTIO_SND_PCI(obj);

    virtio_instance_init_common(obj, &dev->vdev, sizeof(dev->vdev),
                                TYPE_VIRTIO_SND);
}

static const VirtioPCIDeviceTypeInfo virtio_snd_pci_info = {
    .generic_name  = TYPE_VIRTIO_SND_PCI,
    .instance_size = sizeof(VirtIOSoundPCI),
    .instance_init = virtio_snd_pci_instance_init,
    .class_init    = virtio_snd_pci_class_init,
};

/* Create a Virtio Sound PCI device, so '-audio driver,model=virtio' works. */
static int virtio_snd_pci_init(PCIBus *bus, const char *audiodev)
{
    DeviceState *dev;

    dev = qdev_new(TYPE_VIRTIO_SND_PCI);
    qdev_prop_set_string(dev, "audiodev", audiodev);
    qdev_realize_and_unref(dev, BUS(bus), &error_fatal);
    return 0;
}

static void virtio_snd_pci_register(void)
{
    virtio_pci_types_register(&virtio_snd_pci_info);
    pci_register_soundhw("virtio", "Virtio Sound", virtio_snd_pci_init);
}

type_init(virtio_snd_pci_register);
