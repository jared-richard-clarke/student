# SoundIO: C vs Zig

Code examples by Andrew Kelly

## C Source Code

```c
int main(int argc, char **argv) {
    struct SoundIo *soundio = NULL;
    struct SoundIoOutStream *outstream = NULL;
    struct SoundIoDevice *device = NULL;
    int err;
    soundio = soundio_create();
    if (!soundio) {
        err = SoundIoErrorNoMem;
        goto cleanup;
    }
    if ((err = soundio_connect(soundio))) {
        goto cleanup;
    }
    soundio_flush_events(soundio);
    int default_output_index = soundio_default_output_device_index(soundio);
    if (default_output_index < 0) {
        err = SoundIoErrorNoSuchDevice;
        goto cleanup;
    }
    device = soundio_get_output_device(soundio, default_output_index);
    if (!device) {
        goto cleanup;
    }
    outstream = soundio_outstream_create(device);
    if (!outstream) {
        err = SoundIoErrorNoMem;
        goto cleanup;
    }
    outstream->format = SoundIoFormatFloat32NE;
    outstream->write_callback = write_callback;
    if ((err = soundio_outstream_open(outstream))) {
        goto cleanup;
    }
    if ((err = soundio_outstream_start(outstream))) {
        goto cleanup;
    }
    for (;;) soundio_wait_events(soundio);
cleanup:
    fprintf(stderr, "error: %s\n", soundio_strerror(err));
    if (outstream) soundio_outstream_destroy(outstream);
    if (device) soundio_device_unref(device);
    if (soundio) soundio_destroy(soundio);
    return 1;
}
```

## Zig Source Code

```zig
pub fn main() !void {
    const soundio = c.soundio_create() orelse return error.OutOfMemory;
    defer c.soundio_destroy(soundio);

    try sio_err(c.soundio_connect(soundio));

    c.soundio_flush_events(soundio);

    const default_output_index = c.soundio_default_output_device_index(soundio);
    if (default_output_index < 0) return error.NoOutputDevice;

    const device = c.soundio_get_output_device(soundio, default_output_index) orelse return error.OutOfMemory;
    defer c.soundio_device_unref(device);

    const outstream = c.soundio_outstream_create(device) orelse return error.OutOfMemory;
    defer c.soundio_outstream_destroy(outstream);

    outstream.format = c.SoundIoFormatFloat32NE;
    outstream.write_callback = write_callback;

    try sio_err(c.soundio_outstream_open(outstream));
    try sio_err(c.soundio_outstream_start(outstream));

    while (true) c.soundio_wait_events(soundio);
}
```
