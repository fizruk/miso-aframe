# miso-aframe

Miso bindings for A-Frame framework for WebVR.

![Haskell logo in VR mode](images/haskell-logo-voxels-vr.jpg)

### Examples

- "Hello, world!" with basic primitives ([sources](examples/hello-world))
- Haskell logo built with voxels ([sources](examples/haskell-logo-voxels))

### How to build

```
stack setup
stack build
```

### How to run examples

To run an example use `run-example.sh` script:

```
./run-example.sh hello-world
```

This script will build everything if needed and also replace `index.html` when necessary.
If build is complete the script will also `open` the example using default browser.
