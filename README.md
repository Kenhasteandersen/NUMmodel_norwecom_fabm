# NORWECOM.E2E-FABM

An implementation of the Norwegian Ecological Modeling System End-To-End (NORWECOM.E2E) in the Framework for Aquatic Biogeochemical Models (FABM). Used in the PELAGIC project

On the anhur.hi.no terminal server the following setup is necessary:

```bash
ssh -Y <username>@anhur.hi.no
module load gcc-toolset
```

Due to a suspected bug in the NetCDF build scripts, nf-config by default does not point to the right include directory. A custom version of nf-config is available in the `norwecom.e2e.forcing` directory. Helpdesk/IT is aware of this issue, so the following may not be necessary in the future.

```bash
export PATH=/data/osea/norwecom.e2e.forcing/fabm:$PATH
```

To avoid repeating this process,  add it to `.bashrc` as:
```bash
# if on ANHUR
if [ "$HOSTNAME" = anhur.hi.no ]; then
    # Redhat toolset
    module load gcc-toolset

    # Point to correct nf-config
    export PATH=/data/osea/norwecom.e2e.forcing/fabm/bin:$PATH
fi
```

## Download `norwecome2e_fabm`

```bash
git clone git@git.imr.no:norwecome2e/norwecome2e_fabm.git
```

## Download GOTM

```bash
# Clone the GOTM repository
git clone https://github.com/gotm-model/code.git gotm

# Checkout the latest stable release (v6.0.6; 2024-01-18)
cd gotm
git checkout v6.0.6 -b stable

# Clone FABM as well as other external dependencies
git submodule update --init --recursive
```

## Compile GOTM with FABM

Make a build directory:
```bash
mkdir -p build
```

For the following instructions, it is assumed that the directory structure is as follows. Make the necessary adjustments if your directory structure differs.

```bash
a5482@anhur ~/Temp $ tree -d -L 1
.
├── build # Holds the build files
├── gotm # <- Holds the gotm and fabm framework source code
└── norwecome2e_fabm # <- Holds the norwecome2e_fabm source code

2 directories
```

Compile using `cmake` and `make`:

```bash
cd build
cmake -DFABM_INSTITUTES="imr;gotm" -DFABM_IMR_BASE=../norwecome2e_fabm ../gotm
make
```

## Run test model

Run by going into the `station_test` directory:
`../build/gotm --ignore_unknown_config`


