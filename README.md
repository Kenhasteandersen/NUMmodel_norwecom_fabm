# NORWECOM.E2E-FABM

An implementation of the Norwegian Ecological Modeling System End-To-End (NORWECOM.E2E) in the Framework for Aquatic Biogeochemical Models (FABM). Used in the PELAGIC project

## Todo at IMR:
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

## Download 

```bash
# Clone the repository:
git clone https://github.com/Kenhasteandersen/NUMmodel_norwecom_fabm.git

# Clone external dependencies: gotm, fabm, and NUM:
git submodule update --init --recursive
```

If the input file in NUM has changed, you might want to copy it to `input/input.h`

## Compile

Make a build directory:
```bash
mkdir build
```

For the following instructions, it is assumed that the directory structure is as follows. Make the necessary adjustments if your directory structure differs:

```bash
a5482@anhur ~/Temp $ tree -d -L 1
.
├── build # Holds the build files
├── gotm # <- Holds the gotm and fabm framework source code
├── input # <- Holds the NUM input file
├── norwecome2e_fabm # <- Holds the norwecome2e_fabm source code
└── station_test # <- Holds the test site
```

Compile using `cmake` and `make`:

```bash
cd build
cmake -DFABM_INSTITUTES="imr;gotm" -DFABM_IMR_BASE=../norwecome2e_fabm ../gotm
make
```

If the linking step fails to find the netcdf library then linking has to be done manually.
This is done by adding `-DCMAKE_VERBOSE_MAKEFILE=on` to `cmake`. While doing the `make` the last step is the linker step. Copy that, and paste the path to the library into it and link manually.


## Run test model

Run by going into the `station_test` directory:
`../build/gotm --ignore_unknown_config`


