# GIA-2Dtest

A small two dimensional test of a 2-layer Earth (crust + upper Mantle) driven by the load of a prescribed Bueler profile.
To run this case, you need to have Gmsh (tested with version 4.4.1)and Elmer (tested with Elmer revision 09aa1f2) installed. To view the results, you need to have Paraview installed.

Files:
- `README.md` ... this file
- `genmesh.sh` ... script that creates mesh
- `compile.sh` ... script that compiles needed external functions
- `cleanup.sh` ... script that cleans out any objects, mesh-files and results (!!)
- `2layer_ice.geo` ... input file for 
- `2layer_ice.sif` ... the main solver input file
- `parameter.sif` ... definition of run parameters (in LUA syntax)
- `ELMERSOLVER_STARTINFO` ... startinfo file for Elmre run
- `deform.lua` ... definition of needed LUA functions 
- `buelerprofile.f90` ... source code for generating the imposed Bueler profile
- `DummySolver.F90` ... solver used for creating a mask in the beginning
- `serial.pvsm` ... Paraview state file for visualization

For the brave and impatient:
```
$ ./compile && ./genmesh && ElmerSolver
```
