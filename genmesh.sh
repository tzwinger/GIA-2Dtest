npar=6
gmsh -1 -2 2layer_ice.geo
ElmerGrid 14 2 2layer_ice.msh -autoclean -scale 1000.0 1000.0 1.0
ElmerGrid 2 2 2layer_ice -partcell $npar 1 1 
