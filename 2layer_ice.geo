LI1 = 6371.0;
//
UM11 = 6251.0;
UM31 = 5771.0;
minh = 0.1;

Lh  = 2000.0;
r0 = 200.0;
r1 = 300.0;
r2 = 400.0;
resmid = 5.0;
rescent = 1.0;
resout = 20.0;

Point(1) = {   0,  0,  0, rescent};
Point(2) = {  r0,  0,  0, rescent};
Point(3) = {  r1,  0,  0, resmid};
Point(4) = {  r2,  0,  0, resmid};
Point(5) = {  Lh,  0,  0, resout};

Line(1)={1,2};
Line(2)={2,3};
Line(3)={3,4};
Line(4)={4,5};

//+
Extrude {0, UM11 - LI1, 0} {
  Curve{1}; Curve{2}; Curve{3}; Curve{4}; Layers{20}; Recombine;
}

//+
Extrude {0, UM31 - UM11, 0} {
  Curve{5}; Curve{9}; Curve{13}; Curve{17}; Layers{30}; Recombine;
}

//+
Extrude {0,minh, 0} {
  Curve{1}; Curve{2}; Curve{3}; Curve{4}; Layers{10}; Recombine;
}



Physical Surface(1) = {40, 44, 48, 52};
//+
Physical Surface(2) = {8, 12, 16, 20};
//+
Physical Surface(3) = {24, 28, 32, 36};
//
Physical Curve(4) = {37, 41, 45, 49};
//+
Physical Curve(5) = {38};
//+
Physical Curve(6) = {51};
//+
Physical Curve(7) = {1, 2, 3, 4};
//+
Physical Curve(8) = {21, 25, 29, 33};
//+
Physical Curve(9) = {6, 22};
//+
Physical Curve(10) = {19, 35};

