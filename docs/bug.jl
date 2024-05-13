using GTPSA #hide
d1 = Descriptor(2, 10)         
d2 = Descriptor([1, 2, 3], 5)     
d3 = Descriptor(3, 4, 1, 2)    
d4 = Descriptor([6, 5], 8, [4, 3], 7)   
GTPSA.desc_current = d1

using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS()
t2_1 = TPS(5)
t3_1 = TPS(t2_1)
d2 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = TPS() # Uses d2
t2_2 = TPS(6)
t3_2 = TPS(t3_1, use=d2) # Copies and changes Descriptor
GTPSA.show_header = false #hide

using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = ComplexTPS()
t2_1 = ComplexTPS(5)
t3_1 = ComplexTPS(6, TPS(5))
t3_1 = ComplexTPS(TPS(5))
d2 = Descriptor(1, 10); # New Descriptor to order 10
t3_2 = ComplexTPS(t3_1, use=d2) # Copies and changes Descriptor
GTPSA.show_header = false #hide

using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header=true
d1 = Descriptor(3, 5, 2, 5); # 3 vars, 2 params, all to order 5
x1 = vars()
k1 = params()
d2 = Descriptor(2, 5, 1, 5); # 2 vars, 1 param, all to order 5
x2 = vars()
k2 = params()
k1 = params(d1)

using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header=false;#hide
d = Descriptor(2, 6, 3, 6); # 2 variables, 3 parameters all to 6th order
x = vars(d);
k = params(d);
f = 5 + sin(x[1])*sin(x[2])*cos(k[1])
f[[3,1,2]] # Leave out trailing zeros for unincluded variables/parameters
f[[0]] # Scalar part
f[(1,1,1,1,1)] = 123; # Set monomial coefficient
print(f)

using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false; #hide
d = Descriptor(15, 6, 10, 6); # 15 variables, 10 parameters all to 6th order
GTPSA.show_sparse = true; # Use sparse output
x = vars(d);
k = params(d);
f = 5 + sin(x[1])*sin(x[15])*cos(k[10])
f[[1=>3, 15=>1], params=[10=>2]]
f[(1=>1, 15=>2), params=(10=>3,)] = 123; # Set monomial coefficient
print(f)

using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false; #hide
# Example of indexing by monomial index -----------
d = Descriptor(2, 10, 1, 10);
t = TPS(use=d); # Create zero TPS based on d

t[0] = 0;
t[1] = 1;
t[2] = 2;
t[3] = 3;  # or t[param=1] = 3
t[4] = 4;
t[5] = 5; 
t[6] = 6;
t[7] = 7;
t[8] = 8;
t[9] = 9;
t[10] = 10;
print(t)

using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false;#hide
d1 = Descriptor(3, 15, 2, 15); # 3 vars, 2 params, all to order 15
x1 = mono(1)
k1 = mono(param=1)
m312 = mono([3,1,2])
m31221 = mono((3,1,2,2,1)) # Tuples allowed for indexing
m312 = mono([1=>3, 2=>1, 3=>3])
m31221 = mono((1=>3, 2=>1, 3=>2), params=(1=>2, 2=>1))

using GTPSA; #hide
d = Descriptor(2,10);
x = vars(d);
f = x[1] + 2*x[2] + 3*x[1]^2 + 4*x[1]*x[2] + 5*x[2]^2;
g = 5*x[1] + 4*x[2] + 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2;
grad = gradient(f)
J = jacobian([f, g])
H = hessian(f)

using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header = false; #hide
d = Descriptor(5, 10, 2, 10);
x = vars(d);
k = params(d);
f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5]*k[1] + 6*x[3] + 5
g = f[[2,:,1]]
h = f[[2,:,1,:]]

g = f[[1=>2, :, 3=>1, 4=>0, 5=>0], params=[1=>0, 2=>0]]
h = f[(1=>2, 3=>1, :)]  # Colon position is irrelevant in slicing with sparse monomial indexing

fx3 = f[3,:]
fk1 = f[:,param=1]

using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header=false; #hide
d = Descriptor(5, 10, 2, 10);
x = vars(d);
k = params(d);
f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5]*k[1] + 6*x[3] + 5
par(f, 3)
par(f, param=1)
par(f, [2,:,1])
par(f, [2,0,1])
par(f, [1=>2, 3=>1])
par(f, params=[1=>1])

using GTPSA, BenchmarkTools
GTPSA.show_sparse = false; GTPSA.show_header=false; # hide

d = Descriptor(3, 5);
x = vars(d);

@btime $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

@btime @FastGTPSA $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
