# [Monomial Indexing](@id monoindex)
*Get/set individual monomial coefficients*

Individual monomial coefficients in a TPS can be get/set with two methods of indexing: **by order**, and **by sparse monomial**. 

## By Order
`t[<var_1 order>, ..., <var_N order>,<param_1 order>, ..., <param_M order>]`

A particular monomial can be indexed by specifying the orders of each variable and parameter. For example, for a TPS `t` with variables ``x_1``, ``x_2`` and parameter ``k_1`` the ``x_1^3x_2^1k_1^2`` monomial coefficient is accessed with `t[3,1,2]`. The 0th order part (the *scalar* part) of the TPS is indexed with `t[0,0,0]` or equivalently `t[0]`, as leaving out trailing zeros for unincluded variables/parameters is allowed.

### Examples
```@repl
using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header=false;#hide
d = Descriptor(2, 6, 3, 6); # 2 variables, 3 parameters all to 6th order
x = vars(d);
k = params(d);
f = 5 + sin(x[1])*sin(x[2])*cos(k[1])
f[3,1,2] # Leave out trailing zeros for unincluded variables/parameters
f[0] # Scalar part
f[1,1,1,1,1] = 123; # Set monomial coefficient
print(f)
```

## By Sparse Monomial
`t[<ix_var> => <order>, ..., params=[<ix_param> => <order>, ...]]`

In GTPSAs with many variables and parameters, indexing-by-order is inconvenient because each order needs to be included up to the last included variable/parameter with nonzero order. In this case, a particular monomial can be indexed instead by specifying each variable/parameter number and its corresponding order in pairs. For example, for a TPS with variables ``x_1, ..., x_{15}`` and parameters ``k_1, ..., k_{10}``, the ``x_{1}^3x_{15}^1k_{10}^2`` monomial coefficient is accessed with `t[1=>3, 15=>1, params=[10=>2]]`. The scalar part of the TPS cannot be get/set with this method.

### Examples
```@repl
using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false; #hide
d = Descriptor(15, 6, 10, 6); # 15 variables, 10 parameters all to 6th order
GTPSA.show_sparse = true; # Use sparse output
x = vars(d);
k = params(d);
f = 5 + sin(x[1])*sin(x[15])*cos(k[10])
f[1=>3, 15=>1, params=[10=>2]]
f[1=>1, 15=>2, params=[10=>3]] = 123; # Set monomial coefficient
print(f)
```




