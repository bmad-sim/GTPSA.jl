# [Monomial Indexing](@id monoindex)
*Get/set individual monomial coefficients*

Individual monomial coefficients in a TPS can be get/set with three methods of indexing: **by monomial index**, **by order**, and **by sparse monomial**. 

## By Monomial Index
```
t[idx]
t[param=param_idx]
```

*This indexing method requires care when indexing monomials above first order.* Indexes the TPS with all monomials sorted by order. For example, for a TPS with one variable ``\Delta x_1`` and one parameter ``\Delta k_1`` the ``\Delta x_1`` monomial is indexed with `t[1]` and the ``\Delta x_1^2`` monomial is indexed with either `t[3]`. The ``\Delta k_1`` monomial can be indexed with either `t[2]` or equivalently using the `param` helper kwarg `t[param=1]`, which simply adds the number of variables in the GTPSA to the provided index. Note that above first-order, the `param` kwarg is basically useless. The zeroth order part, or the *scalar* part of the TPS, can be set with `t[0]`. This method requires zero allocations for indexing, unlike the other two.

### Examples
```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
# Example of indexing by monomial index -----------
d = Descriptor(2, 10, 1, 10);
t = TPS{d}(); # Create zero TPS based on d

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
```


## By Order
```
t[[<var_1 order>, ..., <var_N order>,<param_1 order>, ..., <param_M order>]]
t[(<var_1 order>, ..., <var_N order>,<param_1 order>, ..., <param_M order>)]
```

A particular monomial can be indexed by specifying the orders of each variable and parameter. For example, for a TPS `t` with variables ``\Delta x_1``, ``\Delta x_2`` and parameters ``\Delta k_1``, ``\Delta k_2``, the ``\Delta x_1^3\Delta x_2^1\Delta k_1^2`` monomial coefficient is accessed with `t[[3,1,2,0]]` or equivalently `t[[3,1,2]]`, as leaving out trailing zeros for unincluded variables/parameters is allowed. A tuple is also allowed instead of a vector for the list of orders.

### Examples
```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
d = Descriptor(2, 6, 3, 6); # 2 variables, 3 parameters all to 6th order
Δx = @vars(d);
Δk = @params(d);
f = 5 + sin(Δx[1])*sin(Δx[2])*cos(Δk[1])
f[[3,1,2]] # Leave out trailing zeros for unincluded variables/parameters
f[[0]] # Scalar part
f[(1,1,1,1,1)] = 123; # Set monomial coefficient
print(f)
```

## By Sparse Monomial
```
t[[<ix_var> => <order>, ...], params=[<ix_param> => <order>, ...]]
t[(<ix_var> => <order>, ...), params=(<ix_param> => <order>, ...)]
```

In GTPSAs with many variables and parameters, indexing-by-order is inconvenient because each order needs to be included up to the last included variable/parameter with nonzero order. In this case, a particular monomial can be indexed instead by specifying each variable/parameter number and its corresponding order in pairs. For example, for a TPS with variables ``\Delta x_1, ..., \Delta x_{15}`` and parameters ``\Delta k_1, ..., \Delta k_{10}``, the ``\Delta x_{1}^3\Delta x_{15}^1\Delta k_{10}^2`` monomial coefficient is accessed with `t[[1=>3, 15=>1], params=[10=>2]]`. The scalar part of the TPS cannot be get/set with this method. A tuple is also allowed instead of a vector for the list of pairs.

### Examples
```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
d = Descriptor(15, 6, 10, 6); # 15 variables, 10 parameters all to 6th order
GTPSA.show_sparse = true; # Use sparse output
Δx = @vars(d);
Δk = @params(d);
f = 5 + sin(Δx[1])*sin(Δx[15])*cos(Δk[10])
f[[1=>3, 15=>1], params=[10=>2]]
f[(1=>1, 15=>2), params=(10=>3,)] = 123; # Set monomial coefficient
print(f)
```



