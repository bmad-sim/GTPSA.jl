# [Definitions](@id definitions)
*This section is incomplete, and will be expanded in the near future*

GTPSA allows for explicitly distinguishing between state *variables* and external *parameters*. For example, suppose you have defined the evolution of some initial state ``x_i`` of a dynamical system to a final state ``x_f`` as ``x_f = \mathcal{M}(x_i,k)``, where ``k`` is some external parameter. When ``\mathcal{M}`` is expanded in powers of ``\Delta x_i`` and ``\Delta k``, distinguishing between the state variable ``x`` and parameter ``k`` can significantly simplify analyses of the system.  In the context of `GTPSA.jl`, we refer to ``\mathcal{M}(x_i,k)`` as a *GTPSA map*. The GTPSA map is strictly different from the so-called *differential algebra (DA) map*, whereby the coordinate system follows the zeroth order part so that it is zero at all states. `GTPSA.jl` only provides functionalities for a GTPSA map; the management of the zeroth order part to create a proper DA map is relegated to a separate analysis package.

The difference between a GTPSA map and a DA map can be shown with a simple example. Consider the two GTPSA maps with only 1 variable, ``M_1`` and ``M_2``:

```math
\begin{aligned}
M_1(x) &= x + 2 \\
M_2(x) &= x^2 + 3x
\end{aligned}
```

Composing the maps ``M_2(M_1(x))`` gives

```math
M_2(M_1(x)) = (x+2)^2 + 3(x+2) = x^2 + 7x+10
```

However, if only 1st order terms are retained in the expressions for ``M_1`` and ``M_2``, we would have

```math
M_2(M_1(x)) = 3(x+2) = 3x + 6
```

This shows that by neglecting the 2nd order term in `M_2`, we obtain 0th and 1st order errors in the composition. These errors can be traced to the finite 0th order term in `M_1`. This is referred to as *feed-down*, and is a problem when our GTPSA map has a 0th order term. One way of solving this problem is to use a coordinate system ``\tilde{x} = x+2``for ``M_1``; using such a coordinate system elevates our map to a DA map.
