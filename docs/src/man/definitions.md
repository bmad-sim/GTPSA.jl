# Definitions
*This section is incomplete, and will be expanded later*

GTPSA allows for explicitly distinguishing between state *variables* and external *parameters*. For example, suppose you have defined the evolution of some initial state $x_i$ of a dynamical system to a final state $x_f$ as $x_f = \mathcal{M}(x_i,k)$, where $k$ is some external parameter. When $\mathcal{M}$ is expanded in powers of $\Delta x_i$ and $\Delta k$, distinguishing between the state variable $x$ and parameter $k$ can significantly simplify analyses of the system.  We refer to $\mathcal{M}(x_i,k)$ as a *map*.