# EfficientSetOptimizer

Tests have been solved using this version

IBM Cplex is not provided and must be acquired. This solver can be purchased but can be obtained for free under certain conditions. Please refer to the IBM academic initiative.

# Installation

Change the path to cplex in `package.yaml`. You can use

```
sed -i 's,/home/sat/cplex2210,/path/to/cplex,g' package.yaml
```

Install the [haskell stack suite](https://docs.haskellstack.org/en/stable/). This is probably packaged on your linux distribution.

```
cd EfficinetSetOptimizer
stack install

```

On linux distributions, the binary is installed in `~/.local/bin/EfficientSetOptimizer-exe`.To uninstall, just remove the git repository, the binary in `~/.local/bin/EfficientSetOptimizer-exe`. If you want to uninstall stack, use your package manager. You may also remove `~/.stack`.


# Execution

syntax: `EfficientSetOptimizer-exe <KP|AP> instance logfile`

## MOKP instances

The lines are the number of objectives, the number of items, the maximum capacity of the knapsack, a matrix, ie a coma-separated list of coma-separated integer, (representing the objective functions) and a coma-separated list of integer (the weights). There must not be empty lines at the end of the file.

## MOAP instances

The lines are The lines are the number of objectives, the number of machines (square root of the number of decision variables) and a coma-separated list of matrices, ie coma-separated list of coma-separated list, denoting each objective coefficient.

