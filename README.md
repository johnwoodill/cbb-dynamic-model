# Optimal spraying and harvesting strategies to combat cbb: a dynamic approach

### Introduction

This repository provides the code for a forward-recursive dynamic programming model to estimate the optimal spraying and harvesting strategies to combat an invasive species to Hawaii coffee, the coffee berry borer (CBB).  To run the model follow the number order of each file name in the parent directory.  Here is a description of each file.

**Dynamic Model**

* `1-parameters.R`: initial parameter estimates to incorporate into the model

* `2-calibrated_markov_chains.R`: calibration of Markov chains based off of SHAC data for spraying and not spraying.

* `3-dynamic_model.R`: full dynamic model

**Functions in R/**

* `cherrygrowth.R`: logistic growth of green cherry based on acres \* projected cherry

* `cherrypricing.R`: dynamic cherry pricing based on Greenwell Farms current pricing 8/24/2016

* `decision.R`: decision to spray or not spray

* `markovcalibration.R`: returns calibrated Markov chains based on projected infestation when spraying or not spraying

* `maxnb.R`: dynamic function the maximize net benefit in $t$ period

* `trans.matrix.R`: standard maximum liklihood estimation of Markov process.

**Other files**

* `figures.R`: builds tables and plots from `3-dynamic_model.R`

* `doc/preliminary_results.Rmd`: document outlining initial results from baseline model

* `schac_data_calibration.R`: calibration of Markov chains from SHAC data.

## Dynamic model outline

Formally, the objective is to maximize the net benefit $NB$ in each period and is defined as:

\begin{equation}
\max_{h,s} NB = \sum_{t=1}^{12} [P_t H_t D_t - c_s S_t - c_h H_t]
\end{equation}

$$ \text{s.t.} \hspace{3mm} S_t = \{0,1\}$$
$$H_t \leq \zeta_t $$
$$\zeta_t = \zeta_{t-1} - H_{t-1}$$ 
$$D_t = V_{CDt} = V_{it-1} \cdot [S_t \cdot SP_t] + V_{it-1} \cdot [(1-S_t) \cdot NSP_t]$$

where $P_t$ is the dynamic cherry pricing based on infestation of C/D position, $H_t$ is the amount of cherry available to harvest, $D_t$ is the economic damage based on the level of C/D position, $c_s$ is the cost to spray, $S_t$ is the choice to spray or not, $c_h$ is the cost per pound of cherry harvested, $\zeta_t$ is the proportion of cherry ripe and ready to harvest, $SP_t$ is the Markov matrix when choosing to spray, and $NSP_t$ is the Markov matrix when choosing to not spray.

Dynamically, the value function $V_t$ is defined as the $NB$ from equation 1,

\begin{equation}
\max_{h_t, s_t} V_t(H_t, D_t, C_t) + V_{t-1} (H_{t-1}, D_{t-1}, C_{t-1}) 
\end{equation}

### Markov process

Let $(\Omega, \cal{F}, \mathbb{P})$ be a probability space with filtration $(\mathcal{F}_t, t \in T)$ with index set $T$, and $(S,\mathcal{S})$ be a measure space.  The stochastic process $X = (X_t, t \in T)$ follows Markov property with the filtration process $\mathcal{F}_t$ for each $A \in \mathcal{S}$ and each $s,t \in T$ where $s<t$. A Markov process is a stochastic process which satisfies the Markov property with respect to the natural filtration (Durrett, 2010):

\begin{equation}
\mathbb{P}(X_t \in A |\mathcal{F}_s) = \mathbb{P}(X_t \in A| X_s)
\end{equation}

For a time-inhomogenous Markov chain,

Initial values ($X_0 = IV$) provide starting values for not infested ($NI$) levels representing population not in cherry; A/B Live ($ABL$) represents CBB in A/B position and alive; A/B dead ($ABD$) represents CBB in A/B position and dead; and C/D ($CD$) represents CBB in C/D position -- also represented as damage $D_t$ below. 
\begin{equation}
IV = 
\begin{bmatrix}
NI & ABL & ABD & CD\\
\end{bmatrix}
\end{equation}

Transition matrices for spraying and not spraying for a markov chain $m$ are defined as ($SP_{t}, NSP_{t}$)  where each matrix defines a probability for each month to allow the markov transition with event probabilities $(P_{ij})$ in the probability space ($\mathbb{P}$) moving through filtration ($\mathcal{F}_t$) from the sample space ($\Omega$).  

\begin{equation}
SP_{t} = 
\begin{bmatrix}
P_{11} & P_{12} & P_{13} & P_{14} \\
P_{21} & P_{22} & P_{23} & P_{24} \\
P_{31} & P_{32} & P_{33} & P_{34} \\
P_{41} & P_{42} & P_{43} & P_{44} \\
\end{bmatrix}
\end{equation} 

\begin{equation}
NSP_{t} = 
\begin{bmatrix}
P_{11} & P_{12} & P_{13} & P_{14} \\
P_{21} & P_{22} & P_{23} & P_{24} \\
P_{31} & P_{32} & P_{33} & P_{34} \\
P_{41} & P_{42} & P_{43} & P_{44} \\
\end{bmatrix}
\end{equation} 

These transition matrices define the movement from $IV$ to the Markov chain process where each event is defined as $V_{it}$

\begin{equation}
V_{it}=
\begin{bmatrix}
NI_t & ABL_t & ABD_t & CD_t
\end{bmatrix}
\end{equation}

Given a binary choice to not spray or spray ($S_t = \{0,1\}$), the current state of $V_{it}$ is given by:

\begin{equation}
V_{it} = V_{it-1} \cdot [S_t \cdot SP] + V_{it-1} \cdot[(1-S_t) \cdot NSP]
\end{equation}
  
Finally, for each period, the damages ($D_t$) from non-marketable coffee cherry is defined as:

\begin{equation}
D_t=V_{CDt}
\end{equation}